/*****************************************************************************************************/
/* Program:  ORACL_code.sas                                                                          */
/* Author:   Jessica C Young, OpioidDataLab															 */
/* Adapted from: Virginia Pate, University of North Carolina at Chapel Hill							 */
/*																									 */
/* Purpose:  Apply the ORACL algorithm to estimate the predicted probability of death at     	     */
/*            disenrollment from insurance claims. Based on work described in mansuscript:           */
/*																									 */
/* Location: This program and the accompanying datasets can be downloaded from ....					 */
/*                 																					 */                             
/*                                                                                                   */
/* Overview: This macro requires three datasets as inputs: 											 */
/*			1) A beneficiary level cohort dataset with all predictors defined						 */
/*			2) Coefficients from Algorithm A, developed using data from 2007-2018 					 */
/*			3) Coefficients from Algorithm B, developed using data from 2007-2011					 */
/*																									 */
/*                                                                                                   */
/* Output:   This macro will output a dataset with the same records as your input cohort dataset.    */
/*           The output dataset will have two additional variables:									 */
/*			 PredDeath18 - the predicted probability of death at disenrollment using algorithm A 	 */
/*			 PredDeath11 - the predicted probability of death at disenrollment using algorithm B 	 */
/*                                                                                                   */
/* Macro parameters:                                                                                 */
/*    COHORTDS: name of cohort dataset (including libname) containing one record per patient 		 */
/*    LIB:      library name where est11.sasbdat and est18.sasbdat are stored                        */
/*    OUTDS:    name of output dataset (including libname)					                         */
/*****************************************************************************************************/


%macro ORACL (cohortds=out.cohort_alg, outds=out.cohort_pred, lib=coeff);
	%LET lib = %UPCASE(&lib);

	*add an intercept variable equal to 1;
	data __cohort;
		set &cohortds.;
		_intercept_=1;
	run;

	*create macrovariables for each predictor in the 2007-2011 model;
	proc sql noprint;
		select distinct substr(name,7) into :EST11_1-:EST11_80 from dictionary.columns
			where upcase(libname)="&lib." and upcase(memname)='EST11' ;
		%LET NumEst11=&SqlObs;
	quit;
	
	data __est11; set &lib..est11; _intercept_=1; run;
	
	*create macrovariables for each predictor in the 2007-2018 model;
	proc sql noprint;
		select distinct substr(name,7) into :EST18_1-:EST18_80 from dictionary.columns
			where upcase(libname)="&lib." and upcase(memname)='EST18' ;
		%LET NumEst18=&SqlObs;
	quit;

	data __est18; set &lib..est18; _intercept_=1; run;


	data &outds.(drop=pred11 pred18);
		merge __cohort __est11 __est18;
		by _intercept_;
		
		PRED11=SUM(%DO c=1 %TO &NumEst11.; &&EST11_&c.. * EST11_&&EST11&c. %IF &c.<&NumEst11. %THEN ,; %END;);
		PRED18=SUM(%DO c=1 %TO &NumEst18.; &&EST18_&c.. * EST18_&&EST18&c. %IF &c.<&NumEst18. %THEN ,; %END;);

		PredDeath11=exp(pred11)/(1+exp(pred11));
		PredDeath18=exp(pred18)/(1+exp(pred18));
	run;
%mend;
%ORACL;






*********************************************************************
*  Program:     OOH_Death_Code
* 
*  Created by:  Jessica Young 
*                                                                    
*  Last Updated:   1/3/2018                                  
*
*  Purpose:   	Define a cohort of patients disenrolling form MarketScan
*             	Add relevant predictors of interest prior to disenrollment
*              	Send cohort to IBM for linkage of to SSA death files
*              	Run model predicting death at disenrollment
*                                         
*  Language:    SAS, VERSION 9.4  
*                                                    
********************************************************************;


/*Processing Macros*/

%macro macarray(stem,list_str, dlm=%str( ));
   %local i ; %global n&stem.;
   %let i=1;

   *count how many elements in the list;
   %do %while(%length(%scan(&list_str,&i,%str(&dlm))) GT 0);
      %let i=%eval(&i. + 1);
   %end;

   *create a macrovariable represnting the number of elements in the list;
   %let n&stem.=%eval(&i.-1);
   %put (&SYSMACRONAME.:) n&stem. = &&n&stem.., str=&list_str.;

   *loop through the elements and create a macrovariable for each element in the list; 
   %do i=1 %to %eval(1+&&n&stem..);
      %global &&stem.&i.;
      %let &stem&i.=%scan(&list_str., &i., %STR(&dlm));
   %end;

   *print to the log all macro variables created;
   %do i=1 %to %eval(&&n&stem..);
      %put (&sysmacroname.:) &stem.&i.= &&&&&stem.&i.;
   %end;
%mend;
%macro define_vars;
   %global covariates covariates_short event_list;
   %let covariates_short= 	bl_0_91_ER
   							bl_91_365_ER	
   						  	bl_0_91_hsp
							bl_91_365_hsp
							dec31_end
							bl_0_365_AMBLIFESUPPORT
							bl_0_365_CANCER
							bl_0_365_ECHOCARDIOGRAPH
			                bl_0_91_FLUSHOT
			                bl_91_365_FLUSHOT
							bl_0_365_HOMEHOSPBED
							bl_0_365_HOMEOXYGEN
							bl_0_365_MAMMOGRAM                
                  			bl_0_365_PROSTATE
		                  	bl_0_365_AKI
		                  	bl_0_365_ALCOHOL
		                    bl_0_365_ANXIETY
		                    bl_0_365_CANCERSCREEN
		                    bl_0_365_CHRONICLIVERDIS
		                    bl_0_365_COPD
		                    bl_0_365_DEMENTIA
							bl_0_365_DEPRESSION
							bl_0_365_DIABETES
							bl_0_365_DMCOMP
							bl_0_365_DRUGDEPENDENCE
							bl_0_365_HEARTFAILFRAILTY
							bl_0_365_HIPFRACTURE
							bl_0_365_HYPERTENSION
							bl_0_365_MI      
							bl_0_365_PNEUMONIA
							bl_0_365_PVD
							bl_0_365_STROKE
							bl_0_365_SUBABUSE
							bl_0_365_TOBACCO
							bl_0_365_WEAKNESS
							bl_0_365_inpt
							;

   %let covariates=bl_30_ER
                  bl_30_hsp
                  bl_91_ER
                  bl_91_hsp
                  bl_182_ER
                  bl_182_hsp
                  bl_365_ER
                  bl_365_hsp
                  dec31_end
                  end_mnth_disenroll
                  bl_30_AMBLIFESUPPORT
                  bl_91_AMBLIFESUPPORT
                  bl_182_AMBLIFESUPPORT
                  bl_365_AMBLIFESUPPORT
                  bl_30_ANNWELLVISIT
                  bl_91_ANNWELLVISIT
                  bl_182_ANNWELLVISIT
                  bl_365_ANNWELLVISIT
                  bl_30_CANCER
                  bl_91_CANCER
                  bl_182_CANCER
                  bl_365_CANCER
                  bl_30_ECHOCARDIOGRAPH
                  bl_91_ECHOCARDIOGRAPH
                  bl_182_ECHOCARDIOGRAPH
                  bl_365_ECHOCARDIOGRAPH
                  bl_30_FLUSHOT
                  bl_91_FLUSHOT
                  bl_182_FLUSHOT
                  bl_365_FLUSHOT
                  bl_30_HOMEHOSPBED
                  bl_91_HOMEHOSPBED
                  bl_182_HOMEHOSPBED
                  bl_365_HOMEHOSPBED
                  bl_30_HOMEOXYGEN
                  bl_91_HOMEOXYGEN
                  bl_182_HOMEOXYGEN
                  bl_365_HOMEOXYGEN
                  bl_30_LIPIDPANEL
                  bl_91_LIPIDPANEL
                  bl_182_LIPIDPANEL
                  bl_365_LIPIDPANEL
                  bl_30_MAMMOGRAM
                  bl_91_MAMMOGRAM
                  bl_182_MAMMOGRAM
                  bl_365_MAMMOGRAM
                  bl_30_NALOXONE
                  bl_91_NALOXONE
                  bl_182_NALOXONE
                  bl_365_NALOXONE
                  bl_30_PROSTATE
                  bl_91_PROSTATE
                  bl_182_PROSTATE
                  bl_365_PROSTATE
                  bl_30_AKI
                  bl_91_AKI
                  bl_182_AKI
                  bl_365_AKI
                  bl_30_ALCOHOL
                  bl_91_ALCOHOL
                  bl_182_ALCOHOL
                  bl_365_ALCOHOL
                  bl_30_ANXIETY
                  bl_91_ANXIETY
                  bl_182_ANXIETY
                  bl_365_ANXIETY
                  bl_30_CANCERSCREEN
                  bl_91_CANCERSCREEN
                  bl_182_CANCERSCREEN
                  bl_365_CANCERSCREEN
                  bl_30_CHRONICLIVERDIS
                  bl_91_CHRONICLIVERDIS
                  bl_182_CHRONICLIVERDIS
                  bl_365_CHRONICLIVERDIS
                  bl_30_COPD
                  bl_91_COPD
                  bl_182_COPD
                  bl_365_COPD
                  bl_30_DEMENTIA
                  bl_91_DEMENTIA
                  bl_182_DEMENTIA
                  bl_365_DEMENTIA
                  bl_30_DEPRESSION
                  bl_91_DEPRESSION
                  bl_182_DEPRESSION
                  bl_365_DEPRESSION
                   bl_30_DIABETES
                   bl_91_DIABETES
                   bl_182_DIABETES
                   bl_365_DIABETES
                   bl_30_DMCOMP
                   bl_91_DMCOMP
                   bl_182_DMCOMP
                   bl_365_DMCOMP
                   bl_30_DRUGDEPENDENCE
                   bl_91_DRUGDEPENDENCE
                   bl_182_DRUGDEPENDENCE
                   bl_365_DRUGDEPENDENCE
                   bl_30_HEARTFAILFRAILTY
                   bl_91_HEARTFAILFRAILTY
                   bl_182_HEARTFAILFRAILTY
                   bl_365_HEARTFAILFRAILTY
                   bl_30_HIPFRACTURE
                   bl_91_HIPFRACTURE
                   bl_182_HIPFRACTURE
                   bl_365_HIPFRACTURE
                   bl_30_HIV
                   bl_91_HIV
                   bl_182_HIV
                   bl_365_HIV
                   bl_30_HYPERTENSION
                   bl_91_HYPERTENSION
                   bl_182_HYPERTENSION
                   bl_365_HYPERTENSION
                   bl_30_MI
                   bl_91_MI
                   bl_182_MI
                   bl_365_MI
                   bl_30_OPIOIDABUSE
                   bl_91_OPIOIDABUSE
                   bl_182_OPIOIDABUSE
                   bl_365_OPIOIDABUSE
                   bl_30_OPIOIDDEP
                   bl_91_OPIOIDDEP
                   bl_182_OPIOIDDEP
                   bl_365_OPIOIDDEP
                   bl_30_OPPOISON
                   bl_91_OPPOISON
                   bl_182_OPPOISON
                   bl_365_OPPOISON
                   bl_30_PNEUMONIA
                   bl_91_PNEUMONIA
                   bl_182_PNEUMONIA
                   bl_365_PNEUMONIA
                   bl_30_PSYCHEXAM
                   bl_91_PSYCHEXAM
                   bl_182_PSYCHEXAM
                   bl_365_PSYCHEXAM
                   bl_30_PVD
                   bl_91_PVD
                   bl_182_PVD
                   bl_365_PVD
                   bl_30_STROKE
                   bl_91_STROKE
                   bl_182_STROKE
                   bl_365_STROKE
                   bl_30_SUBABUSE
                   bl_91_SUBABUSE
                   bl_182_SUBABUSE
                   bl_365_SUBABUSE
                   bl_30_SUICIDE
                   bl_91_SUICIDE
                   bl_182_SUICIDE
                   bl_365_SUICIDE
                   bl_30_TOBACCO
                   bl_91_TOBACCO
                   bl_182_TOBACCO
                   bl_365_TOBACCO
                   bl_30_WEAKNESS
                   bl_91_WEAKNESS
                   bl_182_WEAKNESS
                   bl_365_WEAKNESS
                  bl_30_inpt
                  bl_91_inpt
                  bl_182_inpt
                  bl_365_inpt
                  ;
	%let event_list=
	bl_30_ER
	bl_30_hsp
	bl_91_ER
	bl_91_hsp
			 bl_182_ER
			 bl_182_hsp
			 bl_365_ER
			 bl_365_hsp
			bl_30_AMBLIFESUPPORT
			bl_91_AMBLIFESUPPORT
			bl_182_AMBLIFESUPPORT
			bl_365_AMBLIFESUPPORT
			bl_30_ANNWELLVISIT
			bl_91_ANNWELLVISIT
			bl_182_ANNWELLVISIT
			bl_365_ANNWELLVISIT
			bl_30_CANCER
			bl_91_CANCER
			bl_182_CANCER
			bl_365_CANCER
			bl_30_ECHOCARDIOGRAPH
			bl_91_ECHOCARDIOGRAPH
			bl_182_ECHOCARDIOGRAPH
			bl_365_ECHOCARDIOGRAPH
			bl_30_FLUSHOT
			bl_91_FLUSHOT
			bl_182_FLUSHOT
			bl_365_FLUSHOT
			bl_30_HOMEHOSPBED
			bl_91_HOMEHOSPBED
			bl_182_HOMEHOSPBED
			bl_365_HOMEHOSPBED
			bl_30_HOMEOXYGEN
			bl_91_HOMEOXYGEN
			bl_182_HOMEOXYGEN
			bl_365_HOMEOXYGEN
			bl_30_LIPIDPANEL
			bl_91_LIPIDPANEL
			bl_182_LIPIDPANEL
			bl_365_LIPIDPANEL
			bl_30_MAMMOGRAM
			bl_91_MAMMOGRAM
			bl_182_MAMMOGRAM
			bl_365_MAMMOGRAM
			bl_30_NALOXONE
			bl_91_NALOXONE
			bl_182_NALOXONE
			bl_365_NALOXONE
			bl_30_PROSTATE
			bl_91_PROSTATE
			bl_182_PROSTATE
			bl_365_PROSTATE
			bl_30_AKI
			bl_91_AKI
			bl_182_AKI
			bl_365_AKI
			bl_30_ALCOHOL
			bl_91_ALCOHOL
			bl_182_ALCOHOL
			bl_365_ALCOHOL
			bl_30_ANXIETY
			bl_91_ANXIETY
			bl_182_ANXIETY
			bl_365_ANXIETY
			bl_30_CANCERSCREEN
			bl_91_CANCERSCREEN
			bl_182_CANCERSCREEN
			bl_365_CANCERSCREEN
			bl_30_CHRONICLIVERDIS
			bl_91_CHRONICLIVERDIS
			bl_182_CHRONICLIVERDIS
			bl_365_CHRONICLIVERDIS
			bl_30_COPD
			bl_91_COPD
			bl_182_COPD
			bl_365_COPD
			bl_30_DEMENTIA
			bl_91_DEMENTIA
			bl_182_DEMENTIA
			bl_365_DEMENTIA
			bl_30_DEPRESSION
			bl_91_DEPRESSION
			bl_182_DEPRESSION
			bl_365_DEPRESSION
			 bl_30_DIABETES
			 bl_91_DIABETES
			 bl_182_DIABETES
			 bl_365_DIABETES
			 bl_30_DMCOMP
			 bl_91_DMCOMP
			 bl_182_DMCOMP
			 bl_365_DMCOMP
			 bl_30_DRUGDEPENDENCE
			 bl_91_DRUGDEPENDENCE
			 bl_182_DRUGDEPENDENCE
			 bl_365_DRUGDEPENDENCE
			 bl_30_HEARTFAILFRAILTY
			 bl_91_HEARTFAILFRAILTY
			 bl_182_HEARTFAILFRAILTY
			 bl_365_HEARTFAILFRAILTY
			 bl_30_HIPFRACTURE
			 bl_91_HIPFRACTURE
			 bl_182_HIPFRACTURE
			 bl_365_HIPFRACTURE
			 bl_30_HIV
			 bl_91_HIV
			 bl_182_HIV
			 bl_365_HIV
			 bl_30_HYPERTENSION
			 bl_91_HYPERTENSION
			 bl_182_HYPERTENSION
			 bl_365_HYPERTENSION
			 bl_30_MI
			 bl_91_MI
			 bl_182_MI
			 bl_365_MI
			 bl_30_OPIOIDABUSE
			 bl_91_OPIOIDABUSE
			 bl_182_OPIOIDABUSE
			 bl_365_OPIOIDABUSE
			 bl_30_OPIOIDDEP
			 bl_91_OPIOIDDEP
			 bl_182_OPIOIDDEP
			 bl_365_OPIOIDDEP
			 bl_30_OPPOISON
			 bl_91_OPPOISON
			 bl_182_OPPOISON
			 bl_365_OPPOISON
			 bl_30_PNEUMONIA
			 bl_91_PNEUMONIA
			 bl_182_PNEUMONIA
			 bl_365_PNEUMONIA
			 bl_30_PSYCHEXAM
			 bl_91_PSYCHEXAM
			 bl_182_PSYCHEXAM
			 bl_365_PSYCHEXAM
			 bl_30_PVD
			 bl_91_PVD
			 bl_182_PVD
			 bl_365_PVD
			 bl_30_STROKE
			 bl_91_STROKE
			 bl_182_STROKE
			 bl_365_STROKE
			 bl_30_SUBABUSE
			 bl_91_SUBABUSE
			 bl_182_SUBABUSE
			 bl_365_SUBABUSE
			 bl_30_SUICIDE
			 bl_91_SUICIDE
			 bl_182_SUICIDE
			 bl_365_SUICIDE
			 bl_30_TOBACCO
			 bl_91_TOBACCO
			 bl_182_TOBACCO
			 bl_365_TOBACCO
			 bl_30_WEAKNESS
			 bl_91_WEAKNESS
			 bl_182_WEAKNESS
			 bl_365_WEAKNESS
			 bl_30_inpt
			 bl_91_inpt
			 bl_182_inpt
			 bl_365_inpt

			;
%mend;
/*^^^ Processing Macros ^^^*/

/*STEP 1 FIND COHORT OF INTEREST: PATIENTS DISENROLLING*/
*find all benes disenrolling 2006-OCT2011;
%macro denom;
   *find # patients enrolled during study period for at least one year;
   data enrollment_06_11;
      set der.rxallenroll;
      by enrolid end;
      where start<=mdy(10,31,2011) and end >= mdy(01,01,2006) and  end-start>=365;
   run;

   *subset to last enrollment period during the study period;
   data enrollment_06_11_uid;
      set enrollment_06_11;
      by enrolid end;
      if last.enrolid;
   run;

	*find those disenrolling between 2007-2011;
   data disenrollment;
      set enrollment_06_11_uid;
      by enrolid end;
      day=day(end);
      where mdy(01,01,2007)<=end<=mdy(10,31,2011);
   run;
%mend;
%denom;
%macarray(windows, 30 91 182 365);
%macro deaths(startyr=2006, endyr=2011, baseline=365);
*read in inpatient and outpatient services files;
*find patients who died in hospital and create flags for er and hospice flags different times during baseline;
   proc sql; 
          create table ER_Hsp_death as
         select distinct enrolid,
            sum(bl_30_er) as bl_30_ER label="# ER in 30 days prior" length=3,    
            sum(bl_30_hsp) as bl_30_hsp label="# hospice in 30 days prior" length=3,
            %do w=2 %to &nwindows.; %let last_w=%eval(&w.-1); %let window=&&windows&w..;
               sum(bl_&window._er) as bl_&window._ER label="Binary Indicator for  ER in &&windows&last_w. - &window. days prior" length=3,      
               sum(bl_&window._hsp) as bl_&window._hsp label="Binary Indicator for Hospice in &&windows&last_w. - &window. days prior" length=3,
            %end;
            min(death) as death format=date9. label='Date of In Hospital Death'
         from 
            (%DO i=&startyr %TO &endyr;
            
               select distinct a.enrolid, a.end, b.admdate as svcdate, 
               case when (end-30 <= b.admdate <= end and (stdplac=23 or substr(svcscat,4,2)="20"))
                  then 1 else 0 end as bl_30_er,
               case when (end-30 <= b.admdate <= end and (stdplac=34 or substr(svcscat,4,2)="42"))
                  then 1 else 0 end as bl_30_hsp,
               %do w=2 %to &nwindows.;  %let last_w=%eval(&w.-1); %let window=&&windows&w..;
                  case when (end-&window. <= b.admdate < end-&&windows&last_w. and (stdplac=23 or substr(svcscat,4,2)="20"))
                     then 1 else 0 end as bl_&window._er,
                  case when (end-&window. <= b.admdate <end-&&windows&last_w.  and (stdplac=34 or substr(svcscat,4,2)="42"))
                     then 1 else 0 end as bl_&window._hsp,
               %end;
               case when dstatus in ('20','21','22','23','24','25','26','27','28','29','40','41','42') then b.disdate else . end as death

               from disenrollment(where=(end-&baseline.<="31DEC&i"d and end>="01JAN&i"d)) as a
                     inner join raw.inptserv&i (where=(stdplac=23 or stdplac=34 or 
                                       substr(svcscat,4,2)="20" or substr(svcscat,4,2)="42" or
                                       dstatus in ('20','21','22','23','24','25','26','27','28','29','40','41','42'))) as b
                           on a.enrolid = b.enrolid and a.end-&baseline. <= b.admdate <= a.end

            union corresponding

            select distinct a.enrolid, a.end, b.svcdate, 
               case when (end-30 <= b.svcdate <= end and (stdplac=23 or substr(svcscat,4,2)="20"))
                  then 1 else 0 end as bl_30_er,
               case when (end-30 <= b.svcdate <= end and (stdplac=34 or substr(svcscat,4,2)="42"))
                  then 1 else 0 end as bl_30_hsp,
               %do w=2 %to &nwindows.;  %let last_w=%eval(&w.-1); %let window=&&windows&w..;
                  case when (end-&window. <= b.svcdate < end-&&windows&last_w. and (stdplac=23 or substr(svcscat,4,2)="20"))
                     then 1 else 0 end as bl_&window._er,
                  case when (end-&window. <= b.svcdate <end-&&windows&last_w.  and (stdplac=34 or substr(svcscat,4,2)="42"))
                     then 1 else 0 end as bl_&window._hsp,
               %end;
               . as death
               from disenrollment(where=(end-&baseline.<="31DEC&i"d and end>="01JAN&i"d)) as a
                     inner join raw.outptserv&i (where=(stdplac=23 or stdplac=34 or 
                                       substr(svcscat,4,2)="20" or substr(svcscat,4,2)="42")) as b
                           on a.enrolid = b.enrolid and a.end-&baseline. <= b.svcdate <= a.end

            union corresponding

               select distinct a.enrolid, a.end,  b.admdate as svcdate, 
               case when (end-30 <= b.admdate <= end and (stdplac=23 or substr(svcscat,4,2)="20"))
                  then 1 else 0 end as bl_30_er,
               case when (end-30 <= b.admdate <= end and (stdplac=34 or substr(svcscat,4,2)="42"))
                  then 1 else 0 end as bl_30_hsp,
               %do w=2 %to &nwindows.;  %let last_w=%eval(&w.-1); %let window=&&windows&w..;
                  case when (end-&window. <= b.admdate < end-&&windows&last_w. and (stdplac=23 or substr(svcscat,4,2)="20"))
                     then 1 else 0 end as bl_&window._er,
                  case when (end-&window. <= b.admdate <end-&&windows&last_w.  and (stdplac=34 or substr(svcscat,4,2)="42"))
                     then 1 else 0 end as bl_&window._hsp,
               %end;

               case when dstatus in ('20','21','22','23','24','25','26','27','28','29','40','41','42') then b.disdate else . end as death
               from disenrollment (where=(end-&baseline.<="31DEC&i"d and end>="01JAN&i"d)) as a
                  inner join mdcr.inptserv&i (where=(stdplac=23 or stdplac=34 or 
                                       substr(svcscat,4,2)="20" or substr(svcscat,4,2)="42" or
                                       dstatus in ('20','21','22','23','24','25','26','27','28','29','40','41','42'))) as b
                        on a.enrolid = b.enrolid and a.end-&baseline. <= b.admdate <= a.end

            union corresponding

            select distinct a.enrolid, a.end,  b.svcdate,
               case when (end-30 <= b.svcdate <= end and (stdplac=23 or substr(svcscat,4,2)="20"))
                  then 1 else 0 end as bl_30_er,
               case when (end-30 <= b.svcdate <= end and (stdplac=34 or substr(svcscat,4,2)="42"))
                  then 1 else 0 end as bl_30_hsp,
               %do w=2 %to &nwindows.;  %let last_w=%eval(&w.-1); %let window=&&windows&w..;
                  case when (end-&window. <= b.svcdate < end-&&windows&last_w. and (stdplac=23 or substr(svcscat,4,2)="20"))
                     then 1 else 0 end as bl_&window._er,
                  case when (end-&window. <= b.svcdate <end-&&windows&last_w.  and (stdplac=34 or substr(svcscat,4,2)="42"))
                     then 1 else 0 end as bl_&window._hsp,
               %end;
               . as death
               from disenrollment(where=(end-&baseline.<="31DEC&i"d and end>="01JAN&i"d)) as a
                  inner join mdcr.outptserv&i (where=(stdplac=23 or stdplac=34 or 
                                       substr(svcscat,4,2)="20" or substr(svcscat,4,2)="42")) as b
                     on a.enrolid = b.enrolid and a.end-&baseline. <= b.svcdate <= a.end

            %IF &i<&endYr %THEN union corresponding;
            %END;
         )
         group by enrolid;
   quit;
%mend;
%deaths;

*get emprel status from annual enrollmnet files for benes who disenrolled;
%macro emprel(startyr=2006, endyr=2011);
   proc sql; 
          create table EmpRel as
         select distinct enrolid, min(emprel) as emprel
         from 
            (%DO i=&startyr %TO &endyr;            
                  select distinct a.enrolid, b.emprel
               from disenrollment(where=("01JAN&i"d<=end<="31DEC&i"d)) as a
                     inner join raw.annenr&i as b
                           on a.enrolid = b.enrolid
                  union corresponding
                  select distinct a.enrolid, b.emprel
               from disenrollment(where=("01JAN&i"d<=end<="31DEC&i"d)) as a
                     inner join mdcr.annenr&i as b
                           on a.enrolid = b.enrolid

            %IF &i<&endYr %THEN union corresponding;
            %END;
         )
         group by enrolid;
   quit;
%mend;
%emprel;

%macro merge;
	data out.disenroll_death;
	   merge disenrollment er_hsp_death emprel;
	   by enrolid;
	   hosp_death_flag=0;
	   if not missing(death) then hosp_death_flag=1;
	   dec31_end=0;
	   month_end=month(end);
	   day_end=day(end);
	   year_end=year(end);
	   end_mnth_disenroll=0;
	   if month_end in (1,3,5,7,8,10,12) and day_end=31 then end_mnth_disenroll=1;
	   if month_end in (4,6,9,11) and day_end=30 then end_mnth_disenroll=1;
	   if month_end=2 and mod(year_end,4)=0 and day_end=29 then end_mnth_disenroll=1;
	   if month_end=2 and mod(year_end,4)>0 and day_end=28 then end_mnth_disenroll=1;
	   if month_end=12 and day_end=31 then dec31_end=1;
	   %do w=1 %to &nwindows.;
	      if missing(bl_&&windows&w.._ER) then bl_&&windows&w.._ER=0;
	      if missing(bl_&&windows&w.._hsp) then bl_&&windows&w.._hsp=0;
	   %end;
	   label hosp_death_flag= "Flag for In-hospital Death";
	   label end_mnth_disenroll = "Flag for Disenrollment at End of Month";
	   label dec31_end = "Flag for Disenrollment at End of Year";
	run;
%mend;
%merge;
/*STEP 2: ADD COVARIATES FOR COHORT OF INTEREST*/
%getcodes(print=Y);
%macarray(windows, 30 91 182 365);

%macro bl_covs( startyr=2006, endyr=2011, baseline=365);
	*find latest service date before disenrollment;
	proc sql;
		create table final_dx as
		select distinct enrolid, end, max(svcdate) as final_svcdate label="Last Service Date in Claims Prior to Disenrollment" format=date9. from
		(%DO yr=&startYr %TO &endYr;
				select distinct  a.enrolid, a.end, b.svcdate
			           from out.disenroll_death  as a 
			             inner join der.alldx&yr. as b
			               on a.enrolid = b.enrolid and a.end - &baseline. <= b.svcdate <= a.end

			 union corresponding
				select distinct a.enrolid, a.end,b.svcdate
	          		from out.disenroll_death as a 
		            	inner join mdcrder.alldx&yr as b
			               	on a.enrolid = b.enrolid and a.end - &baseline. <= b.svcdate <= a.end
				%IF &yr<&endYr %THEN union corresponding; %END;
	     ) group by enrolid, end
	     order by enrolid, end;
   quit;

  proc sql;
      create table bldx as 
      select distinct enrolid, end
        %DO d=1 %TO &numDxCov; 
			%do w=1 %to &nwindows.;
            , sum((variable="&&dxCovar&d") * bl_&&windows&w..)  as &&dxCovar&d.._&&windows&w.._dx  
				label="# of Dx Claims for &&dxCovar&d in &&windows&w..-Days Pre-Visit" length=3 
			%end;
		%END;
      from (%DO yr=&startYr %TO &endYr;
			select distinct  a.enrolid, a.end, b.svcdate, c.variable
			,case when a.end-30<=b.svcdate <=a.end then 1 else 0 end as bl_30 length=3
			%do w=2 %to &nwindows.;
				%let last_w=%eval(&w.-1);
			    , case when a.end-&&windows&w..<=b.svcdate <a.end-&&windows&last_w.. then 1 else 0 end as bl_&&windows&w.. length=3
			%end;
		           from out.disenroll_death  as a 
		             inner join der.alldx&yr. as b
		               on a.enrolid = b.enrolid and a.end - &baseline. <= b.svcdate <= a.end
		             inner join dxCodes as c on b.dx&yr. = c.code

		 union corresponding
			select distinct a.enrolid, a.end,b.svcdate, c.variable
			,case when a.end-30<=b.svcdate <=a.end then 1 else 0 end as bl_30 length=3
			%do w=2 %to &nwindows.;
				%let last_w=%eval(&w.-1);
			    , case when a.end-&&windows&w..<=b.svcdate <a.end-&&windows&last_w.. then 1 else 0 end as bl_&&windows&w.. length=3
			%end;
          		from out.disenroll_death as a 
	            	inner join mdcrder.alldx&yr as b
		               	on a.enrolid = b.enrolid and a.end - &baseline. <= b.svcdate <= a.end
	         		inner join dxcodes as c on b.dx&yr.=c.code
			%IF &yr<&endYr %THEN union corresponding; %END;
     ) group by enrolid, end
     order by enrolid, end;
   quit;

      proc sql;
      create table blproc as 
      select distinct enrolid, end
        %DO p=1 %TO &numProcCov; 
			%do w=1 %to &nwindows.;
            , sum((variable="&&ProcCovar&p") * bl_&&windows&w..)  as &&ProcCovar&p.._&&windows&w.._proc  
				label="# of Proc Claims for &&procCovar&p in &&windows&w..-Days Pre-Visit" length=3 
			%end;
		%END;
      from (%DO yr=&startYr %TO &endYr;
				select distinct  a.enrolid, a.end,b.svcdate, c.variable
				,case when a.end-30<=b.svcdate <=a.end then 1 else 0 end as bl_30 length=3
				%do w=2 %to &nwindows.;
					%let last_w=%eval(&w.-1);
				    , case when a.end-&&windows&w..<=b.svcdate <a.end-&&windows&last_w.. then 1 else 0 end as bl_&&windows&w.. length=3
				%end;

		           from out.disenroll_death  as a 
		             inner join der.allproc&yr. as b
		               on a.enrolid = b.enrolid and a.end - &baseline. <= b.svcdate <= a.end
		             inner join procCodes as c on b.proc&yr. = c.code

		 union corresponding
				select distinct a.enrolid, a.end,b.svcdate, c.variable
				,case when a.end-30<=b.svcdate <=a.end then 1 else 0 end as bl_30 length=3
				%do w=2 %to &nwindows.;
					%let last_w=%eval(&w.-1);
				    , case when a.end-&&windows&w..<=b.svcdate <a.end-&&windows&last_w.. then 1 else 0 end as bl_&&windows&w.. length=3
				%end;

          		from out.disenroll_death as a 
	            	inner join mdcrder.allproc&yr as b
		               	on a.enrolid = b.enrolid and a.end - &baseline. <= b.svcdate <= a.end
	         		inner join proccodes as c on b.proc&yr.=c.code
			%IF &yr<&endYr %THEN union corresponding; %END;
     ) group by enrolid, end
     order by enrolid, end;
   quit;

   proc sql;
      create table blinpt as 
      select distinct enrolid, end, 
			sum(bl_30)as inpt_30 label="# of Inpatient Admissions in 30-Days Pre-Visit" length=3 
			%do w=2 %to &nwindows.;
				%let last_w=%eval(&w.-1);
	            , sum(bl_&&windows&w..)  as inpt_&&windows&w..  
					label="# of Inpatient Admissions in &&windows&last_w.. - &&windows&w..-Days Pre-Visit" length=3 
			%end;
      from (%DO yr=&startYr %TO &endYr;
				select distinct  a.enrolid, a.end, b.admdate
				,case when a.end-30<=b.admdate <=a.end then 1 else 0 end as bl_30 length=3
				%do w=2 %to &nwindows.;
					%let last_w=%eval(&w.-1);
				    , case when a.end-&&windows&w..<=b.admdate <a.end-&&windows&last_w.. then 1 else 0 end as bl_&&windows&w.. length=3
				%end;
		           from out.disenroll_death  as a 
		             inner join raw.inptadm&yr. as b
		               on a.enrolid = b.enrolid and a.end - &baseline. <= b.admdate <= a.end

		 union corresponding
				select distinct  a.enrolid, a.end, b.admdate
				,case when a.end-30<=b.admdate <=a.end then 1 else 0 end as bl_30 length=3
				%do w=2 %to &nwindows.;
					%let last_w=%eval(&w.-1);
				    , case when a.end-&&windows&w..<=b.admdate <a.end-&&windows&last_w.. then 1 else 0 end as bl_&&windows&w.. length=3
				%end;
		           from out.disenroll_death  as a 
		             inner join mdcr.inptadm&yr. as b
		               on a.enrolid = b.enrolid and a.end - &baseline. <= b.admdate <= a.end
			%IF &yr<&endYr %THEN union corresponding; %END;
     ) group by enrolid, end
     order by enrolid, end;
   quit;

   *2c: Merge datasets;
	data out.cohort_covs;
		merge out.disenroll_death blproc(in=prc) bldx(in=dgn) blinpt(in=ipt) final_dx;
		by enrolid end;
		format   %DO c=1 %TO &numProcCov; %do w=1 %to &nwindows.;  bl_&&windows&w.._&&procCovar&c.. %end; %end; 
		%DO c=1 %TO &numdxCov;  %do w=1 %to &nwindows.;  bl_&&windows&w.._&&dxCovar&c.. %end; %end; 
			%do w=1 %to &nwindows.;  bl_&&windows&w.._inpt %end; 3.;

		*Sum across covariate variables that have multiple code types in definition;
		%DO c=1 %TO &numdxCov;  
			bl_30_&&dxCovar&c..=max(&&dxCovar&c.._30_dx, &&dxCovar&c.._30_proc,0);
			label bl_30_&&dxCovar&c.. = "Binary Indicator for  &&dxCovar&c. in 30 days prior to Disenrollment";
			%do w=2 %to &nwindows.;  
				%let last_w=%eval(&w.-1);
				bl_&&windows&w.._&&dxCovar&c..=max(&&dxCovar&c.._&&windows&w.._dx, &&dxCovar&c.._&&windows&w.._proc,0);
				label bl_&&windows&w.._&&dxCovar&c.. = "Binary Indicator for &&dxCovar&c. in &&windows&last_w..-&&windows&w.. days prior to Disenrollment";
			%end;
		%END;

		%DO c=1 %TO &numProcCov;  
			bl_30_&&procCovar&c..=max(&&procCovar&c.._30_dx, &&procCovar&c.._30_proc,0);
			label bl_30_&&procCovar&c.. = "Binary Indicator for  &&procCovar&c. in 30 days prior to Disenrollment";
			%do w=2 %to &nwindows.;  
				%let last_w=%eval(&w.-1);
					bl_&&windows&w.._&&procCovar&c..=max(&&procCovar&c.._&&windows&w.._dx, &&procCovar&c.._&&windows&w.._proc,0);
					label bl_&&windows&w.._&&procCovar&c.. = "Binary Indicator for &&procCovar&c. in &&windows&last_w..-&&windows&w.. days prior to Disenrollment";
				%end;
		%END;

		bl_30_inpt=max(inpt_30, 0);
		label bl_30_inpt="Binary Indicator for inpatient admission in the 30 days prior to Disenrollment ";
		%do w=2 %to &nwindows.;  
			%let last_w=%eval(&w.-1);
			bl_&&windows&w.._inpt=max(inpt_&&windows&w.., 0);
			label bl_&&windows&w.._inpt="Binary Indicator for inpatient admissions in the &&windows&last_w..-&&windows&w.. days prior to Disenrollment ";
		%end;
		*For baseline covariates, keep only the combined definitions;
		drop
		%DO c=1 %TO &numProcCov; &&procCovar&c..: %END;
		%DO c=1 %TO &numdxCov; &&dxCovar&c..: %END;
		inpt_:
		bdayflag
		day
		;

	run;
	
%mend;
%bl_covs;
%define_vars;
%macarray(events, &event_list.);
%macarray(ages, 0 18 25 35 45 55 65 75 85);
%macarray(windows, 30 91 182 365);

%macro cohort_covs;	
	data out.disenroll_cohort(drop = month_end year_end day_end);
		set out.cohort_covs(rename=(end=disenroll_date));
		male=0; female=0;
		label sex= "Patient Sex";
		label male="Male Sex";
		label female="Female Sex";
		if sex="1" then male=1;
		if sex="2" then female=1;
		label emprel = "Relation to Employee";
		label Employee="Employee (emprel)";
		label Spouse = "Spouse (emprel)";
		label Child_Other= "Child/Other (emprel)";
		label Unknown_Dependent= "Dependent-Relation Unknown (emprel)";
		Employee=0; Spouse=0; Child_Other=0; Unknown_Dependent=0;
		if emprel=1 then Employee=1;
		else if emprel=2 then Spouse=1;
		else if emprel=3 then Child_Other=1;
		else if emprel=4 then Unknown_Dependent=1;
		year_birth=year(bday);
		label year_birth="Year of Birth";
		label start ="Start Date of Continuous Enrollment";
		label disenroll_date ="End Date of Continuous Enrollment";
		label ccae="Flag for CCAE Insurance";
		label mdcr="Flag for Medicare Supplmental Insurance";
		age=floor(yrdif(bday,disenroll_date,'age'));
		label age="Age in Years at Disenrollment";
		format age_category 3.;
		age_category=.;
		%do l=1 %to %eval(&nages.-1);
			%let u=%eval(&l.+1);
			%let lage=&&ages&l..;
			%let uage=&&ages&u..;
			format age_&lage._&uage. 3.;
			age_&lage._&uage.=0;
			if &lage.<=age<&uage. then do; 
				age_&lage._&uage.=1;
				age_category=&l.;
			end;
			label age_&lage._&uage. = "&lage. <= Age < &uage.";
		%end;
		label age_category="Age Category";
		age_&&ages&nages.=0;
		if age>=&&ages&nages. then do; age_&&ages&nages.=1; age_category=&nages.; end;
		label age_&&ages&nages. = "Age >= &&ages&nages.";
		%do e=1 %to &nevents.;
			if missing(&&events&e..) then &&events&e..=0;
			if &&events&e..>0 then &&Events&e..=1;
		%end;
	 	label bl_30_ER = "Binary Indicator for ER Visit in the 30 days prior to Disenrollment";
	 	label bl_91_ER = "Binary Indicator for ER Visit in the 30 - 91 days prior to Disenrollment";
	 	label bl_182_ER = "Binary Indicator for ER Visit in the 91 - 182 days prior to Disenrollment";
	 	label bl_365_ER = "Binary Indicator for ER Visit in the 182 - 365 days prior to Disenrollment";
	 	label bl_30_hsp = "Binary Indicator for Hospice Visit in the 30 days prior to Disenrollment";
	 	label bl_91_hsp = "Binary Indicator for Hospice Visit in the 30 - 91 days prior to Disenrollment";
	 	label bl_182_hsp = "Binary Indicator for Hospice Visit in the 91 - 182 days prior to Disenrollment";
	 	label bl_365_hsp = "Binary Indicator for Hospice Visit in the 182 - 365 days prior to Disenrollment";
	run;
%mend;
%cohort_covs;

/*STEP 3: TRANSFER DATA TO TRUVEN: Truven adds binary indicator for death after linkage to SSA*/

/*********************DATA PREP TO BE DONE BY  TRUVEN************************************/
/**		I. Subset cohort to patients with SSI linkage             						*/
/**		II. Merge with SSDI to add a variable for death date         					*/
/**		III. Create a flag indicating death                    							*/
/**			 within 30 days of disenrollment                   							*/



/*********************Create_Summary Macro Specifications********************************/
/**  Parameters:	input_path 				= Directory location of input dataset 		*/
/**				    																	*/
/**				   	input_dsn  				= Name of input dataset						*/
/**																						*/
/**					output_path				= Directory where results will be output    */
/**				    																	*/
/**					death_date				= Name of variable containing date			*/
/**											  of death									*/
/**																						*/
/**					death_var				= Name of the variable indicating death		*/
/**                                                                  					*/
/**					create_table_1			= Binary switch - set to 1 to run code 		*/
/**											  creating Table 1 summary 					*/
/**																						*/
/**					summarize_death_dates	= Binary switch - set to 1 to run			*/
/**											  code to create histograms 				*/
/**											  describing time between death and			*/
/**											  disenrollment								*/
/**																						*/		
/**					run_algorithms 			= Binary switch - set to 1 to run code	 	*/
/**											  proc logistic code predicting death		*/
/*	Example call:																		*/
/*		%create_summary (																*/
/*			input_path=  /mnt/files/projects/marketscanccae/OOHDeath/data/random1pct, 	*/
/*          input_dsn=   disenroll_cohort, 												*/
/*          output_path= /mnt/files/projects/marketscanccae/OOHDeath/output,			*/
/*          death_date=  death_date,													*/	
/*          death_var=   death_flag,													*/
/*																						*/	
/*			/*switch variables - turn on / off steps in the program*/					*/
/*          create_table_1=        1, 													*/
/*          summarize_death_dates= 1,													*/
/*          run_algorithms=        1													*/
/*          );																			*/
/*																						*/
/***********************************************************************************	*/;

/*STEP 4: SUMMARIZE DEATH DATES AND RUN PREDICTIVE MODELS*/

%macro create_summary (input_path=, 
                  input_dsn=,
                  output_path=,
                  summarize_death_dates=,
				  cov_list=&covariates.,
                  create_table_1=,
                  run_algorithms=,
                  death_date=,
                  death_var=,
				  plist=);

   	*create libnames;
   	libname in "&input_path.";
   	libname out "&output_path.";

   	ods listing gpath = "&output_path" sge=on;

   	*define covariates of interest;
	%define_vars;

	*create a more parsimonious list of predictors combining timewindows;
	data &input_dsn.;
		set in.&input_dsn.;
			bl_0_91_ER=max(bl_30_ER,bl_91_ER);
	   		bl_91_365_ER=max(bl_182_ER,bl_365_ER);
			bl_0_91_hsp=max(bl_30_hsp,bl_91_hsp);
			bl_91_365_hsp=max(bl_182_hsp,bl_365_hsp);
			bl_0_365_AMBLIFESUPPORT=max(bl_30_AMBLIFESUPPORT,bl_91_AMBLIFESUPPORT,bl_182_AMBLIFESUPPORT,bl_365_AMBLIFESUPPORT);
			bl_0_365_CANCER=max(bl_30_CANCER,bl_91_CANCER,bl_182_CANCER,bl_365_CANCER);
			bl_0_365_ECHOCARDIOGRAPH=max(bl_30_ECHOCARDIOGRAPH,bl_91_ECHOCARDIOGRAPH,bl_182_ECHOCARDIOGRAPH,bl_365_ECHOCARDIOGRAPH);
			bl_0_91_FLUSHOT=max(bl_30_FLUSHOT,bl_91_FLUSHOT);
			bl_91_365_FLUSHOT=max(bl_182_FLUSHOT,bl_365_FLUSHOT);
			bl_0_365_HOMEHOSPBED=max(bl_30_HOMEHOSPBED,bl_91_HOMEHOSPBED,bl_182_HOMEHOSPBED,bl_365_HOMEHOSPBED);
			bl_0_365_HOMEOXYGEN=max(bl_30_HOMEOXYGEN, bl_91_HOMEOXYGEN, bl_182_HOMEOXYGEN, bl_365_HOMEOXYGEN);
			bl_0_365_MAMMOGRAM=max( bl_30_MAMMOGRAM,bl_91_MAMMOGRAM,bl_182_MAMMOGRAM,bl_365_MAMMOGRAM );              
			bl_0_365_PROSTATE=max(bl_30_PROSTATE,bl_91_PROSTATE,bl_182_PROSTATE,bl_365_PROSTATE);
			bl_0_365_AKI=max(bl_30_AKI, bl_91_AKI, bl_182_AKI, bl_365_AKI);
			bl_0_365_ALCOHOL=max(bl_30_ALCOHOL, bl_91_ALCOHOL, bl_182_ALCOHOL, bl_365_ALCOHOL);
			bl_0_365_ANXIETY=max(bl_30_ANXIETY, bl_91_ANXIETY, bl_182_ANXIETY, bl_365_ANXIETY);
			bl_0_365_CANCERSCREEN=max(bl_30_CANCERSCREEN, bl_91_CANCERSCREEN, bl_182_CANCERSCREEN, bl_365_CANCERSCREEN);
			bl_0_365_CHRONICLIVERDIS=max(bl_30_CHRONICLIVERDIS, bl_91_CHRONICLIVERDIS, bl_182_CHRONICLIVERDIS, bl_365_CHRONICLIVERDIS);
			bl_0_365_COPD=max(bl_30_COPD, bl_91_COPD, bl_182_COPD, bl_365_COPD);
			bl_0_365_DEMENTIA=max(bl_30_DEMENTIA, bl_91_DEMENTIA, bl_182_DEMENTIA, bl_365_DEMENTIA);
			bl_0_365_DEPRESSION=max(bl_30_DEPRESSION, bl_91_DEPRESSION, bl_182_DEPRESSION, bl_365_DEPRESSION);
			bl_0_365_DIABETES=max(bl_30_DIABETES, bl_91_DIABETES, bl_182_DIABETES, bl_365_DIABETES);
			bl_0_365_DMCOMP=max(bl_30_DMCOMP, bl_91_DMCOMP, bl_182_DMCOMP, bl_365_DMCOMP);
			bl_0_365_DRUGDEPENDENCE=max(bl_30_DRUGDEPENDENCE, bl_91_DRUGDEPENDENCE, bl_182_DRUGDEPENDENCE, bl_365_DRUGDEPENDENCE);
			bl_0_365_HEARTFAILFRAILTY=max(bl_30_HEARTFAILFRAILTY, bl_91_HEARTFAILFRAILTY, bl_182_HEARTFAILFRAILTY, bl_365_HEARTFAILFRAILTY);
			bl_0_365_HIPFRACTURE=max(bl_30_HIPFRACTURE, bl_91_HIPFRACTURE, bl_182_HIPFRACTURE, bl_365_HIPFRACTURE);
			bl_0_365_HYPERTENSION=max(bl_30_HYPERTENSION, bl_91_HYPERTENSION, bl_182_HYPERTENSION, bl_365_HYPERTENSION);
			bl_0_365_MI =max(bl_30_MI, bl_91_MI, bl_182_MI, bl_365_MI);
			bl_0_365_PNEUMONIA=max(bl_30_PNEUMONIA, bl_91_PNEUMONIA, bl_182_PNEUMONIA, bl_365_PNEUMONIA);
			bl_0_365_PVD=max(bl_30_PVD, bl_91_PVD, bl_182_PVD, bl_365_PVD);
			bl_0_365_STROKE=max(bl_30_STROKE, bl_91_STROKE, bl_182_STROKE, bl_365_STROKE);
			bl_0_365_SUBABUSE=max(bl_30_SUBABUSE, bl_91_SUBABUSE, bl_182_SUBABUSE, bl_365_SUBABUSE);
			bl_0_365_TOBACCO=max(bl_30_TOBACCO, bl_91_TOBACCO, bl_182_TOBACCO, bl_365_TOBACCO);
			bl_0_365_WEAKNESS=max(bl_30_WEAKNESS, bl_91_WEAKNESS, bl_182_WEAKNESS, bl_365_WEAKNESS);
			bl_0_365_inpt=max(bl_30_inpt, bl_91_inpt, bl_182_inpt, bl_365_inpt);
	run;              

   %if &summarize_death_dates. %then %do;
      data death_service_time;
         set &input_dsn. (keep=&death_date. &death_var. disenroll_date final_svcdate);
         death_diff=disenroll_date-&death_date.;
         svc_diff=disenroll_date-final_svcdate;
      run;
      
      goptions device=png300;
      ods rtf file="&output_path./histograms.rtf";
      proc univariate data=death_service_time;
            var death_diff svc_diff;
            histogram/ midpoints=0 to 1 by 0.1;
         run;
      ods rtf close;

   %end;
   %if &run_algorithms. %then %do;
	  %macarray(percentiles, &plist);

      goptions device=pdfc;
      ods pdf file="&output_path./ROC_Curves.pdf" pdftoc=1;

         Title "Model on CCAE Cohort";
         ods proclabel="CCAE";
         ods graphics / imagename="CCAE" reset=index;
         ods output parameterEstimates=out.CCAE_estimates;
         proc logistic data=&input_dsn.(where=(ccae=1)) descending plots(only)=(roc);
            model &death_var. = age age*age male &cov_list./ scale=none clparm=wald;    
			output out=out.CCAE_pred p=pred lower=lcl upper=ucl;
         run;

         Title "Model on MDCR Cohort";
         ods graphics / imagename="MDCR" reset=index;
         ods proclabel="MDCR";
         ods output parameterEstimates=out.MDCR_estimates;
         proc logistic data=&input_dsn.(where=(ccae=0)) descending plots(only)=(roc);
            model &death_var. = age age*age male &cov_list./ scale=none clparm=wald;
			output out=out.MDCR_pred p=pred lower=lcl upper=ucl;
         run;

      ods graphics off;
      ods pdf close; 
      ods listing;

		*summarize predicted values;
		*ccae population;
		proc univariate data=out.ccae_pred;
			var pred;
			output out=ccae_dist
			pctlpre=p_
			pctlpts= &plist.;
		run;

		data _null_;
			set ccae_dist;
			%do p=1 %to &npercentiles;
				%let percentile=&&percentiles&p..;
				call symputx("p_&percentile.", p_&percentile.);
			%end;
		run;

		*sort data by deciles of p-score;
		data ccae_pred;
			set out.ccae_pred;
			%do p=1 %to %eval(&npercentiles.);
				%let percentile=&&percentiles&p..;
				%let l_p = %eval(&p.-1);
				%let l_percentile=&&percentiles&l_p..;
				%if &p.=1 %then %do;
					if pred < &&p_&percentile.. then group=&p.;
				%end; %* p=1 loop;
				%else %do;
					else if &&p_&l_percentile.. <= pred < &&p_&percentile.. then group=&p.;
				%end;
			%end;
			else if pred>=&&&&p_&&percentiles&npercentiles... then group=%eval(&npercentiles.+1);
		run;

		proc freq data=ccae_pred;
			tables group/out=ccae_denom;
		run;
		proc freq data=ccae_pred (where=(&death_var.=1));
			tables group/out=ccae_numer;
		run;

		data out.ccae_pred_summary;
			merge ccae_numer(rename=(count=death)drop=percent in=in_num) 
				  ccae_denom(rename=(count=total) drop=percent);
			by group;
			format group_desc $72.;
			if in_num=0 then death=0;
			if group=1 then group_desc="Pred Prob < percentile & percentile.";
			else if group=2 then group_desc="Pred Prob &l_percentile..< percentile < & percentile.";
			%do p=1 %to %eval(&npercentiles.);
				%let percentile=&&percentiles&p..;
				%let l_p = %eval(&p.-1);
				%let l_percentile=&&percentiles&l_p..;
				%if &p.=1 %then %do;
					if group=&p. then group_desc="Pred Prob < percentile &percentile.";  
				%end; %* p=1 loop;
				%else %do;
					else if group=&p. then group_desc=" Percentile &l_percentile.<= Pred prob < Percentile &percentile.";
				%end;
			%end;
			else if group=%eval(&npercentiles.+1) then group_desc="Pred Prob > percentile &&percentiles&npercentiles..";
			p_death=death/total;
		run;

		*mdcr population;
		proc univariate data=out.mdcr_pred;
			var pred;
			output out=mdcr_dist
			pctlpre=p_
			pctlpts= &plist.;
		run;

		data _null_;
			set mdcr_dist;
			%do p=1 %to &npercentiles;
				%let percentile=&&percentiles&p..;
				call symputx("p_&percentile.", p_&percentile.);
			%end;
		run;

		*sort data by deciles of p-score;
		data mdcr_pred;
			set out.mdcr_pred;
			%do p=1 %to %eval(&npercentiles.);
				%let percentile=&&percentiles&p..;
				%let l_p = %eval(&p.-1);
				%let l_percentile=&&percentiles&l_p..;
				%if &p.=1 %then %do;
					if pred < &&p_&percentile.. then group=&p.;
				%end; %* p=1 loop;
				%else %do;
					else if &&p_&l_percentile.. <= pred < &&p_&percentile.. then group=&p.;
				%end;
			%end;
			else if pred>=&&&&p_&&percentiles&npercentiles... then group=%eval(&npercentiles.+1);
		run;

		proc freq data=mdcr_pred;
			tables group/out=mdcr_denom;
		run;
		proc freq data=mdcr_pred (where=(&death_var.=1));
			tables group/out=mdcr_numer;
		run;

		data out.mdcr_pred_summary;
			merge mdcr_numer(rename=(count=death)drop=percent in=in_num) 
				  mdcr_denom(rename=(count=total) drop=percent);
			by group;
			format group_desc $72.;
			if in_num=0 then death=0;
			if group=1 then group_desc="Pred Prob < percentile & percentile.";
			else if group=2 then group_desc="Pred Prob &l_percentile..< percentile < & percentile.";
			%do p=1 %to %eval(&npercentiles.);
				%let percentile=&&percentiles&p..;
				%let l_p = %eval(&p.-1);
				%let l_percentile=&&percentiles&l_p..;
				%if &p.=1 %then %do;
					if group=&p. then group_desc="Pred Prob < percentile &percentile.";  
				%end; %* p=1 loop;
				%else %do;
					else if group=&p. then group_desc=" Percentile &l_percentile.<= Pred prob < Percentile &percentile.";
				%end;
			%end;
			else if group=%eval(&npercentiles.+1) then group_desc="Pred Prob > percentile &&percentiles&npercentiles..";
			p_death=death/total;
		run;

		ods rtf file="&output_path./Prediction_Summary.rtf" ;
			Title "CCAE Distribution";
			proc print data=ccae_dist;
			run;

			Title "CCAE Predictions";
			proc print data=out.ccae_pred_summary;
				var group group_Desc death total p_death;
			run;

			Title "MDCR Distribution";
			proc print data=MDCR_dist;
			run;

			Title "MDCR Predictions";
			proc print data=out.mdcr_pred_summary;
				var group group_Desc death total p_death;
			run;
	    ods graphics off;
	    ods rtf close; 
	    ods listing;

   %end; %*run algorithm code;
%mend;

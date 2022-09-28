/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, September 27, 2022     TIME: 8:37:01 PM
PROJECT: NC DETECT 2022
PROJECT PATH: Z:\NC DETECT\NC DETECT 2022.egp
---------------------------------------- */

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend _sas_pushchartsize;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend _sas_popchartsize;


%*--------------------------------------------------------------*
 * Tests the current version against a required version. A      *
 * negative result means that the SAS server version is less    *
 * than the version required.  A positive result means that     *
 * the SAS server version is greater than the version required. *
 * A result of zero indicates that the SAS server is exactly    *
 * the version required.                                        *
 *                                                              *
 * NOTE: The parameter maint is optional.                       *
 *--------------------------------------------------------------*;
%macro _SAS_VERCOMP(major, minor, maint);
    %_SAS_VERCOMP_FV(&major, &minor, &maint, &major, &minor, &maint)
%mend _SAS_VERCOMP;

%*--------------------------------------------------------------*
 * Tests the current version against either the required        *
 * foundation or Viya required version depending on whether the *
 * SYSVLONG version is a foundation or Viya one. A negative     *
 * result means that the SAS server version is less than the    *
 * version required.  A positive result means that the SAS      *
 * server version is greater than the version required. A       *
 * result of zero indicates that the SAS server is exactly the  *
 * version required.                                            *
 *                                                              *
 * NOTE: The *maint parameters are optional.                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCOMP_FV(fmajor, fminor, fmaint, vmajor, vminor, vmaint);
    %local major;
    %local minor;
    %local maint;
    %local CurMaj;
    %local CurMin;
    %local CurMnt;

    %* Pull the current version string apart.;
    %let CurMaj = %scan(&sysvlong, 1, %str(.));

    %* The Viya version number has a V on the front which means
       we need to adjust the Maint SCAN funtion index and also
       get the appropriate parameters for the major, minor, and
       maint values we need to check against (foundation or Viya);
    %if %eval(&CurMaj EQ V) %then
        %do;
		   %*   MM mm t           MM = Major version , mm = Minor version , t = Maint version ;
		   %* V.03.04M2P07112018 ;

            %let major = &vmajor;
            %let minor = &vminor;
            %let maint = &vmaint;
			%let CurMaj = %scan(&sysvlong, 2, %str(.));
			%* Index is purposely 2 because V is now one of the scan delimiters ;
			%let CurMin = %scan(&sysvlong, 2, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
			%let CurMnt = %scan(&sysvlong, 3, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
        %end;
    %else
        %do;
		    %* M mm    t           M = Major version , mm = Minor version , t = Maint version ;  
		    %* 9.01.02M0P11212005 ;

            %let major = &fmajor;
            %let minor = &fminor;
            %let maint = &fmaint;
			%let CurMin = %scan(&sysvlong, 2, %str(.));
			%let CurMnt = %scan(&sysvlong, 4, %str(.ABCDEFGHIKLMNOPQRSTUVWXYZ));
        %end;

    %* Now perform the version comparison.;
    %if %eval(&major NE &CurMaj) %then
        %eval(&CurMaj - &major);
    %else
        %if %eval(&minor NE &CurMin) %then
            %eval(&CurMin - &minor);
        %else
            %if "&maint" = "" %then
                %str(0);
            %else
                %eval(&CurMnt - &maint);
%mend _SAS_VERCOMP_FV;

%*--------------------------------------------------------------*
 * This macro calls _SAS_VERCONDCODE_FV() with the passed       *
 * version. If the current server version matches or is newer,  *
 * then the true code (tcode) is executed, else the false code  *
 * (fcode) is executed.                                         *
 * Example:                                                     *
 *  %let isV92 =                                                *
 *     %_SAS_VERCONDCODE(9,2,0,                                 *
 *         tcode=%nrstr(Yes),                                   *
 *         fcode=%nrstr(No))                                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCONDCODE( major, minor, maint, tcode=, fcode= );
    %_SAS_VERCONDCODE_FV( &major, &minor, &maint, &major, &minor, &maint, &tcode, fcode )
%mend _SAS_VERCONDCODE;

%*--------------------------------------------------------------*
 * This macro calls _SAS_VERCOMP_FV() with the passed versions. *
 * If the current server version matches or is newer, then the  *
 * true code (tcode) is executed, else the false code (fcode)   *
 * is executed.                                                 *
 * Example:                                                     *
 *  %let isV92 =                                                *
 *     %_SAS_VERCONDCODE_FV(9,2,0, 3,5,0                        *
 *         tcode=%nrstr(Yes),                                   *
 *         fcode=%nrstr(No))                                    *
 *--------------------------------------------------------------*;
%macro _SAS_VERCONDCODE_FV( fmajor, fminor, fmaint, vmajor, vminor, vmaint, tcode=, fcode= );
    %if %_SAS_VERCOMP_FV(&fmajor, &fminor, &fmaint, &vmajor, &vminor, &vmaint) >= 0 %then
        %do;
        &tcode
        %end;
    %else
        %do;
        &fcode
        %end;
%mend _SAS_VERCONDCODE_FV;

%*--------------------------------------------------------------*
 * Tests the current version to see if it is a Viya version     *
 * number.                                                      *
 * A result of 1 indicates that the SAS server is a Viya        *
 * server.                                                      *
 * A zero result indicates that the server version is not       *
 * that of a Viya server.                                       *
 *--------------------------------------------------------------*;
%macro _SAS_ISVIYA;
    %local Major;

    %* Get the major component of the current version string.;
    %let Major = %scan(&sysvlong, 1, %str(.));

    %* Check if it it V for Viya.;
    %if %eval(&Major EQ V) %then
        %str(1);
    %else
        %str(0);
%mend _SAS_ISVIYA;


ODS PROCTITLE;
OPTIONS DEV=SVG;
GOPTIONS XPIXELS=0 YPIXELS=0;
%macro HTML5AccessibleGraphSupported;
    %if %_SAS_VERCOMP_FV(9,4,4, 0,0,0) >= 0 %then ACCESSIBLE_GRAPH;
%mend;
FILENAME EGHTMLX TEMP;
ODS HTML5(ID=EGHTMLX) FILE=EGHTMLX
    OPTIONS(BITMAP_MODE='INLINE')
    %HTML5AccessibleGraphSupported
    ENCODING='utf-8'
    STYLE=HtmlBlue
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
;

/*   START OF NODE: 2. Cleaning & Analysis   */
%LET _CLIENTTASKLABEL='2. Cleaning & Analysis';
%LET _CLIENTPROCESSFLOWNAME='Process Flow';
%LET _CLIENTPROJECTPATH='Z:\NC DETECT\NC DETECT 2022.egp';
%LET _CLIENTPROJECTPATHHOST='UNC-PF2BDXGN';
%LET _CLIENTPROJECTNAME='NC DETECT 2022.egp';
%LET _SASPROGRAMFILE='';
%LET _SASPROGRAMFILEHOST='';

/*DESCRIPTIVE STATISTICS AND ONE-SAMPLE T-TESTS
update variables to not have "Unknown" so they're not in analysis*/


/*Nickname Libraries*/
LIBNAME wra 'Z:\NC DETECT\WRA';
LIBNAME preg 'Z:\NC DETECT\Pregnancy-Associated';
LIBNAME ncdetect 'Z:\NC DETECT\Cleaned Data';

PROC FORMAT; /*Changing format in Pregnancy_Associated & Pregnancy_Related to be Yes/No*/
	VALUE Pregnancy_Associatedf 0="No" 1="Yes";
	RUN;

DATA ncallyears;
	SET wra.ncallyears;
	RUN;

DATA pregallyears;
	SET preg.pregallyears;
	RUN;

DATA ncallyears; /*Create simple disposition variable*/
	LENGTH Dispo_Simple $ 40;
	SET ncallyears;
	IF DispositionText = 'Unknown' THEN Dispo_Simple =" ";
	ELSE IF DispositionText = 'Admitted to Psychiatric Unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to hospital floor bed' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to intensive care unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to intermediate care/telemetry unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to medical intensive care unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Died' THEN Dispo_Simple = 'Died';
	ELSE IF DispositionText = 'Discharged to home or self-care (routine discharge)' THEN Dispo_Simple = 'Discharged';
	ELSE IF DispositionText = 'Transferred/discharged to home under care of a home IV drug therapy provider' THEN Dispo_Simple = 'Discharged';
	ELSE IF DispositionText = 'Transferred/discharged to home under care of certified home care provider/program' THEN Dispo_Simple = 'Transfer to Other Healthcare';
	ELSE IF DispositionText = 'Transferred/discharged to prison or jail' THEN Dispo_Simple = 'Discharged'; 
	ELSE IF DispositionText = 'Left after receiving medical advice against leaving (i.e., left AMA)' THEN Dispo_Simple = 'Left AMA or Without Being Seen';
	ELSE IF DispositionText = 'Left with advice, after triage and before registration' THEN Dispo_Simple = 'Left AMA or Without Being Seen';
	ELSE IF DispositionText = 'Left without advice, before triage and registration' THEN Dispo_Simple = 'Left AMA or Without Being Seen';
	ELSE IF DispositionText = 'Left without receiving medical advice against leaving (includes left without being seen, eloped)' THEN Dispo_Simple = 'Left AMA or Without Being Seen';
	ELSE IF DispositionText = 'Other' THEN Dispo_Simple = 'Other';
	ELSE IF DispositionText = 'Placed in designated observation unit (not considered an inpatient hospital admission)' THEN Dispo_Simple = 'Observation';
	ELSE IF DispositionText = 'Transferred/discharged to another short-term general hospital' THEN Dispo_Simple = 'Transfer to Another Hospital';
	ELSE IF DispositionText = 'Transferred/discharged to another type of institution' THEN Dispo_Simple = 'Transfer to Other Healthcare';
	ELSE IF DispositionText = 'Transferred/discharged to institution other than a prison or jail' THEN Dispo_Simple = 'Transfer to Other Healthcare';
	ELSE IF DispositionText = 'Transferred/discharged to intermediate care facility' THEN Dispo_Simple = 'Transfer to Other Healthcare';
	ELSE IF DispositionText = 'Transferred/discharged to skilled nursing facility' THEN Dispo_Simple = 'Transfer to Other Healthcare';
	RUN;
DATA pregallyears; /*Create simple disposition variable*/
	LENGTH Dispo_Simple $ 40;
	SET pregallyears;
	IF DispositionText = 'Unknown' THEN Dispo_Simple =" ";
	ELSE IF DispositionText = 'Admitted to Psychiatric Unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to hospital floor bed' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to intensive care unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to intermediate care/telemetry unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to medical intensive care unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Died' THEN Dispo_Simple = 'Died';
	ELSE IF DispositionText = 'Discharged to home or self-care (routine discharge)' THEN Dispo_Simple = 'Discharged';
	ELSE IF DispositionText = 'Transferred/discharged to home under care of a home IV drug therapy provider' THEN Dispo_Simple = 'Discharged';
	ELSE IF DispositionText = 'Transferred/discharged to home under care of certified home care provider/program' THEN Dispo_Simple = 'Transfer to Other Healthcare';
	ELSE IF DispositionText = 'Transferred/discharged to prison or jail' THEN Dispo_Simple = 'Discharged'; 
	ELSE IF DispositionText = 'Left after receiving medical advice against leaving (i.e., left AMA)' THEN Dispo_Simple = 'Left AMA or Without Being Seen';
	ELSE IF DispositionText = 'Left with advice, after triage and before registration' THEN Dispo_Simple = 'Left AMA or Without Being Seen';
	ELSE IF DispositionText = 'Left without advice, before triage and registration' THEN Dispo_Simple = 'Left AMA or Without Being Seen';
	ELSE IF DispositionText = 'Left without receiving medical advice against leaving (includes left without being seen, eloped)' THEN Dispo_Simple = 'Left AMA or Without Being Seen';
	ELSE IF DispositionText = 'Other' THEN Dispo_Simple = 'Other';
	ELSE IF DispositionText = 'Placed in designated observation unit (not considered an inpatient hospital admission)' THEN Dispo_Simple = 'Observation';
	ELSE IF DispositionText = 'Transferred/discharged to another short-term general hospital' THEN Dispo_Simple = 'Transfer to Another Hospital';
	ELSE IF DispositionText = 'Transferred/discharged to another type of institution' THEN Dispo_Simple = 'Transfer to Other Healthcare';
	ELSE IF DispositionText = 'Transferred/discharged to institution other than a prison or jail' THEN Dispo_Simple = 'Transfer to Other Healthcare';
	ELSE IF DispositionText = 'Transferred/discharged to intermediate care facility' THEN Dispo_Simple = 'Transfer to Other Healthcare';
	ELSE IF DispositionText = 'Transferred/discharged to skilled nursing facility' THEN Dispo_Simple = 'Transfer to Other Healthcare';
	RUN;

/*RACE*/
DATA ncallyears; /*Excluding Unknowns from Race, Combining Asian & Pacific Islander*/
	LENGTH Race_Nounknowns $ 30;
	SET ncallyears;
	IF RaceDescription = 'Unknown' THEN Race_Nounknowns =" ";
	ELSE IF RaceDescription = 'African' THEN Race_Nounknowns = 'Black or African American';
	ELSE IF RaceDescription = 'American Indian' THEN Race_Nounknowns = 'American Indian';
	ELSE IF RaceDescription = 'Asian' THEN Race_Nounknowns = 'Asian & Pacific Islander';
	ELSE IF RaceDescription = 'Asian or Pacific' THEN Race_Nounknowns = 'Asian & Pacific Islander';
	ELSE IF RaceDescription = 'Black' THEN Race_Nounknowns = 'Black or African American';
	ELSE IF RaceDescription = 'Black or African' THEN Race_Nounknowns = 'Black or African American';
	ELSE IF RaceDescription = 'European' THEN Race_Nounknowns = 'Other';
	ELSE IF RaceDescription = 'Hispanic or Lati' THEN Race_Nounknowns = 'Other';
	ELSE IF RaceDescription = 'Middle Eastern o' THEN Race_Nounknowns = 'Other';
	ELSE IF RaceDescription = 'Other' THEN Race_Nounknowns = 'Other';
	ELSE IF RaceDescription = 'Pacific Islander' THEN Race_Nounknowns = 'Asian & Pacific Islander';
	ELSE IF RaceDescription = 'White' THEN Race_Nounknowns = 'White';
	RUN;
DATA pregallyears; /*Excluding Unknowns from Race, Combining Asian & Pacific Islander*/
	LENGTH Race_Nounknowns $ 30;
	SET pregallyears;
	IF RaceDescription = 'Unknown' THEN Race_Nounknowns =" ";
	ELSE IF RaceDescription = 'African' THEN Race_Nounknowns = 'Black or African American';
	ELSE IF RaceDescription = 'American Indian' THEN Race_Nounknowns = 'American Indian';
	ELSE IF RaceDescription = 'Asian' THEN Race_Nounknowns = 'Asian & Pacific Islander';
	ELSE IF RaceDescription = 'Asian or Pacific' THEN Race_Nounknowns = 'Asian & Pacific Islander';
	ELSE IF RaceDescription = 'Black' THEN Race_Nounknowns = 'Black or African American';
	ELSE IF RaceDescription = 'Black or African' THEN Race_Nounknowns = 'Black or African American';
	ELSE IF RaceDescription = 'European' THEN Race_Nounknowns = 'Other';
	ELSE IF RaceDescription = 'Hispanic or Lati' THEN Race_Nounknowns = 'Other';
	ELSE IF RaceDescription = 'Middle Eastern o' THEN Race_Nounknowns = 'Other';
	ELSE IF RaceDescription = 'Other' THEN Race_Nounknowns = 'Other';
	ELSE IF RaceDescription = 'Pacific Islander' THEN Race_Nounknowns = 'Asian & Pacific Islander';
	ELSE IF RaceDescription = 'White' THEN Race_Nounknowns = 'White';
	RUN;

/*ETHNICITY*/
DATA ncallyears; /*Excluding <<<2016 values>>> & Unknowns from Ethnicity*/
	LENGTH Ethnicity_Nounknowns $ 24;
	SET ncallyears;
	IF EthnicityDescription = 'Unknown' THEN Ethnicity_Nounknowns =" ";
	ELSE IF EthnicityDescription = 'Hispanic' THEN Ethnicity_Nounknowns = 'Hispanic Origin';
	ELSE IF EthnicityDescription = 'Not of Hispanic Origin' THEN Ethnicity_Nounknowns = 'Not of Hispanic Origin';
	RUN;
DATA pregallyears; /*Excluding <<<2016 values>>> & Unknowns from Ethnicity*/
	LENGTH Ethnicity_Nounknowns $ 24;
	SET pregallyears;
	IF EthnicityDescription = 'Unknown' THEN Ethnicity_Nounknowns =" ";
	ELSE IF EthnicityDescription = 'Hispanic' THEN Ethnicity_Nounknowns = 'Hispanic Origin';
	ELSE IF EthnicityDescription = 'Not of Hispanic Origin' THEN Ethnicity_Nounknowns = 'Not of Hispanic Origin';
	RUN;

/*INSURANCE PAYER*/
DATA ncallyears; /*Excluding Unknowns & combining all non-Medicaid values into 1 non-Medicaid value*/
	LENGTH Insurance_NoUnknowns $ 30;
	SET ncallyears;
	IF InsuranceText = 'Unknown' THEN Insurance_NoUnknowns =" ";
	ELSE IF InsuranceText = 'Insurance Co.' THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = 'Medicaid' THEN Insurance_NoUnknowns = 'Medicaid';
	ELSE IF InsuranceText = 'Medicare' THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = 'No charge' THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = "Other Gov't." THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = 'Self-pay' THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = "Worker's Comp." THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = 'Other' THEN Insurance_NoUnknowns = 'Non-Medicaid';
	RUN;
DATA pregallyears; /*Excluding Unknowns & combining all non-Medicaid values into 1 non-Medicaid value*/
	LENGTH Insurance_NoUnknowns $ 30;
	SET pregallyears;
	IF InsuranceText = 'Unknown' THEN Insurance_NoUnknowns =" ";
	ELSE IF InsuranceText = 'Insurance Co.' THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = 'Medicaid' THEN Insurance_NoUnknowns = 'Medicaid';
	ELSE IF InsuranceText = 'Medicare' THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = 'No charge' THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = "Other Gov't." THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = 'Self-pay' THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = "Worker's Comp." THEN Insurance_NoUnknowns = 'Non-Medicaid';
	ELSE IF InsuranceText = 'Other' THEN Insurance_NoUnknowns = 'Non-Medicaid';
	RUN;

PROC FREQ data=ncAllYears; /*TABLE 1. WRA Proportions and Counts of Demographics by Year*/
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
/*	TABLES RaceDescription / Missing;
	TABLES EthnicityDescription / Missing; */
	TABLES County_Type / Missing;
	TABLES InsuranceText / Missing;
	TABLES Insurance_NoUnknowns / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2016-2021 All ED Visits for WRA";
	RUN;
PROC FREQ data=pregAllYears; /*TABLE 1. Pregnancy-Related Proportions and Counts of Demographics by Year*/
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
/*	TABLES RaceDescription / Missing;
	TABLES EthnicityDescription / Missing;*/
	TABLES County_Type / Missing;
	TABLES InsuranceText / Missing;
	TABLES Insurance_NoUnknowns / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2016-2021 All Pregnancy-Related ED Visits";
	RUN;

DATA ncnot2016; /*evaluating proportions for Race & Ethnicity excluding 2016 values*/
	SET ncAllYears;
	if (Visit_Year > 2016);
	RUN;
DATA pregnot2016;
	SET pregallyears;
	IF (Visit_Year > 2016);
	RUN;

PROC FREQ data=ncnot2016; /*TABLE 1. WRA Proportions and Counts of Demographics by Year*/
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TITLE "2017-2021 All ED Visits for WRA";
	RUN;
PROC FREQ data=pregnot2016; /*TABLE 1. Pregnancy-Related Proportions and Counts of Demographics by Year*/
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TITLE "2017-2021 All Pregnancy-Related ED Visits";
	RUN;

DATA ncnot2016; /*Updating payer values to be more specific than Medicaid and Non-Medicaid*/
	LENGTH Insurance_logreg $ 30;
	SET ncnot2016;
	IF InsuranceText = 'Unknown' THEN Insurance_logreg =" ";
	ELSE IF InsuranceText = 'Insurance Co.' THEN Insurance_logreg = 'Private';
	ELSE IF InsuranceText = 'Medicaid' THEN Insurance_logreg = 'Medicaid';
	ELSE IF InsuranceText = 'Medicare' THEN Insurance_logreg = 'Non-Medicaid Government Payer';
	ELSE IF InsuranceText = 'No charge' THEN Insurance_logreg = 'Other';
	ELSE IF InsuranceText = "Other Gov't." THEN Insurance_logreg = 'Non-Medicaid Government Payer';
	ELSE IF InsuranceText = 'Self-pay' THEN Insurance_logreg = 'Self-pay';
	ELSE IF InsuranceText = "Worker's Comp." THEN Insurance_logreg = 'Other';
	ELSE IF InsuranceText = 'Other' THEN Insurance_logreg = 'Other';
	RUN;
DATA pregallyears; /*Updating payer values to be more specific than Medicaid and Non-Medicaid*/
	LENGTH Insurance_logreg $ 30;
	SET pregallyears;
	IF InsuranceText = 'Unknown' THEN Insurance_logreg =" ";
	ELSE IF InsuranceText = 'Insurance Co.' THEN Insurance_logreg = 'Private';
	ELSE IF InsuranceText = 'Medicaid' THEN Insurance_logreg = 'Medicaid';
	ELSE IF InsuranceText = 'Medicare' THEN Insurance_logreg = 'Non-Medicaid Government Payer';
	ELSE IF InsuranceText = 'No charge' THEN Insurance_logreg = 'Other';
	ELSE IF InsuranceText = "Other Gov't." THEN Insurance_logreg = 'Non-Medicaid Government Payer';
	ELSE IF InsuranceText = 'Self-pay' THEN Insurance_logreg = 'Self-pay';
	ELSE IF InsuranceText = "Worker's Comp." THEN Insurance_logreg = 'Other';
	ELSE IF InsuranceText = 'Other' THEN Insurance_logreg = 'Other';
	RUN;
DATA ncallyears; /*Updating payer values to be more specific than Medicaid and Non-Medicaid*/
	LENGTH Insurance_logreg $ 30;
	SET ncallyears;
	IF InsuranceText = 'Unknown' THEN Insurance_logreg =" ";
	ELSE IF InsuranceText = 'Insurance Co.' THEN Insurance_logreg = 'Private';
	ELSE IF InsuranceText = 'Medicaid' THEN Insurance_logreg = 'Medicaid';
	ELSE IF InsuranceText = 'Medicare' THEN Insurance_logreg = 'Non-Medicaid Government Payer';
	ELSE IF InsuranceText = 'No charge' THEN Insurance_logreg = 'Other';
	ELSE IF InsuranceText = "Other Gov't." THEN Insurance_logreg = 'Non-Medicaid Government Payer';
	ELSE IF InsuranceText = 'Self-pay' THEN Insurance_logreg = 'Self-pay';
	ELSE IF InsuranceText = "Worker's Comp." THEN Insurance_logreg = 'Other';
	ELSE IF InsuranceText = 'Other' THEN Insurance_logreg = 'Other';
	RUN;

/*Creating datasets for individual years*/
DATA nc2016; 
	SET ncAllYears;
	if (Visit_Year = 2016);
	RUN;
	DATA nc2017; 
	SET ncAllYears;
	if (Visit_Year = 2017);
	RUN;
	DATA nc2018; 
	SET ncAllYears;
	if (Visit_Year = 2018);
	RUN;
	DATA nc2019; 
	SET ncAllYears;
	if (Visit_Year = 2019);
	RUN;
	DATA nc2020; 
	SET ncAllYears;
	if (Visit_Year = 2020);
	RUN;
	DATA nc2021; 
	SET ncAllYears;
	if (Visit_Year = 2021);
	RUN;

DATA preg2016; 
	SET pregAllYears;
	if (Visit_Year = 2016);
	RUN;
	DATA preg2017; 
	SET pregAllYears;
	if (Visit_Year = 2017);
	RUN;
	DATA preg2018; 
	SET pregAllYears;
	if (Visit_Year = 2018);
	RUN;
	DATA preg2019; 
	SET pregAllYears;
	if (Visit_Year = 2019);
	RUN;
	DATA preg2020; 
	SET pregAllYears;
	if (Visit_Year = 2020);
	RUN;
	DATA preg2021; 
	SET pregAllYears;
	if (Visit_Year = 2021);
	RUN;

/*Mean age*/
PROC MEANS data=pregallyears N NMISS MAX MIN MEAN STD;
	TITLE 'Mean Age pregnancy-related ED Visits';
	proc means data=preg2016 mean std;
	proc means data=preg2017 mean std;
	proc means data=preg2018 mean std;
	proc means data=preg2019 mean std;
	proc means data=preg2020 mean std;
	proc means data=preg2021 mean std;
	proc means data=pregallyears mean std;

	PROC MEANS data=ncallyears N NMISS MAX MIN MEAN STD;
	TITLE 'Mean Age ED Visits for all WRA';
	proc means data=nc2016 mean std;
	proc means data=nc2017 mean std;
	proc means data=nc2018 mean std;
	proc means data=nc2019 mean std;
	proc means data=nc2020 mean std;
	proc means data=nc2021 mean std;
	proc means data=ncallyears mean std;
	RUN;

/*Calculate proportions for each year*/
PROC FREQ data=preg2016; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2016 Pregnancy-Related";
	RUN;
PROC FREQ data=preg2017; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2017 Pregnancy-Related";
	RUN;
PROC FREQ data=preg2018; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2018 Pregnancy-Related";
	RUN;
PROC FREQ data=preg2019; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2019 Pregnancy-Related";
	RUN;
PROC FREQ data=preg2020; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2020 Pregnancy-Related";
	RUN;
PROC FREQ data=preg2021; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2021 Pregnancy-Related";
	RUN;
PROC FREQ data=nc2016; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2016 All ED Visits for WRA";
	RUN;
PROC FREQ data=nc2017; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2017 All ED Visits for WRA";
	RUN;
PROC FREQ data=nc2018; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2018 All ED Visits for WRA";
	RUN;
PROC FREQ data=nc2019; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2019 All ED Visits for WRA";
	RUN;
PROC FREQ data=nc2020; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2020 All ED Visits for WRA";
	RUN;
PROC FREQ data=nc2021; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2021 All ED Visits for WRA";
	RUN;

/*Check final numbers in spreadsheet*/
PROC FREQ data=pregAllYears; 
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
/*	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;*/
	TABLES County_Type / Missing;
	TABLES Insurance_nounknowns / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2016-2020 All Pregnancy-Related";
	RUN;
PROC FREQ data=pregnot2016;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TITLE "2017-2020 All Pregnancy-Related";
	RUN;
PROC FREQ data=ncAllYears;
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
/*	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;*/
	TABLES County_Type / Missing;
	TABLES Insurance_nounknowns / Missing;
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2016-2020 All ED visits for WRA";
	RUN;
PROC FREQ data=ncnot2016;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TITLE "2017-2020 All ED visits for WRA";
	RUN;


/*Calculate One sample t-tests comparing proportion pregnancy-associated to proportion of population estimated to be pregnant*/
PROC TTEST data=ncAllYears alpha=0.05  h0=0.046 sides=u; /*TABLE B. One Sample T-Test*/
    VAR Pregnancy_Associated;
	TITLE "TABLE A--One Sample T-test 2016-2019"; /*update to include 2020*/
	RUN;
PROC TTEST data=nc2016 alpha=0.05  h0=0.047 sides=u; 
    VAR Pregnancy_Associated;
	TITLE "2016 One Sample T-test";
	RUN;
PROC TTEST data=nc2017 alpha=0.05  h0=0.046 sides=u; 
    VAR Pregnancy_Associated;
	TITLE "2017 One Sample T-test";
	RUN;
PROC TTEST data=nc2018 alpha=0.05  h0=0.046 sides=u; 
    VAR Pregnancy_Associated;
	TITLE "2018 One Sample T-test";
	RUN;
PROC TTEST data=nc2019 alpha=0.05  h0=0.045 sides=u;
    VAR Pregnancy_Associated;
	TITLE "2019 One Sample T-test";
	RUN;

/*Calculate One sample t-tests stratifying demographics for Pregnancy-Associated Visits*/
/*AGE GROUPS*/
PROC FREQ data=pregAllYears;/*15-19*/
	TABLES Age_Group / binomial (p=0.165);
	TITLE 'Age Stratification 2016-2020';
	RUN;
PROC FREQ data=pregAllYears;/*20-24*/
	TABLES Age_Group / binomial (p=0.166 level=2);
	RUN;
PROC FREQ data=pregAllYears;/*25-29*/
	TABLES Age_Group / binomial (p=0.176 level=3);
	RUN;
PROC FREQ data=pregAllYears;/*30-34*/
	TABLES Age_Group / binomial (p=0.167 level=4);
	RUN;
PROC FREQ data=pregAllYears;/*35-39*/
	TABLES Age_Group / binomial (p=0.165 level=5);
	RUN;
PROC FREQ data=pregAllYears;/*40-44*/
	TABLES Age_Group / binomial (p=0.162 level=6);
	RUN;

/*RACE - NOT INCLUDING 2016*/
PROC FREQ data=pregnot2016;/*American Indian*/
	TABLES Race_Nounknowns / binomial (p=0.018);
	TITLE 'Race Stratification 2017-2021';
	RUN;
PROC FREQ data=pregnot2016;/*Asian & Pacific Islander*/
	TABLES Race_Nounknowns / binomial (p=0.044 level=2);
	RUN;
PROC FREQ data=pregnot2016;/*Black*/
	TABLES Race_Nounknowns / binomial (p=0.257 level=3);
	RUN;
/*PROC FREQ data=pregnot2016; Other
	TABLES Race_Nounknowns / binomial (p=0.??? level=4);
	RUN;*/
PROC FREQ data=pregnot2016; /*White*/
	TABLES Race_Nounknowns / binomial (p=0.681 level=5);
	RUN;

/*ETHNICITY - NOT INCLUDING 2016*/
PROC FREQ data=pregnot2016;/*Hispanic*/
	TABLES Ethnicity_Nounknowns / binomial (p=0.109);
	TITLE 'Ethnicity Stratification 2017-2021';
	RUN;
PROC FREQ data=pregnot2016;/*Not of Hispanic Origin*/
	TABLES Ethnicity_Nounknowns / binomial (p=0.892 level=2);
	RUN;

/*TYPE OF COUNTY*/
PROC FREQ data=pregAllYears;/*Rural*/
	TABLES County_Type / binomial (p=0.355);
	TITLE 'Rural';
	RUN;
PROC FREQ data=pregAllYears;/*Suburban*/
	TABLES County_Type / binomial (p=0.252 level=2);
	TITLE 'Suburban';
	RUN;
PROC FREQ data=pregAllYears;/*Urban*/
	TABLES County_Type / binomial (p=0.393 level=3);
	TITLE 'Urban';
	RUN;

/*INSURANCE PAYER*/
PROC FREQ data=pregAllYears;/*Medicaid*/
	TABLES Insurance_NoUnknowns / binomial (p=0.545);
	TITLE 'Medicaid';
	RUN;
PROC FREQ data=pregAllYears;/*Medicaid*/
	TABLES Insurance_NoUnknowns / binomial (p=0.455 level=2);
	TITLE 'Non-Medicaid';
	RUN;

/*DEMOGRAPHICS FOR ADMISSION FROM ED VISIT*/
DATA admittedallyears; 
	SET ncAllYears;
	if (Dispo_Simple = 'Admitted');
	RUN;
DATA pregadmitallyears;
	SET pregallyears;
	if (Dispo_Simple = 'Admitted');
	RUN;

/*count by year visits ending in admission*/
PROC FREQ data=admittedallyears;/*counts ending admission*/
	TABLES Visit_Year;
	TITLE 'All ED visits for WRA ending in admission';
	RUN;
PROC FREQ data=pregadmitallyears;
	TABLES Visit_Year;
	TITLE 'Pregnancy-related ED visits ending in admission';
	RUN;

/*Age group at admission*/
PROC FREQ data=admittedallyears;
	TABLES Age_Group;
	TITLE 'Age for all WRA Admissions 2016-2021';
	RUN;
PROC FREQ data=pregadmitallyears;
	TABLES Age_Group;
	TITLE 'Age for all Pregnancy-Related Admissions 2016-2021';
	RUN;

/*Race at admission*/
PROC FREQ data=admittedallyears;
	TABLES Race_Nounknowns;
	TITLE 'Race for all WRA Admissions 2016-2021';
	RUN;
PROC FREQ data=pregadmitallyears;
	TABLES Race_Nounknowns;
	TITLE 'Race for all Pregnancy-Related Admissions 2016-2021';
	RUN;

/*Ethnicity at admission*/
PROC FREQ data=admittedallyears;
	TABLES Ethnicity_Nounknowns;
	TITLE 'Ethnicity for all WRA Admissions 2016-2021';
	RUN;
PROC FREQ data=pregadmitallyears;
	TABLES Ethnicity_Nounknowns;
	TITLE 'Ethnicity for all Pregnancy-Related Admissions 2016-2021';
	RUN;

/*County at admission*/
PROC FREQ data=admittedallyears;
	TABLES County_Type;
	TITLE 'County for all WRA Admissions 2016-2021';
	RUN;
PROC FREQ data=pregadmitallyears;
	TABLES County_Type;
	TITLE 'County for all Pregnancy-Related Admissions 2016-2021';
	RUN;

/*Payer at admission*/
PROC FREQ data=admittedallyears;
	TABLES Insurance_NoUnknowns;
	TITLE 'Payer for all WRA Admissions 2016-2021';
	RUN;
PROC FREQ data=pregadmitallyears;
	TABLES Insurance_NoUnknowns;
	TITLE 'Payer for all Pregnancy-Related Admissions 2016-2021';
	RUN;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
%LET _SASPROGRAMFILEHOST=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;

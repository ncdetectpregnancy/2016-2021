/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, September 27, 2022     TIME: 8:37:36 PM
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

/*   START OF NODE: 3. AORs and charts   */
%LET _CLIENTTASKLABEL='3. AORs and charts';
%LET _CLIENTPROCESSFLOWNAME='Process Flow';
%LET _CLIENTPROJECTPATH='Z:\NC DETECT\NC DETECT 2022.egp';
%LET _CLIENTPROJECTPATHHOST='UNC-PF2BDXGN';
%LET _CLIENTPROJECTNAME='NC DETECT 2022.egp';
%LET _SASPROGRAMFILE='';
%LET _SASPROGRAMFILEHOST='';

PROC FREQ data=ncallyears; /*TABLES with missing values included in percentages*/
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
/*	TABLES Insurance_NoUnknowns / Missing;*/
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2016-2021 Visits for WRA - Checking for blank values";
	RUN;
PROC FREQ data=pregallyears;
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES Race_Nounknowns / Missing;
	TABLES Ethnicity_Nounknowns / Missing;
	TABLES County_Type / Missing;
/*	TABLES Insurance_NoUnknowns / Missing;*/
	TABLES Insurance_logreg / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE '2016-2021 Pregnancy-Associated Visits - Checking for blank values';
	RUN;

/*ADJUSTED ODDS RATIOS*/

ods graphics on;
/*odds that an ED Visit will be pregnancy-associated (outcome = pregnancy, exposures are the covariates)*/
PROC LOGISTIC data=ncAllYears descending plots(only)=oddsratio (type=horizontalstat);/*Logistic regression with Year covariate, excludes 2016*/
	CLASS Visit_Year(REF='2016') Age_Group(REF="20-24 years") Race_Nounknowns Ethnicity_Nounknowns County_Type Insurance_NoUnknowns(REF="Non-Medicaid");
	MODEL Pregnancy_Associated = Visit_Year Age_Group Race_Nounknowns Ethnicity_Nounknowns County_Type Insurance_NoUnknowns;
	TITLE 'Adjusted Odds Ratios for an ED Visit being pregnancy-associated 2016-2021';
	RUN; 

ods graphics off;	

/*The odds that an ED Visit will result in admission based on whether pregnancy related*/
/*Create admission yes/no variable*/
DATA ncAllYears;
    SET ncAllYears;
	LENGTH Dispo_Admit $ 4;
	IF Dispo_Simple = 'Admitted' THEN Dispo_Admit = 'Yes';
	ELSE IF Dispo_Simple = 'Transfer to Another Hospital' THEN Dispo_Admit = 'Yes';
	ELSE IF (Dispo_Simple ~= 'Admitted' OR 'Transfer to Another Hospital')  THEN Dispo_Admit = 'No';
	RUN;
	PROC FREQ data=ncAllYears;
		TABLES Dispo_Admit;
		TITLE '2017-2020 Disposition All Visits for WRA';
		RUN;
DATA ncallyears_admit;
    SET ncallyears;
	LENGTH Dispo_Admit $ 4;
	IF Dispo_Simple = 'Admitted' THEN Dispo_Admit = 'Yes';
	ELSE IF Dispo_Simple = 'Transfer to Another Hospital' THEN Dispo_Admit = 'Yes';
	ELSE IF (Dispo_Simple ~= 'Admitted' OR 'Transfer to Another Hospital')  THEN Dispo_Admit = 'No';
	RUN;
DATA pregallyears_admit;
    SET pregallyears;
	LENGTH Dispo_Admit $ 4;
	IF Dispo_Simple = 'Admitted' THEN Dispo_Admit = 'Yes';
	ELSE IF Dispo_Simple = 'Transfer to Another Hospital' THEN Dispo_Admit = 'Yes';
	ELSE IF (Dispo_Simple ~= 'Admitted' OR 'Transfer to Another Hospital')  THEN Dispo_Admit = 'No';
	RUN;

/*Analyze the odds that an admission would be pregnancy-associated
	outcome - admission or not
	exposure - pregnancy-associated or not
	covariates - all the other things that were independently associated (so maybe not visit year)*/
ods graphics on;

PROC LOGISTIC data=ncAllYears descending plots(only)=oddsratio;/*Logistic regression for odds that an admission is pregnancy-related, excludes 2016*/
	CLASS Visit_Year(REF='2016') Age_Group(REF="20-24 years") Race_Nounknowns Ethnicity_Nounknowns County_Type Insurance_logreg(REF="Medicaid") Pregnancy_Associated (REF='No');
	MODEL Dispo_Admit = Visit_Year Age_Group Race_Nounknowns Ethnicity_Nounknowns County_Type Insurance_logreg Pregnancy_Associated;
	TITLE "2016-2021 Modeling Admissions from ED";
	RUN;

ods graphics off;	

/*the rest of this doesn't have to be in the code I publish
C
H
A
R
T
S
*/
PROC SGPLOT data = pregallyears pctlevel=graph noautolegend;
	vbar Age_Group / stat=freq;
	TITLE "Pregnancy-Associated ED Visits by Age Group";
	xaxis label='Age Group';
	yaxis grid;
	RUN;
	PROC SGPLOT data = pregNot2016 pctlevel=graph noautolegend;
	vbar Race_NoUnknowns / stat=percent group=Pregnancy_Associated;
	TITLE "Pregnancy-Associated ED Visits by Race (excluding 2016)";
	LABEL Race_NoUnknowns='Patient Race';
	RUN;
	PROC SGPLOT data = pregNot2016 pctlevel=graph noautolegend;
	vbar Ethnicity_NoUnknowns / stat=percent group=Pregnancy_Associated;
	TITLE "Pregnancy-Associated ED Visits by Ethnicity (excluding 2016)";
	LABEL Ethnicity_NoUnknowns='Patient Ethnicity';
	RUN;
	PROC SGPLOT data = pregallyears pctlevel=graph noautolegend;
	vbar County_Type / stat=percent group=Pregnancy_Associated;
	TITLE "Pregnancy-Associated ED Visits by County Type";
	LABEL County_Type='Patient County of Residence';
	RUN;
	PROC SGPLOT data = pregallyears_admit pctlevel=graph noautolegend;
	vbar Dispo_Simple / stat=percent group=Pregnancy_Associated;
	TITLE "Pregnancy-Associated ED Visit Disposition";
	LABEL Dispo_Simple='Disposition';
	RUN;

PROC GCHART data = pregallyears;
	pie Insurance_Nounknowns / type=percent;
	TITLE "Pregnancy-Associated ED Visits by Insurance Payer";
	LABEL Insurance_Nounknowns='Insurance Payer';
	RUN;
PROC GCHART data = pregallyears;
	pie Insurance_logreg / type=percent;
	TITLE "Pregnancy-Associated ED Visits by Insurance Payer";
	LABEL Insurance_logreg='Insurance Payer';
	RUN;
	
	PROC GCHART data=pregallyears_admit;
	pie Dispo_Admit / type=percent;
	TITLE "Pregnancy-Associated ED Visits ending in an Admission 2016-2020";
	RUN;
	PROC GCHART data=ncallyears_admit;
	pie Dispo_Admit / type=percent;
	TITLE "All ED Visits for WRA ending in an Admission 2016-2020";
	RUN;







proc gchart data=pregallyears;
vbar Pregnancy_Associated / discrete type=percent group=Visit_Year subgroup=Insurance_NoUnknowns;
title 'Pregnancy-Associated ED Visits by Visit Insurance Payer';
run;

proc gchart data=pregallyears;
vbar Pregnancy_Associated / discrete group=Insurance_NoUnknowns;
run;
proc freq data=pregallyears;
table Insurance_NoUnknowns / Missing;
run;

proc gchart data=ncallyears;
vbar Pregnancy_Associated / discrete group=County_Type;
run;






proc sgplot data=pregallyears pctlevel=graph ;
	xaxis label='Year';
	yaxis label='Count';
	vbar Visit_Year / Group=Race_Nounknowns;
	run;

proc sgplot data=pregallyears pctlevel=graph ;
	xaxis label='Year';
	yaxis label='Proportion';
	vbar Race_Nounknowns / Group=Insurance_Nounknowns;
	run;

proc sgplot data=pregallyears pctlevel=graph;
	xaxis label='Year';
	yaxis label='Proportion';
	vbar Visit_Year / Group=Age_Group;
	run;






%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTPATHHOST=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
%LET _SASPROGRAMFILEHOST=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;

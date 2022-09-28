/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, September 27, 2022     TIME: 8:34:52 PM
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

/*   START OF NODE: 1. Import all data   */
%LET _CLIENTTASKLABEL='1. Import all data';
%LET _CLIENTPROCESSFLOWNAME='Process Flow';
%LET _CLIENTPROJECTPATH='Z:\NC DETECT\NC DETECT 2022.egp';
%LET _CLIENTPROJECTPATHHOST='UNC-PF2BDXGN';
%LET _CLIENTPROJECTNAME='NC DETECT 2022.egp';
%LET _SASPROGRAMFILE='';
%LET _SASPROGRAMFILEHOST='';

/*Nickname Library*/
LIBNAME ncdetect 'Z:\NC DETECT\Original Data';

DATA nc2016; /*Select File including all 2016 ED visits*/
	INFILE 'Z:\NC DETECT\Original Data\Standard.Dataset.3DigitZIP.2016.txt' FIRSTOBS = 2 delimiter = '|' DSD lrecl=2000 truncover;
	/*Import all variables with proper format*/
	INPUT          
		InternalTrackingID
		Age
		Sex $
		RaceCode $
		RaceDescription :$16.
		EthnicityCode $
		EthnicityDescription :$22.
		County :$50.
		ZIP $
		State $
		VisitID
		InsuranceCode
		InsuranceText :$25.
		ArrivalDate : YYYYMMDD8.
		ArrivalTime :$11.
		TransportCode
		TransportText :$57.
		ChiefComplaint :$200.
		SystolicBP
		DiastolicBP
		InitialEDTemp
		DispositionCode
		DispositionText :$96.
		DispositionDiagDesc :$400.
		Diag1 $
		Diag2 $
		Diag3 $
		Diag4 $
		Diag5 $
		Diag6 $
		Diag7 $
		Diag8 $
		Diag9 $
		Diag10 $
		Diag11 $
		ProcCode1 $
		ProcCode2 $  
		ProcCode3 $  
		ProcCode4 $  
		ProcCode5 $  
		ProcCode6 $  
		ProcCode7 $  
		ProcCode8 $  
		ProcCode9 $  
		ProcCode10 $  
		ProcCode11 $  
		ProcCode12 $  
		ProcCode13 $  
		ProcCode14 $  
		ProcCode15 $  
		ProcCode16 $  
		ProcCode17 $  
		ProcCode18 $  
		ProcCode19 $  
		ProcCode20 $  
		InjuryCode1 $
		InjuryCode2 $
		InjuryCode3 $
		InjuryCode4 $
		InjuryCode5 $;
	RUN;

/*Select File including all 2017 ED visits*/
DATA nc2017;
	INFILE 'Z:\NC DETECT\Original Data\Standard.Dataset.3DigitZIP.2017.txt' FIRSTOBS = 2 delimiter = '|' DSD lrecl=2000 truncover;
	/*Import all variables with proper format*/
	INPUT          
		InternalTrackingID
		Age
		Sex $
		RaceCode $
		RaceDescription :$16.
		EthnicityCode $
		EthnicityDescription :$22.
		County :$50.
		ZIP $
		State $
		VisitID
		InsuranceCode
		InsuranceText :$25.
		ArrivalDate : YYYYMMDD8.
		ArrivalTime :$11.
		TransportCode
		TransportText :$57.
		ChiefComplaint :$200.
		SystolicBP
		DiastolicBP
		InitialEDTemp
		DispositionCode
		DispositionText :$96.
		DispositionDiagDesc :$400.
		Diag1 $
		Diag2 $
		Diag3 $
		Diag4 $
		Diag5 $
		Diag6 $
		Diag7 $
		Diag8 $
		Diag9 $
		Diag10 $
		Diag11 $
		ProcCode1 $
		ProcCode2 $  
		ProcCode3 $  
		ProcCode4 $  
		ProcCode5 $  
		ProcCode6 $  
		ProcCode7 $  
		ProcCode8 $  
		ProcCode9 $  
		ProcCode10 $  
		ProcCode11 $  
		ProcCode12 $  
		ProcCode13 $  
		ProcCode14 $  
		ProcCode15 $  
		ProcCode16 $  
		ProcCode17 $  
		ProcCode18 $  
		ProcCode19 $  
		ProcCode20 $  
		InjuryCode1 $
		InjuryCode2 $
		InjuryCode3 $
		InjuryCode4 $
		InjuryCode5 $;
	RUN;
/*Select File including all 2018 ED visits*/
DATA nc2018;
	INFILE 'Z:\NC DETECT\Original Data\Standard.Dataset.3DigitZIP.2018.txt' FIRSTOBS = 2 delimiter = '|' DSD lrecl=2000 truncover;
	/*Import all variables with proper format*/
	INPUT          
		InternalTrackingID
		Age
		Sex $
		RaceCode $
		RaceDescription :$16.
		EthnicityCode $
		EthnicityDescription :$22.
		County :$50.
		ZIP $
		State $
		VisitID
		InsuranceCode
		InsuranceText :$25.
		ArrivalDate : YYYYMMDD8.
		ArrivalTime :$11.
		TransportCode
		TransportText :$57.
		ChiefComplaint :$200.
		SystolicBP
		DiastolicBP
		InitialEDTemp
		DispositionCode
		DispositionText :$96.
		DispositionDiagDesc :$400.
		Diag1 $
		Diag2 $
		Diag3 $
		Diag4 $
		Diag5 $
		Diag6 $
		Diag7 $
		Diag8 $
		Diag9 $
		Diag10 $
		Diag11 $
		ProcCode1 $
		ProcCode2 $  
		ProcCode3 $  
		ProcCode4 $  
		ProcCode5 $  
		ProcCode6 $  
		ProcCode7 $  
		ProcCode8 $  
		ProcCode9 $  
		ProcCode10 $  
		ProcCode11 $  
		ProcCode12 $  
		ProcCode13 $  
		ProcCode14 $  
		ProcCode15 $  
		ProcCode16 $  
		ProcCode17 $  
		ProcCode18 $  
		ProcCode19 $  
		ProcCode20 $  
		InjuryCode1 $
		InjuryCode2 $
		InjuryCode3 $
		InjuryCode4 $
		InjuryCode5 $;
	RUN;

DATA nc2019;/*Select File including all 2019 ED visits*/
	INFILE 'Z:\NC DETECT\Original Data\Standard.Dataset.3DigitZIP.2019.txt' FIRSTOBS = 2 delimiter = '|' DSD lrecl=2000 truncover;
	/*Import all variables with proper format*/
	INPUT          
		InternalTrackingID
		Age
		Sex $
		RaceCode $
		RaceDescription :$16.
		EthnicityCode $
		EthnicityDescription :$22.
		County :$50.
		ZIP $
		State $
		VisitID
		InsuranceCode
		InsuranceText :$25.
		ArrivalDate : YYYYMMDD8.
		ArrivalTime :$11.
		TransportCode
		TransportText :$57.
		ChiefComplaint :$200.
		SystolicBP
		DiastolicBP
		InitialEDTemp
		DispositionCode
		DispositionText :$96.
		DispositionDiagDesc :$400.
		Diag1 $
		Diag2 $
		Diag3 $
		Diag4 $
		Diag5 $
		Diag6 $
		Diag7 $
		Diag8 $
		Diag9 $
		Diag10 $
		Diag11 $
		ProcCode1 $
		ProcCode2 $  
		ProcCode3 $  
		ProcCode4 $  
		ProcCode5 $  
		ProcCode6 $  
		ProcCode7 $  
		ProcCode8 $  
		ProcCode9 $  
		ProcCode10 $  
		ProcCode11 $  
		ProcCode12 $  
		ProcCode13 $  
		ProcCode14 $  
		ProcCode15 $  
		ProcCode16 $  
		ProcCode17 $  
		ProcCode18 $  
		ProcCode19 $  
		ProcCode20 $  
		InjuryCode1 $
		InjuryCode2 $
		InjuryCode3 $
		InjuryCode4 $
		InjuryCode5 $;
	RUN;
/*Select File including all 2020 ED visits*/
DATA nc2020;
	INFILE 'Z:\NC DETECT\Original Data\Standard.Dataset.3DigitZIP.2020.txt' FIRSTOBS = 2 delimiter = '|' DSD lrecl=2000 truncover;
	/*Import all variables with proper format*/
	INPUT          
		InternalTrackingID
		Age
		Sex $
		RaceCode $
		RaceDescription :$16.
		EthnicityCode $
		EthnicityDescription :$22.
		County :$50.
		ZIP $
		State $
		VisitID
		InsuranceCode
		InsuranceText :$25.
		ArrivalDate : YYYYMMDD8.
		ArrivalTime :$11.
		TransportCode
		TransportText :$57.
		ChiefComplaint :$200.
		SystolicBP
		DiastolicBP
		InitialEDTemp
		DispositionCode
		DispositionText :$96.
		DispositionDiagDesc :$400.
		Diag1 $
		Diag2 $
		Diag3 $
		Diag4 $
		Diag5 $
		Diag6 $
		Diag7 $
		Diag8 $
		Diag9 $
		Diag10 $
		Diag11 $
		ProcCode1 $
		ProcCode2 $  
		ProcCode3 $  
		ProcCode4 $  
		ProcCode5 $  
		ProcCode6 $  
		ProcCode7 $  
		ProcCode8 $  
		ProcCode9 $  
		ProcCode10 $  
		ProcCode11 $  
		ProcCode12 $  
		ProcCode13 $  
		ProcCode14 $  
		ProcCode15 $  
		ProcCode16 $  
		ProcCode17 $  
		ProcCode18 $  
		ProcCode19 $  
		ProcCode20 $  
		InjuryCode1 $
		InjuryCode2 $
		InjuryCode3 $
		InjuryCode4 $
		InjuryCode5 $;
	RUN;
/*Select File including all 2021 ED visits*/
DATA nc2021; /*Note that starting in 2021, 26 ICD-10-CM codes could be sent with the limited dataset*/
	INFILE 'Z:\NC DETECT\Original Data\Standard.Dataset.3DigitZIP.2021.txt' FIRSTOBS = 2 delimiter = '|' DSD lrecl=2000 truncover;
	/*Import all variables with proper format*/
	INPUT          
		InternalTrackingID
		Age
		Sex $
		RaceCode $
		RaceDescription :$16.
		EthnicityCode $
		EthnicityDescription :$22.
		County :$50.
		ZIP $
		State $
		VisitID
		InsuranceCode
		InsuranceText :$25.
		ArrivalDate : YYYYMMDD10.
		ArrivalTime :$11.
		TransportCode
		TransportText :$57.
		ChiefComplaint :$200.
		SystolicBP
		DiastolicBP
		InitialEDTemp
		DispositionCode
		DispositionText :$96.
		DispositionDiagDesc :$400.
		Diag1 $
		Diag2 $
		Diag3 $
		Diag4 $
		Diag5 $
		Diag6 $
		Diag7 $
		Diag8 $
		Diag9 $
		Diag10 $
		Diag11 $
		Diag12 $
		Diag13 $
		Diag14 $
		Diag15 $
		Diag16 $
		Diag17 $
		Diag18 $
		Diag19 $
		Diag20 $
		Diag21 $
		Diag22 $
		Diag23 $
		Diag24 $
		Diag25 $
		Diag26 $;
	RUN;

DATA nc2016; /*Removing unnecessary variables (i.e. columns)*/
	SET nc2016;
	DROP RaceCode	EthnicityCode	ZIP	InsuranceCode	ArrivalTime	TransportCode 	TransportText
		ChiefComplaint	SystolicBP	DiastolicBP	InitialEDTemp	DispositionCode DispositionDiagDesc ProcCode1 ProcCode2 ProcCode3 ProcCode4 ProcCode5 ProcCode6 ProcCode7 ProcCode8 ProcCode9 ProcCode10
		ProcCode11 ProcCode12 ProcCode13 ProcCode14 ProcCode15 ProcCode16 ProcCode17 ProcCode18 ProcCode19 ProcCode20
		InjuryCode1 InjuryCode2 InjuryCode3 InjuryCode4 InjuryCode5;
	RUN;
DATA nc2017;
	SET nc2017;
	DROP RaceCode	EthnicityCode	ZIP	InsuranceCode	ArrivalTime	TransportCode 	TransportText
		ChiefComplaint	SystolicBP	DiastolicBP	InitialEDTemp	DispositionCode DispositionDiagDesc ProcCode1 ProcCode2 ProcCode3 ProcCode4 ProcCode5 ProcCode6 ProcCode7 ProcCode8 ProcCode9 ProcCode10
		ProcCode11 ProcCode12 ProcCode13 ProcCode14 ProcCode15 ProcCode16 ProcCode17 ProcCode18 ProcCode19 ProcCode20
		InjuryCode1 InjuryCode2 InjuryCode3 InjuryCode4 InjuryCode5;
	RUN;
DATA nc2018;
	SET nc2018;
	DROP RaceCode	EthnicityCode	ZIP	InsuranceCode	ArrivalTime	TransportCode 	TransportText
		ChiefComplaint	SystolicBP	DiastolicBP	InitialEDTemp DispositionCode DispositionDiagDesc ProcCode1 ProcCode2 ProcCode3 ProcCode4 ProcCode5 ProcCode6 ProcCode7 ProcCode8 ProcCode9 ProcCode10
		ProcCode11 ProcCode12 ProcCode13 ProcCode14 ProcCode15 ProcCode16 ProcCode17 ProcCode18 ProcCode19 ProcCode20
		InjuryCode1 InjuryCode2 InjuryCode3 InjuryCode4 InjuryCode5;
	RUN;
DATA nc2019;
	SET nc2019;
	DROP RaceCode	EthnicityCode	ZIP	InsuranceCode	ArrivalTime	TransportCode 	TransportText
		ChiefComplaint	SystolicBP	DiastolicBP	InitialEDTemp DispositionCode DispositionDiagDesc ProcCode1 ProcCode2 ProcCode3 ProcCode4 ProcCode5 ProcCode6 ProcCode7 ProcCode8 ProcCode9 ProcCode10
		ProcCode11 ProcCode12 ProcCode13 ProcCode14 ProcCode15 ProcCode16 ProcCode17 ProcCode18 ProcCode19 ProcCode20
		InjuryCode1 InjuryCode2 InjuryCode3 InjuryCode4 InjuryCode5;
	RUN;
DATA nc2020;
	SET nc2020;
	DROP RaceCode	EthnicityCode	ZIP	InsuranceCode	ArrivalTime	TransportCode 	TransportText
		ChiefComplaint	SystolicBP	DiastolicBP	InitialEDTemp DispositionCode DispositionDiagDesc ProcCode1 ProcCode2 ProcCode3 ProcCode4 ProcCode5 ProcCode6 ProcCode7 ProcCode8 ProcCode9 ProcCode10
		ProcCode11 ProcCode12 ProcCode13 ProcCode14 ProcCode15 ProcCode16 ProcCode17 ProcCode18 ProcCode19 ProcCode20
		InjuryCode1 InjuryCode2 InjuryCode3 InjuryCode4 InjuryCode5;
	RUN;
DATA nc2021;
	SET nc2021;
	DROP RaceCode	EthnicityCode	ZIP	InsuranceCode	ArrivalTime	TransportCode 	TransportText
		ChiefComplaint	SystolicBP	DiastolicBP	InitialEDTemp DispositionCode DispositionDiagDesc ProcCode1 ProcCode2 ProcCode3 ProcCode4 ProcCode5 ProcCode6 ProcCode7 ProcCode8 ProcCode9 ProcCode10
		ProcCode11 ProcCode12 ProcCode13 ProcCode14 ProcCode15 ProcCode16 ProcCode17 ProcCode18 ProcCode19 ProcCode20;
	RUN;

/*Remove irrelevant observations (i.e. rows); leaving only Women of Reproductive Age (WRA) residing in NC*/
DATA nc2016; /*Keep all women*/
	SET nc2016;
	IF (Sex = 'F');
	RUN;
DATA nc2017;
	SET nc2017;
	IF (Sex = 'F');
	RUN;
DATA nc2018;
	SET nc2018;
	IF (Sex = 'F');
	RUN;
DATA nc2019;
	SET nc2019;
	IF (Sex = 'F');
	RUN;
DATA nc2020;
	SET nc2020;
	IF (Sex = 'F');
	RUN;
DATA nc2021;
	SET nc2021;
	IF (Sex = 'F');
	RUN;

DATA nc2016; /*Keep NC women*/
	SET nc2016;
	IF (State = 'NC');
	RUN;
DATA nc2017;
	SET nc2017;
	IF (State = 'NC');
	RUN;
DATA nc2018;
	SET nc2018;
	IF (State = 'NC');
	RUN;
DATA nc2019;
	SET nc2019;
	IF (State = 'NC');
	RUN;
DATA nc2020;
	SET nc2020;
	IF (State = 'NC');
	RUN;
DATA nc2021;
	SET nc2021;
	IF (State = 'NC');
	RUN;

DATA nc2016; /* Keep NC-resident WRA*/
	SET nc2016;
	IF (Age >= 15 AND Age <= 44);
	RUN;
DATA nc2017;
	SET nc2017;
	IF (Age >= 15 AND Age <= 44);
	RUN;
DATA nc2018;
	SET nc2018;
	IF (Age >= 15 AND Age <= 44);
	RUN;
DATA nc2019;
	SET nc2019;
	IF (Age >= 15 AND Age <= 44);
	RUN;
DATA nc2020;
	SET nc2020;
	IF (Age >= 15 AND Age <= 44);
	RUN;
DATA nc2021;
	SET nc2021;
	IF (Age >= 15 AND Age <= 44);
	RUN;

DATA nc2016; /*Keep ED Visits that have at least 1 ICD-10-CM diagnosis code*/
	SET nc2016;
	IF (Diag1 > ' ') OR (Diag2 > ' ') OR (Diag3 > ' ') OR (Diag4 > ' ') OR (Diag5 > ' ') OR (Diag6 > ' ')
 	OR (Diag7 > ' ') OR (Diag8 > ' ') OR (Diag9 > ' ') OR (Diag10 > ' ') OR (Diag11 > ' ');
	RUN;
DATA nc2017;
	SET nc2017;
	IF (Diag1 > ' ') OR (Diag2 > ' ') OR (Diag3 > ' ') OR (Diag4 > ' ') OR (Diag5 > ' ') OR (Diag6 > ' ')
 	OR (Diag7 > ' ') OR (Diag8 > ' ') OR (Diag9 > ' ') OR (Diag10 > ' ') OR (Diag11 > ' ');
	RUN;
DATA nc2018;
	SET nc2018;
	IF (Diag1 > ' ') OR (Diag2 > ' ') OR (Diag3 > ' ') OR (Diag4 > ' ') OR (Diag5 > ' ') OR (Diag6 > ' ')
 	OR (Diag7 > ' ') OR (Diag8 > ' ') OR (Diag9 > ' ') OR (Diag10 > ' ') OR (Diag11 > ' ');
	RUN;
DATA nc2019;
	SET nc2019;
	IF (Diag1 > ' ') OR (Diag2 > ' ') OR (Diag3 > ' ') OR (Diag4 > ' ') OR (Diag5 > ' ') OR (Diag6 > ' ')
 	OR (Diag7 > ' ') OR (Diag8 > ' ') OR (Diag9 > ' ') OR (Diag10 > ' ') OR (Diag11 > ' ');
	RUN;
DATA nc2020;
	SET nc2020;
	IF (Diag1 > ' ') OR (Diag2 > ' ') OR (Diag3 > ' ') OR (Diag4 > ' ') OR (Diag5 > ' ') OR (Diag6 > ' ')
 	OR (Diag7 > ' ') OR (Diag8 > ' ') OR (Diag9 > ' ') OR (Diag10 > ' ') OR (Diag11 > ' ');
	RUN;
DATA nc2021;
	SET nc2021;
	IF (Diag1 > ' ') OR (Diag2 > ' ') OR (Diag3 > ' ') OR (Diag4 > ' ') OR (Diag5 > ' ') OR (Diag6 > ' ')
 	OR (Diag7 > ' ') OR (Diag8 > ' ') OR (Diag9 > ' ') OR (Diag10 > ' ') OR (Diag11 > ' ') OR (Diag12 > ' ')
	OR (Diag13 > ' ') OR (Diag14 > ' ') OR (Diag15 > ' ') OR (Diag16 > ' ') OR (Diag17 > ' ')OR (Diag18 > ' ')
	OR (Diag19 > ' ') OR (Diag20 > ' ') OR (Diag21 > ' ') OR (Diag22 > ' ') OR (Diag23 > ' ')OR (Diag24 > ' ')
	OR (Diag25 > ' ') OR (Diag26 > ' ');
	RUN;

DATA nc2016; /*Create Year covariate for modeling*/
    SET nc2016;
	LENGTH Visit_Year $ 4;
	IF ArrivalDate >0 THEN Visit_Year = 2016;
	RUN;
	PROC FREQ data=nc2016;
		TABLES Visit_Year;
		RUN;
DATA nc2017;
    SET nc2017;
	LENGTH Visit_Year $ 4;
	IF ArrivalDate >0 THEN Visit_Year = 2017;
	RUN;
	PROC FREQ data=nc2017;
		TABLES Visit_Year;
		RUN;
DATA nc2018;
    SET nc2018;
	LENGTH Visit_Year $ 4;
	IF ArrivalDate >0 THEN Visit_Year = 2018;
	RUN;
	PROC FREQ data=nc2018;
		TABLES Visit_Year;
		RUN;
DATA nc2019;
    SET nc2019;
	LENGTH Visit_Year $ 4;
	IF ArrivalDate >0 THEN Visit_Year = 2019;
	RUN;
	PROC FREQ data=nc2019;
		TABLES Visit_Year;
		RUN;
DATA nc2020;
    SET nc2020;
	LENGTH Visit_Year $ 4;
	IF ArrivalDate >0 THEN Visit_Year = 2020;
	RUN;
	PROC FREQ data=nc2020;
		TABLES Visit_Year;
		RUN;
DATA nc2021;
    SET nc2021;
	LENGTH Visit_Year $ 4;
	IF ArrivalDate >0 THEN Visit_Year = 2021;
	RUN;
	PROC FREQ data=nc2021;
		TABLES Visit_Year;
		RUN;

/*Creating Age Grouping variable*/
DATA nc2016; /*Age Group*/
	LENGTH Age_Group $ 11; *Here's the length statement;
	SET nc2016;
	IF age > 120 THEN Age_Group =" ";
		ELSE IF age >=15 and age<20 THEN Age_Group="15-19 years";
		ELSE IF age >=20 and age<25 THEN Age_Group="20-24 years";
		ELSE IF age >=25 and age<30 THEN Age_Group="25-29 years";
		ELSE IF age >=30 and age<35 THEN Age_Group="30-34 years";
		ELSE IF age >=35 and age<40 THEN Age_Group="35-39 years";
		ELSE IF age >=40 and age<45 THEN Age_Group="40-44 years";
	RUN;
DATA nc2017;
	LENGTH Age_Group $ 11; *Here's the length statement;
	SET nc2017;
	IF age > 120 THEN Age_Group =" ";
		ELSE IF age >=15 and age<20 THEN Age_Group="15-19 years";
		ELSE IF age >=20 and age<25 THEN Age_Group="20-24 years";
		ELSE IF age >=25 and age<30 THEN Age_Group="25-29 years";
		ELSE IF age >=30 and age<35 THEN Age_Group="30-34 years";
		ELSE IF age >=35 and age<40 THEN Age_Group="35-39 years";
		ELSE IF age >=40 and age<45 THEN Age_Group="40-44 years";
	RUN;
DATA nc2018;
	LENGTH Age_Group $ 11; *Here's the length statement;
	SET nc2018;
	IF age > 120 THEN Age_Group =" ";
		ELSE IF age >=15 and age<20 THEN Age_Group="15-19 years";
		ELSE IF age >=20 and age<25 THEN Age_Group="20-24 years";
		ELSE IF age >=25 and age<30 THEN Age_Group="25-29 years";
		ELSE IF age >=30 and age<35 THEN Age_Group="30-34 years";
		ELSE IF age >=35 and age<40 THEN Age_Group="35-39 years";
		ELSE IF age >=40 and age<45 THEN Age_Group="40-44 years";
	RUN;
DATA nc2019;
	LENGTH Age_Group $ 11; *Here's the length statement;
	SET nc2019;
	IF age > 120 THEN Age_Group =" ";
		ELSE IF age >=15 and age<20 THEN Age_Group="15-19 years";
		ELSE IF age >=20 and age<25 THEN Age_Group="20-24 years";
		ELSE IF age >=25 and age<30 THEN Age_Group="25-29 years";
		ELSE IF age >=30 and age<35 THEN Age_Group="30-34 years";
		ELSE IF age >=35 and age<40 THEN Age_Group="35-39 years";
		ELSE IF age >=40 and age<45 THEN Age_Group="40-44 years";
	RUN;
DATA nc2020;
	LENGTH Age_Group $ 11; *Here's the length statement;
	SET nc2020;
	IF age > 120 THEN Age_Group =" ";
		ELSE IF age >=15 and age<20 THEN Age_Group="15-19 years";
		ELSE IF age >=20 and age<25 THEN Age_Group="20-24 years";
		ELSE IF age >=25 and age<30 THEN Age_Group="25-29 years";
		ELSE IF age >=30 and age<35 THEN Age_Group="30-34 years";
		ELSE IF age >=35 and age<40 THEN Age_Group="35-39 years";
		ELSE IF age >=40 and age<45 THEN Age_Group="40-44 years";
	RUN;
DATA nc2021;
	LENGTH Age_Group $ 11; *Here's the length statement;
	SET nc2021;
	IF age > 120 THEN Age_Group =" ";
		ELSE IF age >=15 and age<20 THEN Age_Group="15-19 years";
		ELSE IF age >=20 and age<25 THEN Age_Group="20-24 years";
		ELSE IF age >=25 and age<30 THEN Age_Group="25-29 years";
		ELSE IF age >=30 and age<35 THEN Age_Group="30-34 years";
		ELSE IF age >=35 and age<40 THEN Age_Group="35-39 years";
		ELSE IF age >=40 and age<45 THEN Age_Group="40-44 years";
	RUN;

/*Create County Type Variable*/
DATA nc2016; /*County Type*/
	LENGTH County_Type $ 11; /*Here's the length statement*/
	SET nc2016;
	IF County = "Alamance" THEN
		County_Type ="Suburban";
	ELSE IF County = "Alexander" THEN
		County_Type = "Rural";
	ELSE IF County = "Alleghany" THEN
		County_Type = "Rural";
	ELSE IF County = "Anson" THEN
		County_Type = "Rural";
	ELSE IF County = "Ashe" THEN
		County_Type = "Rural";
	ELSE IF County = "Avery" THEN
		County_Type = "Rural";
	ELSE IF County = "Beaufort" THEN
		County_Type = "Rural";
	ELSE IF County = "Bertie" THEN
		County_Type = "Rural";
	ELSE IF County = "Bladen" THEN
		County_Type = "Rural";
	ELSE IF County = "Brunswick" THEN
		County_Type = "Rural";
	ELSE IF County = "Buncombe" THEN
		County_Type = "Suburban";
	ELSE IF County = "Burke" THEN
		County_Type = "Rural";
	ELSE IF County = "Cabarrus" THEN
		County_Type = "Suburban";
	ELSE IF County = "Caldwell" THEN
		County_Type = "Rural";
	ELSE IF County = "Camden" THEN
		County_Type = "Rural";
	ELSE IF County = "Carteret" THEN
		County_Type = "Rural";
	ELSE IF County = "Caswell" THEN
		County_Type = "Rural";
	ELSE IF County = "Catawba" THEN
		County_Type = "Suburban";
	ELSE IF County = "Chatham" THEN
		County_Type = "Rural";
	ELSE IF County = "Cherokee" THEN
		County_Type = "Rural";
	ELSE IF County = "Chowan" THEN
		County_Type = "Rural";
	ELSE IF County = "Clay" THEN
		County_Type = "Rural";
	ELSE IF County = "Cleveland" THEN
		County_Type = "Rural";
	ELSE IF County = "Columbus" THEN
		County_Type = "Rural";
	ELSE IF County = "Craven" THEN
		County_Type = "Rural";
	ELSE IF County = "Cumberland" THEN
		County_Type = "Suburban";
	ELSE IF County = "Currituck" THEN
		County_Type = "Rural";
	ELSE IF County = "Dare" THEN
		County_Type = "Rural";
	ELSE IF County = "Davidson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Davie" THEN
		County_Type = "Rural";
	ELSE IF County = "Duplin" THEN
		County_Type = "Rural";
	ELSE IF County = "Durham" THEN
		County_Type = "Urban";
	ELSE IF County = "Edgecombe" THEN
		County_Type = "Rural";
	ELSE IF County = "Forsyth" THEN
		County_Type = "Urban";
	ELSE IF County = "Franklin" THEN
		County_Type = "Rural";
	ELSE IF County = "Gaston" THEN
		County_Type = "Suburban";
	ELSE IF County = "Gates" THEN
		County_Type = "Rural";
	ELSE IF County = "Graham" THEN
		County_Type = "Rural";
	ELSE IF County = "Granville" THEN
		County_Type = "Rural";
	ELSE IF County = "Greene" THEN
		County_Type = "Rural";
	ELSE IF County = "Guilford" THEN
		County_Type = "Urban";
	ELSE IF County = "Halifax" THEN
		County_Type = "Rural";
	ELSE IF County = "Harnett" THEN
		County_Type = "Rural";
	ELSE IF County = "Haywood" THEN
		County_Type = "Rural";
	ELSE IF County = "Henderson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Hertford" THEN
		County_Type = "Rural";
	ELSE IF County = "Hoke" THEN
		County_Type = "Rural";
	ELSE IF County = "Hyde" THEN
		County_Type = "Rural";
	ELSE IF County = "Iredell" THEN
		County_Type = "Suburban";
	ELSE IF County = "Jackson" THEN
		County_Type = "Rural";
	ELSE IF County = "Johnston" THEN
		County_Type = "Rural";
	ELSE IF County = "Jones" THEN
		County_Type = "Rural";
	ELSE IF County = "Lee" THEN
		County_Type = "Rural";
	ELSE IF County = "Lenoir" THEN
		County_Type = "Rural";
	ELSE IF County = "Lincoln" THEN
		County_Type = "Suburban";
	ELSE IF County = "Macon" THEN
		County_Type = "Rural";
	ELSE IF County = "Madison" THEN
		County_Type = "Rural";
	ELSE IF County = "Martin" THEN
		County_Type = "Rural";
	ELSE IF County = "McDowell" THEN
		County_Type = "Rural";
	ELSE IF County = "Mecklenburg" THEN
		County_Type = "Urban";
	ELSE IF County = "Mitchell" THEN
		County_Type = "Rural";
	ELSE IF County = "Montgomery" THEN
		County_Type = "Rural";
	ELSE IF County = "Moore" THEN
		County_Type = "Rural";
	ELSE IF County = "Nash" THEN
		County_Type = "Rural";
	ELSE IF County = "New Hanover" THEN
		County_Type = "Urban";
	ELSE IF County = "Northampton" THEN
		County_Type = "Rural";
	ELSE IF County = "Onslow" THEN
		County_Type = "Rural";
	ELSE IF County = "Orange" THEN
		County_Type = "Suburban";
	ELSE IF County = "Pamlico" THEN
		County_Type = "Rural";
	ELSE IF County = "Pasquotank" THEN
		County_Type = "Rural";
	ELSE IF County = "Pender" THEN
		County_Type = "Rural";
	ELSE IF County = "Perquimans" THEN
		County_Type = "Rural";
	ELSE IF County = "Person" THEN
		County_Type = "Rural";
	ELSE IF County = "Pitt" THEN
		County_Type = "Suburban";
	ELSE IF County = "Polk" THEN
		County_Type = "Rural";
	ELSE IF County = "Randolph" THEN
		County_Type = "Rural";
	ELSE IF County = "Richmond" THEN
		County_Type = "Rural";
	ELSE IF County = "Robeson" THEN
		County_Type = "Rural";
	ELSE IF County = "Rockingham" THEN
		County_Type = "Rural";
	ELSE IF County = "Rowan" THEN
		County_Type = "Suburban";
	ELSE IF County = "Rutherford" THEN
		County_Type = "Rural";
	ELSE IF County = "Sampson" THEN
		County_Type = "Rural";
	ELSE IF County = "Scotland" THEN
		County_Type = "Rural";
	ELSE IF County = "Stanly" THEN
		County_Type = "Rural";
	ELSE IF County = "Stokes" THEN
		County_Type = "Rural";
	ELSE IF County = "Surry" THEN
		County_Type = "Rural";
	ELSE IF County = "Swain" THEN
		County_Type = "Rural";
	ELSE IF County = "Transylvania" THEN
		County_Type = "Rural";
	ELSE IF County = "Tyrrell" THEN
		County_Type = "Rural";
	ELSE IF County = "Union" THEN
		County_Type = "Suburban";
	ELSE IF County = "Vance" THEN
		County_Type = "Rural";
	ELSE IF County = "Wake" THEN
		County_Type = "Urban";
	ELSE IF County = "Warren" THEN
		County_Type = "Rural";
	ELSE IF County = "Washington" THEN
		County_Type = "Rural";
	ELSE IF County = "Watauga" THEN
		County_Type = "Rural";
	ELSE IF County = "Wayne" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilkes" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilson" THEN
		County_Type = "Rural";
	ELSE IF County = "Yadkin" THEN
		County_Type = "Rural";
	ELSE IF County = "Yancey" THEN
		County_Type = "Rural";
	RUN;

DATA nc2017;
	LENGTH County_Type $ 11; /*Here's the length statement*/
	SET nc2017;
	IF County = "Alamance" THEN
		County_Type ="Suburban";
	ELSE IF County = "Alexander" THEN
		County_Type = "Rural";
	ELSE IF County = "Alleghany" THEN
		County_Type = "Rural";
	ELSE IF County = "Anson" THEN
		County_Type = "Rural";
	ELSE IF County = "Ashe" THEN
		County_Type = "Rural";
	ELSE IF County = "Avery" THEN
		County_Type = "Rural";
	ELSE IF County = "Beaufort" THEN
		County_Type = "Rural";
	ELSE IF County = "Bertie" THEN
		County_Type = "Rural";
	ELSE IF County = "Bladen" THEN
		County_Type = "Rural";
	ELSE IF County = "Brunswick" THEN
		County_Type = "Rural";
	ELSE IF County = "Buncombe" THEN
		County_Type = "Suburban";
	ELSE IF County = "Burke" THEN
		County_Type = "Rural";
	ELSE IF County = "Cabarrus" THEN
		County_Type = "Suburban";
	ELSE IF County = "Caldwell" THEN
		County_Type = "Rural";
	ELSE IF County = "Camden" THEN
		County_Type = "Rural";
	ELSE IF County = "Carteret" THEN
		County_Type = "Rural";
	ELSE IF County = "Caswell" THEN
		County_Type = "Rural";
	ELSE IF County = "Catawba" THEN
		County_Type = "Suburban";
	ELSE IF County = "Chatham" THEN
		County_Type = "Rural";
	ELSE IF County = "Cherokee" THEN
		County_Type = "Rural";
	ELSE IF County = "Chowan" THEN
		County_Type = "Rural";
	ELSE IF County = "Clay" THEN
		County_Type = "Rural";
	ELSE IF County = "Cleveland" THEN
		County_Type = "Rural";
	ELSE IF County = "Columbus" THEN
		County_Type = "Rural";
	ELSE IF County = "Craven" THEN
		County_Type = "Rural";
	ELSE IF County = "Cumberland" THEN
		County_Type = "Suburban";
	ELSE IF County = "Currituck" THEN
		County_Type = "Rural";
	ELSE IF County = "Dare" THEN
		County_Type = "Rural";
	ELSE IF County = "Davidson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Davie" THEN
		County_Type = "Rural";
	ELSE IF County = "Duplin" THEN
		County_Type = "Rural";
	ELSE IF County = "Durham" THEN
		County_Type = "Urban";
	ELSE IF County = "Edgecombe" THEN
		County_Type = "Rural";
	ELSE IF County = "Forsyth" THEN
		County_Type = "Urban";
	ELSE IF County = "Franklin" THEN
		County_Type = "Rural";
	ELSE IF County = "Gaston" THEN
		County_Type = "Suburban";
	ELSE IF County = "Gates" THEN
		County_Type = "Rural";
	ELSE IF County = "Graham" THEN
		County_Type = "Rural";
	ELSE IF County = "Granville" THEN
		County_Type = "Rural";
	ELSE IF County = "Greene" THEN
		County_Type = "Rural";
	ELSE IF County = "Guilford" THEN
		County_Type = "Urban";
	ELSE IF County = "Halifax" THEN
		County_Type = "Rural";
	ELSE IF County = "Harnett" THEN
		County_Type = "Rural";
	ELSE IF County = "Haywood" THEN
		County_Type = "Rural";
	ELSE IF County = "Henderson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Hertford" THEN
		County_Type = "Rural";
	ELSE IF County = "Hoke" THEN
		County_Type = "Rural";
	ELSE IF County = "Hyde" THEN
		County_Type = "Rural";
	ELSE IF County = "Iredell" THEN
		County_Type = "Suburban";
	ELSE IF County = "Jackson" THEN
		County_Type = "Rural";
	ELSE IF County = "Johnston" THEN
		County_Type = "Rural";
	ELSE IF County = "Jones" THEN
		County_Type = "Rural";
	ELSE IF County = "Lee" THEN
		County_Type = "Rural";
	ELSE IF County = "Lenoir" THEN
		County_Type = "Rural";
	ELSE IF County = "Lincoln" THEN
		County_Type = "Suburban";
	ELSE IF County = "Macon" THEN
		County_Type = "Rural";
	ELSE IF County = "Madison" THEN
		County_Type = "Rural";
	ELSE IF County = "Martin" THEN
		County_Type = "Rural";
	ELSE IF County = "McDowell" THEN
		County_Type = "Rural";
	ELSE IF County = "Mecklenburg" THEN
		County_Type = "Urban";
	ELSE IF County = "Mitchell" THEN
		County_Type = "Rural";
	ELSE IF County = "Montgomery" THEN
		County_Type = "Rural";
	ELSE IF County = "Moore" THEN
		County_Type = "Rural";
	ELSE IF County = "Nash" THEN
		County_Type = "Rural";
	ELSE IF County = "New Hanover" THEN
		County_Type = "Urban";
	ELSE IF County = "Northampton" THEN
		County_Type = "Rural";
	ELSE IF County = "Onslow" THEN
		County_Type = "Rural";
	ELSE IF County = "Orange" THEN
		County_Type = "Suburban";
	ELSE IF County = "Pamlico" THEN
		County_Type = "Rural";
	ELSE IF County = "Pasquotank" THEN
		County_Type = "Rural";
	ELSE IF County = "Pender" THEN
		County_Type = "Rural";
	ELSE IF County = "Perquimans" THEN
		County_Type = "Rural";
	ELSE IF County = "Person" THEN
		County_Type = "Rural";
	ELSE IF County = "Pitt" THEN
		County_Type = "Suburban";
	ELSE IF County = "Polk" THEN
		County_Type = "Rural";
	ELSE IF County = "Randolph" THEN
		County_Type = "Rural";
	ELSE IF County = "Richmond" THEN
		County_Type = "Rural";
	ELSE IF County = "Robeson" THEN
		County_Type = "Rural";
	ELSE IF County = "Rockingham" THEN
		County_Type = "Rural";
	ELSE IF County = "Rowan" THEN
		County_Type = "Suburban";
	ELSE IF County = "Rutherford" THEN
		County_Type = "Rural";
	ELSE IF County = "Sampson" THEN
		County_Type = "Rural";
	ELSE IF County = "Scotland" THEN
		County_Type = "Rural";
	ELSE IF County = "Stanly" THEN
		County_Type = "Rural";
	ELSE IF County = "Stokes" THEN
		County_Type = "Rural";
	ELSE IF County = "Surry" THEN
		County_Type = "Rural";
	ELSE IF County = "Swain" THEN
		County_Type = "Rural";
	ELSE IF County = "Transylvania" THEN
		County_Type = "Rural";
	ELSE IF County = "Tyrrell" THEN
		County_Type = "Rural";
	ELSE IF County = "Union" THEN
		County_Type = "Suburban";
	ELSE IF County = "Vance" THEN
		County_Type = "Rural";
	ELSE IF County = "Wake" THEN
		County_Type = "Urban";
	ELSE IF County = "Warren" THEN
		County_Type = "Rural";
	ELSE IF County = "Washington" THEN
		County_Type = "Rural";
	ELSE IF County = "Watauga" THEN
		County_Type = "Rural";
	ELSE IF County = "Wayne" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilkes" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilson" THEN
		County_Type = "Rural";
	ELSE IF County = "Yadkin" THEN
		County_Type = "Rural";
	ELSE IF County = "Yancey" THEN
		County_Type = "Rural";
	RUN;
DATA nc2018;
	LENGTH County_Type $ 11; /*Here's the length statement*/
	SET nc2018;
	IF County = "Alamance" THEN
		County_Type ="Suburban";
	ELSE IF County = "Alexander" THEN
		County_Type = "Rural";
	ELSE IF County = "Alleghany" THEN
		County_Type = "Rural";
	ELSE IF County = "Anson" THEN
		County_Type = "Rural";
	ELSE IF County = "Ashe" THEN
		County_Type = "Rural";
	ELSE IF County = "Avery" THEN
		County_Type = "Rural";
	ELSE IF County = "Beaufort" THEN
		County_Type = "Rural";
	ELSE IF County = "Bertie" THEN
		County_Type = "Rural";
	ELSE IF County = "Bladen" THEN
		County_Type = "Rural";
	ELSE IF County = "Brunswick" THEN
		County_Type = "Rural";
	ELSE IF County = "Buncombe" THEN
		County_Type = "Suburban";
	ELSE IF County = "Burke" THEN
		County_Type = "Rural";
	ELSE IF County = "Cabarrus" THEN
		County_Type = "Suburban";
	ELSE IF County = "Caldwell" THEN
		County_Type = "Rural";
	ELSE IF County = "Camden" THEN
		County_Type = "Rural";
	ELSE IF County = "Carteret" THEN
		County_Type = "Rural";
	ELSE IF County = "Caswell" THEN
		County_Type = "Rural";
	ELSE IF County = "Catawba" THEN
		County_Type = "Suburban";
	ELSE IF County = "Chatham" THEN
		County_Type = "Rural";
	ELSE IF County = "Cherokee" THEN
		County_Type = "Rural";
	ELSE IF County = "Chowan" THEN
		County_Type = "Rural";
	ELSE IF County = "Clay" THEN
		County_Type = "Rural";
	ELSE IF County = "Cleveland" THEN
		County_Type = "Rural";
	ELSE IF County = "Columbus" THEN
		County_Type = "Rural";
	ELSE IF County = "Craven" THEN
		County_Type = "Rural";
	ELSE IF County = "Cumberland" THEN
		County_Type = "Suburban";
	ELSE IF County = "Currituck" THEN
		County_Type = "Rural";
	ELSE IF County = "Dare" THEN
		County_Type = "Rural";
	ELSE IF County = "Davidson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Davie" THEN
		County_Type = "Rural";
	ELSE IF County = "Duplin" THEN
		County_Type = "Rural";
	ELSE IF County = "Durham" THEN
		County_Type = "Urban";
	ELSE IF County = "Edgecombe" THEN
		County_Type = "Rural";
	ELSE IF County = "Forsyth" THEN
		County_Type = "Urban";
	ELSE IF County = "Franklin" THEN
		County_Type = "Rural";
	ELSE IF County = "Gaston" THEN
		County_Type = "Suburban";
	ELSE IF County = "Gates" THEN
		County_Type = "Rural";
	ELSE IF County = "Graham" THEN
		County_Type = "Rural";
	ELSE IF County = "Granville" THEN
		County_Type = "Rural";
	ELSE IF County = "Greene" THEN
		County_Type = "Rural";
	ELSE IF County = "Guilford" THEN
		County_Type = "Urban";
	ELSE IF County = "Halifax" THEN
		County_Type = "Rural";
	ELSE IF County = "Harnett" THEN
		County_Type = "Rural";
	ELSE IF County = "Haywood" THEN
		County_Type = "Rural";
	ELSE IF County = "Henderson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Hertford" THEN
		County_Type = "Rural";
	ELSE IF County = "Hoke" THEN
		County_Type = "Rural";
	ELSE IF County = "Hyde" THEN
		County_Type = "Rural";
	ELSE IF County = "Iredell" THEN
		County_Type = "Suburban";
	ELSE IF County = "Jackson" THEN
		County_Type = "Rural";
	ELSE IF County = "Johnston" THEN
		County_Type = "Rural";
	ELSE IF County = "Jones" THEN
		County_Type = "Rural";
	ELSE IF County = "Lee" THEN
		County_Type = "Rural";
	ELSE IF County = "Lenoir" THEN
		County_Type = "Rural";
	ELSE IF County = "Lincoln" THEN
		County_Type = "Suburban";
	ELSE IF County = "Macon" THEN
		County_Type = "Rural";
	ELSE IF County = "Madison" THEN
		County_Type = "Rural";
	ELSE IF County = "Martin" THEN
		County_Type = "Rural";
	ELSE IF County = "McDowell" THEN
		County_Type = "Rural";
	ELSE IF County = "Mecklenburg" THEN
		County_Type = "Urban";
	ELSE IF County = "Mitchell" THEN
		County_Type = "Rural";
	ELSE IF County = "Montgomery" THEN
		County_Type = "Rural";
	ELSE IF County = "Moore" THEN
		County_Type = "Rural";
	ELSE IF County = "Nash" THEN
		County_Type = "Rural";
	ELSE IF County = "New Hanover" THEN
		County_Type = "Urban";
	ELSE IF County = "Northampton" THEN
		County_Type = "Rural";
	ELSE IF County = "Onslow" THEN
		County_Type = "Rural";
	ELSE IF County = "Orange" THEN
		County_Type = "Suburban";
	ELSE IF County = "Pamlico" THEN
		County_Type = "Rural";
	ELSE IF County = "Pasquotank" THEN
		County_Type = "Rural";
	ELSE IF County = "Pender" THEN
		County_Type = "Rural";
	ELSE IF County = "Perquimans" THEN
		County_Type = "Rural";
	ELSE IF County = "Person" THEN
		County_Type = "Rural";
	ELSE IF County = "Pitt" THEN
		County_Type = "Suburban";
	ELSE IF County = "Polk" THEN
		County_Type = "Rural";
	ELSE IF County = "Randolph" THEN
		County_Type = "Rural";
	ELSE IF County = "Richmond" THEN
		County_Type = "Rural";
	ELSE IF County = "Robeson" THEN
		County_Type = "Rural";
	ELSE IF County = "Rockingham" THEN
		County_Type = "Rural";
	ELSE IF County = "Rowan" THEN
		County_Type = "Suburban";
	ELSE IF County = "Rutherford" THEN
		County_Type = "Rural";
	ELSE IF County = "Sampson" THEN
		County_Type = "Rural";
	ELSE IF County = "Scotland" THEN
		County_Type = "Rural";
	ELSE IF County = "Stanly" THEN
		County_Type = "Rural";
	ELSE IF County = "Stokes" THEN
		County_Type = "Rural";
	ELSE IF County = "Surry" THEN
		County_Type = "Rural";
	ELSE IF County = "Swain" THEN
		County_Type = "Rural";
	ELSE IF County = "Transylvania" THEN
		County_Type = "Rural";
	ELSE IF County = "Tyrrell" THEN
		County_Type = "Rural";
	ELSE IF County = "Union" THEN
		County_Type = "Suburban";
	ELSE IF County = "Vance" THEN
		County_Type = "Rural";
	ELSE IF County = "Wake" THEN
		County_Type = "Urban";
	ELSE IF County = "Warren" THEN
		County_Type = "Rural";
	ELSE IF County = "Washington" THEN
		County_Type = "Rural";
	ELSE IF County = "Watauga" THEN
		County_Type = "Rural";
	ELSE IF County = "Wayne" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilkes" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilson" THEN
		County_Type = "Rural";
	ELSE IF County = "Yadkin" THEN
		County_Type = "Rural";
	ELSE IF County = "Yancey" THEN
		County_Type = "Rural";
	RUN;
DATA nc2019;
	LENGTH County_Type $ 11; /*Here's the length statement*/
	SET nc2019;
	IF County = "Alamance" THEN
		County_Type ="Suburban";
	ELSE IF County = "Alexander" THEN
		County_Type = "Rural";
	ELSE IF County = "Alleghany" THEN
		County_Type = "Rural";
	ELSE IF County = "Anson" THEN
		County_Type = "Rural";
	ELSE IF County = "Ashe" THEN
		County_Type = "Rural";
	ELSE IF County = "Avery" THEN
		County_Type = "Rural";
	ELSE IF County = "Beaufort" THEN
		County_Type = "Rural";
	ELSE IF County = "Bertie" THEN
		County_Type = "Rural";
	ELSE IF County = "Bladen" THEN
		County_Type = "Rural";
	ELSE IF County = "Brunswick" THEN
		County_Type = "Rural";
	ELSE IF County = "Buncombe" THEN
		County_Type = "Suburban";
	ELSE IF County = "Burke" THEN
		County_Type = "Rural";
	ELSE IF County = "Cabarrus" THEN
		County_Type = "Suburban";
	ELSE IF County = "Caldwell" THEN
		County_Type = "Rural";
	ELSE IF County = "Camden" THEN
		County_Type = "Rural";
	ELSE IF County = "Carteret" THEN
		County_Type = "Rural";
	ELSE IF County = "Caswell" THEN
		County_Type = "Rural";
	ELSE IF County = "Catawba" THEN
		County_Type = "Suburban";
	ELSE IF County = "Chatham" THEN
		County_Type = "Rural";
	ELSE IF County = "Cherokee" THEN
		County_Type = "Rural";
	ELSE IF County = "Chowan" THEN
		County_Type = "Rural";
	ELSE IF County = "Clay" THEN
		County_Type = "Rural";
	ELSE IF County = "Cleveland" THEN
		County_Type = "Rural";
	ELSE IF County = "Columbus" THEN
		County_Type = "Rural";
	ELSE IF County = "Craven" THEN
		County_Type = "Rural";
	ELSE IF County = "Cumberland" THEN
		County_Type = "Suburban";
	ELSE IF County = "Currituck" THEN
		County_Type = "Rural";
	ELSE IF County = "Dare" THEN
		County_Type = "Rural";
	ELSE IF County = "Davidson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Davie" THEN
		County_Type = "Rural";
	ELSE IF County = "Duplin" THEN
		County_Type = "Rural";
	ELSE IF County = "Durham" THEN
		County_Type = "Urban";
	ELSE IF County = "Edgecombe" THEN
		County_Type = "Rural";
	ELSE IF County = "Forsyth" THEN
		County_Type = "Urban";
	ELSE IF County = "Franklin" THEN
		County_Type = "Rural";
	ELSE IF County = "Gaston" THEN
		County_Type = "Suburban";
	ELSE IF County = "Gates" THEN
		County_Type = "Rural";
	ELSE IF County = "Graham" THEN
		County_Type = "Rural";
	ELSE IF County = "Granville" THEN
		County_Type = "Rural";
	ELSE IF County = "Greene" THEN
		County_Type = "Rural";
	ELSE IF County = "Guilford" THEN
		County_Type = "Urban";
	ELSE IF County = "Halifax" THEN
		County_Type = "Rural";
	ELSE IF County = "Harnett" THEN
		County_Type = "Rural";
	ELSE IF County = "Haywood" THEN
		County_Type = "Rural";
	ELSE IF County = "Henderson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Hertford" THEN
		County_Type = "Rural";
	ELSE IF County = "Hoke" THEN
		County_Type = "Rural";
	ELSE IF County = "Hyde" THEN
		County_Type = "Rural";
	ELSE IF County = "Iredell" THEN
		County_Type = "Suburban";
	ELSE IF County = "Jackson" THEN
		County_Type = "Rural";
	ELSE IF County = "Johnston" THEN
		County_Type = "Rural";
	ELSE IF County = "Jones" THEN
		County_Type = "Rural";
	ELSE IF County = "Lee" THEN
		County_Type = "Rural";
	ELSE IF County = "Lenoir" THEN
		County_Type = "Rural";
	ELSE IF County = "Lincoln" THEN
		County_Type = "Suburban";
	ELSE IF County = "Macon" THEN
		County_Type = "Rural";
	ELSE IF County = "Madison" THEN
		County_Type = "Rural";
	ELSE IF County = "Martin" THEN
		County_Type = "Rural";
	ELSE IF County = "McDowell" THEN
		County_Type = "Rural";
	ELSE IF County = "Mecklenburg" THEN
		County_Type = "Urban";
	ELSE IF County = "Mitchell" THEN
		County_Type = "Rural";
	ELSE IF County = "Montgomery" THEN
		County_Type = "Rural";
	ELSE IF County = "Moore" THEN
		County_Type = "Rural";
	ELSE IF County = "Nash" THEN
		County_Type = "Rural";
	ELSE IF County = "New Hanover" THEN
		County_Type = "Urban";
	ELSE IF County = "Northampton" THEN
		County_Type = "Rural";
	ELSE IF County = "Onslow" THEN
		County_Type = "Rural";
	ELSE IF County = "Orange" THEN
		County_Type = "Suburban";
	ELSE IF County = "Pamlico" THEN
		County_Type = "Rural";
	ELSE IF County = "Pasquotank" THEN
		County_Type = "Rural";
	ELSE IF County = "Pender" THEN
		County_Type = "Rural";
	ELSE IF County = "Perquimans" THEN
		County_Type = "Rural";
	ELSE IF County = "Person" THEN
		County_Type = "Rural";
	ELSE IF County = "Pitt" THEN
		County_Type = "Suburban";
	ELSE IF County = "Polk" THEN
		County_Type = "Rural";
	ELSE IF County = "Randolph" THEN
		County_Type = "Rural";
	ELSE IF County = "Richmond" THEN
		County_Type = "Rural";
	ELSE IF County = "Robeson" THEN
		County_Type = "Rural";
	ELSE IF County = "Rockingham" THEN
		County_Type = "Rural";
	ELSE IF County = "Rowan" THEN
		County_Type = "Suburban";
	ELSE IF County = "Rutherford" THEN
		County_Type = "Rural";
	ELSE IF County = "Sampson" THEN
		County_Type = "Rural";
	ELSE IF County = "Scotland" THEN
		County_Type = "Rural";
	ELSE IF County = "Stanly" THEN
		County_Type = "Rural";
	ELSE IF County = "Stokes" THEN
		County_Type = "Rural";
	ELSE IF County = "Surry" THEN
		County_Type = "Rural";
	ELSE IF County = "Swain" THEN
		County_Type = "Rural";
	ELSE IF County = "Transylvania" THEN
		County_Type = "Rural";
	ELSE IF County = "Tyrrell" THEN
		County_Type = "Rural";
	ELSE IF County = "Union" THEN
		County_Type = "Suburban";
	ELSE IF County = "Vance" THEN
		County_Type = "Rural";
	ELSE IF County = "Wake" THEN
		County_Type = "Urban";
	ELSE IF County = "Warren" THEN
		County_Type = "Rural";
	ELSE IF County = "Washington" THEN
		County_Type = "Rural";
	ELSE IF County = "Watauga" THEN
		County_Type = "Rural";
	ELSE IF County = "Wayne" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilkes" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilson" THEN
		County_Type = "Rural";
	ELSE IF County = "Yadkin" THEN
		County_Type = "Rural";
	ELSE IF County = "Yancey" THEN
		County_Type = "Rural";
	RUN;
DATA nc2020;
	LENGTH County_Type $ 11; /*Here's the length statement*/
	SET nc2020;
	IF County = "Alamance" THEN
		County_Type ="Suburban";
	ELSE IF County = "Alexander" THEN
		County_Type = "Rural";
	ELSE IF County = "Alleghany" THEN
		County_Type = "Rural";
	ELSE IF County = "Anson" THEN
		County_Type = "Rural";
	ELSE IF County = "Ashe" THEN
		County_Type = "Rural";
	ELSE IF County = "Avery" THEN
		County_Type = "Rural";
	ELSE IF County = "Beaufort" THEN
		County_Type = "Rural";
	ELSE IF County = "Bertie" THEN
		County_Type = "Rural";
	ELSE IF County = "Bladen" THEN
		County_Type = "Rural";
	ELSE IF County = "Brunswick" THEN
		County_Type = "Rural";
	ELSE IF County = "Buncombe" THEN
		County_Type = "Suburban";
	ELSE IF County = "Burke" THEN
		County_Type = "Rural";
	ELSE IF County = "Cabarrus" THEN
		County_Type = "Suburban";
	ELSE IF County = "Caldwell" THEN
		County_Type = "Rural";
	ELSE IF County = "Camden" THEN
		County_Type = "Rural";
	ELSE IF County = "Carteret" THEN
		County_Type = "Rural";
	ELSE IF County = "Caswell" THEN
		County_Type = "Rural";
	ELSE IF County = "Catawba" THEN
		County_Type = "Suburban";
	ELSE IF County = "Chatham" THEN
		County_Type = "Rural";
	ELSE IF County = "Cherokee" THEN
		County_Type = "Rural";
	ELSE IF County = "Chowan" THEN
		County_Type = "Rural";
	ELSE IF County = "Clay" THEN
		County_Type = "Rural";
	ELSE IF County = "Cleveland" THEN
		County_Type = "Rural";
	ELSE IF County = "Columbus" THEN
		County_Type = "Rural";
	ELSE IF County = "Craven" THEN
		County_Type = "Rural";
	ELSE IF County = "Cumberland" THEN
		County_Type = "Suburban";
	ELSE IF County = "Currituck" THEN
		County_Type = "Rural";
	ELSE IF County = "Dare" THEN
		County_Type = "Rural";
	ELSE IF County = "Davidson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Davie" THEN
		County_Type = "Rural";
	ELSE IF County = "Duplin" THEN
		County_Type = "Rural";
	ELSE IF County = "Durham" THEN
		County_Type = "Urban";
	ELSE IF County = "Edgecombe" THEN
		County_Type = "Rural";
	ELSE IF County = "Forsyth" THEN
		County_Type = "Urban";
	ELSE IF County = "Franklin" THEN
		County_Type = "Rural";
	ELSE IF County = "Gaston" THEN
		County_Type = "Suburban";
	ELSE IF County = "Gates" THEN
		County_Type = "Rural";
	ELSE IF County = "Graham" THEN
		County_Type = "Rural";
	ELSE IF County = "Granville" THEN
		County_Type = "Rural";
	ELSE IF County = "Greene" THEN
		County_Type = "Rural";
	ELSE IF County = "Guilford" THEN
		County_Type = "Urban";
	ELSE IF County = "Halifax" THEN
		County_Type = "Rural";
	ELSE IF County = "Harnett" THEN
		County_Type = "Rural";
	ELSE IF County = "Haywood" THEN
		County_Type = "Rural";
	ELSE IF County = "Henderson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Hertford" THEN
		County_Type = "Rural";
	ELSE IF County = "Hoke" THEN
		County_Type = "Rural";
	ELSE IF County = "Hyde" THEN
		County_Type = "Rural";
	ELSE IF County = "Iredell" THEN
		County_Type = "Suburban";
	ELSE IF County = "Jackson" THEN
		County_Type = "Rural";
	ELSE IF County = "Johnston" THEN
		County_Type = "Rural";
	ELSE IF County = "Jones" THEN
		County_Type = "Rural";
	ELSE IF County = "Lee" THEN
		County_Type = "Rural";
	ELSE IF County = "Lenoir" THEN
		County_Type = "Rural";
	ELSE IF County = "Lincoln" THEN
		County_Type = "Suburban";
	ELSE IF County = "Macon" THEN
		County_Type = "Rural";
	ELSE IF County = "Madison" THEN
		County_Type = "Rural";
	ELSE IF County = "Martin" THEN
		County_Type = "Rural";
	ELSE IF County = "McDowell" THEN
		County_Type = "Rural";
	ELSE IF County = "Mecklenburg" THEN
		County_Type = "Urban";
	ELSE IF County = "Mitchell" THEN
		County_Type = "Rural";
	ELSE IF County = "Montgomery" THEN
		County_Type = "Rural";
	ELSE IF County = "Moore" THEN
		County_Type = "Rural";
	ELSE IF County = "Nash" THEN
		County_Type = "Rural";
	ELSE IF County = "New Hanover" THEN
		County_Type = "Urban";
	ELSE IF County = "Northampton" THEN
		County_Type = "Rural";
	ELSE IF County = "Onslow" THEN
		County_Type = "Rural";
	ELSE IF County = "Orange" THEN
		County_Type = "Suburban";
	ELSE IF County = "Pamlico" THEN
		County_Type = "Rural";
	ELSE IF County = "Pasquotank" THEN
		County_Type = "Rural";
	ELSE IF County = "Pender" THEN
		County_Type = "Rural";
	ELSE IF County = "Perquimans" THEN
		County_Type = "Rural";
	ELSE IF County = "Person" THEN
		County_Type = "Rural";
	ELSE IF County = "Pitt" THEN
		County_Type = "Suburban";
	ELSE IF County = "Polk" THEN
		County_Type = "Rural";
	ELSE IF County = "Randolph" THEN
		County_Type = "Rural";
	ELSE IF County = "Richmond" THEN
		County_Type = "Rural";
	ELSE IF County = "Robeson" THEN
		County_Type = "Rural";
	ELSE IF County = "Rockingham" THEN
		County_Type = "Rural";
	ELSE IF County = "Rowan" THEN
		County_Type = "Suburban";
	ELSE IF County = "Rutherford" THEN
		County_Type = "Rural";
	ELSE IF County = "Sampson" THEN
		County_Type = "Rural";
	ELSE IF County = "Scotland" THEN
		County_Type = "Rural";
	ELSE IF County = "Stanly" THEN
		County_Type = "Rural";
	ELSE IF County = "Stokes" THEN
		County_Type = "Rural";
	ELSE IF County = "Surry" THEN
		County_Type = "Rural";
	ELSE IF County = "Swain" THEN
		County_Type = "Rural";
	ELSE IF County = "Transylvania" THEN
		County_Type = "Rural";
	ELSE IF County = "Tyrrell" THEN
		County_Type = "Rural";
	ELSE IF County = "Union" THEN
		County_Type = "Suburban";
	ELSE IF County = "Vance" THEN
		County_Type = "Rural";
	ELSE IF County = "Wake" THEN
		County_Type = "Urban";
	ELSE IF County = "Warren" THEN
		County_Type = "Rural";
	ELSE IF County = "Washington" THEN
		County_Type = "Rural";
	ELSE IF County = "Watauga" THEN
		County_Type = "Rural";
	ELSE IF County = "Wayne" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilkes" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilson" THEN
		County_Type = "Rural";
	ELSE IF County = "Yadkin" THEN
		County_Type = "Rural";
	ELSE IF County = "Yancey" THEN
		County_Type = "Rural";
	RUN;
DATA nc2021;
	LENGTH County_Type $ 11; /*Here's the length statement*/
	SET nc2021;
	IF County = "Alamance" THEN
		County_Type ="Suburban";
	ELSE IF County = "Alexander" THEN
		County_Type = "Rural";
	ELSE IF County = "Alleghany" THEN
		County_Type = "Rural";
	ELSE IF County = "Anson" THEN
		County_Type = "Rural";
	ELSE IF County = "Ashe" THEN
		County_Type = "Rural";
	ELSE IF County = "Avery" THEN
		County_Type = "Rural";
	ELSE IF County = "Beaufort" THEN
		County_Type = "Rural";
	ELSE IF County = "Bertie" THEN
		County_Type = "Rural";
	ELSE IF County = "Bladen" THEN
		County_Type = "Rural";
	ELSE IF County = "Brunswick" THEN
		County_Type = "Rural";
	ELSE IF County = "Buncombe" THEN
		County_Type = "Suburban";
	ELSE IF County = "Burke" THEN
		County_Type = "Rural";
	ELSE IF County = "Cabarrus" THEN
		County_Type = "Suburban";
	ELSE IF County = "Caldwell" THEN
		County_Type = "Rural";
	ELSE IF County = "Camden" THEN
		County_Type = "Rural";
	ELSE IF County = "Carteret" THEN
		County_Type = "Rural";
	ELSE IF County = "Caswell" THEN
		County_Type = "Rural";
	ELSE IF County = "Catawba" THEN
		County_Type = "Suburban";
	ELSE IF County = "Chatham" THEN
		County_Type = "Rural";
	ELSE IF County = "Cherokee" THEN
		County_Type = "Rural";
	ELSE IF County = "Chowan" THEN
		County_Type = "Rural";
	ELSE IF County = "Clay" THEN
		County_Type = "Rural";
	ELSE IF County = "Cleveland" THEN
		County_Type = "Rural";
	ELSE IF County = "Columbus" THEN
		County_Type = "Rural";
	ELSE IF County = "Craven" THEN
		County_Type = "Rural";
	ELSE IF County = "Cumberland" THEN
		County_Type = "Suburban";
	ELSE IF County = "Currituck" THEN
		County_Type = "Rural";
	ELSE IF County = "Dare" THEN
		County_Type = "Rural";
	ELSE IF County = "Davidson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Davie" THEN
		County_Type = "Rural";
	ELSE IF County = "Duplin" THEN
		County_Type = "Rural";
	ELSE IF County = "Durham" THEN
		County_Type = "Urban";
	ELSE IF County = "Edgecombe" THEN
		County_Type = "Rural";
	ELSE IF County = "Forsyth" THEN
		County_Type = "Urban";
	ELSE IF County = "Franklin" THEN
		County_Type = "Rural";
	ELSE IF County = "Gaston" THEN
		County_Type = "Suburban";
	ELSE IF County = "Gates" THEN
		County_Type = "Rural";
	ELSE IF County = "Graham" THEN
		County_Type = "Rural";
	ELSE IF County = "Granville" THEN
		County_Type = "Rural";
	ELSE IF County = "Greene" THEN
		County_Type = "Rural";
	ELSE IF County = "Guilford" THEN
		County_Type = "Urban";
	ELSE IF County = "Halifax" THEN
		County_Type = "Rural";
	ELSE IF County = "Harnett" THEN
		County_Type = "Rural";
	ELSE IF County = "Haywood" THEN
		County_Type = "Rural";
	ELSE IF County = "Henderson" THEN
		County_Type = "Suburban";
	ELSE IF County = "Hertford" THEN
		County_Type = "Rural";
	ELSE IF County = "Hoke" THEN
		County_Type = "Rural";
	ELSE IF County = "Hyde" THEN
		County_Type = "Rural";
	ELSE IF County = "Iredell" THEN
		County_Type = "Suburban";
	ELSE IF County = "Jackson" THEN
		County_Type = "Rural";
	ELSE IF County = "Johnston" THEN
		County_Type = "Rural";
	ELSE IF County = "Jones" THEN
		County_Type = "Rural";
	ELSE IF County = "Lee" THEN
		County_Type = "Rural";
	ELSE IF County = "Lenoir" THEN
		County_Type = "Rural";
	ELSE IF County = "Lincoln" THEN
		County_Type = "Suburban";
	ELSE IF County = "Macon" THEN
		County_Type = "Rural";
	ELSE IF County = "Madison" THEN
		County_Type = "Rural";
	ELSE IF County = "Martin" THEN
		County_Type = "Rural";
	ELSE IF County = "McDowell" THEN
		County_Type = "Rural";
	ELSE IF County = "Mecklenburg" THEN
		County_Type = "Urban";
	ELSE IF County = "Mitchell" THEN
		County_Type = "Rural";
	ELSE IF County = "Montgomery" THEN
		County_Type = "Rural";
	ELSE IF County = "Moore" THEN
		County_Type = "Rural";
	ELSE IF County = "Nash" THEN
		County_Type = "Rural";
	ELSE IF County = "New Hanover" THEN
		County_Type = "Urban";
	ELSE IF County = "Northampton" THEN
		County_Type = "Rural";
	ELSE IF County = "Onslow" THEN
		County_Type = "Rural";
	ELSE IF County = "Orange" THEN
		County_Type = "Suburban";
	ELSE IF County = "Pamlico" THEN
		County_Type = "Rural";
	ELSE IF County = "Pasquotank" THEN
		County_Type = "Rural";
	ELSE IF County = "Pender" THEN
		County_Type = "Rural";
	ELSE IF County = "Perquimans" THEN
		County_Type = "Rural";
	ELSE IF County = "Person" THEN
		County_Type = "Rural";
	ELSE IF County = "Pitt" THEN
		County_Type = "Suburban";
	ELSE IF County = "Polk" THEN
		County_Type = "Rural";
	ELSE IF County = "Randolph" THEN
		County_Type = "Rural";
	ELSE IF County = "Richmond" THEN
		County_Type = "Rural";
	ELSE IF County = "Robeson" THEN
		County_Type = "Rural";
	ELSE IF County = "Rockingham" THEN
		County_Type = "Rural";
	ELSE IF County = "Rowan" THEN
		County_Type = "Suburban";
	ELSE IF County = "Rutherford" THEN
		County_Type = "Rural";
	ELSE IF County = "Sampson" THEN
		County_Type = "Rural";
	ELSE IF County = "Scotland" THEN
		County_Type = "Rural";
	ELSE IF County = "Stanly" THEN
		County_Type = "Rural";
	ELSE IF County = "Stokes" THEN
		County_Type = "Rural";
	ELSE IF County = "Surry" THEN
		County_Type = "Rural";
	ELSE IF County = "Swain" THEN
		County_Type = "Rural";
	ELSE IF County = "Transylvania" THEN
		County_Type = "Rural";
	ELSE IF County = "Tyrrell" THEN
		County_Type = "Rural";
	ELSE IF County = "Union" THEN
		County_Type = "Suburban";
	ELSE IF County = "Vance" THEN
		County_Type = "Rural";
	ELSE IF County = "Wake" THEN
		County_Type = "Urban";
	ELSE IF County = "Warren" THEN
		County_Type = "Rural";
	ELSE IF County = "Washington" THEN
		County_Type = "Rural";
	ELSE IF County = "Watauga" THEN
		County_Type = "Rural";
	ELSE IF County = "Wayne" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilkes" THEN
		County_Type = "Rural";
	ELSE IF County = "Wilson" THEN
		County_Type = "Rural";
	ELSE IF County = "Yadkin" THEN
		County_Type = "Rural";
	ELSE IF County = "Yancey" THEN
		County_Type = "Rural";
	RUN;

/********ADDING PREGNANCY-ASSOCIATED COLUMNS AND CREATING DATASETS BY YEAR AND ALL 5 YEARS*****

/*Create new columns to evaluate Diag1-Diag11 for pregnancy-associated ICD-10-CM codes*/
DATA nc2016; /*Evaluate dx for Pregnancy-Associated ICD-10 codes*/
			Pregnancy_Associated = 0;
           array diag[11] $ diag1-diag11;
           SET nc2016;
	do i=1 to 11;
	/*Pregnancy-Associated*/
	IF index(UpCase(diag[i]),'Z33') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'Z34') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'O') >0
		THEN Pregnancy_Associated=1;
		END;
		RUN;
DATA nc2017;
			Pregnancy_Associated = 0;
           array diag[11] $ diag1-diag11;
           SET nc2017;
	do i=1 to 11;
	/*Pregnancy-Associated*/
	IF index(UpCase(diag[i]),'Z33') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'Z34') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'O') >0
		THEN Pregnancy_Associated=1;
		END;
		RUN;
DATA nc2018;
			Pregnancy_Associated = 0;
           array diag[11] $ diag1-diag11;
           SET nc2018;
	do i=1 to 11;
	/*Pregnancy-Associated*/
	IF index(UpCase(diag[i]),'Z33') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'Z34') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'O') >0
		THEN Pregnancy_Associated=1;
		END;
		RUN;
DATA nc2019;
			Pregnancy_Associated = 0;
           array diag[11] $ diag1-diag11;
           SET nc2019;
	do i=1 to 11;
	/*Pregnancy-Associated*/
	IF index(UpCase(diag[i]),'Z33') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'Z34') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'O') >0
		THEN Pregnancy_Associated=1;
		END;
		RUN;
DATA nc2020;
			Pregnancy_Associated = 0;
           array diag[11] $ diag1-diag11;
           SET nc2020;
	do i=1 to 11;
	/*Pregnancy-Associated*/
	IF index(UpCase(diag[i]),'Z33') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'Z34') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'O') >0
		THEN Pregnancy_Associated=1;
		END;
		RUN;
DATA nc2021;
			Pregnancy_Associated = 0;
           array diag[26] $ diag1-diag26;
           SET nc2021;
	do i=1 to 26;
	/*Pregnancy-Associated*/
	IF index(UpCase(diag[i]),'Z33') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'Z34') >0
		THEN Pregnancy_Associated=1;
		ELSE IF index(UpCase(diag[i]), 'O') >0
		THEN Pregnancy_Associated=1;
		END;
		RUN;

PROC FORMAT; /*Changing format in Pregnancy_Associated & Pregnancy_Related to be Yes/No*/
	VALUE Pregnancy_Associatedf 0="No" 1="Yes";
	RUN;
	DATA nc2016; SET nc2016;
		FORMAT Pregnancy_Associated Pregnancy_Associatedf.;
		RUN;
			PROC PRINT data=nc2016 (obs = 15);
			TITLE "2016";
			RUN;
	DATA nc2017; SET nc2017;
		FORMAT Pregnancy_Associated Pregnancy_Associatedf.;
		RUN;
			PROC PRINT data=nc2017 (obs = 15);
			TITLE "2017";
			RUN;
	DATA nc2018; SET nc2018;
		FORMAT Pregnancy_Associated Pregnancy_Associatedf.;
		RUN;
			PROC PRINT data=nc2018 (obs = 15);
			TITLE "2018";
			RUN;
	DATA nc2019; SET nc2019;
		FORMAT Pregnancy_Associated Pregnancy_Associatedf.;
		RUN;
			PROC PRINT data=nc2019 (obs = 15);
			TITLE "2019";
			RUN;
	DATA nc2020; SET nc2020;
		FORMAT Pregnancy_Associated Pregnancy_Associatedf.;
		RUN;
			PROC PRINT data=nc2020 (obs = 15);
			TITLE "2020";
			RUN;
	DATA nc2021; SET nc2021;
		FORMAT Pregnancy_Associated Pregnancy_Associatedf.;
		RUN;
			PROC PRINT data=nc2021 (obs = 15);
			TITLE "2021";
			RUN;

DATA nc2016; /*Create simple disposition variable*/
	LENGTH Dispo_Simple $ 40;
	SET nc2016;
	IF DispositionText = 'Unknown' THEN Dispo_Simple =" ";
	ELSE IF DispositionText = 'Admitted to Psychiatric Unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to hospital floor bed' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
	ELSE IF DispositionText = 'Admitted to intermediate care/telemetry unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to medical intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
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

DATA nc2017; /*Create simple disposition variable*/
	LENGTH Dispo_Simple $ 40;
	SET nc2017;
	IF DispositionText = 'Unknown' THEN Dispo_Simple =" ";
	ELSE IF DispositionText = 'Admitted to Psychiatric Unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to hospital floor bed' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
	ELSE IF DispositionText = 'Admitted to intermediate care/telemetry unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to medical intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
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

DATA nc2018; /*Create simple disposition variable*/
	LENGTH Dispo_Simple $ 40;
	SET nc2018;
	IF DispositionText = 'Unknown' THEN Dispo_Simple =" ";
	ELSE IF DispositionText = 'Admitted to Psychiatric Unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to hospital floor bed' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
	ELSE IF DispositionText = 'Admitted to intermediate care/telemetry unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to medical intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
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

DATA nc2019; /*Create simple disposition variable*/
	LENGTH Dispo_Simple $ 40;
	SET nc2019;
	IF DispositionText = 'Unknown' THEN Dispo_Simple =" ";
	ELSE IF DispositionText = 'Admitted to Psychiatric Unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to hospital floor bed' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
	ELSE IF DispositionText = 'Admitted to intermediate care/telemetry unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to medical intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
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

DATA nc2020; /*Create simple disposition variable*/
	LENGTH Dispo_Simple $ 40;
	SET nc2020;
	IF DispositionText = 'Unknown' THEN Dispo_Simple =" ";
	ELSE IF DispositionText = 'Admitted to Psychiatric Unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to hospital floor bed' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
	ELSE IF DispositionText = 'Admitted to intermediate care/telemetry unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to medical intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
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

DATA nc2021; /*Create simple disposition variable*/
	LENGTH Dispo_Simple $ 40;
	SET nc2021;
	IF DispositionText = 'Unknown' THEN Dispo_Simple =" ";
	ELSE IF DispositionText = 'Admitted to Psychiatric Unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to hospital floor bed' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
	ELSE IF DispositionText = 'Admitted to intermediate care/telemetry unit' THEN Dispo_Simple = 'Admitted';
	ELSE IF DispositionText = 'Admitted to medical intensive care unit' THEN Dispo_Simple = 'Admitted to ICU';
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

/*Exported each file (right click - Export) and saved to encrypted Z:\NC DETECT\WRA*/

/*COMBINING DATASETS INTO ONE LARGE 2016-2020 DATASET*/

/*Reordering Variables, Sorting Observations by VisitID*/
DATA nc2016; /*Sorting all ED Visits for NC-resident WRA by VisitID*/
	RETAIN VisitID InternalTrackingID Age_Group Age Sex RaceDescription EthnicityDescription County_Type County Dispo_Simple DispositionText Pregnancy_Associated;
	SET nc2016;
	RUN;
		PROC SORT data=nc2016;
		by VisitID;
		RUN;
DATA nc2017;
	RETAIN VisitID InternalTrackingID Age_Group Age Sex RaceDescription EthnicityDescription County_Type County Dispo_Simple DispositionText Pregnancy_Associated;
	SET nc2017;
	RUN;
		PROC SORT data=nc2017;
		by VisitID;
		RUN;
DATA nc2018;
	RETAIN VisitID InternalTrackingID Age_Group Age Sex RaceDescription EthnicityDescription County_Type County Dispo_Simple DispositionText Pregnancy_Associated;
	SET nc2018;
	RUN;
		PROC SORT data=nc2018;
		by VisitID;
		RUN;
DATA nc2019;
	RETAIN VisitID InternalTrackingID Age_Group Age Sex RaceDescription EthnicityDescription County_Type County Dispo_Simple DispositionText Pregnancy_Associated;
	SET nc2019;
	RUN;
		PROC SORT data=nc2019;
		by VisitID;
		RUN;
DATA nc2020;
	RETAIN VisitID InternalTrackingID Age_Group Age Sex RaceDescription EthnicityDescription County_Type County Dispo_Simple DispositionText Pregnancy_Associated;
	SET nc2020;
	RUN;
		PROC SORT data=nc2020;
		by VisitID;
		RUN;
DATA nc2021;
	RETAIN VisitID InternalTrackingID Age_Group Age Sex RaceDescription EthnicityDescription County_Type County Dispo_Simple DispositionText Pregnancy_Associated;
	SET nc2021;
	RUN;
		PROC SORT data=nc2021;
		by VisitID;
		RUN;

DATA nc1617; /*Combining all WRA datasets*/
	SET nc2016 nc2017;
	RUN;
DATA nc161718;
	SET nc1617 nc2018;
	RUN;
DATA nc16171819;
	SET nc161718 nc2019;
	RUN;
DATA nc1617181920;
	SET nc16171819 nc2020;
	RUN;
DATA ncAllYears; /*contains all ED Visits for WRA residing in NC from 2016-2021*/
	SET nc1617181920 nc2021;
	RUN;

DATA preg2016; /*Creating Pregnancy-Related datasets*/
	SET nc2016;
	where Pregnancy_Associated=1;
	RUN;
DATA preg2017;
	SET nc2017;
	where Pregnancy_Associated=1;
	RUN;
DATA preg2018;
	SET nc2018;
	where Pregnancy_Associated=1;
	RUN;
DATA preg2019;
	SET nc2019;
	where Pregnancy_Associated=1;
	RUN;
DATA preg2020;
	SET nc2020;
	where Pregnancy_Associated=1;
	RUN;
DATA preg2021;
	SET nc2021;
	where Pregnancy_Associated=1;
	RUN;
DATA pregAllYears;
	SET ncAllYears;
	where Pregnancy_Associated=1;
	RUN;

PROC FREQ data=preg2021; /*TABLE 1. WRA Proportions and Counts of Demographics by Year*/
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES RaceDescription / Missing;
	TABLES EthnicityDescription / Missing;
	TABLES County_Type / Missing;
	TABLES InsuranceText / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2021 All WRA";
	RUN;

/*Exported all pregancy-associated ED visit datasets to 'Z:\NC DETECT\Pregnancy-Associated' */

PROC FREQ data=ncAllYears; /*TABLE 1. WRA Proportions and Counts of Demographics by Year*/
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES RaceDescription / Missing;
	TABLES EthnicityDescription / Missing;
	TABLES County_Type / Missing;
	TABLES InsuranceText / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2016-2021 All WRA";
	RUN;
	PROC FREQ data=ncAllYears;
		TABLES Age_Group RaceDescription EthnicityDescription County_Type InsuranceText Dispo_Simple;
		RUN;
PROC FREQ data=pregAllYears; /*TABLE 1. WRA Proportions and Counts of Demographics by Year*/
	TABLES Visit_Year / Missing;
	TABLES Age_Group / Missing;
	TABLES RaceDescription / Missing;
	TABLES EthnicityDescription / Missing;
	TABLES County_Type / Missing;
	TABLES InsuranceText / Missing;
	TABLES Dispo_Simple / Missing;
	TITLE "2016-2021 All Pregnancy-Associated";
	RUN;
	PROC FREQ data=pregAllYears;
		TABLES Age_Group RaceDescription EthnicityDescription County_Type InsuranceText Dispo_Simple;
		RUN;

/*Sum of charts with a Pregnancy-Associated Dx*/
PROC TABULATE data=pregAllYears; /*Sample Population Size*/
	VAR Pregnancy_Associated;
	TABLE Pregnancy_Associated all='All WRA 2016-2021';
	TITLE "Pregnancy-Associated Counts";
	RUN;

/*calculate frequencies for pregnancy-Associated & pregnancy-Related ED Visits*/
PROC FREQ data = ncAllYears; /*TABLE B. Counts of Associated/Related Visits by Year*/
	TABLES Pregnancy_Associated;   
	TITLE "2016-2021 All ED Visits for WRA";
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

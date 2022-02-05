*******************************************************************************;
*** This program is designed to generate the predictive model help the NGO  ***;
*** better identify the who will give and how much they will give in the    ***;
*** upcoming fundraising campaign. The program contains two models: a 		***;
*** Logistic Regression Model for predicting who will give, and a General	***;
*** Linear Model for the amount that they might give. Before generating the ***;
*** model, this program has used several methods to filter the variables	***;
*** including: Variable Clustering by second Eigenvalue & Greenacre method,	***;
*** Spearman & Hoeffding correlation test, and generating interactions.		***;
*** Also, the program split the data into two groups, training & testing,  	***;
*** in order to improve the model.											***;
*******************************************************************************;
*******************************************************************************;
*** This program is prepared by the Group 8 in MBAN 5210 by Nov 12th 2020	***;
*******************************************************************************;

/* Set the path of dataset */
%let path =/home/u48669152/Assignment;

/* Greenacre's algorithm from c3s2d2.sas */
%macro greenacre(dsn=,var=,target=);
	proc means data=&dsn nway noprint;
		class &var;
		var &target;
		output mean=prop out=level;
	run;
	title1 'Class Proportions and Frequencies';
	proc print data=level noobs;
		var &var prop _freq_;
	run;
	ods output clusterhistory=history;
	title1 'Ward''s Method';	
	proc cluster data=level method=ward outtree=dendrogram;
		freq _freq_;
		var prop;
		id &var;
	run;
	proc freq data=&dsn noprint;
		table &var*&target/chisq;
		output chisq out=chi(keep=_pchi_);
	run;
	data cutoff;
		if _n_=1 then set chi;
		set history;
		if numberofclusters > 1 then do;
			chisquare=_pchi_*rsquared;
			degfree=numberofclusters-1;
			logpvalue=logsdf('CHISQ',chisquare,degfree);
		end;
	run;
	title1 "Plot of the Log of the P-Value by Number of Clusters";
	proc sgplot data=cutoff;
		xaxis label="Number of Clusters";
		yaxis label="Log of P-Value";
		scatter y=logpvalue x=numberofclusters 
			/ markerattrs=(color=blue symbol=circlefilled);
	run;
	title1 "Number of Clusters Yielding the Minimum Log P-Value";
	proc sql;
		select NumberOfClusters into :ncl
		from cutoff
		having logpvalue=min(logpvalue);	
	quit;
	proc tree data=dendrogram nclusters=&ncl out=results noprint; 
		id &var;
	run;
	proc sort data=results;
		by clusname;
	run;
	title1 'Proposed Solution';
	proc print data=results noobs;
		by clusname;
		var &var;
	run;
%mend greenacre;

****************************************************************************;
*** Part 1: Generate the Model                                           ***;
****************************************************************************;

/* Import the data and fill in the missing value */
data Cortex_data;
	set "&path/hist2.sas7bdat"(rename=(Recency=intRecency Seniority=intSeniority));	
	/* Replace the missing value with 0 and NA */
	
	ARRAY Column_Name Frequency TotalGift MinGift MaxGift;
	DO OVER Column_Name ;
		IF Column_Name = "." 
		THEN Column_Name = 0;
	END;
		
	logSalary=log(salary + 1);
	logReferrals=log(Referrals + 1);
	logTotalGift=log(totalgift + 1);
	logMaxGift=log(Maxgift + 1);
	logMinGift=log(Mingift + 1);
	logAmtLastYear=log(AmtLastYear+1);
	
	Seniority=PUT(intSeniority,2.);
	if intSeniority='.' Then Seniority='NA';
	Recency=PUT(intRecency,2.);
	if intRecency='.' Then Recency='NA';
run;

/* Set relevant variables */
%let intervals = Age logSalary LogAmtLastYear logTotalGift logReferrals Frequency logMaxGift logMinGift;

/* Data splitting into train and valid */
proc sort data=Cortex_data out=Cortex_data_sorted;
	by GaveThisYear;
run;

proc surveyselect noprint data=Cortex_data_sorted seed=44444 samprate=.7
	stratumseed=restore outall out=sample;
	strata GaveThisYear;
run;

proc freq data=sample;
	tables GaveThisYear*selected;
run;

data cortex_data_train(drop=selected SelectionProb SamplingWeight)
	cortex_data_valid(drop=selected SelectionProb SamplingWeight);
	set sample;
	if selected=1 then output cortex_data_train;
	else output cortex_data_valid;
run;


/* Cluster for SeniorList --> 4 clusters: 0,1-2,3-6,7-10 */
%greenacre(dsn=cortex_data_train,var=SeniorList,target=GaveThisYear);
%greenacre(dsn=cortex_data_train,var=Seniority, target=GaveThisYear);


data cortex_data_train_collapse;
	set cortex_data_train;
	if SeniorList in (1,2) then SeniorList_1='1-2';
		else if SeniorList in (3,4,5,6) then SeniorList_1='3-6';
		else if SeniorList in (7,8,9,10) then SeniorList_1 ='7-10';
		else SeniorList_1='0';
		
	if seniority in (' 9','10') then seniority_1='9-10';
		else if Seniority in (' 1',' 2') then Seniority_1='1-2';
		else if seniority in (' 4',' 5',' 6') then Seniority_1='4-6';
		else if Seniority in (' 7',' 8') then Seniority_1='7-8';
		else Seniority_1=Seniority;

	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;
	
run;

data cortex_data_valid_collapse;
	set cortex_data_valid;
	if SeniorList in (1,2) then SeniorList_1='1-2';
		else if SeniorList in (3,4,5,6) then SeniorList_1='3-6';
		else if SeniorList in (7,8,9,10) then SeniorList_1 ='7-10';
		else SeniorList_1='0';
	if seniority in (' 9','10') then seniority_1='9-10';
		else if Seniority in (' 1',' 2') then Seniority_1='1-2';
		else if seniority in (' 4',' 5',' 6') then Seniority_1='4-6';
		else if Seniority in (' 7',' 8') then Seniority_1='7-8';
		else Seniority_1=Seniority;
		
	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;
run;


/* Variable Reduction*/
/* Numerical Variable Clustering */
ods select clustersummary rsquare;

proc varclus data=Cortex_data_train_collapse maxeigen=0.7 hi outtree=tree;
   var &intervals;
run;

/* Result:We get 5 cluster, from each cluster, we used the varibale with least 1-R**2 ratio */
%let reduced_interval = Age logAmtLastYear logSalary logReferrals logTotalGift; 

/* Screening Irrelevant Inputs */
ods select none;
ods output spearmancorr=spearman
           hoeffdingcorr=hoeffding;


/*Spearman & Hoeffding Correlation test */
/* Result: No variable shows weak association*/
proc corr data=Cortex_data_train_collapse spearman hoeffding;
   var GaveThisYear;
   with &reduced_interval;
run;


ods select all;
/* Adding the categorical variable, now we get the all qualified variables */
%let reduced = logSalary logAmtLastYear logTotalGift NbActivities_1 Salary Age LogReferrals SeniorList_1
		Woman Education City Contact Seniority_1 GaveLastYear;

/*Select the Interaction */
title1 'Determine P-Value for Entry and Retention';
proc sql;
	select 1-probchi(log(sum(GaveThisYear ge 0)),1) into :sl
	from Cortex_data_train_collapse;
quit;

/* Detect the Interactions */
/* Select Interaction will take around 10 mins */


/*proc logistic data=Cortex_data_train_collapse;
	class Woman(param=ref ref='1') Education(param=ref ref='University / College') 
   	City(param=ref ref='City') Seniorlist_1(param=ref ref='0')
   	Seniority_1(param=ref ref='NA') Contact(param=ref ref='0') 
   	GaveLastYear(param=ref ref='0') NbActivities_1(param=ref ref='0');
	model GaveThisYear(event='1')= &reduced
			LogTotalGift|NbActivities_1|LogSalary|logAmtLastYear|Age|Woman|Education|LogReferrals|
			City|Seniorlist_1|Seniority_1|Contact|GaveLastYear @2 / 
			include=13 clodds=pl selection=forward slentry=&sl;
run;
*/
/* Result: Add relevant interactions to the Model */ 
%let interactions = 		
Woman*Contact
Age*Contact
seniority_1*Contact	
Contact*NbActivities_1
City*Contact
logAmtLastYear*SeniorList_1
SeniorList_1*Contact
City*SeniorList_1
logReferrals*NbActivities_1
GaveLastYear
Education*City	
Education*Contact	
seniority_1*NbActivities_1
logTotalGift*Contact	
logSalary*Contact	
logReferrals*seniority_1	
logReferrals*Contact	
Contact*GaveLastYear	
SeniorList_1*NbActivities_1
GaveLastYear*NbActivities_1	
logTotalGift*seniority_1
SeniorList_1*seniority_1
logAmtLastYear*City
logReferrals*GaveLastYear
City*NbActivities_1;

/* Generate logistic Model for GaveThisYear and Score the model with validating Data*/
ods select roccurve;
proc logistic data=cortex_data_train_collapse outmodel=ModelP;
	class Woman(param=ref ref='1') Education(param=ref ref='University / College') 
   		City(param=ref ref='City') Seniorlist_1(param=ref ref='0') 
   		Contact(param=ref ref='0') GaveLastYear(param=ref ref='0')
   		Seniority_1(param=ref ref='NA') NbActivities_1(param=ref ref='0');
	model GaveThisYear(event='1')= &reduced &interactions/ 
   		clodds=pl selection=backward fast slstay=&sl hier=single; 
   	score data=cortex_data_valid_collapse fitstat out=cortex_scored_valid outroc=roc;
run;


/* Generate the model for Amount of Giving*/
Data Cortex_Given_train;
	set Cortex_data_train_collapse;
	if GaveThisYear NE 1 then delete;
run;

Data Cortex_Given_valid;
	set Cortex_data_valid_collapse;
	if GaveThisYear NE 1 then delete;
run;

/* Set relevant variables */
%let vars = Woman Age LogSalary Education City Seniorlist_1 NbActivities_1 LogReferrals LogTotalGift  
Contact GaveLastYear LogAmtLastYear seniority_1;

%let squared= LogSalary*LogSalary  Logtotalgift*Logtotalgift LogAmtLastYear*LogAmtLastYear LogReferrals*LogReferrals; 

/* Identify Multicollinearity */
/* Remove MaxGift */
/*
proc reg data=Cortex_Given_train;
	model AmtThisYear=&intervals/ vif collin collinoint;
	delete MaxGift;
run;
quit;
*/
	
/* Traditional GLM */
proc glmselect data=Cortex_Given_train valdata=cortex_given_valid;
    class Woman Education City Seniorlist_1 Contact GaveLastYear NbActivities_1
     	 Seniority_1 /param=glm ref=first;
    model AmtThisYear= &vars &squared
    Woman|Age|LogSalary|Education|City|SeniorList_1|NbActivities_1|LogReferrals|
    LogTotalGift|Contact|GaveLastYear|logAmtLastYear|Seniority_1 @2
 		/ selection=backward select=validate showpvalues choose=validate;
 	store work.ModelA;
run;
quit;


***************************************************************************;
*** Part 2 Apply the Model to two dataset, Contact & NoContact          ***;
***************************************************************************;


/* Import the Contact dataset and manipulate the data */
data Cortex_Contact;
	set "&path/score2_contact.sas7bdat" (rename=(Recency=intRecency Seniority=intSeniority));	
	/* Replace the missing value with 0 and NA*/
	ARRAY Column_Name Frequency TotalGift MinGift MaxGift;
	DO OVER Column_Name ;
		IF Column_Name = "." 
		THEN Column_Name = 0;
	END;
	Seniority=PUT(intSeniority,2.);
	if intSeniority='.' Then Seniority='NA';
	
	logSalary=log(salary + 1);
	logReferrals=log(Referrals + 1);
	logTotalGift=log(totalgift + 1);
	logAmtLastYear=log(AmtLastYear+1);
	
	/* Collapse SeniorList,Recency,Seniority	 */
	if SeniorList in (1,2) then SeniorList_1='1-2';
		else if SeniorList in (3,4,5,6) then SeniorList_1='3-6';
		else if SeniorList in (7,8,9,10) then SeniorList_1 ='7-10';
		else SeniorList_1='0';

	if seniority in (' 9','10') then seniority_1='9-10';
		else if Seniority in (' 1',' 2') then Seniority_1='1-2';
		else if seniority in (' 4',' 5',' 6') then Seniority_1='4-6';
		else if Seniority in (' 7',' 8') then Seniority_1='7-8';
		else Seniority_1=Seniority;

	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;
run;

/* Apply the model to Contact */
proc logistic inmodel=ModelP;
	score data=Cortex_Contact out=ResultContactP;
run;

proc plm restore=ModelA;
	score data=Cortex_Contact out=ResultContactA;
run;

/* Merge the Result and generate EC */
Data ResultContact;
	merge ResultContactP(rename=(p_1=PGivingContact)) 
	ResultContactA (rename=(predicted=PredContact));
	by ID;
	EC = PGivingContact * PredContact;
	keep ID PGivingContact PredContact EC;
run;
/* Import the NoContact dataset and manipulate the data */

data Cortex_NoContact;
	set "&path/score2_nocontact.sas7bdat"(rename=(Recency=intRecency Seniority=intSeniority));	
	/* Replace the missing value with 0 and NA */
	ARRAY Column_Name Frequency TotalGift MinGift MaxGift;
	DO OVER Column_Name ;
		IF Column_Name = "." 
		THEN Column_Name = 0;
	END;
	Seniority=PUT(intSeniority,2.);
	if intSeniority='.' Then Seniority='NA';
	
	logSalary=log(salary + 1);
	logReferrals=log(Referrals + 1);
	logTotalGift=log(totalgift + 1);
	logAmtLastYear=log(AmtLastYear+1);	
	/* Collapse SeniorList,Recency, Seniority	 */
	if SeniorList in (1,2) then SeniorList_1='1-2';
		else if SeniorList in (3,4,5,6) then SeniorList_1='3-6';
		else if SeniorList in (7,8,9,10) then SeniorList_1 ='7-10';
		else SeniorList_1='0';

	if seniority in (' 9','10') then seniority_1='9-10';
		else if Seniority in (' 1',' 2') then Seniority_1='1-2';
		else if seniority in (' 4',' 5',' 6') then Seniority_1='4-6';
		else if Seniority in (' 7',' 8') then Seniority_1='7-8';
		else Seniority_1=Seniority;

	if NbActivities >=4 then NbActivities_1='>=4';
		else NbActivities_1=NbActivities;
run;

/* Apply the model to the NoContact */
proc logistic inmodel=ModelP;
	score data=Cortex_NoContact out=ResultNoContactP;
run;

proc plm restore=ModelA;
	score data=Cortex_NoContact out=ResultNoContactA;
run;

/* Merge the Result and generate ENC */
Data ResultNoContact;
	merge ResultNoContactP(rename=(p_1=PGivingNoContact)) 
	ResultNoContactA (rename=(predicted=PredNoContact));
	by ID;
	ENC = PGivingNoContact * PredNoContact;
	keep ID PredNoContact PGivingNoContact ENC;
run;

/* Calculate the Uplift */
Data Result;
	merge ResultContact ResultNoContact;
	by ID;
	Uplift = EC - ENC;
run;

**********************************************************************;
*** Part 3: Choose the cutoff and who to contact                   ***;
**********************************************************************;

proc sort data=Result out=Result;
	by descending Uplift ;
run;

/* Choose the cutoff point
   Contact every person have the uplift over 25 */
Data Choose_to_contact;
	set Result;
	if Uplift > 30 then Chosen = 1;
	*if Chosen NE 1  then count + 1;
	*if uplift > 5 and count <= 60000 then Chosen = 1;
run;

/* Generate the csv file */
Data Chosen_ID;
	set Choose_to_contact;
	if Chosen NE 1 then delete;
	Keep ID;
run;

proc export data=Chosen_ID outfile="&path/Cortex_1.csv" 
	dbms=csv replace;
	putnames=no;
run;


**********************************************************************;
*** Part 4: Describe the poeple that is chosen to contact			**;
**********************************************************************;
proc sort data=cortex_data out=c_sort;
	by id;
run;

proc sort data=choose_to_contact out=cc_sort;
	by id;
run;

data rrr;
	merge c_sort cc_sort;
	by id;
	if chosen=1;
run;

proc freq data=rrr;
	tables woman education city;
run;


PPSItemAnalysis<-function(RespFileName, GroupInfoFileName){
  
  ### Load Libraries ###
  
  library(ltm)
  library(difNLR)
  library(difR)
  library(tidyverse)
  library(plyr)
  library(dplyr)
  library(readr)
  library(readxl)
  library(ShinyItemAnalysis)
  library(mirt)
  library(ggplot2)
  library(lordif)
  library(officer)
  library(magrittr)
  
   #-----------------------------------------------------------------------------------------------------------------------------
   # Read
   # Before
   # Running:
   #        
   #        This script assumes that (1) the user is working from the 'Documents' directory on a Windows PC (version 7 or later), 
   #        (2) assessment data files are located in the user's 'Downloads' folder, and (3) assessment data has been either
   #        downloaded from the Performance Matters assessment management platform or adjusted to fit the formats described below. 
   #
   #        Be sure to change the script's directory assignment and filename adjustment functions as necessary if you are
   #        working from a different configuration.
   #
   #        This script is built to analyze and produce reports on batches of assessment results. Each iteration of the script
   #        requires 3 files to exist prior to running: (1) DEMOGRAPHICS INFORMATION for all students taking the assessment, 
   #        (2) ASSESSMENT RESPONSE DATA from all students taking the assessment (as rows) and all assessment items (as columns),
   #        and (3) a TEST DETAILS file to be used for matching assessment ID numbers with each analyzed assessment item. If 
   #        your test details file is comprehensive for a given set of assessments, you can use the same test details file for
   #        every iteration of this script, even as you input different response and demographics data.
   #
   #        This script was originally written to accomodate a test results data export format that ended each response data
   #        file with '.ITEM'. For reference, an example filename using the presumed naming conventions for the default script
   #        might look like this: 15-16 GR9 DBQ HISTORY QUARTER TWO.ITEM . You may need to adjust the file read-in code to 
   #        accommodate your district's export format.
   #
   #        
   #        Presumed DEMOGRAPHICS INFORMATION file format is an Excel workbook (.xlsx), saved in the user's 'Downloads' directory,
   #        with one row per student and 9 variables in the following order:
   #
   #          1) 'Student Name' (format does not matter; this variable will be deleted)
   #          2) 'Student ID'
   #          3) 'Gender' (Factor Levels: 'M' or 'F') --> Gender
   #          4) 'Gifted' (Factor Levels: 'Yes' or 'No') --> Gifted Status
   #          5) 'Ethnicity' (Factor Levels: 'African American', 'Asian', 'Caucasian', 'Hispanic', 'Multi-Racial', 
   #                                         'Hawaiian / PI', 'American Indian')--> Ethnicity
   #          6) 'SWD' (Factor Levels: 'Yes' or 'No') --> Students with Disabilities flag
   #          7) 'LEP' (Factor Levels: 'Yes' or 'No') --> Limited English Proficiency flag
   #          8) 'ED' (Factor Levels: 'Yes' or 'No') --> Economically Disadvantaged flag
   #          9) Assessment Score Variable (name and format do not matter; this variable will be deleted)
   #
   #
   #        Presumed ASSESSMENT RESPONSE DATA file format is a comma separated values (.csv) file saved in the user's 'Downloads'
   #        directory with a top row indicating assessment item number, a second row indicating overall percent correct 
   #        per assessment item, and one row per student thereafter. All item score values should take on binary formatting, 
   #        with a '1' if the student answered correctly and a '0' if not. Variables (columns) are presumed to take the 
   #        following order (variable names do not matter here, as long as they are compatible with RStudio conventions):
   #
   #          1) Student name (format does not matter; this variable will be deleted)
   #          2) Student ID Number (must match Student ID numbers from Demographics Information file)
   #          3) Student Test Score (format does not matter; this variable will be deleted)
   #          4) Points Earned/Points Possible (format does not matter; this variable will be deleted)
   #          5) One variable per assessment item
   #        
   #             * To simplify use of this script, users with file formats that do no match what is described above may wish
   #               to insert placeholder columns with NA values for variables 1,3, and 4 above.
   #
   #        
   #        Presumed TEST DETAILS file format is a comma separated values (.csv) file named 'Test_Details.csv' saved in
   #        the 'Documents' directory with one row per test and a header row indicating names for the following variables:
   #        
   #          1) Test ID (Exact variable name does not matter)
   #          2) Test Title (Exact variable name does not matter; VALUES MUST MATCH test title(s) from the RespFileName argument)
   #          3) 'Assessment Category' (Not Used)
   #          4) 'Primary Course Type' (Not Used)
   #          5) 'Grade Levels' (Not Used)
   #          6) 'Performance Band' (Not Used)
   #          7) 'Has Results Flag' (Used if Present)
   #          8+) NA - None of these variables will be used
   #        
   #        * If you are not using exports from an assessment management platform (original script was written for exports from the
   #          Performance Matters assessment system) and you plan to run this script on multiple assessments, simply create a .csv
   #          file or dataframe with 2 variables--test ID and test name--for all active asssessments and assign it to the 
   #          'tlist_active' object below (comment out the 'tlist' object assignment line).
   #          
   #          Note again that your test names must be an exact copy or subset of the list of testnames from which 
   #          your 'RespFileName' argument was pulled.
   #
   #        * If you only want to run this script on one assessment, you can comment out the test details section and just
   #          assign your test ID number to the 'tID' object below.
   #
   #        * If your response data is encoded as polytomous (non-binary) with an answer key row, an adaptation of this loop (courtesy
   #          of Pittsburgh Public Schools DREA Department Intern Molly Rohrer) may be useful in converting the data to binary format:
   #                
   #                b<-orig_responses%>%select(colnames(orig_responses)[1])
   #                new<-as.data.frame(matrix(nrow = nrow(b), ncol = ncol(b)))
   #                for (row in 2:nrow(b)){
   #                  for (col in 1:ncol(b)){
   #                    ifelse(
   #                      b[row, col] == b[1, col], new_responses[row, col] <- 1, new_responses[row, col] <- 0)}}
   #
   #                new_responses<-new_responses[-1,]
   #
   #
   #_________________________________________________________________________________________________________________________________
  
  
  ############################################
  ### Create Required Objects for Analysis ###
  ############################################
  
   
  ## Read Assessment Response File and Demographics File, and assign 
  #   testname/testmatch objects for identifying this assessment later ##
   
  setwd("~/../Downloads")  
  testmatch<-str_replace(RespFileName,"[^A-z0-9)]+ITEM", "")
  responses<-read.csv(paste0(RespFileName,".csv"))
  groupinfo<-read_excel(paste0(GroupInfoFileName,".xlsx"))
  setwd("~/../Documents")  
  testname<-testmatch
  
  ## Read test details file ##
  tlist<-read.csv("~/Test_Details.csv", stringsAsFactors = FALSE)
  
  ## Filter tlist dataframe to include only those assessments that have associated student response results (see notes
  #  above if your file format differs) ##
  tlist_active<-tlist%>%filter(Has.Results.Flag=="true")
  colnames(tlist_active)[1:2]<-c("Test.ID","Test.Title")
  
  ## Create test ID number object ##
  tID<-tlist_active$Test.ID[str_trim(tlist_active$Test.Title, side = "both")==testmatch]
  
  
  
  #############################################################
  ### Clean Student Response File for Recordkeeping Output ###
  #############################################################
  
  
  ## Create a folder (in 'Documents' directory) named with the assessment title to store script outputs ##
  if(dir.exists(testname)==F){
    dir.create(testname)
  }
  
  ## Remove Item %Correct Row and Student Name, Percentage Correct, and PP/PE Columns ##
  r1<-responses[-1,c(1,5:ncol(responses))]
  
  ## Replace N/A Values with 0 (i.e. re-categorize unanswered items as incorrect). Adjust "N/A" if necessary to match your
  #  data's missing values encoding format. ##
  r1[r1=="N/A"]<-0
  
  ## Remove Unused Factor Levels ##
  r2<-droplevels(r1)
  
  ## Place Rubric-scored Items into a Separate Object ##
  r3<-matrix(nrow = nrow(r2))
  
  ## Initialize vector to store indices of non-binary assessment items from response info. dataframe ##
  l0<-c()
  
  ## Store non-binary assessment item response data in 'r3' object; store indicies in l0 object ##
  for(i in 2:ncol(r2)){
    if(max(length(unique(r2[,i])))>2&length(unique(r2[,i]))<30 | max(length(unique(r2[,i])))>2&length(unique(r2[,i]))==1){
      
      for(i in 1:ncol(r2)){
        if((length(unique(r2[,i]))>2&length(unique(r2[,i]))<30) | max(length(unique(r2[,i])))>2&length(unique(r2[,i]))==1){
          r3<-cbind(r3,r2[,i])
          l0<-c(l0,i)
          #r2<-r2[,-i]
        }}
      j<-c()
      # eliminate redundant entries in indexing object #
      for(i in 1:length(l0)){
        j[i]<-l0[i]
        j<-unique(j)
      }
      
    }}
  
  # This and the following section prevent errors for assessments with no rubric-scored items #
  r3<-matrix(nrow = nrow(r2))
  j<-unique(j)
  r3<-r2[,j[which(is.na(j)!=T)]]
  r2<-r2[,-j[which(is.na(j)!=T)]]
  
  r3<-cbind(c(rep(NA,nrow(as.data.frame(r3)))),r3)
  
  #Remove NA Row if it exists#
  np<-c()
  for(i in 1:ncol(r3)){
    if(unique(is.na(r3[,i]))=="TRUE"){
      nalist<-c(np,i)
    }
  }
  
  if(exists("nalist")==T){
    r4<-r3[,-nalist]
  }else{
    r4<-r3
  }
  
  
  ## Re-join rubric scored (r5) and binary (r6) item response dataframes with Student ID and add Test ID ##
  r5<-cbind(SID=responses[2:nrow(responses),1],TestID=c(rep(tID,nrow(as.data.frame(r4)))),r4)
  r6<-cbind(SID=r2[,1],TestID=c(rep(tID,nrow(r2))),r2[,2:ncol(r2)])
  
  ## Save Objects to CSV Files in the test name folder within the user's 'Documents' directory ##
  setwd("~/../Documents")
  
    # This redundant 'if' statement corrects for working directory issues on some devices #
  if(dir.exists(testname)==F){
    dir.create(testname)
  }
  
  write.csv(r6,paste0("~/",testname,"/",testname,".Responses.csv"), row.names=F)
  write.csv(r5,paste0("~/",testname,"/",testname,".RubricItems.Responses.csv"), row.names=F)
  
  
  ##################################################################
  ### Prepare Demographics Information Recordkeeping Output File ###
  ##################################################################
  
  ## Remove Student Name and Score Columns. Note: this section presumes the first (left-most) variable contains student names
  #   and the last (right-most) variable contains student scores for the given assessment. See file formatting notes at
  #   the beginning of this script for more information. ##
  
  g1<-groupinfo[,c(-1,-ncol(groupinfo))]
  
  ## Save object as a CSV in the test name folder within the user's 'Documents' directory ##
  setwd("~/../Documents")
  write.csv(g1,paste0("~/",testname,"/",testname,".GroupInfo.csv"), row.names = F)
  
  ##Return Directory to Documents##
  setwd("~/../")
  setwd(dir = "Documents")
  
  
  
  #########################################################################
  ### Clean Response Data and Demographics Information for DIF Analysis ###
  #########################################################################
  
  
  ### Read and clean Sample Data ###
  groups<-g1
  responses<-r2
  
    # Redundant 'if' statement to correct for working directory issues on some devices #
  if(dir.exists(testname)==F){
    dir.create(testname)
  }
  
  ## Ensure Student ID columns share the same name ##
  colnames(responses)[1]<-colnames(groups)[1]
  
  ## Create Key vector from Response Data ##
  key<-responses[1,]
  key<-key[2:length(key)]
  
  ## Create separate demographic identification vectors to use as DIF groups later ##
  gender<-groups$Gender
  gifted<-groups$Gifted
  ethnicity<-groups$Ethnicity
  disabilities<-groups$SWD
  LEP<-groups$LEP
  ED<-groups$ED
  
  ## Create Master Dataframe ##
  alldata<-join(groups,responses,by=colnames(responses)[1], type="inner", match="first")
  
  
  ## Create new dataframes for selected DIF analysis by demographic ##
  
    # First, ensure no NA values persist in response data (adjust for your data's default NA encoding)
  binresponses<-responses
  binresponses[binresponses=="N/A"]<-NA
  binresponses[binresponses=="N/A"]<-NA
  binresponses[is.na(binresponses)]<-0
  
   # For error checking: code to note factor levels of response data for each item:
  # for(i in ncol(binresponses)){
  #   factor(binresponses[i])
  # }
  
  
  # Note: You may need to adjust the 'group' variable categorization criteria below (e.g. 'M' for Male, 'F' for Female)
  #       according to the way demographics are encoded in your data.
  
  #Gender#
  datagender<-cbind(binresponses, gender)           # <-- Create response dataframe labeled by gender
  datagender<-datagender[,-1]                       # <-- Remove student ID variable
  colnames(datagender)[ncol(datagender)]<-"group"   # <-- Rename gender label as "group" for DIF analysis function later
  dfgender<-as.data.frame(datagender)%>%mutate(group=case_when(
    group=="M"~0,
    group=="F"~1,
    group!="M"&group!="F"~0
  ))                                                # <-- Re-code 'group' variable as binary (male = reference, female = focal)
  matgender<-as.matrix(dfgender)                    # <-- Re-create data as a matrix for DIF analysis function later
  
  
  #Ethnicity#
  dataethnicity<-cbind(binresponses, ethnicity)
  dataethnicity<-dataethnicity[,-1]
  colnames(dataethnicity)[ncol(dataethnicity)]<-"group"
  dfethnicity<-as.data.frame(dataethnicity)%>%mutate(group=case_when(
    group=="African American"~0,
    group!="African American"~1
  ))                              # <-- Re-code 'group' variable as binary (African American = reference, Other = focal)
  matethnicity<-as.matrix(dfethnicity)
  
  
  #Gifted#
  datagifted<-cbind(binresponses, gifted)
  datagifted<-datagifted[,-1]
  colnames(datagifted)[ncol(datagifted)]<-"group"
  dfgifted<-as.data.frame(datagifted)%>%mutate(group=case_when(
    group=="No"~0,
    group=="Yes"~1,
    group!="No"&group!="Yes"~0
  ))                              # <-- Re-code 'group' variable as binary (Non-gifted = reference, Gifted = focal)
  matgifted<-as.matrix(dfgifted)
  
  #SWD#
  dataSWD<-cbind(binresponses, disabilities)
  dataSWD<-dataSWD[,-1]
  colnames(dataSWD)[ncol(dataSWD)]<-"group"
  dfSWD<-as.data.frame(dataSWD)%>%mutate(group=case_when(
    group=="No"~0,
    group=="Yes"~1,
    group!="No"&group!="Yes"~0
  ))                              # <-- Re-code 'group' variable as binary (WITHOUT disabilities = reference, WITH disabilities = focal)
  matSWD<-as.matrix(dfSWD)
  
  #LEP#
  dataLEP<-cbind(binresponses,LEP)
  dataLEP<-dataLEP[,-1]
  colnames(dataLEP)[ncol(dataLEP)]<-"group"
  dfLEP<-as.data.frame(dataLEP)%>%mutate(group=case_when(
    group=="No"~0,
    group=="Yes"~1,
    group!="No"&group!="Yes"~0
  ))                              # <-- Re-code 'group' variable as binary (Non-LEP = reference, LEP = focal)
  matLEP<-as.matrix(dfLEP)
  
  #ED#
  dataED<-cbind(binresponses,ED)
  dataED<-dataED[,-1]
  colnames(dataED)[ncol(dataED)]<-"group"
  dataED<-as.data.frame(dataED)
  dfED<-dataED%>%mutate(group=case_when(
    group=="No"~0,
    group=="Yes"~1,
    group!="No"&group!="Yes"~0
  ))                              # <-- Re-code 'group' variable as binary (Non-ED = reference, ED = focal)
  matED<-as.matrix(dfED)
  
  ###################################################
  ### Prepare Additional Objects for DIF Analysis ###
  ###################################################
  
  ## Assign 'data' objects for each model ##
  
  genderdata <- matgender[, -ncol(matgender)]
  ethnicitydata <- matethnicity[, -ncol(matethnicity)]
  gifteddata <- matgifted[, -ncol(matgifted)]
  SWDdata <- matSWD[, -ncol(matSWD)]
  LEPdata <- matLEP[, -ncol(matLEP)]
  EDdata <- matED[, -ncol(matED)]
  
  
  ## Assign 'group' objects for each model ##
  
  gendergroup <- matgender[, "group"]
  ethnicitygroup <- matethnicity[, "group"]
  giftedgroup <- matgifted[, "group"]
  SWDgroup <- matSWD[, "group"]
  LEPgroup <- matLEP[, "group"]
  EDgroup <- matED[, "group"]
  
  
  ###############################################################################
  ### Run DIF Models for Each Group - Gender, Ethnicity, Gifted, SWD, LEP, ED ###
  ###############################################################################
  
    # If statements first test for sufficient sample size (>=400), then run DIF models #
  
  ##GENDER##
  if(length(gendergroup[gendergroup==1])>=400 & length(gendergroup[gendergroup==0])>=400){
    genderfit1PL <- difRaju(Data = genderdata, group = gendergroup, focal.name = 1, model = "1PL", p.adjust.method = "none", purify = T, alpha = .1, signed = F) 
    # Coefficients for all items 
    gender_tab_coef1PL <- genderfit1PL$itemParInit 
    
    #Create vectors of Raju's statistic and P-value for each item#
    genderRaj<-as.vector(genderfit1PL$RajuZ)
    genderPVal<-as.vector(genderfit1PL$adjusted.p)
    
    #Initialize dataframe with gender DIF stats; add indicator variable for DIF#
    dfinit<-data.frame(genderRaj, genderPVal)
    dfDIF<-dfinit%>%mutate(genderDIF=if_else(genderPVal<=.1,1,0))
    
    #Output PDF depicting plots of items with DIF by gender#
    if(genderfit1PL$DIFitems!="No DIF item detected"){
      genderDIFplots<-plotDIFirt(parameters = gender_tab_coef1PL, item =as.vector(genderfit1PL$DIFitems), test = "Raju") 
      pdf(file=paste0("~/",testname,"/",testname,".genderDIFplots.pdf"))
      genderDIFplots
    }
  }else{
    genderRaj<-c(rep(NA,ncol(binresponses)-1))
    genderPVal<-c(rep(NA,ncol(binresponses)-1))
    dfinit<-data.frame(genderRaj, genderPVal)
    dfDIF<-dfinit%>%mutate(genderDIF=c(rep(NA,nrow(dfinit))))
  }
  if(length(gendergroup[gendergroup==1])>=400 & length(gendergroup[gendergroup==0])>=400){
    if(genderfit1PL$DIFitems!="No DIF item detected"){
      dev.off()
    }
  }
  
  
  ##ETHNICITY##
  if(length(ethnicitygroup[ethnicitygroup==1])>=400 & length(ethnicitygroup[ethnicitygroup==0])>=400){
    ethnicityfit1PL <- difRaju(Data = ethnicitydata, group = ethnicitygroup, focal.name = 1, model = "1PL", p.adjust.method = "none", purify = T, alpha = .1) 

    # Coefficients for all items #
    ethnicity_tab_coef1PL <- ethnicityfit1PL$itemParInit 

    # Create vectors of Raju's statistic and P-value for each item #
    ethnicityRaj<-as.vector(ethnicityfit1PL$RajuZ)
    ethnicityPVal<-as.vector(ethnicityfit1PL$adjusted.p)
    
    # Bind ethnicity DIF statistics to output dataframe; add indicator variable for DIF #
    dfinit<-data.frame(dfDIF,ethnicityRaj,ethnicityPVal)
    dfDIF<-dfinit%>%mutate(ethnicityDIF=if_else(ethnicityPVal<=.1,1,0))
    
    # Output PDF depicting plots of items with DIF by ethnicity #
    if(ethnicityfit1PL$DIFitems!="No DIF item detected"){
      ethnicityDIFplots<-plotDIFirt(parameters = ethnicity_tab_coef1PL, item =as.vector(ethnicityfit1PL$DIFitems), test = "Raju") 
      pdf(file=paste0("~/",testname,"/",testname,".ethnicityDIFplots.pdf"))
      ethnicityDIFplots
    }
  }else{
    ethnicityRaj<-c(rep(NA,ncol(binresponses)-1))
    ethnicityPVal<-c(rep(NA,ncol(binresponses)-1))
    dfinit<-data.frame(dfDIF,ethnicityRaj, ethnicityPVal)
    dfDIF<-dfinit%>%mutate(ethnicityDIF=c(rep(NA,nrow(dfinit)))) 
  }
  if(length(ethnicitygroup[ethnicitygroup==1])>=400 & length(ethnicitygroup[ethnicitygroup==0])>=400){
    if(ethnicityfit1PL$DIFitems!="No DIF item detected"){
      dev.off()
    }
  }
  
  
  ##GIFTED##
  if(length(giftedgroup[giftedgroup==1])>=400 & length(giftedgroup[giftedgroup==0])>=400){
    giftedfit1PL <- difRaju(Data = gifteddata, group = giftedgroup, focal.name = 1, model = "1PL", p.adjust.method = "none", purify = T, alpha = .1) 
    
    # Coefficients for all items #
    gifted_tab_coef1PL <- giftedfit1PL$itemParInit 

    # Create vectors of Raju's statistic and P-value for each item #
    giftedRaj<-as.vector(giftedfit1PL$RajuZ)
    giftedPVal<-as.vector(giftedfit1PL$adjusted.p)
    
    # Bind gifted DIF statistics to output dataframe; add indicator variable for DIF #
    dfinit<-data.frame(dfDIF,giftedRaj,giftedPVal)
    dfDIF<-dfinit%>%mutate(giftedDIF=if_else(giftedPVal<=.1,1,0))
    
    # Output PDF depicting plots of items with DIF by gifted status #
    if(giftedfit1PL$DIFitems!="No DIF item detected"){
      giftedDIFplots<-plotDIFirt(parameters = gifted_tab_coef1PL, item =as.vector(giftedfit1PL$DIFitems), test = "Raju") 
      pdf(file=paste0("~/",testname,"/",testname,".giftedDIFplots.pdf"))
      giftedDIFplots
    }
  }else{
    giftedRaj<-c(rep(NA,ncol(binresponses)-1))
    giftedPVal<-c(rep(NA,ncol(binresponses)-1))
    dfinit<-data.frame(dfDIF, giftedRaj, giftedPVal)
    dfDIF<-dfinit%>%mutate(giftedDIF=c(rep(NA,nrow(dfinit)))) 
  }
  if(length(giftedgroup[giftedgroup==1])>=400 & length(giftedgroup[giftedgroup==0])>=400){
    if(giftedfit1PL$DIFitems!="No DIF item detected"){
      dev.off()
    }
  }
  
  
  ##SWD##
  if(length(SWDgroup[SWDgroup==1])>=400 & length(SWDgroup[SWDgroup==0])>=400){
    SWDfit1PL <- difRaju(Data = SWDdata, group = SWDgroup, focal.name = 1, model = "1PL", p.adjust.method = "none", purify = T, alpha = .1) 

    # Coefficients for all items #
    SWD_tab_coef1PL <- SWDfit1PL$itemParInit 
    
    # Create vectors of Raju's statistic and P-value for each item #
    SWDRaj<-as.vector(SWDfit1PL$RajuZ)
    SWDPVal<-as.vector(SWDfit1PL$adjusted.p)
    
    # Bind SWD DIF statistics to output dataframe; add indicator variable for DIF #
    dfinit<-data.frame(dfDIF,SWDRaj,SWDPVal)
    dfDIF<-dfinit%>%mutate(SWDDIF=if_else(SWDPVal<=.1,1,0))
    
    # Output PDF depicting plots of items with DIF by SWD status #
    if(SWDfit1PL$DIFitems!="No DIF item detected"){
      SWDDIFplots<-plotDIFirt(parameters = SWD_tab_coef1PL, item =as.vector(SWDfit1PL$DIFitems), test = "Raju") 
      pdf(file=paste0("~/",testname,"/",testname,".SWDDIFplots.pdf"))
      SWDDIFplots
    }
  }else{
    SWDRaj<-c(rep(NA,ncol(binresponses)-1))
    SWDPVal<-c(rep(NA,ncol(binresponses)-1))
    dfinit<-data.frame(dfDIF,SWDRaj, SWDPVal)
    dfDIF<-dfinit%>%mutate(SWDDIF=c(rep(NA,nrow(dfinit)))) 
  }
  if(length(SWDgroup[SWDgroup==1])>=400 & length(SWDgroup[SWDgroup==0])>=400){
    if(SWDfit1PL$DIFitems!="No DIF item detected"){
      dev.off()
    }
  }
  
  
  ##LEP##
  if(length(LEPgroup[LEPgroup==1])>=400 & length(LEPgroup[LEPgroup==0])>=400){
    LEPfit1PL <- difRaju(Data = LEPdata, group = LEPgroup, focal.name = 1, model = "1PL", p.adjust.method = "none", purify = T, alpha = .1) 

    # Coefficients for all items #
    LEP_tab_coef1PL <- LEPfit1PL$itemParInit 
    
    # Create vectors of Raju's statistic and P-value for each item #
    LEPRaj<-as.vector(LEPfit1PL$RajuZ)
    LEPPVal<-as.vector(LEPfit1PL$adjusted.p)
    
    # Bind LEP DIF statistics to output dataframe; add indicator variable for DIF #
    dfinit<-data.frame(dfDIF,LEPRaj,LEPPVal)
    dfDIF<-dfinit%>%mutate(LEPDIF=if_else(LEPPVal<=.1,1,0))
    
    # Output PDF depicting plots of items with DIF by LEP #
    if(LEPfit1PL$DIFitems!="No DIF item detected"){
      LEPDIFplots<-plotDIFirt(parameters = LEP_tab_coef1PL, item =as.vector(LEPfit1PL$DIFitems), test = "Raju") 
      pdf(file=paste0("~/",testname,"/",testname,".LEPDIFplots.pdf"))
      LEPDIFplots
    }
  }else{
    LEPRaj<-c(rep(NA,ncol(binresponses)-1))
    LEPPVal<-c(rep(NA,ncol(binresponses)-1))
    dfinit<-data.frame(dfDIF,LEPRaj, LEPPVal)
    dfDIF<-dfinit%>%mutate(LEPDIF=c(rep(NA,nrow(dfinit)))) 
  }
  if(length(LEPgroup[LEPgroup==1])>=400 & length(LEPgroup[LEPgroup==0])>=400){
    if(LEPfit1PL$DIFitems!="No DIF item detected"){
      dev.off()
    }
  }
  
  ##ED##
  if(length(EDgroup[EDgroup==1])>=400 & length(EDgroup[EDgroup==0])>=400){
    EDfit1PL <- difRaju(Data = EDdata, group = EDgroup, focal.name = 1, model = "1PL", p.adjust.method = "none", purify = T, alpha = .1) 
    
    # Coefficients for all items #
    ED_tab_coef1PL <- EDfit1PL$itemParInit 
    
    # Create vectors of Raju's statistic and P-value for each item #
    EDRaj<-as.vector(EDfit1PL$RajuZ)
    EDPVal<-as.vector(EDfit1PL$adjusted.p)
    
    # Bind ethnicity DIF statistics to output dataframe; add indicator variable for DIF #
    dfinit<-data.frame(dfDIF,EDRaj,EDPVal)
    dfDIF<-dfinit%>%mutate(EDDIF=if_else(EDPVal<=.1,1,0))
    
    # Output PDF depicting plots of items with DIF by ED status #
    if(EDfit1PL$DIFitems!="No DIF item detected"){
      EDDIFplots<-plotDIFirt(parameters = ED_tab_coef1PL, item =as.vector(EDfit1PL$DIFitems), test = "Raju") 
      pdf(file=paste0("~/",testname,"/",testname,".EDDIFplots.pdf"))
      EDDIFplots
    }
  }else{
    EDRaj<-c(rep(NA,ncol(binresponses)-1))
    EDPVal<-c(rep(NA,ncol(binresponses)-1))
    dfinit<-data.frame(dfDIF,EDRaj, EDPVal)
    dfDIF<-dfinit%>%mutate(EDDIF=c(rep(NA,nrow(dfinit)))) 
  }
  if(length(EDgroup[EDgroup==1])>=400 & length(EDgroup[EDgroup==0])>=400){
    if(EDfit1PL$DIFitems!="No DIF item detected"){
      dev.off()
    }
  }
  
  ############################################################################################
  ### Collect General (full population) IRT Characteristics for each item, using 3PL model ###
  ############################################################################################
  
    # Note: This script uses 1PL models for DIF analysis and 3PL models for general IRT characteristics. This
    #       accommodates sample-size challenges particular to the author's school district. Districts with larger 
    #       volumes of student response data may wish to use 2- or 3-parameter models for DIF analysis. Districts with 
    #       smaller volumes of student response data may wish to use 1-parameter models for all analysis.
  
  ## Clean up NA values in response data if necessary ##
  binresponses[binresponses=="N/A"]<-NA
  binresponses[binresponses=="N/A"]<-NA
  binresponses[is.na(binresponses)]<-0
  
  ## Create new dataframe with responses only (no SID) ##
  responsesanon<-binresponses[,2:ncol(binresponses)]

  ## Run 3PL model on all response data; create dataframe with alpha, beta, and c-value coefficients for all items ##
  threePM<-tpm(data = responsesanon,type = "latent.trait", IRT.param = T)
  coef3<-coefficients(threePM)
  coef3df<-as.data.frame(coef3)
  
  ## Create and add assessment ID and item number variables to dfout Object ##
  tIDvar<-c(rep(tID,nrow(coef3df)))
  itemnumber<-c(1:nrow(coef3df))
  dfout<-cbind("Test ID"=tIDvar,"Item Number"=itemnumber,coef3df,dfDIF)
  
  ## Overwrite DIF analysis information in output dataframe if sample size was insufficient for analysis ##
  if(length(gendergroup[gendergroup==1])<400 | length(gendergroup[gendergroup==0])<400){
    dfout$genderDIF<-"Insufficient Sample Size"
  }
  if(length(ethnicitygroup[ethnicitygroup==1])<400 | length(ethnicitygroup[ethnicitygroup==0])<400){
    dfout$ethnicityDIF<-"Insufficient Sample Size"
  }
  if(length(giftedgroup[giftedgroup==1])<400 | length(giftedgroup[giftedgroup==0])<400){
    dfout$giftedDIF<-"Insufficient Sample Size"
  }
  if(length(SWDgroup[SWDgroup==1])<400 | length(SWDgroup[SWDgroup==0])<400){
    dfout$SWDDIF<-"Insufficient Sample Size"
  }
  if(length(LEPgroup[LEPgroup==1])<400 | length(LEPgroup[LEPgroup==0])<400){
    dfout$LEPDIF<-"Insufficient Sample Size"
  }
  if(length(EDgroup[EDgroup==1])<400 | length(EDgroup[EDgroup==0])<400){
    dfout$EDDIF<-"Insufficient Sample Size"
  }
  
  ## Write DIF and general IRT analysis results to CSV in test name folder under user's 'Documents' directory ##
  write.csv(dfout, paste0("~/",testname,"/",testname,".DIF.Analysis.csv"), row.names = F)
  
  
  #_______________________________________________________________#
  #-----------------   End of Analysis Section   -----------------#
  #_______________________________________________________________#
  
  
  
  
  
  
  
  
  
  
  
  #################################################################
  ### Create Written Report Summary of Assessment Item Analysis ###
  #################################################################
  
  
  ##Create New Dataframe with item numbers as first column##
  ilist<-c(1:nrow(dfout))
  df0<-cbind(ilist,dfout)
  
  ##Create Binary Flags for Item Characterists to be Reviewed (Guessing, Difficulty, Discrimination)## 
  df0<-df0%>%mutate(GuessFlag=if_else(Gussng>.25,1,0),
                    DiffFlag=if_else((Dffclt>2|Dffclt<(-2)),1,0),
                    DiscFlag=if_else(Dscrmn<.5,1,0))
  
  ##Initiate Word Document##
  doc_out<-read_docx()
  
  ##Add Inital Report Text##
  doc_out<-doc_out%>%
    body_add_par("Assessment Analysis", style = "graphic title" )%>%
    body_add_par(testname, style = "graphic title")%>%
    body_add_par(paste0("Generated On: ", Sys.time()), style = "table title")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Summary", style = "heading 1")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("This report identifies assessment items flagged for review based on either (A) general population (all students tested) item response analysis characteristics or (B) differential item functioning (DIF) for one or more of the following demographic subgroupings:", style = "Normal")%>%
    body_add_par("     - Gender", style = "Normal")%>%
    body_add_par("     - Ethnicity", style = "Normal")%>%
    body_add_par("     - Gifted Status", style = "Normal")%>%
    body_add_par("     - Students with Disabilities", style = "Normal")%>%
    body_add_par("     - Limited English Proficiency", style = "Normal")%>%
    body_add_par("     - Economically Disadvantaged", style = "Normal")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("The table below includes a summary of all flagged items and categories (as above, General Item Response Issues, Differential Item Functioning, or both). The corresponding sections that follow present further detail regarding each item's response characteristics or degree of DIF.", style = "Normal")%>%
    body_add_par("", style = "Normal")
  
  ##Create Summary Labels in Dataframe, Generate New DF for Summary Section##
  df0<-df0%>%mutate(AnyDIF=if_else(
    genderDIF==1 | ethnicityDIF ==1 | giftedDIF==1 | SWDDIF==1 | LEPDIF==1 | EDDIF==1, 1, 0
  ))%>%
    mutate(AnyGeneral=if_else(
      GuessFlag==1 | DiffFlag==1 | DiscFlag==1, 1, 0
    ))%>%
    mutate(Flag_Type=case_when(
      AnyDIF==1 & AnyGeneral==1~"General & DIF",
      AnyDIF==0 & AnyGeneral==1~"General",
      AnyDIF==1 & AnyGeneral==0~"DIF",
      AnyDIF==0 & AnyGeneral==0~"NA"
    ))
  
  df0$Flag_Type[df0$Flag_Type=="NA"]<-NA
  
  dfsum<-df0%>%filter(is.na(Flag_Type)==F)%>%dplyr::select(ilist,Flag_Type)
  sumnames<-c("Item Number", "Flag Type")
  colnames(dfsum)<-sumnames
  
  ## Add Summary Flags Dataframe to Document as Table ##
  doc_out<-doc_out%>%
    body_add_table(dfsum, style = "table_template")%>%
    body_add_par("", style="Normal")%>%
    body_add_par("", style="Normal")%>%
    slip_in_text("Recommended Use of This Report", style = "strong", pos = "after")%>%
    body_add_par("As you review the analysis in sections 2 and 3, make note of test items that warrant concern and circle the basis for intervention in the table above. For example, if item 6 was flagged for both 'General', due to low difficulty, and 'DIF', but the DIF was minimal, you might circle 'General' but not 'DIF' in the table above so that when you review the test item content, you remember to examine qualities of the question that may have affected difficulty, but you avoid spending needless time exploring causes for DIF if the degree of DIF was in fact insubstantial.", style = "Normal")
  
  #############################################################
  ################### GENERAL ITEM ANALYSIS ################### 
  #############################################################
  
  #############
  ### INTRO ###
  #############
  
  doc_out<-doc_out%>%
    body_add_par("General Item Analysis", style = "heading 1")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("This section flags items for review based on discrimination (alpha value), difficulty (beta value), or aberrant response parameters (c value), as obtained from results using all student responses (i.e. prior to subgrouping for DIF analysis). Please review the listed items in each category.")%>%
    body_add_par("", style = "Normal")
  
  
  ######################
  ### DISCRIMINATION ###
  ######################
  
  ## Create Section Header and Explanation Text ##
  
  doc_out<-doc_out%>%
    body_add_par("Discrimination", style = "heading 2")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Items flagged here have weak 'discrimination' parameters, as estimated through a 3-parameter latent trait model. Correct answers for these items are generally ineffective for determining a given student's ability level (i.e. correct vs. incorrect responses do not accurately indicate students' content mastery).")%>%
    body_add_par("", style ="Normal")
  
  ## Create List of Discrimination Flag Items ##
  discvec<-df0%>%filter(DiscFlag==1)%>%pull(ilist)
  
  ## Create Dataframe of Discrimination Flag Items and Statistics ##
  dfdisc<-df0%>%filter(DiscFlag==1)%>%dplyr::select(ilist, Dscrmn)
  discnames<-c("Item Number", "Discrimination Score")
  colnames(dfdisc)<-discnames
  
  noflag<-"NO ITEMS FLAGGED!"
  
  if(length(discvec)>=1){
    ## Create Temporary Image File for Discrimination Flags Plot ##
    srcdisc <- tempfile(fileext = ".png")
    png(filename = srcdisc, width = 5, height = 4, units = 'in', res = 300)
    plot.tpm(threePM, type = "ICC", items = discvec)
    dev.off()
    
    ## Add Discrimination Flag Item List and Plots to Document ##
    disc1<-NULL
    
    for(i in 1:length(discvec)){
      disc1<-paste0(disc1,"  ",discvec[i])
    }
    
    doc_out<-doc_out%>%
      body_add_par(paste0("Item numbers: ", disc1), style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("Item Statistics: ", style = "Normal")%>%
      body_add_table(dfdisc, style = "table_template")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("Item Characteristic Curves for Flagged Items:", style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_img(src = srcdisc, width = 5, height = 4, style = "centered")
  } else
  {
    doc_out<-doc_out%>%
      body_add_par(paste0("Item numbers: ", noflag), style = "Normal")%>%
      body_add_par("", style = "Normal")
  }
  
  
  ##################
  ### DIFFICULTY ###
  ##################
  
  ## Create Section Header and Explanation Text ##
  
  doc_out<-doc_out%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Difficulty", style = "heading 2")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Items flagged here measure student ability more than 2 standard deviations away from the mean (i.e. the question is informative about a maximum of approximately 5% of students who complete this assessment). If an item's measured difficulty in the following table is above or below what was intended, please review the question content and answer options.", style = "Normal")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("***(Note: difficulty metrics displayed here may vary somewhat from the difficulty values presented in other assessment analysis platforms (e.g. the Unify/Performance Matters system in the original author's school district) due to a difference in parameters/degrees of freedom permitted in one model vs. the other (for those interested, this analysis uses a 3-parameter (3PL) model to allow for bottom-asymptote aberrant responses (often called 'guessing') while other systems may use , for example, a 2PL model, which forces the bottom-asymptote value to 0 for all items). In nearly all cases, items flagged as >|2| standard deviations here are also identified as >|2| standard deviations in other analytical programs, even if the specific numbers differ.)***", style = "Normal")%>%
    body_add_par("", style ="Normal")
  
  ## Create List of Difficulty Flag Items ##
  diffvec<-df0%>%filter(DiffFlag==1)%>%pull(ilist)
  
  ## Create Dataframe of Difficulty Flag Items and Statistics ##
  dfdiff<-df0%>%filter(DiffFlag==1)%>%dplyr::select(ilist, Dffclt)
  diffnames<-c("Item Number", "Difficulty (beta) Score")
  colnames(dfdiff)<-diffnames
  
  if(length(diffvec)>=1){
    
    ## Create Temporary Image File for Difficulty Flags Plot ##
    srcdiff <- tempfile(fileext = ".png")
    png(filename = srcdiff, width = 5, height = 4, units = 'in', res = 300)
    plot.tpm(threePM, type = "ICC", items = diffvec)
    dev.off()
    
    ## Add Difficulty Flag Item List and Plots to Document ##
    diff1<-NULL
    
    for(i in 1:length(diffvec)){
      diff1<-paste0(diff1,"  ",diffvec[i])
    }
    
    doc_out<-doc_out%>%
      body_add_par(paste0("Item numbers: ", diff1), style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("Item Statistics: ", style = "Normal")%>%
      body_add_table(dfdiff, style = "table_template")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("Item Characteristic Curves for Flagged Items:", style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_img(src = srcdiff, width = 5, height = 4, style = "centered")
  } else {
    doc_out<-doc_out%>%
      body_add_par(paste0("Item numbers: ", noflag), style = "Normal")%>%
      body_add_par("", style = "Normal")
  }
  
  #########################
  ### ABERRANT RESPONSE ###
  #########################
  
  ## Create Section Header and Explanation Text ##
  
  doc_out<-doc_out%>%
    body_add_par("Aberrant Response", style = "heading 2")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Items flagged here have an abberant response parameter greater than .25. This indicates that students with the lowest possible ability level (i.e. no content knowledge whatsoever) had a >25% chance of answering the item correctly. Please review the flagged items.")%>%
    body_add_par("", style ="Normal")
  
  ## Create List of A.R. Flag Items ##
  gvec<-df0%>%filter(GuessFlag==1)%>%pull(ilist)
  
  ## Create Dataframe of Guess Flag Items and Statistics ##
  dfguess<-df0%>%filter(GuessFlag==1)%>%dplyr::select(ilist, Gussng)
  guessnames<-c("Item Number", "Aberrant Response Score")
  colnames(dfguess)<-guessnames
  
  if(length(gvec)>=1){
    
    ## Create Temporary Image File for Guess Flags Plot ##
    srcguess <- tempfile(fileext = ".png")
    png(filename = srcguess, width = 5, height = 4, units = 'in', res = 300)
    plot.tpm(threePM, type = "ICC", items = gvec)
    dev.off()
    
    ## Add Guess Flag Item List and Plots to Document ##
    g1<-NULL
    
    for(i in 1:length(gvec)){
      g1<-paste0(g1,"  ",gvec[i])
    }
    
    doc_out<-doc_out%>%
      body_add_par(paste0("Item numbers: ", g1), style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("Item Statistics: ", style = "Normal")%>%
      body_add_table(dfguess, style = "table_template")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("Item Characteristic Curves for Flagged Items:", style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_img(src = srcguess, width = 5, height = 4, style = "centered")
  } else {
    doc_out<-doc_out%>%
      body_add_par(paste0("Item numbers: ", noflag), style = "Normal")%>%
      body_add_par("", style = "Normal")
  }
  
  
  
  
  ####################################################
  ################### DIF ANALYSIS ################### 
  ####################################################
  
  
  #############
  ### INTRO ###
  #############
  
  ## Create Section Header and Intro Paragraph ##
  
  doc_out<-doc_out%>%
    body_add_par("Differential Item Functioning (DIF) Analysis", style = "heading 1")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Summary", style = "heading 2")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Items described in this section have been flagged for exhibiting DIF. Reviewers should note that the analysis behind this program raises a DIF flag if the difference between subgroup responses for two given students with the same level of content mastery is statistically significant. That is, items are first identified if they exhibit low probability of the observed DIF happening by chance (i.e. low statistical p-value (not to be confused with the 'percent correct' p-value used in some assessment management platforms)), as opposed to the actual direction or degree of DIF. For any flagged item, a visualization of the DIF is provided below; ", style = "Normal")%>%
    slip_in_text("reviewers should examine the visualizations for flagged items to determine whether the degree of DIF is problematic. If so, item content should be subject to review.", style = "strong", pos = "after")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("The table below summarizes items flagged for DIF, as well as the subgroup associated with the given flag.")%>%
    body_add_par("", style = "Normal")
  
  ## Create Dataframe for Summary Table ##
  
  dfDIFsum<-df0%>%
    filter(AnyDIF==1)%>%
    mutate(
      genderDIF=if_else(genderDIF==1,"Gender","0"),
      ethnicityDIF=if_else(ethnicityDIF==1, "Ethnicity","0"),
      giftedDIF=if_else(giftedDIF==1, "Gifted","0"),
      SWDDIF=if_else(SWDDIF==1, "SWD","0"),
      LEPDIF=if_else(LEPDIF==1, "LEP","0"),
      EDDIF=if_else(EDDIF==1, "ED", "0")
    )%>%
    mutate(DIFsum=str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_remove_all(paste(genderDIF,ethnicityDIF,giftedDIF,SWDDIF,LEPDIF,EDDIF),"0"),"[//> //<]",", "),"([^A-z][,]|[,.,])","")," ",", "), "(([,].$)|(^[,]))", "")
    )%>%
    dplyr::select(ilist,DIFsum)
  
  DIFsumnames<-c("Item Number", "Flag")
  colnames(dfDIFsum)<-DIFsumnames
  
  ## Insert Summary Table into Document ##
  
  #Create List of Categories with Sample Size below 400#
  
  l1<-list()
  for(i in c("gender", "ethnicity", "gifted", "SWD", "LEP", "ED")){
    if(df0[2,paste0(i,"DIF")]=="Insufficient Sample Size"){
      l1[i]<-i
    }
    
  }
  
  h1<-c()
  
  for(i in 1:length(names(l1))){
    h1<-paste(h1,names(l1)[i])
  }
  
  #Add summary flag table if DIF items detected; display sample size message if appropriate#
  
  if(dim(dfDIFsum)[1]>0){
    doc_out<-doc_out%>%
      body_add_table(dfDIFsum, style = "table_template")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      slip_in_text(paste0("Note: Sample size among the following subgroups was insufficient to identify DIF (or lack of DIF): ",str_replace_all(str_replace_all(h1,"[\\>\\s]",", "),"^[,]",""), "."))%>%
      body_add_par("", style="Normal")%>%
      body_add_par("", style = "Normal")
    
  } else{
    doc_out<-doc_out%>%
      body_add_par("NO ITEMS FLAGGED!", style="Normal")
    if((length(gendergroup[gendergroup==1])<400 | length(gendergroup[gendergroup==0])<400) &
       (length(ethnicitygroup[ethnicitygroup==1])<400 | length(ethnicitygroup[ethnicitygroup==0])<400) &
       (length(giftedgroup[giftedgroup==1])<400 | length(giftedgroup[giftedgroup==0])<400) &
       (length(LEPgroup[LEPgroup==1])<400 | length(LEPgroup[LEPgroup==0])<400) &
       (length(SWDgroup[SWDgroup==1])<400 | length(SWDgroup[SWDgroup==0])<400) &
       (length(EDgroup[EDgroup==1])<400 | length(EDgroup[EDgroup==0])<400)
    ){
      doc_out<-doc_out%>%
        body_add_par("Sample size for all subgroups is insufficient to identify DIF (or lack of DIF).", style = "Normal")%>%
        body_add_par("", style = "Normal")%>%
        body_add_par("", style = "Normal")
    }
  }
  
  
  ##################
  ### Gender DIF ###
  ##################
  
  ## Prepare Item List and Image Objects for Printing in Document ##
  
  #Item Vector#
  genderDIFvec<-df0%>%filter(genderDIF==1)%>%pull(ilist)
  
  #Item List#
  gend1<-NULL
  
  for(i in 1:length(genderDIFvec)){
    gend1<-paste0(gend1,"  ",genderDIFvec[i])
  }
  
  #Image Files#
  if(length(genderDIFvec>0)){
    for(i in 1:length(genderDIFvec)){
      png(filename = paste0("~/",testname,"/genderDIFplot",i,".png"), width = 5, height = 4, units = 'in', res = 300)
      print(plotDIFirt(parameters = gender_tab_coef2PL, item = genderDIFvec[i], test = "Raju"))
      dev.off()
    }
  }
  
  ## Write Introductory Text ##
  doc_out<-doc_out%>%
    body_add_par("Gender", style = "heading 2")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Items flagged in this section have exhibited differential item functioning (DIF) by gender. That is, two students of the same level of content mastery but different genders perform differently on the item in question. As mentioned above, items are initially flagged based upon the statistical significance--not the degree or direction--of the DIF. Therefore, it is important that reviewers examine the charts below to determine whether the degree of DIF is substantial enough to warrant further evaluation of the assessment item.", style = "Normal")%>%
    body_add_par("", style = "Normal")
  
  
  ## Write Item List and Images/NoDIF Message, as Appropriate. NOTE: Users who changed reference/focal group
  #  assignments in the analysis section will need to adjust the wording in this section to avoid misleading readers! ##    
  if(sum(genderDIFvec)>0){
    doc_out<-doc_out%>%
      body_add_par("The following items have been flagged for DIF based on gender. In the charts that follow, 'Reference Group' refers to male students; 'Focal Group' refers to female students.", style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par(paste0("Item numbers: ", gend1), style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("DIF Plots: ", style = "Normal")%>%
      body_add_par("", style = "Normal")
    
    for(i in 1:length(genderDIFvec)){
      srci<-paste0("~/",testname,"/genderDIFplot",i,".png")
      slip_in_img(doc_out, srci, width = 2.5, height = 2, pos = "after")
      slip_in_text(doc_out, "                               ", pos = "after")
    }
  }else{
    doc_out<-doc_out%>%
      body_add_par("NO DIF DETECTED (Or insufficient sample size)!")%>%
      body_add_par("", style = "Normal")
    
  }
  
  
  #####################
  ### Ethnicity DIF ###
  #####################
  
  ## Prepare Item List and Image Objects for Printing in Document ##
  
  #Item Vector#
  ethnicityDIFvec<-df0%>%filter(ethnicityDIF==1)%>%pull(ilist)
  
  #Item List#
  eth1<-NULL
  
  for(i in 1:length(ethnicityDIFvec)){
    eth1<-paste0(eth1,"  ", ethnicityDIFvec[i])
  }
  
  #Image Files#
  if(length(ethnicityDIFvec)>0){
    for(i in 1:length(ethnicityDIFvec)){
      png(filename = paste0("~/",testname,"/ethnicityDIFplot",i,".png"), width = 5, height = 4, units = 'in', res = 300)
      print(plotDIFirt(parameters = ethnicity_tab_coef2PL, item = ethnicityDIFvec[i], test = "Raju"))
      dev.off()
    }
  }
  
  ## Write Introductory Text ##
  doc_out<-doc_out%>%
    body_add_par("Ethnicity", style = "heading 2")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Items flagged in this section have exhibited differential item functioning (DIF) by ethnicity. That is, two students of the same level of content mastery but different ethnicities perform differently on the item in question. As mentioned above, items are initially flagged based upon the statistical significance--not the degree or direction--of the DIF. Therefore, it is important that reviewers examine the charts below to determine whether the degree of DIF is substantial enough to warrant further evaluation of the assessment item.", style = "Normal")%>%
    body_add_par("", style = "Normal")
  
  
  ## Write Item List and Images/NoDIF Message, as Appropriate. NOTE: Users who changed reference/focal group
  #  assignments in the analysis section will need to adjust the wording in this section to avoid misleading readers!
  #  Additionally, users with different capacity for analysis based on sample size will want to remove or adjust the
  #  note concerning selection of ethnicity for analysis. ##    
  if(sum(ethnicityDIFvec)>0){
    doc_out<-doc_out%>%
      body_add_par("The following items have been flagged for DIF based on ethnicity. In the charts that follow, 'Reference Group' refers to African American students; 'Focal Group' refers to non-African American students. (Due to limited sample size among student responses, DIF analysis cannot be accurately conducted based on other ethnicities at this time.)", style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par(paste0("Item numbers: ", eth1), style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("DIF Plots: ", style = "Normal")%>%
      body_add_par("", style = "Normal")
    
    for(i in 1:length(ethnicityDIFvec)){
      srci<-paste0("~/",testname,"/ethnicityDIFplot",i,".png")
      slip_in_img(doc_out, srci, width = 2.5, height = 2, pos = "after")
      slip_in_text(doc_out, "                               ", pos = "after")
    }
  }else{
    doc_out<-doc_out%>%
      body_add_par("NO DIF DETECTED (Or insufficient sample size)!")%>%
      body_add_par("", style = "Normal")
    
  }
  
  
  
  ##################
  ### Gifted DIF ###
  ##################
  
  ## Prepare Item List and Image Objects for Printing in Document ##
  
  #Item Vector#
  giftedDIFvec<-df0%>%filter(giftedDIF==1)%>%pull(ilist)
  
  #Item List#
  gifted1<-NULL
  
  for(i in 1:length(giftedDIFvec)){
    gifted1<-paste0(gifted1,"  ", giftedDIFvec[i])
  }
  
  #Image Files#
  if(length(giftedDIFvec)>0){
    for(i in 1:length(giftedDIFvec)){
      png(filename = paste0("~/",testname,"/giftedDIFplot",i,".png"), width = 5, height = 4, units = 'in', res = 300)
      print(plotDIFirt(parameters = gifted_tab_coef2PL, item = giftedDIFvec[i], test = "Raju"))
      dev.off()
    }
  }
  
  ## Write Introductory Text ##
  doc_out<-doc_out%>%
    body_add_par("Gifted", style = "heading 2")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Items flagged in this section have exhibited differential item functioning (DIF) by gifted status. That is, for two students with the same level of content mastery where one is labeled 'gifted' and the other is not, the students perform differently on the item in question. As mentioned above, items are initially flagged based upon the statistical significance--not the degree or direction--of the DIF. Therefore, it is important that reviewers examine the charts below to determine whether the degree of DIF is substantial enough to warrant further evaluation of the assessment item.", style = "Normal")%>%
    body_add_par("", style = "Normal")
  
  
  ## Write Item List and Images/NoDIF Message, as Appropriate. NOTE: Users who changed reference/focal group
  #  assignments in the analysis section will need to adjust the wording in this section to avoid misleading readers! ##    
  if(sum(giftedDIFvec)>0){
    doc_out<-doc_out%>%
      body_add_par("The following items have been flagged for DIF based on gifted status. In the charts that follow, 'Reference Group' refers to students NOT labeled as gifted; 'Focal Group' refers to students labeled as gifted.", style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par(paste0("Item numbers: ", gifted1), style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("DIF Plots: ", style = "Normal")%>%
      body_add_par("", style = "Normal")
    
    for(i in 1:length(giftedDIFvec)){
      srci<-paste0("~/",testname,"/giftedDIFplot",i,".png")
      slip_in_img(doc_out, srci, width = 2.5, height = 2, pos = "after")
      slip_in_text(doc_out, "                               ", pos = "after")
    }
  }else{
    doc_out<-doc_out%>%
      body_add_par("NO DIF DETECTED (Or insufficient sample size)!")%>%
      body_add_par("", style = "Normal")
    
  }
  
  
  
  ###############
  ### SWD DIF ###
  ###############
  
  ## Prepare Item List and Image Objects for Printing in Document ##
  
  #Item Vector#
  SWDDIFvec<-df0%>%filter(SWDDIF==1)%>%pull(ilist)
  
  #Item List#
  SWD1<-NULL
  
  for(i in 1:length(SWDDIFvec)){
    SWD1<-paste0(SWD1,"  ", SWDDIFvec[i])
  }
  
  #Image Files#
  if(length(SWDDIFvec)>0){
    for(i in 1:length(SWDDIFvec)){
      png(filename = paste0("~/",testname,"/SWDDIFplot",i,".png"), width = 5, height = 4, units = 'in', res = 300)
      print(plotDIFirt(parameters = SWD_tab_coef2PL, item = SWDDIFvec[i], test = "Raju"))
      dev.off()
    }
  }
  
  ## Write Introductory Text ##
  doc_out<-doc_out%>%
    body_add_par("Students with Disabilities", style = "heading 2")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Items flagged in this section have exhibited differential item functioning (DIF) by disability status. That is, for two students with the same level of content mastery wherein one is labeled as a 'student with disability' (SWD) and the other is not, the students perform differently on the item in question. As mentioned above, items are initially flagged based upon the statistical significance--not the degree or direction--of the DIF. Therefore, it is important that reviewers examine the charts below to determine whether the degree of DIF is substantial enough to warrant further evaluation of the assessment item.", style = "Normal")%>%
    body_add_par("", style = "Normal")
  
  
  ## Write Item List and Images/NoDIF Message, as Appropriate. NOTE: Users who changed reference/focal group
  #  assignments in the analysis section will need to adjust the wording in this section to avoid misleading readers! ##    
  if(sum(SWDDIFvec)>0){
    doc_out<-doc_out%>%
      body_add_par("The following items have been flagged for DIF based on SWD status. In the charts that follow, 'Reference Group' refers to students NOT labeled as SWD; 'Focal Group' refers to students labeled as SWD.", style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par(paste0("Item numbers: ", SWD1), style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("DIF Plots: ", style = "Normal")%>%
      body_add_par("", style = "Normal")
    
    for(i in 1:length(SWDDIFvec)){
      srci<-paste0("~/",testname,"/SWDDIFplot",i,".png")
      slip_in_img(doc_out, srci, width = 2.5, height = 2, pos = "after")
      slip_in_text(doc_out, "                               ", pos = "after")
    }
  }else{
    doc_out<-doc_out%>%
      body_add_par("NO DIF DETECTED (Or insufficient sample size)!")%>%
      body_add_par("", style = "Normal")
    
  }
  
  
  
  ###############
  ### LEP DIF ###
  ###############
  
  ## Prepare Item List and Image Objects for Printing in Document ##
  
  #Item Vector#
  LEPDIFvec<-df0%>%filter(LEPDIF==1)%>%pull(ilist)
  
  #Item List#
  LEP1<-NULL
  
  for(i in 1:length(LEPDIFvec)){
    LEP1<-paste0(LEP1,"  ", LEPDIFvec[i])
  }
  
  #Image Files#
  if(length(LEPDIFvec)>0){
    for(i in 1:length(LEPDIFvec)){
      png(filename = paste0("~/",testname,"/LEPDIFplot",i,".png"), width = 5, height = 4, units = 'in', res = 300)
      print(plotDIFirt(parameters = LEP_tab_coef2PL, item = LEPDIFvec[i], test = "Raju"))
      dev.off()
    }
  }
  
  ## Write Introductory Text ##
  doc_out<-doc_out%>%
    body_add_par("Students with Limited English Proficiency", style = "heading 2")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Items flagged in this section have exhibited differential item functioning (DIF) by 'Limited English Proficiency' (LEP) status. That is, for two students with the same level of content mastery where one is labeled as a 'student with limited English proficiency' and the other is not, the students perform differently on the item in question. As mentioned above, items are initially flagged based upon the statistical significance--not the degree or direction--of the DIF. Therefore, it is important that reviewers examine the charts below to determine whether the degree of DIF is substantial enough to warrant further evaluation of the assessment item.", style = "Normal")%>%
    body_add_par("", style = "Normal")
  
  
  ## Write Item List and Images/NoDIF Message, as Appropriate. NOTE: Users who changed reference/focal group
  #  assignments in the analysis section will need to adjust the wording in this section to avoid misleading readers! ##    
  if(sum(LEPDIFvec)>0){
    doc_out<-doc_out%>%
      body_add_par("The following items have been flagged for DIF based on LEP status. In the charts that follow, 'Reference Group' refers to students NOT labeled as LEP; 'Focal Group' refers to students labeled as LEP.", style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par(paste0("Item numbers: ", LEP1), style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("DIF Plots: ", style = "Normal")%>%
      body_add_par("", style = "Normal")
    
    for(i in 1:length(LEPDIFvec)){
      srci<-paste0("~/",testname,"/LEPDIFplot",i,".png")
      slip_in_img(doc_out, srci, width = 2.5, height = 2, pos = "after")
      slip_in_text(doc_out, "                               ", pos = "after")
    }
  }else{
    doc_out<-doc_out%>%
      body_add_par("NO DIF DETECTED (Or insufficient sample size)!")%>%
      body_add_par("", style = "Normal")
    
  }
  
  
  ##############
  ### ED DIF ###
  ##############
  
  ## Prepare Item List and Image Objects for Printing in Document ##
  
  #Item Vector#
  EDDIFvec<-df0%>%filter(EDDIF==1)%>%pull(ilist)
  
  #Item List#
  ED1<-NULL
  
  for(i in 1:length(EDDIFvec)){
    ED1<-paste0(ED1,"  ", EDDIFvec[i])
  }
  
  #Image Files#
  if(length(EDDIFvec)>0){
    for(i in 1:length(EDDIFvec)){
      png(filename = paste0("~/",testname,"/EDDIFplot",i,".png"), width = 5, height = 4, units = 'in', res = 300)
      print(plotDIFirt(parameters = ED_tab_coef2PL, item = EDDIFvec[i], test = "Raju"))
      dev.off()
    }
  }
  
  ## Write Introductory Text 
  #  *NOTE: Users whose districts do not use Federal Poverty Rate (FPR) as the standard for economically disadvantaged
  #         status will need to change the description and note about FPR in this section. ##
  doc_out<-doc_out%>%
    body_add_par("'Economically Disadvantaged' Students", style = "heading 2")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("Items flagged in this section have exhibited differential item functioning (DIF) by 'Economically Disadvantaged' (ED) status. That is, for two students with the same level of content mastery where one is living below the Federal Poverty Rate (FPR) and the other is not, the students perform differently on the item in question. As mentioned above, items are initially flagged based upon the statistical significance--not the degree or direction--of the DIF. Therefore, it is important that reviewers examine the charts below to determine whether the degree of DIF is substantial enough to warrant further evaluation of the assessment item.", style = "Normal")%>%
    body_add_par("", style = "Normal")%>%
    body_add_par("(Note: at this time, the FPR is the most accurate indicator of socioeconomic status available to the district. This analysis acknowledges, however, that the low threshold for FPR qualification makes it a crude indicator of economic distress. I.e., families earning, e.g., $15,000/year above the FPR may still fall well within a better qualitative definition of economic distress.)", style = "Normal")%>%
    body_add_par("", style = "Normal")
  
  
  ## Write Item List and Images/NoDIF Message, as Appropriate. NOTE: Users who changed reference/focal group
  #  assignments in the analysis section will need to adjust the wording in this section to avoid misleading readers! ##    
  if(sum(EDDIFvec)>0){
    doc_out<-doc_out%>%
      body_add_par("The following items have been flagged for DIF based on ED status. In the charts that follow, 'Reference Group' refers to students NOT labeled as ED; 'Focal Group' refers to students labeled as ED.", style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par(paste0("Item numbers: ", ED1), style = "Normal")%>%
      body_add_par("", style = "Normal")%>%
      body_add_par("DIF Plots: ", style = "Normal")%>%
      body_add_par("", style = "Normal")
    
    for(i in 1:length(EDDIFvec)){
      srci<-paste0("~/",testname,"/EDDIFplot",i,".png")
      slip_in_img(doc_out, srci, width = 2.5, height = 2, pos = "after")
      slip_in_text(doc_out, "                               ", pos = "after")
    }
  }else{
    doc_out<-doc_out%>%
      body_add_par("NO DIF DETECTED (Or insufficient sample size)!")%>%
      body_add_par("", style = "Normal")
    
  }
  
  
  ## Write Document to File ##
  print(doc_out, target = paste0("~/",testname,"/",testname,".ReadMe.docx"))
  
  ## Write Record of Assessment Title into Terminal (helpful if iterating this function over a list of assessments) ##
  print(testname)
  
  
}
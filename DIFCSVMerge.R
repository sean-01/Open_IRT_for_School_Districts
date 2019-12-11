
########################
###  Merge Function  ###
########################

  # Purpose: Users who have run the PPSItemAnalysis.R script on multiple assessments can use this script to merge the item analysis results
  # (i.e. difficulty, discrimination, and aberrant response parameters, as well as DIF flags for each subgroup) into a single .csv
  # file for further analysis or integration into an item review database.


DIFCSVMerge<-function(filetag,start_at_char=1,end_at_char=5, RegexPattern=FALSE){
  
  #-----------------------------------------------------------------------------------------------------------------------------
  # Read
  # Before
  # Running:
  #        
  #           This script operates on two assumptions:
  #               (1) The DIF Analysis CSV files to be merged were created in separate folders, according to test name, 
  #                   using the PPSItemAnalysis R script.
  #               (2) Those folders containing the files to be merged all share the same X-digit filetag in the filename
  #                   (for example, each folder might begin with '17-18'). Users can easily adjust the script below to 
  #                   adapt to different conditions.
  #
  #
  #           Users can adjust the filetag matching settings with the start_at_char and end_at_char arguments. The script will
  #           match file/folder names where characters [start_at_char] through [end_at_char] match [filetag], where [start_at_char]
  #           and [end_at_char] are character indexes (e.g. to match 'pre' in both 'premonition' and 'preparation', the
  #           start_at_char argument should be =1, end_at_char should be =3, and filetag should be ='pre').
  #
  #____________________________________________________________________________________________________________________________
  
  
  ## Load Required Packages ##
  
  if(require("dplyr")){
    print("Package Requirement 1 of 4 Satisfied")
  } else {
    print("trying to install dplyr")
    install.packages("dplyr")
    if(require(dplyr)){
      print("dplyr installed and loaded")
    } else {
      stop("could not install dplyr package. Please install manually.")
    }
  }
  
  if(require("readr")){
    print("Package Requirement 1 of 4 Satisfied")
  } else {
    print("trying to install readr")
    install.packages("readr")
    if(require(readr)){
      print("readr installed and loaded")
    } else {
      stop("could not install readr package. Please install manually.")
    }
  }
  
  if(require("tidyverse")){
    print("Package Requirement 1 of 4 Satisfied")
  } else {
    print("trying to install tidyverse")
    install.packages("tidyverse")
    if(require(tidyverse)){
      print("tidyverse installed and loaded")
    } else {
      stop("could not install tidyverse package. Please install manually.")
    }
  }
  
  if(require("stringr")){
    print("Package Requirement 1 of 4 Satisfied")
  } else {
    print("trying to install stringr")
    install.packages("stringr")
    if(require(stringr)){
      print("stringr installed and loaded")
    } else {
      stop("could not install stringr package. Please install manually.")
    }
  }
  
  
  ## Set grepl() fixed argument to TRUE if user entered literal, FALSE if user entered regular expression ##
  if(RegexPattern==FALSE){ 
         formals(grepl)[5]<-TRUE
         }
  if(RegexPattern==TRUE){
    formals(grepl)[5]<-FALSE
  }
  
  ## Initialize List Container with Length Equal to the Number of Matching Files ##
  l1<-list(rep(NA, length(list.files()[grepl(filetag, str_sub(list.files(),start=start_at_char, end=end_at_char))])))
  
  ## Assign each placeholder in the list to one of the selected analysis files ## 
  for(i in 1:length(list.files()[grepl(filetag, str_sub(list.files(),start=start_at_char, end=end_at_char))])){
      l1[[i]]<-read.csv(paste0("~/",list.files()[grepl(filetag,str_sub(list.files(),start=start_at_char,end=end_at_char))][i],"/",list.files()[grepl(filetag,str_sub(list.files(),start=start_at_char,end=end_at_char))][i],".DIF.Analysis.csv"), stringsAsFactors = F)
  }
  
  ## Initialize Second List for Filtering Dataframes ##
  l2<-l1
  
  ## Run Loop to Reselect DIF Analysis Dataframes with Only the Relevant Variables ##
  for(i in 1:length(l2)){
    l2[[i]]<-l1[[i]]%>%dplyr::select(Test.ID, Item.Number, Gussng, Dffclt, Dscrmn, genderDIF,ethnicityDIF,giftedDIF,SWDDIF,LEPDIF,EDDIF)
  }
  
  ## Initialize Master Dataframe with First Component Dataframe ##
  longDIF<-l2[[1]]
  
  ## Run Loop to Bind All Remaining DIF Analysis Dataframes to Master Dataframe ##
  for(i in 2:length(l2)){
    longDIF<-rbind(longDIF,l2[[i]])
  }
  
  ## Add Primary Key Variable (Concatenation of Test ID Number and Item Number) to Enable Future Data Joins ##
  longDIF<-longDIF%>%mutate(pkey=paste0(Test.ID,Item.Number))
  
  ## Write Master Dataframe to CSV ##
  write.csv(longDIF, "Item_Analysis_All_Assessments.csv", row.names = F)
  
  ## Return grepl() function to its original greatness, if its defaults were altered ##
  if(formals(grepl)[5]==TRUE){
    formals(grepl)[5]<-FALSE
  }

}

#####################################

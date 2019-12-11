ItemStandardsMerge<-function(RespFileName){
  
  
  #-----------------------------------------------------------------------------------------------------------------------------
  # Read
  # Before
  # Running:
  #        
  #           This script can be used as a framework for using student response data files as a key to match each assessment
  #           item with its associated standard code. The function presumes the user has a 'Test_Details.csv' file in their
  #           working directory with all assessment titles and assessment ID numbers. The function also presumes that the 
  #           student response data file is named with exactly the same title as the associated assessment entry in the 
  #           'Test_Details.csv' file. 
  #
  #           Most likely, users from outside the original author's district (Pittsburgh Public Schools) will need to adjust
  #           the subsequent code to match the output format of their assessment management system's item
  #           standards export files. Currently (as of 2019), the code is written to match formatting from the following
  #           report from the Performance Matters system: My Reports>>District Reports>>Item Analysis Excel Export (District).
  #
  #____________________________________________________________________________________________________________________________
  
  
  ## Check for required packages; install and load if not present ##
  if(require("rio")){
    print("Package Requirement Satisfied")
  } else {
    print("trying to install rio")
    install.packages("rio")
    if(require(rio)){
      print("rio installed and loaded")
    } else {
      stop("could not install rio package. Please install manually.")
    }
  }
  
  
  
  ## Create testname variable for identifying given assessment ##
  testmatch<-str_replace(RespFileName,"[^A-z0-9)]+ITEM", "")
  
  ## Read-in master list of district assessments (necessary for match with assessment ID number) ##
  tlist<-read.csv("~/Test_Details.csv", stringsAsFactors = FALSE)
  
  ## Filter master list to only include tests for which the district has student response data (this
   # controls for errors caused by assessments with inactive duplicates in the system) ##
  tlist_active<-tlist%>%filter(Has.Results.Flag=="true")
  
  ## Create assessment ID variable by matching response data filename with an assessment title from the filtered master list ##
  tID<-tlist_active$Unify.Test.ID[str_trim(tlist_active$Test.Title, side = "both")==str_trim(testmatch, side = "both")]
  
  ## Use test name variable to establish a new variable specifying the file containing the assessment item standards; read
   # in the item standards file ##
  fname<-str_replace(paste0("~/../Downloads/Item Analysis Excel Export (District) - ",RespFileName),".ITEM", ".xlsx")
  df<-import(fname)

  ## Remove empty rows and columns from the item standards dataframe ##
  df<-df[c(-1,-2,-3),-c(2:5)]
  
  ## Remove extraneous content located below the actual item standards listing in the dataframe ##
  for(i in 1:nrow(df)){
    # if(is.na(df[i,1])&is.na(df[i,2])){    <--   This code was used for a .csv conversion
    #   df<-df[1:(i-1),]                    <--   of the item standards file.
    # } else if(is.na(df[i,1])==F){
      if((df[i,1]=="" | is.na(df[i,1])) & (df[i,2]=="" | is.na(df[i,2]))){
        df<-df[1:(i-1),]
    }
  }
  
  ## Drop rows with NA values (controls for strange formatting that sometimes remains even after preceding for-loop) ##
  df<-drop_na(df)
  
  ## Append assessment ID variable as a new column in the cleaned item standards dataframe ##
  dfdone<-data.frame("AssessmentID"=c(rep(tID,nrow(df)-1)), "ItemNumber"=df[2:nrow(df),1],"Standard(s)"=df[2:nrow(df),2])
  
  ## Convert item number variable to string format (controls for issues where extraneous content causes this variable 
   # to be read as a factor). ##
  dfdone$ItemNumber<-as.character(dfdone$ItemNumber)
  
  ## Remove section number and hyphen from item number variable to leave only the raw item number ##
  for(i in 1:nrow(dfdone)){
    dfdone[i,2]<-str_replace(dfdone[i,2],"([0-9]\\-)","")
  }
  
  ## Write processed item standards dataframe to a .csv file in the given assessment's item analysis directory ##
  write.csv(dfdone, paste0("~/",testmatch,"/",testmatch,".ItemStandards.csv"), row.names = F)
  
}


# Repository Overview
This repository hosts a project by the Pittsburgh Public Schools' Department of Data, Research, Evaluation, and Assessment (DREA) to detect bias and general item issues in assessment items using latent trait modeling (LTM) and differential item functioning (DIF) analysis.

The project is meant to be distributable to other districts seeking to implement quantitative bias detection practices in their assessment administration/review processes. The model district (Pittsburgh Public Schools) has built the review framework around the following steps:

   1. Analysis of student response data using latent trait modeling (analytics team)
   2. Internal vetting of items flagged for review in step 1 (analytics team)
   3. Review of analysis and flagged item content; revision of items with apparent problems (curriculum/assessment teams)
   4. Stakeholder (students, parents, teachers, community members, etc.) panel review of items flagged for bias (community review panel)
   5. Final review of stakeholder content revision suggestions; reintegration of improved items into assessment bank; documentation of 
      expected source(s) of bias (curriculum/assessment teams)  

This repository hosts files designed to assist primarily with step 1--analysis of student response data. However, the model district may upload documents and a database file template (MS Access) that will be useful for management of other steps in the process as the project's pilot run continues.

# Prerequisites for Using Scripts
The scripts included here presume the user district has already downloaded, cleaned, and saved in the proper directories the following files:
  * Student response information, encoded dichotomously (0 = incorrect response, 1 = correct) with one row per student and one column 
    per assessment item (one file per assessment)
  * Student demographic information with one row per student, and columns indicating sex, ethnicity, gifted status, disability status, 
    English proficiency status, and socioeconomic status. These variables can be customized by other user districts (they will need to 
    make appropriate adjustments in script code as well); however, this is the default format (one file per assessment with records in       an order that matches the student response information file).
  * Test Details spreadsheet that includes a master list of asessments to be analyzed, their associated assessment ID numbers, and other 
    information as desired by the user district.
    
User districts may wish to consult the included 'sample_student_response' and 'sample_student_demographics' files for a template to match their own files against (note: these are dummy files to demonstrate a template; all values were randomly generated). The introductory commentary for each script file also includes greater detail regarding the files required for analysis.

# File Overviews and Outputs
This section describes each file included in this repository, including the outputs associated with script files. District users unfamiliar with using R should scroll to the bottom of this readme for step-by-step instructions on setting up a computer to conduct item analysis using the files in this repository.

**PPSItemAnalysis.R**

This is the main script for the item analysis function. The function conducts IRT and DIF analysis for the selected assessment and creates an assessment analysis directory (path = \[AssessmentName]) within the user's working directory, containing the following files:
  * Written item analysis report (\[AssessmentName]/\[AssessmentName].ReadMe)
  * 3PL model parameters for all items on the given assessment (\[AssessmentName]/\[AssessmentName].DIF.Analysis)
  * Record of student response data for all analyzed (dichotomous) items (\[AssessmentName]/\[AssessmentName].Responses)
  * Record of student response data for rubric-scored (not analyzed) items (\[AssessmentName]/\[AssessmentName].RubricItems.Responses)
  * Record of student demographic tags used to analyze DIF (\[AssessmentName]/\[AssessmentName].GroupInfo)
  * PDFs showing visualizations of all DIF items detected, by category (\[AssessmentName]/\[AssessmentName].\[category]DIFplots)
  * Individual .png files depicting all DIF items detected, individually (\[AssessmentName]/\[AssessmentName].\[category]DIFplot\[itemnumber])

The 'sample_student_response' FOLDER in this repository is included to show users the above outputs from the 'PPSItemAnalysis.R' script. That is, for every iteration of the PPSItemAnalysis function, the script creates a folder on the user's device including a range of files matching the list above (individual files will vary according to the item flags detected in the analysis of any given assessment). The output directory and filenames will always match the assessment name the user assigned to the 'RespFileName' argument passed to the PPSItemAnalysis function.

The primary sample utput file is '\[testname].ReadMe.docx'. This written report outlines all LTM and DIF flags for the given assessment in significant detail.

**DIFCSVMerge.R**

This script--useful for users who have run the PPSItemAnalysis.R function on multiple assessments--iterates through the output folders for a selected range of assessments and binds together the item analysis .csv files for each assessment. Consolidating all item analysis into a single file will be helpful for districts integrating this information into an item review tracking database or other interface meant to be used by multiple stakeholders in the review process. Assessment/analytics teams seeking to conduct analysis on the aggregate effectiveness of assessment items (for example by grade, subject, etc.) may find this function particularly useful. Users can enter either a fixed folder name prefix or a regular expression pattern to select assessments to include (helpful for grabbing, e.g., all assessments that start with '2017-2018', 'Math', 'Grade 6', etc.).

The function's output is a single .csv file with all aforementioned statistics for every item of every selected assessment (regardless of whether each item was flagged for DIF), labeled with the 'pkey' variable, which is a concatenation of the test ID number and the item number (useful for further matching of item statistics with other information, such as item content or academic standards).

**ItemStandardsMerge.R**

This script provides a template for districts whose assessment management systems provide exports of the standards (Common Core or other) associated with each item, by assessment. The main problem this function solves for the author's district is that standards exports are not by default formatted for matching and analysis, and do not include row-wise assessment identification. The script creates test and item ID variables to associate with records of each item's standards, enabling this data to be joined with other item-level analysis. This will be particularly useful for districts seeking to model patterns of assessment item bias.

This function's output is a single .csv file containing each item number, associated test ID number, and associated standard codes for the selected assessment. These standard codes can be used to match item standards against full descriptions of each academic standards for users who are able to procure a master list of standard codes/standards.

**District Item Analysis - MS Access Interface Template**

This is a download-able MS Access template that may be useful for user districts' item review process management. The template will be easiest to interact with if users first run the item analysis function on all selected assessments, then run the DIFCSVMerge function to consolidate item analysis files into one spreadsheet, and append the resulting consolidated file to the 'Item_Analysis_All_Assessments' table in the Access template. Process management will also be easier for user districts that can append a spreadsheet of asessment details to the 'Assessments' table (however, users could also enter records of each assessment one-by-one through the Enter New assessment button).

# How Do I Run These Files???

For district users unfamiliar with the R programming language, the steps below should be helpful in setting up a computer to conduct item analysis using the scripts in this repository. These steps assume you have already prepared your assessment and student demographic data to match the sample file format and saved it in your Downloads directory, in addition to saving a 'Test_Details.csv' file in your Documents directory (match the format in the sample Test_Details file from this repository to avoid errors).

   1. Visit https://www.r-project.org/ to download the correct version of R for your computer. This enables your computer to run R script files; however, most users do not use this download to actually write and interact with files. Think of it like installing a router in your home for connecting to the internet--it gives you the ability to use a wifi network, but you don't use the router itself to stream media and surf the web.
   2. Visit https://www.rstudio.com/products/rstudio/download/ to download the latest version of RStudio. This is the interface most people use to write, edit, and run R script files. Think of this like the laptop or cell phone you might use to connect to the internet once your internet service is up and running. This is the program most users will use to run the script files included in this repository.
   3. Download the PPSItemAnalysis.R file from this repository. 
   4. Open RStudio and click File>>New File>>R Script (or pres Ctrl+Shift+N)

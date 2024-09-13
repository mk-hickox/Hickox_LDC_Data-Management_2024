
# MetaData ----------------------------------------------------------------
#Author: MK Hickox
#Date: Sept. 11,2024
#Title: Data Cleaning Assignment, LDC Data Management 2024.

#Data Source: BWG Database (supplied by Dr.Diane Srivastava, UBC)
#Dataset Used: Visits
#Working Directory: This R Project, containing the above data
#GitHub: https://github.com/mk-hickox/Hickox_LDC_Data-Management_2024

# The Problem/Goal of Script ----------------------------------------------
This criterion is linked to a Learning OutcomeDescription: The Problem
(1) Does the brief written description correctly identify the chosen data cleaning task, including where in the dataset(s) the issues might arise?
  
  (2) Does the description include examples of the sorts of issues that were identified in the data?

# instructions ------------------------------------------------------------

Dates and coordinates
Check all dates are ISO standards (yyyy-mm-dd, ISO 8601Links to an external site.). [suggested packages: lubridate; see example code in tutorial 2]. Check these dates are in the range 1997-2024.
For the 'visits' dataframe, change the name 'date' to 'visit_date', and add columns for day, month, and year of each visit. Place these columns immediately after the date column. Lastly, add the 'date' column from 'visits' to the 'bromeliads' dataframe, immediately after the column 'collection_date'. Does the visit_date match with the 'collection_date' in the bromeliads dataframe?
  
  Assign a coordinate reference system to all coordinates, and then project geographic coordinates. [suggested package: sf; see example code in tutorial 2]. Make sure these coordinates are on planet Earth, i.e. somewhere between N and S poles.

# Script ------------------------------------------------------------------
## Load Packages
library(tidyverse)
library(lubridate)
library(sf)
library(taxize)
library(myTAI)
library(renv)
library(assertr)

## Load Dataset
getwd() #confirm working directory is project

myfiles <- list.files(path = "00_RawData/BWG_database/", pattern = "*.csv", full.names = TRUE) #loads files

list2env(
  lapply(
    setNames(myfiles, 
             make.names(
               gsub(".*1_", "", 
                    tools::file_path_sans_ext(myfiles)))), 
    read_csv), 
  envir = .GlobalEnv) #imports tables and makes names more succinct 

# 1. Check data for class/format errors -------------------------------------------
#Problem 1:
    #Here, I want to check that my columns of interest (Date, lat and long) are all the correct class and format.

view(visits) #open dataset 
str(visits) 
#Solution 1:
    #The date column class is "Date", and the lat and long are listed as numerical, so all looks good for now.
    #Visual inspection also confirms that the date follows ISO standard (ymd), but if it HAD been incorrectly formatted, we would have used: visits$date <- as_date(visits$date) 
                                                                                                                                            #visits$date <- ymd(visits$date)

# 2. Reformat the visits column -------------------------------------------
#Problem 2:
    #Here, I want to rename the "date" column to "visit_date" and also separate date into multiple columns of "year", "month", and "day"
    #Once the columns are split, I also want to confirm that the years fall within the possible dates (according to the study design)
    #All dates must fall between 1997-2024, as data was not collected outside of this range. Alternate dates indicate errors.
visits <-
  visits %>%
dplyr::rename(visit_date = date) #Renaming the "date" column to "visit_date"

visits <- #Isolating the month, day, and year and adding as new columns
  visits %>%
  mutate(visit_year= year(visits$visit_date), #getting the years of visits
       visit_month= month(visits$visit_date), #getting the month of visits
      visit_day= day(visits$visit_date)) %>% #getting day of visit
  relocate(visit_year,visit_month,visit_day, .after = 4) #Moving the new columns after the "visit_date" column (i.e. column 4)

view(visits) #confirm that all changes look good via visual inspection!

range(visits$year) #Checking the range of years.The years fall between 1997-2010, which is as expected. Data pass this check.

#Solution 2:
  #Here, I renamed the "date" column to "visit_date" using dplyr rename. 
  #I then split the "visit_date" column into year, month, and day using lubridicate, and confirmed that the years are valid (ranging between 1997-2024).
  #I added the new columns into the "visits" dataset using mutate and rearranged the header order using "relocate"

# 3. Add relevant column to bromeliad dataset -----------------------------
#Problem 3:
  #I need to add the "visit_date" column from visits to the bromeliad dataset.
  #The new column needs to be ordered after the "collection_date" column

view(bromeliads) #Opening the bromeliad dataset. Looks like "visit_id" connects the visits and bromeliads datasets


bromeliads_output<-
  visits %>%
  select(visit_date, visit_id) %>% #selecting only the "visit_date" column (which I want to move) and the visit_id (which acts as the key)
  left_join(.,bromeliads, by= "visit_id") %>% #joining tables by "visit_id"
  relocate("visit_date", .after= "collection_date") #moving the new column after "collection_date"

view(bromeliads_output) #checking output visually
names(bromeliads_visit) #checking column names and numbers

bromeliads_output %>%
  verify(collection_date == visit_date) #Checking to see if the row values for both columns are identical.
                                        # Get an error message with 74 failures, meaning that for 74 rows, the values are not identical.
nrow(bromeliads_output)                 #There are a total of 76 rows, so only 2 rows are identical for both columns.
                                        #This indicates that the collection and visit dates are not the same. 
bromeliads_output %>%
  select(collection_date, visit_date) %>%
  view() #Isolating the columns of interest for visual inspection
        # Further inspection indicates that the collection_date contains many NA, and the date is often later than the visit_date.
       
#Solution 3:
  #I added the visit_date in the correct order from the visits dataset to the bromeliads dataset.
  #I compared both columns and found that 74/76 were not identical.
  #Collection_date contains many NA and later dates than visit_date.
  #This likely means that not all bromeliads were collection and/or that collections often took place after the initial visit
  #Should confirm this with dataset authors to ensure that is isn't an error
  #The cleaned and reorganied datasets were then saved in 03_Output.


# 4. Lat/Long Projection --------------------------------------------------



range(visits$latitude) #Checking the range of latitude values. Range is 10.983-10.983, 
range(visits$longitude) #Checking the longitude range. Range is -85.433- -85.433,

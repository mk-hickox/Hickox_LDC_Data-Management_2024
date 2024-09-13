
# MetaData ----------------------------------------------------------------
#Author: MK Hickox
#Date: Sept. 11,2024
#Title: Data Cleaning Assignment, LDC Data Management 2024.

#Data Source: BWG Database (supplied by Dr.Diane Srivastava, UBC)
#Dataset Used: Visits, Bromeliads
#Working Directory: This R Project, containing the above data
#GitHub: https://github.com/mk-hickox/Hickox_LDC_Data-Management_2024

# instructions ------------------------------------------------------------
#Quoted directly from the Dates/coordinates task of the Cleaning Assignment:
  #1. Check all dates are ISO standards (yyyy-mm-dd, ISO 8601Links to an external site.). [suggested packages: lubridate; see example code in tutorial 2]. Check these dates are in the range 1997-2024.
  #2. For the 'visits' dataframe, change the name 'date' to 'visit_date', and add columns for day, month, and year of each visit. Place these columns immediately after the date column. 
  #3. Lastly, add the 'date' column from 'visits' to the 'bromeliads' dataframe, immediately after the column 'collection_date'. Does the visit_date match with the 'collection_date' in the bromeliads dataframe?
  #4. Assign a coordinate reference system to all coordinates, and then project geographic coordinates. [suggested package: sf; see example code in tutorial 2]. Make sure these coordinates are on planet Earth, i.e. somewhere between N and S poles.

#Goal of this script: Identify errors in spatial and temporal data, and join aspects of multiple datasets!
# Script ------------------------------------------------------------------
## Load Packages
library(tidyverse)
library(lubridate)
library(sf)
library(renv)
library(assertr)
library(readr)

renv::init

## Load Dataframe:
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
    #Here, I want to check that my column of interest (Date) is in the correct class and format.

view(visits) #open dataset 
view(bromeliads) #open dataset

visits$date <- as_date(visits$date) #converting to date format
visits$date <- ymd(visits$date) #ensuring that the dates are in y-m-d format.
bromeliads$collection_date <- as_date(bromeliads$collection_date) #converting to date format
bromeliads$collection_date <- ymd(bromeliads$collection_date)#ensuring that the dates are in y-m-d format.

#Checks to confirm that all data are now correct
class(bromeliads$collection_date) #class is date, which is what I want!
unique(bromeliads$collection_date) #checking date format, looks good (yyy-mm-dd)
class(visits$date) #checking to see if class is date. It is, so all good! 
head(visits$date) #checking date format, looks good (yyy-mm-dd)

#Solution 1:
    #"date" and "collection_date" columns in visits and bromeliads datasets are of class "Date" and in the format of y-m-d. 
    #Note that the columns were already in the correct format, but I showed the steps to adjust them (for reference) 

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
  relocate(visit_year,visit_month,visit_day, .after = visit_date) #Moving the new columns after the "visit_date" column

#Checks:
view(visits) #confirm that all changes look good via visual inspection!
names(visits) #confirming column names are added and are in correct order
range(visits$visit_year) #Checking the range of years.The years fall between 1997-2010, which is as expected (i.e. between 1997-2024). Data pass this check.

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

#Checks:
view(bromeliads_output) #checking output visually. Looks good.
names(bromeliads_output) #checking column names and order

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
  #I added the visit_date in the correct order from the visits dataset to the bromeliads dataset (linking by visit_id)
  #I compared both columns and found that 74/76 were not identical.
  #Collection_date contains many NA and later dates than visit_date.
  #This likely means that not all bromeliads were collected and/or that collections often took place after the initial visit
  #Should confirm this with dataset authors to ensure that is isn't an error

# 4. Lat/Long Projection --------------------------------------------------
#Problem 4:
  #Here, we want to make sure that the coordinates all make sense (i.e. are within the possible ranges on Earth).
  #We also want to assign a coordinate reference system and project the coordinates 
visits_output %>%
  select(longitude,latitude) %>%
  range() #output range for longitude is: -85.433 and for latitude is: 10.983.
          #longitude should be between -180 to +180, and this is the case.
          #latitude should be between -90 to +90 and this is the case.
          #lat and long.data makes sense.

utm_lat_long<-
  visits_output %>%
  select(longitude,latitude) %>%
  st_as_sf(., coords = c("longitude", "latitude"), #isolating the columns of interest
               crs = "+proj=longlat +datum=WGS84") %>% #assign coordinate reference system WGS84
  st_transform(., crs = "+proj=utm +zone=16 +datum=WGS84") #assigning the projection 

visits_output<- 
  visits_output %>%
  mutate(utm_lat_long = utm_lat_long$geometry) #saving the transformed spatial data in the dataframe 

#Check
view(visits_output) #Ensuring that my changes were made via visual inspection.

#Save files and package info
write_csv(visits_output,"./02_Output/00_Cleaned_visits.csv") #saving the cleaned visits output file
write_csv(bromeliads_output,"./02_Output/00_Cleaned_bromeliads.csv") #saving the cleaned bromeliads output file
renv::snapshot() #saving package info.

#Solution 4:
  #I checked the range of the latitude and longitude values, and there were all within the possible range.
  #I then assigned crs: WGS84, projected the data (zone 16), and saved the projection in the output file.
  #The datasets are now clean and ready for analysis!
  #I saved the cleaned output files in 03_Output


##End of script!
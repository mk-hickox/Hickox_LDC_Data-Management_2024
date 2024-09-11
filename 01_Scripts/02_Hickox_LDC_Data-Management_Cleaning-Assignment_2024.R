
# MetaData ----------------------------------------------------------------
#Author: MK Hickox
#Date: Sept. 11,2024
#Title: Data Cleaning Assignment, LDC Data Management 2024.
#Data Source: BWG Database (supplied by Dr.Diane Srivastava, UBC)
#Dataset Used: Visits

# instructions ------------------------------------------------------------

Dates and coordinates
Check all dates are ISO standards (yyyy-mm-dd, ISO 8601Links to an external site.). [suggested packages: lubridate; see example code in tutorial 2]. Check these dates are in the range 1997-2024.
For the 'visits' dataframe, change the name 'date' to 'visit_date', and add columns for day, month, and year of each visit. Place these columns immediately after the date column. Lastly, add the 'date' column from 'visits' to the 'bromeliads' dataframe, immediately after the column 'collection_date'. Does the visit_date match with the 'collection_date' in the bromeliads dataframe?
  
  Assign a coordinate reference system to all coordinates, and then project geographic coordinates. [suggested package: sf; see example code in tutorial 2]. Make sure these coordinates are on planet Earth, i.e. somewhere between N and S poles.

# Script ------------------------------------------------------------------



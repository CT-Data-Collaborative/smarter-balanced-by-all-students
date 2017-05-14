library(dplyr)
library(datapkg)

##################################################################
#
# Processing Script for Smarter-Balanced-by-All-Students
# Created by Jenna Daly
# On 05/14/17
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("All-Students", sub_folders, value=T)
path_to_top_level <- (paste0(getwd(), "/", data_location))
path_to_raw_data <- (paste0(getwd(), "/", data_location, "/", "raw"))
all_csvs <- dir(path_to_raw_data, recursive=T, pattern = ".csv") 
combined <- dir(path_to_raw_data, recursive=T, pattern = "combined") 
indiv_grades <- all_csvs[!all_csvs %in% combined]

all_dist_indiv <- grep("_ct", indiv_grades, value=T, invert=T)
all_state_indiv <- grep("_ct", indiv_grades, value=T)

all_dist_combined <- grep("_ct", combined, value=T, invert=T)
all_state_combined <- grep("_ct", combined, value=T)

##Individual Grade data
#District
sb_dist_indiv <- data.frame(stringsAsFactors = F)
sb_dist_indiv_noTrend <- grep("trend", all_dist_indiv, value=T, invert=T)
for (i in 1:length(sb_dist_indiv_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", sb_dist_indiv_noTrend[i]), stringsAsFactors=F, header=F )
  colnames(current_file) <- current_file[5,]
  current_file <- current_file[-c(1:5),]
  current_file <- current_file[, !(names(current_file) == "District Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(sb_dist_indiv_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  sb_dist_indiv <- rbind(sb_dist_indiv, current_file)
}

#State
sb_state_indiv <- data.frame(stringsAsFactors = F)
sb_state_indiv_noTrend <- grep("trend", all_state_indiv, value=T, invert=T)
for (i in 1:length(sb_state_indiv_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", sb_state_indiv_noTrend[i]), stringsAsFactors=F, header=F )
  colnames(current_file) <- current_file[5,]
  current_file <- current_file[-c(1:5),]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(sb_state_indiv_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  sb_state_indiv <- rbind(sb_state_indiv, current_file)
}

##Combined Grade data
#District
sb_dist_combined <- data.frame(stringsAsFactors = F)
sb_dist_combined_noTrend <- grep("trend", all_dist_combined, value=T, invert=T)
for (i in 1:length(sb_dist_combined_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", sb_dist_combined_noTrend[i]), stringsAsFactors=F, header=F )
  colnames(current_file) <- current_file[4,]
  current_file <- current_file[-c(1:4),]
  current_file <- current_file[, !(names(current_file) == "District Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(sb_dist_combined_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  sb_dist_combined <- rbind(sb_dist_combined, current_file)
}

#State
sb_state_combined <- data.frame(stringsAsFactors = F)
sb_state_combined_noTrend <- grep("trend", all_state_combined, value=T, invert=T)
for (i in 1:length(sb_state_combined_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", sb_state_combined_noTrend[i]), stringsAsFactors=F, header=F )
  colnames(current_file) <- current_file[4,]
  current_file <- current_file[-c(1:4),]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(sb_state_combined_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  sb_state_combined <- rbind(sb_state_combined, current_file)
}

##Combine indiv and combined district and state
#rename columns in both dist and state (indiv)
colnames(sb_dist_indiv) <- colnames(sb_state_indiv)

#For state data, set District columns to CT
sb_state_indiv$District <- "Connecticut"
sb_state_combined$District <- "Connecticut"

#Add Grade columns to combined data sets
sb_dist_combined$Grade <- "All"
sb_state_combined$Grade <- "All"

#Add AvgVSS columns to combined data sets
sb_dist_combined$AverageVSS <- NA
sb_state_combined$AverageVSS <- NA

#rename columns in both dist and state (combined)
colnames(sb_dist_combined) <- colnames(sb_state_combined)

#Combine dist and state in combined data
sb_combined <- rbind(sb_dist_combined, sb_state_combined)

#Combine dist and state in indiv data
sb_indiv <- rbind(sb_dist_indiv, sb_state_indiv)

#Rename columns in indiv and combined, so they can combine
colnames(sb_combined) <- c("District",                          
                           "Subject",                          
                           "Total Number of Students",           
                           "Total Number Tested",               
                           "Smarter Balanced Participation Rate", 
                           "Total Number with Scored Tests",   
                           "Level 1 Not Met Count",            
                           "Level 1 Not Met Percent",                                
                           "Level 2 Approaching Count",        
                           "Level 2 Approaching Percent",                                
                           "Level 3 Met Count",               
                           "Level 3 Met Percent",                                
                           "Level 4 Exceeded Count",         
                           "Level 4 Exceeded Percent",                                
                           "Level 3 & 4 Met or Exceeded Count", 
                           "Level 3 & 4 Met or Exceeded Percent",                                
                           "Year",                            
                           "Grade",                            
                           "AverageVSS")

colnames(sb_indiv) <- c("District",                         
                        "Grade",                       
                        "Subject",                       
                        "Total Number of Students",          
                        "Total Number Tested",                
                        "Smarter Balanced Participation Rate",
                        "Total Number with Scored Tests",    
                        "Level 1 Not Met Count",           
                        "Level 1 Not Met Percent",                                 
                        "Level 2 Approaching Count",       
                        "Level 2 Approaching Percent",                                 
                        "Level 3 Met Count",               
                        "Level 3 Met Percent",                                 
                        "Level 4 Exceeded Count",          
                        "Level 4 Exceeded Percent",                                 
                        "Level 3 & 4 Met or Exceeded Count",  
                        "Level 3 & 4 Met or Exceeded Percent",                                
                        "AverageVSS",                       
                        "Year")

#Put both df's in same order
sb_combined <- sb_combined %>% 
  select (`District`,                         
          `Grade`,                       
          `Subject`,                       
          `Total Number of Students`,          
          `Total Number Tested`,                
          `Smarter Balanced Participation Rate`,
          `Total Number with Scored Tests`,    
          `Level 1 Not Met Count`,           
          `Level 1 Not Met Percent`,                                 
          `Level 2 Approaching Count`,       
          `Level 2 Approaching Percent`,                                 
          `Level 3 Met Count`,               
          `Level 3 Met Percent`,                                 
          `Level 4 Exceeded Count`,          
          `Level 4 Exceeded Percent`,                                 
          `Level 3 & 4 Met or Exceeded Count`,  
          `Level 3 & 4 Met or Exceeded Percent`,                                
          `AverageVSS`,                       
          `Year`)

sb_indiv <- sb_indiv %>% 
  select (`District`,                         
          `Grade`,                       
          `Subject`,                       
          `Total Number of Students`,          
          `Total Number Tested`,                
          `Smarter Balanced Participation Rate`,
          `Total Number with Scored Tests`,    
          `Level 1 Not Met Count`,           
          `Level 1 Not Met Percent`,                                 
          `Level 2 Approaching Count`,       
          `Level 2 Approaching Percent`,                                 
          `Level 3 Met Count`,               
          `Level 3 Met Percent`,                                 
          `Level 4 Exceeded Count`,          
          `Level 4 Exceeded Percent`,                                 
          `Level 3 & 4 Met or Exceeded Count`,  
          `Level 3 & 4 Met or Exceeded Percent`,                                
          `AverageVSS`,                       
          `Year`)

#bind together
sb <- rbind(sb_indiv, sb_combined)

#backfill Districts
district_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-school-district-list/master/datapackage.json'
district_dp <- datapkg_read(path = district_dp_URL)
districts <- (district_dp$data[[1]])

sb_fips <- merge(sb, districts, by.x = "District", by.y = "District", all=T)

sb_fips$District <- NULL

sb_fips<-sb_fips[!duplicated(sb_fips), ]

sb_fips$Grade[sb_fips$Grade == "3"] <- "Grade 3"
sb_fips$Grade[sb_fips$Grade == "4"] <- "Grade 4"
sb_fips$Grade[sb_fips$Grade == "5"] <- "Grade 5"
sb_fips$Grade[sb_fips$Grade == "6"] <- "Grade 6"
sb_fips$Grade[sb_fips$Grade == "7"] <- "Grade 7"
sb_fips$Grade[sb_fips$Grade == "8"] <- "Grade 8"
sb_fips$Grade[sb_fips$Grade == "11"] <- "Grade 11"


#backfill year
years <- c("2014-2015",
           "2015-2016")
subject <- c("ELA", 
             "Math")
grade <- c("Grade 3",
           "Grade 4",
           "Grade 5",
           "Grade 6",
           "Grade 7",
           "Grade 8",
           "Grade 11", 
           "All")

backfill_years <- expand.grid(
  `FixedDistrict` = unique(districts$`FixedDistrict`),
  `Year` = years,
  `Subject` = subject,
  `Grade` = grade
)

backfill_years$FixedDistrict <- as.character(backfill_years$FixedDistrict)
backfill_years$Year <- as.character(backfill_years$Year)

backfill_years <- arrange(backfill_years, FixedDistrict)

complete_sb <- merge(sb_fips, backfill_years, all=T)

#remove duplicated Year rows
complete_sb <- complete_sb[!with(complete_sb, is.na(complete_sb$Year)),]

#return blank in FIPS if not reported
complete_sb$FIPS <- as.character(complete_sb$FIPS)
complete_sb[["FIPS"]][is.na(complete_sb[["FIPS"]])] <- ""

#recode missing data with -6666
complete_sb[is.na(complete_sb)] <- -6666

#recode suppressed data with -9999
complete_sb[complete_sb == "*"]<- -9999

names(complete_sb)[names(complete_sb) == "FixedDistrict"] <- "District"

#reshape from wide to long format
cols_to_stack <- c("Total Number of Students", 
                   "Total Number Tested", 
                   "Smarter Balanced Participation Rate",
                   "Total Number with Scored Tests", 
                   "Level 1 Not Met Count", 
                   "Level 1 Not Met Percent", 
                   "Level 2 Approaching Count", 
                   "Level 2 Approaching Percent", 
                   "Level 3 Met Count", 
                   "Level 3 Met Percent", 
                   "Level 4 Exceeded Count", 
                   "Level 4 Exceeded Percent", 
                   "Level 3 & 4 Met or Exceeded Count", 
                   "Level 3 & 4 Met or Exceeded Percent", 
                   "AverageVSS")

long_row_count = nrow(complete_sb) * length(cols_to_stack)

complete_sb_long <- reshape(complete_sb,
                                        varying = cols_to_stack,
                                        v.names = "Value",
                                        timevar = "Variable",
                                        times = cols_to_stack,
                                        new.row.names = 1:long_row_count,
                                        direction = "long"
)

#reorder columns and remove ID column
complete_sb_long <- complete_sb_long[order(complete_sb_long$District, complete_sb_long$Year),]
complete_sb_long$id <- NULL

#Add Measure Type

complete_sb_long$"Measure Type" <- NA
##fill in 'Measure Type' column based on criteria listed below
complete_sb_long$"Measure Type"[which(complete_sb_long$Variable %in% c("Total Number of Students", 
                                                                       "Total Number Tested", 
                                                                       "Total Number with Scored Tests", 
                                                                       "Level 1 Not Met Count", 
                                                                       "Level 2 Approaching Count", 
                                                                       "Level 3 Met Count", 
                                                                       "Level 4 Exceeded Count", 
                                                                       "Level 3 & 4 Met or Exceeded Count",
                                                                       "AverageVSS"))] <- "Number"

complete_sb_long$"Measure Type"[which(complete_sb_long$Variable %in% c("Smarter Balanced Participation Rate",
                                                                       "Level 1 Not Met Percent", 
                                                                       "Level 2 Approaching Percent", 
                                                                       "Level 3 Met Percent", 
                                                                       "Level 4 Exceeded Percent", 
                                                                       "Level 3 & 4 Met or Exceeded Percent"))] <- "Percent"

#Order columns
complete_sb_long <- complete_sb_long %>% 
  select(`District`, `FIPS`, `Year`, `Grade`, `Subject`, `Variable`, `Measure Type`, `Value`)

#Use this to find if there are any duplicate entires for a given district
# test <- complete_sb_long[,c("District", "Year")]
# test2<-test[duplicated(test), ]

#Write CSV
write.table(
  complete_sb_long,
  file.path(path_to_top_level, "data", "smarter_balanced_all_students_2015-2016.csv"),
  sep = ",",
  row.names = F
)


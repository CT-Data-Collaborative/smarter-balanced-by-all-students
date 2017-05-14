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
all_state_csvs <- dir(path_to_raw_data, recursive=T, pattern = "ct.csv") 
all_dist_csvs <- all_csvs[!all_csvs %in% all_state_csvs]

sb_dist <- data.frame(stringsAsFactors = F)
sb_dist_noTrend <- grep("trend", all_dist_csvs, value=T, invert=T)
for (i in 1:length(sb_dist_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", sb_dist_noTrend[i]), stringsAsFactors=F, header=F )
  colnames(current_file) <- current_file[4,]
  current_file <- current_file[-c(1:4),]
  current_file <- current_file[, !(names(current_file) == "District Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(sb_dist_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  sb_dist <- rbind(sb_dist, current_file)
}

#Add statewide data...
sb_state <- data.frame(stringsAsFactors = F)
sb_state_noTrend <- grep("trend", all_state_csvs, value=T, invert=T)
for (i in 1:length(sb_state_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", sb_state_noTrend[i]), stringsAsFactors=F, header=F )
  colnames(current_file) <- current_file[4,]
  current_file <- current_file[-c(1:4),]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(sb_state_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  sb_state <- rbind(sb_state, current_file)
}

#Combine district and state
#set District column to CT
sb_state$District <- "Connecticut"

#rename columns in both dist and state
colnames(sb_dist) <- colnames(sb_state)

#bind together
sb <- rbind(sb_state, sb_dist)

#backfill Districts
district_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-school-district-list/master/datapackage.json'
district_dp <- datapkg_read(path = district_dp_URL)
districts <- (district_dp$data[[1]])

sb_fips <- merge(sb, districts, by.x = "District", by.y = "District", all=T)

sb_fips$District <- NULL

sb_fips<-sb_fips[!duplicated(sb_fips), ]

#backfill year
years <- c("2014-2015",
           "2015-2016")
subject <- c("ELA", 
             "Math")

backfill_years <- expand.grid(
  `FixedDistrict` = unique(districts$`FixedDistrict`),
  `Year` = years,
  `Subject` = subject
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

colnames(complete_sb) <- c("Subject", 
                           "Year", 
                           "District", 
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
                           "FIPS")

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
                   "Level 3 & 4 Met or Exceeded Percent")

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
                                                                       "Level 3 & 4 Met or Exceeded Count"))] <- "Number"

complete_sb_long$"Measure Type"[which(complete_sb_long$Variable %in% c("Smarter Balanced Participation Rate",
                                                                       "Level 1 Not Met Percent", 
                                                                       "Level 2 Approaching Percent", 
                                                                       "Level 3 Met Percent", 
                                                                       "Level 4 Exceeded Percent", 
                                                                       "Level 3 & 4 Met or Exceeded Percent"))] <- "Percent"

#Order columns
complete_sb_long <- complete_sb_long %>% 
  select(`District`, `FIPS`, `Year`, `Subject`, `Variable`, `Measure Type`, `Value`)

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


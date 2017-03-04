library(dplyr)
#set your working directory
#setwd('/Users/xiaowenfeng/Desktop/Class/INFO370/Info370-Final-Project')

#read in all csv files
on.campus.data <- data.frame(read.csv('./data/oncampus_crime.csv', header = TRUE))
off.campus.data <- data.frame(read.csv('./data/noncampus_crime.csv', header = TRUE))
public.data <- data.frame(read.csv('./data/publicproperty_crime.csv', header = TRUE))
college.data <- data.frame(read.csv('./data/collegescorecard.csv', header = TRUE))

#replace all NA to 0 for off-campus crime incidents
off.campus.data[is.na(off.campus.data)] <- 0

#sort all 3 data frames by school id
on.campus.data <- arrange(on.campus.data, UNITID_P)
off.campus.data <- arrange(off.campus.data, UNITID_P)
public.data <- arrange(public.data, UNITID_P)

#filter only have crime-related data
on.campus.col <- on.campus.data[,grepl('14', colnames(on.campus.data))]
off.campus.col <- off.campus.data[,grepl('14', colnames(off.campus.data))]
public.col <- public.data[,grepl('14', colnames(public.data))]

# aggregate crime-related data that happened in different locations
total <- on.campus.col + off.campus.col + public.col

#get all not crime-related columns
crime.data <- on.campus.data %>%
  select(UNITID_P,INSTNM,
         Sector_desc,men_total,
         women_total,Total)
#create final data frame
crime.data <- data.frame(c(crime.data,total))
#change school id name 
colnames(crime.data)[1] <- 'UNITID'
#drop the last 3 digits of school campus id, in order to aggregate data with same school id
crime.data$UNITID <- round(crime.data$UNITID/1000, digits=0)


# aggregate each crime by school
crime.data <- crime.data %>% group_by(UNITID) %>% 
  mutate_each(funs(sum), -UNITID, -INSTNM, -Sector_desc, -men_total, -women_total, -Total)

# keep only one row for each unique school id
crime.data <- subset(crime.data, !duplicated(UNITID))
View(crime.data)
#calculate total crime number
total_crime <- rowSums(crime.data[,7:17])
#create crime rate per 1000 population variable
crime.data$crime_rate <- total_crime / crime.data$Total * 1000
View(crime.data)
crime.data <- crime.data %>%
  select(UNITID,INSTNM,
         Sector_desc,crime_rate,Total)

college.data<- college.data %>%
  select(UNITID,DEP_STAT_PCT_IND,LOCALE,PREDDEG, PAR_ED_PCT_1STGEN, MD_FAMINC)

# join college data and crime data 
final.data <- inner_join(crime.data,college.data, by='UNITID')
#View(final.data)
final.data$UNITID <- as.factor(final.data$UNITID)
levels(final.data$UNITID)
# replace null and unavailable data to NA
#is.na(final.data) <- final.data$MD_FAMINC == 0
is.na(final.data) <- final.data == "NULL"
is.na(final.data) <- final.data =="PrivacySuppressed"
# remove NA data, 3758 rows left
final.data <- na.omit(final.data)

# change names
colnames(final.data)[3:10] <- c("sector", "crime_rate", "student_pop"
                                ,"fin_indep", "locale", "pre_degree", "firstgen_pct", "fam_income")
# correct data type format
sapply(final.data,class)
#remove rows with outlier data
final.data <- final.data %>% filter(locale != -3)
final.data <- final.data %>% filter(crime_rate < 1000 & fam_income > 0)
# correct data type format
final.data$locale <- as.factor(final.data$locale)
final.data$fin_indep <- as.numeric(as.character(final.data$fin_indep))
final.data$pre_degree <- as.factor(final.data$pre_degree)
final.data$firstgen_pct <- as.numeric(as.character(final.data$firstgen_pct))
final.data$fam_income<- as.numeric(as.character(final.data$fam_income))
View(final.data)
write.csv(final.data, file = "cleaned.csv")


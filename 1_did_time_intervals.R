####################################################################################################################################
                                              ### L O A D   P A C K A G E S ###
####################################################################################################################################

library(tidyverse)
library(peRspective)
library(lubridate)
library(anytime)
library(rvest)
library(curl)
library(fuzzyjoin)
library(fs)
library(data.table)
library(stargazer)
library(did)
library(dplyr)
library(parallel)
library(plyr)
library(data.table) 
library(parallel)

####################################################################################################################################
                                    ### P R E - P R O C E S S   T U C K E R   D A T A ###
####################################################################################################################################

# load tucker data: data contains the twitter handles for all treated journalists.
# description: "URL" contains URL to transcript, "Dato" is the date of the show, "Tekst" is the transcript, "names" is the full
# "names" of the journalist called out by Tucker Carlson, "twitter_handle", "gender", "show" is a id of the specific show, and
# "id" is row specific
tucker_df <- read_csv("")

# remove any duplicate twitter_handles (Max Boot)
tucker_df <- distinct(tucker_df, twitter_handle, .keep_all = TRUE)

# make names lower case names and title them afterwards
tucker_df$names <- str_to_title(tucker_df$names)

# add column with showtime: Tucker Carlson Tonight airs a 8:00 P.M. so I add 20 hours to the values in column "Date"
tucker_df$showtime <- as.Date(tucker_df$Date, "%y/%m/%d %h:%m:%s") + hours(20)
tucker_df$weekday = weekdays(tucker_df$Date) # add weekday to new column "weekday" 

# index column
tucker_df$treatment_id <- 1:nrow(tucker_df)

# SINGLE TREATMENT
# create dataframe for Staggered treatment adoption (binary setup) implies that if a journalist is called out by Tucker Carlson i.e.
# treated. That journalist belongs to the treatment group throughout the whole period of analysis. Thus a journalist can only 
# appear once in the analysis. The code below detect the first time a journalist was called out by Tucker and removes
# all subsequent "treatments". Data.frame goes from 577 rows to 210 rows i.e. unique treated journalist.
single_treatment <- tucker_df %>%
  group_by(twitter_handle) %>%
  slice_min(order_by = treatment_id, n = 1)

# order data.frame after Dato variable
single_treatment <- single_treatment[order(single_treatment$Date),] 

# make a seperate list for all start_dates and end_dates
single_start_date_treat <- single_treatment$start_date # five days before treatment 
single_end_date_treat <- single_treatment$end_date # five days after treatment

# make a function that will detect days between start_date and end_date
itemizeDates <- function(startDate="12-30-11", endDate="1-4-12", 
                         format="%Y-%m-%d") {
  out <- seq(as.Date(startDate, format=format), 
             as.Date(endDate, format=format), by="days")  
  format(out, format)
}

# create a data.frame with timeintervals in column and days between start_date and end_date in rows
# the dataframe contains 137 columns each with 11 rows. The first row is the start_date_treat and the last end_date_treat
# multiple_time_int_treat <- as.data.frame(mapply(itemizeDates, multiple_start_date_treat, multiple_end_date_treat))
single_time_int_treat <- as.data.frame(mapply(itemizeDates, single_start_date_treat, single_end_date_treat))

# save dataset with time_inteval for the Twitter API
#write.csv(multiple_time_int_treat,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\main_df\\multiple_time_int_treat.csv", row.names = FALSE)
write.csv(single_time_int_treat,"", row.names = FALSE)

# save dataset with proper timestamps for the Twitter API
# write.csv(multiple_treatment,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\main_df\\multiple_treatment.csv", row.names = FALSE)
# In numbers I manually add show_id unique for each show. The show where the first journalist is treated will be show 1
write.csv(single_treatment,"", row.names = FALSE)


####################################################################################################################################
                            ### J O U R N A L I S T   C O N T R O L   G R O U P   D A T A ###
####################################################################################################################################

# use not yet treated journalists as the control group. Write code that results 140 data.frames each consisting of not yet treated 
# journalists that will be used to build the search query for the control group
single_control <- read_csv("")

# order data.frame after data with the first show/treatment at the top 
control_matrix <- single_control[order(single_control$start_date),]

# create date id variable now that I have all the journalists in my treatment group
# index column
control_matrix$treatment_id <- 1:nrow(control_matrix)


# make a seperate list for all start_dates and end_dates
# multiple_start_date_treat <- multiple_treatment$start_date # five days before treatment 
# multiple_end_date_treat <- multiple_treatment$end_date # five days after treatment
single_start_date_control <- control_matrix$start_date # five days before treatment 
single_end_date_control <- control_matrix$end_date # five days after treatment

# make a function that will detect days between start_date and end_date
itemizeDates <- function(startDate="12-30-11", endDate="1-4-12", 
                         format="%Y-%m-%d") {
  out <- seq(as.Date(startDate, format=format), 
             as.Date(endDate, format=format), by="days")  
  format(out, format)
}

# convert to data.frame
single_time_int_control <- as.data.frame(mapply(itemizeDates, single_start_date_control, single_end_date_control))

# convert to single list
date_list <- as.list(single_time_int_control)
date_list <- unlist(date_list, recursive=FALSE) # unlist list of lists

# create seqeuence of dates between first and last day
all_dates <- as.character(seq(as.Date(date_list[1]), as.Date(tail(date_list, n=1)), by="days"))

# detect dates in date_list (i.e. dates for which I want to extract tweets) that are not in all_dates (i.e. all dates
# between the first and the last day)
difs <- setdiff(all_dates, date_list)

# all dates I want to extract  tweets from
dates <- setdiff(date_list, difs)

# empty list for dates that I want to extract tweets from
list_list <- c()

# The loop itarates over every treatment_id i.e. journalist in control matrix.
# Then the if statement checks if a start date comes after the initial start date for the first journalist.
# If the date does come after the first date in the control the statement is TRUE and it saves all dates
# from the very first start_date of the analysis to the first start_date for the particular journalist minus 1 day in 
# list t. Then I remove all dates for which I do not want data from i.e. all dates that are not part of the analysis.
for (i in 1:length(control_matrix$treatment_id))
{
  if(control_matrix$start_date[i] > control_matrix$start_date[1])
  {
    t <- as.character(seq(as.Date(control_matrix$start_date[1]), (as.Date(control_matrix$start_date[i]) -1), by="days"))
    b <- setdiff(t, difs) # remove all days for which I do not want tweets
    list_list[[i]] <- b # save dates to list_list
  }
}

new_list <- c()
for (i in 1:length(control_matrix$treatment_id))
{
  if(control_matrix$start_date[i] > control_matrix$start_date[1])
  {
    t <- as.character(seq(as.Date(control_matrix$start_date[1]), (as.Date(control_matrix$start_date[i]) -1), by="days"))
    b <- setdiff(t, dates) # remove all days for which I do not want tweets
    new_list[[i]] <- b # save dates to list_list
  }
}
g <- plyr::ldply(new_list, rbind)
g = t(g) # reverse dataframe so each column represent a journalist and rows dates

# data frame with dates for control group
k <- plyr::ldply(list_list, rbind)
l = t(k) # reverse dataframe so each column represent a journalist and rows dates

# save data.frame my_list_df
write.csv(l,"", row.names = FALSE)



####################################################################################################################################
           ### P R E - P R O C E S S   T U C K E R   D A T A: I N T E N S   T R E A T M E N T (removed from paper) ###
####################################################################################################################################

# load tucker data: data contains the twitter handles for all treated journalists.
# description: "URL" contains URL to transcript, "Dato" is the date of the show, "Tekst" is the transcript, "names" is the full
# "names" of the journalist called out by Tucker Carlson, "twitter_handle", "gender", "show" is a id of the specific show, and
# "id" is row specific
full_names_10 <- read_csv("")

# remove any duplicate twitter_handles (Max Boot)
full_names_10 <- distinct(full_names_10, twitter_handle, .keep_all = TRUE)

# make names lower case names and title them afterwards
full_names_10$names <- str_to_title(full_names_10$names)

# add column with showtime: Tucker Carlson Tonight airs a 8:00 P.M. so I add 20 hours to the values in column "Date"
full_names_10$showtime <- as.Date(full_names_10$Date, "%y/%m/%d %h:%m:%s") + hours(20)
full_names_10$weekday = weekdays(full_names_10$Date) # add weekday to new column "weekday" 

# index column
full_names_10$treatment_id <- 1:nrow(full_names_10)

# SINGLE TREATMENT
# create dataframe for Staggered treatment adoption (binary setup) implies that if a journalist is called out by Tucker Carlson i.e.
# treated. That journalist belongs to the treatment group throughout the whole period of analysis. Thus a journalist can only 
# appear once in the analysis. The code below detect the first time a journalist was called out by Tucker and removes
# all subsequent "treatments". Data.frame goes from 577 rows to 210 rows i.e. unique treated journalist.
single_treatment_10 <- full_names_10 %>%
  group_by(twitter_handle) %>%
  slice_min(order_by = treatment_id, n = 1)

# order data.frame after Dato variable
single_treatment_10 <- single_treatment_10[order(single_treatment_10$Date),] 

# make a seperate list for all start_dates and end_dates
single_start_date_treat_10 <- single_treatment_10$start_date # five days before treatment 
single_end_date_treat_10 <- single_treatment_10$end_date # five days after treatment

# make a function that will detect days between start_date and end_date
itemizeDates <- function(startDate="12-30-11", endDate="1-4-12", 
                         format="%Y-%m-%d") {
  out <- seq(as.Date(startDate, format=format), 
             as.Date(endDate, format=format), by="days")  
  format(out, format)
}

# create a data.frame with time intervals in column and days between start_date and end_date in rows
# the data frame contains 122 columns each with 11 rows. The first row is the start_date_treat and the last end_date_treat
# multiple_time_int_treat <- as.data.frame(mapply(itemizeDates, multiple_start_date_treat, multiple_end_date_treat))
single_time_int_treat_10 <- as.data.frame(mapply(itemizeDates, single_start_date_treat_10, single_end_date_treat_10))

# save dataset with time_inteval for the Twitter API
write.csv(single_time_int_treat_10,"", row.names = FALSE)

# save dataset with proper timestamps for the Twitter API
# write.csv(multiple_treatment,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\main_df\\multiple_treatment.csv", row.names = FALSE)
# In numbers I manually add show_id unique for each show. The show where the first journalist is treated will be show 1
write.csv(single_treatment_10,"", row.names = FALSE)


####################################################################################################################################
   ### J O U R N A L I S T   C O N T R O L   G R O U P   D A T A: I N T E N S   T R E A T M E N T (removed from paper) ###
####################################################################################################################################

# use not yet treated journalists as the control group. Write code that results 140 data.frames each consisting of not yet treated 
# journalists that will be used to build the search query for the control group
single_control_10 <- read_csv("")

# order data.frame after data with the first show/treatment at the top 
control_matrix_10 <- single_control_10[order(single_control_10$start_date),]

# create date id variable now that I have all the journalists in my treatment group
# index column
control_matrix_10$treatment_id <- 1:nrow(control_matrix_10)


# make a separate list for all start_dates and end_dates
# multiple_start_date_treat <- multiple_treatment$start_date # five days before treatment 
# multiple_end_date_treat <- multiple_treatment$end_date # five days after treatment
single_start_date_control_10 <- control_matrix_10$start_date # five days before treatment 
single_end_date_control_10 <- control_matrix_10$end_date # five days after treatment

# convert to data.frame
single_time_int_control_10 <- as.data.frame(mapply(itemizeDates, single_start_date_control_10, single_end_date_control_10))

# convert to single list
date_list_10 <- as.list(single_time_int_control_10)
date_list_10 <- unlist(date_list_10, recursive=FALSE) # unlist list of lists

# create sequence of dates between first and last day
all_dates_10 <- as.character(seq(as.Date(date_list_10[1]), as.Date(tail(date_list_10, n=1)), by = "days"))

# detect dates in date_list (i.e. dates for which I want to extract tweets) that are not in all_dates (i.e. all dates
# between the first and the last day)
difs_10 <- setdiff(all_dates_10, date_list_10)

# all dates I want to extract  tweets from
dates_10 <- setdiff(date_list_10, difs_10)

# empty list for dates that I want to extract tweets from
list_list_10 <- c()

# The loop itarates over every treatment_id i.e. journalist in control matrix.
# Then the if statement checks if a start date comes after the initial start date for the first journalist.
# If the date does come after the first date in the control the statement is TRUE and it saves all dates
# from the very first start_date of the analysis to the first start_date for the particular journalist minus 1 day in 
# list t. Then I remove all dates for which I do not want data from i.e. all dates that are not part of the analysis.
for (i in 1:length(control_matrix_10$treatment_id))
{
  if(control_matrix_10$start_date[i] > control_matrix_10$start_date[1])
  {
    t <- as.character(seq(as.Date(control_matrix_10$start_date[1]), (as.Date(control_matrix_10$start_date[i]) -1), by="days"))
    b <- setdiff(t, difs_10) # remove all days for which I do not want tweets
    list_list_10[[i]] <- b # save dates to list_list
  }
}

# data frame with dates for control group
c_10 <- plyr::ldply(list_list_10, rbind)
l_10 = t(c_10) # reverse data frame so each column represent a journalist and rows dates

# save data.frame my_list_df
write.csv(l_10,"", row.names = FALSE)




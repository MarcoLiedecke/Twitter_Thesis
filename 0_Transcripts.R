# load packages
library(LexisNexisTools)
library(striprtf)
library(tidyverse)
library(quanteda)
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
library(stringr)

# load documents
LNToutput <- lnt_read("")

# seperate data.frames in LNToutput
meta_df <- LNToutput@meta # meta data
articles_df <- LNToutput@articles # transcripts
paragraphs_df <- LNToutput@paragraphs # ???

# merge meta data and transcripts using ID column
articles_meta_df <- merge(meta_df, articles_df, by = "ID")
# select ID, Date, and Article (transcripts) column 
transcripts <- articles_meta_df %>%
  select(ID, Date, Article)

# save data.frame transcripts
write.csv(transcripts,"", row.names = FALSE)  

# convert to Quanteda format
# tokenize transcripts
toks <- tokens(transcripts$Article, what = "word",
               remove_punct = TRUE, 
               remove_symbols = TRUE,
               remove_numbers = TRUE)

# convert tokens to lower case
toks <- tokens_tolower(toks)

# create ngrams (unigrams, bigrams, and trigrams)
transcript_trigrams <- tokens_ngrams(toks, n = 1:3, concatenator = " ")

# load data.frame with all names detected in transcripts by Spacy
# the data.frame is messy with 5200 rows most of which were not actual names or names of non-journalists
names_df <- read_csv("")

# drop na rows for trial 5200 rows
names_df <- names_df %>%
  drop_na()

# select journalist names
names <- names_df %>%
  select(names)

# full names list i.e "anne applebaum"
names_list <- as.list(names)
full.list = as.list(names_list)
names(full.list) = names_list

# uni names list i.e. "anne", "applebaum" 
unlist_names <- unlist(names_list)
uni_names <- unlist(unique(strsplit(unlist_names, " ")))
uni_names <- as.list(uni_names)
uni_names <- uni_names %>% 
  flatten_chr()
uni.list = as.list(uni_names)
names(uni.list) = uni.list

# all names list i.e "anne", "applebaum, "anne applebaum"
all_names <- c(names_list, uni_names)
all_names <- all_names %>% 
  flatten_chr()
all.list = as.list(all_names)
names(all.list) = all_names


# define dictionary: journalist names I want to keep in the dfm
dict_full_names <- dictionary(full.list, tolower = TRUE) # full names in a string
dict_uni_names <- dictionary(uni.list, tolower = TRUE) # first and last name in separate strings
dict_all_names <- dictionary(all.list, tolower = TRUE) # full name in string and first and last name in separate strings

# create dfm
dfm <- dfm(transcript_trigrams, tolower = TRUE)

### FULL NAMES (bigrams/trigrams)
dfm_full_names = dfm_select(dfm, pattern = dict_full_names) # apply custom dict 
df_full_names <- convert(dfm_full_names, to = "data.frame") # convert to df
df_full_names <- rowid_to_column(df_full_names, "show_id")


### FIRST AND LAST NAME (unigrams)
dfm_uni_names = dfm_select(dfm, pattern = dict_uni_names) # apply custom dict
df_uni_names <- convert(dfm_uni_names, to = "data.frame") # convert to df


### FULL NAMES AND FIRST AND LAST NAME (unigrams/bigrams/trigrams)
dfm_all_names = dfm_select(dfm, pattern = dict_all_names) # apply custom dict
df_all_names <- convert(dfm_all_names, to = "data.frame") # convert to df


# remove all features/journalist with less than X mentions within a document 
#df_full_1 <- df_full_names[sapply(df_full_names, function(x) max(x, na.rm = T) >= 1)]
df_full <- df_full_names[sapply(df_full_names, function(x) max(x, na.rm = T) >= 5)]
df_full_5_9 <- df_full_names[sapply(df_full_names, function(x) max(x, na.rm = T) %in% (5:9))]
#df_full_6 <- df_full_names[sapply(df_full_names, function(x) max(x, na.rm = T) >= 6)]
#df_full_7 <- df_full_names[sapply(df_full_names, function(x) max(x, na.rm = T) >= 7)]
#df_full_8 <- df_full_names[sapply(df_full_names, function(x) max(x, na.rm = T) >= 8)]
#df_full_9 <- df_full_names[sapply(df_full_names, function(x) max(x, na.rm = T) >= 9)]
df_full_10 <- df_full_names[sapply(df_full_names, function(x) max(x, na.rm = T) >= 10)]
#df_uni <- df_uni_names[sapply(df_uni_names, function(x) max(x, na.rm = T) >= 5)]
#df_all <- df_all_names[sapply(df_all_names, function(x) max(x, na.rm = T) >= 5)]

### isolate days for which a journalists have been called out at least 5 times
# remove all rows that does not have a value equal or above 4
# rename show_id to ID of mergning 
df_full <- df_full %>% 
  filter_at(vars(3:144), any_vars(. >= 5)) %>% 
  dplyr::rename("ID" = "show_id")

### FOR APPENDIX
# count total number of treatments in case a journalist could be treated more than once
df_full_descriptive <- df_full %>% 
  dplyr::select(c(3:144))
df_full_descriptive <- data.frame(stack(df_full_descriptive[1:ncol(df_full_descriptive)]))
sum(df_full_descriptive$values >= 5) # 621 times of treatment

df_full_descriptive <- df_full_descriptive %>% 
  filter(values >= 5)

df_full_descriptive <- count(df_full_descriptive$ind)
df_full_descriptive$freq <- as.numeric(df_full_descriptive$freq)
df_full_descriptive <- df_full_descriptive[order(-df_full_descriptive$freq),]
###

df_full <- merge(df_full, transcripts, by = "ID")

# remove article column as takes a lot of memory
df_full$Article <- NULL

# Make a list of the first show_id for each journalist with a mention rate equal or above 6
list_dates <- list()
class(list_dates)
for (i in c(3:144)){
  dates <- df_full[df_full[i] >= 5,'ID']
  list_dates[i] <- dates
}

# remove empty list elements
list_dates <- Filter(Negate(is.null), list_dates)

# create a list of the journalist names 
k <- as.list(names(df_full[3:144]))

# merge show_id with names list and convert to data.frame
tt <- do.call(rbind, Map(data.frame, ID=list_dates, names=k))

# merge data.frame with show_id and names with dates data.frame
dates_df <- df_full %>% 
  select(ID, Date)
full_names <- merge(tt, dates_df, by = "ID")
 

# add timeinterval for extraction of tweets +- 5 days from the day of the show
# the interval of +- 5 days is somewhat arbitrary. However, do to the monthly extraction limit of 10 milion the interval 
# must be limited
full_names$start_date <- as.Date(full_names$Date, "%y/%m/%d") - days(5)
full_names$end_date <- as.Date(full_names$Date, "%y/%m/%d") + days(5)

# date.frame with ID, names, twitter_handle, media, gender, Date, start_date, end_date
full_names <- merge(names_df, full_names, by = "names")

# sort data.frame after date
full_names <- full_names[order(full_names$Date),]

# save data.frame date.frame
write.csv(full_names,"", row.names = FALSE)  


# isolate days for which a journalists have been called out at least 10 times
df_full_10 <- df_full_10 %>% 
  filter_at(vars(3:32), any_vars(. >= 10)) %>% 
  dplyr::rename("ID" = "show_id")


### FOR APPENDIX
# count total number of treatments in case a journalist could be treated more than once
df_full_descriptive_10 <- df_full_10 %>% 
  dplyr::select(c(3:32))
df_full_descriptive_10 <- data.frame(stack(df_full_descriptive_10[1:ncol(df_full_descriptive_10)]))
sum(df_full_descriptive_10$values >= 10) # 621 times of treatment

df_full_descriptive_10 <- df_full_descriptive_10 %>% 
  filter(values >= 10)

df_full_descriptive_10 <- count(df_full_descriptive_10$ind)
df_full_descriptive_10$freq <- as.numeric(df_full_descriptive_10$freq)
df_full_descriptive_10 <- df_full_descriptive_10[order(-df_full_descriptive_10$freq),]
###




# merge data.frame with intensly treated journalists and transcript data.frame
df_full_10 <- merge(df_full_10, transcripts, by = "ID")

# remove article column as takes a lot of memory
df_full_10$Article <- NULL

# Make a list of the first show_id for each journalist with a mention rate equal or above 10
list_dates <- list()
for (i in c(3:32)){
  dates <- df_full_10[df_full_10[i] >= 10,'ID']
  list_dates[i] <- dates
}

# remove empty list elements
list_dates <- Filter(Negate(is.null), list_dates)

# create a list of the journalist names 
t_10 <- as.list(names(df_full_10[3:32]))

# merge show_id with names list and convert to data.frame
t_10 <- do.call(rbind, Map(data.frame, ID = list_dates, names = t_10))

# merge data.frame with show_id and names with dates data.frame
df_full_10_dates <- df_full_10 %>% 
  select(ID, Date)
full_names_10 <- merge(t_10, df_full_10_dates, by = "ID")


# add timeinterval for extraction of tweets +- 5 days from the day of the show
# the interval of +- 5 days is somewhat arbitrary. However, do to the monthly extraction limit of 10 milion the interval 
# must be limited
full_names_10$start_date <- as.Date(full_names_10$Date, "%y/%m/%d") - days(5)
full_names_10$end_date <- as.Date(full_names_10$Date, "%y/%m/%d") + days(5)

# date.frame with ID, names, twitter_handle, media, gender, Date, start_date, end_date
full_names_10 <- merge(names_df, full_names_10, by = "names")

# sort data.frame after date
full_names_10 <- full_names_10[order(full_names_10$Date),]

# save data.frame date.frame
write.csv(full_names_10,"", row.names = FALSE)  


# isolate days for which a journalists have been called out at least 10 times
# id column was removed when filterins so I create a new id column 
df_full_5_9$show_id <- 1:nrow(df_full_5_9)

df_full_5_9 <- df_full_5_9 %>% 
  filter_at(vars(1:112), any_vars(. <= 9)) %>% 
  filter_at(vars(1:112), any_vars(. >= 5)) %>% 
  dplyr::rename("ID" = "show_id")

# merge data.frame with intensly treated journalists and transcript data.frame
df_full_5_9 <- merge(df_full_5_9, transcripts, by = "ID")

# remove article column as takes a lot of memory
df_full_5_9$Article <- NULL

# Make a list of the first show_id for each journalist with a mention rate equal or above 10
list_dates <- list()
for (i in c(2:113)){
  dates <- df_full_5_9[df_full_5_9[i] >= 5,'ID']
  list_dates[i] <- dates
}

# remove empty list elements
list_dates <- Filter(Negate(is.null), list_dates)

# create a list of the journalist names 
t_5_9 <- as.list(names(df_full_5_9[2:113]))

# merge show_id with names list and convert to data.frame
t_5_9 <- do.call(rbind, Map(data.frame, ID = list_dates, names = t_5_9))

# merge data.frame with show_id and names with dates data.frame
df_full_5_9_dates <- df_full_5_9 %>% 
  select(ID, Date)
full_names_5_9 <- merge(t_5_9, df_full_5_9_dates, by = "ID")


# add timeinterval for extraction of tweets +- 5 days from the day of the show
# the interval of +- 5 days is somewhat arbitrary. However, do to the monthly extraction limit of 10 milion the interval 
# must be limited
full_names_5_9$start_date <- as.Date(full_names_5_9$Date, "%y/%m/%d") - days(5)
full_names_5_9$end_date <- as.Date(full_names_5_9$Date, "%y/%m/%d") + days(5)

# date.frame with ID, names, twitter_handle, media, gender, Date, start_date, end_date
full_names_5_9 <- merge(names_df, full_names_5_9, by = "names")

# sort data.frame after date
full_names_5_9 <- full_names_5_9[order(full_names_5_9$Date),]

# save data.frame date.frame
write.csv(full_names_5_9,"", row.names = FALSE)  







####################################################################################################################################
### LOAD   PACKAGES
####################################################################################################################################

library(tidyverse) # for everything 
library(peRspective) # API scoring tweets
library(lubridate) # date related
library(anytime) # also date related
library(curl)
library(fuzzyjoin) # working with strings
library(fs)
library(data.table)
library(stargazer) # pretty regression output
library(plyr)
library(did)
library(ggpubr)
library(stringr)
library(forcats)
library(lfe) # did estimator 
library(RColorBrewer) # colors
library(parallel) # multithreading
library(zoo)
library(hms)
library(ggthemes)
library(devtools)
#library(bacondecomp)
library(showtext) # roboto font
library(grid)
#library(modelsummary) # descriptive statistics table
#library(gt)
#library(MASS)
library(summarytools)
library(data.table)
library(xtable)
library(scales)
library(fixest) # event study
library(broom) # tidy regression output to data.frame
library(Synth) # synthetic controls
#library(TwoWayFEWeights) # Estimation of the Weights Attached to the Two-Way Fixed Effects Regressions
#library(pretrends)

## Loading Google fonts (https://fonts.google.com/)
font_add_google("Roboto Mono")

## Automatically use showtext to render text
showtext_auto()

####################################################################################################################################
#1 LOAD TWEETS FOR MAIN ANALYSIS
####################################################################################################################################

### HYPOTHESIS 1
# treatment group (hypothesis 1)
# load csv files for treatment group called out 5 times or more 
treatment_file_path <- dir_ls("")
treatment_file_content <- list()

# loop through file_path and load all csv files for treatment group into list file_content 
for (i in seq_along(treatment_file_path)) {
  treatment_file_content[[i]] <- read_csv(
    file = treatment_file_path[[i]]
  )
}

# control group (hypthesis 1)
# load csv files for control group
control_file_path <- dir_ls("")
control_file_content <- list()

# loop through file_path and load all csv files for control group into list file_content 
for (i in seq_along(control_file_path)) {
  control_file_content[[i]] <- read_csv(
    file = control_file_path[[i]]
  )
}

# rowbind all csv files for treatment group and control group separately (hypthesis 1)
treatment_file_content[[1]]$created_at <- as.character(treatment_file_content[[1]]$created_at) # convert POSXCT to character
treatment_group <- ldply(treatment_file_content, use.names = TRUE) # treatment group
control_file_content[[1]]$created_at <- as.character(control_file_content[[1]]$created_at) # convert POSXCT to character
control_group <- ldply(control_file_content, use.names = TRUE) # control group


# remove all headers and tweets with more than one @twitter_handle to make sure all tweets
# only relates to the specific treated journalist. Lastly, remove all duplicates (not a problem after new python code)  
treatment_group <- treatment_group %>%
  filter(id != "id") %>%
  filter(!str_count(tweet, "@") > 1) %>%
  mutate(journalist_id = as.numeric(journalist_id)) %>%
  arrange(journalist_id) %>%
  distinct()

control_group <- control_group %>%
  filter(id != "id") %>%
  filter(!str_count(tweet, "@") > 1) %>%
  mutate(journalist_id = as.numeric(journalist_id)) %>%
  arrange(journalist_id) %>%
  distinct()


# add id column to treatment group and control group. 
# This step is required by the Perspective API as an input variable
treatment_group <- rowid_to_column(treatment_group, "text_id")
control_group <- rowid_to_column(control_group, "text_id")


### SPLIT DATA SETS INTO SUB-DATASET FOR FASTER SCORING OF THE PERSPECTIVE API
# due to slow a rate of scoring tweets approx. 200 a minute (quota cap. is 1800 tweets a minute) I run the scoring process 
# in several R session simultaniously. Therefore I split my data.frame into several smaller data.frames
# treatment group
#t1 <- treatment_group %>% 
#  slice(0:120000)
#t2 <- treatment_group %>% 
#  slice(120001:240000)
#t3 <- treatment_group %>% 
#  slice(240001:360000)
#t4 <- treatment_group %>% 
#  slice(360001:480000)
#t5 <- treatment_group %>% 
#  slice(480001:609177)
#write.csv(t1,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\treatment_group\\t1.csv", 
#          row.names = FALSE)
#write.csv(t2,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\treatment_group\\t2.csv", 
#          row.names = FALSE)
#write.csv(t3,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\treatment_group\\t3.csv", 
#          row.names = FALSE)
#write.csv(t4,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\treatment_group\\t4.csv", 
#          row.names = FALSE)
#write.csv(t5,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\treatment_group\\t5.csv", 
#          row.names = FALSE)


# control group
#c1 <- control_group %>% 
#  slice(0:320000)
#c2 <- control_group %>% 
#  slice(320001:640000)
#c3 <- control_group %>% 
#  slice(640001:960000)
#c4 <- control_group %>% 
#  slice(960001:1280000)
#c5 <- control_group %>% 
#  slice(1280001:1634178)
#c6 <- control_group %>% 
#  slice(1634178:1807358)
#write.csv(c1,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c1.csv", 
#          row.names = FALSE)
#write.csv(c2,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c2.csv", 
#          row.names = FALSE)
#write.csv(c3,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c3.csv", 
#          row.names = FALSE)
#write.csv(c4,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c4.csv", 
#          row.names = FALSE)
#write.csv(c5,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c5.csv", 
#          row.names = FALSE)
#write.csv(c6,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c6.csv", 
#          row.names = FALSE)


# load treatment_group_scores and control_group_scores
t1_scores <- read_csv("")
t2_scores <- read_csv("")
t3_scores <- read_csv("")
t4_scores <- read_csv("")
t5_scores <- read_csv("")
treatment_group_scores <- do.call("rbind", list(t1_scores, t2_scores, t3_scores, t4_scores, t5_scores)) # row bind all data.frames

# control group
c1_scores <- read_csv("")
c2_scores <- read_csv("")
c3_scores <- read_csv("")
c4_scores <- read_csv("")
c5_scores <- read_csv("")
c6_scores <- read_csv("")
control_group_scores <- do.call("rbind", list(c1_scores, c2_scores, c3_scores, c4_scores, c5_scores, c6_scores)) # row bind all data.frames

# merge data with Perpsective scores with data on journalsits 
treatment <- merge(treatment_group_scores, treatment_group, by = "text_id")
control <- merge(control_group_scores, control_group, by = "text_id")


# load dataset with details on treatment group
# variables: names, twitter_handle, media, gender, weekday, ID, treatment_id, showtime, date, start_date, and end_date
tucker_df <- read_csv("")

# rename "treatment_id" to "journalist_id", select columns "date_id", "journalist_id", "show_id", "names", "twitter_handle", 
# "showtime", "gender"
treatment_names <- tucker_df %>%
  dplyr::rename(journalist_id = treatment_id) %>%
  dplyr::rename(show_date = Date) %>%
  dplyr::select(journalist_id, names, twitter_handle, showtime, gender, media, show_date)

# merge treatment names and treatment group in order to add background data such as name, gender, and showtime
treatment <- merge(treatment_names, treatment, by = "journalist_id")
control <- merge(treatment_names, control, by = "journalist_id")


# TREATMENT GROUP
# clean treatment group: select relevant columns, create day column, treatment dummy, group, and day count. Drop all NaN
c_treatment <- treatment %>%
  #filter(created_at != 'created_at') %>%
  drop_na() %>%
  dplyr::select(names, journalist_id, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT, tweet,
                text_id, created_at, showtime, show_date, gender, media) %>%
  mutate(created_at = ymd_hms(created_at),
         group = "treatment",
         #group = case_when(created_at > showtime ~ "treatment",
         #                  created_at < showtime ~ "control"),
         date = as_date(created_at),
         day_count = as.numeric(factor(date)),
         tal = 1,
         treatment_dummy = 1,
         time_to_treat = as.numeric(difftime(date, show_date , units = c("days"))))


# add time dummy: if a tweet was tweeted after the journalist was mentioned by Tucker it is 1 i.e. treated and 
# if the tweet was tweeted before treatment it is 0
#tg$treatment_dummy <- ifelse(tg$showtime <= as_datetime(tg$created_at), 1, 0)

# add time dummy: if a tweet was tweeted after the journalist was mentioned by Tucker it is 1 i.e. treated and 
# if the tweet was tweeted before treatment it is 0
#treatment_group_10$treatment_dummy <- ifelse(treatment_group_10$showtime <= as_datetime(treatment_group_10$created_at), 1, 0)

# CONTROL GROUP
# clean control group: select relevant columns, create day column, treatment dummy, group, and day count. Drop all NaN
c_control <- control %>%
  filter(created_at != 'created_at') %>%
  drop_na() %>%
  dplyr::select(names, journalist_id, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT, tweet,
                text_id, created_at, showtime, show_date, gender, media) %>%
  mutate(group = "control",
         date = as_date(created_at),
         treatment_dummy = 0,
         tal = 1,
         time_to_treat = 0)


# combine treatment group and control group data frame will
motherload <- rbind.fill(c_control, c_treatment)

# add day_count variable. day 1 starts with the first day 2018-11-22 in Jim Acostas period as 
# he is the first treated journalist
motherload <- motherload %>%
  mutate(day_count = as.numeric(factor(date))) %>%
  group_by(journalist_id) %>%
  #mutate(gname = case_when(treatment_dummy == 1 ~ first(day_count) + 5,
  #                         treatment_dummy == 0 ~ 0)) %>%
  mutate(gname = case_when(treatment_dummy == 1 ~ first(day_count) + 5,
                           treatment_dummy == 0 ~ first(day_count) + 5)) %>%
  ungroup(journalist_id)


# save motherload data set
#write.csv(motherload,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\motherload.csv", 
#          row.names = FALSE)


####################################################################################################################################
### 3) PERSPECTIVE API: ESTIMATE PROBABILITY SCORES
####################################################################################################################################
# Tweets are scored using the following code. However, tweets are scored in several different sessions as a way using multithreading.
# That is also way the data.frame for treatment and control are split into smaller data.frames. 

# API key: Google Perspective 
#Sys.setenv(perspective_api_key = "MY KEY")
#perspective_api_key = "MY KEY"


# TREATMENT GROUP
#treatment_group_scores <- treatment_group %>%
#  prsp_stream(text = tweet,
#             text_id = text_id,
#             score_model = c("TOXICITY", "SEVERE_TOXICITY", "SEXUALLY_EXPLICIT", "IDENTITY_ATTACK", "INSULT", "THREAT"),
#             score_model = "TOXICITY",
#             safe_output = TRUE,
#             sleep = 0.034,
#             verbose = T
#  )
# save treatment_group_score
#write.csv(treatment_group_scores,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\treatment_group\\treatment_group_scores.csv", 
          #row.names = FALSE)


# CONTROL GROUP
#control_group_scores <- control_group %>%
#  prsp_stream(text = tweet,
#              text_id = text_id,
#              score_model = c("TOXICITY", "SEVERE_TOXICITY", "SEXUALLY_EXPLICIT", "IDENTITY_ATTACK", "INSULT", "THREAT"),
#              safe_output = TRUE,
#              sleep = 0.034,
#              verbose = T
#  )
# save control_group_score
#write.csv(control_group_scores,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\control_group_scores.csv",
          #row.names = FALSE) 


####################################################################################################################################
### 4) CORRELATION TABLES FOR ATTRIBUTES
####################################################################################################################################

correaltion_df <- motherload %>%
  select(TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT)

correlation.matrix <- cor(correaltion_df[,c("TOXICITY", "SEVERE_TOXICITY", "SEXUALLY_EXPLICIT", "IDENTITY_ATTACK", "INSULT", "THREAT")])
stargazer(correlation.matrix, title="Correlation Matrix")

####################################################################################################################################
### 5) FIGURE 4
####################################################################################################################################

# identify the 20 journalists that receive most online harassment 
most_hated_jr <- motherload %>%
  group_by(names) %>%
  mutate(mean_TOXICITY = mean(TOXICITY, na.rm=TRUE),
         mean_SEVERE_TOXICITY = mean(SEVERE_TOXICITY, na.rm=TRUE),
         mean_SEXUALLY_EXPLICIT = mean(SEXUALLY_EXPLICIT, na.rm=TRUE),
         mean_INSULT = mean(INSULT, na.rm=TRUE),
         mean_IDENTITY_ATTACK = mean(IDENTITY_ATTACK, na.rm=TRUE),
         mean_THREAT = mean(THREAT, na.rm=TRUE)) %>%
  select(names, gender, mean_TOXICITY, mean_SEVERE_TOXICITY, mean_SEXUALLY_EXPLICIT, mean_INSULT, mean_IDENTITY_ATTACK, mean_THREAT) %>%
  distinct()

most_hated_jr %>%
  group_by(gender) %>%
  dplyr::count(gender)
most_hated_jr %>%
  group_by(gender) %>%
  dplyr::summarize(gender_mean_TOXICITY = mean(mean_TOXICITY)) 
most_hated_jr %>%
  group_by(gender) %>%
  dplyr::summarize(gender_mean_SEVERE_TOXICITY = mean(mean_SEVERE_TOXICITY))
most_hated_jr %>%
  group_by(gender) %>%
  dplyr::summarize(gender_mean_SEXUALLY_EXPLICIT = mean(mean_SEXUALLY_EXPLICIT))
most_hated_jr %>%
  group_by(gender) %>%
  dplyr::summarize(gender_mean_INSULT = mean(mean_INSULT))
most_hated_jr %>%
  group_by(gender) %>%
  dplyr::summarize(gender_mean_IDENTITY_ATTACK = mean(mean_IDENTITY_ATTACK))
most_hated_jr %>%
  group_by(gender) %>%
  dplyr::summarize(gender_mean_THREAT = mean(mean_THREAT))

colMeans(most_hated_jr[, c(3, 4, 5, 6, 7, 8)]) # mean attribute probability scores for treated journalist in the post treatment period


# function moves text to next line
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}


fig_mean_TOXICITY <- ggplot(most_hated_jr, aes(x = fct_rev(fct_reorder(names, mean_TOXICITY)), y = mean_TOXICITY, fill = gender)) +
  geom_col(width = 0.6) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_text(size=8),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = 0.138112, color = "black", linetype = "dashed", size = 0.3) +
  annotate("text", x = 108, y = 0.158112, parse = TRUE, label = "Avg.", size = 4, family = "Roboto Mono") +
  annotate("text", x = 120, y = 0.158112, parse = TRUE, label = "0.14", size = 4, family = "Roboto Mono") +
  geom_hline(yintercept = 0.140, color = "#66aaf4", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0.135, color = "#f46966", linetype = "dashed", size = 0.3) +
  scale_fill_manual("", values = c("M" = "#66aaf4", "W" = "#f46966"), 
                    labels = addline_format(c("Men n=83", "Women n=54"))) +  
  scale_y_continuous(limits = c(0.0, 0.42),
                     expand = c(0, 0)) +
  xlab("Journalists by column") +
  ylab("TOXICITY score")


fig_mean_SEVERE_TOXICITY <- ggplot(most_hated_jr, aes(x = fct_rev(fct_reorder(names, mean_SEVERE_TOXICITY)), y = mean_SEVERE_TOXICITY, fill = gender)) +
  geom_col(width = 0.6) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_text(size=8),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25),) +
  geom_hline(yintercept = 0, color = "black") +
  annotate("text", x = 108, y = 0.1429555, parse = TRUE, label = "Avg", size = 4, family = "Roboto Mono") +
  annotate("text", x = 120, y = 0.1429555, parse = TRUE, label = "0.12", size = 4, family = "Roboto Mono") +
  geom_hline(yintercept =  0.123, color = "#66aaf4", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0.123, color = "#f46966", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0.1229555, color = "black", linetype = "dashed", size = 0.3) +
  scale_fill_manual("", values = c("M" = "#66aaf4", "W" = "#f46966"), 
                    labels = addline_format(c("Men n=83", "Women n=54"))) + 
  scale_y_continuous(limits = c(0.0, 0.42),
                     expand = c(0, 0)) +
  xlab("Journalists by column") +
  ylab("SEVERE TOXICITY score")

fig_mean_SEXUALLY_EXPLICIT <- ggplot(most_hated_jr, aes(x = fct_rev(fct_reorder(names, mean_SEXUALLY_EXPLICIT)), y = mean_SEXUALLY_EXPLICIT, fill = gender)) +
  geom_col(width = 0.6) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_text(size=8),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25),) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = 0.1144154, color = "black", linetype = "dashed", size = 0.3) +
  annotate("text", x = 108, y = 0.1344154, parse = TRUE, label = "Avg.", size = 4, family = "Roboto Mono") +
  annotate("text", x = 120, y = 0.1344154, parse = TRUE, label = "0.11", size = 4, family = "Roboto Mono") +
  geom_hline(yintercept =  0.110, color = "#66aaf4", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0.122, color = "#f46966", linetype = "dashed", size = 0.3) +
  scale_fill_manual("", values = c("M" = "#66aaf4", "W" = "#f46966"), 
                    labels = addline_format(c("Men n=83", "Women n=54"))) + 
  scale_y_continuous(limits = c(0.0, 0.42),
                     expand = c(0, 0)) +
  xlab("Journalists by column") +
  ylab("SEXUALLY EXPLICIT score")


fig_mean_INSULT <- ggplot(most_hated_jr, aes(x = fct_rev(fct_reorder(names, mean_INSULT)), y = mean_INSULT, fill = gender)) +
  geom_col(width = 0.6) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_text(size=8),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25),) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = 0.2124842, color = "black", linetype = "dashed", size = 0.3) +
  annotate("text", x = 108, y = 0.2324842, parse = TRUE, label = "Avg.", size = 4, family = "Roboto Mono") +
  annotate("text", x = 120, y = 0.2324842, parse = TRUE, label = "0.21", size = 4, family = "Roboto Mono") +
  geom_hline(yintercept =  0.215, color = "#66aaf4", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0.208, color = "#f46966", linetype = "dashed", size = 0.3) +
  scale_fill_manual("", values = c("M" = "#66aaf4", "W" = "#f46966"), 
                    labels = addline_format(c("Men n=83", "Women n=54"))) + 
  scale_y_continuous(limits = c(0.0, 0.42),
                     expand = c(0, 0)) +
  xlab("Journalists by column") +
  ylab("INSULT score")


fig_mean_IDENTITY_ATTACK <- ggplot(most_hated_jr, aes(x = fct_rev(fct_reorder(names, mean_IDENTITY_ATTACK)), y = mean_IDENTITY_ATTACK, fill = gender)) +
  geom_col(width = 0.6) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_text(size=8),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25),) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = 0.1468604, color = "black", linetype = "dashed", size = 0.3) +
  annotate("text", x = 108, y = 0.1668604, parse = TRUE, label = "Avg.", size = 4, family = "Roboto Mono") +
  annotate("text", x = 120, y = 0.1668604, parse = TRUE, label = "0.15", size = 4, family = "Roboto Mono") +
  geom_hline(yintercept =  0.146, color = "#66aaf4", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0.148, color = "#f46966", linetype = "dashed", size = 0.3) +
  scale_fill_manual("", values = c("M" = "#66aaf4", "W" = "#f46966"), 
                    labels = addline_format(c("Men n=83", "Women n=54"))) +  
  scale_y_continuous(limits = c(0.0, 0.42),
                     expand = c(0, 0)) +
  xlab("Journalists by column") +
  ylab("IDENTITY ATTACK score")
  

fig_mean_THREAT <- ggplot(most_hated_jr, aes(x = fct_rev(fct_reorder(names, mean_THREAT)), y = mean_THREAT, fill = gender)) +
  geom_col(width = 0.6) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_text(size=8),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25),) +
  geom_hline(yintercept = 0, color = "black") +
  annotate("text", x = 108, y = 0.1828085, parse = TRUE, label = "Avg.", size = 4, family = "Roboto Mono") +
  annotate("text", x = 120, y = 0.1828085, parse = TRUE, label = "0.16", size = 4, family = "Roboto Mono") +
  geom_hline(yintercept =  0.162, color = "#66aaf4", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0.165, color = "#f46966", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0.1628085, color = "black", linetype = "dashed", size = 0.3) +
  scale_fill_manual("", values = c("M" = "#66aaf4", "W" = "#f46966"), 
                    labels = addline_format(c("Men n=83", "Women n=54"))) +
  scale_y_continuous(limits = c(0.0, 0.42),
                     expand = c(0, 0)) +
  xlab("Journalists by column") +
  ylab("THREAT score")

fig_mean_online_harassment <- ggarrange(fig_mean_TOXICITY, fig_mean_SEVERE_TOXICITY, fig_mean_SEXUALLY_EXPLICIT, 
                                       fig_mean_IDENTITY_ATTACK, fig_mean_INSULT, fig_mean_THREAT,
                                      ncol = 2, nrow = 3,
                                      common.legend = TRUE, legend = "bottom")


# save fig_mean_online_harassment
ggsave(path = "", 
       "fig_mean_online_harassment.pdf", width = 10, height = 6)




####################################################################################################################################
### 6) FIGURE 2
####################################################################################################################################

# Difference in differences sample figure
df <- data.frame (journalist  = c("A", "A", 
                                  "A", "A", 
                                  "B", "B", "B", "B",
                                  "B", "B",
                                  "C", "C", "C", "C", "C"),
                  Score = c(0.52, 0.60, 
                            0.65, 0.73,
                            0.32, 0.40, 0.48, 0.56,
                            0.62, 0.7,
                            0.24, 0.32, 0.40, 0.48, 0.56),
                  Time = c(0, 1,
                           1, 2,
                           0, 1, 2, 3,
                           3, 4,
                           0, 1, 2, 3, 4)
)


text_high <- textGrob("t1", gp=gpar(fontsize = 10, fontfamily = "Roboto Mono"))
text_low <- textGrob("t2", gp=gpar(fontsize = 10, fontfamily = "Roboto Mono"))

text_high <- "t[a]^*"
text_low <- "t[b]^*"
  
label1 <- "italic(y[it]^a)"
label2 <- "italic(y[it]^b)"
label3 <- "italic(y[it]^c)"


did_fig <- ggplot(df, aes(x = Time, y = Score, group = journalist,  linetype = journalist)) +
  geom_line(size = 0.6) +
  scale_x_continuous(limits = c(-0.05, 4.05),
                     expand = c(0, 0)) +
  theme_minimal() +
  geom_hline(yintercept = 0) + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        text = element_text(family = "Roboto Mono", size = 15)) +
  scale_y_continuous(limits = c(-0.15, 0.83),
                     expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1, color = "#f46966"),
             size = 1) +
  geom_vline(aes(xintercept = 3, color = "#f46966"),
             size = 1) +
  annotate("text", x = 0.5, y = 0.04, parse = TRUE, label = "PRE(a)", size = 4, family = "Roboto Mono") +
  annotate("text", x = 2, y = 0.04, parse = TRUE, label = "MID(a,b)", size = 4, family = "Roboto Mono") +
  annotate("text", x = 3.5, y = 0.04, parse = TRUE, label = "POST(b)", size = 4, family = "Roboto Mono") +
  annotate("text", x = 0.5, y = 0.61, parse = TRUE, label = label1, size = 6, family = "Roboto Mono") +
  annotate("text", x = 2, y = 0.53, parse = TRUE, label =  label2, size = 6, family = "Roboto Mono") +
  annotate("text", x = 3.5, y = 0.57, parse = TRUE, label = label3, size = 6, family = "Roboto Mono") +
  #annotate("text", x = 3.5, y = -0.57, parse = TRUE, label = text_high, size = 6, family = "Roboto Mono") +
  #annotate("text", x = 3.5, y = -0.57, parse = TRUE, label = text_low, size = 6, family = "Roboto Mono") +
  geom_segment(lineend = "butt", linejoin = "mitre", size = 0.2,
               aes(x = 0.8, y = 0.04, xend = 0.95, yend = 0.04),
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(lineend = "butt", linejoin = "mitre", size = 0.2,
               aes(x = 2.45, y = 0.04, xend = 2.95, yend = 0.04),
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(lineend = "butt", linejoin = "mitre", size = 0.2,
               aes(x = 3.8, y = 0.04, xend = 3.95, yend = 0.04),
               arrow = arrow(length = unit(0.3, "cm"))) + 
  geom_segment(lineend = "butt", linejoin = "mitre", size = 0.2,
             aes(x = 0.20, y = 0.04, xend = 0.05, yend = 0.04),
             arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(lineend = "butt", linejoin = "mitre", size = 0.2,
               aes(x = 1.55, y = 0.04, xend = 1.05, yend = 0.04),
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(lineend = "butt", linejoin = "mitre", size = 0.2,
               aes(x = 3.20, y = 0.04, xend = 3.05, yend = 0.04),
               arrow = arrow(length = unit(0.3, "cm"))) +
  annotation_custom(text_high, xmin = 1, xmax = 1, ymin = -0.02, ymax = -0.02)  + 
  annotation_custom(text_low, xmin = 3, xmax = 3, ymin = -0.02, ymax = -0.02) +
  coord_cartesian(ylim = c(-0.0, 0.83), clip = "off")


# save bar plot on distribution of tweets
ggsave(path = "", 
       "did_fig.pdf", width = 6, height = 4)


####################################################################################################################################
### 7) FIGURE 3
####################################################################################################################################


# load treatment group for descripitve statistics
descriptive <- read_csv("")

# create a media count variable
descriptive <- descriptive %>%
  group_by(media) %>%
  mutate(count = n()) 

# count for gender
count(descriptive$gender)

# plot the distribution of media in bar plot
media_gender_distribution <- ggplot(descriptive, aes(x = fct_infreq(media), fill = gender)) +
  geom_bar(position = "stack", stat = "count", alpha=0.8) +
  scale_y_continuous(limits = c(0, 47),
                     expand = c(0, 0)) +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  ylab("") +
  scale_fill_manual("", values = c("W" = "#f46966", "M" = "#66aaf4"),
                    labels = addline_format(c("Female n=54", "Male n=83"))) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.text = element_text(size = 12),
        text = element_text(family = "Roboto Mono", size = 15),
        axis.text.x = element_text(angle = 45, vjust = 1.03, hjust = 1, size = 10))
# save plot with media and gender distribution
ggsave(path = "", 
       "media_gender_distribution.pdf", width = 10, height = 4)



####################################################################################################################################
### 8) Figure 3: Distribution of tweets ###
####################################################################################################################################

# caluclate daily average tweets
average <- motherload %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(tweets_per_day = n())

mean(average$tweets_per_day)

motherload <- motherload %>% 
  mutate(factor_date = as.factor(date))

# plot distribution of tweets
distribution_tweets <- ggplot(motherload, aes(x = factor_date, y = TOXICITY, fill = group)) +
  geom_bar(stat="identity", position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0, 0)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title=element_blank(),
        panel.ontop = TRUE,
        text = element_text(family = "Roboto Mono", size = 15),
        legend.position = "bottom") +
  scale_fill_manual(labels = c("Control", "Treatment"), values = c("#CCCCCC", "#595959")) +
  #scale_x_discrete(breaks=c("1","200","400", "600", "800"),
                   #labels=c("day 1","day 200","day 400", "day 600", "day 800"))
  #scale_y_continuous(limits = c(0, 1500),
                     #expand = c(0, 0)) +
 xlab("Time") + ylab("")

# save bar plot on distribution of tweets
ggsave(path = "", 
       "distribution_tweets.pdf", width = 10, height = 3)

####################################################################################################################################
 ###  P O I N T   P L O T   F O R   T R E A T M E N T   A N D   C O N T R O L   G R O U P ###
####################################################################################################################################


# geom point plot with geom smooth of all six categories
# extract time stamp from datetime where a tweet was tweeted
motherload$hms <- format(as_datetime(motherload$created_at),   # Extract hours, minutes & seconds
                         format = "%H:%M:%S")

# create a 1-11 variable for each journalist
motherload <- motherload %>%
  group_by(journalist_id) %>%
  mutate(indiviual_count = as.numeric(factor(date))) %>%
  mutate(fake_date = case_when(indiviual_count == 1 ~ as.Date("2016-11-01 00:00:00"),
                               indiviual_count == 2 ~ as.Date("2016-11-02 00:00:00"),
                               indiviual_count == 3 ~ as.Date("2016-11-03 00:00:00"),
                               indiviual_count == 4 ~ as.Date("2016-11-04 00:00:00"),
                               indiviual_count == 5 ~ as.Date("2016-11-05 00:00:00"),
                               indiviual_count == 6 ~ as.Date("2016-11-06 00:00:00"),
                               indiviual_count == 7 ~ as.Date("2016-11-07 00:00:00"),
                               indiviual_count == 8 ~ as.Date("2016-11-08 00:00:00"),
                               indiviual_count == 9 ~ as.Date("2016-11-09 00:00:00"),
                               indiviual_count == 10 ~ as.Date("2016-11-10 00:00:00"),
                               indiviual_count == 11 ~ as.Date("2016-11-11 23:59:59")))
# merge date and time
motherload$ymfhms <- as_datetime(paste(motherload$fake_date, motherload$hms), format="%Y-%m-%d %H:%M:%S")

# create group before and after treatment
motherload <- motherload %>%
  mutate(bf_af = ifelse(ymfhms < as.POSIXct("2016-11-06 20:00:00"), 0, 1)) 

# axis limits
lims <- as.POSIXct(strptime(c("2016-11-01 00:00:00","2016-11-11 23:59:59:"), format = "%Y-%m-%d  %H"))    



####################################################################################################################################
### HYPOTHESIS 1 (EVENT STUDY)
####################################################################################################################################
# subset data-frame
event_df <- motherload %>%
  select(journalist_id, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT,
         treatment_dummy, time_to_treat, day_count, group)


# create time_to_treat and treatment_group variable for event study
event_df <- event_df %>% 
    mutate(treatment = case_when(time_to_treat == -5 ~ 0,
                               time_to_treat == -4 ~ 0,
                               time_to_treat == -3 ~ 0,
                               time_to_treat == -2 ~ 0,
                               time_to_treat == -1 ~ 0,
                               (time_to_treat == 0 & treatment_dummy == 1) ~ 1,
                               time_to_treat == 5 ~ 1,
                               time_to_treat == 4 ~ 1,
                               time_to_treat == 3 ~ 1,
                               time_to_treat == 2 ~ 1,
                               time_to_treat == 1 ~ 1,
                               treatment_dummy == 0 ~ 0)) %>%
  select(journalist_id, day_count, treatment, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, 
         IDENTITY_ATTACK, INSULT, THREAT, time_to_treat, group) %>%
  mutate(treatment_group = case_when(group == "control" ~ 0,
                                     group == "treatment" ~ 1))




event_df_scores <- colMeans(event_df[, c(4, 5, 6, 7, 8, 9)]) # mean attribute probability scores
event_treat_df <- event_df %>% # subset data for treated journalist in the post treatment period
  subset(treatment == 1)
event_treat_df_scores <- colMeans(event_treat_df[, c(4, 5, 6, 7, 8, 9)]) # mean attribute probability scores for treated journalist in the post treatment period

event_study_table_1 <- rbind(event_df_scores, event_treat_df_scores)
stargazer(event_study_table_1)

# TOXICITY 
# previous model had day 0 as reference feols(TOXICITY ~ i(time_to_treat, treatment_dummy, ref = 0) etc.
toxicity_twfe = feols(TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                 journalist_id + day_count,                           ## unit FE and time FE
                 cluster = ~journalist_id,                            ## cluster at journalist level 
                 data = event_df)


toxicity_twfe <- iplot(toxicity_twfe, only.params = TRUE)$prms



toxicity_twfe_fig <- ggplot(toxicity_twfe, aes(x=x, y=estimate)) + 
  geom_linerange(aes(xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(size = 3, shape = 16, fill = "white") +
  scale_y_continuous(limits = c(-0.015, 0.065),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(toxicity_twfe$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. TOXICITY") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
      text = element_text(family = "Roboto Mono", size = 12),
      legend.position = "bottom",
      axis.text.x = element_text(vjust = 1),
      legend.title = element_blank(),
      legend.key = element_rect(fill = NA, colour = "white", size = 0.25))


# SEVERE TOXICITY
servere_toxicity_twfe = feols(SEVERE_TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                              journalist_id + day_count,                           ## unit FE and time FE
                                              cluster = ~journalist_id,                            ## cluster at journalist level 
                                              data = event_df)

servere_toxicity_twfe <- iplot(servere_toxicity_twfe, only.params = TRUE)$prms


servere_toxicity_twfe_fig <- ggplot(servere_toxicity_twfe, aes(x=x, y=estimate)) + 
  geom_linerange(aes(xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(size = 3, shape = 16, fill = "white") +
  scale_y_continuous(limits = c(-0.015, 0.065),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(toxicity_twfe$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. SEVERE TOXICITY") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))


# SEXUALLY EXPLICIT 
sexually_explicit_twfe = feols(SEXUALLY_EXPLICIT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                                         journalist_id + day_count,                           ## unit FE and time FE
                                                       cluster = ~journalist_id,                            ## cluster at journalist level 
                                                       data = event_df)

sexually_explicit_twfe <- iplot(sexually_explicit_twfe, only.params = TRUE)$prms


sexually_explicit_twfe_fig <- ggplot(sexually_explicit_twfe, aes(x=x, y=estimate)) + 
  geom_linerange(aes(xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(size = 3, shape = 16, fill = "white") +
  scale_y_continuous(limits = c(-0.015, 0.065),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(toxicity_twfe$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. SEXUALLY EXPLICIT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))



# IDENTITY ATTACK
identity_attack_twfe = feols(IDENTITY_ATTACK ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                               journalist_id + day_count,                           ## unit FE and time FE
                             cluster = ~journalist_id,                            ## cluster at journalist level 
                             data = event_df)

identity_attack_twfe <- iplot(identity_attack_twfe, only.params = TRUE)$prms


identity_attack_twfe_fig <- ggplot(identity_attack_twfe, aes(x=x, y=estimate)) + 
  geom_linerange(aes(xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(size = 3, shape = 16, fill = "white") +
  scale_y_continuous(limits = c(-0.015, 0.065),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(toxicity_twfe$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. IDENTITY ATTACK") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))



# INSULT
insult_twfe = feols(INSULT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                      journalist_id + day_count,                           ## unit FE and time FE
                    cluster = ~journalist_id,                            ## cluster at journalist level 
                    data = event_df)

insult_twfe <- iplot(insult_twfe, only.params = TRUE)$prms


insult_twfe_fig <- ggplot(insult_twfe, aes(x=x, y=estimate)) + 
  geom_linerange(aes(xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(size = 3, shape = 16, fill = "white") +
  scale_y_continuous(limits = c(-0.015, 0.065),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(toxicity_twfe$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. INSULT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))



# THREAT
threat_twfe = feols(THREAT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                      journalist_id + day_count,                           ## unit FE and time FE
                    cluster = ~journalist_id,                            ## cluster at journalist level 
                    data = event_df)

threat_twfe <- iplot(threat_twfe, only.params = TRUE)$prms


threat_twfe_fig <- ggplot(threat_twfe, aes(x=x, y=estimate)) + 
  geom_linerange(aes(xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(size = 3, shape = 16, fill = "white") +
  scale_y_continuous(limits = c(-0.015, 0.065),
                    expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(toxicity_twfe$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. THREAT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))



twfe_fig_combined <- ggarrange(toxicity_twfe_fig, servere_toxicity_twfe_fig, sexually_explicit_twfe_fig, 
                                    identity_attack_twfe_fig, insult_twfe_fig, threat_twfe_fig,
                                    ncol = 2, nrow = 3,
                                    common.legend = TRUE, legend = "bottom", font.label = list(size = 20))

Event_Study_Fig <- annotate_figure(twfe_fig_combined, top = text_grob("Event study estimates with 95 CIs for all journalist and media", 
                                                    color = "black", face = "bold", family = "Roboto Mono", size = 14))

# save 
ggsave(path = "", 
       "twfe_fig_combined.pdf", width = 10, height = 8)
####################################################################################################################################
### HYPOTHESIS 1 (GENERALIZED DID)
####################################################################################################################################


# TOXICITY (all media)
all_model_1 <- felm(TOXICITY ~ treatment              # outcome variable regressed treatment variable
                    | factor(journalist_id) + factor(day_count) # unit FE and time FE
                    | 0                                         # no instrumental variables
                    | journalist_id,                            # cluster at journalist level 
                    data = event_df)

# SEVERE_TOXICITY (all media)
all_model_2 <- felm(SEVERE_TOXICITY ~ treatment
                    | factor(journalist_id) + factor(day_count)
                    | 0
                    | journalist_id,
                    data = event_df)

# SEXUALLY_EXPLICIT (all media)
all_model_3 <- felm(SEXUALLY_EXPLICIT ~ treatment
                    | factor(journalist_id) + factor(day_count)
                    | 0
                    | journalist_id,
                    data = event_df)

# IDENTITY_ATTACK (all media)
all_model_4 <- felm(IDENTITY_ATTACK ~ treatment
                    | factor(journalist_id) + factor(day_count)
                    | 0
                    | journalist_id,
                    data = event_df)

# INSULT (all media)
all_model_5 <- felm(INSULT ~ treatment
                    | factor(journalist_id) + factor(day_count)
                    | 0
                    | journalist_id,
                    data = event_df)

# THREAT (all media)
all_model_6 <- felm(THREAT ~ treatment
                    | factor(journalist_id) + factor(day_count)
                    | 0
                    | journalist_id,
                    data = event_df)


# Stargazer table: A full table showing regression results for all 6 categories of toxicity 
stargazer(all_model_1, all_model_2, all_model_3, all_model_4, all_model_5, all_model_6, type = "latex", 
          title = "Restults: the Tucker Carlson Effect",
          dep.var.labels = c("Toxicity", "Severe", "Sexually", "Identity", "Insult", "Threat"),
          covariate.labels = "Treatment",
          add.lines = list(c("Unit FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Time FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          keep.stat = c("rsq", "n"), star.cutoffs = c(0.05, 0.01, 0.001),
          column.sep.width = "-5pt") # Show just the r-square and number of obs.


####################################################################################################################################
### HYPOTHESIS 2: MEDIA (EVENT STUDY)
####################################################################################################################################

# subset data-frame
media_df <- motherload %>%
  select(journalist_id, gender, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT,
         treatment_dummy, time_to_treat, day_count, media, group) %>%
  mutate(bias = case_when(media == "CNN" ~ 1,
                          media == "The Washington Post" ~ 1,
                          media == "The Atlantic" ~ 1,
                          media == "The Verge" ~ 1,
                          media == "CBS" ~ 1,
                          media == "The Intercept" ~ 1,
                          media == "NYT" ~ 1,
                          media == "ABC" ~ 1,
                          media == "MSNBC" ~ 1,
                          media == "NBC" ~ 1,
                          media == "Outkick" ~ 2,
                          media == "CNBC" ~ 2,
                          media == "Fox News" ~ 3,
                          media == "Breitbart News" ~ 3,
                          media == "The Federalist" ~ 3,
                          media == "Boston Herald" ~ 3,
                          media == "Full Measure News" ~ 4,
                          media == "The Sun" ~ 4,
                          media == "Sirius XM" ~ 4,
                          media == "The Grayzone" ~ 4,
                          media == "Compact Magazine" ~ 4,
                          media == "GB News" ~ 4,
                          media == "The Daily Beast" ~ 4,
                          media == "Comedy Central" ~ 4,
                          media == "TMZ" ~ 4,
                          media == "Freelance" ~ 4))

media_df <- media_df %>% 
  mutate(treatment = case_when(time_to_treat == -5 ~ 0,
                               time_to_treat == -4 ~ 0,
                               time_to_treat == -3 ~ 0,
                               time_to_treat == -2 ~ 0,
                               time_to_treat == -1 ~ 0,
                               (time_to_treat == 0 & treatment_dummy == 1) ~ 1,
                               time_to_treat == 5 ~ 1,
                               time_to_treat == 4 ~ 1,
                               time_to_treat == 3 ~ 1,
                               time_to_treat == 2 ~ 1,
                               time_to_treat == 1 ~ 1,
                               treatment_dummy == 0 ~ 0)) %>%
  select(journalist_id, day_count, treatment, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, 
         IDENTITY_ATTACK, INSULT, THREAT, time_to_treat, group, bias) %>%
  mutate(treatment_group = case_when(group == "control" ~ 0,
                                     group == "treatment" ~ 1))


not_rated_df <- media_df[!(media_df$bias == 1 | media_df$bias == 2 | media_df$bias == 3),] # data.frame with journalist from not-rated media
media_df <- media_df[!(media_df$bias == 2 | media_df$bias == 4),] # data.frame with journalist from right-wing and left-wing media


# keep right wing journalists as control but remove them from the treatment group
left_df <- media_df %>% 
  filter(!(bias == 3 & treatment_group == 1)) 

# keep left wing journalists as contorl but remove them from the treatment group
right_df <- media_df %>% 
  filter(!(bias == 1 & treatment_group == 1))


left_df_score <- left_df  %>% 
  filter(treatment == 1)

right_df_score <- right_df  %>% 
  filter(treatment == 1)

media_df_score <- colMeans(media_df[, c(4, 5, 6, 7, 8, 9)]) # mean attribute probability scores for treated journalist in the post treatment period
left_df_score <- colMeans(left_df_score[, c(4, 5, 6, 7, 8, 9)]) # mean attribute probability scores for treated journalist in the post treatment period
right_df_score <- colMeans(right_df_score[, c(4, 5, 6, 7, 8, 9)]) # mean attribute probability scores for treated journalist in the post treatment period

event_study_table_2 <- rbind(media_df_score, left_df_score, right_df_score)
stargazer(event_study_table_2)


# TOXICITY
left_toxicity_twfe = feols(TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                             journalist_id + day_count,                           ## unit FE and time FE
                           cluster = ~journalist_id,                            ## cluster at journalist level 
                           data = left_df)

right_toxicity_twfe = feols(TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                              journalist_id + day_count,                           ## unit FE and time FE
                            cluster = ~journalist_id,                            ## cluster at journalist level 
                            data = right_df)


left_toxicity_twfe <- iplot(left_toxicity_twfe, only.params = TRUE)$prms
left_toxicity_twfe$bias <- "Left wing"
right_toxicity_twfe <- iplot(right_toxicity_twfe, only.params = TRUE)$prms
right_toxicity_twfe$bias <- "Right wing"
media_toxicity <- rbind(left_toxicity_twfe, right_toxicity_twfe)

media_toxicity_twfe_fig <- ggplot(media_toxicity) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = bias, group = bias), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = bias, group = bias), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. TOXICITY") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("black", "#8D9797"))




# SEVERE TOXICITY
left_severe_toxicity_twfe = feols(SEVERE_TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                    journalist_id + day_count,                           ## unit FE and time FE
                                  cluster = ~journalist_id,                            ## cluster at journalist level 
                                  data = left_df)

right_severe_toxicity_twfe = feols(SEVERE_TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                     journalist_id + day_count,                           ## unit FE and time FE
                                   cluster = ~journalist_id,                            ## cluster at journalist level 
                                   data = right_df)


left_severe_toxicity_twfe <- iplot(left_severe_toxicity_twfe, only.params = TRUE)$prms
left_severe_toxicity_twfe$bias <- "Left wing media"
right_severe_toxicity_twfe <- iplot(right_severe_toxicity_twfe, only.params = TRUE)$prms
right_severe_toxicity_twfe$bias <- "Right wing media"
media_severe_toxicity <- rbind(left_severe_toxicity_twfe, right_severe_toxicity_twfe)

media_severe_toxicity_twfe_fig <- ggplot(media_severe_toxicity) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = bias, group = bias), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = bias, group = bias), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_severe_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. SEVERE TOXICITY") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("black", "#8D9797"))


# SEXUALLY EXPLICIT 
left_sexually_explicit_twfe = feols(SEXUALLY_EXPLICIT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                      journalist_id + day_count,                           ## unit FE and time FE
                                    cluster = ~journalist_id,                            ## cluster at journalist level 
                                    data = left_df)

right_sexually_explicit_twfe = feols(SEXUALLY_EXPLICIT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                       journalist_id + day_count,                           ## unit FE and time FE
                                     cluster = ~journalist_id,                            ## cluster at journalist level 
                                     data = right_df)


left_sexually_explicit_twfe <- iplot(left_sexually_explicit_twfe, only.params = TRUE)$prms
left_sexually_explicit_twfe$bias <- "Left wing media"
right_sexually_explicit_twfe <- iplot(right_sexually_explicit_twfe, only.params = TRUE)$prms
right_sexually_explicit_twfe$bias <- "Right wing media"
media_sexually_explicit <- rbind(left_sexually_explicit_twfe, right_sexually_explicit_twfe)

media_sexually_explicit_twfe_fig <- ggplot(media_sexually_explicit) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = bias, group = bias), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = bias, group = bias), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_sexually_explicit$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. SEXUALLY EXPLICIT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("black", "#8D9797"))



# IDENTITY ATTACK
left_identity_attack_twfe = feols(IDENTITY_ATTACK ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                    journalist_id + day_count,                           ## unit FE and time FE
                                  cluster = ~journalist_id,                            ## cluster at journalist level 
                                  data = left_df)

right_identity_attack_twfe = feols(IDENTITY_ATTACK ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                     journalist_id + day_count,                           ## unit FE and time FE
                                   cluster = ~journalist_id,                            ## cluster at journalist level 
                                   data = right_df)


left_identity_attack_twfe <- iplot(left_identity_attack_twfe, only.params = TRUE)$prms
left_identity_attack_twfe$bias <- "Left wing media"
right_identity_attack_twfe <- iplot(right_identity_attack_twfe, only.params = TRUE)$prms
right_identity_attack_twfe$bias <- "Right wing media"
media_identity_attack <- rbind(left_identity_attack_twfe, right_identity_attack_twfe)

media_identity_attack_twfe_fig <- ggplot(media_identity_attack) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = bias, group = bias), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = bias, group = bias), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_identity_attack$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. IDENTITY ATTACK") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("black", "#8D9797"))


# INSULT
left_insult_twfe = feols(INSULT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                           journalist_id + day_count,                           ## unit FE and time FE
                         cluster = ~journalist_id,                            ## cluster at journalist level 
                         data = left_df)

right_insult_twfe = feols(INSULT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                            journalist_id + day_count,                           ## unit FE and time FE
                          cluster = ~journalist_id,                            ## cluster at journalist level 
                          data = right_df)


left_insult_twfe <- iplot(left_insult_twfe, only.params = TRUE)$prms
left_insult_twfe$bias <- "Left wing media"
right_insult_twfe <- iplot(right_insult_twfe, only.params = TRUE)$prms
right_insult_twfe$bias <- "Right wing media"
media_insult <- rbind(left_insult_twfe, right_insult_twfe)

media_insult_twfe_fig <- ggplot(media_insult) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = bias, group = bias), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = bias, group = bias), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_insult$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. INSULT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("black", "#8D9797"))


# THREAT
left_threat_twfe = feols(THREAT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                           journalist_id + day_count,                           ## unit FE and time FE
                         cluster = ~journalist_id,                            ## cluster at journalist level 
                         data = left_df)

right_threat_twfe = feols(THREAT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                            journalist_id + day_count,                           ## unit FE and time FE
                          cluster = ~journalist_id,                            ## cluster at journalist level 
                          data = right_df)


left_threat_twfe <- iplot(left_threat_twfe, only.params = TRUE)$prms
left_threat_twfe$bias <- "Left wing media"
right_threat_twfe <- iplot(right_threat_twfe, only.params = TRUE)$prms
right_threat_twfe$bias <- "Right wing media"
media_threat <- rbind(left_threat_twfe, right_threat_twfe)

media_threat_twfe_fig <- ggplot(media_threat) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = bias, group = bias), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = bias, group = bias), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_threat$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. THREAT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("black", "#8D9797"))



media_fig_combined_left <- ggarrange(media_toxicity_twfe_fig, media_severe_toxicity_twfe_fig, media_sexually_explicit_twfe_fig, 
                                     media_identity_attack_twfe_fig, media_insult_twfe_fig, media_threat_twfe_fig,
                                     ncol = 2, nrow = 3,
                                     common.legend = TRUE, legend = "bottom", font.label = list(size = 20))

# save 
ggsave(path = "", 
       "media_fig_combined_left.pdf", width = 10, height = 8)


# TOXICITY
left_toxicity_twfe = feols(TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                             journalist_id + day_count,                           ## unit FE and time FE
                           cluster = ~journalist_id,                            ## cluster at journalist level 
                           data = left_df)

right_toxicity_twfe = feols(TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                              journalist_id + day_count,                           ## unit FE and time FE
                            cluster = ~journalist_id,                            ## cluster at journalist level 
                            data = right_df)


left_toxicity_twfe <- iplot(left_toxicity_twfe, only.params = TRUE)$prms
left_toxicity_twfe$bias <- "Left wing"
right_toxicity_twfe <- iplot(right_toxicity_twfe, only.params = TRUE)$prms
right_toxicity_twfe$bias <- "Right wing"
media_toxicity <- rbind(left_toxicity_twfe, right_toxicity_twfe)

media_toxicity_twfe_fig <- ggplot(media_toxicity) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = bias, group = bias), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = bias, group = bias), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. TOXICITY") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("black", "#8D9797"))


###########################################################################
############ Event study for not rated media journalists ##################
###########################################################################

# TOXICITY
nr_toxicity_twfe = feols(TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                  journalist_id + day_count,                           ## unit FE and time FE
                                cluster = ~journalist_id,                            ## cluster at journalist level 
                                data = not_rated_df)


nr_toxicity_twfe <- iplot(nr_toxicity_twfe, only.params = TRUE)$prms


nr_toxicity_twfe_fig <- ggplot(nr_toxicity_twfe) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(aes(x=x, y=estimate), size = 3, shape = 16) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_severe_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. TOXICITY") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))


# SEVERE TOXICITY
nr_severe_toxicity_twfe = feols(SEVERE_TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                    journalist_id + day_count,                           ## unit FE and time FE
                                  cluster = ~journalist_id,                            ## cluster at journalist level 
                                  data = not_rated_df)


nr_severe_toxicity_twfe <- iplot(nr_severe_toxicity_twfe, only.params = TRUE)$prms


nr_severe_toxicity_twfe_fig <- ggplot(nr_severe_toxicity_twfe) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(aes(x=x, y=estimate), size = 3, shape = 16) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_severe_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. SEVERE TOXICITY") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))

# SEXUALLY EXPLICIT 
nr_sexually_explicit_twfe = feols(SEXUALLY_EXPLICIT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                  journalist_id + day_count,                           ## unit FE and time FE
                                cluster = ~journalist_id,                            ## cluster at journalist level 
                                data = not_rated_df)


nr_sexually_explicit_twfe <- iplot(nr_sexually_explicit_twfe, only.params = TRUE)$prms


nr_sexually_explicit_twfe_fig <- ggplot(nr_sexually_explicit_twfe) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(aes(x=x, y=estimate), size = 3, shape = 16) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_severe_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. SEXUALLY EXPLICIT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))



# IDENTITY ATTACK
nr_identity_attack_twfe = feols(IDENTITY_ATTACK ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                    journalist_id + day_count,                           ## unit FE and time FE
                                  cluster = ~journalist_id,                            ## cluster at journalist level 
                                  data = not_rated_df)


nr_identity_attack_twfe <- iplot(nr_identity_attack_twfe, only.params = TRUE)$prms


nr_identity_attack_twfe_fig <- ggplot(nr_identity_attack_twfe) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(aes(x=x, y=estimate), size = 3, shape = 16) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_severe_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. IDENTITY ATTACK") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))


# INSULT
nr_insult_twfe = feols(INSULT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                  journalist_id + day_count,                           ## unit FE and time FE
                                cluster = ~journalist_id,                            ## cluster at journalist level 
                                data = not_rated_df)


nr_insult_twfe <- iplot(nr_insult_twfe, only.params = TRUE)$prms


nr_insult_twfe_fig <- ggplot(nr_insult_twfe) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(aes(x=x, y=estimate), size = 3, shape = 16) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_severe_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. INSULT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))


# THREAT
nr_threat_twfe = feols(THREAT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                         journalist_id + day_count,                           ## unit FE and time FE
                       cluster = ~journalist_id,                            ## cluster at journalist level 
                       data = not_rated_df)


nr_threat_twfe <- iplot(nr_threat_twfe, only.params = TRUE)$prms


nr_threat_twfe_fig <- ggplot(nr_threat_twfe) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high)) +
  geom_point(aes(x=x, y=estimate), size = 3, shape = 16) +
  scale_y_continuous(limits = c(-0.045, 0.115),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(media_severe_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. THREAT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25))



nr_fig_combined_left <- ggarrange(nr_toxicity_twfe_fig, nr_severe_toxicity_twfe_fig, nr_sexually_explicit_twfe_fig, 
                                     nr_identity_attack_twfe_fig, nr_insult_twfe_fig, nr_threat_twfe_fig,
                                     ncol = 2, nrow = 3,
                                     common.legend = TRUE, legend = "bottom", font.label = list(size = 20))

# save 
ggsave(path = "", 
       "nr_fig_combined_left.pdf", width = 10, height = 8)


####################################################################################################################################
### HYPOTHESIS 2: MEDIA (GENERALIZED DID)
####################################################################################################################################

# TOXICITY (left/centrist media)
left_model_1 <- felm(TOXICITY ~ treatment                  # outcome variable regressed treatment variable
                     | factor(journalist_id) + factor(day_count) # unit FE and time FE
                     | 0                                         # no instrumental variables
                     | journalist_id,                            # cluster at journalist level 
                     data = left_df)

# SEVERE_TOXICITY (left/centrist media)
left_model_2 <- felm(SEVERE_TOXICITY ~ treatment
                     | factor(journalist_id) + factor(day_count)
                     | 0
                     | journalist_id,
                     data = left_df)

# SEXUALLY_EXPLICIT (left/centrist media)
left_model_3 <- felm(SEXUALLY_EXPLICIT ~ treatment
                     | factor(journalist_id) + factor(day_count)
                     | 0
                     | journalist_id,
                     data = left_df)

# IDENTITY_ATTACK (left/centrist media)
left_model_4 <- felm(IDENTITY_ATTACK ~ treatment
                     | factor(journalist_id) + factor(day_count)
                     | 0
                     | journalist_id,
                     data = left_df)

# INSULT (left/centrist media)
left_model_5 <- felm(INSULT ~ treatment
                     | factor(journalist_id) + factor(day_count)
                     | 0
                     | journalist_id,
                     data = left_df)

# THREAT (left/centrist media)
left_model_6 <- felm(THREAT ~ treatment
                     | factor(journalist_id) + factor(day_count)
                     | 0
                     | journalist_id,
                     data = left_df)


# Stargazer table: A full table showing regression results for all 6 categories of toxicity 
stargazer(left_model_1, left_model_2, left_model_3, left_model_4, left_model_5, left_model_6, type = "latex", 
          title = "Hypothesis 3 Left Wing Media: the Tucker Carlson Effect on Left Wing Media",
          dep.var.labels = c("Toxicity", "Severe", "Sexually", "Identity", "Insult", "Threat"),
          covariate.labels = "Treatment",
          add.lines = list(c("Unit FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Time FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          keep.stat = c("rsq", "n"), star.cutoffs = c(0.05, 0.01, 0.001),
          column.sep.width = "-5pt") # Show just the r-square and number of obs.



# TOXICITY (left/centrist media)
right_model_1 <- felm(TOXICITY ~ treatment                  # outcome variable regressed treatment variable
                     | factor(journalist_id) + factor(day_count) # unit FE and time FE
                     | 0                                         # no instrumental variables
                     | journalist_id,                            # cluster at journalist level 
                     data = right_df)

# SEVERE_TOXICITY (left/centrist media)
right_model_2 <- felm(SEVERE_TOXICITY ~ treatment
                     | factor(journalist_id) + factor(day_count)
                     | 0
                     | journalist_id,
                     data = right_df)

# SEXUALLY_EXPLICIT (left/centrist media)
right_model_3 <- felm(SEXUALLY_EXPLICIT ~ treatment
                     | factor(journalist_id) + factor(day_count)
                     | 0
                     | journalist_id,
                     data = right_df)

# IDENTITY_ATTACK (left/centrist media)
right_model_4 <- felm(IDENTITY_ATTACK ~ treatment
                     | factor(journalist_id) + factor(day_count)
                     | 0
                     | journalist_id,
                     data = right_df)

# INSULT (left/centrist media)
right_model_5 <- felm(INSULT ~ treatment
                     | factor(journalist_id) + factor(day_count)
                     | 0
                     | journalist_id,
                     data = right_df)

# THREAT (left/centrist media)
right_model_6 <- felm(THREAT ~ treatment
                     | factor(journalist_id) + factor(day_count)
                     | 0
                     | journalist_id,
                     data = right_df)


# Stargazer table: A full table showing regression results for all 6 categories of toxicity 
stargazer(right_model_1, right_model_2, right_model_3, right_model_4, right_model_5, right_model_6, type = "latex", 
          title = "Hypothesis 3 Right Wing Media: the Tucker Carlson Effect on Right Wing Media",
          dep.var.labels = c("Toxicity", "Severe", "Sexually", "Identity", "Insult", "Threat"),
          covariate.labels = "Treatment",
          add.lines = list(c("Unit FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Time FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          keep.stat = c("rsq", "n"), star.cutoffs = c(0.05, 0.01, 0.001),
          column.sep.width = "-5pt") # Show just the r-square and number of obs.

####################################################################################################################################
### HYPOTHESIS 3: GENDER (EVENT STUDY)
####################################################################################################################################

# subset data-frame
gender_df <- motherload %>%
  select(journalist_id, gender, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT,
         treatment_dummy, time_to_treat, day_count, media, group, gender)

#
gender_df <- gender_df %>% 
  mutate(treatment = case_when(time_to_treat == -5 ~ 0,
                               time_to_treat == -4 ~ 0,
                               time_to_treat == -3 ~ 0,
                               time_to_treat == -2 ~ 0,
                               time_to_treat == -1 ~ 0,
                               (time_to_treat == 0 & treatment_dummy == 1) ~ 1,
                               time_to_treat == 5 ~ 1,
                               time_to_treat == 4 ~ 1,
                               time_to_treat == 3 ~ 1,
                               time_to_treat == 2 ~ 1,
                               time_to_treat == 1 ~ 1,
                               treatment_dummy == 0 ~ 0)) %>%
  select(journalist_id, day_count, treatment, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, 
         IDENTITY_ATTACK, INSULT, THREAT, time_to_treat, group, gender) %>%
  mutate(treatment_group = case_when(group == "control" ~ 0,
                                     group == "treatment" ~ 1))


female_df <- gender_df %>% 
  filter(treatment_group == 0 | gender == "W")
male_df <- gender_df %>% 
  filter(treatment_group == 0 | gender == "M")

female_df_score <- female_df  %>% 
  filter(treatment == 1)

male_df_score <- male_df  %>% 
  filter(treatment == 1)


female_df_score <- colMeans(female_df_score[, c(4, 5, 6, 7, 8, 9)]) # mean attribute probability scores for treated journalist in the post treatment period
male_df_score <- colMeans(male_df_score[, c(4, 5, 6, 7, 8, 9)]) # mean attribute probability scores for treated journalist in the post treatment period

event_study_table_3 <- rbind(event_df_scores, male_df_score, female_df_score)
stargazer(event_study_table_3)



# TOXICITY
female_toxicity_twfe = feols(TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                             journalist_id + day_count,                           ## unit FE and time FE
                           cluster = ~journalist_id,                            ## cluster at journalist level 
                           data = female_df)

male_toxicity_twfe = feols(TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                           journalist_id + day_count,                           ## unit FE and time FE
                         cluster = ~journalist_id,                            ## cluster at journalist level 
                         data = male_df)


female_toxicity_twfe <- iplot(female_toxicity_twfe, only.params = TRUE)$prms
female_toxicity_twfe$gender <- "Women"
male_toxicity_twfe <- iplot(male_toxicity_twfe, only.params = TRUE)$prms
male_toxicity_twfe$gender <- "Men"
gender_toxicity <- rbind(female_toxicity_twfe, male_toxicity_twfe)

gender_toxicity_twfe_fig <- ggplot(gender_toxicity) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = gender, group = gender), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = gender, group = gender), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.018, 0.108),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(gender_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. TOXICITY") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("#66aaf4", "#f46966"))



# SEVERE TOXICITY
female_severe_toxicity_twfe = feols(SEVERE_TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                      journalist_id + day_count,                           ## unit FE and time FE
                    cluster = ~journalist_id,                            ## cluster at journalist level 
                    data = female_df)

male_severe_toxicity_twfe = feols(SEVERE_TOXICITY ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                    journalist_id + day_count,                           ## unit FE and time FE
                  cluster = ~journalist_id,                            ## cluster at journalist level 
                  data = male_df)


female_severe_toxicity_twfe <- iplot(female_severe_toxicity_twfe, only.params = TRUE)$prms
female_severe_toxicity_twfe$gender <- "Women"
male_severe_toxicity_twfe <- iplot(male_severe_toxicity_twfe, only.params = TRUE)$prms
male_severe_toxicity_twfe$gender <- "Men"
gender_severe_toxicity <- rbind(female_severe_toxicity_twfe, male_severe_toxicity_twfe)

gender_severe_toxicity_twfe_fig <- ggplot(gender_severe_toxicity) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = gender, group = gender), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = gender, group = gender), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.018, 0.108),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(gender_severe_toxicity$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. SEVERE TOXICITY") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("#66aaf4", "#f46966"))


# SEXUALLY EXPLICIT 
female_sexually_explicit_twfe = feols(SEXUALLY_EXPLICIT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                      journalist_id + day_count,                           ## unit FE and time FE
                                    cluster = ~journalist_id,                            ## cluster at journalist level 
                                    data = female_df)

male_sexually_explicit_twfe = feols(SEXUALLY_EXPLICIT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                    journalist_id + day_count,                           ## unit FE and time FE
                                  cluster = ~journalist_id,                            ## cluster at journalist level 
                                  data = male_df)


female_sexually_explicit_twfe <- iplot(female_sexually_explicit_twfe, only.params = TRUE)$prms
female_sexually_explicit_twfe$gender <- "Women"
male_sexually_explicit_twfe <- iplot(male_sexually_explicit_twfe, only.params = TRUE)$prms
male_sexually_explicit_twfe$gender <- "Men"
gender_sexually_explicit <- rbind(female_sexually_explicit_twfe, male_sexually_explicit_twfe)

gender_sexually_explicit_twfe_fig <- ggplot(gender_sexually_explicit) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = gender, group = gender), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = gender, group = gender), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.018, 0.108),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(gender_sexually_explicit$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. SEXUALLY EXPLICIT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("#66aaf4", "#f46966"))



# IDENTITY ATTACK
female_identity_attack_twfe = feols(IDENTITY_ATTACK ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                        journalist_id + day_count,                           ## unit FE and time FE
                                      cluster = ~journalist_id,                            ## cluster at journalist level 
                                      data = female_df)

male_identity_attack_twfe = feols(IDENTITY_ATTACK ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                      journalist_id + day_count,                           ## unit FE and time FE
                                    cluster = ~journalist_id,                            ## cluster at journalist level 
                                    data = male_df)


female_identity_attack_twfe <- iplot(female_identity_attack_twfe, only.params = TRUE)$prms
female_identity_attack_twfe$gender <- "Women"
male_identity_attack_twfe <- iplot(male_identity_attack_twfe, only.params = TRUE)$prms
male_identity_attack_twfe$gender <- "Men"
gender_identity_attack <- rbind(female_identity_attack_twfe, male_identity_attack_twfe)

gender_identity_attack_twfe_fig <- ggplot(gender_identity_attack) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = gender, group = gender), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = gender, group = gender), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.018, 0.108),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(gender_identity_attack$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. IDENTITY ATTACK") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("#66aaf4", "#f46966"))



# INSULT
female_insult_twfe = feols(INSULT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                      journalist_id + day_count,                           ## unit FE and time FE
                                    cluster = ~journalist_id,                            ## cluster at journalist level 
                                    data = female_df)

male_insult_twfe = feols(INSULT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                                    journalist_id + day_count,                           ## unit FE and time FE
                                  cluster = ~journalist_id,                            ## cluster at journalist level 
                                  data = male_df)


female_insult_twfe <- iplot(female_insult_twfe, only.params = TRUE)$prms
female_insult_twfe$gender <- "Women"
male_insult_twfe <- iplot(male_insult_twfe, only.params = TRUE)$prms
male_insult_twfe$gender <- "Men"
gender_insult <- rbind(female_insult_twfe, male_insult_twfe)

gender_insult_twfe_fig <- ggplot(gender_insult) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = gender, group = gender), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = gender, group = gender), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.018, 0.108),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(gender_insult$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. INSULT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("#66aaf4", "#f46966"))



# THREAT
female_threat_twfe = feols(THREAT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                             journalist_id + day_count,                           ## unit FE and time FE
                           cluster = ~journalist_id,                            ## cluster at journalist level 
                           data = female_df)

male_threat_twfe = feols(THREAT ~ i(time_to_treat, treatment_group, ref = -1) |  ## key interaction: time × treatment status 
                           journalist_id + day_count,                           ## unit FE and time FE
                         cluster = ~journalist_id,                            ## cluster at journalist level 
                         data = male_df)


female_threat_twfe <- iplot(female_threat_twfe, only.params = TRUE)$prms
female_threat_twfe$gender <- "Women"
male_threat_twfe <- iplot(male_threat_twfe, only.params = TRUE)$prms
male_threat_twfe$gender <- "Men"
gender_threat <- rbind(female_threat_twfe, male_threat_twfe)

gender_threat_twfe_fig <- ggplot(gender_threat) + 
  geom_linerange(aes(x=x, y=estimate,
                     xmin = x, xmax = x,
                     ymin = ci_low, ymax = ci_high, color = gender, group = gender), position = position_dodge(0.5)) +
  geom_point(aes(x=x, y=estimate, color = gender, group = gender), size = 3, shape = 16, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.018, 0.108),
                     expand = c(0, 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = 1,
             linetype = "Tucker Carlson Tonight") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(gender_threat$x)) +
  geom_vline(xintercept = -0.5, linetype = 5) +
  ylab("Coef. THREAT") + xlab("Days") +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 12),
        legend.position = "bottom",
        axis.text.x = element_text(vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  scale_color_manual(values = c("#66aaf4", "#f46966"))




gender_twfe_fig_combined <- ggarrange(gender_toxicity_twfe_fig, gender_severe_toxicity_twfe_fig, gender_sexually_explicit_twfe_fig, 
                                    gender_identity_attack_twfe_fig, gender_insult_twfe_fig, gender_threat_twfe_fig,
                                    ncol = 2, nrow = 3,
                                    common.legend = TRUE, legend = "bottom", font.label = list(size = 20))

# save 
ggsave(path = "", 
       "gender_twfe_fig_combined.pdf", width = 10, height = 8)

####################################################################################################################################
### HYPOTHESIS 3: GENDER (GENERALIZED DID)
####################################################################################################################################

# TOXICITY
w_model_1 <- felm(TOXICITY ~ treatment                   # outcome variable regressed treatment variable
                  | factor(journalist_id) + factor(day_count) # unit FE and time FE
                  | 0                                         # no instrumental variables
                  | journalist_id,                            # cluster at journalist level 
                  data = female_df)

# SEVERE_TOXICITY
w_model_2 <- felm(SEVERE_TOXICITY ~ treatment
                  | factor(journalist_id) + factor(day_count)
                  | 0
                  | journalist_id,
                  data = female_df)

# SEXUALLY_EXPLICIT
w_model_3 <- felm(SEXUALLY_EXPLICIT ~ treatment
                  | factor(journalist_id) + factor(day_count)
                  | 0
                  | journalist_id,
                  data = female_df)

# IDENTITY_ATTACK
w_model_4 <- felm(IDENTITY_ATTACK ~ treatment
                  | factor(journalist_id) + factor(day_count)
                  | 0
                  | journalist_id,
                  data = female_df)

# INSULT
w_model_5 <- felm(INSULT ~ treatment
                  | factor(journalist_id) + factor(day_count)
                  | 0
                  | journalist_id,
                  data = female_df)

# THREAT
w_model_6 <- felm(THREAT ~ treatment
                  | factor(journalist_id) + factor(day_count)
                  | 0
                  | journalist_id,
                  data = female_df)


# Stargazer table: A full table showing regression results for all 6 categories of toxicity 
stargazer(w_model_1, w_model_2, w_model_3, w_model_4, w_model_5, w_model_6, type = "latex", 
          title = "Hyppthesis 4: The Tucker Carlson Effect on Women Journalists",
          dep.var.labels = c("Toxicity", "Severe", "Sexually", "Identity", "Insult", "Threat"),
          covariate.labels = "Treatment",
          add.lines = list(c("Unit FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Time FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          keep.stat = c("rsq", "n"),
          column.sep.width = "-5pt") # Show just the r-square and number of obs.



# TOXICITY
m_model_1 <- felm(TOXICITY ~ treatment               # outcome variable regressed treatment variable
                  | factor(journalist_id) + factor(day_count) # unit FE and time FE
                  | 0                                         # no instrumental variables
                  | journalist_id,                            # cluster at journalist level 
                  data = male_df)

# SEVERE_TOXICITY
m_model_2 <- felm(SEVERE_TOXICITY ~ treatment
                  | factor(journalist_id) + factor(day_count)
                  | 0
                  | journalist_id,
                  data = male_df)

# SEXUALLY_EXPLICIT
m_model_3 <- felm(SEXUALLY_EXPLICIT ~ treatment
                  | factor(journalist_id) + factor(day_count)
                  | 0
                  | journalist_id,
                  data = male_df)

# IDENTITY_ATTACK
m_model_4 <- felm(IDENTITY_ATTACK ~ treatment
                  | factor(journalist_id) + factor(day_count)
                  | 0
                  | journalist_id,
                  data = male_df)

# INSULT
m_model_5 <- felm(INSULT ~ treatment
                  | factor(journalist_id) + factor(day_count)
                  | 0
                  | journalist_id,
                  data = male_df)

# THREAT
m_model_6 <- felm(THREAT ~ treatment
                  | factor(journalist_id) + factor(day_count)
                  | 0
                  | journalist_id,
                  data = male_df)


# Stargazer table: A full table showing regression results for all 6 categories of toxicity 
stargazer(m_model_1, m_model_2, m_model_3, m_model_4, m_model_5, m_model_6, type = "latex",
          title = "Hypothesis 4: The Tucker Carlson Effect on Male Journalists",
          dep.var.labels = c("Toxicity", "Severe", "Sexually", "Identity", "Insult", "Threat"),
          covariate.labels = "Male ",
          add.lines = list(c("Unit FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Time FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          keep.stat = c("rsq", "n"), star.cutoffs = c(0.05, 0.01, 0.001),
          column.sep.width = "-5pt") # Show just the r-square and number of obs.



####################################################################################################################################
### T H E   H O U R S   A R O U N D   T U C K E R   C A R L S O N   T O N I G H T ###
####################################################################################################################################
tct_time <- motherload %>% 
  subset(treatment_dummy == 1) %>%
  select(journalist_id, names, gender, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT,
         created_at, showtime)

tct_time$created_at <-  as_datetime(tct_time$created_at)
tct_time$showtime_before <- tct_time$showtime - hours(3)
tct_time$showtime_after <- tct_time$showtime + hours(3)

tct_time_before <- tct_time %>% rowwise() %>%
  mutate(match = ifelse(between(created_at, showtime_before, showtime), 1, 0)) %>%
  select(-c(showtime_before, showtime)) %>%
  arrange(created_at, desc(match))

tct_time_after <- tct_time %>% rowwise() %>%
  mutate(match = ifelse(between(created_at, showtime, showtime_after), 2, 0)) %>%
  select(-c(showtime, showtime_after)) %>%
  arrange(created_at, desc(match))

tct_time_before <- tct_time_before %>% 
  select(TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT,
         created_at, match)

tct_time_after <- tct_time_after %>% 
  select(TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT,
         created_at, match)

tct <- rbind(tct_time_before, tct_time_after)
tct <- tct %>% 
  subset(match != 0)
count(tct$match)
tct <- tct %>%
  mutate(cat_toxicity = case_when((TOXICITY > 0  & TOXICITY < 0.1) ~ "0 - 0.1",
                                  (TOXICITY > 0.1  & TOXICITY < 0.2) ~ "0.1 - 0.2",
                                  (TOXICITY > 0.2  & TOXICITY < 0.3) ~ "0.2 - 0.3",
                                  (TOXICITY > 0.3  & TOXICITY < 0.4) ~ "0.3 - 0.4",
                                  (TOXICITY > 0.4  & TOXICITY < 0.5) ~ "0.4 - 0.5",
                                  (TOXICITY > 0.5  & TOXICITY < 0.6) ~ "0.5 - 0.6",
                                  (TOXICITY > 0.6  & TOXICITY < 0.7) ~ "0.6 - 0.7",
                                  (TOXICITY > 0.7  & TOXICITY < 0.8) ~ "0.7 - 0.8",
                                  (TOXICITY > 0.8  & TOXICITY < 0.9) ~ "0.8 - 0.9",
                                  (TOXICITY > 0.9  & TOXICITY < 1) ~ "0.9 - 1")) %>%
  mutate(cat_severe = case_when((SEVERE_TOXICITY > 0  & SEVERE_TOXICITY < 0.1) ~ "0 - 0.1",
                                (SEVERE_TOXICITY > 0.1  & SEVERE_TOXICITY < 0.2) ~ "0.1 - 0.2",
                                (SEVERE_TOXICITY > 0.2  & SEVERE_TOXICITY < 0.3) ~ "0.2 - 0.3",
                                (SEVERE_TOXICITY > 0.3  & SEVERE_TOXICITY < 0.4) ~ "0.3 - 0.4",
                                (SEVERE_TOXICITY > 0.4  & SEVERE_TOXICITY < 0.5) ~ "0.4 - 0.5",
                                (SEVERE_TOXICITY > 0.5  & SEVERE_TOXICITY < 0.6) ~ "0.5 - 0.6",
                                (SEVERE_TOXICITY > 0.6  & SEVERE_TOXICITY < 0.7) ~ "0.6 - 0.7",
                                (SEVERE_TOXICITY > 0.7  & SEVERE_TOXICITY < 0.8) ~ "0.7 - 0.8",
                                (SEVERE_TOXICITY > 0.8  & SEVERE_TOXICITY < 0.9) ~ "0.8 - 0.9",
                                (SEVERE_TOXICITY > 0.9  & SEVERE_TOXICITY < 1) ~ "0.9 - 1")) %>%
  mutate(cat_sex = case_when((SEXUALLY_EXPLICIT > 0  & SEXUALLY_EXPLICIT < 0.1) ~ "0 - 0.1",
                             (SEXUALLY_EXPLICIT > 0.1  & SEXUALLY_EXPLICIT < 0.2) ~ "0.1 - 0.2",
                             (SEXUALLY_EXPLICIT > 0.2  & SEXUALLY_EXPLICIT < 0.3) ~ "0.2 - 0.3",
                             (SEXUALLY_EXPLICIT > 0.3  & SEXUALLY_EXPLICIT < 0.4) ~ "0.3 - 0.4",
                             (SEXUALLY_EXPLICIT > 0.4  & SEXUALLY_EXPLICIT < 0.5) ~ "0.4 - 0.5",
                             (SEXUALLY_EXPLICIT > 0.5  & SEXUALLY_EXPLICIT < 0.6) ~ "0.5 - 0.6",
                             (SEXUALLY_EXPLICIT > 0.6  & SEXUALLY_EXPLICIT < 0.7) ~ "0.6 - 0.7",
                             (SEXUALLY_EXPLICIT > 0.7  & SEXUALLY_EXPLICIT < 0.8) ~ "0.7 - 0.8",
                             (SEXUALLY_EXPLICIT > 0.8  & SEXUALLY_EXPLICIT < 0.9) ~ "0.8 - 0.9",
                             (SEXUALLY_EXPLICIT > 0.9  & SEXUALLY_EXPLICIT < 1) ~ "0.9 - 1")) %>%
  mutate(cat_identity = case_when((IDENTITY_ATTACK > 0  & IDENTITY_ATTACK < 0.1) ~ "0 - 0.1",
                                  (IDENTITY_ATTACK > 0.1  & IDENTITY_ATTACK < 0.2) ~ "0.1 - 0.2",
                                  (IDENTITY_ATTACK > 0.2  & IDENTITY_ATTACK < 0.3) ~ "0.2 - 0.3",
                                  (IDENTITY_ATTACK > 0.3  & IDENTITY_ATTACK < 0.4) ~ "0.3 - 0.4",
                                  (IDENTITY_ATTACK > 0.4  & IDENTITY_ATTACK < 0.5) ~ "0.4 - 0.5",
                                  (IDENTITY_ATTACK > 0.5  & IDENTITY_ATTACK < 0.6) ~ "0.5 - 0.6",
                                  (IDENTITY_ATTACK > 0.6  & IDENTITY_ATTACK < 0.7) ~ "0.6 - 0.7",
                                  (IDENTITY_ATTACK > 0.7  & IDENTITY_ATTACK < 0.8) ~ "0.7 - 0.8",
                                  (IDENTITY_ATTACK > 0.8  & IDENTITY_ATTACK < 0.9) ~ "0.8 - 0.9",
                                  (IDENTITY_ATTACK > 0.9  & IDENTITY_ATTACK < 1) ~ "0.9 - 1")) %>%
  mutate(cat_insult = case_when((INSULT > 0  & INSULT < 0.1) ~ "0 - 0.1",
                                (INSULT > 0.1  & INSULT < 0.2) ~ "0.1 - 0.2",
                                (INSULT > 0.2  & INSULT < 0.3) ~ "0.2 - 0.3",
                                (INSULT > 0.3  & INSULT < 0.4) ~ "0.3 - 0.4",
                                (INSULT > 0.4  & INSULT < 0.5) ~ "0.4 - 0.5",
                                (INSULT > 0.5  & INSULT < 0.6) ~ "0.5 - 0.6",
                                (INSULT > 0.6  & INSULT < 0.7) ~ "0.6 - 0.7",
                                (INSULT > 0.7  & INSULT < 0.8) ~ "0.7 - 0.8",
                                (INSULT > 0.8  & INSULT < 0.9) ~ "0.8 - 0.9",
                                (INSULT > 0.9  & INSULT < 1) ~ "0.9 - 1")) %>%
  mutate(cat_threat = case_when((THREAT > 0  & THREAT < 0.1) ~ "0 - 0.1",
                                (THREAT > 0.1  & THREAT < 0.2) ~ "0.1 - 0.2",
                                (THREAT > 0.2  & THREAT < 0.3) ~ "0.2 - 0.3",
                                (THREAT > 0.3  & THREAT < 0.4) ~ "0.3 - 0.4",
                                (THREAT > 0.4  & THREAT < 0.5) ~ "0.4 - 0.5",
                                (THREAT > 0.5  & THREAT < 0.6) ~ "0.5 - 0.6",
                                (THREAT > 0.6  & THREAT < 0.7) ~ "0.6 - 0.7",
                                (THREAT > 0.7  & THREAT < 0.8) ~ "0.7 - 0.8",
                                (THREAT > 0.8  & THREAT < 0.9) ~ "0.8 - 0.9",
                                (THREAT > 0.9  & THREAT < 1) ~ "0.9 - 1"))


tct_toxicity <- tct %>%
  group_by(match) %>% 
  dplyr::count(cat_toxicity) %>% 
  mutate(percent = n/sum(n)* 100)


tct_TOXICITY <- ggplot(tct_toxicity, aes(x = cat_toxicity, y = percent, fill = as.factor(match))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% TOXICITY") + xlab("") +
  scale_fill_manual(labels = c("3 hour interval pre treatment", "3 hour interval post treatment"), values = c("#999999", "#454545"))



tct_severe <- tct %>%
  group_by(match) %>% 
  dplyr::count(cat_severe) %>% 
  mutate(percent = n/sum(n)* 100)


tct_SEVERE <- ggplot(tct_severe, aes(x = cat_severe, y = percent, fill = as.factor(match))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% SEVERE TOXICITY") + xlab("") +
  scale_fill_manual(labels = c("3 hour interval pre treatment", "3 hour interval post treatment"), values = c("#999999", "#454545"))



tct_identity <- tct %>%
  group_by(match) %>% 
  dplyr::count(cat_identity) %>% 
  mutate(percent = n/sum(n)* 100)

tct_IDENTITY <- ggplot(tct_identity, aes(x = cat_identity, y = percent, fill = as.factor(match))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 80),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% IDENTITY ATTACK") + xlab("") +
  scale_fill_manual(labels = c("3 hour interval pre treatment", "3 hour interval post treatment"), values = c("#999999", "#454545"))



tct_sex <- tct %>%
  group_by(match) %>% 
  dplyr::count(cat_sex) %>% 
  mutate(percent = n/sum(n)* 100)


tct_SEXUALLY <- ggplot(tct_sex, aes(x = cat_sex, y = percent, fill = as.factor(match))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% SEXUALLY EXPLICIT") + xlab("") +
  scale_fill_manual(labels = c("3 hour interval pre treatment", "3 hour interval post treatment"), values = c("#999999", "#454545"))


tct_insult <- tct %>%
  group_by(match) %>% 
  dplyr::count(cat_insult) %>% 
  mutate(percent = n/sum(n)* 100)  

tct_INSULT <- ggplot(tct_insult, aes(x = cat_insult, y = percent, fill = as.factor(match))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% INSULT") + xlab("") +
  scale_fill_manual(labels = c("3 hour interval pre treatment", "3 hour interval post treatment"), values = c("#999999", "#454545"))



tct_threat <- tct %>%
  group_by(match) %>% 
  dplyr::count(cat_threat) %>% 
  mutate(percent = n/sum(n)* 100)  

tct_THREAT <- ggplot(tct_threat, aes(x = cat_threat, y = percent, fill = as.factor(match))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% THREAT") + xlab("Perspective API score intervals") +
  scale_fill_manual(labels = c("3 hour interval pre treatment", "3 hour interval post treatment"), values = c("#999999", "#454545"))


tct_fig_distribution_by_atrribute <- ggarrange(tct_TOXICITY, tct_SEVERE, tct_IDENTITY, 
                                               tct_SEXUALLY, tct_INSULT, tct_THREAT,
                                               ncol = 1, nrow = 6,
                                               common.legend = TRUE, legend = "bottom", font.label = list(size = 20))


# save 
ggsave(path = "", 
       "tct_fig_distribution_by_atrribute.pdf", width = 8, height = 10)

####################################################################################################################################
### Figure 5: Interval distribution for attribute Perspective scores ###
####################################################################################################################################

# create intvals for each attribute
interval_table <- motherload %>%
  select(gender, media, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT) %>%
  mutate(cat_toxicity = case_when((TOXICITY > 0  & TOXICITY < 0.1) ~ "0 - 0.1",
                                  (TOXICITY > 0.1  & TOXICITY < 0.2) ~ "0.1 - 0.2",
                                  (TOXICITY > 0.2  & TOXICITY < 0.3) ~ "0.2 - 0.3",
                                  (TOXICITY > 0.3  & TOXICITY < 0.4) ~ "0.3 - 0.4",
                                  (TOXICITY > 0.4  & TOXICITY < 0.5) ~ "0.4 - 0.5",
                                  (TOXICITY > 0.5  & TOXICITY < 0.6) ~ "0.5 - 0.6",
                                  (TOXICITY > 0.6  & TOXICITY < 0.7) ~ "0.6 - 0.7",
                                  (TOXICITY > 0.7  & TOXICITY < 0.8) ~ "0.7 - 0.8",
                                  (TOXICITY > 0.8  & TOXICITY < 0.9) ~ "0.8 - 0.9",
                                  (TOXICITY > 0.9  & TOXICITY < 1) ~ "0.9 - 1")) %>%
  mutate(cat_severe = case_when((SEVERE_TOXICITY > 0  & SEVERE_TOXICITY < 0.1) ~ "0 - 0.1",
                                (SEVERE_TOXICITY > 0.1  & SEVERE_TOXICITY < 0.2) ~ "0.1 - 0.2",
                                (SEVERE_TOXICITY > 0.2  & SEVERE_TOXICITY < 0.3) ~ "0.2 - 0.3",
                                (SEVERE_TOXICITY > 0.3  & SEVERE_TOXICITY < 0.4) ~ "0.3 - 0.4",
                                (SEVERE_TOXICITY > 0.4  & SEVERE_TOXICITY < 0.5) ~ "0.4 - 0.5",
                                (SEVERE_TOXICITY > 0.5  & SEVERE_TOXICITY < 0.6) ~ "0.5 - 0.6",
                                (SEVERE_TOXICITY > 0.6  & SEVERE_TOXICITY < 0.7) ~ "0.6 - 0.7",
                                (SEVERE_TOXICITY > 0.7  & SEVERE_TOXICITY < 0.8) ~ "0.7 - 0.8",
                                (SEVERE_TOXICITY > 0.8  & SEVERE_TOXICITY < 0.9) ~ "0.8 - 0.9",
                                (SEVERE_TOXICITY > 0.9  & SEVERE_TOXICITY < 1) ~ "0.9 - 1")) %>%
  mutate(cat_sex = case_when((SEXUALLY_EXPLICIT > 0  & SEXUALLY_EXPLICIT < 0.1) ~ "0 - 0.1",
                             (SEXUALLY_EXPLICIT > 0.1  & SEXUALLY_EXPLICIT < 0.2) ~ "0.1 - 0.2",
                             (SEXUALLY_EXPLICIT > 0.2  & SEXUALLY_EXPLICIT < 0.3) ~ "0.2 - 0.3",
                             (SEXUALLY_EXPLICIT > 0.3  & SEXUALLY_EXPLICIT < 0.4) ~ "0.3 - 0.4",
                             (SEXUALLY_EXPLICIT > 0.4  & SEXUALLY_EXPLICIT < 0.5) ~ "0.4 - 0.5",
                             (SEXUALLY_EXPLICIT > 0.5  & SEXUALLY_EXPLICIT < 0.6) ~ "0.5 - 0.6",
                             (SEXUALLY_EXPLICIT > 0.6  & SEXUALLY_EXPLICIT < 0.7) ~ "0.6 - 0.7",
                             (SEXUALLY_EXPLICIT > 0.7  & SEXUALLY_EXPLICIT < 0.8) ~ "0.7 - 0.8",
                             (SEXUALLY_EXPLICIT > 0.8  & SEXUALLY_EXPLICIT < 0.9) ~ "0.8 - 0.9",
                             (SEXUALLY_EXPLICIT > 0.9  & SEXUALLY_EXPLICIT < 1) ~ "0.9 - 1")) %>%
  mutate(cat_identity = case_when((IDENTITY_ATTACK > 0  & IDENTITY_ATTACK < 0.1) ~ "0 - 0.1",
                                  (IDENTITY_ATTACK > 0.1  & IDENTITY_ATTACK < 0.2) ~ "0.1 - 0.2",
                                  (IDENTITY_ATTACK > 0.2  & IDENTITY_ATTACK < 0.3) ~ "0.2 - 0.3",
                                  (IDENTITY_ATTACK > 0.3  & IDENTITY_ATTACK < 0.4) ~ "0.3 - 0.4",
                                  (IDENTITY_ATTACK > 0.4  & IDENTITY_ATTACK < 0.5) ~ "0.4 - 0.5",
                                  (IDENTITY_ATTACK > 0.5  & IDENTITY_ATTACK < 0.6) ~ "0.5 - 0.6",
                                  (IDENTITY_ATTACK > 0.6  & IDENTITY_ATTACK < 0.7) ~ "0.6 - 0.7",
                                  (IDENTITY_ATTACK > 0.7  & IDENTITY_ATTACK < 0.8) ~ "0.7 - 0.8",
                                  (IDENTITY_ATTACK > 0.8  & IDENTITY_ATTACK < 0.9) ~ "0.8 - 0.9",
                                  (IDENTITY_ATTACK > 0.9  & IDENTITY_ATTACK < 1) ~ "0.9 - 1")) %>%
  mutate(cat_insult = case_when((INSULT > 0  & INSULT < 0.1) ~ "0 - 0.1",
                                (INSULT > 0.1  & INSULT < 0.2) ~ "0.1 - 0.2",
                                (INSULT > 0.2  & INSULT < 0.3) ~ "0.2 - 0.3",
                                (INSULT > 0.3  & INSULT < 0.4) ~ "0.3 - 0.4",
                                (INSULT > 0.4  & INSULT < 0.5) ~ "0.4 - 0.5",
                                (INSULT > 0.5  & INSULT < 0.6) ~ "0.5 - 0.6",
                                (INSULT > 0.6  & INSULT < 0.7) ~ "0.6 - 0.7",
                                (INSULT > 0.7  & INSULT < 0.8) ~ "0.7 - 0.8",
                                (INSULT > 0.8  & INSULT < 0.9) ~ "0.8 - 0.9",
                                (INSULT > 0.9  & INSULT < 1) ~ "0.9 - 1")) %>%
  mutate(cat_threat = case_when((THREAT > 0  & THREAT < 0.1) ~ "0 - 0.1",
                                (THREAT > 0.1  & THREAT < 0.2) ~ "0.1 - 0.2",
                                (THREAT > 0.2  & THREAT < 0.3) ~ "0.2 - 0.3",
                                (THREAT > 0.3  & THREAT < 0.4) ~ "0.3 - 0.4",
                                (THREAT > 0.4  & THREAT < 0.5) ~ "0.4 - 0.5",
                                (THREAT > 0.5  & THREAT < 0.6) ~ "0.5 - 0.6",
                                (THREAT > 0.6  & THREAT < 0.7) ~ "0.6 - 0.7",
                                (THREAT > 0.7  & THREAT < 0.8) ~ "0.7 - 0.8",
                                (THREAT > 0.8  & THREAT < 0.9) ~ "0.8 - 0.9",
                                (THREAT > 0.9  & THREAT < 1) ~ "0.9 - 1"))


# descriptive table 
# frequence table on gender 
freq(interval_table$gender,
     report.nas = FALSE, cumul = FALSE)




# TOXICITY
interval_toxicity <- interval_table %>%
  group_by(gender) %>% 
  dplyr::count(cat_toxicity) %>% 
  mutate(percent = n/sum(n)* 100)

dis_TOXICITY <- ggplot(interval_toxicity, aes(x = cat_toxicity, y = percent, fill = gender)) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% TOXICITY") + xlab("") +
  scale_fill_manual(labels = c("Women", "Men"), values = c("#f46966", "#66aaf4"))


# SEVERE TOXICITY
interval_severe <- interval_table %>%
  group_by(gender) %>% 
  dplyr::count(cat_severe) %>% 
  mutate(percent = n/sum(n)* 100)

dis_SEVERE <- ggplot(interval_severe, aes(x = cat_severe, y = percent, fill = gender)) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% SEVERE TOXICITY") + xlab("") +
  scale_fill_manual(labels = c("Women", "Men"), values = c("#f46966", "#66aaf4"))


# IDENTITY ATTACK
interval_identity <- interval_table %>%
  group_by(gender) %>% 
  dplyr::count(cat_identity) %>% 
  mutate(percent = n/sum(n)* 100)

dis_IDENTITY <- ggplot(interval_identity, aes(x = cat_identity, y = percent, fill = gender)) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% IDENTITY ATTACK") + xlab("") +
  scale_fill_manual(labels = c("Women", "Men"), values = c("#f46966", "#66aaf4"))
  

# SEXUALLY EXPLICIT
interval_sex <- interval_table %>%
  group_by(gender) %>% 
  dplyr::count(cat_sex) %>% 
  mutate(percent = n/sum(n)* 100)

dis_SEXUALLY <- ggplot(interval_sex, aes(x = cat_sex, y = percent, fill = gender)) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% SEXUALLY EXPLICIT") + xlab("") +
  scale_fill_manual(labels = c("Women", "Men"), values = c("#f46966", "#66aaf4"))


# INSULT
interval_insult <- interval_table %>%
  group_by(gender) %>% 
  dplyr::count(cat_insult) %>% 
  mutate(percent = n/sum(n)* 100)  

dis_INSULT <- ggplot(interval_insult, aes(x = cat_insult, y = percent, fill = gender)) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% INSULT") + xlab("") +
  scale_fill_manual(labels = c("Women", "Men"), values = c("#f46966", "#66aaf4"))


# THREAT
interval_threat <- interval_table %>%
  group_by(gender) %>% 
  dplyr::count(cat_threat) %>% 
  mutate(percent = n/sum(n)* 100)  

dis_THREAT <- ggplot(interval_threat, aes(x = cat_threat, y = percent, fill = gender)) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-0.00, 75),
                     expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% THREAT") + xlab("Perspective API score intervals") +
  scale_fill_manual(labels = c("Women", "Men"), values = c("#f46966", "#66aaf4"))


fig_distribution_by_atrribute <- ggarrange(dis_TOXICITY, dis_SEVERE, dis_SEXUALLY, dis_IDENTITY, 
                                     dis_INSULT, dis_THREAT,
                                     ncol = 1, nrow = 6,
                                     common.legend = TRUE, legend = "bottom", font.label = list(size = 20))

# save 
ggsave(path = "", 
       "fig_distribution_by_atrribute.pdf", width = 10, height = 9)

####################################################################################################################################
### Interval distribution for attribute Perspective scores on media (APPENDIX) ###
####################################################################################################################################


media_table <- interval_table %>%
  mutate(bias = case_when(media == "CNN" ~ 1,
                          media == "The Washington Post" ~ 1,
                          media == "The Atlantic" ~ 1,
                          media == "The Verge" ~ 1,
                          media == "CBS" ~ 1,
                          media == "The Intercept" ~ 1,
                          media == "NYT" ~ 1,
                          media == "ABC" ~ 1,
                          media == "MSNBC" ~ 1,
                          media == "NBC" ~ 1,
                          media == "Outkick" ~ 2,
                          media == "CNBC" ~ 2,
                          media == "Fox News" ~ 3,
                          media == "Breitbart News" ~ 3,
                          media == "The Federalist" ~ 3,
                          media == "Boston Herald" ~ 3,
                          media == "Full Measure News" ~ 4,
                          media == "The Sun" ~ 4,
                          media == "Sirius XM" ~ 4,
                          media == "The Grayzone" ~ 4,
                          media == "Compact Magazine" ~ 4,
                          media == "GB News" ~ 4,
                          media == "The Daily Beast" ~ 4,
                          media == "Comedy Central" ~ 4,
                          media == "TMZ" ~ 4,
                          media == "Freelance" ~ 4))


# remove moderate media and not rated media
media_table <- media_table[!(media_table$bias == 2 | media_table$bias == 4),]



# descriptive table 
media_table_stat <- ctable(
  x = media_table$bias,
  y = media_table$cat_toxicity
)


# descriptive table 
# frequence table on gender 
freq(media_table$bias,
     report.nas = FALSE, cumul = FALSE)


# TOXICITY
media_interval_toxicity <- media_table %>%
  group_by(bias) %>% 
  dplyr::count(cat_toxicity) %>% 
  mutate(percent = n/sum(n)* 100)

media_dis_TOXICITY <- ggplot(media_interval_toxicity, aes(x = cat_toxicity, y = percent, fill = factor(bias))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  #scale_y_continuous(limits = c(-0.00, 100),
   #                  expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% TOXICITY") + xlab("") +
  scale_fill_manual(labels = c("Left wing", "Right wing"), values = c("#454545", "#999999"))


# SEVERE TOXICITY
media_interval_severe_toxicity <- media_table %>%
  group_by(bias) %>% 
  dplyr::count(cat_severe) %>% 
  mutate(percent = n/sum(n)* 100)

media_dis_SEVERE <- ggplot(media_interval_severe_toxicity, aes(x = cat_severe, y = percent, fill = factor(bias))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  #scale_y_continuous(limits = c(-0.00, 100),
  #                   expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% SEVERE TOXICITY") + xlab("") +
  scale_fill_manual(labels = c("Left wing", "Right wing"), values = c("#454545", "#999999"))


# IDENTITY ATTACK
media_identity_attack <- media_table %>%
  group_by(bias) %>% 
  dplyr::count(cat_identity) %>% 
  mutate(percent = n/sum(n)* 100)

media_dis_IDENTITY <- ggplot(media_identity_attack, aes(x = cat_identity, y = percent, fill = factor(bias))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
 #scale_y_continuous(limits = c(-0.00, 100),
 #                  expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% IDENTITY ATTACK") + xlab("") +
  scale_fill_manual(labels = c("Left wing", "Right wing"), values = c("#454545", "#999999"))


# SEXUALLY EXPLICIT
media_sexually_explicit <- media_table %>%
  group_by(bias) %>% 
  dplyr::count(cat_sex) %>% 
  mutate(percent = n/sum(n)* 100)

media_dis_SEXUALLY <- ggplot(media_sexually_explicit, aes(x = cat_sex, y = percent, fill = factor(bias))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  #scale_y_continuous(limits = c(-0.00, 100),
  #                  expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% SEXUALLY EXPLICIT") + xlab("") +
  scale_fill_manual(labels = c("Left wing", "Right wing"), values = c("#454545", "#999999"))



# INSULT
media_insult <- media_table %>%
  group_by(bias) %>% 
  dplyr::count(cat_insult) %>% 
  mutate(percent = n/sum(n)* 100)

media_dis_INSULT <- ggplot(media_insult, aes(x = cat_insult, y = percent, fill = factor(bias))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  #scale_y_continuous(limits = c(-0.00, 100),
  #                  expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% INSULT") + xlab("") +
  scale_fill_manual(labels = c("Left wing", "Right wing"), values = c("#454545", "#999999"))


# THREAT
media_threat <- media_table %>%
  group_by(bias) %>% 
  dplyr::count(cat_threat) %>% 
  mutate(percent = n/sum(n)* 100)

media_dis_THREAT <- ggplot(media_threat, aes(x = cat_threat, y = percent, fill = factor(bias))) +
  geom_col(position=position_dodge()) +
  geom_text(aes(label = round(percent, 1)), vjust = -0.2,  position = position_dodge(.9), family = "Roboto Mono", size = 3) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() + 
  #scale_y_continuous(limits = c(-0.00, 100),
  #                   expand = c(0, 0)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(family = "Roboto Mono", size = 9),
        axis.title.y = element_text(size = 8),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, vjust = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = "white", size = 0.25)) +
  ylab("% THREAT") + xlab("") +
  scale_fill_manual(labels = c("Left wing", "Right wing"), values = c("#454545", "#999999"))


fig_distribution_by_atrribute <- ggarrange(media_dis_TOXICITY, media_dis_SEVERE, media_dis_IDENTITY, 
                                           media_dis_SEXUALLY, media_dis_INSULT, media_dis_THREAT,
                                           ncol = 1, nrow = 6,
                                           common.legend = TRUE, legend = "bottom", font.label = list(size = 20))

# save 
ggsave(path = "", 
       "fig_distribution_by_atrribute.pdf", width = 10, height = 10)




### ANALYSIS USING JOURNALISTS THAT HAVE BEEN MENTIONED AT LEAST TEN TIMES WAS REMOVED FROM THE PAPER 
### CODE USED FOR THE ANALYSIS I BELOW
####################################################################################################################################
### LOAD TWEETS FOR INTENSE TREATMENT ANALYSIS
####################################################################################################################################

### I REPEATE THE SAME CODE BUT FOR JOURNALIST THAT HAVE BEEN CALLED OUT AT LEAST 10 TIMES WITHIN AN EPISODE
### HYPOTHESIS 2
# treatment group (hypothesis 2)
# load csv files for treatment group called out 10 times or more 
treatment_file_path_10 <- dir_ls("")
treatment_file_content_10 <- list()

# loop through file_path and load all csv files for with intense treatment group into list file_content 
for (i in seq_along(treatment_file_path_10)) {
  treatment_file_content_10[[i]] <- read_csv(
    file = treatment_file_path_10[[i]]
  )
}

# control group (hypothesis 2)
# load csv files for control group
control_file_path_10 <- dir_ls("")
control_file_content_10 <- list()

# loop through file_path and load all csv files for control group into list file_content 
for (i in seq_along(control_file_path_10)) {
  control_file_content_10[[i]] <- read_csv(
    file = control_file_path_10[[i]]
  )
}


# rowbind all csv files for treatment group and control group separately (hypthesis 1)
treatment_group_10 <- ldply(treatment_file_content_10, use.names = TRUE) # intens treatment group
control_group_10 <- ldply(control_file_content_10, use.names = TRUE) # intens control group


# remove all headers and tweets with more than one @twitter_handle to make sure all tweets
# only relates to the specific treated journalist. Lastly, remove all duplicates (not a problem after new python code)  
treatment_group_10 <- treatment_group_10 %>%
  filter(id != "id") %>%
  filter(!str_count(tweet, "@") > 1) %>%
  mutate(journalist_id = as.numeric(journalist_id)) %>%
  arrange(journalist_id) %>%
  distinct()

control_group_10 <- control_group_10 %>%
  filter(id != "id") %>%
  filter(!str_count(tweet, "@") > 1) %>%
  mutate(journalist_id = as.numeric(journalist_id)) %>%
  arrange(journalist_id) %>%
  distinct()


# add id column to treatment group and control group. 
# This step is required by the Perspective API as an input variable
treatment_group_10 <- rowid_to_column(treatment_group_10, "text_id")
control_group_10 <- rowid_to_column(control_group_10, "text_id")

### SPLIT DATA SETS INTO SUB-DATASET FOR FASTER SCORING OF THE PERSPECTIVE API
# due to slow a rate of scoring tweets approx. 200 a minute (quota cap. is 1800 tweets a minute) I run the scoring process 
# in several R session simultaniously. Therefore I split my data.frame into several smaller data.frames
# treatment group
#t1_10 <- treatment_group_10 %>% 
#  slice(0:20000)
#t2_10 <- treatment_group_10 %>% 
#  slice(20001:42075)
#write.csv(t1_10,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\treatment_group\\t1_10.csv", 
#          row.names = FALSE)
#write.csv(t2_10,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\treatment_group\\t2_10.csv", 
#          row.names = FALSE)

# control group
#c1_10 <- control_group_10 %>% 
#  slice(0:30000)
#c2_10 <- control_group_10 %>% 
#  slice(30001:60000)
#c3_10 <- control_group_10 %>% 
#  slice(60001:90000)
#c4_10 <- control_group_10 %>% 
#  slice(90001:120000)
#c5_10 <- control_group_10 %>% 
#  slice(120001:137164)
#write.csv(c1_10,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c1_10.csv", 
#          row.names = FALSE)
#write.csv(c2_10,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c2_10.csv", 
#          row.names = FALSE)
#write.csv(c3_10,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c3_10.csv", 
#          row.names = FALSE)
#write.csv(c4_10,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c4_10.csv", 
#          row.names = FALSE)
#write.csv(c5_10,"C:\\Users\\Marco Liedecke\\Desktop\\Twitter_Tone\\Data\\control_group\\c5_10.csv", 
#          row.names = FALSE)


# load treatment_group_scores and control_group_scores
t1_10_scores <- read_csv("")
t2_10_scores <- read_csv("")
treatment_group_scores_10 <- do.call("rbind", list(t1_10_scores, t2_10_scores)) # row bind all data.frames

# control group
c1_10_scores <- read_csv("")
c2_10_scores <- read_csv("")
c3_10_scores <- read_csv("")
c4_10_scores <- read_csv("")
c5_10_scores <- read_csv("")
control_group_scores_10 <- do.call("rbind", list(c1_10_scores, c2_10_scores, c3_10_scores, c4_10_scores, c5_10_scores)) # row bind all data.frames

# merge data with Perpsective scores with data on journalsits 
treatment_10 <- merge(treatment_group_scores_10, treatment_group_10, by = "text_id")
control_10 <- merge(control_group_scores_10, control_group_10, by = "text_id")

# load dataset with details on treatment group
# variables: names, twitter_handle, media, gender, weekday, ID, treatment_id, showtime, date, start_date, and end_date
tucker_df_10 <- read_csv("")

# rename "id" to "journalist_id", select columns "date_id", "journalist_id", "show_id", "names", "twitter_handle", 
# "showtime", "gender", and slice the data by names so only the first treatment per journalist is selected
treatment_names_10 <- tucker_df_10 %>%
  dplyr::rename(journalist_id = treatment_id) %>%
  dplyr::rename(show_date = Date) %>%
  dplyr::select(journalist_id, names, twitter_handle, showtime, gender, media, show_date)


# merge treatment names and treatment group in order to add background data such as name, gender, and showtime
treatment_10 <- merge(treatment_names_10, treatment_10, by = "journalist_id")
control_10 <- merge(treatment_names_10, control_10, by = "journalist_id")


# TREATMENT GROUP
# clean treatment group: select relevant columns, create day column, treatment dummy, group, and day count. Drop all NaN
c_treatment_10 <- treatment_10 %>%
  #filter(created_at != 'created_at') %>%
  drop_na() %>%
  dplyr::select(names, journalist_id, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT, tweet,
                text_id, created_at, showtime, show_date, gender, media) %>%
  mutate(created_at = ymd_hms(created_at),
         group = "treatment",
         #group = case_when(created_at > showtime ~ "treatment",
         #                  created_at < showtime ~ "control"),
         date = as_date(created_at),
         day_count = as.numeric(factor(date)),
         tal = 1,
         treatment_dummy = 1,
         time_to_treat = as.numeric(difftime(date, show_date , units = c("days"))))


# add time dummy: if a tweet was tweeted after the journalist was mentioned by Tucker it is 1 i.e. treated and 
# if the tweet was tweeted before treatment it is 0
#tg$treatment_dummy <- ifelse(tg$showtime <= as_datetime(tg$created_at), 1, 0)

# add time dummy: if a tweet was tweeted after the journalist was mentioned by Tucker it is 1 i.e. treated and 
# if the tweet was tweeted before treatment it is 0
#treatment_group_10$treatment_dummy <- ifelse(treatment_group_10$showtime <= as_datetime(treatment_group_10$created_at), 1, 0)

# CONTROL GROUP
# clean control group: select relevant columns, create day column, treatment dummy, group, and day count. Drop all NaN
c_control_10 <- control_10 %>%
  filter(created_at != 'created_at') %>%
  drop_na() %>%
  dplyr::select(names, journalist_id, TOXICITY, SEVERE_TOXICITY, SEXUALLY_EXPLICIT, IDENTITY_ATTACK, INSULT, THREAT, tweet,
                text_id, created_at, showtime, show_date, gender, media) %>%
  mutate(group = "control",
         date = as_date(created_at),
         treatment_dummy = 0,
         tal = 1,
         time_to_treat = 0)


# combine treatment group and control group data frame will
motherload_10 <- rbind.fill(c_control_10, c_treatment_10)

# add day_count variable. day 1 starts with the first day 2018-11-22 in Jim Acostas period as 
# he is the first treated journalist
motherload_10 <- motherload_10 %>%
  mutate(day_count = as.numeric(factor(date))) %>%
  group_by(journalist_id) %>%
  #mutate(gname = case_when(treatment_dummy == 1 ~ first(day_count) + 5,
  #                         treatment_dummy == 0 ~ 0)) %>%
  mutate(gname = case_when(treatment_dummy == 1 ~ first(day_count) + 5,
                           treatment_dummy == 0 ~ first(day_count) + 5)) %>%
  ungroup(journalist_id)


####################################################################################################################################
### DID I N T E N S   T R E A T M E N T   A L L   J O U R N A L I S T S ###
####################################################################################################################################

# TOXICITY (all media)
all_model_1_10 <- felm(TOXICITY ~ treatment_dummy              # outcome variable regressed treatment variable
                       | factor(journalist_id) + factor(day_count) # unit FE and time FE
                       | 0                                         # no instrumental variables
                       | journalist_id,                            # cluster at journalist level 
                       data = fatherload)

# SEVERE_TOXICITY (all media)
all_model_2_10 <- felm(SEVERE_TOXICITY ~ treatment_dummy
                       | factor(journalist_id) + factor(day_count)
                       | 0
                       | journalist_id,
                       data = fatherload)

# SEXUALLY_EXPLICIT (all media)
all_model_3_10 <- felm(SEXUALLY_EXPLICIT ~ treatment_dummy
                       | factor(journalist_id) + factor(day_count)
                       | 0
                       | journalist_id,
                       data = fatherload)

# IDENTITY_ATTACK (all media)
all_model_4_10 <- felm(IDENTITY_ATTACK ~ treatment_dummy
                       | factor(journalist_id) + factor(day_count)
                       | 0
                       | journalist_id,
                       data = fatherload)

# INSULT (all media)
all_model_5_10 <- felm(INSULT ~ treatment_dummy
                       | factor(journalist_id) + factor(day_count)
                       | 0
                       | journalist_id,
                       data = fatherload)

# THREAT (all media)
all_model_6_10 <- felm(THREAT ~ treatment_dummy
                       | factor(journalist_id) + factor(day_count)
                       | 0
                       | journalist_id,
                       data = fatherload)


# Stargazer table: A full table showing regression results for all 6 categories of toxicity 
stargazer(all_model_1_10, all_model_2_10, all_model_3_10, all_model_4_10, all_model_5_10, all_model_6_10, type = "latex", 
          title = "Restults: the Tucker Carlson Effect with intense treatment",
          dep.var.labels = c("Toxicity", "Severe", "Sexually", "Identity", "Insult", "Threat"),
          covariate.labels = "Treatment",
          add.lines = list(c("Unit FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Time FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          keep.stat = c("rsq", "n"),
          column.sep.width = "-5pt") # Show just the r-square and number of obs.

####################################################################################################################################
### DID I N T E N S   T R E A T M E N T   L E F T   J O U R N A L I S T S ###
####################################################################################################################################

fatherload_left <- fatherload %>%
  filter(media != "Fox News",
         media != "Breitbart News",
         media != "The Federalist",
         media != "Boston Herald",
         media != "Freelance",
         media != "Compact Magazine",
         media != "GB News",
         media != "The Daily Beast",
         media != "Comedy Central",
         media != "TMZ",
         media != "Outkick",
         media != "CNBC")


# TOXICITY (all media)
all_model_1_10_left <- felm(TOXICITY ~ treatment_dummy              # outcome variable regressed treatment variable
                            | factor(journalist_id) + factor(day_count) # unit FE and time FE
                            | 0                                         # no instrumental variables
                            | journalist_id,                            # cluster at journalist level 
                            data = fatherload_left)

# SEVERE_TOXICITY (all media)
all_model_2_10_left <- felm(SEVERE_TOXICITY ~ treatment_dummy
                            | factor(journalist_id) + factor(day_count)
                            | 0
                            | journalist_id,
                            data = fatherload_left)

# SEXUALLY_EXPLICIT (all media)
all_model_3_10_left <- felm(SEXUALLY_EXPLICIT ~ treatment_dummy
                            | factor(journalist_id) + factor(day_count)
                            | 0
                            | journalist_id,
                            data = fatherload_left)

# IDENTITY_ATTACK (all media)
all_model_4_10_left <- felm(IDENTITY_ATTACK ~ treatment_dummy
                            | factor(journalist_id) + factor(day_count)
                            | 0
                            | journalist_id,
                            data = fatherload_left)

# INSULT (all media)
all_model_5_10_left <- felm(INSULT ~ treatment_dummy
                            | factor(journalist_id) + factor(day_count)
                            | 0
                            | journalist_id,
                            data = fatherload_left)

# THREAT (all media)
all_model_6_10_left <- felm(THREAT ~ treatment_dummy
                            | factor(journalist_id) + factor(day_count)
                            | 0
                            | journalist_id,
                            data = fatherload_left)


# Stargazer table: A full table showing regression results for all 6 categories of toxicity 
stargazer(all_model_1_10_left, all_model_2_10_left, all_model_3_10_left, all_model_4_10_left, all_model_5_10_left, all_model_6_10_left, type = "latex", 
          title = "Restults: the Tucker Carlson Effect with intense treatment on left wing journalists",
          dep.var.labels = c("Toxicity", "Severe", "Sexually", "Identity", "Insult", "Threat"),
          covariate.labels = "Treatment",
          add.lines = list(c("Unit FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Time FE",   "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          keep.stat = c("rsq", "n"),
          column.sep.width = "-5pt") # Show just the r-square and number of obs.




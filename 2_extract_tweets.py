import requests  # sending GET requests from the API
import os  # saving access tokens and for file management when creating and adding to the dataset
import json  # dealing with json responses we receive from the API
import pandas as pd  # For displaying the data after
import csv  # saving the response data in CSV format
import numpy as np
import tweepy  # interaction with Twitter API
import dateutil.parser
from time import sleep  # sleep between request to Twitter API
import difflib
import re
from datetime import datetime, timezone
import rfc3339
import spacy  # detect names in transcripts
import math
import itertools
from fuzzywuzzy import fuzz  # filter out similar names
from fuzzywuzzy import process
from fuzzywuzzy.process import dedupe

########################################################################################################################
### L O A D   T R A N C R I P T S   A N D   D E T E C T   J O U R N A L I S T S ###
########################################################################################################################
# load data.frame with transcripts, Date, and ID (show_id) column
transcripts = pd.read_csv(r"", encoding='latin-1')

# load spacy to detect names in transcripts
nlp = spacy.load(r"")

# crate empty list for names mentioned by Tucker Carlson and index indicating the specific episode
names = []
ID = []

# 1. loop over transcripts as a string since nlp only takes strings as input
for idx, trans in transcripts["Article"].astype(str).items():
    doc = nlp(trans)
    # 2. loop over each transcript and detect if a word or bigram is == "PERSON" if so append the
    # name to list names and ID to list ID
    for ent in doc.ents:
        if ent.label_ == "PERSON":
            names.append(ent.text)
            ID.append(idx)

# create data.frame with index and all names
names_df = pd.DataFrame(
    {'names': names,
     "ID": ID
     })

# drop duplicate names as I am (for now) only interested in the names and not the specific show the
# name was mentioned in
names_df = names_df.drop_duplicates(subset='names')

# remove all unigrams (i.e. not full name)
names_df = names_df[names_df['names'].str.contains(" ")]

# turn names to lower case and remove duplicate names
names_df['names'] = names_df['names'].str.lower()
names_df = names_df.drop_duplicates(subset='names')

# keep only one example of similar names correcting for typos etc.
# first turn data.frame column "names" to list as input to fuzzywuzzy function.
# lasty turn the list back to a data.frame
names_list = names_df["names"].tolist()
names_df = list(dedupe(names_list, threshold=80, scorer=fuzz.ratio))  # 80 ratio is somewhat arbitrary
names_df = pd.DataFrame(names_df, columns=['names'])

# save dataframe with all names Tucker Carlson has mentioned on his show.
# I will manually code the data.frame in Numbers adding twitter_handle, gender, and media variable
names_df.to_csv(r"",
                encoding='utf-8',
                index=False)

########################################################################################################################
### T W I T T E R   F U N C T I O N S ###
########################################################################################################################
# The following functions are written by Andrew Edward and are presented in the Medium article "An Extensive Guide to
# collecting tweets from Twitter API v2 for academic research using Python 3". I only tweaked the functions a little
# to suit my purpose

# twitter token
os.environ[
    'TOKEN'] = ''


def auth():
    return os.getenv('TOKEN')


def create_headers(bearer_token):
    headers = {"Authorization": "Bearer {}".format(bearer_token)}
    return headers


# function that hit search_all endpoint
def create_url(keyword, start_date, end_date, max_results=500):
    search_url = "https://api.twitter.com/2/tweets/search/all"  # search all endpoint

    # change params based on the endpoint you are using
    query_params = {'query': keyword,
                    'start_time': start_date,
                    'end_time': end_date,
                    'max_results': max_results,
                    'expansions': 'author_id',
                    'tweet.fields': 'id,text,author_id,created_at',
                    'next_token': {}}
    return search_url, query_params


# function that will connect to twitter API
def connect_to_endpoint(url, headers, params, next_token=None):
    params['next_token'] = next_token  # params object received from create_url function
    response = requests.request("GET", url, headers=headers, params=params)
    if response.status_code != 200:
        raise Exception(response.status_code, response.text)
    return response.json()


# append function will be used to append tweets to csv file
def append_to_csv(json_response, fileName):
    # A counter variable
    counter = 0

    # Open OR create the target CSV file
    csvFile = open(fileName, "a", newline="", encoding='utf-8')
    csvWriter = csv.writer(csvFile)

    # Loop through each tweet
    for tweet in json_response['data']:
        # 1. Author ID
        author_id = tweet['author_id']

        # 2. Time created
        created_at = dateutil.parser.parse(tweet['created_at'])

        # 3. Tweet ID
        tweet_id = tweet['id']

        # 4. Tweet text
        text = tweet['text']

        # 5. id of journalist (not part of twitter object)
        journalist_id = journalist

        # Assemble all data in a list
        res = [author_id, created_at, tweet_id, text, journalist_id]

        # Append the result to the CSV file
        csvWriter.writerow(res)
        counter += 1

    # When done, close the CSV file
    csvFile.close()

    # Print the number of tweets for this iteration
    print("# of Tweets added from this response: ", counter)


########################################################################################################################
### T R E A T M E N T   G R O U P ###
########################################################################################################################
# import time intervals for treated journalists and add "T00:00:01.000Z" so it fits Twitters ISO 8601/RFC 3339 time
# format. By adding "T00:00:01.000Z" to the existing date I set the start of tweets extraction the second a new day
# starts
start_time_treat = pd.read_csv(r"")
for column in start_time_treat:
    start_time_treat[column] = [date + "T00:00:01.000Z" for date in
                                start_time_treat[column]]  # correct time so it fits twitters format
start_time_treat = start_time_treat.T.values.tolist()  # create a nested list of all start_times

# import time intervals for treated journalists and add "T23:59:59.000Z" so it fits Twitters ISO 8601/RFC 3339 time
# format. By adding "T23:59:59.000Z" to the existing date I set the end of tweets extraction a second before a
# new day starts
end_time_treat = pd.read_csv(r"")
for column in end_time_treat:
    end_time_treat[column] = [date + "T23:59:59.000Z" for date in
                              end_time_treat[column]]  # correct time so it fits twitters format
end_time_treat = end_time_treat.T.values.tolist()  # create a nested list of all end_times

# load lexisnexis treatment: data have been processed in Numbers by adding twitter handles for all 137 journalists
# that have been called out by Tucker more than 5 times within an episode.
single_treatment = pd.read_csv(
    r"",
    encoding='unicode_escape')

ids = single_treatment["treatment_id"]  # id unique to journalist
username_treat = single_treatment["twitter_handle"].tolist()  # list all journalist twitter handles
name_treat = single_treatment["names"].tolist()  # list all journalist names
date_treat = single_treatment["Date"].tolist()  # list date used for naming csv files i.e date of treatment

# make search_query looping over journalist names and usernames from list name_treat and username_treat
search_query_treat = []  # empty list to contain 137 search queries
for u, n in zip(username_treat, name_treat):
    query = ("(" + u + " " + "OR" + " " + '"' + n + '"' + ")" + " " + "-is:retweet")  # remove retweets
    search_query_treat.append(query)

# Calculate the total number of days I need data for the treatment group
tal = 0
for listElem in start_time_treat:
    tal += len(listElem)
print('Total Number of days for treatment group: ', tal)  # 1507

# 22
start_time_treat = start_time_treat[130:137]
end_time_treat = end_time_treat[130:137]
search_query_treat = search_query_treat[130:137]
username_treat = username_treat[130:137]
date_treat = date_treat[130:137]
ids = ids[130:137]

bearer_token = auth()
headers = create_headers(bearer_token)
max_results = 500  # number of tweets extracted per round

# Total number of tweets we collected from the loop
total_tweets = 0

# code extract tweets from 138 queries, start and end dates and saves each to a unique csv file
# named after the day of Tuckers show.
# for day, query, start, end in zip(date_treat, search_query_treat, start_time_treat, end_time_treat):
# for list in a:
# for number in list:
for suffixes_1, suffixes_2, name, user_handle, date, journalist in zip(start_time_treat, end_time_treat,
                                                                       search_query_treat, username_treat,
                                                                       date_treat, ids):
    for s1, s2 in zip(suffixes_1, suffixes_2):


        # create csv files named after date
        csvFile = open(date + "_" + user_handle + ".csv", "a", newline="", encoding='utf-8')
        csvWriter = csv.writer(csvFile)

        # create headers for the four variables: author_id, created_at, id, and tweet
        csvWriter.writerow(
            ['author_id', 'created_at', 'id', 'tweet', 'journalist_id'])
        csvFile.close()

        # Inputs
        count = 0  # Counting tweets per time period/journalist
        max_count = 3500  # Max tweets per time period/journalist

        flag = True
        next_token = None

        # add journalist id to the tweet object while looping over the list of ids in list ids
        #for item in json_response["data"]:
            #item["journalist_id"] = journalist

        # item in json_reponse" line below
        #if result_count == 0:  # added this continue statement in case result_count == 0
            #sleep(3.1)
            #continue

        # Check if flag is true
        while flag:

            # create url for tweet extraction based on for loop:
            # loop over 215 queries, start and end dates
            url = create_url(name, s1, s2, max_results)
            json_response = connect_to_endpoint(url[0], headers, url[1], next_token)
            result_count = json_response['meta']['result_count']  # Change: moved this line above the "for

            # Check if max_count reached
            if count >= max_count:
                break
            print("-------------------")
            print("Token: ", next_token)

            #if result_count == 0:  # added this continue statement in case result_count == 0
                #sleep(3.1)
                #continue

            if 'next_token' in json_response['meta']:
                #  Save the token to use for next call
                next_token = json_response['meta']['next_token']
                print("Next Token: ", next_token)
                if result_count is not None and result_count > 0 and next_token is not None:
                    print("Start Date: ", s1, "Name of journalist:", user_handle)
                    append_to_csv(json_response, date + "_" + user_handle + ".csv")
                    count += result_count
                    total_tweets += result_count
                    print("Total # of Tweets added: ", total_tweets)
                    print("-------------------")
                    sleep(4.1)
                    # If no next token exists
            else:
                if result_count is not None and result_count > 0:
                    print("-------------------")
                    print("Start Date: ", s1, "Name of journalist:", user_handle)
                    append_to_csv(json_response, date + "_" + user_handle + ".csv")
                    count += result_count
                    total_tweets += result_count
                    print("Total # of Tweets added: ", total_tweets)
                    print("-------------------")
                    sleep(4.1)

                    # Since this is the final request, turn flag to false to move to the next time period.
                flag = False
                next_token = None
            sleep(4.1)
print("Total number of results: ", total_tweets)

for suffixes_1, suffixes_2, name, user_handle, date, journalist in zip(start_time_treat, end_time_treat,
                                                                       search_query_treat, username_treat,
                                                                       date_treat, ids):
    for s1, s2 in zip(suffixes_1, suffixes_2):

        # Inputs
        count = 0  # Counting tweets per time period/journalist
        max_count = 3000  # Max tweets per time period/journalist

        flag = True
        next_token = None

        # create csv files named after date
        csvFile = open(date + "_" + user_handle + ".csv", "a", newline="", encoding='utf-8')
        csvWriter = csv.writer(csvFile)

        # create headers for the four variables: author_id, created_at, id, and tweet
        csvWriter.writerow(
            ['author_id', 'created_at', 'id', 'tweet', 'journalist_id'])
        csvFile.close()

        # create url for tweet extraction based on for loop:
        # loop over 215 queries, start and end dates
        url = create_url(name, s1, s2, max_results)
        json_response = connect_to_endpoint(url[0], headers, url[1], next_token)

        result_count = json_response['meta']['result_count']  # Change: moved this line above the "for
        # item in json_reponse" line below
        if result_count == 0:  # added this continue statement in case result_count == 0
            sleep(3.1)
            continue

        # add journalist id to the tweet object while looping over the list of ids in list ids
        for item in json_response["data"]: item["journalist_id"] = journalist

        # Check if flag is true
        while flag:

            # Check if max_count reached
            if count >= max_count:
                break
            print("-------------------")

            print("Start Date: ", s1, "Name of journalist:", user_handle)
            append_to_csv(json_response, date + "_" + user_handle + ".csv")
            count += result_count
            total_tweets += result_count
            print("Total # of Tweets added: ", total_tweets)
            print("-------------------")

            flag = False
            next_token = None
            sleep(3.1)

print("Total number of results: ", total_tweets)

########################################################################################################################
### U N I V E R S A L   L O O P :  J O U R N A L I S T   C A L L E D   O U T  ( C O N T R O L  G R O U P ) ###
########################################################################################################################
# load dataframe with control group periods and days. The data.frame contains 136 columns each representing a journalist
# in the order that they receive treatment. Therefore, the second treated journalist (1) will act as control for the
# shortest time and the last treated journalist(137) will act as control for the longest time
start_time_control = pd.read_csv(
    r"",
    encoding='unicode_escape')
start_time_control_ = start_time_control.apply(lambda x: pd.Series(x.dropna().values))
start_time_control = start_time_control.fillna('')

# add time to date object
for column in start_time_control:
    try:
        start_time_control[column] = [date + "T00:00:00.000Z" for date in start_time_control[column]]
    except:
        pass  # doing nothing on exception

# save data.frame with start time for control group
start_time_control.to_csv(
    r"",
    encoding='utf-8',
    index=False)

# load data.frame with start time for control group (manually removed "T00:00:01.000Z" in Numbers)
start_time_control = pd.read_csv(
    r"",
    encoding='unicode_escape')
start_time_control_ = start_time_control.apply(lambda x: pd.Series(x.dropna().values))
start_time_control = start_time_control.fillna('0')
start_time_control = start_time_control.T.values.tolist()  # create a nested list of all end_times
start_time_control = [[i for i in nested if i != '0'] for nested in start_time_control]

# I do the same to end time as has been done to start time
# import time intervals and add "T23:59:59.000Z" so it fits Twitters ISO 8601/RFC 3339 time format.
end_time_control = pd.read_csv(
    r"",
    encoding='unicode_escape')
end_time_control = end_time_control.apply(lambda x: pd.Series(x.dropna().values))
end_time_control = end_time_control.fillna('')

for column in end_time_control:
    try:
        end_time_control[column] = [date + "T23:59:59.000Z" for date in end_time_control[column]]
    except:
        pass  # doing nothing on exception

# save data.frame with end time for control group
end_time_control.to_csv(
    r"",
    encoding='utf-8',
    index=False)

# load data.frame with end time for control group (manually removed "T23:59:59.000Z" in Numbers)
end_time_control = pd.read_csv(
    r"",
    encoding='unicode_escape')
end_time_control = end_time_control.apply(lambda x: pd.Series(x.dropna().values))
end_time_control = end_time_control.fillna('0')
end_time_control = end_time_control.T.values.tolist()  # create a nested list of all end_times
end_time_control = [[i for i in nested if i != '0'] for nested in end_time_control]

# search queries for control group
# load data.frame with all names and twitter handles etc.
single_control = pd.read_csv(
    r"",
    encoding="unicode_escape")
control_names = single_control["names"]
control_handles = single_control["twitter_handle"]
control_names = control_names.T.values.tolist()
control_handles = control_handles.T.values.tolist()
# because the first element/journalist in the list (Acosta) is the first to be treated he will not be part of the
# control_group since the control_group consists of not-yet-treated journalists. Code below removes the first element
del control_names[0]
del control_handles[0]

# make search_query for the control group by looping over journalist twitter_handles that have not yet been treated
# results in 121 search queries
control_query = []  # empty list to contain search query for the control group
for name, handle in zip(control_names, control_handles):
    query = ("(" + name + " " + "OR" + " " + '"' + handle + '"' + ")" + " " + "-is:retweet")  # remove retweets
    control_query.append(query)

# CONTROL_INDEX
control_id_list = single_control["treatment_id"]
control_id_list = control_id_list.T.values.tolist()
del control_id_list[0]  # remove first treatment_id for Acosta (description above)

# count number of days/elements in start_time_control. The number will be the number of times the code will
# hit the Twitter API and with that number I can calculate the max_tweet per day and time the code will take
# to extract all tweets

# Iterate over the list and add the size of all internal lists
tal = 0
for listElem in start_time_control:
    tal += len(listElem)
print('Total Number of days : ', tal)  # 63134

# Inputs for tweets
bearer_token = auth()
headers = create_headers(bearer_token)
max_results = 100  # number of tweets extracted per round
keyword = control_query

# Total number of tweets we collected from the loop
total_tweets = 0

# code extract tweets from 234 queries, start and end dates and saves each to a unique csv file
# named after the day of Tuckers show.'
for suffixes_1, suffixes_2, query, journalist, user_handle in zip(start_time_control, end_time_control,
                                                                  control_query, control_id_list, control_handles):
    for s1, s2 in zip(suffixes_1, suffixes_2):
        # Inputs
        count = 0  # Counting tweets per time period/journalist
        max_count = 100  # Max tweets per time period/journalist

        flag = True
        next_token = None

        # create csv files named after date
        csvFile = open(user_handle + ".csv", "a", newline="", encoding='utf-8')
        csvWriter = csv.writer(csvFile)

        # create headers for the four variables: author_id, created_at, id, and tweet
        csvWriter.writerow(
            ['author_id', 'created_at', 'id', 'tweet', 'journalist_id'])
        csvFile.close()

        # create url for tweet extraction based on for loop:
        # loop over 215 queries, start and end dates
        url = create_url(query, s1, s2, max_results)
        json_response = connect_to_endpoint(url[0], headers, url[1], next_token)

        result_count = json_response['meta']['result_count']  # Change: moved this line above the "for
        # item in json_reponse" line below
        if result_count == 0:  # added this continue statement in case result_count == 0
            sleep(3.1)
            continue

        # add journalist id to the tweet object while looping over the list of ids in list ids
        for item in json_response["data"]: item["journalist_id"] = journalist

        # Check if flag is true
        while flag:

            # Check if max_count reached
            if count >= max_count:
                break
            print("-------------------")
            print("Token: ", next_token)

            if 'next_token' in json_response['meta']:
                #  Save the token to use for next call
                next_token = json_response['meta']['next_token']
                print("Next Token: ", next_token)
                if result_count is not None and result_count > 0 and next_token is not None:
                    print("Start Date: ", s1, "Name of journalist:", user_handle)
                    append_to_csv(json_response, user_handle + ".csv")
                    count += result_count
                    total_tweets += result_count
                    print("Total # of Tweets added: ", total_tweets)
                    print("-------------------")
                    sleep(3.1)
                    # If no next token exists
            else:
                if result_count is not None and result_count > 0:
                    print("-------------------")
                    print("Start Date: ", s1, "Name of journalist:", user_handle)
                    append_to_csv(json_response, user_handle + ".csv")
                    count += result_count
                    total_tweets += result_count
                    print("Total # of Tweets added: ", total_tweets)
                    print("-------------------")
                    sleep(3.1)

                    # Since this is the final request, turn flag to false to move to the next time period.
                    flag = False
                    next_token = None
                    sleep(3.1)
print("Total number of results: ", total_tweets)

# code broke with Dennis Prager so I restart the code with him index 39
# broke with Charlie Duff index 104
# broke with Aaron Mate index 124
# broke with David Frum index 127
# broke with Matthew Rosenberg index 129
# broke with Nina Jankowicz
start_time_control = start_time_control[135:136]
end_time_control = end_time_control[135:136]
control_query = control_query[135:136]
control_id_list = control_id_list[135:136]
control_handles = control_handles[135:136]

for suffixes_1, suffixes_2, ctr_query, journalist, user_handle in zip(start_time_control, end_time_control,
                                                                      control_query, control_id_list, control_handles):
    for s1, s2 in zip(suffixes_1, suffixes_2):

        # Inputs
        count = 0  # Counting tweets per time period/journalist
        max_count = 100  # Max tweets per time period/journalist

        flag = True
        next_token = None

        # create csv files named after date
        csvFile = open(user_handle + ".csv", "a", newline="", encoding='utf-8')
        csvWriter = csv.writer(csvFile)

        # create headers for the four variables: author_id, created_at, id, and tweet
        csvWriter.writerow(
            ['author_id', 'created_at', 'id', 'tweet', 'journalist_id'])
        csvFile.close()

        # create url for tweet extraction based on for loop:
        # loop over 215 queries, start and end dates
        url = create_url(ctr_query, s1, s2, max_results)
        json_response = connect_to_endpoint(url[0], headers, url[1], next_token)

        result_count = json_response['meta']['result_count']  # Change: moved this line above the "for
        # item in json_reponse" line below
        if result_count == 0:  # added this continue statement in case result_count == 0
            sleep(4)
            continue

        # add journalist id to the tweet object while looping over the list of ids in list ids
        for item in json_response["data"]: item["journalist_id"] = journalist

        # Check if flag is true
        while flag:

            # Check if max_count reached
            if count >= max_count:
                break
            print("-------------------")

            print("Start Date: ", s1, "Name of journalist:", user_handle)
            append_to_csv(json_response, user_handle + ".csv")
            count += result_count
            total_tweets += result_count
            print("Total # of Tweets added: ", total_tweets)
            print("-------------------")

            flag = False
            next_token = None
            sleep(4)

print("Total number of results: ", total_tweets)

########################################################################################################################
### T R E A T M E N T   G R O U P:  1 0  I N T E N S   T R E A T M E N T ###
########################################################################################################################
# import time intervals for treated journalists and add "T00:00:01.000Z" so it fits Twitters ISO 8601/RFC 3339 time
# format. By adding "T00:00:01.000Z" to the existing date I set the start of tweets extraction the second a new day
# starts
start_time_treat_10 = pd.read_csv(
    r"")
for column in start_time_treat_10:
    start_time_treat_10[column] = [date + "T00:00:01.000Z" for date in
                                   start_time_treat_10[column]]  # correct time so it fits twitters format
start_time_treat_10 = start_time_treat_10.T.values.tolist()  # create a nested list of all start_times

# import time intervals for treated journalists and add "T23:59:59.000Z" so it fits Twitters ISO 8601/RFC 3339 time
# format. By adding "T23:59:59.000Z" to the existing date I set the end of tweets extraction a second before a
# new day starts
end_time_treat_10 = pd.read_csv(
    r"")
for column in end_time_treat_10:
    end_time_treat_10[column] = [date + "T23:59:59.000Z" for date in
                                 end_time_treat_10[column]]  # correct time so it fits twitters format
end_time_treat_10 = end_time_treat_10.T.values.tolist()  # create a nested list of all end_times

# load lexisnexis treatment: data have been processed in Numbers by adding twitter handles for all 137 journalists
# that have been called out by Tucker more than 5 times within an episode.
single_treatment_10 = pd.read_csv(
    r"",
    encoding='unicode_escape')

ids_10 = single_treatment_10["treatment_id"]  # id unique to journalist
username_treat_10 = single_treatment_10["twitter_handle"].tolist()  # list all journalist twitter handles
name_treat_10 = single_treatment_10["names"].tolist()  # list all journalist names
date_treat_10 = single_treatment_10["Date"].tolist()  # list date used for naming csv files i.e date of treatment

# make search_query looping over journalist names and usernames from list name_treat and username_treat
search_query_treat_10 = []  # empty list to contain 137 search queries
for u, n in zip(username_treat_10, name_treat_10):
    query_10 = ("(" + u + " " + "OR" + " " + '"' + n + '"' + ")" + " " + "-is:retweet")  # remove retweets
    search_query_treat_10.append(query_10)

# Calculate the total number of days I need data for the treatment group
tal = 0
for listElem in start_time_treat_10:
    tal += len(listElem)
print('Total Number of days for treatment group: ', tal)  # 319

# Inputs for tweets
bearer_token = auth()
headers = create_headers(bearer_token)
max_results = 500  # number of tweets extracted per round

# Total number of tweets we collected from the loop
total_tweets = 0

for suffixes_1, suffixes_2, name, user_handle, date, journalist in zip(start_time_treat_10, end_time_treat_10,
                                                                       search_query_treat_10, username_treat_10,
                                                                       date_treat_10, ids_10):
    for s1, s2 in zip(suffixes_1, suffixes_2):

        # Inputs
        count = 0  # Counting tweets per time period/journalist
        max_count = 500  # Max tweets per time period/journalist

        flag = True
        next_token = None

        # create csv files named after date
        csvFile = open(date + "_" + user_handle + "_10" + ".csv", "a", newline="", encoding='utf-8')
        csvWriter = csv.writer(csvFile)

        # create headers for the four variables: author_id, created_at, id, and tweet
        csvWriter.writerow(
            ['author_id', 'created_at', 'id', 'tweet', 'journalist_id'])
        csvFile.close()

        # create url for tweet extraction based on for loop:
        # loop over 215 queries, start and end dates
        url = create_url(name, s1, s2, max_results)
        json_response = connect_to_endpoint(url[0], headers, url[1], next_token)

        result_count = json_response['meta']['result_count']  # Change: moved this line above the "for
        # item in json_reponse" line below
        if result_count == 0:  # added this continue statement in case result_count == 0
            sleep(3.1)
            continue

        # add journalist id to the tweet object while looping over the list of ids in list ids
        for item in json_response["data"]: item["journalist_id"] = journalist

        # Check if flag is true
        while flag:

            # Check if max_count reached
            if count >= max_count:
                break
            print("-------------------")

            print("Start Date: ", s1, "Name of journalist:", user_handle)
            append_to_csv(json_response, date + "_" + user_handle + "_10" + ".csv")
            count += result_count
            total_tweets += result_count
            print("Total # of Tweets added: ", total_tweets)
            print("-------------------")

            flag = False
            next_token = None
            sleep(3.1)

print("Total number of results: ", total_tweets)

########################################################################################################################
###  C O N T R O L  G R O U P: 1 0   I N T E N S   T R E A T M E N T ###
########################################################################################################################
# load dataframe with control group periods and days. The data.frame contains 137 columns each representing a journalist
# in the order that they receive treatment. Therefore, the second treated journalist (1) will act as control for the
# shortest time and the last treated journalist(137) will act as control for the longest time
start_time_control_10 = pd.read_csv(
    r"",
    encoding='unicode_escape')
start_time_control_10 = start_time_control_10.apply(lambda x: pd.Series(x.dropna().values))
start_time_control_10 = start_time_control_10.fillna('')

# add time to date object
for column in start_time_control_10:
    try:
        start_time_control_10[column] = [date + "T00:00:00.000Z" for date in start_time_control_10[column]]
    except:
        pass  # doing nothing on exception

# save data.frame with start time for control group
start_time_control_10.to_csv(
    r"",
    encoding='utf-8',
    index=False)

# load data.frame with start time for control group (manually removed "T00:00:00.000Z" in Numbers)
start_time_control_10 = pd.read_csv(
    r"",
    encoding='unicode_escape')
start_time_control_10 = start_time_control_10.apply(lambda x: pd.Series(x.dropna().values))
start_time_control_10 = start_time_control_10.fillna('0')
start_time_control_10 = start_time_control_10.T.values.tolist()  # create a nested list of all end_times
start_time_control_10 = [[i for i in nested if i != '0'] for nested in start_time_control_10]

# I do the same to end time as has been done to start time
# import time intervals and add "T23:59:59.000Z" so it fits Twitters ISO 8601/RFC 3339 time format.
end_time_control_10 = pd.read_csv(
    r"",
    encoding='unicode_escape')
end_time_control_10 = end_time_control_10.apply(lambda x: pd.Series(x.dropna().values))
end_time_control_10 = end_time_control_10.fillna('')

for column in end_time_control_10:
    try:
        end_time_control_10[column] = [date + "T23:59:59.000Z" for date in end_time_control_10[column]]
    except:
        pass  # doing nothing on exception

# save data.frame with end time for control group
end_time_control_10.to_csv(
    r"",
    encoding='utf-8',
    index=False)

# load data.frame with end time for control group (manually removed "T23:59:59.000Z" in Numbers)
end_time_control_10 = pd.read_csv(
    r"",
    encoding='unicode_escape')
end_time_control_10 = end_time_control_10.apply(lambda x: pd.Series(x.dropna().values))
end_time_control_10 = end_time_control_10.fillna('0')
end_time_control_10 = end_time_control_10.T.values.tolist()  # create a nested list of all end_times
end_time_control_10 = [[i for i in nested if i != '0'] for nested in end_time_control_10]

# search queries for control group
# load data.frame with all names and twitter handles etc.
single_control_10 = pd.read_csv(
    r"",
    encoding="unicode_escape")
control_names_10 = single_control_10["names"]
control_handles_10 = single_control_10["twitter_handle"]
control_names_10 = control_names_10.T.values.tolist()
control_handles_10 = control_handles_10.T.values.tolist()
# because the first element/journalist in the list (Acosta) is the first to be treated he will not be part of the
# control_group since the control_group consists of not-yet-treated journalists. Code below removes the first element
del control_names_10[0]
del control_handles_10[0]

# make search_query for the control group by looping over journalist twitter_handles that have not yet been treated
# results in 121 search queries
control_query_10 = []  # empty list to contain search query for the control group
for name, handle in zip(control_names_10, control_handles_10):
    query_10 = ("(" + name + " " + "OR" + " " + '"' + handle + '"' + ")" + " " + "-is:retweet")  # remove retweets
    control_query_10.append(query_10)

# CONTROL_INDEX
control_id_list_10 = single_control_10["treatment_id"]
control_id_list_10 = control_id_list_10.T.values.tolist()
del control_id_list_10[0]  # remove first treatment_id for Acosta (description above)

# count number of days/elements in start_time_control. The number will be the number of times the code will
# hit the Twitter API and with that number I can calculate the max_tweet per day and time the code will take
# to extract all tweets

# Iterate over the list and add the size of all internal lists
tal = 0
for listElem in start_time_control_10:
    tal += len(listElem)
print('Total Number of days : ', tal)  # 4551 days meaning collecting tweet should take approx. 5 hours

# Inputs for tweets
bearer_token = auth()
headers = create_headers(bearer_token)
max_results = 100  # number of tweets extracted per round
keyword = control_query_10

# Total number of tweets we collected from the loop
total_tweets = 0

for suffixes_1, suffixes_2, ctr_query, journalist, user_handle in zip(start_time_control_10, end_time_control_10,
                                                                      control_query_10, control_id_list_10,
                                                                      control_handles_10):
    for s1, s2 in zip(suffixes_1, suffixes_2):

        # Inputs
        count = 0  # Counting tweets per time period/journalist
        max_count = 100  # Max tweets per time period/journalist

        flag = True
        next_token = None

        # create csv files named after date
        csvFile = open(user_handle + "_10" + ".csv", "a", newline="", encoding='utf-8')
        csvWriter = csv.writer(csvFile)

        # create headers for the four variables: author_id, created_at, id, and tweet
        csvWriter.writerow(
            ['author_id', 'created_at', 'id', 'tweet', 'journalist_id'])
        csvFile.close()

        # create url for tweet extraction based on for loop:
        # loop over 215 queries, start and end dates
        url = create_url(ctr_query, s1, s2, max_results)
        json_response = connect_to_endpoint(url[0], headers, url[1], next_token)

        result_count = json_response['meta']['result_count']  # Change: moved this line above the "for
        # item in json_reponse" line below
        if result_count == 0:  # added this continue statement in case result_count == 0
            sleep(4)
            continue

        # add journalist id to the tweet object while looping over the list of ids in list ids
        for item in json_response["data"]: item["journalist_id"] = journalist

        # Check if flag is true
        while flag:

            # Check if max_count reached
            if count >= max_count:
                break
            print("-------------------")

            print("Start Date: ", s1, "Name of journalist:", user_handle)
            append_to_csv(json_response, user_handle + "_10" + ".csv")
            count += result_count
            total_tweets += result_count
            print("Total # of Tweets added: ", total_tweets)
            print("-------------------")

            flag = False
            next_token = None
            sleep(4)

print("Total number of results: ", total_tweets)

########################################################################################################################
### T R E A T M E N T   G R O U P:  8  I N T E N S   T R E A T M E N T ###
########################################################################################################################
# import time intervals for treated journalists and add "T00:00:01.000Z" so it fits Twitters ISO 8601/RFC 3339 time
# format. By adding "T00:00:01.000Z" to the existing date I set the start of tweets extraction the second a new day
# starts
start_time_treat_10 = pd.read_csv(
    r"")
for column in start_time_treat_10:
    start_time_treat_10[column] = [date + "T00:00:01.000Z" for date in
                                   start_time_treat_10[column]]  # correct time so it fits twitters format
start_time_treat_10 = start_time_treat_10.T.values.tolist()  # create a nested list of all start_times

# import time intervals for treated journalists and add "T23:59:59.000Z" so it fits Twitters ISO 8601/RFC 3339 time
# format. By adding "T23:59:59.000Z" to the existing date I set the end of tweets extraction a second before a
# new day starts
end_time_treat_10 = pd.read_csv(
    r"")
for column in end_time_treat_10:
    end_time_treat_10[column] = [date + "T23:59:59.000Z" for date in
                                 end_time_treat_10[column]]  # correct time so it fits twitters format
end_time_treat_10 = end_time_treat_10.T.values.tolist()  # create a nested list of all end_times

# load lexisnexis treatment: data have been processed in Numbers by adding twitter handles for all 137 journalists
# that have been called out by Tucker more than 5 times within an episode.
single_treatment_10 = pd.read_csv(
    r"",
    encoding='unicode_escape')

ids_10 = single_treatment_10["treatment_id"]  # id unique to journalist
username_treat_10 = single_treatment_10["twitter_handle"].tolist()  # list all journalist twitter handles
name_treat_10 = single_treatment_10["names"].tolist()  # list all journalist names
date_treat_10 = single_treatment_10["Date"].tolist()  # list date used for naming csv files i.e date of treatment

# make search_query looping over journalist names and usernames from list name_treat and username_treat
search_query_treat_10 = []  # empty list to contain 137 search queries
for u, n in zip(username_treat_10, name_treat_10):
    query_10 = ("(" + u + " " + "OR" + " " + '"' + n + '"' + ")" + " " + "-is:retweet")  # remove retweets
    search_query_treat_10.append(query_10)

# Calculate the total number of days I need data for the treatment group
tal = 0
for listElem in start_time_treat_10:
    tal += len(listElem)
print('Total Number of days for treatment group: ', tal)  # 319

# Inputs for tweets
bearer_token = auth()
headers = create_headers(bearer_token)
max_results = 500  # number of tweets extracted per round

# Total number of tweets we collected from the loop
total_tweets = 0

for suffixes_1, suffixes_2, name, user_handle, date, journalist in zip(start_time_treat_10, end_time_treat_10,
                                                                       search_query_treat_10, username_treat_10,
                                                                       date_treat_10, ids_10):
    for s1, s2 in zip(suffixes_1, suffixes_2):

        # Inputs
        count = 0  # Counting tweets per time period/journalist
        max_count = 500  # Max tweets per time period/journalist

        flag = True
        next_token = None

        # create csv files named after date
        csvFile = open(date + "_" + user_handle + "_10" + ".csv", "a", newline="", encoding='utf-8')
        csvWriter = csv.writer(csvFile)

        # create headers for the four variables: author_id, created_at, id, and tweet
        csvWriter.writerow(
            ['author_id', 'created_at', 'id', 'tweet', 'journalist_id'])
        csvFile.close()

        # create url for tweet extraction based on for loop:
        # loop over 215 queries, start and end dates
        url = create_url(name, s1, s2, max_results)
        json_response = connect_to_endpoint(url[0], headers, url[1], next_token)

        result_count = json_response['meta']['result_count']  # Change: moved this line above the "for
        # item in json_reponse" line below
        if result_count == 0:  # added this continue statement in case result_count == 0
            sleep(3.1)
            continue

        # add journalist id to the tweet object while looping over the list of ids in list ids
        for item in json_response["data"]: item["journalist_id"] = journalist

        # Check if flag is true
        while flag:

            # Check if max_count reached
            if count >= max_count:
                break
            print("-------------------")

            print("Start Date: ", s1, "Name of journalist:", user_handle)
            append_to_csv(json_response, date + "_" + user_handle + "_10" + ".csv")
            count += result_count
            total_tweets += result_count
            print("Total # of Tweets added: ", total_tweets)
            print("-------------------")

            flag = False
            next_token = None
            sleep(3.1)

print("Total number of results: ", total_tweets)

########################################################################################################################
###  C O N T R O L  G R O U P:  8  I N T E N S   T R E A T M E N T ###
########################################################################################################################
# load dataframe with control group periods and days. The data.frame contains 137 columns each representing a journalist
# in the order that they receive treatment. Therefore, the second treated journalist (1) will act as control for the
# shortest time and the last treated journalist(137) will act as control for the longest time
start_time_control_10 = pd.read_csv(
    r"",
    encoding='unicode_escape')
start_time_control_10 = start_time_control_10.apply(lambda x: pd.Series(x.dropna().values))
start_time_control_10 = start_time_control_10.fillna('')

# add time to date object
for column in start_time_control_10:
    try:
        start_time_control_10[column] = [date + "T00:00:00.000Z" for date in start_time_control_10[column]]
    except:
        pass  # doing nothing on exception

# save data.frame with start time for control group
start_time_control_10.to_csv(
    r"",
    encoding='utf-8',
    index=False)

# load data.frame with start time for control group (manually removed "T00:00:00.000Z" in Numbers)
start_time_control_10 = pd.read_csv(
    r"",
    encoding='unicode_escape')
start_time_control_10 = start_time_control_10.apply(lambda x: pd.Series(x.dropna().values))
start_time_control_10 = start_time_control_10.fillna('0')
start_time_control_10 = start_time_control_10.T.values.tolist()  # create a nested list of all end_times
start_time_control_10 = [[i for i in nested if i != '0'] for nested in start_time_control_10]

# I do the same to end time as has been done to start time
# import time intervals and add "T23:59:59.000Z" so it fits Twitters ISO 8601/RFC 3339 time format.
end_time_control_10 = pd.read_csv(
    r"",
    encoding='unicode_escape')
end_time_control_10 = end_time_control_10.apply(lambda x: pd.Series(x.dropna().values))
end_time_control_10 = end_time_control_10.fillna('')

for column in end_time_control_10:
    try:
        end_time_control_10[column] = [date + "T23:59:59.000Z" for date in end_time_control_10[column]]
    except:
        pass  # doing nothing on exception

# save data.frame with end time for control group
end_time_control_10.to_csv(
    r"",
    encoding='utf-8',
    index=False)

# load data.frame with end time for control group (manually removed "T23:59:59.000Z" in Numbers)
end_time_control_10 = pd.read_csv(
    r"",
    encoding='unicode_escape')
end_time_control_10 = end_time_control_10.apply(lambda x: pd.Series(x.dropna().values))
end_time_control_10 = end_time_control_10.fillna('0')
end_time_control_10 = end_time_control_10.T.values.tolist()  # create a nested list of all end_times
end_time_control_10 = [[i for i in nested if i != '0'] for nested in end_time_control_10]

# search queries for control group
# load data.frame with all names and twitter handles etc.
single_control_10 = pd.read_csv(
    r"",
    encoding="unicode_escape")
control_names_10 = single_control_10["names"]
control_handles_10 = single_control_10["twitter_handle"]
control_names_10 = control_names_10.T.values.tolist()
control_handles_10 = control_handles_10.T.values.tolist()
# because the first element/journalist in the list (Acosta) is the first to be treated he will not be part of the
# control_group since the control_group consists of not-yet-treated journalists. Code below removes the first element
del control_names_10[0]
del control_handles_10[0]

# make search_query for the control group by looping over journalist twitter_handles that have not yet been treated
# results in 121 search queries
control_query_10 = []  # empty list to contain search query for the control group
for name, handle in zip(control_names_10, control_handles_10):
    query_10 = ("(" + name + " " + "OR" + " " + '"' + handle + '"' + ")" + " " + "-is:retweet")  # remove retweets
    control_query_10.append(query_10)

# CONTROL_INDEX
control_id_list_10 = single_control_10["treatment_id"]
control_id_list_10 = control_id_list_10.T.values.tolist()
del control_id_list_10[0]  # remove first treatment_id for Acosta (description above)

# count number of days/elements in start_time_control. The number will be the number of times the code will
# hit the Twitter API and with that number I can calculate the max_tweet per day and time the code will take
# to extract all tweets

# Iterate over the list and add the size of all internal lists
tal = 0
for listElem in start_time_control_10:
    tal += len(listElem)
print('Total Number of days : ', tal)  # 4551 days meaning collecting tweet should take approx. 5 hours

# Inputs for tweets
bearer_token = auth()
headers = create_headers(bearer_token)
max_results = 100  # number of tweets extracted per round
keyword = control_query_10

# Total number of tweets we collected from the loop
total_tweets = 0

for suffixes_1, suffixes_2, ctr_query, journalist, user_handle in zip(start_time_control_10, end_time_control_10,
                                                                      control_query_10, control_id_list_10,
                                                                      control_handles_10):
    for s1, s2 in zip(suffixes_1, suffixes_2):

        # Inputs
        count = 0  # Counting tweets per time period/journalist
        max_count = 100  # Max tweets per time period/journalist

        flag = True
        next_token = None

        # create csv files named after date
        csvFile = open(user_handle + "_10" + ".csv", "a", newline="", encoding='utf-8')
        csvWriter = csv.writer(csvFile)

        # create headers for the four variables: author_id, created_at, id, and tweet
        csvWriter.writerow(
            ['author_id', 'created_at', 'id', 'tweet', 'journalist_id'])
        csvFile.close()

        # create url for tweet extraction based on for loop:
        # loop over 215 queries, start and end dates
        url = create_url(ctr_query, s1, s2, max_results)
        json_response = connect_to_endpoint(url[0], headers, url[1], next_token)

        result_count = json_response['meta']['result_count']  # Change: moved this line above the "for
        # item in json_reponse" line below
        if result_count == 0:  # added this continue statement in case result_count == 0
            sleep(4)
            continue

        # add journalist id to the tweet object while looping over the list of ids in list ids
        for item in json_response["data"]: item["journalist_id"] = journalist

        # Check if flag is true
        while flag:

            # Check if max_count reached
            if count >= max_count:
                break
            print("-------------------")

            print("Start Date: ", s1, "Name of journalist:", user_handle)
            append_to_csv(json_response, user_handle + "_10" + ".csv")
            count += result_count
            total_tweets += result_count
            print("Total # of Tweets added: ", total_tweets)
            print("-------------------")

            flag = False
            next_token = None
            sleep(4)

print("Total number of results: ", total_tweets)

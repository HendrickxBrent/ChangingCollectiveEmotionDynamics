# -*- coding: utf-8 -*-
"""
Created on Sun Jul 10 20:58:35 2022

@author: Brent
"""

import csv
import pandas as pd
import glob
import os



import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.feature_extraction.text import CountVectorizer
import nltk 
import string
import re





os.chdir("C:\\Users\\Brent\\Documents\\Psychology\\KUL\\Psychologie\\thesis\\Twitter\\January\\weeks")




data = pd.read_csv(r'allApril.csv')



df2 = data


###### links + mentions    (NOT punctuation)
############################################

def remove(text):
    #text = re.sub('[0-9]+', '', text)           # numbers
    text = re.sub(r"http\S+", "", text)         #links
    text = re.sub(r"www.\S+", "", text)         #links
    text = re.sub("@[A-Za-z0-9_]+","", text)    #remove @mentions
    
    #words
    text = re.sub(r"like", "", text)
    text = re.sub(r"good", "", text)
    text = re.sub(r"lol", "", text)
    text = re.sub(r"love", "", text)
    text = re.sub(r"better", "", text)
    text = re.sub(r"well", "", text)
    text = re.sub(r"care", "", text)
    

    return text

df2['Tweet'] = df2['text'].apply(lambda x: remove(x))

df2.head(20) #yes


###### lowercase
##############################

df2['Tweet'] = df2['Tweet'].str.lower()
df2.head(20)







#####################################################################
## create column to rank data per month, day and hour
#####################################################################

# create new column
df2["datecode"] = np.nan

from datetime import datetime, time

## make columns that indicate the month/day/hour the tweet was posted
df2['Month'] = pd.DatetimeIndex(df2['created_at']).month
df2['Hour'] = pd.DatetimeIndex(df2['created_at']).hour   # '10' = a tweet posted between 10 and 11
df2['Day'] = pd.DatetimeIndex(df2['created_at']).day
df2['Minute'] = pd.DatetimeIndex(df2['created_at']).minute
df2['Second'] = pd.DatetimeIndex(df2['created_at']).second


df2['procentscraped'] = ((3600-(df2['Minute']*60+df2['Second']))/3600)      ## how far in the hour is this tweet scraped -> needed to calculate probability sampling (look for every hour to the smallest value)

## make sure days/hours with less than one digit gets a '0' in front of it
df2['Day'] = df2['Day'].apply(lambda x: '{0:0>2}'.format(x))
df2['Hour'] = df2['Hour'].apply(lambda x: '{0:0>2}'.format(x))

## combine month/day/hour into one variable
## for example: "30210" is a tweet posted in March the second between 10-11
df2["datecode"] = df2['Month'].map(str) + df2['Day'].map(str) + df2['Hour'].map(str)


### rank the data, this is the column to be used to make the time series
df2["Rank"] = df2["datecode"].rank(method='dense')


#os.chdir("C:\\Users\\Brent\\Documents\\Psychology\\KUL\\Psychologie\\thesis\\Twitter\\2019\\allmarch")







#df2.to_csv('Feb2019Clean.csv')

df2.to_csv('MaiClean.csv')


#####################################################################
#####################################################################
## 
##           go to LIWC   &   come back with new file
##
#####################################################################
#####################################################################


os.chdir("C:\\Users\\Brent\\Documents\\Psychology\\KUL\\Psychologie\\thesis\\Twitter\\Mai")


# dt = pd.read_csv('Feb2019Clean.csv')
dt = pd.read_csv('MaiCleanLIWC.csv')
#dt = pd.read_csv('April2020Clean.csv')
dt = pd.read_csv('Jan2020CleanLIWC.csv')
dt = pd.read_csv('April2020CleanLIWC.csv')
dt = pd.read_csv('missingCleanLIWC.csv')
df_new = df2[['Index', 'author.id','created at','Tweet']]


#dt = df2
df2 = dt
small = dt.head(5000000)
example = dt.head(3000000)

small = df2.head(1000000)
df2 = small
df2 = df2[['author.id','Tweet']]

##          word count COVID
####################################################

from nltk.tokenize import TweetTokenizer
def process_tweet(Tweet):  #### it works well enough
    tokenizer = TweetTokenizer(preserve_case=False, strip_handles=True, reduce_len=True)
    tweet_tokens = tokenizer.tokenize(Tweet)
    count = 0
    covid = 'covid'
    coronavirus = 'coronavirus'
    corona = 'corona'
    virus = 'virus'
    for word in tweet_tokens:
        if (covid in tweet_tokens):
            count = count + 1
        if (coronavirus in tweet_tokens):
            count = count + 1
        if ((corona and virus) in tweet_tokens):
            count = count + 1
    return count



    
    

df2['COVID'] = df2['Tweet'].apply(lambda x: process_tweet(x))
df = df2['COVID'].value_counts()
df = pd.value_counts(df2['COVID']).to_frame().reset_index()
print(df)
pd.crosstab(index=df['COVID'], columns='index')

df2['COVID'] = df2['COVID']> 0
df2['COVID'].astype('bool')
df2['COVID'] = df2['COVID'].astype(int) 



head = df2.head(20)
head





new = example.groupby('datecode').agg('sum')

whole = df2.groupby('datecode').agg('sum')


new




## save file

whole.to_csv('Mai2020Rready.csv')


from nltk.tokenize import TweetTokenizer
def process_tweet(Tweet):  #### it works well enough
    #tokenizer = TweetTokenizer(preserve_case=False, strip_handles=True, reduce_len=True)
    #tweet_tokens = tokenizer.tokenize(Tweet)
    count = 0
    covid = 'covid'
    coronavirus = 'coronavirus'
    corona = 'corona'
    virus = 'virus'
    for word in tweet_tokens:
        if (covid in tweet_tokens):
            count = count + 1
        if (coronavirus in tweet_tokens):
            count = count + 1
        if ((corona and virus) in tweet_tokens):
            count = count + 1
    return count




#####################################################################
#####################################################################
## 
##           cases data
##
#####################################################################
#####################################################################


os.chdir("C:\\Users\\Brent\\Documents\\Psychology\\KUL\\Psychologie\\thesis\\Twitter\\All2020")


cases = pd.read_csv('data_2022-Jul-13.csv')


cases0 = cases[['date','cumFirstEpisodesBySpecimenDate','newFirstEpisodesBySpecimenDate']].copy()

casesfebapril = cases0.iloc[734:840]

casesfebapril.to_csv('cases.csv')


# data_2022-Jul-13




#####################################################################
#####################################################################
## 
##           text
##
#####################################################################
#####################################################################


os.chdir("C:\\Users\\Brent\\Documents\\Psychology\\KUL\\Psychologie\\thesis\\Twitter\\Mai")

cases = pd.read_csv('MaiClean.csv')




cases0 = cases.rename(columns={cases.columns[3]: 'time'})
cases1 = cases0.head(20)
df2 = cases0[['Tweet','time']]

df3 = df2.head(10000)


##          word count COVID
####################################################






df2['COVID'] = df2['Tweet'].str.contains('covid')
df2['coronavirus'] = df2['Tweet'].str.contains('coronavirus')
df2['corona'] = df2['Tweet'].str.contains('corona')
df2['virus'] = df2['Tweet'].str.contains('virus')
df2['lockdown'] = df2['Tweet'].str.contains('lockdown')
df2['pandemic'] = df2['Tweet'].str.contains('pandemic')
df2['epidemic'] = df2['Tweet'].str.contains('epidemic')
df2['outbreak'] = df2['Tweet'].str.contains('outbreak')
df2['quarantine'] = df2['Tweet'].str.contains('quarantine')
df2['social'] = df2['Tweet'].str.contains('social')
df2['distance'] = df2['Tweet'].str.contains('distance')
df2['infected'] = df2['Tweet'].str.contains('infected')
df2['cases'] = df2['Tweet'].str.contains('cases')


df2['COVID'] = df2['COVID']> 0
df2['COVID'] = df2['COVID'] > 0
df2['coronavirus'] = df2['coronavirus']> 0
df2['corona'] = df2['corona']> 0 
df2['virus'] =df2['virus']> 0
df2['lockdown'] = df2['lockdown']> 0
df2['pandemic'] = df2['pandemic']> 0
df2['epidemic'] = df2['epidemic']> 0
df2['outbreak'] = df2['outbreak']> 0
df2['quarantine'] = df2['quarantine']> 0
df2['social'] = df2['social']> 0 
df2['distance'] =df2['distance']> 0
df2['infected'] = df2['infected']> 0
df2['cases'] = df2['cases']> 0

df2['COVID'].astype('bool')
df2['coronavirus'].astype('bool')
df2['corona'].astype('bool')
df2['virus'].astype('bool')
df2['lockdown'].astype('bool')
df2['pandemic'].astype('bool')
df2['epidemic'].astype('bool')
df2['outbreak'].astype('bool')
df2['quarantine'].astype('bool')
df2['social'].astype('bool')
df2['distance'].astype('bool')
df2['infected'].astype('bool')
df2['cases'].astype('bool')

df2['COVID'] = df2['COVID'].astype(int) 
df2['coronavirus'] = df2['coronavirus'].astype(int) 
df2['corona'] = df2['corona'].astype(int) 
df2['virus'] =df2['virus'].astype(int) 
df2['lockdown'] = df2['lockdown'].astype(int) 
df2['pandemic'] = df2['pandemic'].astype(int) 
df2['epidemic'] = df2['epidemic'].astype(int) 
df2['outbreak'] = df2['outbreak'].astype(int) 
df2['quarantine'] = df2['quarantine'].astype(int) 
df2['social'] = df2['social'].astype(int) 
df2['distance'] =df2['distance'].astype(int) 
df2['infected'] = df2['infected'].astype(int) 
df2['cases'] = df2['cases'].astype(int) 

df3 = df2.head(10000)

df2.to_csv('MaiCOVID.csv')


################ got to r
####################################################

os.chdir("C:\\Users\\Brent\\Documents\\Psychology\\KUL\\Psychologie\\thesis\\Twitter\\All2020\\figures")

cases = pd.read_csv('MaiCOVIDtoP.csv')
head = cases.head(10)

df2 = cases
head = df2.head(10)

df2["datecode"] = np.nan

from datetime import datetime, time

## make columns that indicate the month/day/hour the tweet was posted
df2['Month'] = pd.DatetimeIndex(df2['time']).month
df2['Hour'] = pd.DatetimeIndex(df2['time']).hour   # '10' = a tweet posted between 10 and 11
df2['Day'] = pd.DatetimeIndex(df2['time']).day
df2['Minute'] = pd.DatetimeIndex(df2['time']).minute
df2['Second'] = pd.DatetimeIndex(df2['time']).second


df2['procentscraped'] = ((3600-(df2['Minute']*60+df2['Second']))/3600)      ## how far in the hour is this tweet scraped -> needed to calculate probability sampling (look for every hour to the smallest value)

## make sure days/hours with less than one digit gets a '0' in front of it
df2['Day'] = df2['Day'].apply(lambda x: '{0:0>2}'.format(x))
df2['Hour'] = df2['Hour'].apply(lambda x: '{0:0>2}'.format(x))

## combine month/day/hour into one variable
## for example: "30210" is a tweet posted in March the second between 10-11
df2["datecode"] = df2['Month'].map(str) + df2['Day'].map(str) + df2['Hour'].map(str)

whole = df2.groupby('datecode').agg('sum')
## save file

whole.to_csv('MaiCOVIDRready.csv')




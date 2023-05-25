# -*- coding: utf-8 -*-
"""
Created on Tue Jan 12 13:22:01 2021

@author: Will
"""

import os
import glob
import sys
import pandas as pd
import numpy as np
import requests
import random
import time
import json
import re
from scrapy import Selector
from datetime import datetime
import ast
from IMDB_Functions import *

##################
## Instructions ##
##################

#1. Assign the category alias to filter data
cat_alias_filter = ["BEST FILM","BEST LEAD ACTOR","BEST LEAD ACTRESS",
                    "BEST SUPPORTING ACTOR", "BEST SUPPORTING ACTRESS",
                    "BEST DIRECTOR", "BEST ORIGINAL SCRIPT", "BEST ADAPTED SCRIPT",
                    "BEST SCRIPT", "BEST CINEMATOGRAPHY", "BEST FILM EDITING",
                    "BEST PRODUCTION DESIGN", "BEST COSTUME", "BEST MAKEUP AND HAIR",
                    "BEST MUSIC", "BEST SONG", "BEST SOUND MIXING", "BEST SOUND EDITING",
                    "BEST SOUND", "BEST VFX", "BEST DOCUMENTARY", "BEST ANIMATION",
                    "BEST FOREIGN FILM"]
#cat_alias_filter = ["BEST FILM","BEST LEAD ACTOR","BEST LEAD ACTRESS",
#                    "BEST DIRECTOR", "BEST ORIGINAL SCRIPT", "BEST ADAPTED SCRIPT",
#                    "BEST SCRIPT", "BEST FILM EDITING"]
#cat_alias_filter = ["BEST ORIGINAL SCRIPT", "BEST SCRIPT"]
cat_alias_filter = ["BEST SOUND MIXING", "BEST SOUND"]

filename_strip = 'Best_SoundMixing'
event_name = 'Oscars'
#event_name = 'GoldenGlobes'

########################
## Reading Data Files ##
########################

DF_DIR = 'C:\\Users\\Will\\Documents\\Data Science\\Projeto Tree'
all_files = glob.glob(DF_DIR + "/Tree_Oscar*.csv")

li = []

for filename in all_files:
    df = pd.read_csv(filename, index_col=None, header=0)
    li.append(df)

IMDB_DATA = pd.concat(li, axis=0, ignore_index=True)
IMDB_DATA.drop('Unnamed: 0',axis=1, inplace=True)

#Converting these columns from String to List
IMDB_DATA.P_NOMINEES = IMDB_DATA.P_NOMINEES.apply(ast.literal_eval)
IMDB_DATA.S_NOMINEES = IMDB_DATA.S_NOMINEES.apply(ast.literal_eval)

####################
## Data Wrangling ##
####################

IMDB_DATA.columns
IMDB_DATA.CATEGORY_GROUP.unique()

#Adding new columns to represent the number of nominees
IMDB_DATA['LEN_P_NOMINEES'] = IMDB_DATA.P_NOMINEES.apply(len)
IMDB_DATA['LEN_S_NOMINEES'] = IMDB_DATA.S_NOMINEES.apply(len)

#Filter all the rows related to Film Editing Categories
FILM_DATA = IMDB_DATA[IMDB_DATA.CATEGORY_ALIAS.isin(cat_alias_filter)]

#Check which rows have more than 1 nominee
FILM_DATA[(FILM_DATA['LEN_P_NOMINEES']>1)]
FILM_DATA[(FILM_DATA['LEN_S_NOMINEES']>1)]
FILM_DATA[(FILM_DATA['LEN_S_NOMINEES']>1) & (FILM_DATA['LEN_P_NOMINEES']>1)]

FILM_DATA.CATEGORY.unique()

#Filter all the rows from last 20 years
FILM_DATA = FILM_DATA[FILM_DATA.YEAR > 2000]
FILM_DATA.CATEGORY.unique()
FILM_DATA.AWARD.unique()


#######################################
## Section to Filter Specific Awards ##
#######################################

#Filter only the rows related to Film Awards
#awards_selection = []

#FILM_DATA = FILM_DATA[FILM_DATA.AWARD.isin(awards_selection)]


#One row per nominee. Note that "Names" and "Titles" will be in different rows
NOMINEES = FILM_DATA.explode('P_NOMINEES')
NOMINEES = NOMINEES.explode('S_NOMINEES')

#Delete rows without primary nominees
NOMINEES = NOMINEES[(NOMINEES.LEN_P_NOMINEES!=0)]

#Retrieve the IMDB ID from the Primary Nominee data
NOMINEES['P_IMDB'] = NOMINEES.P_NOMINEES.apply(lambda x: x['const'])
NOMINEES['P_NAME'] = NOMINEES.P_NOMINEES.apply(lambda x: x['name'])
#Since the Secondary Nominee can be empty, additional lines are required
empty_s_nominees = ~(NOMINEES.LEN_S_NOMINEES!=0)
NOMINEES.loc[empty_s_nominees,'S_NOMINEES'] = 0
NOMINEES['S_IMDB'] = NOMINEES.S_NOMINEES.apply(lambda x: x['const'] if x!=0 else '')
NOMINEES['S_NAME'] = NOMINEES.S_NOMINEES.apply(lambda x: x['name'] if x!=0 else '')

#Creating Title and Person columns
NOMINEES['TITLE'] = pd.Series(dtype=object)
NOMINEES['PERSON'] = pd.Series(dtype=object)
NOMINEES['TITLE_ID'] = pd.Series(dtype=object)
NOMINEES['PERSON_ID'] = pd.Series(dtype=object)

#Assigning the correct values to TITLE and PERSON
p_nominee_is_title = NOMINEES['P_IMDB'].str.contains('tt')
p_nominee_is_name = NOMINEES['P_IMDB'].str.contains('nm')
NOMINEES.loc[p_nominee_is_title,'TITLE'] = NOMINEES[p_nominee_is_title].P_NAME
NOMINEES.loc[p_nominee_is_name,'PERSON'] = NOMINEES[p_nominee_is_name].P_NAME
NOMINEES.loc[p_nominee_is_title,'TITLE_ID'] = NOMINEES[p_nominee_is_title].P_IMDB
NOMINEES.loc[p_nominee_is_name,'PERSON_ID'] = NOMINEES[p_nominee_is_name].P_IMDB

s_nominee_is_title = NOMINEES['S_IMDB'].str.contains('tt')
s_nominee_is_name = NOMINEES['S_IMDB'].str.contains('nm')
NOMINEES.loc[s_nominee_is_title,'TITLE'] = NOMINEES[s_nominee_is_title].S_NAME
NOMINEES.loc[s_nominee_is_name,'PERSON'] = NOMINEES[s_nominee_is_name].S_NAME
NOMINEES.loc[s_nominee_is_title,'TITLE_ID'] = NOMINEES[s_nominee_is_title].S_IMDB
NOMINEES.loc[s_nominee_is_name,'PERSON_ID'] = NOMINEES[s_nominee_is_name].S_IMDB

cols_to_drop = ['P_NOMINEES', 'S_NOMINEES', 'LEN_P_NOMINEES', 'LEN_S_NOMINEES', 
                'P_IMDB', 'P_NAME', 'S_IMDB', 'S_NAME']
NOMINEES.drop(cols_to_drop,axis=1, inplace=True)


########################################
## Preparing data frame for modelling ##
########################################

groupby_cols = ['TITLE_ID','TITLE','EVENT','AWARD',
                'CATEGORY','CATEGORY_ALIAS','YEAR','IS_WINNER']

MODEL_DF = NOMINEES[groupby_cols].drop_duplicates()

#Goal: Predict Oscar Winners
#The final dataset should contain all nominees from awards
#that the Oscar nominees participated

#List of all event nominees
event_nominees = MODEL_DF[MODEL_DF.EVENT == event_name].TITLE_ID.unique()


#List all awards that had at least one Oscar nominee nominated
VALID_AWARDS = MODEL_DF[MODEL_DF.TITLE_ID.isin(event_nominees)]
VALID_AWARDS = VALID_AWARDS[['EVENT','AWARD','CATEGORY','YEAR']].drop_duplicates()
VALID_AWARDS = VALID_AWARDS.apply(tuple,axis=1)
MODEL_DF = MODEL_DF[MODEL_DF[['EVENT','AWARD','CATEGORY','YEAR']].apply(tuple,axis=1).isin(VALID_AWARDS)]

MODEL_FEATURES = MODEL_DF[['EVENT','AWARD','CATEGORY_ALIAS']].drop_duplicates()
MODEL_FEATURES = MODEL_FEATURES.apply(tuple,axis=1)
MODEL_FILMS = MODEL_DF.TITLE_ID.unique()

#MODEL_DF['FEATURE'] = MODEL_DF[['EVENT','AWARD','CATEGORY_ALIAS','IS_WINNER']].apply(tuple,axis=1)
MODEL_DF['FEATURE'] = MODEL_DF[['EVENT','AWARD','CATEGORY_ALIAS']].apply(tuple,axis=1)

#Making sure there's only one pair TITLE_ID x FEATURE
max(MODEL_DF.groupby(['TITLE_ID']).count().FEATURE)
MODEL_TESTE = MODEL_DF.groupby(['TITLE_ID','FEATURE']).count()
MODEL_TESTE[MODEL_TESTE.YEAR > 2]
sum(MODEL_DF.groupby(['TITLE_ID','FEATURE']).count().YEAR > 2)

pivot_keys = ['TITLE_ID','TITLE','FEATURE','YEAR','N/W','VALUE']
PIVOT_DF = pd.DataFrame(columns=pivot_keys)
PIVOT_ROW = dict.fromkeys(pivot_keys)
countdown = len(MODEL_FILMS)
for film in MODEL_FILMS:
    start_time = time.time()
    for feature in MODEL_FEATURES:
        nominee = MODEL_DF[(MODEL_DF.TITLE_ID == film) & (MODEL_DF.FEATURE == feature)]
        PIVOT_ROW['TITLE_ID'] = film
        PIVOT_ROW['FEATURE'] = feature
        len_nominee = len(nominee)
        if (len_nominee>0): 
            for nominee_index in range(0,len_nominee):
                PIVOT_ROW['YEAR'] = nominee['YEAR'].values[nominee_index]
                PIVOT_ROW['TITLE'] = nominee['TITLE'].values[nominee_index]

                if (nominee['IS_WINNER'].values[nominee_index]==True):
                    PIVOT_ROW['N/W'] = 'W'
                    PIVOT_ROW['VALUE'] = 1
                    PIVOT_DF = PIVOT_DF.append(PIVOT_ROW, ignore_index=True)
                else:
                    PIVOT_ROW['N/W'] = 'W'
                    PIVOT_ROW['VALUE'] = 0
                    PIVOT_DF = PIVOT_DF.append(PIVOT_ROW, ignore_index=True)                
                PIVOT_ROW['N/W'] = 'N'
                PIVOT_ROW['VALUE'] = 1
                PIVOT_DF = PIVOT_DF.append(PIVOT_ROW, ignore_index=True)
            if (len_nominee>1): 
                print("Filme indicado a prêmios semelhantes numa mesma premiação:")
                print(nominee)
    stop_time = time.time()
    print("{0} - {1} - {2}".format(countdown,stop_time-start_time,PIVOT_ROW['TITLE']))
    countdown = countdown - 1

        # else: #film was not nominated in this feature
        #     film_years = MODEL_DF[(MODEL_DF.TITLE_ID == film)].YEAR.unique()
        #     feature_years = MODEL_DF[(MODEL_DF.FEATURE == feature)].YEAR.unique()
        #     for film_year in film_years:
        #         if (film_year in feature_years):
        #             #film was nominated in the same year, but for a different feature
        #             PIVOT_ROW['YEAR'] = film_year
        #             PIVOT_ROW['N/W'] = 'N'
        #             PIVOT_ROW['VALUE'] = 0
        #             PIVOT_DF = PIVOT_DF.append(PIVOT_ROW, ignore_index=True)

PIVOT_DF['COLUMN'] = PIVOT_DF[['FEATURE','N/W']].apply(tuple,axis=1)


#Criação de arquivo só com linhas de indicados:
TEMP_DF = PIVOT_DF 
PIVOT_DF = PIVOT_DF[PIVOT_DF['N/W']=='N']
filename_insert = '_Nominees'
PIVOT_DF = PIVOT_DF.drop(columns=['FEATURE','N/W'],axis=1)
PIVOT_DF = PIVOT_DF.drop_duplicates()
PIVOT_DF = PIVOT_DF.groupby(['TITLE_ID','TITLE','YEAR','COLUMN'],as_index=False).agg({"VALUE":"max"})
FINAL_DF = PIVOT_DF.pivot(columns='COLUMN',
                     values='VALUE',
                     index=['TITLE_ID','TITLE','YEAR'])
DF_DIR = 'C:\\Users\\Will\\Documents\\Data Science\\Projeto Tree\\Oscars 2020'
DF_CSV = DF_DIR + '\\Tree_' + filename_strip + filename_insert + '.csv'
FINAL_DF.to_csv(DF_CSV)

#Criação de arquivo com linhas de indicados e vencedores:
PIVOT_DF = TEMP_DF
filename_insert = '_All_Results'
PIVOT_DF = PIVOT_DF.drop(columns=['FEATURE','N/W'],axis=1)
PIVOT_DF = PIVOT_DF.drop_duplicates()
PIVOT_DF = PIVOT_DF.groupby(['TITLE_ID','TITLE','YEAR','COLUMN'],as_index=False).agg({"VALUE":"max"})
FINAL_DF = PIVOT_DF.pivot(columns='COLUMN',
                         values='VALUE',
                         index=['TITLE_ID','TITLE','YEAR'])
DF_DIR = 'C:\\Users\\Will\\Documents\\Data Science\\Projeto Tree\\Oscars 2020'
DF_CSV = DF_DIR + '\\Tree_' + filename_strip + filename_insert + '.csv'
FINAL_DF.to_csv(DF_CSV)


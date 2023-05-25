# -*- coding: utf-8 -*-
"""
Created on Mon Jan 11 15:51:54 2021

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

########################
## Reading Data Files ##
########################

DF_DIR = 'C:\\Users\\Will\\Documents\\Data Science\\Projeto Tree\\Oscars Datasets'
all_files = glob.glob(DF_DIR + "/IMDB_*.csv")

li = []

for filename in all_files:
    df = pd.read_csv(filename, index_col=None, header=0)
    li.append(df)

IMDB_DATA = pd.concat(li, axis=0, ignore_index=True)
IMDB_DATA.drop('Unnamed: 0',axis=1, inplace=True)
IMDB_DATA.Event_Awards = IMDB_DATA.Event_Awards.apply(ast.literal_eval)

IMDB_DATA.Event_Title.unique()
IMDB_DATA.columns

#####################################
## Populating New Awards DataFrame ##
#####################################

#Slicing data to test steps
TEST_DATA = IMDB_DATA
#TEST_DATA = IMDB_DATA[IMDB_DATA.Event_Year == 2019]
#TEST_DATA = TEST_DATA[TEST_DATA.Event_Title == 'SDFCS']
#TEST_DATA = IMDB_DATA[IMDB_DATA.Event_Title == 'SDFCS']

#Initializing data frames
df_columns = ['EVENT','YEAR','AWARD','CATEGORY','CATEGORY_ALIAS','CATEGORY_GROUP',
              'P_NOMINEES','S_NOMINEES','IS_WINNER']
AWARDS_DATA = pd.DataFrame(columns = df_columns)
AWARDS_ROW = dict.fromkeys(df_columns)

for event_key,EVENT in TEST_DATA.iterrows():
    print(EVENT.Event_Title)
    print(EVENT.Event_Year)  
    print("{} Awards".format(len(EVENT.Event_Awards)))
    for AWARD in EVENT.Event_Awards:
        #AWARDS_ROW['AWARD'] = AWARD['awardName']
        print("{0} has {1} categories.".format(AWARD['awardName'],
                                              len(AWARD['categories'])))
        for CATEGORY in AWARD['categories']:   
            #AWARDS_ROW['CATEGORY'] = CATEGORY['categoryName']
            for NOMINEE in CATEGORY['nominations']:
                AWARDS_ROW['EVENT'] = EVENT.Event_Title
                AWARDS_ROW['YEAR'] = EVENT.Event_Year
                AWARDS_ROW['AWARD'] = NOMINEE['awardName']
                AWARDS_ROW['CATEGORY'] = NOMINEE['categoryName']
                AWARDS_ROW['CATEGORY_ALIAS'] = get_category_alias(AWARDS_ROW['CATEGORY'])
                AWARDS_ROW['CATEGORY_GROUP'] = get_category_group(AWARDS_ROW['CATEGORY_ALIAS'])
                AWARDS_ROW['P_NOMINEES'] = NOMINEE['primaryNominees']
                AWARDS_ROW['S_NOMINEES'] = NOMINEE['secondaryNominees']
                AWARDS_ROW['IS_WINNER'] = NOMINEE['isWinner']
                AWARDS_DATA = AWARDS_DATA.append(AWARDS_ROW, ignore_index=True)
            
AWARDS_CHECK = AWARDS_DATA[AWARDS_DATA.CATEGORY_ALIAS == "NONE"]
AWARDS_CHECK.CATEGORY.unique()

AWARDS_CHECK = AWARDS_DATA[AWARDS_DATA.CATEGORY_GROUP == "NONE"]
AWARDS_CHECK.CATEGORY_ALIAS.unique()

DF_DIR = 'C:\\Users\\Will\\Documents\\Data Science\\Projeto Tree'
DF_CSV = DF_DIR + '\\Tree_Oscars_2020.csv'
AWARDS_DATA.to_csv(DF_CSV)

########################
## EXPLODING NOMINEES ##
########################


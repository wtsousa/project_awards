# -*- coding: utf-8 -*-
"""
Created on Sun Jan 10 23:45:04 2021

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

##### GENERAL FUNCTIONS ####

def find_between(s, first, last ):
    try:
        start = s.index( first ) + len( first )
        end = s.index( last, start )
        return s[start:end]
    except ValueError:
        return "Deu Ruim find_between"
    

##### MAIN SECTION #####

## Initializing Data Frame
DF_ALL_AWARDS = pd.DataFrame()
DF_ALL_AWARDS['Event_Title'] = pd.Series(dtype='object')
DF_ALL_AWARDS['Event_Year'] = pd.Series(dtype='object')
DF_ALL_AWARDS['Event_Awards'] = pd.Series(dtype='object')

#Parasite is 2020
award_title = "CAS"
first_year = 2000
last_year = 2021
event_id = "ev0000175"

for award_year in range(first_year, last_year + 1):
    print(award_year)

    url = "https://www.imdb.com/event/{0}/{1}/1/?ref_=ev_eh".format(event_id,award_year)

    if ((award_year < 202)&(award_year > 200)):
        url = "https://www.imdb.com/event/{0}/{1}/1/?ref_=ev_eh".format(event_id,award_year-1)


    if (award_year == 200):
        url = "https://www.imdb.com/event/ev0000392/2005/2/?ref_=ev_eh"

    if (award_year == 203):
        url = "https://www.imdb.com/event/ev0000392/2002/2/?ref_=ev_eh"
        
    if (award_year == 200):
        url = "https://www.imdb.com/event/ev0000392/2000/2/?ref_=ev_eh"


    req = requests.get(url)
    imdb_html = req.text
    html_selector = Selector(text=imdb_html)


    ##################################
    ## Retrieving Awards Dictionary ##
    ##################################

    start_str = "IMDbReactWidgets.NomineesWidget.push(['center-3-react',"
    end_str = "]);"
    awards_str = find_between(imdb_html,start_str,end_str)
    awards_dict = json.loads(awards_str)

    awards_categories = awards_dict['nomineesWidgetModel']
    awards_categories = awards_categories['eventEditionSummary']
    awards_list = awards_categories['awards']

    DF_AWARDS = pd.DataFrame()
    DF_AWARDS['Event_Title'] = pd.Series(award_title)
    DF_AWARDS['Event_Year'] = pd.Series(award_year)
    DF_AWARDS['Event_Awards'] = [awards_list]
    
    DF_ALL_AWARDS = DF_ALL_AWARDS.append(DF_AWARDS, ignore_index=True)
    
    waittime = 5 + random.randint(1,3)
    print("... waiting {} seconds ...".format(waittime))
    time.sleep(waittime)


DF_DIR = 'C:\\Users\\Will\\Documents\\Data Science\\Projeto Tree\\Oscars Datasets'
DF_CSV = DF_DIR + '\\IMDB_{0}_{1}_{2}_Raw.csv'.format(award_title,first_year,last_year)
DF_ALL_AWARDS.to_csv(DF_CSV)

awards_categories = awards_list[0]

print("Award Name: {}".format(awards_categories['awardName']))
print("Award ID: {}".format(awards_categories['id']))

awards_categories = awards_categories['categories']

num_categories = len(awards_categories)

for category_details in awards_categories:
    print(category_details["categoryName"])
    nominees_list = category_details["nominations"]
    for nominee in nominees_list:
        category_name = nominee['categoryName']
        if len(nominee['primaryNominees']) > 1:
            print("MAIS DE UM GANHADOR")
        nominee_name = nominee['primaryNominees'][0]['name']
        #nominee_original_name = nominee['primaryNominees'][0]['originalName']
        nominee_imdb_id = nominee['primaryNominees'][0]['const']
        is_winner = nominee['isWinner']
        secondary_nominees = nominee['secondaryNominees']
        print(" - {0} {1}".format(nominee_name,is_winner))
    


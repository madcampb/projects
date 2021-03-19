import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
from datetime import datetime


def make_url_list(first_url, num_pages=20):
    urls = [first_url]
    
    base_url = first_url[0:26]
    
    for i in range(2,num_pages):
        modified_url = base_url + 'page/' + str(i)
        urls.append(modified_url)
        
    return(urls)


def get_claims(url):
    '''
    URL taken from factcheck.afp.com
    
    This just extracts the url, headline and date from the list
    Could actually navigate to extracted URL and get more info
    
    Returns df with url, headline, date, time
    '''
    page = requests.get(url)

    soup = BeautifulSoup(page.content, 'html.parser')

    urls = list()
    base_url = 'https://checkyourfact.com'

    headlines = list()

    dates = list()

    feedpost = soup.find('atom')
    feats = feedpost.find('features')
    
    if feats: 
        cards = feats.find_all("a")
    
        for card in cards:
            url = card['href']
            urls.append(base_url + url)
            hl = card.find("span").get_text()
            headlines.append(hl)
            dt = card['href'][1:11]
            dates.append(dt) # needs refactoring
        
    article_cards = feedpost.find('articles').find_all("a")
    
    for card in article_cards:
        url = card['href']
        urls.append(base_url + url)
        hl = card.find("name").get_text()
        headlines.append(hl)
        dt = card['href'][1:11]
        dates.append(dt)

    df = pd.DataFrame(list(zip(urls, headlines, dates)), 
                       columns =['url', 'headline', 'date']) 

    df['date'] = df['date'].map(lambda dt_obj: datetime.strptime(dt_obj, '%Y/%m/%d').strftime('%d-%m-%Y'))

    return(df)


first_url = 'https://checkyourfact.com/?fbclid=IwAR1WS2XZjpejAlLm34KrHtYvT0fy9ep5FFbTVLSREAPtxGVPLrxHXRPkm0k'

urls = make_url_list(first_url, num_pages=25)

all_data = pd.DataFrame()

for url in urls:
    print(url)
    page_data = get_claims(url)
    all_data = pd.concat([all_data, page_data])
    
all_data.to_csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/checkyourfact.csv')

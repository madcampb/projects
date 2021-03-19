import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
from datetime import datetime

def get_ap_claims(url):
    '''
    URL taken from factcheck.afp.com
    
    This just extracts the url, headline and date from the list
    Could actually navigate to extracted URL and get more info
    
    Returns df with url, headline, date, time
    '''
    page = requests.get(url)

    soup = BeautifulSoup(page.content, 'html.parser')

    urls = list()
    base_url = 'https://apnews.com'

    headlines = list()

    dates = list()

    # times = list()

    claims = list()

    feedpost = soup.find('article', attrs={'class': 'feed-0-2-16 feed'})
    if feedpost is None:
        feedpost = soup.find('main')
    cards = feedpost.find('article').find_all('div', {'class': 'FeedCard Component-wireStory-0-2-104'})
    for card in cards:
        url = card.find("a")['href']
        urls.append(base_url + url)
        hl = card.find("h1").get_text()
        headlines.append(hl)
        dt = card.find_all("span")[1].get_text() 
        dates.append(dt) # needs refactoring
        cl = card.find("p").get_text()
        claims.append(cl)

    df = pd.DataFrame(list(zip(urls, headlines, dates, claims)), 
                       columns =['url', 'headline', 'date', 'claim']) 

    df['date'] = df['date'].map(lambda dt_obj: datetime.strptime(dt_obj, '%B %d, %Y %Z').strftime('%d-%m-%Y'))

    return(df)


url = 'https://apnews.com/hub/fact-checking'

df = get_claims(url)
df.to_csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/ap.csv')

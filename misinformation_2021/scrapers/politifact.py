import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
from datetime import datetime

def make_url_list(first_url, num_pages=20):

    urls = [first_url]
    base_url = first_url[:-1]

    for i in range(2,num_pages):
        modified_url = base_url + '?page=' + str(i) 
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
    soup = BeautifulSoup(page.content,'html.parser')

    urls = list()

    headlines = list()
    rating = list()

    dates = list()

    feedpost = soup.find('div', attrs={'class': 'o-listicle__inner'})

    cards = feedpost.find_all('li')
    for card in cards:
        url = card.find_all("a")[1]['href']
        urls.append(url)
        hl = card.find_all("a")[1].get_text()[1:-1]
        headlines.append(hl)
        rt = card.find_all('img')[2]['alt']
        rating.append(rt)
        dt = card.find('footer').get_text().split('•')[1][1:-1].strip()
        dates.append(dt) #needs refactoring

    df = pd.DataFrame(list(zip(urls, headlines, rating, dates)), 
                       columns =['url', 'headline', 'rating', 'date'])


    df['date'] = df['date'].map(lambda dt_obj: datetime.strptime(dt_obj, '%B %d, %Y').strftime('%d-%m-%Y'))

    return(df)


first_url = 'https://www.politifact.com/factchecks/list/'

urls = make_url_list(first_url, num_pages=25)

all_data = pd.DataFrame()

for url in urls:
    print(url)
    page_data = get_claims(url)
    all_data = pd.concat([all_data, page_data])
    
all_data.to_csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/politifact.csv')

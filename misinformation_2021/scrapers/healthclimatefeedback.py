import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
from datetime import datetime


def make_url_list(first_url, num_pages=20):

    urls = [first_url]
    base_url = first_url[:-1]

    for i in range(2,num_pages):
        modified_url = base_url + '/?page=' + str(i) 
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
    details = list()
    headlines = list()
    sources = list()
    rating = list()

    dates = list()

    feedpost = soup.find('div', attrs={'class': 'content row'})

    cards = feedpost.find_all('div', {'class':'row'})
    for card in cards:
        url = card.find("a")['href']
        urls.append(url)
        hl = card.find("h3").get_text().strip()
        headlines.append(hl)
        deet = card.find('div', {'class':'feedpages-excerpt feedpages__claim__container__content__text mb1'}).get_text()[1:].strip()
        details.append(deet)
        sr = card.find_all('span')[1].get_text()
        sources.append(sr)
        rt = card.find_all('img')[1]['src'].split('/')[-1].split('_')[1].split('.')[0]
        rating.append(rt)
        dt = card.find_all('span')[3].get_text()
        dates.append(dt) #needs refactoring

    df = pd.DataFrame(list(zip(urls, headlines, details, sources, rating, dates)), 
                       columns =['url', 'headline', 'details', 'source', 'rating', 'date'])


    df['date'] = df['date'].map(lambda dt_obj: datetime.strptime(dt_obj, '%d %b %Y').strftime('%d-%m-%Y'))

    return(df)



feedbackurls = ['https://climatefeedback.org/claim-reviews/', 'https://healthfeedback.org/claim-reviews/']

for url in feedbackurls:

    base = url.split('/')[2].split('.')[0]
    urls = make_url_list(url, num_pages=75)

    all_data = pd.DataFrame()

    for url in urls:
        print(url)
        page_data = get_claims(url)
        all_data = pd.concat([all_data, page_data])

    all_data.to_csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/{}.csv'.format(base))

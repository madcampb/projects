import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
from datetime import datetime

def make_url_list(first_url, num_pages=20):

    urls = [first_url]
    base_url = first_url[:-13]
    end_url = first_url[-12:]

    for i in range(2,num_pages):
        modified_url = base_url + str(i) + end_url
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

    base_url = 'https://www.reuters.com/'
    urls = list()
    details = list()
    headlines = list()

    dates = list()

    feedpost = soup.find('div', attrs={'class': 'column1 col col-10'})

    cards = feedpost.find_all('article')
    for card in cards:
        url = card.find("a")['href']
        urls.append(base_url + url)
        hl = card.find("h3").get_text().strip()
        headlines.append(hl)
        deet = card.find("p").get_text()
        details.append(deet)
        dt = card.find('span').get_text()
        dates.append(dt) #needs refactoring

    df = pd.DataFrame(list(zip(urls, headlines, details, dates)), 
                       columns =['url', 'headline', 'details', 'date'])


    today = df[df['date'].str.contains('EDT')]
    today['date'] = datetime.today().strftime('%d-%m-%Y')

    before = df[~df['date'].str.contains('EDT')]
    before['date'] = before['date'].map(lambda dt_obj: datetime.strptime(dt_obj, '%b %d %Y').strftime('%d-%m-%Y'))

    all_together = pd.concat([today, before])
    
    return(all_together)


url = 'https://www.reuters.com/news/archive/reutersComService?view=page&page=1&pageSize=10'
urls = make_url_list(url, num_pages=61)

all_data = pd.DataFrame()

for url in urls:
    print(url)
    page_data = get_claims(url)
    all_data = pd.concat([all_data, page_data])

all_data.to_csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/reuters.csv')

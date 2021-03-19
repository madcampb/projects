import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
from datetime import datetime


def make_url_list(first_url, num_pages=20):
    urls = [first_url]
    base_url = first_url[0:26]

    end_url = first_url[25:]

    for i in range(2,num_pages):
        modified_url = base_url + 'page/' + str(i) + end_url
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
    details = list()

    dates = list()

    feedpost = soup.find('div', attrs={'class': 'col-lg-8 content-area'})

    cards = feedpost.find_all('article')
    for card in cards:
        url = card.find("a")['href']
        urls.append(url)
        hl = card.find("a").get_text()
        headlines.append(hl)
        det = card.find('p').get_text()
        details.append(det)
        dt = card.find('div', {'class':'entry-meta'}).get_text()[3:].strip()
        dates.append(dt) # needs refactoring

    df = pd.DataFrame(list(zip(urls, headlines, details, dates)), 
                       columns =['url', 'headline', 'details', 'date'])


    df['date'] = df['date'].map(lambda dt_obj: datetime.strptime(dt_obj, '%B %d, %Y').strftime('%d-%m-%Y'))

    return(df)



first_url = 'https://www.factcheck.org/?fbclid=IwAR1PzRjDeRQyQc1eYBDC2pL3VXAGO8vo1ScycnWmyf1ibKY4Kk7InLD4JMw'

urls = make_url_list(first_url, num_pages=20)

all_data = pd.DataFrame()

for url in urls:
    print(url)
    page_data = get_claims(url)
    all_data = pd.concat([all_data, page_data])
    
all_data.to_csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/factcheck.csv')

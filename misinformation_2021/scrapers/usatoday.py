import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
import datetime

def get_claims(url):
    '''
    URL taken from factcheck.afp.com
    
    This just extracts the url, headline and date from the list
    Could actually navigate to extracted URL and get more info
    
    Returns df with url, headline, date, time
    '''
    page = requests.get(url)
    soup = BeautifulSoup(page.content,'html.parser')

    base_url = 'https://www.usatoday.com/'
    urls = list()
    details = list()
    headlines = list()

    dates = list()

    feedpost = soup.find('div', attrs={'class': 'gnt_pr'})

    top_post = feedpost.find("a")

    url = top_post['href']
    urls.append(base_url + url)
    hl = top_post.get_text().strip()
    headlines.append(hl)
    deet = ''
    details.append(deet)
    dt = top_post.find('div', {'class':'gnt_sbt gnt_sbt__ms gnt_sbt__ts gnt_sbt__mg'})['data-c-dt']
    dates.append(dt)

    cards = feedpost.find('div', {'class', 'gnt_m gnt_m_flm'}).find_all('a')
    for card in cards:
        url = card['href']
        urls.append(base_url + url)
        hl = card.get_text().strip()
        headlines.append(hl)
        deet = card['data-c-br']
        details.append(deet)
        if card.find('div', {'class':'gnt_m_flm_sbt gnt_sbt gnt_sbt__ms gnt_sbt__ts'}):
            dt = card.find('div', {'class':'gnt_m_flm_sbt gnt_sbt gnt_sbt__ms gnt_sbt__ts'})['data-c-dt']
        else: dt = ''
        dates.append(dt) #needs refactoring


    df = pd.DataFrame(list(zip(urls, headlines, details, dates)), 
                       columns =['url', 'headline', 'details', 'date'])
    df = df[df['date'] != '']


    yday = df[df['date'].str.contains('ET')]
    yday['date'] = (datetime.datetime.today() - datetime.timedelta(days=1)).strftime('%d-%m-%Y')

    before = df[~df['date'].str.contains('ET')]
    before['date'] = before['date'].map(lambda dt_obj: datetime.datetime.strptime(dt_obj, '%b. %d, %Y').strftime('%d-%m-%Y'))

    all_together = pd.concat([yday, before])

    return(all_together)


url = 'https://www.usatoday.com/news/factcheck/'
page_data = get_claims(url)

page_data.to_csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/usatoday.csv')

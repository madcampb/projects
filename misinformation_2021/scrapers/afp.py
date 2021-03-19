import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup


def make_url_list(base_URL, num_pages=20):
    '''
    base_URL :  URL from first page of AFP website
    num_pages : how many scrolling pages do you want to scrape (num extra URLs)
    
    returns list of URLs
    '''
    afp_urls = [base_URL]
    
    for i in range(1,num_pages):
        modified_url = base_URL + '&page=' + str(i)
        afp_urls.append(modified_url)
        
    return(afp_urls)


def get_claims(URL):
    '''
    URL taken from factcheck.afp.com
    
    This just extracts the url, headline and date from the list
    Could actually navigate to extracted URL and get more info
    
    Returns df with url, headline, date, time
    '''
    page = requests.get(URL)

    soup = BeautifulSoup(page.content, 'html.parser')

    urls = list()
    base_url = 'https://factcheck.afp.com'

    headlines = list()

    dates = list()

    times = list()

    featured_post = soup.find('div', attrs={'class': 'featured-post'})
    if featured_post is None:
        featured_post = soup.find('main')
    cards = featured_post.select(".card")
    for card in cards:
        url = card.find("a")['href']
        urls.append(base_url + url)
        hl = card.find("h4").get_text()[2:].strip()
        headlines.append(hl)
        dt = card.find("small").get_text().split(' ')[2] 
        dates.append(dt)
        tm = card.find("small").get_text().split(' ')[4]
        times.append(tm)

    df = pd.DataFrame(list(zip(urls, headlines, dates, times)), 
                   columns =['url', 'headline', 'date', 'time']) 

    return(df)



### run all the code

base_URL = 'https://factcheck.afp.com/afp-usa?fbclid=IwAR3In37HRkagU6Lc-63sRxwo7wq_lkAighsVI4EDmtDkwE6WoJEVg6bEDoc'

all_urls = make_url_list(base_URL, num_pages=24)

all_data = pd.DataFrame()

for url in all_urls:
    page_data = get_claims(url)
    all_data = pd.concat([all_data, page_data])
    

### save df to data folder   
all_data.to_csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/afp.csv')



import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
from datetime import datetime
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
import time
import os


def get_claims(url):
    '''
    URL taken from factcheck.afp.com
    
    This just extracts the url, headline and date from the list
    Could actually navigate to extracted URL and get more info
    
    Returns df with url, headline, date, time
    '''
    # This starts an instance of the Chrome browser to be controlled
    # by the selenium webdriver
    driver = webdriver.Chrome(executable_path='/Users/madelinecampbell/Desktop/chromedriver')
    # driver = webdriver.Chrome()

    # This is simply the driver getting the page
    driver.get(url)

    # try-except acts as a loop that on each iteration is asking if more exists
    # while True:
    #     try:
    #         loadmore = driver.find_element_by_id("bottomPager")
    #         loadmore.click()
    #     except NoSuchElementException:
    #         print("Reached bottom of page")
    #         break

    SCROLL_PAUSE_TIME = 1.25

    # Get scroll height
    last_height = driver.execute_script("return document.body.scrollHeight")

    while True:
        # Scroll down to bottom
        driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")

        # Wait to load page
        time.sleep(SCROLL_PAUSE_TIME)

        # Calculate new scroll height and compare with last scroll height
        new_height = driver.execute_script("return document.body.scrollHeight")
        if new_height == last_height:
            break
        last_height = new_height

    soup = BeautifulSoup(driver.page_source,'html.parser')

    # page = requests.get(url)

    # soup = BeautifulSoup(page.content, 'html.parser')

    urls = list()

    headlines = list()
    reviews = list()

    dates = list()

    feedpost = soup.find('div', attrs={'class': 'portable-archive-list'})
    cards = feedpost.find_all('div', {'class':'post-preview portable-archive-post has-image has-author-line'})
    for card in cards:
        url = card.find("a")['href']
        urls.append(url)
        hl = card.find("a").get_text()
        headlines.append(hl)
        rev = card.find_all("a")[1].get_text()
        reviews.append(rev)
        dt = card.find("table").find('td', {'class': 'post-meta-item post-date'}).get_text()
        dates.append(dt) # needs refactoring

    df = pd.DataFrame(list(zip(urls, headlines, reviews, dates)), 
                       columns =['url', 'headline', 'review', 'date'])


    df2019 = df.loc[df['date'].str[-4:] == '2019'] #
    df2019['date'] = df2019['date'].map(lambda dt_obj: datetime.strptime(dt_obj, '%b %d, %Y').strftime('%d-%m-%Y'))

    df2020 = df.loc[df['date'].str[-4:] == '2020'] #
    df2020['date'] = df2020['date'].map(lambda dt_obj: datetime.strptime(dt_obj, '%b %d, %Y').strftime('%d-%m-%Y'))

    today = df.loc[df['date'].str[-3:] == 'ago']
    today['date'] = datetime.today().strftime('%d-%m-%Y')

    df2021 = df.loc[(df['date'].str[-3:] != 'ago') & (df['date'].str[-4:] != '2020') & (df['date'].str[-4:] != '2019')]
    df2021['date'] = df2021['date'] + ', 2021'
    df2021['date'] = df2021['date'].map(lambda dt_obj: datetime.strptime(dt_obj, '%b %d, %Y').strftime('%d-%m-%Y'))

    all_better = pd.concat([today, df2021, df2020, df2019])
    
    return(all_better)


url = 'https://factcheck.thedispatch.com/archive'

df = get_claims(url)
df.to_csv('/Users/madelinecampbell/Documents/GitHub/projects/misinformation_2021/data/dispatch.csv')

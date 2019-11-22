# coding=utf-8
from bs4 import BeautifulSoup
import requests
import time
import sys


class HtmlWatcher():

    def __init__(self):
        self.dict = {}
        self.prev_tags = []

    def __get_HTML_from_website(self, url):
        try:
            get_from_url = requests.get("http://" + url)
        except requests.exceptions.RequestException as e:
            print(e)
            sys.exit(1)
        return get_from_url.text

    def __check_HTML(self, html):
        html_tags = []
        count = 0

        print("---------WATCHING---------")

        for tag in html.prettify().split("\n"):
            html_tags.append(str(tag).strip())

        if self.prev_tags == html_tags:
            print("no change")
            return ""

        if html.title.string not in self.dict:
            self.dict[html.title.string] = html_tags
        else:
            for line_index, (prev_tag, cur_tag) in enumerate(zip(self.prev_tags, html_tags)):
                if prev_tag != cur_tag:
                    count += 1
                    print(
                        f"{count} | Change from {prev_tag} to {cur_tag} in line {line_index}.")

        self.prev_tags = html_tags
        return ""

    def watch_HTML(self, url, wait_seconds):
        while True:
            html = self.__get_HTML_from_website(url)
            soup = BeautifulSoup(html, 'html.parser')
            self.__check_HTML(soup)
            time.sleep(wait_seconds)
        return ""


html_watcher = HtmlWatcher()

html_watcher.watch_HTML("bartek3pl.github.io/Github-Users-Finder/", 20)
html_watcher.watch_HTML("geeksforgeeks.org/", 60)

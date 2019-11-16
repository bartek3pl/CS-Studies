# coding=utf-8
from bs4 import BeautifulSoup
import requests
import time

dict = {}
prev_tags = []


def get_HTML_from_website(url):
    get_from_url = requests.get("http://" + url)
    return get_from_url.text


def check_HTML(html):
    global prev_tags
    html_tags = []
    count = 0

    # print("---------WATCHING---------")

    for tag in html.prettify().split("\n"):
        html_tags.append(str(tag).strip())

    if prev_tags == html_tags:
        # print("no change")
        return ""

    if html.title.string not in dict:
        dict[html.title.string] = html_tags
    else:
        for line_index, (prev_tag, cur_tag) in enumerate(zip(prev_tags, html_tags)):
            if prev_tag != cur_tag:
                count += 1
                print(
                    f"{count} | Change from {prev_tag} to {cur_tag} in line {line_index}.")

    prev_tags = html_tags
    return html_tags


def watch_HTML(url, wait_seconds):
    while True:
        html = get_HTML_from_website(url)
        soup = BeautifulSoup(html, 'html.parser')
        check_HTML(soup)
        time.sleep(wait_seconds)
    return ""


watch_HTML("bartek3pl.github.io/Github-Users-Finder/", 20)
watch_HTML("https://www.geeksforgeeks.org/", 60)

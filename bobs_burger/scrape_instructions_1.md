---
title: "scrape instructions"
---

## SUMMARY

Your task is to scrape certain elements on a single web page. The page is mostly javascript so that that simply reading the html will not work. You, the agent, will need to simulate interaction with the web page to click on buttons and then extract the text from the pop-up window that appears.

## EXPECTED OUTPUT

A csv file with 45 columns: Actor, Character(s),Year, season number, episode number.

## TARGET URL

A static version of the page source is in the local file "imdb.html" but this is to show the structure. It is not the live page with working scripts.

The url for the page you will interact with is [ ](%22https://www.imdb.com/title/tt1561755/fullcredits/?ref_=tt_cst_sm%22). It has a list with the cast of the TV Show, "Bob's Burgers"

## HINTS TO EXTRACTING THE DATA

Each section on the page is bounded with XREF //*[@id="__next"]/main/div/section/div/section/div/div[1]/section[3]'<div> ipc-title__wrapper

We are interested in the "CAST" section of the page.

The rows of the cast list begin with HTML '<li class="ipc-metadata-list-summary-item sc-9eb08875-0 juAIqL full-credits-page-list-item" data-testid="name-credits-list-item">'

The text in the row includes the actor name followed by the charactor name, the number of episodes the actor appeared in and the year or years they appeared. We are only interested in the rows where "1 episode" is the number of episodes.

The text in each row "1 episode" is part of a clickable button.

# EXECUTION

Before executing create an R -language script to accomplish the task. Use the tidyverse vernacular.  Packages which will be useful are tidyverse, rvest, and chromote and xml2.

Extract the actor name, character and year from the CAST section for those who appear in only 1 episode. Add these to the data frame.

Simulate clicks on the buttons and parse the text of the pop-up window for each row.

From the pop-up, extract the season number and episode number in the format S\<#\>E\<#\>. Add these to the data frame.

Execute the task and save the data frame as a csv file.
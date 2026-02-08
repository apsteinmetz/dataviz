---
title: "scrape instructions"
---

## SUMMARY

Your task is to scrape certain elements on a multiple web pages from imdb.com

## EXPECTED OUTPUT

A csv file called "imdb_bob.csv" with  8 columns:
season,
episode, 
title,
aired_date,
synopsis,
imdb_rating,
imdb_vote_count
thumbnail_url, 
imdb_episode_url, 

## TARGET URLS

There are 16 pages to read. The URLs are of the form

https://www.imdb.com/title/tt1561755/episodes/?season=nn&ref_=ttep

where "nn" is the season number, 1-16. do not add leading zeroes to single digit season numbers when forming the URL.

## HINTS TO EXTRACTING THE DATA
All the data we want are within these "article" HTML tags :
'<article class="sc-64257d69-1 mOJzu episode-item-wrapper">
</article>'

The number of articles on each of the pages may vary.

Each of the data elements to extract can be found as follows, in order of appearance:

The thumbnail url_will be the src of the img tag.

The imdb_episode_url will be the href within 
'<div class="ipc-title ipc-title--base ipc-title--title ipc-title-link-no-icon ipc-title--on-textPrimary sc-5372d523-8 jtXJUZ">'

 '<div class="ipc-title__text"> ' will contain the season and episode in the form Sn.En Title

This will be followed by a <span class="sc-5372d523-10 knzESm"> element with the air date in the form Day of week, mmm,dd,yyyy

The synopsis text will be within <div class="ipc-overflowText ipc-overflowText--base">

The imdb_rating will be in <span class="ipc-rating-star--rating">

The vote count will be in <span class="ipc-rating-star--voteCount">

# EXECUTION

Before executing create an R -language script called "scrape_imdb_2.r" to accomplish the task. Use the tidyverse vernacular.  Packages which will be useful are tidyverse, rvest, and chromote and xml2.

Ask if clarification is needed.

Execute the task and save the data frame as a csv file.
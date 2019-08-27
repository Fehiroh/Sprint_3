# BGG_recommeder_Sprint_1

For the repository information (what the folders are, what they contain), scroll down to the **Repository Layout** section

## Overview: Purpose, and Progress
The purpose is to create a webapp that runs a boardgame recommender system, which can recommend new boardgames based on either what a user has 
previously enjoyed or collaborative (still undecided). 

In Sprint 1, I have explored the content data (characteristics of board games) to understand what feature engineering and dimensional reduction 
will have to take place in order to make the development of a recommender system possible, and come up with a script that performs these processes. 
I have also used webscrapping to generate two new dataframes, one that contains user  information and one that links users to the last 25 boardgames 
they have played and enjoyed in order to facilitate the development of a collaborative recommendation system, should that become desirable or necessary. 

## Requirements
In order to run the R files contained in this repository, one needs to have R version 3.6.0 or greater installed. The necessary libraries will all 
automatically install (if necessary) and load due to the usage of the p_load() function from the pacman package. Here is an exhaustive list of the
libraries used throughout, presented alphabetically: 

* caret
* corrplot
* e1701
* httr
* RColorBrewer
* RCurl 
* rpart
* RSQite
* rvest
* scrapeR 
* splitstackshape
* stats
* tidyverse
* wesanderson

## Further Work
1. I would like to tighten up some of the scrapping, to create a more exhaustive list of power-users (users that have logged at least 100 games).
 This will involve creating a larger sample of users to begin with but will also involve modification of the second scraping script. 
 As it stands, it is difficult to get a  user/rating transaction per board game, so I would like to find a way to either manipulate the API in 
 a way I haven’t currently thought of in order to get that information, or to create a proxy for it. One promising idea for the proxy is to 
 modify the second script to get as many records of plays as possible per user,  bind this into a data frame, group by boardgame,  
 store the max count of plays as a new  feature,  summarize this information, arrange descending, and then only save the top 25. 
 This functions as a way to generate only the most useful games( in terms of enjoyment) per  user while not overly hindering the computation 
 of the recommender system, though there  might need to be some normalize this number by time passed since first play in order to avoid 
 temporal bias. 
2. Additionally, I would like to extend the board game characteristics to include games that have come out since 2016, which I have some promising 
leads for. This would create more value for the app by making its suggestions more contemporary. 
3. I would like to due some further research into recommender systems in order to optimize the development/design of the final product.
4. I would like to come up with an input system for the user to start entering their data / general UI design for the webapp, 
though this might have to be delayed another sprint. 


## Repository Layout
These folders contained the files necessaary to create a recommender system for 
boardgames, based off of webscraping of Boardgamegeek.com, which was performed 
using R.

### **EDA**
Contains a pdf export of the early EDA of the data, which was initially performe in R (see **Files**).  

### **Files**
Contains the end results of my scrapping, as well as my initial work with the data set(lab2(version2).R)

###  **Scraping** 
Contains the R scripts used to scrape boardgamegeek and create the files present in the files folder.

For additional information on each folder, see the readme files located in each.

The original data for all boardgames can be found [here](https://www.kaggle.com/gabrio/board-games-dataset)

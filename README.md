# Boardgame_rule_submission_Sprint_3

![titlepage](https://github.com/Fehiroh/Sprint_3/blob/master/Files/figures/Wheat%20from%20Chaff_%20Catan-Style.png)

For the repository information (what the folders are, what they contain), scroll down to the **Repository Layout** section

## Deliverables
### [Report](https://github.com/Fehiroh/Sprint_3/blob/master/sprint_3_report_actual.pdf)
### [Slideshow](https://docs.google.com/presentation/d/1IjqS0c9IrSU9zLHhI3vR-Se-s4pI8rofXUI-NmjC4NY/edit?usp=sharing)

## Overview: Purpose, and Progress
The purpose is to create a webapp that can sort board game rule submissions into those which are likely to be well received by the boardgame community versus those that aren't.

In Sprint 3, I have trained three classification (SVM, Random Forest, and Logistic Regression) algorithms to predict whether a game is likely to be well received based on its mechanics, themes, and play conditions.
 The success of each model has also been evaluated after dimensional reduction using feature importance (which was calculated via Random Forest), with SVM being the most successsful by far, with a (sensitivity + specificity) /2 of 0.955. 
 ![evaluation](https://github.com/Fehiroh/Sprint_3/blob/master/Files/figures/Wheat%20from%20Chaff_%20Catan-Style%20(1).png)
 The SVM model was integrated into a Shiny webapp which I developed to automatically sort boardgames upon submission. The underlying MySQL database that this application uses, as well as the storage of the pdfs according to the prediction of the model,  are stored in a private  Azure portal. Due to security risks of leaking actual NDA protected material that is stored in this portal,  neither is currenly publically accessible, but I plan to address that. 
 
### Images of the App's UI
#### Overall UI for Submission
![overall](https://github.com/Fehiroh/Sprint_3/blob/master/Files/figures/overallUI.PNG)
#### Some of the Features that Assist in Data Entry
![browse](https://github.com/Fehiroh/Sprint_3/blob/master/Files/figures/Categories_and_mech_selection.PNG)
Note: Not shown are the string cleaning and check functions in the Server Side that Assist with Data Integrity. 



## Requirements
In order to run the R files contained in this repository, one needs to have R version 3.6.0 or greater installed. The necessary libraries will all 
automatically install (if necessary) and load due to the usage of the p_load() function from the pacman package. Here is an exhaustive list of the
libraries used throughout, presented alphabetically: 

* the AzureR family
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
1. Protecting the Data base from SQL injection. 
2. Moving the Database to an Azure portal without any personal information.
3. Uploading to Shiny



## Repository Layout
These folders contained the files necessaary to create a recommender system for 
boardgames, based off of webscraping of Boardgamegeek.com, which was performed 
using R.

### **EDA**
Contains a pdf export of the early EDA of the data, which was initially performe in R (see **Files**).  

### **Files**
Contains the end results of my scrapping, as well as my initial work with the data set(lab2(version2).R)

###  **Scraping** 
Contains the R scripts used to scrape boardgamegeek and create the files present in the files folder.

For additional information on each folder, see the readme files located in each.

The original data for all boardgames can be found [here](https://www.kaggle.com/gabrio/board-games-dataset)

#Scraping

This folder, Scraping, contains two R scripts which were used to scrape BGG's
website and create csvs that will facillitate the recommender system.

The first file: actual_xml_scraping.R uses the "play?" query from BGG's website to 
create a list of 5000 valid usernames and their associated user_ids. 

Following that, scraping_for_transactions.R uses the csv created by  the first file 
to generate a dataframe that ties multiple games to each user. 
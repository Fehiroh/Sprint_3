
if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load("scrapeR", "rvest", "tidyverse", "RCurl", "httr")

# urls for scraping 



base_url <- "https://www.boardgamegeek.com/xmlapi2/"


play_url <- paste0(base_url, "plays?")

user_play_query <- paste0(play_url, "username=")



# if you want your own information, input the filepath to the csv / data created
# by the first xml srape, as generated by actual_xml_scraping.R
url_to_csv <- "https://raw.githubusercontent.com/Fehiroh/xml_scraping_BGG/master/list_of_users"
user_reference_df <- read_csv(url_to_csv)  
  # CHANGE_NECESSARY follow instructions above


# creating the dataframe to store the results of every iteration
plays_by_user_df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("username", "object_name", "object_id")
colnames(plays_by_user_df ) <- x



for (user in user_reference_df$username){
  
  test_request3 <- GET(paste0(user_play_query, user))
  test_data3 <- content(test_request3)
  
  
  items <- xml_find_all(test_data3, "///item")
  object_name_list <- xml_attr(items, "name")
  object_id_list <- xml_attr(items, "objectid")
  
  if( length(object_name_list) > 0 & length(object_id_list) > 0){
    if( length(object_name_list) > 25 & length(object_id_list) > 25){
      object_name_list <- object_name_list[1:25]
      object_id_list <- object_id_list[1:25]
  
    }
    played_by_user <- do.call(rbind, 
                              Map(data.frame, 
                                  object_name = object_name_list, 
                                  object_id = object_id_list))
  
    if(nrow(played_by_user) > 0) {
      
      played_by_user <- played_by_user %>% 
        mutate(username = user)
      
      
      plays_by_user_df <<- rbind(plays_by_user_df, played_by_user) 
    }
    }
}

write_csv(plays_by_user_df, "plays_by_user_df.csv") #CHANGE_NECESSARY, pull your filepath here


#"https://www.boardgamegeek.com/xmlapi2/plays?username=Animal82&id=13"
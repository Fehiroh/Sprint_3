
# libraries

if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load("scrapeR", "rvest", "tidyverse", "RCurl", "httr")



# urls for scraping 

base_url <- "https://www.boardgamegeek.com/xmlapi2/"

play_url <- paste0(base_url, "plays?")


# creating table to store database in 

actual_user_df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("username", "user_id", "game_id")
colnames(actual_user_df) <- x

# creation of database
for (i in 13:90000){
  
  # setting up http  
  id_variable <- i
  id_constant <- "id="
  id_phrase <- paste0(id_constant, id_variable)
  
  request_phrase <- paste0(play_url, id_phrase)
  
  # httr to get xml
  request <- GET(request_phrase)
  info <- content(request)
  
  
  #getting to the correct xml node, and getting the info wanted
  players <- xml_find_all(info, "///player ")
  username_list <- xml_attr(players, "username")
  user_id_list <- xml_attr(players, "userid")
  
  if( length(username_list) > 0 & length(user_id_list) > 0)
  #converting to df
  user_df <- do.call(rbind, Map(data.frame, username = username_list, user_id = user_id_list))
  
  if(nrow(user_df) > 0) {
   
     user_df <- user_df %>%
      filter(user_id != 0) %>% 
      mutate(game_id = i)
    
    actual_user_df <<- rbind(actual_user_df, user_df) %>% 
      distinct(username, user_id, game_id)
    
    non_duplicates <- actual_user_df %>% 
      distinct(username, user_id)
    
    
    if(length(non_duplicates$username) >= 5000){
      break()
    }
  }
}



# saving the results
write_csv(non_duplicates, "C:/Users/afehir/Documents/R/df_of_BGG_users.csv")

write_csv(actual_user_df, "C:/Users/afehir/Documents/R/df_of_BGG_users_with_game_id.csv")

boxplot(as.integer(non_duplicates$user_id))

summary(as.integer(non_duplicates$user_id))


non_duplicates$user_id_int <- as.double(as.character(non_duplicates$user_id))

boxplot(non_duplicates$user_id_int)

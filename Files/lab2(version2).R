
#let's get those libraries up and running ###
library(tidyverse)
library(RSQLite)
library(corrplot)
library(splitstackshape)
library(caret)
library(e1071)
library(stats)
library(rpart)
library(RColorBrewer)

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="C:/Users/fehi7/Downloads/database.sqlite")

# list all tables
tables <- dbListTables(con)

# exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

# create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", 
                                                           tables[[i]], "'", sep=""))
}

## okay,all of the tables that aren't lDataFrames[[1]] are included within table one, so I'm
## going to ignore them, and focus on lDataFrames[[1]] ### 
boardgame_data <- lDataFrames[[1]]


## let's take a quick look ## 

glimpse(boardgame_data)
dim(boardgame_data)
class(boardgame_data)
colnames(boardgame_data)

##Okay, and how much data is missing? ## 
summary(is.na(boardgame_data))
dat_missing <- colMeans(is.na(boardgame_data))
summary(dat_missing)
barplot(dat_missing, las=3, cex.names = .58, 
        ylab = "Proportion of Data Missing", ylim = c(0, 1))

##okay, wow, that't a lot of missing data. A lot of it seems to be for user-generated
## variables, like poll data, user comments, things like that, so lets remove 
## most of that ##

col_keep <- c("game.id", "details.name", "details.maxplayers",
              "details.minage", "details.yearpublished", 
              "stats.average","details.minplayers", "details.minplaytime",
              "details.maxplaytime", "polls.language_dependence" ,
              "stats.subtype.boardgame.bayesaverage", "stats.subtype.boardgame.pos", 
              "attributes.boardgameartist", "attributes.boardgamecategory", 
              "attributes.boardgamedesigner", "attributes.boardgamemechanic",
              "stats.average", "stats.averageweight", "stats.usersrated")
col_keep

#Also, while we are at it, let's remove video-games, and boardgame expansions ### 

game_data_clean <- boardgame_data %>% 
  filter(game.type == "boardgame", attributes.boardgameexpansion != TRUE) %>% # 
  select(col_keep)

glimpse(game_data_clean)

boardgame_data_clean <- boardgame_data %>% 
  filter(game.type == "boardgame", attributes.boardgameexpansion != TRUE) %>% # 
  select(col_keep)

head(boardgame_data_clean)

boardgame_data_clean$game.id <- as.integer(boardgame_data_clean$game.id)

dat_missing <- colMeans(is.na(boardgame_data))

clean_data_missing <- colMeans(is.na(boardgame_data_clean))


# the names of the games are important, but let's get them out of the way by turning them 
# into rownames. 
rownames(boardgame_data_clean) <- make.names(boardgame_data_clean$details.name, unique=TRUE)
boardgame_data_clean <- boardgame_data_clean %>% 
  select(-details.name)



## lets take a look at the dates, and see if they all line up, starting with year 
## of publication. 

summary(boardgame_data_clean$details.yearpublished)
summary(game_data_clean$details.yearpublished)

## Min is -3000, which I'm not keen on, lets look into that, and then remove everything ## 
## from earlier than 1800 ## 

early_games <- boardgame_data_clean %>%
  filter (details.yearpublished < 1900)

summary(early_games)

##  okay there's a lot of clasics, like chess, and Go here, but also alot of unknown games.
##  As you can't really bank on creating a classic on the scale of Backagammon, and these
##  early games are going to throw off EDA, let's remove them from our data-set

boardgame_data_clean <- boardgame_data_clean %>% 
  filter (details.yearpublished > 1900)

plot(details.yearpublished ~., boardgame_data_clean)

## Okay, that's a little better, there are still some massive outliers though, ## 
## particularily in minimum play time ## 
plot(details.yearpublished ~ details.minplaytime, boardgame_data_clean, log = "x")

boxplot(bdc6$details.maxplaytime ~  bdc6$stats.average)
  + ab_line(h=6.9)

##from personal experience, I know that the min.playtime for anything with critical
### reception is going to be less than 12 hours, with most games clocking in at 2-4, 
## and several heavier games around  the 5-10 mark.  So I'm going to remove anything 
## tha lasts longer than the large grouping before 1000 minutes, and I'm going to drop 
## the log scaling to make getting that cut-off number easier. 

plot(details.yearpublished ~ details.minplaytime, game_data_clean, 
     xlim = c(100,1000), ylim = c(1940, 2020))

##Okay, this confirms what I was thinking, so lets remove anything 
## that takes more than 600 minutes to play and anything that takes less than a minute 

boardgame_data_clean <- boardgame_data_clean %>% 
  filter (details.minplaytime <= 600, details.minplaytime >1) %>%
  filter (details.maxplaytime > 1, details.maxplaytime < 600)

# great! # 

summary(boardgame_data_clean)
plot(details.yearpublished ~., boardgame_data_clean)

## Still some weird stuff though. Neither the Mininum number of players, 
## nor the maximum number of players can actually be '0', so lets remove those 

boardgame_data_clean <- boardgame_data_clean %>% 
  filter (details.minplayers > 0, details.maxplayers > 0)

plot(details.yearpublished ~., boardgame_data_clean)

plot(details.yearpublished ~stats.usersrated, game_data_clean, log = "x", 
     ylim = c(1975,2020), xlab = "Users Rated (log)", ylab = "Year Published", 
     main = "Using User Rating to Filter out Fann-made Variations") + 
      abline(v= 20, col = "red", cex = 2)

summary(boardgame_data_clean)

hist(bdc6$details.yearpublished, breaks = 60, 
     main = "Temporal Distribution of Boardgame Publication", xlab= "Year Published")

hist(bdc6$stats.averageweight, breaks = 20, 
     main = "Distribution by  Boardgame Weight", xlab= "Boardgame Weight")

summary(bdc6$stats.averageweight)

goody_weighty <- lm(stats.average~stats.averageweight, bdc6)
goody_weighty 
plot(stats.average~stats.averageweight, bdc6, xlab = " Weight", ylab = "Average Rating", 
     main = "Relationship between Complexity and Ratings") + 
  abline ( reg = goody_weighty, col = "red")

summary(bdc6$details.yearpublished)

#So, how about this minimum age of 0, is that an error? 

plot(boardgame_data_clean$details.minage)
sum(boardgame_data_clean$details.minage == "0")
##251 games fall into this category, 

# okay, so it seems like minimum ago of zero might be a filler for games where the 
# minimum age isn't declared. 

#I'm going to print the name of games that fall into this category and see if I reognize 
# any, and see if it's temporally related. 

no_age_games <-  boardgame_data_clean %>%
  filter(boardgame_data_clean$details.minage == "0")

summary(no_age_games)

## distribution of publication year seems to match larger dataset. but the users rated
## seems much lower, which leads me to belive a lot of these are self-published or variants
## of existing games. Let's remove eveything with less than 20 user ratings since we 
## need to do that anyways, and it'll help us est the 'unpublished hypothesis'.

boardgame_data_clean <-  boardgame_data_clean %>%
  filter(stats.usersrated >= 20)

summary(boardgame_data_clean)

no_age_games <-  boardgame_data_clean %>%
  filter(boardgame_data_clean$details.minage == "0")

summary(no_age_games)

##looks like we cut out about 100 games by removing the ones without ratings, but we 
## have about 150 legitimate games that have invalid minimum player ages, let's return 
## to that later, for now, let's see if complexity weights of 0 are also fillers 

plot(boardgame_data_clean$stats.averageweight, main = "Assessing the Validity of 0 Weight Games", 
     ylab = "Average Weight")

## Okay, that's DEFINITELY  the case, so let's remove them 
boardgame_data_clean <-  boardgame_data_clean %>%
  filter(stats.averageweight >= 1)

summary(boardgame_data_clean)
dim(boardgame_data_clean)

clean_data_missing <- colMeans(is.na(boardgame_data_clean))
barplot(clean_data_missing)

plot(details.yearpublished~., boardgame_data_clean)

## Let's also remove anythng without mechanics or category ## 

bad_cat <- dplyr::filter(boardgame_data_clean,  !is.na(attributes.boardgamecategory))
bad_mech <- dplyr::filter(boardgame_data_clean,  !is.na(attributes.mechanics))

boardgame_data_clean2 <-  boardgame_data_clean %>%
  filter(-badcat)

# for reasons discussed in the paper, I'm removing boardgame artist and designer. 
#  language dependance is being removed because there are too many missing values 
boardgame_data_clean <-  boardgame_data_clean %>% 
  select(-polls.language_dependence,-attributes.boardgameartist, 
         -attributes.boardgamedesigner)

bgd_clean <-  boardgame_data_clean %>% 
              str_to_lower(.)
str_replace(colnames(boardgame_data_clean), ".", "_") %>% 
              str_replace(colnames(boardgame_data_clean), "attributes", "") %>% 
              str_replace(colnames(boardgame_data_clean),"stats", "") %>%
              str_replace(colnames(boardgame_data_clean),"boardgame", "") %>%
              str_replace(colnames(boardgame_data_clean),"polls", "") 


## let's try to get the categorical variables up and running starting with categories"

mechanic_with_dummies <- cSplit_e(data, "attributes.boardgamemechanic", type="character", fill=0, drop=TRUE)
category_with_dummies <- cSplit_e(data, "attributes.boardgamecategory", type="character", fill=0, drop=TRUE)

boardgame_data_clean <- cSplit_e(boardgame_data_clean, "attributes.boardgamemechanic", type="character", fill=0, drop=TRUE)
boardgame_data_clean<- cSplit_e(boardgame_data_clean, "attributes.boardgamecategory", type="character", fill=0, drop=TRUE)

write_csv(boardgame_data_clean, "boardgame_data_with_expanded.csv")

dim(category_with_dummies)

cor(boardgame_data_clean)

category_with_dummies <- category_with_dummies %>% 
  select(-attributes.boardgamemechanic)

plot(stats.average ~., data=category_with_dummies)
plot(stats.subtype.boardgame.pos ~.,data=category_with_dummies)

boxplot(stats.average ~ details.minplaytime, data=category_with_dummies)




##EDA ## 
# Having continuous variables for years is making the overarching temporal trends
# difficult to assess, so let's use dplyr to solve that issue 

plot <-  boardgame_data_clean %>% 
  mutate(discrete_years=as.factor(plyr::round_any(as.numeric(details.yearpublished), 5))) %>%
  mutate(discrete_weights=as.factor(plyr::round_any(as.numeric(stats.averageweight), .5))) %>% 
  filter(as.numeric(details.yearpublished) >=1900 & as.numeric(details.yearpublished) <= 2016)


## ratings over time  ### 
 ggplot(plot, aes(discrete_years, stats.average, fill=discrete_years)) +
  geom_boxplot(alpha=.4) +
  theme_bw() +
  ylab("Avg. Rating") + xlab("Year Groups") +
  geom_hline(yintercept=mean(bdc6$stats.average, na.rm=TRUE), color="black") +
  theme(legend.position="none")


##Position over time ## 
ggplot(plot, 
       aes(discrete_years, stats.subtype.boardgame.pos, fill=discrete_years)) +
  geom_boxplot(alpha=.4) +
  theme_bw() +
  ylab("Position") + xlab("Year Groups") +
  geom_hline(yintercept=mean(plot$stats.subtype.boardgame.pos, na.rm=TRUE), color="black") +
  theme(legend.position="none")

#weight over time ##
ggplot(plot, 
       aes(discrete_years, stats.averageweight, fill=discrete_years)) +
  geom_boxplot(alpha=.4) +
  theme_bw() +
  ylab("Weight (1 to 5)") + xlab("Year Groups") +
  geom_hline(yintercept=mean(plot$stats.averageweight, na.rm=TRUE), color="black") +
  theme(legend.position="none")
        
boxplot(stats.average ~  discrete_years, data = plot,
        xlab = "Year of Publication (5 year groups)", ylab = "Average Rating ", 
        main = "The Relationship between Boardgame Ratings and Time") + 
  abline( h = mean(plot$stats.average))

 ## Interest
ggplot(plot, 
       aes(discrete_weights, stats.average)) +
  geom_boxplot(alpha=.4) +
  theme_bw() +
  ylab("Average Rating") + xlab("Binned Average Weight") +
  geom_hline(yintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black") +
  theme(legend.position="none")

## maximum time ## 
ggplot(plot, 
       aes(details.maxplaytime, stats.average, colour = stats.average )) +
  geom_jitter(alpha=.4) +
  geom_smooth() +
  theme_bw() +
  ylab("Average Rating") + xlab("Maximum Playtime") +
  geom_hline(yintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black") +
  theme(legend.position="right") +
  scale_x_log10() + 
  scale_color_gradient( low = "blue", high = "yellow")

# Minimum Time # 
ggplot(plot, 
       aes(details.minplaytime, stats.average, colour= stats.averageweight)) +
  geom_jitter(shape = 1, width = .2) +
  geom_smooth () + 
  theme_bw() +
  ylab("Average Rating") + xlab("Minimum Playtime") +
  geom_hline(yintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black") +
  theme(legend.position="right") +
  scale_x_log10() +
  scale_color_gradient( low = "yellow", high = "purple")


names(plot)
cat_and_stats <- c(64:145)
cat_col <- plot[,cat_and_stats]
n <- colnames(cat_col)

ggplot(plot, 
       aes(stats.averageweight, stats.average, fill=stats.averageweight, group= stats.averagweight)) +
  geom_boxplot(alpha=.4) +
  theme_bw() +
  ylab("Weight (1 to 5)") + xlab("Year Groups") +
  geom_hline(yintercept=mean(boardgame_data_clean$stats.averageweight, na.rm=TRUE), color="black") +
  theme(legend.position="none")




boxplot(just_stats$stats.averageweight ~ just_stats$time_spread)

# time_spread seems to correlate with weight, so the longer something takes, 
# generally the more complicated it is. 

boxplot(just_stats$stats.averageweight ~ just_stats$player_spread)

boardgame_data_clean2 <- boardgame_data_clean

colnames(boardgame_data_clean2) <- gsub(" ", "", colnames(boardgame_data_clean2))
colnames(boardgame_data_clean2) <- gsub("/", "-", colnames(boardgame_data_clean2))
colnames(boardgame_data_clean2) <- gsub("[[:punct:]]", "_", colnames(boardgame_data_clean2))


## let's see how correlated our non-categorical variables are by making a new 
# df called "just_stats"

just_stats <- boardgame_data_clean %>% 
  select(-starts_with("attributes")) %>% 
  mutate(player_spread = (details.maxplayers-details.minplayers)) %>% 
  mutate(time_spread = (details.maxplaytime-details.minplaytime))

mech_only$stats.average <- boardgame_data_clean$stats.average

hist(bdc5$stats.average, main = "Distribution of Average Ratings", 
     xlab = "Average Rating", breaks = 40)
hist(as.double(plot$discrete_weights))

names(plot)

summary(just_stats$player_spread)

boxplot (just_stats$stats.average ~just_stats$player_spread)
        
boxplot (just_stats$stats.average ~just_stats$time_spread,
         main="Averge Rating by Time Spread", xlab="Time Spread (in minutes)", 
         ylab = "Average Rating") + 
  abline( h = mean(boardgame_data_clean$stats.average))
        
boxplot (boardgame_data_clean$stats.average ~boardgame_data_clean$details.maxplayers,
         main="Averge Rating by Maximum Players", xlab="Maximum Players", 
         ylab = "Average Rating") + 
  abline( h = mean(boardgame_data_clean$stats.average))

summary(bdc6$stats.average)




ggplot(plot, aes(discrete_years, stats.average, fill=discrete_years)) +
  geom_boxplot(alpha=.4) +
  theme_bw() +
  ylab("Avg. Rating") + xlab("Year Groups") +
  geom_hline(yintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black") +
  theme(legend.position="none")

boardgame_data_clean$time_spread <- just_stats$time_spread
boardgame_data_clean$player_spread <- just_stats$player_spread

# seems like it doesn't really matter how much the player spread is, but average rating 
# might be correlated to time, with more recent games generally earning better reviews
# whether that is indicative of biases in the rating system, an increase in game quality,
# or a combination of the two is unknown. 

lapply(c(cat_col),function(col) { ggplot(cat_col$stats.average, aes_string(cat_col)) + 
    geom_boxplot() + coord_flip()
                  })
### How correlated are our non-categorical variables? ## 
just_stats$stats.average <- as.numeric(just_stats$stats.average)
colnames(just_stats)
gone = c(1,9:10)
stats_minus_id <- just_stats[,-gone]
correl <- cor(stats_minus_id)
corrplot(correl, method = "shade", type = "full",order = "hclust",
         tl.cex = .65, tl.col = "black")



colnames(mech_only) <- str_replace(colnames(mech_only), 
                                  "attributes.boardgamemechanic_", 
                                  "")

mech_only$stats.average <- as.numeric(mech_only$stats.average)
mech_correl <- cor(mech_only)
corrplot(mech_correl, method = "shade", type = "full",order = "hclust",
         tl.cex = .2, tl.col = "black")



colnames(cat_only) <- str_replace(colnames(cat_only), 
                                  "attributes.boardgamecategory_", 
                                  "")
cat_only$stats.average <- as.numeric(boardgame_data_clean$stats.average)
cat_only$stats.averaage <- NULL
cat_correl <- cor(cat_only)
cat_correl

par( mar= c(6, 4, 4, 2) + 0.1)

stats_to_cats <-data.frame(cat_correl) %>% 
  select (stats.average)

stats_to_cats
plot(stats_to_cats, xlim =c(-0.3, 0.3))


corrplot(stats_to_cats, method = "shade", type = "full",order = "hclust",
         tl.cex = .4, tl.col = "black")


summary(boardgame_data_clean$stats.average)
## The only variable-pairs that have potentially problematic correlations are
#   maxplayers-player_spread and  minplaytime-maxplaytime. 




## so, let's definite a successful game as one that is over median and mode of 
## the averagerating ei. >= 6.9, and make a function to check that, called 
#  success_check

success_check <- function(x){
  if(input >= 6.9){
    output <- 1
  } else{
    output <- 0
  }
  return (output)
}

col_quality_check <- function(data, col_being_checked = c("whatever"){
  positive_rows <- data %>% 
     filter(col_being_checked == 1)
  summary (positive_rows$stats.average)
}


col_quality_check(boardgame_data_clean, attributes.boardgamecategory_))

positive_rows <- boardgame_data_clean %>% 
  filter(attributes.boardgamecategory_Adventure == 1)

summary(positive_rows$stats.average)

test_cat_and_mech <- boardgame_data_clean %>% 
  select(-c(1:5), -149, -150)

barplot(lapply(test_cat_and_mech, col_quality_check))

success_check <- function(x){
  if(x >= 6.9){
    output <- 1
  } else{
    output <- 0
  }
  return (output)
}

boardgame_data_clean$successful <- as.integer(map(
  boardgame_data_clean$stats.average, success_check))
class(boardgame_data_clean$successful)

boardgame_data_clean <- boardgame_data_clean %>% 
  mutate(successful = success_check(stats.average))



boxplot(boardgame_data_clean2$stats_average ~
          boardgame_data_clean$`attributes.boardgamemechanic_Action / Movement Programming`)

## Action / Movemnt prograamming is better than average 


## So, how man boardgames are in each category? ## 
category <- boardgame_data_clean %>%
  select(starts_with('attributes.boardgamecategory')) 
  
colnames(category) <- str_replace(colnames(category), 
            "attributes.boardgamecategory_", 
            "")
category_count <- apply(category, 2,  sum)
category_count <- sort(category_count, decreasing = TRUE)
barplot(category_count, names.arg = FALSE)

par(mar=c(11,5,4,4))
category_count_plot <-barplot(category_count[0:10], las=3, cex.names = 1.5, ylim =c(0, 1000), 
                              ylab = "Number of Games in Category", cex.lab = 2) 
category_count_plot2 <-barplot(category_count[11:20], las=2, cex.names = 1.5, ylim =c(0, 1000), 
                               ylab = "Number of Games in Category") 
category_count_plot3 <-barplot(category_count[21:30], las=2, cex.names = 1, 
                               ylim =c(0, 1000),  ylab = FALSE ) 

## and how many boardgames have each mechanic ? ## 
  
mechanic <- boardgame_data_clean %>%
  select(starts_with('attributes.boardgamemech')) 


colnames(mechanic) <- str_replace(colnames(mechanic), 
                                  "attributes.boardgamemechanic_", 
                                  "")
mechanic_count <- apply(mechanic, 2,  sum)
mechanic_count <- sort(mechanic_count, decreasing = TRUE)
mechanic_count


barplot(mechanic_count, names.arg = FALSE) 
mechanic_count_plot <-barplot(mechanic_count[0:10], las=2, cex.names = .8, ylim =c(0, 1000), 
                              ylab = "Number of Games with Mechanic") 
mechanic_count_plot2 <-barplot(mechanic_count[11:20], las=2, cex.names = .8, ylim =c(0, 1000), 
                              ylab = "Number of Games with Mechanic")
mechanic_count_plot <-barplot(mechanic_count[21:30], las=2, cex.names = .8, ylim =c(0, 1000), 
                              ylab = "Number of Games with Mechanic")


names(mechanic_count)

cat_only <- boardgame_data_clean %>%
  select(starts_with("attributes.boardgamecat"))

cat_only$cat_sum <- rowSums(cat_only)
boardgame_data_clean$cat_sum <- cat_only$cat_sum
         
boxplot(boardgame_data_clean$stats.average~boardgame_data_clean$cat_sum)
#Number of Categories does not seems to correlate to stats.average

mech_only <- boardgame_data_clean %>%
  select(starts_with("attributes.boardgamemech"))

mech_only$mech_sum <- rowSums(mech_only)
boardgame_data_clean$mech_sum <- mech_only$mech_sum

boxplot(boardgame_data_clean$stats.average~boardgame_data_clean$details.minage, 
        main = "Relationship between Minimum Age and Average Rating \n(0 = No Minimum Age)",
        ylab = "Average Rating", xlab = "Minimum Recommended Age") + 
        abline(h = mean(boardgame_data_clean$stats.average))

boxplot(boardgame_data_clean$stats.average~boardgame_data_clean$mech_sum, 
        main = "Relationship between Number of Mechanics and Average Rating",
        ylab = "Average Rating", xlab = "Total Number of Mechanics") + 
        abline(h = mean(boardgame_data_clean$stats.average))
  
# Number of mechanics does seem to correlate to stats.average



                ############################
                #                          #
                #         MODELING         #
                #                          #
                ############################


# I've decided to use three different models to address this business problem:

# 1) DECISION TREE, since it's simple to interpret, and deals well w/ non-linearity 
#         and complex relationships between the IV and the DVs
# 2) SVM, since it's good at multi-dimensional categorization, I'm ultimately
#         trying to categorize new input as either a) likely to be successful, or 
#         b) likely to be unsucessful,. Additionally, the data has a lot of variables
#         so this is also a good fit.
# 3) LOGISTIC REGRESSION, since Log Reg is a staple for any data analysis, and the 
#         end goal is to calculate the probability of a binary outcome, Log Reg 
#         seemed like an obvious choice. Because it is a GLM, it also helps dodge 
#         some of the issues we might have run into using a regular linear regression.


# Quick modular function for confusion matrices # 
conf_mat_plot <- function(x1, x2, x3, x4, title){
  Actual <- factor(c(0, 0, 1, 1))
  Predicted <- factor(c(0, 1, 0, 1))
  Y      <- c(x1, x2, x3, x4)
  X <- sum(x1, x2, x3, x4)
  df <- data.frame(Actual, Predicted, Y)
  
  df <- df %>% 
    mutate(percentage = Y/X*100)
  
  ggplot(data =  df, mapping = aes(x = Actual, y = Predicted)) +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", percentage)),
              colour = "red", size = 8) +
    scale_fill_gradient(low = "white", high = "black") +
    theme_bw() + theme(legend.position = "none",
                       plot.title = element_text(size = rel(1.4), hjust = 0.5), 
                       axis.title = element_text(size = rel(1.25)), 
                       axis.text = element_text(size= rel(1.25))) + 
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(levels(Predicted))) + 
    ggtitle(title) 
}




## Okay, it's time to split the data into testing and training 
## let's start by making the df name a little less wordy 

bdc <- na.omit(boardgame_data_clean)
bdc$details.name <- NULL
names(bdc) <- make.names(names(bdc))
colnames(bdc) <- make.names(colnames(bdc))
bdc$successful <- NULL
bdc$stats.subtype.boardgame.pos <- NULL
bdc$stats.subtype.boardgame.bayesaverage <- NULL
bdc$stats.usersrated <- NULL
bdc$game.id <- NULL


test_bdc <- bdc
test_bdc[1:10] <- scale(test_bdc[1:10])
test_bdc[144:145] <- scale(test_bdc[144:145])
test_bdc[11:143] <- map(test_bdc[11:143], as.factor)
test_sample = sample.split(test_bdc, SplitRatio = .75)
test_train = subset(test_bdc, sample == TRUE)
test_test  = subset(test_bdc, sample == FALSE)



## and lets split bdc into two sets for training and testing at a 3:1 ratio

require(caTools)
set.seed(101) 
og_sample = sample.split(bdc, SplitRatio = .75)
og_train = subset(bdc, sample == TRUE)
og_test  = subset(bdc, sample == FALSE)


# What do the models have to beat?    # 

# Baseline model - predict the mean of the training data
best.guess <- mean(train$stats.average) 

# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-test$stats.average)^2))
RMSE.baseline    # 0.702021


MAE.baseline <- mean(abs(best.guess-test$stats.average))
MAE.baseline    # 0.550675

                    # DECISION TREE 
install.packages("tree")
install.packages("randomForest")
require(tree)
require(randomForest)

rndm_frst_model <- randomForest(stats.average~., data= (train), importance=TRUE,
                                proximity=TRUE)

plot(rndm_frst_model)

print(rndm_frst_model)


rndm_frst_pred <- predict(rndm_frst_model, test, type="response")
summary(rndmn_frst_pred)

plot(rndn_frst_pred)

RMSE.rf_pred <- sqrt(mean((rndm_frst_pred-test$stats.average)^2))
print(RMSE.rf_pred) #0.405816

MAE.rtree <- mean(abs(rndmn_frst_pred-test$succesful))
MAE.rtree #NaN


RMSE.rf_pred <- sqrt(mean((rndm_frst_pred-test$stats.average)^2))
print(RMSE.rf_pred) #0.405816

MAE.rtree <- mean(abs(rndmn_frst_pred-test$succesful))
MAE.rtree #NaN

#  finding and removing the bad variables 
imp <- round(importance(rndm_frst_model), 2)
bad_cols <- which(imp < 0)
bad_cols 

colnames(train)

bdc2 <- bdc %>% 
    select(-11, -21, -23, -30,  -35,  -37,  -40,  -50,  -52,  -54,  -66,  -68,  -69,  -70,  
           -71,  -78, -85,  -87,  -91,  -99, -100, -105, -107, -108, -114, -116, -118, -120,
           -126, -128, -131, -132, -139, -140)

sample = sample.split(bdc2, SplitRatio = .75)
train = subset(bdc2, sample == TRUE)
test  = subset(bdc2, sample == FALSE)
glimpse(bdc5)
bdc6 <- bdc5

write_csv(bdc6, "boad_gaames_dim_reduced.csv")
colnames(bdc6) <- str_replace(colnames(bdc6), 
                                   "attributes.boardgame", 
                                   "")

colnames(bdc6)
rndm_frst_model2 <- randomForest(stats.average~., data= (train), importance=TRUE,
                                proximity=TRUE)


rndm_frst_pred2 <- predict(rndm_frst_model2, test, type="response")
summary(rndm_frst_pred2)

RMSE.rf_pred <- sqrt(mean((rndm_frst_pred2-test$stats.average)^2))
print(RMSE.rf_pred) # 0.5592058


MAE.rtree <- mean(abs(rndm_frst_pred2-test$succesful))
MAE.rtree #NaN

plot(rndm_frst_model2)

round(importance(rndm_frst_model2), 2)

imp2 <- round(importance(rndm_frst_model2), 2)
bad_cols2 <- which(imp2 < 0)
bad_cols2

bdc3 <- bdc2 %>% 
  select(-10, -14,  -19,  -20,  -21,  -26,  -29,  -33,  -34,  -42,  -44,  -54,  -64,
         -70,  -71,  -80,  -83,  -91,  -94,  -97, -100, -105)

sample = sample.split(bdc3, SplitRatio = .75)
train = subset(bdc3, sample == TRUE)
test  = subset(bdc3, sample == FALSE)

bestmtry <- tuneRF(test[,-4], test[,4], stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry) #mtry of 24 has lowest OOBError

rndm_frst_model3 <- randomForest(stats.average~., data= (train), importance=TRUE,
                                 proximity=TRUE, mtry=24)

rndm_frst_pred3 <- predict(rndm_frst_model3, test, type="response")

RMSE.rf_pred3 <- sqrt(mean((rndm_frst_pred3-test$stats.average)^2))
print(RMSE.rf_pred3) #  0.5469902

round(importance(rndm_frst_model3), 2)

imp3 <- round(importance(rndm_frst_model3), 2)
bad_cols3 <- which(imp3 < 0)
bad_cols3


# Round 4, removing the results of bad_cols3# 
bdc4 <- bdc3 %>% 
  select(-12, -16, -20, -22, -32, -33, -42, -44, -49, -50, -63, -66,
         -67, -72, -73, -74, -75, -78)

sample = sample.split(bdc4, SplitRatio = .75)
train = subset(bdc4, sample == TRUE)
test  = subset(bdc4, sample == FALSE)

rndm_frst_model4 <- randomForest(stats.average~., data= (train), importance=TRUE,
                                 proximity=TRUE, mtry=22, node_size = 3)

imp4 <- round(importance(rndm_frst_model4), 2)
bad_cols4 <- which(imp4 < 0)
bad_cols4



RMSE.rf_pred4 <- sqrt(mean((rndm_frst_pred4-test$stats.average)^2))
print(RMSE.rf_pred4) # 0.5484522

rndm_frst_pred4 <- predict(rndm_frst_model4, test, type="response")

bestmtry <- tuneRF(test[,-4], test[,4], stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

# Round 5 # 
bdc5 <- bdc4 %>% 
  select(-11, -17, -18, -20, -27, -29, -32, -33, -34, -36, -40, -45, -49, -52, 
         -53, -58, -60)

sample5 = sample.split(bdc5, SplitRatio = .75)
train5 = subset(bdc5, sample == TRUE)
test5  = subset(bdc5, sample == FALSE)

rndm_frst_model5 <- randomForest(stats.average~., data= (train5), importance=TRUE,
                                 proximity=TRUE, mtry=24)



RMSE.rf_pred5 <- sqrt(mean((rndm_frst_pred5-test$stats.average)^2))
print(RMSE.rf_pred5) #  0.549851

round(importance(rndm_frst_model5), 2)
plot(rndm_frst_model5)
print(rndm_frst_model5)

test5$predicted_response <- predict(rndm_frst_model5, test5)

test$predicted_successful <-NULL 
test$actual_successful <- NULL 

test5 <- test5 %>% 
  mutate(actual_successful = factor(unlist(map(test$stats.average, success_check))))
  
test5 <- test5 %>% 
  mutate(predicted_successful = factor(unlist(map(test$predicted_response, success_check))))


confusionMatrix(data = test5$predicted_successful,  
                  reference = test5$actual_successful,
                  positive = '1')

 # confusion Matrix for Random Forest 

#          Reference
#                 0   1
# Prediction  0 316 115
#             1  71 242

#Accuracy : 0.75            
#95% CI : (0.7173, 0.7807)
#No Information Rate : 0.5202          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.4968          
#Mcnemar's Test P-Value : 0.001616        

#Sensitivity : 0.6779          
#Specificity : 0.8165          
#Pos Pred Value : 0.7732          
#Neg Pred Value : 0.7332          
#Prevalence : 0.4798          
#Detection Rate : 0.3253          
#Detection Prevalence : 0.4207          
#Balanced Accuracy : 0.7472          

#'Positive' Class : 1      

         #      Confusion Matrix (%) for Random Forest (5th iteration)
rf_conf_mat_plot <- conf_mat_plot(316, 115, 71, 242, 
                                   "Confusion Matrix (%) for Random Forest (5th Iteration)")
rf_conf_mat_plot

      ###              Logistic                     ####


##          Attempt with the dimension reduction 

glm_train <- train 
glm_test <- test 

glm_train <- glm_train %>% 
  mutate(actual_successful = factor(unlist(map(stats.average, success_check))))

names(glm_train)

scaled_glm_train <- glm_train
scaled_glm_train[1:9] <- scale(scaled_glm_train[1:9])
scaled_glm_train[53:54] <- scale(scaled_glm_train[53:54])
scaled_glm_train[10:52] <- map(scaled_glm_train[10:52], as.factor)
scaled_glm_train <- scaled_glm_train[-c(4)]

scaled_glm_test <- glm_test
scaled_glm_test[1:9] <- scale(scaled_glm_test[1:9])
scaled_glm_test[53:54] <- scale(scaled_glm_test[53:54])
scaled_glm_test[10:52] <- map(scaled_glm_test[10:52], as.factor)
scaled_glm_test <- scaled_glm_test[-c(4, 55:56)]


glm_model <- glm(actual_successful~., family=binomial(link="logit"), scaled_glm_train)

summary(glm_model)

options(warn=-1)      #turn off warnings
glm_results <- predict(glm_model, scaled_glm_test)
options(warn=1)

prob_check_fifty <- function(x){
  output <- 0
  if(x > .5){
    output <- 1
  } 
  return (output)
}


fifty_glm_results <- lapply(glm_results, prob_check_fifty)

scaled_glm_test$fifty_percent_results <- factor(unlist(fifty_glm_results))

confusionMatrix(data = scaled_glm_test$fifty_percent_results, 
                reference = glm_test$actual_successful,
                positive = '1')
?confusionMatrix
#Confusion Matrix and Statistics

#       Reference
      #   0   1
      #0 328 165
      #1  59 192

#Accuracy : 0.6989          
#95% CI : (0.6645, 0.7317)
#No Information Rate : 0.5202          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.3898          
#Mcnemar's Test P-Value : 2.29e-12        

#Sensitivity : 0.8475          
#Specificity : 0.5378          
#Pos Pred Value : 0.6653          
#Neg Pred Value : 0.7649          
#Prevalence : 0.5202          
#Detection Rate : 0.4409          
#Detection Prevalence : 0.6626          
#Balanced Accuracy : 0.6927          

#'Positive' Class : 0    

glm_conf_mat_plot <- conf_mat_plot(328, 165, 59, 192, 
                                   "Confusion Matrix (%) for Logistic Regression")
glm_conf_mat_plot

#             Attempt without dimension reduction 

og_glm_train <- og_train 
og_glm_test <- og_test 

og_glm_train <- og_glm_train %>% 
  mutate(actual_successful = factor(unlist(map(stats.average, success_check))))

names(og_glm_train)

og_scaled_glm_train <- og_glm_train
og_scaled_glm_train[1:10] <- scale(og_scaled_glm_train[1:10])
og_scaled_glm_train[144:145] <- scale(og_scaled_glm_train[144:145])
og_scaled_glm_train[11:143] <- map(og_scaled_glm_train[11:143], as.factor)
og_scaled_glm_train <- og_scaled_glm_train[-c(4)]

og_scaled_glm_test <- og_glm_test
og_scaled_glm_test[1:10] <- scale(og_scaled_glm_test[1:10])
og_scaled_glm_test[144:145] <- scale(og_scaled_glm_test[144:145])
og_scaled_glm_test[11:143] <- map(og_scaled_glm_test[11:143], as.factor)
og_scaled_glm_test <- og_scaled_glm_test[-c(4)]

og_glm_model <- glm(actual_successful~., family=binomial(link="logit"), og_scaled_glm_train)

summary(og_glm_model)

options(warn=-1)      #turn off warnings
og_glm_results <- predict(og_glm_model, og_scaled_glm_test)
options(warn=1)

og_fifty_glm_results <- lapply(glm_results, prob_check_fifty)
og_scaled_glm_test$fifty_percent_results <- factor(unlist(og_fifty_glm_results))


                             ####    SVM     ###

svm_train <- train 
svm_test <- test


# turn stats average into categorical data #
svm_train <- svm_train %>% 
  mutate(actual_successful = factor(unlist(map(stats.average, success_check))))

svm_train$stats.average <- NULL
svm_test$col_act <- NULL 

# Make model 
svm_model <- svm(actual_successful~., svm_test)

glimpse(svm_train)
glimpse(svm_test)

summary(svm_model)
print(svm_model)



svm_test_predicted<- predict(svm_model, svm_test)
svm_test$predicted_response <- predict(svm_model, svm_test)

svm_conf_mat <- confusionMatrix(data = svm_test$predicted_response, 
                reference = svm_test$actual_successful,
                positive = '1')

#                Reference
#                   0   1
# Prediction    0 368  15
#               1  19 342

#Accuracy : 0.9543          
# 95% CI : (0.9367, 0.9681)
# No Information Rate : 0.5202          
# P-Value [Acc > NIR] : <2e-16          

# Kappa : 0.9085          
# Mcnemar's Test P-Value : 0.6069          

#Sensitivity : 0.9580          
#Specificity : 0.9509          
#Pos Pred Value : 0.9474          
#Neg Pred Value : 0.9608          
#Prevalence : 0.4798          
#Detection Rate : 0.4597          
#Detection Prevalence : 0.4852          
# Balanced Accuracy : 0.9544          

# Positive' Class : 1               

# 95 % Accuracy, clear winner 

svm_conf_mat_plot <- conf_mat_plot(368, 15, 19, 342, " Confusion Matrix (%) for SVM")
svm_conf_mat_plot

sum_actual <- mean(as.numeric(test$actual_successful))
s


model_performance <- data.frame(read.csv("Laab2_model_Perf.csv"))

.479839

ggplot(model_performance, 
       aes(model, specifi, colour= discrete_weights)) +
  geom_jitter(alpha=.4) +
  theme_bw() +
  ylab("Average Rating") + xlab("Maximum Playtime") +
  geom_hline(yintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black") +
  theme(legend.position="right") +
  scale_x_log10()


ordered_svm_test <- test[order(svm_test$predicted_response),]


lift.chart(c("CCS.glm", "CCS.rpart"), data=CCSVal, targLevel="Yes",
            trueResp=0.01, type="cumulative", sub="Validation")

layout(matrix(c(1,2), 2, 1))
data(CCS)
CCS$Sample <- create.samples(CCS, est=0.4, val=0.4)
CCSEst <- CCS[CCS$Sample == "Estimation",]
CCS.glm <- glm(MonthGive ~ DonPerYear + LastDonAmt + Region + YearsGive,
               family=binomial(logit), data=CCSEst)
library(rpart)
CCS.rpart <- rpart(MonthGive ~ DonPerYear + LastDonAmt + Region + YearsGive,
                   data=CCSEst, cp=0.0074)
CCSVal <- CCS[CCS$Sample == "Validation",]
lift.chart(c("CCS.glm", "CCS.rpart"), data=CCSVal, targLevel="Yes",
           trueResp=0.01, type="cumulative", sub="Validation")
lift.chart(c("CCS.glm", "CCS.rpart"), data=CCSVal, targLevel="Yes",
           trueResp=0.01, type="incremental", sub="Validation")

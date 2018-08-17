#Declaring required packages
library(recommenderlab)
library(ggplot2)
library(dplyr)
library(caret)

#Loading data from from data set to df
setwd("C:/Bhumit_Documents/Upgrad/Domain Elective/M3 Assignment - Recommendation System")

beer_df <- read.csv("beer_data.csv")

#--------------------------------------------------------------------------------------------------------
#-----------------------------------1) Data preparation--------------------------------------------------
#--------------------------------------------------------------------------------------------------------

#1.1 Choose only those beers that have at least N number of reviews
# Answer:  N = 50

total_row = nrow(beer_df)
total_row #475984

#Removing rows with review_overall = 0
beer_df <- beer_df %>% filter(review_overall != 0, review_overall != "", review_profilename != "")
total_row = nrow(beer_df)
total_row #475878

#Getting the distinct rows i.e remove the same user for same beer_id
beer_df <- distinct(beer_df, beer_beerid, review_profilename, .keep_all = TRUE)
total_row = nrow(beer_df)
total_row #474456

#-----------------------Calculating N for beer_beerid --------------------

View(beer_df)

reviews_groupby_beerid <- beer_df %>% group_by(beer_beerid) %>% summarise(  NumberOfReview_beerid = n() )

Unique_beer_count = nrow(reviews_groupby_beerid)
Unique_beer_count #40302

summary(reviews_groupby_beerid)
head(reviews_groupby_beerid)



#Plotting number of reviews for each beer
ggplot(reviews_groupby_beerid, aes(x=reviews_groupby_beerid$beer_beerid, y = reviews_groupby_beerid$NumberOfReview_beerid,fill="Number of Reviews")) + geom_bar(stat = "identity")
#So here from Bar Cart we can see one Beer havign 750+ reviews and marjority having number of reviews between 0 to 250


#plotting Histogram with Beer Id on X Axis
ggplot(data=reviews_groupby_beerid, aes(reviews_groupby_beerid$beer_beerid)) +  geom_histogram(bins=1000) 
# Here we can see majority of the Beer Id columns are touching the 50 Reviews Mark
View(reviews_groupby_beerid)
# Fetching Beers which have more than 50 reviews
beers_reviewed_50 = reviews_groupby_beerid %>%  filter(reviews_groupby_beerid$NumberOfReview_beerid >= 50)
nrow(beers_reviewed_50)
#it fetches 2064 rows which should be good enough for futhers analysis

#Merging based on beer_beerid with the original data frame
df_matrix = merge( beer_df, beers_reviewed_50, by = "beer_beerid")
View(df_matrix)

#-----------------------Calculating N for review_profilename --------------------

reviews_groupby_profilename <- beer_df %>% group_by(review_profilename) %>% summarise(  NumberOfReview_profilename = n() )

Unique_Profilename_count = nrow(reviews_groupby_profilename)
Unique_Profilename_count #22497

summary(reviews_groupby_profilename)
head(reviews_groupby_profilename)

#Plotting number of reviews made by each profilename
ggplot(reviews_groupby_profilename, aes(x=contreviews_groupby_profilename$review_profilename, y = reviews_groupby_profilename$NumberOfReview_profilename,fill="Number of Reviews")) + geom_bar(stat = "identity") + xlab("Profile Names") + ylab("Number Of Reviews")
#So here from Bar Cart we can see Hghest User Reviews cross 1500 reviews and marjority having number of reviews between 0 to 100


#plotting Histogram with Reviews Count on X Axis
ggplot(data=reviews_groupby_profilename, aes(reviews_groupby_profilename$NumberOfReview_profilename)) + geom_histogram(bins=100) + xlim(0,100)  #geom_histogram(stat="count") 
# Here we can see majority of the Reviews have made less than 100 number of reviews

# Fetching review_profilename which have more than 100 reviews
reviews_profilename_100 = reviews_groupby_profilename %>%  filter(reviews_groupby_profilename$NumberOfReview_profilename >= 100)
nrow(reviews_profilename_100)
#it fetches 1149 rows only

# Fetching review_profilename which have more than 50 reviews
reviews_profilename_50 = reviews_groupby_profilename %>%  filter(reviews_groupby_profilename$NumberOfReview_profilename >= 50)
nrow(reviews_profilename_50)
#it fetches 2186 rows which should be good enough for futhers analysis

View(reviews_profilename_50)

#Merging based on review_profilename with the data frame where which already have beer_beerid > 50
df_matrix = merge( df_matrix, reviews_profilename_50, by = "review_profilename")
View(df_matrix)




#1.2 Convert this data frame to a "realratingMatrix" before you build your collaborative filtering models


class(df_matrix)
beer_matrix <- as(df_matrix, "realRatingMatrix")

class(beer_matrix)

str(beer_matrix)

# get some informtaion
dimnames(beer_matrix)
rowCounts(beer_matrix)
colCounts(beer_matrix)
rowMeans(beer_matrix)

#--------------------------------------------------------------------------------------------------------
#-----------------------------------2) Data Exploration--------------------------------------------------
#--------------------------------------------------------------------------------------------------------


#2.1 Determine how similar the first ten users are with each other and visualise it
similar_users <- similarity(beer_matrix[1:10, ],
                            method = "cosine",
                            which = "users")


#Similarity matrix for users
as.matrix(similar_users)


#Visualise User Similarity
image(as.matrix(similar_users), main = "User similarity")



#2.1 Compute and visualise the similarity between the first 10 beers
similar_beers <- similarity(beer_matrix[,1:10 ],
                            method = "cosine",
                            which = "items")

#Similarity matrix for Beers
as.matrix(similar_beers)

#Visualise similarity matrix
image(as.matrix(similar_beers), main = "Beer similarity")


#2.3 What are the unique values of ratings?

# coerce the matrix to a dataframe
beer_df_coerce <- as(beer_matrix, "data.frame")
str(beer_df_coerce)

uniqueRatings = unique(beer_df_coerce$rating)
uniqueRatings
#[1] 5.0 3.0 4.0 4.5 2.5 3.5 2.0 1.5 1.0
# is the unique set of ratings which are given by users


#4.4 Visualise the rating values and notice:

View(beer_df_coerce)

# 4.4.1 average beer ratings
qplot(getRatings(beer_matrix), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")

# We can see that the Histogram is skewed around rating 3 and 4

summary(getRatings(beer_matrix)) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   3.500   4.000   3.867   4.500   5.000 
#Rattings Skewed between 3.5 and 4.5


avg_rating_groupby_beerid <- beer_df_coerce %>% group_by(item) %>% summarise(  AverageReviewsForBeer = mean(rating) )
View(avg_rating_groupby_beerid)
#Here we can see the Avg rating for each individual beer


# 4.4.2 The average user ratings

qplot(rowCounts(beer_matrix), binwidth = 10, 
      main = "Reviewes Done by User", 
      xlab = "Number of users", 
      ylab = "Count of Beer Reviewed")
#Majority of the ratings have come from the 250 users the remaining have not Reviewed many Beers

avg_rating_groupby_Users <- beer_df_coerce %>% group_by(user) %>% summarise(  AverageReviewsPerUser = mean(rating) )
View(avg_rating_groupby_Users)
#Here we can see the Avg Rating given by Individial Users


# 4.4.3 The average number of ratings given to the beers

avg_rating_beer <- df_matrix %>% group_by(item) %>%  summarise(  AverageReviewsForBeer = mean(rating) )
View(avg_rating_beer)
# Avgrage rating across all the beers is 3.86

groupby_number_rating_Beers <- df_matrix %>% group_by(beer_beerid) %>% summarise(  CountOfReviewsBeer = n() )
View(groupby_number_rating_Beers)
summary(groupby_number_rating_Beers )
#review_profilename CountOfReviewsUser
#beer_beerid    CountOfReviewsBeer
#Min.   :    5   Min.   : 20.00    
#1st Qu.: 1590   1st Qu.: 52.00    
#Median :10302   Median : 74.00    
#Mean   :19706   Mean   : 98.18    
#3rd Qu.:36351   3rd Qu.:118.00    
#Max.   :75086   Max.   :475.00

#From summary we can see that on Average a Beer gets 98 reviews (for Beers which have been reviwed by more than 50 users )

# 4.4.4 The average number of ratings given by the users
 
groupby_number_rating_Users <- df_matrix %>% group_by(review_profilename) %>% summarise(  CountOfReviewsUser = n() )
View(groupby_number_rating_Users)
summary(groupby_number_rating_Users)
#review_profilename CountOfReviewsUser
#1759Girl      :   1     Min.   :  8.0     
#1fastz28      :   1     1st Qu.: 48.0     
#3Vandoo       :   1     Median : 68.0     
#4000qtrap     :   1     Mean   : 92.7     
#4DAloveofSTOUT:   1     3rd Qu.:115.0     
#99bottles     :   1     Max.   :518.0     
#(Other)       :2180

#From summary we can see that on Average an User performs approv 93 reviews (for users who have made more than 50 reviews )


#-----------------------------------------------------------------------------------------------------------------------
#------------------------------------------3) Recommendation Models-----------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

#1)Divide your data into training and testing datasets

#----- with k = 1 method = split --------------------

scheme <- evaluationScheme(beer_matrix, method = "split", train = .9,
                           k = 1, given = 2, goodRating = 4)

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# Running Algorithm for recomending Beers
results <- evaluate(scheme, algorithms, n=c(1, 5, 10, 15, 20))
class(results)

#Comparing UBFC and IBFC
plot(results, annotate = 1:4, legend="topleft")


#----- with k = 5 i.e doing cross validation 5 times --------------------

scheme <- evaluationScheme(beer_matrix, method = "cross-validation", train = .9,
                           k = 5, given = 2, goodRating = 4)

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# Running Algorithm for recomending Beers
results <- evaluate(scheme, algorithms, n=c(1, 5, 10, 15, 20))
class(results)

#Comparing UBFC and IBFC models
plot(results, annotate = 1:4, legend="topleft")

#from the ROC curves we can say UBFC would be better choice for Recomendations


#---------------- Using the UBFC model for Creating Recomednations ---------------------------

UBCF_model <- Recommender(getData(scheme, "train"), "UBCF")


#--------------Recomdations for Cokes-----------------------------------
beer_matrix['cokes']

Recommendations_Cokes <- predict(UBCF_model, beer_matrix['cokes'] , n="5")
as(Recommendations_Cokes, "list")
#$cokes
#[1] "7971"  "47658" "1158"  "1717"  "1339" 


#--------------Recomdations for Genog-----------------------------------


Recommendations_Genog <- predict(UBCF_model, beer_matrix['genog'] , n="5")
as(Recommendations_Genog, "list")
#$genog
#[1] "2093"  "6075"  "932"   "11757" "1010"

#--------------Recomdations for Giblet-----------------------------------


Recommendations_Giblet <- predict(UBCF_model, beer_matrix['giblet'] , n="5")
as(Recommendations_Giblet, "list")
#$giblet
#[1] "4083"  "29619" "1545"  "19960" "10325"

#Thus the above mentioned beer ids recommended using the UBCF model can be presented to the customers. 



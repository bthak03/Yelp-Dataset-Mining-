library(DMwR)
library(class)
library(caret)
library(ISLR)
library(ggplot2)
library(gmodels)
library(stringr)
library(jsonlite)
library(tibble)
library(dplyr)
library(magrittr)
library(tm)
library(SnowballC)
library(wordcloud)
library(sentimentr)
library(cowplot)
library(pROC)
library(randomForest)
library(sentimentr)

# Creating a connection for review file as the file is to big to load on 8 gb ram 
review <- "yelp_review.csv"
con <- file(description = review, open = "r")


# loading business file 
file_user1 <- "business.json"
business_yelo <-jsonlite::stream_in(textConnection(readLines(file_user1, n=5000000)),verbose=T)
str(business_yelo)
# flattening multilevel business.json file 
b_flat <- flatten(business_yelo) 
rm(business_yelo)



## creating index of required variables 
b_flat$categories <- tolower(b_flat$categories)


b_yelp <- b_flat %>% filter(str_detect(categories, "indian")) 

rm(b_flat)

b_index <- b_yelp$business_id

rm(b_yelp)

## working with reviews file 
## first 100k rows from reviews 

con <- file(description = review, open = "r")

review_yelp <- read.table(con, nrows = 100000, skip = 0 , header = T, fill = TRUE, sep =",")

close(con)

review_yelp <- review_yelp %>% filter(business_id %in% b_index)
# removing extra columns
review_yelp <- review_yelp[,1:8]


## second 100k rows from reviews 
con <- file(description = review, open = "r")
review_yelp_1 <- read.table(con, nrows = 100000, skip = 100000 , header = F, fill = TRUE, sep =",")

close(con)

review_yelp_1 <- review_yelp_1 %>% filter(V3 %in% b_index)

# removing extra columns
review_yelp_1 <- review_yelp_1[,1:8]
# renaming columns
colnames(review_yelp)[c(1:8)] <- c("V1","V2","V3","V4","V5","V6","V7","V8")


## For third 100k rows from reviews
con <- file(description = review, open = "r")
review_yelp_2 <- read.table(con, nrows = 100000, skip = 200000 , header = F, fill = TRUE, sep =",")


review_yelp_2 <- review_yelp_2 %>% filter(V3 %in% b_index)

close(con)
## removing extra columns created
review_yelp_2 <- review_yelp_2[,1:8]

## For fourth 100k rows from reviews 
con <- file(description = review, open = "r")
review_yelp_3 <- read.table(con, nrows = 100000, skip = 300000 , header = F, fill = TRUE, sep =",")

review_yelp_3 <- review_yelp_3 %>% filter(V3 %in% b_index)

close(con)

# removing extra columns
review_yelp_3 <- review_yelp_3[,1:8]

## For fifth 100k rows from reviews
con <- file(description = review, open = "r")
review_yelp_4 <- read.table(con, nrows = 100000, skip = 400000 , header = F, fill = TRUE, sep =",")

review_yelp_4 <- review_yelp_4 %>% filter(V3 %in% b_index)

close(con)

# removing extra columns
review_yelp_4 <- review_yelp_4[,1:8]

## for sixth 100k rows from reviews
con <- file(description = review, open = "r")
review_yelp_5 <- read.table(con, nrows = 100000, skip = 500000 , header = F, fill = TRUE, sep =",")

review_yelp_5 <- review_yelp_5 %>% filter(V3 %in% b_index)

close(con)

# removing extra columns
review_yelp_5 <- review_yelp_5[,1:8]

## for seventh 100k rows from reviews 
con <- file(description = review, open = "r")
review_yelp_6 <- read.table(con, nrows = 100000, skip = 600000 , header = F, fill = TRUE, sep =",")

review_yelp_6 <- review_yelp_6 %>% filter(V3 %in% b_index)

close(con)

review_yelp
# removing extra columns
review_yelp_6 <- review_yelp_6[,1:8]

######## binding df #######
rv_yelp <- rbind(review_yelp, review_yelp_1, review_yelp_2, review_yelp_3,review_yelp_4,
                 review_yelp_5, review_yelp_6)
#### removing columns ##3
rm(review_yelp)
rm(review_yelp_1)
rm(review_yelp_2)
rm(review_yelp_3)
rm(review_yelp_4)
rm(review_yelp_5)
rm(review_yelp_6)

##




### checking for Indian restaunt 

rev <-  as.character(rv_yelp$V6)



# Wordplot

rev_analysis <- Corpus(VectorSource(rev)) %>% tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,c( stopwords("english"), "food","place", "get","thats", "since","even") ) %>%
  tm_map(stripWhitespace)

inspect(rev_analysis)



rev_analysis <- TermDocumentMatrix(rev_analysis)
rev_analysis <- as.matrix(rev_analysis)
rev_analysis <- sort(rowSums(rev_analysis), decreasing = T )
rev_analysis <- data.frame(word = names(rev_analysis), freq = rev_analysis)



set.seed(1234)
wordcloud(words = rev_analysis$word, freq = rev_analysis$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Perfoming sentiment analysis after filtering reviews contaning word authentic in it

rv_y_filter <- rv_yelp %>%
  filter(str_detect(V6, "authentic"))

rv_y_filter$V6 <- as.character(rv_y_filter$V6)

sentiment_analysis <- rv_y_filter$V6 %>% sentiment_by()
sentiment_analysis %>% rownames_to_column()


########################
yelp_ml <- data.frame(rv_y_filter[,2:3]) 
yelp_ml$index <- c(1:471)
yelp_ml$authentic <- ifelse(sentiment_analysis$ave_sentiment <= 0,0,1 )
rm(rv_y_filter)
rm(rv_yelp)
rm(file_user1)
rm(con)
rm(rev_analysis)

yelp_user_index <- yelp_ml$V2
yelp_business_index <- yelp_ml$V3

user_info <- read.csv("yelp_user.csv")

filter_user <- user_info %>% filter(user_id %in% yelp_user_index)

temp <- data.frame(yelp_user_index) 


rm(user_info)
### for yelp business


file_user1 <- "business.json"
business_yelo <-jsonlite::stream_in(textConnection(readLines(file_user1, n=5000000)),verbose=T)
str(business_yelo)
b_flat <- flatten(business_yelo) 
rm(business_yelo)

##  filter
business_filter <- b_flat %>% filter(business_id %in% yelp_business_index)
rm(b_flat)
############


##### final data frame
yelp_final <- business_filter[,c("business_id", "city","state","stars","review_count")] 
rm(business_filter)


colnames(yelp_ml)[c(1,2)] <- c("user_id","business_id")
# finding number of indian reviewers per restaurnt 
names <-   scan("indian_names.txt", what = character())


filter_user$indian_user <- ifelse(filter_user$name %in% names, 1, 0)


temp <-  left_join(yelp_ml, filter_user, by = "user_id")


number_of_indian <- temp %>% group_by(business_id) %>% summarise(sum1 = sum(indian_user)) 



yelp_final <- left_join(yelp_final,number_of_indian,"business_id")
colnames(yelp_final)[6] <- c("nu_indian_user")


### creation of authentic or not in final data frame 


d <- yelp_ml %>% group_by(business_id) %>% summarise(avg = mean(authentic) )


d$authentic <-  ifelse(d$avg >= 1, 1,0)
d <- d[,c(1,3)]
## Extra code
summary(d$avg)
which(d$authentic == 0) 
sum(d$authentic)

#### IMPLEMENTATION OF RANDOM FOREST ####

final_frame <- left_join(yelp_final, d, "business_id")
final_ml <- final_frame[,4:7]
final_ml$authentic <- ifelse(final_ml$authentic == 1, "A", "O")
final_ml$authentic <- as.factor(final_ml$authentic)







# Partition of data for train and test
set.seed(123)
ind_ml <- sample(2, nrow(final_ml), replace = T, prob = c(0.4,0.6) ) 
train_ml <- final_ml[ind_ml == 1,]
test_ml <- final_ml[ind_ml == 2,]

### using SMOTE to oversample the dataset 
u_train <- SMOTE(authentic~., train_ml, perc.over = 100, perc.under = 200)
prop.table(table(u_train$authentic))


# traning the model 
set.seed(111)
model <- randomForest(authentic ~ . , u_train, ntree = 100, mtry = 1  )
model


pred <- predict(model, test_ml)
pred_mat <-  confusionMatrix(predict(model, test_ml), test_ml$authentic, positive ="A")
table(test_ml$authentic)
pred
pred_mat



# Plotting the out of bag error rate


oob_error_rate <- data.frame(
  Trees = rep(1:nrow(model$err.rate), times = 3), 
  Types = rep(c("OOB", "A", "O"), each = nrow(model$err.rate)), 
  Error = c(model$err.rate[, "OOB"], 
            model$err.rate[, "A"], 
            model$err.rate[, "O"])
)


ggplot(oob_error_rate, aes(x= Trees, y = Error)) + 
  geom_line(aes(color = Types))















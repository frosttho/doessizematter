##########################################
#                                        #
#          Masterarbeit am               #
#  Lehrstuhl für quantitatives Marketing #
#        und Konsumentenverhalten        #
#                                        #
#          (c) 2023, Thomas Frost        #
#           Universität Mannheim         #
#                                        #
#                                        #
#        tfrost@mail.uni-mannheim.de     #
#                                        #
##########################################

# 01 - Functions ##############################################

monthdiffs <- function(date) {
  if (is.na(date) == FALSE) {
    diff <- (2022 - year(date)) * 12      # Base time: December 2022
    diff <- diff + (12 - month(date))
    if (diff < 0) {
      diff <- 0
    }
    return(diff)
  } else {
    return(NA)
  }
}

monthdiffsTo <- function(datefrom, dateto) {
  dateto <- as.Date(paste(strsplit(dateto, "/")[[1]][2], strsplit(dateto, "/")[[1]][1], "01", sep="-"))
  if ((is.na(datefrom) | is.na(dateto))) {
    return(NA)
  } else {
    diff <- (year(dateto) - year(datefrom)) * 12
    diff <- diff + (month(dateto) - month(datefrom))
    if (diff < 0) {
      diff <- 0
    }
    return(diff)
  }
}


# 02 - Setup ##################################################

library(tidyverse)

# 03 - Data Preparation #######################################

## 03.01 - Read data ##########################################

# STUDY 1
dataset <- read_csv("data/finaldataset_v3.csv", col_types = "ccnccnffnnfnlnnnclfcclcffnnff")
profiles <- read_csv2("data/profiles_gender.csv", col_types = "cnfllnff")
profilesv2 <- read_csv("data/profiles_v2.csv", col_types = "cnflnlnnnclfcclcf")
topics_account <- read_csv("data/topics_account.csv", col_types = "fc")
initialscraping <- read_tsv("data/Instagram__Posts_corrected_v5.tsv", col_types = "ccncccnncccfcn")
moreinitialscraping <- read_delim("data/Instagram__Posts_corrected.tsv", delim = "\t", quote="", col_types = "ccnTccnnccccc")
created <- read_csv("data/final_users_list_created.csv", col_types = "ncc")

created <- created[-1]
Sys.setlocale(category = "LC_ALL", locale = "en")
created$created <- as.Date(paste("01", created$created), format = "%d %B %Y")

# STUDY 2
study2 <- read_csv("data/study2_data_socialblade_topics.csv", col_types = "ccnccnfnfnncnlnnnclccclccfnc")



## 03.02 - combine account-level data #########################
colnames(topics_account) <- c("label", "Username")

profiles <- left_join(profiles, topics_account, by = "Username", keep = FALSE, na_matches = "never")
colnames(profiles)[9] <- "Topic.Account"

dataset$positive <- dataset$sentiment.label == "POSITIVE"
dataset$positive <- dataset$positive * 1

## 03.03 - group all data to account level ####################

by_account <- dataset %>%
  group_by(Username) %>%
  summarise(positives = sum(positive), num_posts = n()) %>%
  merge(data.frame(Username = profiles$Username,
                   followers.new = profiles$followers_new,
                   size.new = profiles$size_new,
                   Topic.Account = profiles$Topic.Account,
                   gender = profiles$gender,
                   followees = profilesv2$followees,
                   is_business_account = profilesv2$is_business_account,
                   is_verified = profilesv2$is_verified
  ),
  by = "Username", all.y = FALSE, sort = FALSE)

by_account$rel.positives <- by_account$positives / by_account$num_posts
by_account$size.new <- ordered(by_account$size.new, c("nano", "mikro", "midtier", "makro", "mega"))
by_account$rel.negatives <- 1 - by_account$rel.positives


# 04 - missing variables ######################################

## 04.01 - bringing back the number of posts per account ######

dataset <- left_join(dataset, by_account[c("Username", "num_posts")], by = "Username", na_matches = "never", multiple = "first")


## 04.02 - Geo Location #######################################

dataset <- left_join(dataset, data.frame(Post.ID = initialscraping$`Post ID`, Location = initialscraping$`Location of Post`),  by = "Post.ID", na_matches = "never", multiple = "first")

## 04.03 - Post Length ########################################

moreinitialscraping$Words <- strsplit(moreinitialscraping$Text, " ")
moreinitialscraping$NumWords <- lapply(X = moreinitialscraping$Words, FUN = length)

merger <- select(moreinitialscraping, 'Post ID', 'NumWords')

colnames(merger)[1] <- "Post.ID"

dataset <- left_join(dataset, merger,  by = "Post.ID", na_matches = "never", multiple = "first")

## 04.04 - account age ########################################

created$age <- unlist(lapply(created$created, monthdiffs))
dataset <- left_join(dataset, select(created, Username, age), by = "Username", na_matches = "never", multiple = "first")
by_account <- left_join(by_account, select(created, Username, age), by = "Username", na_matches = "never", multiple = "first")
study2 <- left_join(study2, select(created, Username, created), by = "Username", na_matches = "never", multiple = "first")


## 04.05 - post date ##########################################

dataset <- left_join(dataset, data.frame(Post.ID = initialscraping$`Post ID`, Date.UTC = initialscraping$`Date UTC`), by = "Post.ID", na_matches = "never", multiple = "first")
dataset$Date.UTC <- substr(dataset$Date.UTC, 1, 10)
dataset$Date.UTC <- as.Date(dataset$Date.UTC)
dataset$month <- month(dataset$Date.UTC)

# 05 - setup dataset model 1 ##################################

model1 <- data.frame(
  follower.class = dataset$size.new,
  gender = dataset$gender,
  num.Followees = dataset$followees,
  is.business = dataset$is.business,
  is.verified = dataset$is.verified,
  num.Posts = dataset$num_posts,
  account.age = dataset$age,
  is.located = dataset$Location != 'None',
  num.Hashtags = dataset$TotalNumHashtags,
  topic.post = dataset$Topic.Post,
  num.Mentions = dataset$TotalNumMentions,
  is.carousel = dataset$Product.Type == 'carousel_container',
  post.length = unlist(dataset$NumWords),
  sentiment = dataset$sentiment.label,
  username = as.factor(dataset$Username),
  month = dataset$month
)

# 06 - setup dataset model 2.1 ################################

model2.1 <- data.frame(
  follower.count = dataset$followers.new,
  gender = dataset$gender,
  num.Followees = dataset$followees,
  is.business = dataset$is.business,
  is.verified = dataset$is.verified,
  num.Posts = dataset$num_posts,
  account.age = dataset$age,
  is.located = dataset$Location != 'None',
  num.Hashtags = dataset$TotalNumHashtags,
  topic.post = dataset$Topic.Post,
  num.Mentions = dataset$TotalNumMentions,
  is.carousel = dataset$Product.Type == 'carousel_container',
  post.length = unlist(dataset$NumWords),
  sentiment = dataset$sentiment.label,
  likes = dataset$Likes,
  comments = dataset$Comments
)

# 07 - setup dataset model 2.2 ################################

model2.2 <- data.frame(
  follower.class = dataset$size.new,
  follower.count = dataset$followers.new,
  gender = dataset$gender,
  num.Followees = dataset$followees,
  is.business = dataset$is.business,
  is.verified = dataset$is.verified,
  num.Posts = dataset$num_posts,
  account.age = dataset$age,
  is.located = dataset$Location != 'None',
  num.Hashtags = dataset$TotalNumHashtags,
  topic.post = dataset$Topic.Post,
  num.Mentions = dataset$TotalNumMentions,
  is.carousel = dataset$Product.Type == 'carousel_container',
  post.length = unlist(dataset$NumWords),
  sentiment = dataset$sentiment.label,
  likes = dataset$Likes,
  comments = dataset$Comments
)


# 08 - STUDY 2 ################################################

## 08.01 - Data wrangling #####################################

study2$age <- mapply(FUN = monthdiffsTo, study2$created, study2$my)

study2$Words <- strsplit(study2$Text, " ")
study2$NumWords <- lapply(X = study2$Words, FUN = length)


## 08.02 - setup dataset study 2 ##############################

model.study2 <- data.frame(
  follower.count = study2$followers.posttime,
  gender = study2$gender,
  num.Followees = study2$followees.posttime,
  is.business = study2$is.business,
  is.verified = study2$is.verified,
  num.Posts = study2$mediacount.posttime,
  account.age = study2$age,
  is.located = study2$Location != 'None',
  num.Hashtags = study2$TotalNumHashtags,
  topic.post = study2$post.topic,
  num.Mentions = study2$TotalNumMentions,
  is.carousel = study2$Product.Type == 'carousel_container',
  post.length = unlist(study2$NumWords),
  sentiment = study2$sentiment.label,
  username = study2$Username
)


remove(list = setdiff(ls(), c("model1", "model2.1", "model2.2", "model.study2")))         # remove everything that is not needed for running the model estimation



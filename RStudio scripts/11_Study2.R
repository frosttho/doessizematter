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

# 01 - Setup ##################################################

library(tidyverse)

# 03 - Data Import ############################################

posts <- read_csv("data/processed-dataset_Study2.csv", col_types = "nccnccnfnc")
posts_preproc <- read_tsv("data/Instagram__Posts_corrected_SiEBERT-input.tsv", col_types = "ccncccnncccfcnc")
results <- read_csv("data/predicted-full_Study2.csv", col_types = "ncnfn")
profiles <- read_csv("data/profiles_v2.csv")
profile_gender <- read_csv2("data/profiles_gender.csv")

followers <- read_csv("data/followers_hist.csv")
followees <- read_csv("data/followees_hist.csv", col_types = "Dncnn")
mediacount <- read_csv("data/mediacount_hist.csv", col_types = "Dncnn")

topics <- read_csv("data/study2_topics_posts.csv", col_types = "fc")


all.equal(posts$Text, results$text)   # Check for equality of post captions

results$text <- gsub("\\n", ' ', results$text)        # Remove line breaks
results$text <- gsub("^\\.+$", '', results$text)      # remove all captions that only contain dots as the language identifier reads them as file paths ~tf

posts$Text <- gsub("\\n", ' ', posts$Text)        # Remove line breaks
posts$Text <- gsub("^\\.+$", '', posts$Text)      # Exclude all Texts that only contain dots as the language identifier reads them as file paths ~tf



# 04 - Merging ################################################

finaldataset <- cbind(posts, results$label, results$score)
colnames(finaldataset)[2] <- "Username"

finaldataset <- left_join(finaldataset, profiles, by = "Username")
finaldataset <- left_join(finaldataset, data.frame(Username = profile_gender$Username, gender = profile_gender$gender), by = "Username")

finaldataset$gender <- as.factor(finaldataset$gender)

finaldataset <- finaldataset[-1]

# delete duplicates
duplicates <- as.data.frame(1:length(as.vector(duplicated(finaldataset$`Post ID`)))*as.vector(duplicated(finaldataset$`Post ID`)))
colnames(duplicates) <- ("row")
duplicates <- as.vector(filter(duplicates, row != 0))

finaldataset <- finaldataset[-unlist(duplicates),]

# 05 - Delete further nonsense ################################

finaldataset <- filter(finaldataset, is.na(finaldataset$Text) == FALSE) # empty because it contained only emojis
finaldataset <- finaldataset[-c(9,14)]


# 05.1 - new Variable NumOfMentions ###########################

finaldataset$Words <- strsplit(finaldataset$Text, " ")

countmentions <- function(liste) {
  i <- 0
  if(length(unlist(liste)) != 0) {
    for (j in 1:length(liste)) {
      if(substring(liste[j], 1, 1) == "@") {
        i <- i + 1
      }
    }
  } else {
    i <- 0
  }
  return(i)
}

finaldataset$TotalNumMentions <- sapply(finaldataset$Words, countmentions)

# delete captions that only contain hashtags or mentions
hashtag_mention_detection <- function(strng) {
  hashtag <- substring(strng, 1, 1) == "#"
  mention <- substring(strng, 1, 1) == "@"
  return(hashtag | mention)
}

finaldataset$refs <- lapply(finaldataset$Words, hashtag_mention_detection)

onlyrefs <- function(wordlist) {
  return(all(unlist(wordlist)))
}

finaldataset$onlyrefs <- sapply(finaldataset$refs, onlyrefs)

finaldataset <- filter(finaldataset, onlyrefs == FALSE)

finaldataset <- finaldataset[-c(27,29,30)]

colnames(finaldataset) <- c("Username", "Post.ID", "Comments", "Title", "Text", "Likes", "Product.Type", "TotalNumHashtags", "sentiment.label", "sentiment.score", "followers.old", "size.old", "User.ID", "is.private", "mediacount", "followers.new", "followees", "external_url", "is.business", "business.category", "biography", "fullname", "is.verified", "profilepic", "size.new", "gender", "TotalNumMentions")

posts_preproc$date <- substr(posts_preproc$`Date UTC`, 1, 10)
posts_preproc$date <- as.Date(posts_preproc$date)
posts_preproc$my <- paste(month(posts_preproc$date), year(posts_preproc$date), sep = "/")

finaldataset <- left_join(finaldataset, data.frame(Post.ID = posts_preproc$`Post ID`, my = posts_preproc$my, Location = posts_preproc$`Location of Post`), by = "Post.ID", na_matches = "never", multiple = "first")



# 06 - generate full follower history #########################

followers$my <- paste(followers$month, followers$year, sep = "/")
my_vector <- paste(rep(1:12, 4), c(rep(2019, 12), rep(2020, 12), rep(2021, 12), rep(2022, 12)), sep = "/")

num_users <- length(levels(as.factor(followers$username)))

followers <- left_join(data.frame(my = rep(my_vector, num_users), username = rep(levels(as.factor(followers$username)), each = 48)), followers, by = c("my", "username"))

followers$organic <- FALSE
followers[(is.na(followers$followers) == FALSE),]$organic <- TRUE

getMonth <- function(string) {
  return(strsplit(string, "/")[[1]][1])
}

getYear <- function(string) {
  return(strsplit(string, "/")[[1]][2])
}


followers[(followers$organic == FALSE),]$month <- sapply(followers[(followers$organic == FALSE),]$my, getMonth)
followers[(followers$organic == FALSE),]$year <- sapply(followers[(followers$organic == FALSE),]$my, getYear)

followers$month <- as.numeric(followers$month)
followers$year <- as.numeric(followers$year)

followers$monthcont <- ((followers$year - 2019) * 12) + followers$month

ContextAwareFill <- function(user, followers) {
  counter_i = 0
  counter_j = 0
  for (i in 1:48) {
    if (is.na(filter(followers, username == user)$followers[i]) == FALSE) {
    } else {
      for (j in i+1:48) {
        if (is.na(filter(followers, username == user)$followers[j]) == FALSE) {
          counter_j <- j
          break
        }
      }
      i.1_value <- filter(followers, username == user)$followers[i-1]
      j_value <- filter(followers, username == user)$followers[j]
      
      new_value <- round(i.1_value + ((j_value - i.1_value) / (counter_j-i+1)))
      
      followers[(followers$username == user & followers$monthcont == i),]$followers <- new_value
    }
  }
  return(followers)
}


for (i in 1:length(levels(as.factor(followers$username)))) {
  followers <- ContextAwareFill(levels(as.factor(followers$username))[i], followers = followers)
}


# 07 - additional SocialBlade measures ########################

followees$my <- paste(followees$month, followees$year, sep = "/")
mediacount$my <- paste(mediacount$month, mediacount$year, sep = "/")


socialblade.measures <- left_join(select(followers, username, my, organic, monthcont, followers), select(followees, followees, username, my), by = join_by(username, my), na_matches="never")
socialblade.measures <- left_join(socialblade.measures, select(mediacount, mediacount, username, my), by = join_by(username, my), na_matches="never")

ContextAwareFillFollowees <- function(user, sb.m) {
  counter_i = 0
  counter_j = 0
  for (i in 1:48) {
    if (is.na(filter(sb.m, username == user)$followees[i]) == FALSE) {
    } else {
      for (j in i+1:48) {
        if (is.na(filter(sb.m, username == user)$followees[j]) == FALSE) {
          counter_j <- j
          break
        }
      }
      i.1_value <- filter(sb.m, username == user)$followees[i-1]
      j_value <- filter(sb.m, username == user)$followees[j]
      
      new_value <- round(i.1_value + ((j_value - i.1_value) / (counter_j-i+1)))
      
      sb.m[(sb.m$username == user & sb.m$monthcont == i),]$followees <- new_value
    }
  }
  return(sb.m)
}

ContextAwareFillMediaCount <- function(user, sb.m) {
  counter_i = 0
  counter_j = 0
  for (i in 1:48) {
    if (is.na(filter(sb.m, username == user)$mediacount[i]) == FALSE) {
    } else {
      for (j in i+1:48) {
        if (is.na(filter(sb.m, username == user)$mediacount[j]) == FALSE) {
          counter_j <- j
          break
        }
      }
      i.1_value <- filter(sb.m, username == user)$mediacount[i-1]
      j_value <- filter(sb.m, username == user)$mediacount[j]
      
      new_value <- round(i.1_value + ((j_value - i.1_value) / (counter_j-i+1)))
      
      sb.m[(sb.m$username == user & sb.m$monthcont == i),]$mediacount <- new_value
    }
  }
  return(sb.m)
}

for (i in 1:length(levels(as.factor(socialblade.measures$username)))) {
  socialblade.measures <- ContextAwareFillFollowees(levels(as.factor(socialblade.measures$username))[i], sb.m = socialblade.measures)
}

for (i in 1:length(levels(as.factor(socialblade.measures$username)))) {
  socialblade.measures <- ContextAwareFillMediaCount(levels(as.factor(socialblade.measures$username))[i], sb.m = socialblade.measures)
}


finaldataset.sb <- left_join(finaldataset, select(socialblade.measures, Username = username, my, followers.posttime = followers, followees.posttime = followees, mediacount.posttime = mediacount), by = join_by(Username, my), na_matches = "never")


# 08 - add topics from topic modelling ########################

finaldataset.sb <- left_join(finaldataset.sb, select(topics, Post.ID, post.topic = label), by = "Post.ID", na_matches = "never", multiple = "first")


######################################## EXPORT #####################################


write_csv(finaldataset.sb, "data/study2_data_socialblade_topics.csv")


#####################################################################################
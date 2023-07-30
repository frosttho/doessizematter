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

getRegressionTable <- function(model) {
  df <- as.data.frame(coefficients(model))
  df$var <- rownames(df)
  rownames(df) <- 1:dim(df)[1]
  df <- data.frame(var = df$var, coefficients = paste(round(df$`coefficients(model)`, 4), " (", round(sqrt(diag(vcov(model))), 3), ")", sep = ""), p = round(summary(model)$coefficients[,4], 3))
  return(df)
}

outputTable<- function(df, filename) {
  write.table(df, file = paste(filename, ".txt", sep = ""), sep = ";", quote = FALSE, row.names = F)
  return(TRUE)
}

scaleNumeric <- function(vector) {
  if (is.numeric(vector)) {
    return(scale(vector))
  } else {
    return(vector)
  }
}

stars <- function(vector) {
  sapply(vector, function(v) {
    if(v < 0.001) {
      return("***")
    } else if(v < 0.01) {
      return('**')
    } else if(v < 0.05) {
      return('*')
    } else {
      return("")
    }
  }
  )
}




library(tidyverse)
library(lme4)


# GENERAL
created <- read_csv("data/final_users_list_created.csv", col_types = "ncc")

created <- created[-1]
created$created <- as.Date(paste("01", created$created), format = "%d %B %Y")


# STUDY 2
study2 <- read_csv("data/study2_data_socialblade_topics.csv", col_types = "ccnccnfnfnncnlnnnclccclccfnc")




study2 <- left_join(study2, select(created, Username, created), by = "Username", na_matches = "never", multiple = "first")


study2$age <- mapply(FUN = monthdiffsTo, study2$created, study2$my)

study2$Words <- strsplit(study2$Text, " ")
study2$NumWords <- lapply(X = study2$Words, FUN = length)


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




model.study2.z <- as.data.frame(lapply(model.study2, scaleNumeric))

model.study2.z$sentiment <- factor(model.study2.z$sentiment, levels = c("NEGATIVE", "POSITIVE"), ordered = TRUE)      # set as ordered to make the "dummy" of negative = 0 and positive = 1
model.study2.z$gender <- relevel(model.study2.z$gender, ref = "f")


random_study2_wo_followercount <- glmer(formula = sentiment ~ follower.count +
                              gender +
                              num.Followees +
                              is.business +
                              is.verified +
                              num.Posts +
                              account.age +
                              is.located +
                              num.Hashtags +
                              topic.post +
                              num.Mentions +
                              is.carousel +
                              post.length +
                              (1 + num.Followees + num.Posts + account.age + num.Hashtags + num.Mentions + is.carousel + post.length | username),
                            data = model.study2.z, family = binomial(link = "logit"), nAGQ = 0)

summary(random_study2_wo_followercount)

random_study2_wo_followees <- glmer(formula = sentiment ~ follower.count +
                              gender +
                              num.Followees +
                              is.business +
                              is.verified +
                              num.Posts +
                              account.age +
                              is.located +
                              num.Hashtags +
                              topic.post +
                              num.Mentions +
                              is.carousel +
                              post.length +
                              (1 + num.Posts + account.age + num.Hashtags + num.Mentions + is.carousel + post.length | username),
                            data = model.study2.z, family = binomial(link = "logit"), nAGQ = 0)

summary(random_study2_wo_followees)

random_study2_wo_numposts <- glmer(formula = sentiment ~ follower.count +
                              gender +
                              num.Followees +
                              is.business +
                              is.verified +
                              num.Posts +
                              account.age +
                              is.located +
                              num.Hashtags +
                              topic.post +
                              num.Mentions +
                              is.carousel +
                              post.length +
                              (1 + account.age + num.Hashtags + num.Mentions + is.carousel + post.length | username),
                            data = model.study2.z, family = binomial(link = "logit"), nAGQ = 0)

summary(random_study2_wo_numposts)

random_study2_wo_accountage <- glmer(formula = sentiment ~ follower.count +
                              gender +
                              num.Followees +
                              is.business +
                              is.verified +
                              num.Posts +
                              account.age +
                              is.located +
                              num.Hashtags +
                              topic.post +
                              num.Mentions +
                              is.carousel +
                              post.length +
                              (1 + num.Hashtags + num.Mentions + is.carousel + post.length | username),
                            data = model.study2.z, family = binomial(link = "logit"), nAGQ = 0)

summary(random_study2_wo_accountage)

random_study2_wo_carousel <- glmer(formula = sentiment ~ follower.count +
                                       gender +
                                       num.Followees +
                                       is.business +
                                       is.verified +
                                       num.Posts +
                                       account.age +
                                       is.located +
                                       num.Hashtags +
                                       topic.post +
                                       num.Mentions +
                                       is.carousel +
                                       post.length +
                                       (1 + num.Hashtags + num.Mentions + post.length | username),
                                     data = model.study2.z, family = binomial(link = "logit"), nAGQ = 0)

summary(random_study2_wo_carousel)

















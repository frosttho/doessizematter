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


# 01 - Library Installs #######################################

install.packages('rvest')
install.packages('textclean')

# 02 - Setup ##################################################

library(xml2)
library(tidyverse)
library(rvest)
library(textclean)
library(jsonlite)


# 03 - Functions ##############################################

getSBFromHTML <- function(file, diagrname) {
  # read HTML file
  html_read <- read_html(file)
  
  # identify script tags
  scripts <- html_elements(x = html_read, css = 'body script')
  
  # identify script tag with Highchart data and replace escaped characters
  script <- html_text(scripts[grepl('Highcharts', html_text(scripts))])
  script <- replace_white(script)
  
  # identify single diagram definitions
  fc_pos <- gregexec('Highcharts\\.chart\\(.*?\\{', script)
  
  start <- as.vector(unlist(fc_pos)) + as.vector(attr(fc_pos[[1]], "match.length")) - 1
  stop <- as.vector(unlist(fc_pos))[-1] - 4
  
  stop <- append(stop, nchar(script) - 7)
  
  fc_pos <- data.frame(start, stop)
  
  # extract single diagram definitions
  function_calls <- substring(
    text = script,
    first = fc_pos$start,
    last = fc_pos$stop
  )
  
  # search for diagram names
  name_pos <- gregexec("Highcharts\\.chart\\('.*?'", script)
  
  names <- substring(
    text = script,
    first = as.vector(unlist(name_pos)) + 18,
    last = as.vector(unlist(name_pos)) + as.vector(attr(name_pos[[1]], "match.length")) - 2
  )
  
  # search for diagram data
  data_pos <- gregexec('data: \\[\\[.*?\\]\\]', function_calls)
  
  start <- unlist(data_pos)
  stop <-  start + as.vector(unlist(lapply(data_pos, function(listitem) {attr(listitem, "match.length")})))
  
  data_pos <- data.frame(start = start + 6, stop)
  
  # extract diagram data
  data <- substring(
    text = function_calls,
    first = data_pos$start,
    last = data_pos$stop
  )
  
  data <- data.frame(data, name = names)
  
  # save diagram data as R data object
  return(as.data.frame(fromJSON(
    filter(data, name == diagrname)$data
  )))
}

getsize <- function(followers) {
  result <- case_when(
    (followers <= 1000) ~ NA,
    (followers > 1000 && followers <= 10000) ~ "nano",
    (followers > 10000 && followers <= 50000) ~ "mikro",
    (followers > 50000 && followers <= 500000) ~ "midtier",
    (followers > 500000 && followers <= 1000000) ~ "makro",
    (followers > 1000000) ~ "mega"
  )
  return(result)
}


# Function to show the graph of number of media items in profile
getMediaPlot <- function(user) {
  df <- getSBFromHTML(paste('socialblade_data/', user, '.htm', sep = ""), 'graph-instagram-monthly-media-container')
  df$V1 <- as.Date(as.POSIXct(df$V1 / 1000, origin="1970-01-01"))
  
  plot <- ggplot(data = df, aes(x = V1, y = V2)) +
    geom_point() +
    geom_line() +
    ylab("Number of posts online") +
    xlab("Date") +
    ggtitle(paste("graph for user", user)) +
    theme_light()
  
  plot
}


# Function to return followers of a specific user
getFollowers <- function(user) {
  df <- getSBFromHTML(paste('socialblade_data/', user, '.htm', sep = ""), 'graph-instagram-monthly-followers-container')
  df$V1 <- as.Date(as.POSIXct(df$V1 / 1000, origin="1970-01-01"))
  df$size <- sapply(df$V2, getsize)
  df$Username <- user
  return(df)
}

# Function to return followees of a specific user
getFollowees <- function(user) {
  df <- getSBFromHTML(paste('socialblade_data/', user, '.htm', sep = ""), 'graph-instagram-monthly-following-container')
  df$V1 <- as.Date(as.POSIXct(df$V1 / 1000, origin="1970-01-01"))
  df$Username <- user
  return(df)
}

# Function to return followees of a specific user
getMediaCount <- function(user) {
  df <- getSBFromHTML(paste('socialblade_data/', user, '.htm', sep = ""), 'graph-instagram-monthly-media-container')
  df$V1 <- as.Date(as.POSIXct(df$V1 / 1000, origin="1970-01-01"))
  df$Username <- user
  return(df)
}


# 04 - Import #################################################

profiles <- read_csv2("data/profiles_gender.csv", col_types = "cnfllnff")


# 05 - Profile Selection ######################################

chosen_profiles <- data.frame(Username = c(""))
#chosen_profiles <- read_csv("data/chosen_profiles.csv")

i <- 1

getMediaPlot(filter(profiles, size_new == "makro")$Username[i])

# only execute this line if profile matches criteria
chosen_profiles <- rbind(chosen_profiles, data.frame(Username = c(filter(profiles, size_new == "makro")$Username[i]), size = c("makro")))

i <- i+1

write_csv(chosen_profiles, "data/chosen_profiles.csv")

# 06 - Follower Data collection ###############################

followers <- as.data.frame(sapply(chosen_profiles$Username, getFollowers))
followers <- as.data.frame(t(followers))
followers <- data.frame(date = as.Date(unlist(followers$V1), origin = "1970-01-01"), followers = unlist(followers$V2), size = unlist(followers$size), username = unlist(followers$Username))

rownames(followers) <- 1:dim(followers)[1]

followers$month <- month(followers$date)
followers$year <- year(followers$date)


# Find out which data point is the earliest per user

minim <- as.data.frame(tapply(followers$year, followers$username, min))
minim$user <- rownames(minim)

minim$month <- apply(minim, MARGIN = 1, FUN = function(df, followers) {
  return(min(filter(followers, username == df[2], year == df[1])$month))
}, followers)

colnames(minim) <- c("year", "username", "month")

followers_19plus <- filter(followers, username %in% 
                             filter(minim, (year < 2019 | (year == 2019 & month == 1)))$username
                           )
followers_19plus <- filter(followers_19plus, (year >= 2019 & year < 2023))

write_csv(followers_19plus, "data/followers_hist.csv")




# 07 - Followee Data collection ###############################

followees <- as.data.frame(sapply(chosen_profiles$Username, getFollowees))
followees <- as.data.frame(t(followees))
followees <- data.frame(date = as.Date(unlist(followees$V1), origin = "1970-01-01"), followees = unlist(followees$V2), username = unlist(followees$Username))

rownames(followees) <- 1:dim(followees)[1]

followees$month <- month(followees$date)
followees$year <- year(followees$date)


# Find out which data point is the earliest per user

minim <- as.data.frame(tapply(followees$year, followees$username, min))
minim$user <- rownames(minim)

minim$month <- apply(minim, MARGIN = 1, FUN = function(df, followees) {
  return(min(filter(followees, username == df[2], year == df[1])$month))
}, followees)

colnames(minim) <- c("year", "username", "month")

followees_19plus <- filter(followees, username %in% 
                             filter(minim, (year < 2019 | (year == 2019 & month == 1)))$username
)
followees_19plus <- filter(followees_19plus, (year >= 2019 & year < 2023))

write_csv(followees_19plus, "data/followees_hist.csv")




# 08 - Media Count collection #################################

mediacount <- as.data.frame(sapply(chosen_profiles$Username, getMediaCount))
mediacount <- as.data.frame(t(mediacount))
mediacount <- data.frame(date = as.Date(unlist(mediacount$V1), origin = "1970-01-01"), mediacount = unlist(mediacount$V2), username = unlist(mediacount$Username))

rownames(mediacount) <- 1:dim(mediacount)[1]

mediacount$month <- month(mediacount$date)
mediacount$year <- year(mediacount$date)


# Find out which data point is the earliest per user

minim <- as.data.frame(tapply(mediacount$year, mediacount$username, min))
minim$user <- rownames(minim)

minim$month <- apply(minim, MARGIN = 1, FUN = function(df, mediacount) {
  return(min(filter(mediacount, username == df[2], year == df[1])$month))
}, mediacount)

colnames(minim) <- c("year", "username", "month")

mediacount_19plus <- filter(mediacount, username %in% 
                             filter(minim, (year < 2019 | (year == 2019 & month == 1)))$username
)
mediacount_19plus <- filter(mediacount_19plus, (year >= 2019 & year < 2023))

write_csv(mediacount_19plus, "data/mediacount_hist.csv")

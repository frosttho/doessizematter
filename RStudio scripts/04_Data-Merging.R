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

install.packages('tidyverse')


# 02 - Setup ##################################################

library(tidyverse)


# 03 - Data Import ############################################

posts <- read_csv("data/processed-dataset.csv", col_types = "nccnccnc")
posts_preproc <- read_tsv("data/Instagram__Posts_corrected_v5.tsv", col_types = "ccncccnncccfcn")
results <- read_csv("data/predicted-full.csv")
profiles <- read_csv("data/profiles_v2.csv")
profile_gender <- read_csv2("data/profiles_gender.csv")

all.equal(posts$Text, results$text)   # Check for equality of post captions

results$text <- gsub("\\n", ' ', results$text)        # Remove line breaks
results$text <- gsub("^\\.+$", '', results$text)      # remove all captions that only contain dots as the language identifier reads them as file paths ~tf

posts$Text <- gsub("\\n", ' ', posts$Text)        # Remove line breaks
posts$Text <- gsub("^\\.+$", '', posts$Text)      # Exclude all Texts that only contain dots as the language identifier reads them as file paths ~tf



# 04 - Merging ################################################

finaldataset <- cbind(posts, results$label, results$score)
colnames(finaldataset)[2] <- "Username"

finaldataset <- merge(finaldataset, profiles, all.x = TRUE, sort = FALSE)
finaldataset <- merge(finaldataset, data.frame(Username = profile_gender$Username, gender = profile_gender$gender), by = "Username", all.x = TRUE, sort = FALSE)

finaldataset$gender <- as.factor(finaldataset$gender)

# doppelte Posts löschen
duplicates <- as.data.frame(1:length(as.vector(duplicated(posts_preproc$`Post ID`)))*as.vector(duplicated(posts_preproc$`Post ID`)))
colnames(duplicates) <- ("row")
duplicates <- as.vector(filter(duplicates, row != 0))

posts_preproc <- posts_preproc[-unlist(duplicates),]

finaldataset <- merge(finaldataset, data.frame(Post.ID = posts_preproc$`Post ID`, TotalNumHashtags = posts_preproc$TotalNumHashtags), all.x = TRUE, sort = FALSE, by.x = "Post ID", by.y = "Post.ID")


# 05 - Delete further nonsense ################################

finaldataset <- filter(finaldataset, is.na(finaldataset$Text) == FALSE) # empty because it contained only emojis
finaldataset <- finaldataset[-c(3,13)]


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

colnames(finaldataset) <- c("Post.ID", "Username", "Comments", "Title", "Text", "Likes", "Product.Type", "sentiment.label", "sentiment.score", "followers.old", "size.old", "User.ID", "is.private", "mediacount", "followers.new", "followees", "external_url", "is.business", "business.category", "biography", "fullname", "is.verified", "profilepic", "size.new", "gender", "TotalNumHashtags", "TotalNumMentions")
write_csv(finaldataset, "data/finaldataset_v2.csv")

# 05 - Adding Topics ##########################################

print("This is done in 05_LanguageDetection.R")

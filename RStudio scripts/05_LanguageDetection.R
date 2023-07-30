##########################################
#                                        #
#          Masterarbeit am               #
#  Lehrstuhl für quantitatives Marketing #
#        und Konsumentenverhalten        #
#                                        #
#          (c) 2023, Thomas Frost        #
#           Universität Mannheim         #
#                                        #
#        based on Mouselimis (2022)      #
#                                        #
#        tfrost@mail.uni-mannheim.de     #
#                                        #
##########################################

# 01 - Library Installs #######################################

install.packages('tidyverse')
install.packages("fastText")


# 02 - Setup ##################################################

library(tidyverse)
library(fastText)
library(readxl)

# 03 - Data import ############################################

dataset <- read_csv("data/finaldataset_v2.csv", col_types = "ccnccnffnnfnf")
topics_account <- read_csv("data/topics_account.csv", col_types = "fc")
topics_posts <- read_csv("data/topics_posts.csv", col_types = "fc")


# 04 - Detection ##############################################

large_model = "lid.176.bin"

language = language_identification(input_obj = dataset$Text,
                                   pre_trained_language_model_path = large_model,
                                   verbose = TRUE)

language$iso_lang_1 <- as.factor(language$iso_lang_1)

summary(language$iso_lang_1)

postlang <- cbind(dataset, language$iso_lang_1)
colnames(postlang)[28] <- "language"
postlang_non_en <- filter(postlang, language != "en")


# 05 - Export for manual check in Excel #######################

write_excel_csv(postlang_non_en, "check-for-non-english.csv")

# 06 - Implement manual changes ###############################

manual_non_en <- read_excel("data/05-1_LanguageDetection.xlsx")

manual_non_en <- manual_non_en[-1]

#dataset2 <- merge(dataset, manual_non_en, sort = FALSE, all.x = TRUE, by = "Post.ID")
dataset2 <- left_join(postlang, manual_non_en, by = "Post.ID", keep = TRUE, na_matches = "never")

dataset2 <- filter(dataset2, (language.x == "en" | language.y == "en"))
dataset2 <- dataset2[1:27]

colnames(dataset2)[1] <- "Post.ID"
colnames(dataset2)[5] <- "Text"

# 07 - delete duplicate post ##################################

dataset2 <- dataset2[-45609,]
rownames(dataset2) <- 1:dim(dataset2)[1]


# 08 - Adding Topics ##########################################

colnames(topics_account) <- c("label", "Username")

dataset3 <- left_join(dataset2, topics_account, by = "Username", keep = FALSE, na_matches = "never")
colnames(dataset3)[28] <- "Topic.Account"

dataset3 <- left_join(dataset3, topics_posts, by = "Post.ID", keep = FALSE, na_matches = "never")
colnames(dataset3)[29] <- "Topic.Post"


# 09 - Finally export the final dataset #######################

write_csv(dataset3, "data/finaldataset_v3.csv")



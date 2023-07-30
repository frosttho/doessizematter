##########################################
#                                        #
#          Masterarbeit am               #
#  Lehrstuhl für quantitatives Marketing #
#        und Konsumentenverhalten        #
#                                        #
#          (c) 2023, Thomas Frost        #
#           Universität Mannheim         #
#                                        #
#        tfrost@mail.uni-mannheim.de     #
#                                        #
##########################################

# 01 - Library Installs #######################################

install.packages('tidyverse')
install.packages('reshape')

# 02 - Setup ##################################################

library(tidyverse)
library(reshape)

# 03 - Data Import

profiles_target <- read_csv("data/fulldf-export.csv")
profiles_read <- read_tsv("data/Instagram__Profiles.tsv")
profiles_error <- read_csv("data/UserList_NoInstagramAccountFound.csv", col_names = FALSE)

colnames(profiles_target)[2] <- 'Username'
colnames(profiles_error)[1] <- 'Username'

profiles_error$found <- FALSE;
profiles_read$found <- TRUE;

# 04 - Matching

profiles_target_short <- data.frame(profiles_target$Username, profiles_target$Instagram.Followers, profiles_target$size)
colnames(profiles_target_short) <- c("Username", "followers_old", "size_old")

profiles <- merge_recurse(list(profiles_target_short, profiles_read, profiles_error), by = "Username")
colnames(profiles)[8] <- 'followers_new'

# 05 - Descriptive

summary(profiles$found)
summary(profiles$is_private)


# 06 - change in account size

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

profiles$size_new <- unlist(map(profiles$followers_new, getsize))

profiles$size_new <- as.factor(profiles$size_new)

table(profiles$size_old, profiles$size_new, useNA = "ifany")

write_csv(profiles, "data/profiles_v2.csv")



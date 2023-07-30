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
install.packages('PMCMRplus')
install.packages('car')
install.packages('nortest')
install.packages('cowplot')
install.packages('ggpubr')


# 02 - Setup ##################################################

library(tidyverse)
library(readxl)
library(PMCMRplus)
library(nortest)
library(gridExtra)
library(cowplot)
library(ggpubr)

# 03 - Data Preparation #######################################

## 03.01 - Read data ##########################################
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

remove(list = setdiff(ls(), c("dataset", "by_account")))         # remove everything that is not needed for running the model estimation






# 04 - Key Figures ############################################

## 04.01 - (Cross) tables #####################################


# Account variables
table(dataset$size.new, useNA = "no")/dim(dataset)[1]                             # Share of posts that can be attributed to an account of size x

tapply(by_account$num_posts, by_account$size.new, mean)                           # average number of posts per account in each size class

table(by_account$is_business_account)/dim(by_account)[1]                          # number of business accounts in % globally
table(by_account$is_business_account, by_account$size.new) / rep(as.vector(table(by_account$size.new)), each = 2)
                                                                                  # in % per size class

table(by_account$is_verified)/dim(by_account)[1]                                  # verified accounts in % globally

table(by_account$gender)/dim(by_account)[1]                                       # Geschlechterverteilung


# post variables
table(dataset$sentiment.label)/dim(dataset)[1]                                    # Anteile positive und negative Posts

summary(filter(dataset, Likes > -1)$Likes)

summary(filter(dataset, Likes > -1)$Comments)



## 04.02 - Plots ##############################################

# Boxplots of Comments and Likes

boxplot_likes <- ggplot(filter(dataset, Likes > -1), aes(y = Likes)) +
  geom_boxplot() +
  theme_light() +
  theme(axis.text.x=element_blank(),
        panel.border = element_blank()) +
  scale_y_log10() +
  ylab("") +
  ggtitle("Distribution of Likes")


boxplot_comments <- ggplot(filter(dataset, Likes > -1), aes(y = Comments)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_light() +
  theme(axis.text.x=element_blank(),
        panel.border = element_blank()) +
  ylab("") +
  ggtitle("Distribution of Comments")

boxplot_likes
boxplot_comments

do.call("grid.arrange", c(list(boxplot_likes, boxplot_comments), ncol=2))



# Boxplot of average share of positive posts in each size class

by_account$size.new <- factor(by_account$size.new, ordered = FALSE)
by_account$size.new <- as.character(by_account$size.new)

by_account$size.new[by_account$size.new == "mikro"] <- "micro"
by_account$size.new[by_account$size.new == "makro"] <- "macro"

by_account$size.new <- factor(by_account$size.new, ordered = TRUE, levels = c("nano", "micro", "midtier", "macro", "mega"))

by_account.plotdata <- filter(by_account, is.na(size.new) == FALSE)

by_account_plot <- ggplot(by_account.plotdata, aes(x = size.new, y = rel.positives)) +
  geom_boxplot() + 
  xlab("Size Classes") + 
  ylab("Share of positive posts") +
  ggtitle("Share of positive posts per Instagram account") +
  theme_light() +
  theme(panel.border = element_blank())

by_account_plot


# 05 - ANOVA SiEBERT ##########################################

## 05.01 - Requirements #######################################

hartleyTest(by_account$rel.positives, by_account$size.new)               # Hartley (F_max) Test for Homoscedasticity

# test for normal distribution
make_plot <- function(by_account, steps, sizeclass) {
  by_acct_mean <- tapply(by_account$rel.positives, by_account$size.new, mean)
  by_acct_sd <- tapply(by_account$rel.positives, by_account$size.new, sd)
  
  # Beobachtungen ################################################################
  
  sort_by_account <- by_account[order(by_account$rel.positives),]
  
  sort_by_account$posclass <- floor(sort_by_account$rel.positives / steps)
  
  graphdata <- sort_by_account %>%
    filter(size.new == sizeclass) %>%
    select("posclass") %>%
    table %>%
    as.data.frame
  
  graphdata$posclass <- as.numeric(as.character(graphdata$posclass))
  
  graphdata <- merge(data.frame(posclass = 1:(1/steps)), graphdata, by = 'posclass', all.x = TRUE, sort = TRUE)
  
  graphdata$Freq[is.na(graphdata$Freq)] <- 0
  
  graphdata$min <- graphdata$posclass * steps
  graphdata$max <- (graphdata$posclass + 1) * steps
  
  
  # Normalverteilung #############################################################
  
  graphdata$norm <- (pnorm(graphdata$max, mean = by_acct_mean[sizeclass], sd = by_acct_sd[sizeclass]) - pnorm(graphdata$min, mean = by_acct_mean[sizeclass], sd = by_acct_sd[sizeclass])) * table(by_account$size.new)[sizeclass]
  
  #graphdata$norm <- dnorm(x = graphdata$posclass, mean = by_acct_mean[sizeclass], sd = by_acct_sd[sizeclass]) * 22
  
  graphdata <- reshape(graphdata, direction = 'long', varying = c("Freq", "norm"), v.names = "Freq", timevar = "source", times = c("Freq", "Norm"))
  
  # Graph ########################################################################
  
  title <- sizeclass
  

  graphdata_graph <- ggplot(graphdata) +
    geom_col(aes(x = posclass, y = Freq, fill = source), position = "dodge") +
    ggtitle(title) +
    theme_light() +
    theme(panel.border = element_blank()) +
    theme(legend.position = "none") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  return(graphdata_graph)
}



a <- lapply(c("nano", "micro", "midtier", "macro", "mega"), function(size) {
  make_plot(by_account = by_account, steps = 0.1, sizeclass = size)
})

legend <- ggplot(data.frame(Legend = c("empirical distribution","normal distribution","normal distribution","empirical distribution"), b = c(1,1,1,1), c = c(2,2,2,2))) +
  geom_col(aes(x = b, y = c, fill = Legend)) +
  theme(legend.title = element_blank())

legend <- as_ggplot(get_legend(legend))

a <- append(a, list(legend))

do.call("grid.arrange", c(a, ncol=2))      # Adjusted from https://stackoverflow.com/questions/10706753/how-do-i-arrange-a-variable-list-of-plots-using-grid-arrange

anova <- aov(by_account$rel.negatives ~ by_account$size.new)
summary(anova)










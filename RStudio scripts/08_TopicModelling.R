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
install.packages("quanteda")
install.packages("ldaPrototype")
install.packages("Twitmo")
install.packages("textmineR")
install.packages("LDAvis")


# 02 - Setup ##################################################

set.seed(123)

library(tidyverse)
library(readxl)
library(quanteda)
library(ldaPrototype)
library(Twitmo)
library(textmineR)
library(LDAvis)
library(Matrix)
library(slam)

# 03 - Data Import ############################################

posts <- read_delim("data/Instagram__Posts_corrected.tsv", delim = "\t", quote="", col_types = "ccnTccnnccccc")
posts <- select(posts, c("Username (shortcode)", "Post ID", "Text"))
lemma <- read.delim("lemmatization-en.txt", sep = "\t")

posts <- posts[-which(duplicated(posts$`Post ID`)),]                 # remove duplicates
posts$Text <- gsub("\\\\n", ' ', posts$Text)                           # replace \n with space

# 04 - Preprocessing ##########################################

# 04.01 Tokenization

posttexts <- as.vector(posts$Text)
posttexts <- setNames(posttexts, posts$`Post ID`)

tok <- tokens(posttexts, split_tags = TRUE)

# 04.02 Convert to lowercase
tok <- tokens_tolower(tok)


# 04.03 Remove punctuation
tok <- tokens(tok, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)


# 04.04 Remove Stop words
tok <- tokens_remove(tok, pattern = c(stopwords("en"), "n", "i’m"))

# 04.05 Lemmatization
colnames(lemma) <- c("base", "variant")
lemma$base <- tolower(lemma$base)
lemma$variant <- tolower(lemma$variant)

lemma <- lemma[-which(lemma$variant == "data"),]          # don't change "data" to "datum"
lemma <- lemma[-which(lemma$variant == "sentencing"),]    # don't change "sentencing" to "sentence"
lemma <- lemma[!duplicated(lemma$variant),]               # remove duplicates

tok_lemmatized <- tokens_replace(tok, lemma$variant, lemma$base)

DFM <- dfm(tok_lemmatized)



# 04.06 Pruning ###############################################
# 04.06.01 Effect of Pruning ##################################

# How many words are included if words are excluded that don't appear at least count times?
get_countaffected <- function(dfm, count) {
  trimmed <- dfm_trim(dfm, min_termfreq = count)
  return(nfeat(trimmed))
}

# Ho many words are included if words are excluded that don't appear in at least prop% of the documents
get_propaffected <- function(dfm, prop) {
  trimmed <- dfm_trim(dfm, min_docfreq = prop, docfreq_type = "prop")
  return(nfeat(trimmed))
}

affected_count <- data.frame(count = 1:10)
affected_prop <- data.frame(prop = seq(0.00001, 0.001, by = 0.00002))
affected_count$affected <- sapply(X = affected_count$count, FUN = get_countaffected, dfm = DFM)
affected_prop$affected  <- sapply(X = affected_prop$prop, FUN = get_propaffected, dfm = DFM)

plot_countaffected <- ggplot(affected_count, aes(x = count, y = affected)) +
  geom_line() +
  xlab("Minimum occurence of word in documents") +
  ylab("vocabulary size") +
  ggtitle("Effect of absolute pruning") +
  scale_x_continuous(minor_breaks = seq(1, 25, by = 1)) +
  theme_light()
plot_countaffected

plot_propaffected <- ggplot(affected_prop, aes(x = prop, y = affected)) +
  geom_line() +
  xlab("occurence in at least x% of documents") +
  ylab("vocabulary size") +
  ggtitle("Effect of relative pruning") +
  theme_light()
  #scale_x_continuous(minor_breaks = seq(0, 25, by = 1))
plot_propaffected



# 04.06.02 Execution ##########################################

DFM_trimmed <- dfm_trim(DFM, min_termfreq = 2)

topfeatures(DFM_trimmed)

dfm_matrix <- convert(DFM_trimmed, to = "lda")


# 05 - Model Generation #######################################

K_range <- seq(4, 35, 1)                                             # Adapted from Bittermann (2022a)
candidate_models <- list()
model_iter <- 100

for (i in 1:length(K_range)){
  print(paste(i, " / ", length(K_range)))
  candidate_models[[i]] <- FitLdaModel(dtm = DFM_trimmed,
                                       k = K_range[i],
                                       iterations = model_iter,
                                       alpha = 1/K_range[i],
                                       optimize_alpha = TRUE)
}




# 06 - LDAvis #################################################

model <- candidate_models[[20]]

jsona <- createJSON(phi = model$phi, theta = model$theta, doc.length = as.vector(ntoken(DFM_trimmed)), vocab = colnames(model$phi), term.frequency = TermDocFreq(model$data)$term_freq)
serVis(jsona)

remove(model)

# 07 -  Coherence and Exclusivity #############################

exclusivity <- function(model, dfm, num.words) {                     # Adapted from Bittermann (2022a)
  phi <- model$phi
  p_w <- colSums(dfm)/sum(dfm)
  
  res <- apply(phi, 1, function(x, num.words, pw) {
    x <- log(x/pw)
    return((sort(x, decreasing = TRUE)[1:num.words]))
  }, num.words, p_w)
  
  return(colMeans(res))
}

coherence <- function(model, DFM, N = 10) {                          # Adapted from Wiedemann and Niekler (2017)
  
  # Ensure matrix or Matrix-format (convert if SparseM)
  require(Matrix)
  require(slam)
  if (is.simple_triplet_matrix(DFM)) {
    DFM <- sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }
  
  DTMBIN <- DFM
  DTMBIN[DTMBIN > 0] <- 1
  
  documentFrequency <- colSums(DTMBIN)
  names(documentFrequency) <- colnames(DTMBIN)
  
  K <- dim(model$phi)[1]
  
  topNtermsPerTopic <- GetTopTerms(model$phi, M = N)
  allTopicModelTerms <- unique(as.vector(topNtermsPerTopic))
  
  DTMpreprocessed <- DTMBIN[, allTopicModelTerms]
  DTMpreprocessedCooc <- t(DTMpreprocessed) %*% DTMpreprocessed
  DTMpreprocessedCooc <- t((DTMpreprocessedCooc + 1) / colSums(DTMpreprocessed))
  DTMpreprocessedCooc <- log(DTMpreprocessedCooc)
  DTMpreprocessedCooc <- as.matrix(DTMpreprocessedCooc)
  
  coherence <- rep(0, K)
  pb <- txtProgressBar(max = K)
  for (topicIdx in 1:K) {
    setTxtProgressBar(pb, topicIdx)
    topWordsOfTopic <- topNtermsPerTopic[, topicIdx]
    coherence[topicIdx] <- 0
    for (m in 2:length(topWordsOfTopic)) {
      for (l in 1:(m-1)) {
        mTerm <- topWordsOfTopic[m]
        lTerm <- topWordsOfTopic[l]
        coherence[topicIdx] <- coherence[topicIdx] + DTMpreprocessedCooc[mTerm, lTerm]
      }
    }
  }
  close(pb)
  return(coherence)
}
  
  
  
exc <- lapply(candidate_models, exclusivity, DFM_trimmed, num.words = 10)
exc_mean <- unlist(lapply(exc, mean))

coh <- lapply(candidate_models, coherence, DFM_trimmed, N = 10)
coh_mean <- unlist(lapply(coh, mean))


graph_df <- data.frame(scale(exc_mean), scale(coh_mean))
graph_df$mean <- apply(graph_df, 1, mean)
graph_df$x <- min(K_range):max(K_range)
colnames(graph_df) <- c("excl", "cohe", "mean", "x")

graph <- ggplot(graph_df, aes(y = exc_mean)) +
  geom_line(aes(x = x, y = excl, colour = "exclusivity"), linewidth = 1) +
  geom_line(aes(x = x, y = cohe, colour = "coherence"), linewidth = 1) +
  geom_line(aes(x = x, y = mean, colour = "mean"), linewidth = 1) +
  scale_color_manual(name = "Quality Metrics", values = c("exclusivity" = "darkblue", "coherence" = "darkred", "mean" = "darkgreen")) +
  ggtitle("Topic Quality Metrics per post") +
  xlab("number of topics K") +
  ylab("Scaled Score") +
  scale_x_continuous(breaks = seq(min(graph_df$x), max(graph_df$x), by = 2)) +
  theme_light()

graph



# 08 - play the same song again (aggregate to user level) #####

user_level_data <- posts %>%
  group_by(`Username (shortcode)`) %>%
  summarise(fulltext = paste(Text, collapse = " "))

acctexts <- as.vector(user_level_data$fulltext)
acctexts <- setNames(acctexts, user_level_data$`Username (shortcode)`)

acctok <- tokens(acctexts, split_tags = TRUE)


acctok <- tokens_tolower(acctok)    # Convert to lowercase
acctok <- tokens(acctok, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)  # Remove punctuation
acctok <- tokens_remove(acctok, pattern = c(stopwords("en"), "n", "i’m"))   # Remove Stop words

# Lemmatization
acctok_lemmatized <- tokens_replace(acctok, lemma$variant, lemma$base)

accDFM <- dfm(acctok_lemmatized)



# DECIDE PRUNING
acc_affected_count <- data.frame(count = 1:10)
acc_affected_prop <- data.frame(prop = seq(0.00001, 0.001, by = 0.00002))
acc_affected_count$affected <- sapply(X = acc_affected_count$count, FUN = get_countaffected, dfm = DFM)
acc_affected_prop$affected  <- sapply(X = acc_affected_prop$prop, FUN = get_propaffected, dfm = DFM)

plot_acc_countaffected <- ggplot(acc_affected_count, aes(x = count, y = affected)) +
  geom_line() +
  xlab("Minimum occurence of word in documents") +
  ylab("vocabulary size") +
  ggtitle("Effect of absolute pruning") +
  scale_x_continuous(minor_breaks = seq(1, 10, by = 0.5))
plot_acc_countaffected

plot_acc_propaffected <- ggplot(acc_affected_prop, aes(x = prop, y = affected)) +
  geom_line() +
  xlab("occurence in at least x% of documents") +
  ylab("vocabulary size") +
  ggtitle("Effect of relative pruning")
  #scale_x_continuous(minor_breaks = seq(0, 25, by = 1))
plot_acc_propaffected


acc_DFM_trimmed <- dfm_trim(accDFM, min_termfreq = 2)
topfeatures(acc_DFM_trimmed)
acc_dfm_matrix <- convert(acc_DFM_trimmed, to = "lda")

acc_K_range <- seq(4, 35, 1)                                             # Adapted from Bittermann (2022a)
acc_candidate_models <- list()
acc_model_iter <- 100

for (i in 1:length(acc_K_range)){
  print(paste(i, " / ", length(acc_K_range)))
  acc_candidate_models[[i]] <- FitLdaModel(dtm = acc_DFM_trimmed,
                                       k = acc_K_range[i],
                                       iterations = acc_model_iter,
                                       alpha = 1/acc_K_range[i],
                                       optimize_alpha = TRUE)
}



acc_exc <- lapply(acc_candidate_models, exclusivity, acc_DFM_trimmed, num.words = 10)
acc_exc_mean <- unlist(lapply(acc_exc, mean))

acc_coh <- lapply(acc_candidate_models, coherence, acc_DFM_trimmed, N = 10)
acc_coh_mean <- unlist(lapply(acc_coh, mean))


acc_graph_df <- data.frame(scale(acc_exc_mean), scale(acc_coh_mean))
acc_graph_df$mean <- apply(acc_graph_df, 1, mean)
acc_graph_df$x <- min(acc_K_range):max(acc_K_range)
colnames(acc_graph_df) <- c("excl", "cohe", "mean", "x")

acc_graph <- ggplot(acc_graph_df, aes(y = mean)) +
  geom_line(aes(x = x, y = excl, colour = "exclusivity"), linewidth = 1) +
  geom_line(aes(x = x, y = cohe, colour = "coherence"), linewidth = 1) +
  geom_line(aes(x = x, y = mean, colour = "mean"), linewidth = 1) +
  scale_color_manual(name = "Quality Metrics", values = c("exclusivity" = "darkblue", "coherence" = "darkred", "mean" = "darkgreen")) +
  ggtitle("Topic Quality Metrics per account") +
  xlab("number of topics K") +
  ylab("Scaled Score") +
  scale_x_continuous(breaks = seq(min(graph_df$x), max(graph_df$x), by = 2)) +
  theme_light()

acc_graph



# 09 - Manual Labeling of Topics ##############################          
                                                                     # adapted from Bittermann (2022a)

# account basis
acc_select <- acc_K_range %in% c(6,15,19,22,29,31,34)
acc_candidates_inspect <- acc_candidate_models[acc_select]

for (i in 1:length(acc_candidates_inspect)){
  print(paste("### Topics for k =", dim(acc_candidates_inspect[[i]]$phi)[1], "###"))
  print(GetTopTerms(acc_candidates_inspect[[i]]$phi, 10))
}

acc_final_model <- acc_candidates_inspect[[7]]

acc_labels = c("REMOVE",
               "Cooking",
               "Skin & Beauty",
               "REMOVE",
               "U.S. politics",
               "Religion",
               "Fitness",
               "TV and series",
               "Fashion Shopping",
               "Shopping Promo",
               "Hair",
               "REMOVE",
               "Basketball",
               "Motivation",
               "REMOVE",
               "Fashion Shopping",
               "REMOVE",
               "Hair",
               "REMOVE",
               "Street Fashion",
               "Music Festivals",
               "Sports",
               "Hair",
               "Stand-Up Comedy",
               "REMOVE",
               "Hair",
               "Makeup & Cosmetics",
               "Happy / Celebrations",
               "Entrepreneur",
               "REMOVE",
               "Michigan",
               "REMOVE",
               "REMOVE",
               "Horoscope")

acc_theta <- as.data.frame(acc_final_model$theta)
acc_theta <- acc_theta[,-which(acc_labels == "REMOVE")]

acc_max_theta <- apply(acc_theta, 1, function(x){which.max(x)})
acc_max_theta <- as.data.frame(acc_max_theta)
acc_max_theta$label <- acc_labels[-which(acc_labels == "REMOVE")][acc_max_theta$acc_max_theta]


acc_plot_max_theta <- ggplot(acc_max_theta, aes(x = label)) +
  geom_bar() +
  ggtitle("Topic Distribution on account level (k = 24)") +
  xlab("Topic Label") +
  ylab("Number of accounts") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

acc_plot_max_theta


# post basis
select <- K_range %in% c(33)
candidates_inspect <- candidate_models[select]

for (i in 1:length(candidates_inspect)){
  print(paste("### Topics for k =", dim(candidates_inspect[[i]]$phi)[1], "###"))
  print(GetTopTerms(candidates_inspect[[i]]$phi, 10))
}

final_model <- candidates_inspect[[1]]

labels <- c("Horoscope",
            "REMOVE",
            "Health",
            "REMOVE",
            "REMOVE",
            "Street Fashion",
            "Beauty & Makeup",
            "Hair",
            "Music Festival",
            "REMOVE",
            "Hair",
            "Happy / Celebrations",
            "Hair",
            "Michigan",
            "Fashion",
            "Hair",
            "REMOVE",
            "TV and series",
            "U.S. politics",
            "Giveaway",
            "Basketball",
            "Football",
            "Hair",
            "Cooking",
            "REMOVE",
            "Fitness",
            "Stand-Up Comedy",
            "REMOVE",
            "TV and series",
            "Photography",
            "Fitness",
            "Hair",
            "Beauty & Makeup"
            )

theta <- as.data.frame(final_model$theta)
theta <- theta[,-which(labels == "REMOVE")]

max_theta <- apply(theta, 1, function(x){which.max(x)})
max_theta <- as.data.frame(max_theta)
max_theta$label <- labels[-which(labels == "REMOVE")][max_theta$max_theta]


plot_max_theta <- ggplot(max_theta, aes(x = label)) +
  geom_bar() +
  ggtitle("Topic Distribution on post level (k = 26)") +
  xlab("Topic Label") +
  ylab("Number of posts") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

plot_max_theta


# 10 - Finally export this data for insertion #################

# ACCOUNT
acc_max_theta$account <- rownames(acc_max_theta)
rownames(acc_max_theta) <- 1:dim(acc_max_theta)[1]
acc_max_theta <- acc_max_theta[,-1]

write_csv(acc_max_theta, 'data/topics_account.csv')


# POST
max_theta$Post.ID <- rownames(max_theta)
rownames(max_theta) <- 1:dim(max_theta)[1]
max_theta <- max_theta[,-1]

write_csv(max_theta, 'data/topics_posts.csv')




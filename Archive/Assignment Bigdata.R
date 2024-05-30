library(vosonSML)
library(igraph)
library(tuber)


#DATA COLLECTION
my_api_key <- 'AIzaSyD-dsawrlag5P0R2TgjGvcbmHeTwccWr0A'

yt_auth <- Authenticate('youtube', apiKey = my_api_key)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#QUESTION 2: COLLECT DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##Video 1 "Kill this love"##
video1_url <- c('https://www.youtube.com/watch?v=2S24-y0Ij3Y')

video1_yt_data <- yt_auth |> Collect(videoIDs = video1_url,
                              maxComments = 5000,
                              writeToFile = TRUE,
                              verbose = TRUE)
View(video1_yt_data)

##Video 2 "Pink Venom" ##

video2_url <- c('https://www.youtube.com/watch?v=gQlMMD8auMs')

video2_yt_data  <- yt_auth |> Collect(videoIDs = video2_url,
                               maxComments = 5000,
                               writeToFile = TRUE,
                               verbose = TRUE)
View(video2_yt_data )
## Video 3 "Stay" ###
video3_url <- c('https://www.youtube.com/watch?v=FzVR_fymZw4')

video3_yt_data <- yt_auth |> Collect(videoIDs = video3_url,
                               maxComments = 5000,
                               writeToFile = TRUE,
                               verbose = TRUE)

View(video3_yt_data)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#QUESTION 3 -ACTOR NETWORK - PageRank - YOUTUBE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###Video 1: "KILL THIS LOVE"###
video1_actor_network_yt <- video1_yt_data |> Create('actor')
video1_actor_network_graph_yt <- video1_actor_network_yt |> Graph()
plot(video1_actor_network_graph_yt, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)
write_graph(video1_actor_network_graph_yt, file = 'YoutubeActorVideo1.graphml', format = 'graphml')
video1_rank_actor_network_yt <- sort(page_rank(video1_actor_network_graph_yt)$vector, decreasing = TRUE)
V(video1_actor_network_graph_yt)$name <- V(video1_actor_network_graph_yt)$screen_name
video1_rank_actor_network_yt <- sort(page_rank(video1_actor_network_graph_yt)$vector, decreasing = TRUE)
head(video1_rank_actor_network_yt, n = 5)
video1_rank_actor_network_yt <- video1_rank_actor_network_yt[!is.na(names(video1_rank_actor_network_yt))]
head(video1_rank_actor_network_yt, n = 5)

# Create a DataFrame with the top 5 actors
video1_top_actors <- head(video1_rank_actor_network_yt, n = 5)
video1_top_actors_names <- names(video1_top_actors)

# Create a DataFrame for visualization
video1_top_actors_df <- data.frame(
  Actor = video1_top_actors_names,
  PageRank = as.numeric(video1_top_actors)
)
library(ggplot2)
# Plot the top actors by PageRank
ggplot(video1_top_actors_df, aes(x = reorder(Actor, -PageRank), y = PageRank, fill = Actor)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 5 Actors by PageRank Score: Kill This Love",
    x = "Actor",
    y = "PageRank Score"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

###VIDEO 2: "PINK VENOM" ###
video2_actor_network_yt <- video2_yt_data |> Create('actor')
video2_actor_network_graph_yt <- video2_actor_network_yt |> Graph()
plot(video2_actor_network_graph_yt, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)
write_graph(video2_actor_network_graph_yt, file = 'YoutubeActorVideo2.graphml', format = 'graphml')
video2_rank_actor_network_yt <- sort(page_rank(video2_actor_network_graph_yt)$vector, decreasing = TRUE)
V(video2_actor_network_graph_yt)$name <- V(video2_actor_network_graph_yt)$screen_name
video2_rank_actor_network_yt <- sort(page_rank(video2_actor_network_graph_yt)$vector, decreasing = TRUE)
head(video2_rank_actor_network_yt, n = 5)
#Remove <NA> value
video2_rank_actor_network_yt <- video2_rank_actor_network_yt[!is.na(names(video2_rank_actor_network_yt))]
head(video2_rank_actor_network_yt, n = 5)

# Create a DataFrame with the top 5 actors
video2_top_actors <- head(video2_rank_actor_network_yt, n = 5)
video2_top_actors_names <- names(video2_top_actors)

# Create a DataFrame for visualization
video2_top_actors_df <- data.frame(
  Actor = video2_top_actors_names,
  PageRank = as.numeric(video2_top_actors)
)
library(ggplot2)
# Plot the top actors by PageRank
ggplot(video2_top_actors_df, aes(x = reorder(Actor, -PageRank), y = PageRank, fill = Actor)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 5 Actors by PageRank Score: Pink Venom",
    x = "Actor",
    y = "PageRank Score"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

####Video 3: "STAY" ####
video3_actor_network_yt <- video3_yt_data |> Create('actor')
video3_actor_network_graph_yt <- video3_actor_network_yt |> Graph()
plot(video3_actor_network_graph_yt, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)
write_graph(video3_actor_network_graph_yt, file = 'YoutubeActorVideo2.graphml', format = 'graphml')
video3_rank_actor_network_yt <- sort(page_rank(video3_actor_network_graph_yt)$vector, decreasing = TRUE)
V(video3_actor_network_graph_yt)$name <- V(video3_actor_network_graph_yt)$screen_name
video3_rank_actor_network_yt <- sort(page_rank(video3_actor_network_graph_yt)$vector, decreasing = TRUE)
head(video3_rank_actor_network_yt, n = 5)
#Remove <NA> value
video3_rank_actor_network_yt <- video3_rank_actor_network_yt[!is.na(names(video3_rank_actor_network_yt))]
head(video3_rank_actor_network_yt, n = 5)

# Create a DataFrame with the top 5 actors
video3_top_actors <- head(video3_rank_actor_network_yt, n = 5)
video3_top_actors_names <- names(video3_top_actors)

# Create a DataFrame for visualization
video3_top_actors_df <- data.frame(
  Actor = video3_top_actors_names,
  PageRank = as.numeric(video3_top_actors)
)
library(ggplot2)
# Plot the top actors by PageRank
ggplot(video3_top_actors_df, aes(x = reorder(Actor, -PageRank), y = PageRank, fill = Actor)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 5 Actors by PageRank Score: Stay",
    x = "Actor",
    y = "PageRank Score"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#QUESTION4: UNIQUE ACTORS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Video 1: "KILL THIS LOVE"##
# Calculate the number of unique actors
video1_num_unique_actors_yt <- length(unique(video1_yt_data$AuthorDisplayName))
print(video1_num_unique_actors_yt)

##Video 2: "PINK VENOM"##
# Calculate the number of unique actors
video2_num_unique_actors_yt <- length(unique(video2_yt_data$AuthorDisplayName))
print(video2_num_unique_actors_yt)

#Video 3: "STAY"##
# Calculate the number of unique actors
video3_num_unique_actors_yt <- length(unique(video3_yt_data$AuthorDisplayName))
print(video3_num_unique_actors_yt)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#QUESTION 5: SPOTIFY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

install.packages("spotifyr")
library(ggplot2)
library(ggridges)
library(spotifyr)

app_id <- 'c3e13276a142417c8792f0f1a678df21'
app_secret <- '939a0a3828e54e02a9298256936d6b47'

# Authentication for spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()

# Find some data on the artist BlackPink
find_my_artist <- search_spotify('BlackPink', type = 'artist')
View(find_my_artist)

# Find albums
albums <- get_artist_albums("41MozSoPIsD1dJM0CLPjZF", include_groups = c("album", "single", "appears_on", "compilation"))
View(albums)

#5.1 How many years have they been active?

song_data <- data.frame(Name = albums$name,
                        Release_Date = as.Date(albums$release_date))

sorted_songs <- song_data[order(song_data$Release_Date), ]

print(sorted_songs)

#5.2 What are the prevalent features of their songs?

audio_features <- get_artist_audio_features('41MozSoPIsD1dJM0CLPjZF')
View(audio_features)

audio_features <- audio_features[!duplicated(audio_features$track_name),]

#Use the ggplot2 and ggrides packages to visualise the audio features grouped by album:

ggplot(audio_features, aes(x = valence, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle('Catchy in BlackPink Albums',
          subtitle = "Based on valence from Spotify's Web API")


# Find some data on the artist BlackPink
find_my_artist <- search_spotify('BlackPink', type = 'artist')
View(find_my_artist)

#Get information about the artist related to BlackPink
related_bm <- get_related_artists('41MozSoPIsD1dJM0CLPjZF')
View(related_bm)

# Create a new data frame related to artirst
related_all <- related_bm

for (artist in related_bm$id){
  related <- get_related_artists(artist)
  related_all <- rbind(related_all, related)
}
View(related_all)
#Remove duplicate rows
related_unique <- unique(related_all)
View(related_unique)

#ANALYSE DATA VISUALLY AND PREPARE IT FOR CLUSTERING
ggplot(related_unique, aes(x = popularity, y = followers.total)) +
  geom_point(stat = 'identity') +
  xlab('Popularity') +
  ylab('Followers Count') +
  ggtitle('Popularity vs Followers')

# Count outliers

ggplot(related_unique, aes(x= (popularity), y=log(followers.total)))+
  geom_point(stat = 'identity') + 
  xlab('Popularity') +
  ylab('Log Followers Count') +
  ggtitle('Popularity vs Log Followers')

# New dataframe name related_log
log_followers_count <- log(related_unique$followers.total)
related_log <- data.frame(related_unique$name, related_unique$popularity, log(related_unique$followers.total))
View(related_log)

#Run the k-means clustering algorithm
artist_clusters <- kmeans(related_log[ , 2:3],
                          centers = 5,
                          iter.max = 10,
                          nstart = 10)

related_log$cluster <- factor(artist_clusters$cluster)
View(related_log)

#Visualisation
ggplot(related_log, aes(x = related_log[ , 2], 
                        y = related_log[ , 3],
                        colour = cluster)) +
  geom_point(stat = "identity") + 
  xlab("Popularity") + 
  ylab("Log Followers Count") + 
  ggtitle("Popularity vs Log Followers")
#Look at the values of the cluster centres
artist_clusters$centers
write.csv(albums, file = "BlackPinkAlbums.csv")

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#QUESTION 6 - Term-Document Matrices~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
library(vosonSML)
library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(ggplot2)
library(igraph)

#VIDEO 1: "KILL THIS LOVE"
# Text Cleaning
video1_yt_clean_text <- video1_yt_data$Comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> 
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation() |> 
  replace_emoji() |>
  replace_emoticon()

# Create Corpus
video1_yt_corpus <- VCorpus(VectorSource(video1_yt_clean_text))
video1_yt_corpus[[1]]$content
video1_yt_corpus[[5]]$content

# Further Preprocessing
video1_yt_corpus <- tm_map(video1_yt_corpus, content_transformer(tolower))
video1_yt_corpus <- tm_map(video1_yt_corpus, removeWords, stopwords(kind = "SMART"))
video1_yt_corpus <- tm_map(video1_yt_corpus, stripWhitespace)

# Term-Document Matrix
video1_dtm_yt <- DocumentTermMatrix(video1_yt_corpus)

# Sort words by total frequency across all documents
video1_freq_yt <- sort(colSums(as.matrix(video1_dtm_yt)), decreasing = TRUE)

# Extract top 10 terms with highest frequency
video1_top_terms_yt <- head(video1_freq_yt, n = 10)

# Print the results
print(video1_top_terms_yt )

#Create dataframe
video1_word_frequ_df_yt <- data.frame(word = names(video1_top_terms_yt), video1_top_terms_yt )
# Convert the top_terms object to a data frame
video1_word_frequ_df_yt <- data.frame(word = names(video1_top_terms_yt ), frequency = video1_top_terms_yt )
ggplot(subset(video1_word_frequ_df_yt), aes(x = reorder(word, -video1_top_terms_yt ), y = video1_top_terms_yt)) +
  geom_bar(stat = "identity", fill = "lightpink", width = 0.6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
        axis.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold")) +
  labs(title = "Top 10 Word Frequency in Kill This Love video",
       x = "Words",
       y = "Frequency") +
  coord_flip()
print(video1_word_frequ_df_yt)

#VIDEO 2: "PINK VENOM"
# Text Cleaning
video2_yt_clean_text <- video2_yt_data$Comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> 
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation() |> 
  replace_emoji() |>
  replace_emoticon()

# Create Corpus
video2_yt_corpus <- VCorpus(VectorSource(video2_yt_clean_text))
video2_yt_corpus[[1]]$content
video2_yt_corpus[[5]]$content

# Further Preprocessing
video2_yt_corpus <- tm_map(video2_yt_corpus, content_transformer(tolower))
video2_yt_corpus <- tm_map(video2_yt_corpus, removeWords, stopwords(kind = "SMART"))
video2_yt_corpus <- tm_map(video2_yt_corpus, stripWhitespace)

# Term-Document Matrix
video2_dtm_yt <- DocumentTermMatrix(video2_yt_corpus)

# Sort words by total frequency across all documents
video2_freq_yt <- sort(colSums(as.matrix(video2_dtm_yt)), decreasing = TRUE)

# Extract top 10 terms with highest frequency
video2_top_terms_yt <- head(video2_freq_yt, n = 10)

# Print the results
print(video2_top_terms_yt)
video2_word_frequ_df_yt <- data.frame(word = names(video2_top_terms_yt), video2_top_terms_yt)

# Convert the top_terms object to a data frame
video2_word_frequ_df_yt <- data.frame(word = names(video2_top_terms_yt), frequency = video2_top_terms_yt)
ggplot(subset(video2_word_frequ_df_yt), aes(x = reorder(word, -video2_top_terms_yt), y = video2_top_terms_yt)) +
  geom_bar(stat = "identity", fill = "deeppink", width = 0.6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
        axis.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold")) +
  labs(title = "Top 10 Word Frequency in Pink Venom video",
       x = "Words",
       y = "Frequency") +
  coord_flip()

#VIDEO 3: "STAY"
# Text Cleaning
video3_yt_clean_text <- video3_yt_data$Comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> 
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation() |> 
  replace_emoji() |>
  replace_emoticon()

# Create Corpus
video3_yt_corpus <- VCorpus(VectorSource(video3_yt_clean_text))
video3_yt_corpus[[1]]$content
video3_yt_corpus[[5]]$content

# Further Preprocessing
video3_yt_corpus <- tm_map(video3_yt_corpus, content_transformer(tolower))
video3_yt_corpus <- tm_map(video3_yt_corpus, removeWords, stopwords(kind = "SMART"))
video3_yt_corpus <- tm_map(video3_yt_corpus, stripWhitespace)

# Term-Document Matrix
video3_dtm_yt <- DocumentTermMatrix(video3_yt_corpus)

# Sort words by total frequency across all documents
video3_freq_yt <- sort(colSums(as.matrix(video3_dtm_yt)), decreasing = TRUE)

# Extract top 10 terms with highest frequency
video3_top_terms_yt <- head(video3_freq_yt, n = 10)

# Print the results
print(video3_top_terms_yt)
video3_word_frequ_df_yt <- data.frame(word = names(video3_top_terms_yt), video3_top_terms_yt)


# Convert the top_terms object to a data frame
video3_word_frequ_df_yt <- data.frame(word = names(video3_top_terms_yt), frequency = video3_top_terms_yt)
ggplot(subset(video3_word_frequ_df_yt), aes(x = reorder(word, video3_top_terms_yt), y = video3_top_terms_yt)) +
  geom_bar(stat = "identity", fill = "black", width = 0.6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
        axis.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold")) +
  labs(title = "Top 10 Word Frequency in Stay video",
       x = "Words",
       y = "Frequency") +
  coord_flip()
print(video3_word_frequ_df_yt)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #QUESTION 7: TEXT PROCESSING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(vosonSML)
library(dplyr)
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(ggplot2)
library(igraph)
#VIDEO 1: KILL THIS LOVE
#Convert to be dataframe
video1_yt_clean_df <- data.frame(video1_yt_clean_text)
#BIGRAM TOKENISATION
video1_yt_bigrams <- video1_yt_clean_df |> unnest_tokens(output = bigram,
                                           input = video1_yt_clean_text,
                                           token ='ngrams',
                                           n=2)
View(video1_yt_bigrams)

#Count the number of occurrences, split them across two columns
video1_yt_bigrams_table <- video1_yt_bigrams |>
  count(bigram, sort = TRUE) |>
  separate(bigram, c('left','right'))

#Remove stopwords
video1_yt_bigrams_nonstops <- video1_yt_bigrams_table |>
  anti_join(stop_words, join_by(left== word)) |>
  anti_join(stop_words,join_by(right == word))
View(video1_yt_bigrams_nonstops)
video1_yt_bigrams_nonstops <- video1_yt_bigrams_nonstops[complete.cases(video1_yt_bigrams_nonstops),]
View(video1_yt_bigrams_nonstops)

#Keep only those that occur at least twice
video1_yt_bigrams_nonstops <- video1_yt_bigrams_nonstops |> filter(n >=2)
View(video1_yt_bigrams_nonstops)

#Semantic Network Graph
video1_yt_bigrams_graph <- graph_from_data_frame(video1_yt_bigrams_nonstops,directed = FALSE)

#Get the number of nodes and edges
vcount(video1_yt_bigrams_graph)
ecount(video1_yt_bigrams_graph)
#Remove any loops or multiple edges between the same two words:
video1_yt_bigrams_graph <- simplify(video1_yt_bigrams_graph)
vcount(video1_yt_bigrams_graph)
ecount(video1_yt_bigrams_graph)

#Page Rank algorithm on our semantic network:
video1_rank_rd_bigram <- sort(page_rank(video1_yt_bigrams_graph)$vector, decreasing = TRUE)
video1_rank_rd_bigram[1:10]

#VISUALISATION
video1_rank_rd_bigram_top10 <-video1_rank_rd_bigram[1:10]

# Subset the original graph to include only the top 10 nodes
video1_top10_nodes <- names(video1_rank_rd_bigram_top10)
video1_subgraph_top10 <- induced_subgraph(video1_yt_bigrams_graph, vids = video1_top10_nodes)

# Plot the subgraph
plot(video1_subgraph_top10, vertex.label = NA, vertex.size = 4, edge.arrow.size = 0.5)

# Write the subgraph to a file
write_graph(video1_subgraph_top10, file = 'Ranktop10_video1.graphml', format = 'graphml')

#VIDEO 2: PINK VENOM

#Convert to be dataframe
video2_yt_clean_df <- data.frame(video2_yt_clean_text)
#BIGRAM TOKENISATION
video2_yt_bigrams <- video2_yt_clean_df |> unnest_tokens(output = bigram,
                                                         input = video2_yt_clean_text,
                                                         token ='ngrams',
                                                         n=2)
View(video2_yt_bigrams)

#Count the number of occurrences, split them across two columns
video2_yt_bigrams_table <- video2_yt_bigrams |>
  count(bigram, sort = TRUE) |>
  separate(bigram, c('left','right'))

View(video2_yt_bigrams_table)

#Remove stopwords
video2_yt_bigrams_nonstops <- video2_yt_bigrams_table |>
  anti_join(stop_words, join_by(left== word)) |>
  anti_join(stop_words,join_by(right == word))
View(video2_yt_bigrams_nonstops)
video2_yt_bigrams_nonstops <- video2_yt_bigrams_nonstops[complete.cases(video2_yt_bigrams_nonstops),]
View(video2_yt_bigrams_nonstops)

#Keep only those that occur at least twice
video2_yt_bigrams_nonstops <- video2_yt_bigrams_nonstops |> filter(n >=2)
View(video2_yt_bigrams_nonstops)

#Semantic Network Graph
video2_yt_bigrams_graph <- graph_from_data_frame(video2_yt_bigrams_nonstops,directed = FALSE)

#Get the number of nodes and edges
vcount(video2_yt_bigrams_graph)
ecount(video2_yt_bigrams_graph)
#Remove any loops or multiple edges between the same two words:
video2_yt_bigrams_graph <- simplify(video2_yt_bigrams_graph)
vcount(video2_yt_bigrams_graph)
ecount(video2_yt_bigrams_graph)

#Page Rank algorithm on our semantic network:
video2_rank_rd_bigram <- sort(page_rank(video2_yt_bigrams_graph)$vector, decreasing = TRUE)
video2_rank_rd_bigram[1:10]

#VISUALISATION
video2_rank_rd_bigram_top10 <-video2_rank_rd_bigram[1:10]

# Subset the original graph to include only the top 10 nodes
video2_top10_nodes <- names(video2_rank_rd_bigram_top10)
video2_subgraph_top10 <- induced_subgraph(video2_yt_bigrams_graph, vids = video2_top10_nodes)

# Plot the subgraph
plot(video2_subgraph_top10, vertex.label = NA, vertex.size = 4, edge.arrow.size = 0.5)

# Write the subgraph to a file
write_graph(video2_subgraph_top10, file = 'Ranktop10_video2.graphml', format = 'graphml')

#VIDEO 3: STAY

#Convert to be dataframe
video3_yt_clean_df <- data.frame(video3_yt_clean_text)
#BIGRAM TOKENISATION
video3_yt_bigrams <- video3_yt_clean_df |> unnest_tokens(output = bigram,
                                                         input = video3_yt_clean_text,
                                                         token ='ngrams',
                                                         n=2)
View(video3_yt_bigrams)

#Count the number of occurrences, split them across two columns
video3_yt_bigrams_table <- video3_yt_bigrams |>
  count(bigram, sort = TRUE) |>
  separate(bigram, c('left','right'))

View(video3_yt_bigrams_table)

#Remove stopwords
video3_yt_bigrams_nonstops <- video3_yt_bigrams_table |>
  anti_join(stop_words, join_by(left== word)) |>
  anti_join(stop_words,join_by(right == word))
View(video3_yt_bigrams_nonstops)
video3_yt_bigrams_nonstops <- video3_yt_bigrams_nonstops[complete.cases(video3_yt_bigrams_nonstops),]
View(video3_yt_bigrams_nonstops)

#Keep only those that occur at least twice
video3_yt_bigrams_nonstops <- video3_yt_bigrams_nonstops |> filter(n >=2)
View(video3_yt_bigrams_nonstops)

#Semantic Network Graph
video3_yt_bigrams_graph <- graph_from_data_frame(video3_yt_bigrams_nonstops,directed = FALSE)

#Get the number of nodes and edges
vcount(video3_yt_bigrams_graph)
ecount(video3_yt_bigrams_graph)
#Remove any loops or multiple edges between the same two words:
video3_yt_bigrams_graph <- simplify(video3_yt_bigrams_graph)
vcount(video3_yt_bigrams_graph)
ecount(video3_yt_bigrams_graph)

#Page Rank algorithm on our semantic network:
video3_rank_rd_bigram <- sort(page_rank(video3_yt_bigrams_graph)$vector, decreasing = TRUE)
#VISUALISATION
video3_rank_rd_bigram_top10 <-video3_rank_rd_bigram[1:10]

# Subset the original graph to include only the top 10 nodes
video3_top10_nodes <- names(video3_rank_rd_bigram_top10)
video3_subgraph_top10 <- induced_subgraph(video3_yt_bigrams_graph, vids = video3_top10_nodes)

# Plot the subgraph
plot(video3_subgraph_top10, vertex.label = NA, vertex.size = 4, edge.arrow.size = 0.5)

# Write the subgraph to a file
write_graph(video3_subgraph_top10, file = 'Ranktop10_video3.graphml', format = 'graphml')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#QUESION 8: CENTRALITY ANALYSIS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(vosonSML)
library(igraph)
library(dplyr)
library(textclean)
library(tidytext)
library(tidyr)
library(tm)

#VIDEO 1: KILL THIS LOVE
# YOUTUBE actor network 
video1_yt_actor_network <- video1_yt_data |>
  Create('actor') |>
  AddText(video1_yt_data,
          replicateFromText = TRUE,
          verbose = TRUE)

# Create graph from the network and change IDs to screen names
video1_yt_actor_graph <- video1_yt_actor_network |> Graph()
V(video1_yt_actor_graph)$name <- V(video1_yt_actor_graph)$screen_name

# Save and write graph to file
saveRDS(video1_yt_actor_graph, file = 'YouTubeActorVideo1.rds')
write_graph(video1_yt_actor_graph, file = 'YouTubeActorVideo1.graphml', format = 'graphml')

# Load YouTubeActor
video1_yt_network_graph <- readRDS('YouTubeActorVideo1.rds')
length(V(video1_yt_network_graph))
V(video1_yt_network_graph)$name[1:20]

# Centrality Analysis
video1_yt_comps <- components(video1_yt_network_graph, mode = 'weak')
# Find out how many components were found, how many nodes are in each component
video1_yt_comps$no
video1_yt_comps$csize
head(video1_yt_comps$membership, n = 30)

# Find the largest component
video1_yt_largest_comp<- which.max(video1_yt_comps$csize)

# Extract a sub-graph from the original graph
video1_yt_comp_subgraph <- video1_yt_network_graph |>
  induced_subgraph(vids = which(video1_yt_comps$membership == video1_yt_largest_comp))

# Remove NA values from vertex names in the subgraph
video1_yt_comp_subgraph <- delete_vertices(video1_yt_comp_subgraph, which(is.na(V(video1_yt_comp_subgraph)$name)))
#Degree Centrality
sort(degree(video1_yt_comp_subgraph, mode = 'in'), decreasing = TRUE) [1:20]
sort(degree(video1_yt_comp_subgraph, mode = 'out'), decreasing = TRUE) [1:20]
sort(degree(video1_yt_comp_subgraph, mode = 'total'), decreasing = TRUE) [1:20]

#Closeness Centrality
sort(closeness(video1_yt_comp_subgraph, mode = 'in'), decreasing = TRUE) [1:20]
sort(closeness(video1_yt_comp_subgraph, mode = 'out'), decreasing = TRUE) [1:20]
sort(closeness(video1_yt_comp_subgraph, mode = 'total'), decreasing = TRUE) [1:20]

#Betweenness Centrality
sort(betweenness(video1_yt_comp_subgraph, directed = FALSE),decreasing = TRUE)[1:20]

#OPTIONAL: CREATE DATAFRAME
# Get the top 20 nodes based on in-degree centrality
video1_top_in_degree <- sort(degree(video1_yt_comp_subgraph, mode = 'in'), decreasing = TRUE)[1:20]

# Create a data frame for in-degree centrality
video1_in_degree_df <- data.frame(
  node = names(video1_top_in_degree),
  in_degree = as.vector(video1_top_in_degree)
)

# Get the top 20 nodes based on out-degree centrality
video1_top_out_degree <- sort(degree(video1_yt_comp_subgraph, mode = 'out'), decreasing = TRUE)[1:20]

# Create a data frame for out-degree centrality
video1_out_degree_df <- data.frame(
  node = names(video1_top_out_degree),
  out_degree = as.vector(video1_top_out_degree)
)

# Get the top 20 nodes based on total-degree centrality
video1_top_total_degree <- sort(degree(video1_yt_comp_subgraph, mode = 'total'), decreasing = TRUE)[1:20]

# Create a data frame for total-degree centrality
video1_total_degree_df <- data.frame(
  node = names(video1_top_total_degree),
  total_degree = as.vector(video1_top_total_degree)
)

# Print or view the data frames
print(video1_in_degree_df)
print(video1_out_degree_df)
print(video1_total_degree_df)


# Get the top 20 nodes based on in-closeness centrality
video1_top_in_closeness <- sort(closeness(video1_yt_comp_subgraph, mode = 'in'), decreasing = TRUE)[1:20]

# Create a data frame for in-closeness centrality
video1_in_closeness_df <- data.frame(
  node = names(video1_top_in_closeness),
  in_closeness = as.vector(video1_top_in_closeness)
)

# Get the top 20 nodes based on out-closeness centrality
video1_top_out_closeness <- sort(closeness(video1_yt_comp_subgraph, mode = 'out'), decreasing = TRUE)[1:20]

# Create a data frame for out-closeness centrality
video1_out_closeness_df <- data.frame(
  node = names(video1_top_out_closeness),
  out_closeness = as.vector(video1_top_out_closeness)
)

# Get the top 20 nodes based on total-closeness centrality
video1_top_total_closeness <- sort(closeness(video1_yt_comp_subgraph, mode = 'total'), decreasing = TRUE)[1:20]

# Create a data frame for total-closeness centrality
video1_total_closeness_df <- data.frame(
  node = names(video1_top_total_closeness),
  total_closeness = as.vector(video1_top_total_closeness)
)

# Print or view the data frames
print(video1_in_closeness_df)
print(video1_out_closeness_df)
print(video1_total_closeness_df)

head(video1_rank_actor_network_yt,n=20)


#VIDEO 2: PINK VENOM
# YOUTUBE actor network 
video2_yt_actor_network <- video2_yt_data |>
  Create('actor') |>
  AddText(video2_yt_data,
          replicateFromText = TRUE,
          verbose = TRUE)

# Create graph from the network and change IDs to screen names
video2_yt_actor_graph <- video2_yt_actor_network |> Graph()
V(video2_yt_actor_graph)$name <- V(video2_yt_actor_graph)$screen_name

# Save and write graph to file
saveRDS(video2_yt_actor_graph, file = 'YouTubeActorVideo2.rds')
write_graph(video2_yt_actor_graph, file = 'YouTubeActorVideo2.graphml', format = 'graphml')

# Load YouTubeActor
video2_yt_network_graph <- readRDS('YouTubeActorVideo2.rds')
length(V(video2_yt_network_graph))
V(video2_yt_network_graph)$name[1:20]

# Centrality Analysis
video2_yt_comps <- components(video2_yt_network_graph, mode = 'weak')
# Find out how many components were found, how many nodes are in each component
video2_yt_comps$no
video2_yt_comps$csize
head(video2_yt_comps$membership, n = 30)

# Find the largest component
video2_yt_largest_comp<- which.max(video2_yt_comps$csize)

# Extract a sub-graph from the original graph
video2_yt_comp_subgraph <- video2_yt_network_graph |>
  induced_subgraph(vids = which(video2_yt_comps$membership == video2_yt_largest_comp))

# Remove NA values from vertex names in the subgraph
video2_yt_comp_subgraph <- delete_vertices(video2_yt_comp_subgraph, which(is.na(V(video2_yt_comp_subgraph)$name)))
#Degree Centrality
sort(degree(video2_yt_comp_subgraph, mode = 'in'), decreasing = TRUE) [1:20]
sort(degree(video2_yt_comp_subgraph, mode = 'out'), decreasing = TRUE) [1:20]
sort(degree(video2_yt_comp_subgraph, mode = 'total'), decreasing = TRUE) [1:20]

#Closeness Centrality
sort(closeness(video2_yt_comp_subgraph, mode = 'in'), decreasing = TRUE) [1:20]
sort(closeness(video2_yt_comp_subgraph, mode = 'out'), decreasing = TRUE) [1:20]
sort(closeness(video2_yt_comp_subgraph, mode = 'total'), decreasing = TRUE) [1:20]

#Betweenness Centrality
sort(betweenness(video2_yt_comp_subgraph, directed = FALSE),decreasing = TRUE)[1:20]

#OPTIONAL: CREATE DATAFRAME
# Get the top 20 nodes based on in-degree centrality
video2_top_in_degree <- sort(degree(video2_yt_comp_subgraph, mode = 'in'), decreasing = TRUE)[1:20]

# Create a data frame for in-degree centrality
video2_in_degree_df <- data.frame(
  node = names(video2_top_in_degree),
  in_degree = as.vector(video2_top_in_degree)
)

# Get the top 20 nodes based on out-degree centrality
video2_top_out_degree <- sort(degree(video2_yt_comp_subgraph, mode = 'out'), decreasing = TRUE)[1:20]

# Create a data frame for out-degree centrality
video2_out_degree_df <- data.frame(
  node = names(video2_top_out_degree),
  out_degree = as.vector(video2_top_out_degree)
)

# Get the top 20 nodes based on total-degree centrality
video2_top_total_degree <- sort(degree(video2_yt_comp_subgraph, mode = 'total'), decreasing = TRUE)[1:20]

# Create a data frame for total-degree centrality
video2_total_degree_df <- data.frame(
  node = names(video2_top_total_degree),
  total_degree = as.vector(video2_top_total_degree)
)

# Print or view the data frames
print(video2_in_degree_df)
print(video2_out_degree_df)
print(video2_total_degree_df)


# Get the top 20 nodes based on in-closeness centrality
video2_top_in_closeness <- sort(closeness(video2_yt_comp_subgraph, mode = 'in'), decreasing = TRUE)[1:20]

# Create a data frame for in-closeness centrality
video2_in_closeness_df <- data.frame(
  node = names(video2_top_in_closeness),
  in_closeness = as.vector(video2_top_in_closeness)
)

# Get the top 20 nodes based on out-closeness centrality
video2_top_out_closeness <- sort(closeness(video2_yt_comp_subgraph, mode = 'out'), decreasing = TRUE)[1:20]

# Create a data frame for out-closeness centrality
video2_out_closeness_df <- data.frame(
  node = names(video2_top_out_closeness),
  out_closeness = as.vector(video2_top_out_closeness)
)

# Get the top 20 nodes based on total-closeness centrality
video2_top_total_closeness <- sort(closeness(video2_yt_comp_subgraph, mode = 'total'), decreasing = TRUE)[1:20]

# Create a data frame for total-closeness centrality
video2_total_closeness_df <- data.frame(
  node = names(video2_top_total_closeness),
  total_closeness = as.vector(video2_top_total_closeness)
)

# Print or view the data frames
print(video2_in_closeness_df)
print(video2_out_closeness_df)
print(video2_total_closeness_df)

head(video2_rank_actor_network_yt,n=20)


#VIDEO 3: STAY
# YOUTUBE actor network 
video3_yt_actor_network <- video3_yt_data |>
  Create('actor') |>
  AddText(video3_yt_data,
          replicateFromText = TRUE,
          verbose = TRUE)

# Create graph from the network and change IDs to screen names
video3_yt_actor_graph <- video3_yt_actor_network |> Graph()
V(video3_yt_actor_graph)$name <- V(video3_yt_actor_graph)$screen_name

# Save and write graph to file
saveRDS(video3_yt_actor_graph, file = 'YouTubeActorVideo3.rds')
write_graph(video3_yt_actor_graph, file = 'YouTubeActorVideo3.graphml', format = 'graphml')

# Load YouTubeActor
video3_yt_network_graph <- readRDS('YouTubeActorVideo3.rds')
length(V(video3_yt_network_graph))
V(video3_yt_network_graph)$name[1:20]

# Centrality Analysis
video3_yt_comps <- components(video3_yt_network_graph, mode = 'weak')
# Find out how many components were found, how many nodes are in each component
video3_yt_comps$no
video3_yt_comps$csize
head(video3_yt_comps$membership, n = 30)

# Find the largest component
video3_yt_largest_comp<- which.max(video3_yt_comps$csize)

# Extract a sub-graph from the original graph
video3_yt_comp_subgraph <- video3_yt_network_graph |>
  induced_subgraph(vids = which(video3_yt_comps$membership == video3_yt_largest_comp))

# Remove NA values from vertex names in the subgraph
video3_yt_comp_subgraph <- delete_vertices(video3_yt_comp_subgraph, which(is.na(V(video3_yt_comp_subgraph)$name)))
#Degree Centrality
sort(degree(video3_yt_comp_subgraph, mode = 'in'), decreasing = TRUE) [1:20]
sort(degree(video3_yt_comp_subgraph, mode = 'out'), decreasing = TRUE) [1:20]
sort(degree(video3_yt_comp_subgraph, mode = 'total'), decreasing = TRUE) [1:20]

#Closeness Centrality
sort(closeness(video3_yt_comp_subgraph, mode = 'in'), decreasing = TRUE) [1:20]
sort(closeness(video3_yt_comp_subgraph, mode = 'out'), decreasing = TRUE) [1:20]
sort(closeness(video3_yt_comp_subgraph, mode = 'total'), decreasing = TRUE) [1:20]

#Betweenness Centrality
sort(betweenness(video3_yt_comp_subgraph, directed = FALSE),decreasing = TRUE)[1:20]

#OPTIONAL: CREATE DATAFRAME
# Get the top 20 nodes based on in-degree centrality
video3_top_in_degree <- sort(degree(video3_yt_comp_subgraph, mode = 'in'), decreasing = TRUE)[1:20]

# Create a data frame for in-degree centrality
video3_in_degree_df <- data.frame(
  node = names(video3_top_in_degree),
  in_degree = as.vector(video3_top_in_degree)
)

# Get the top 20 nodes based on out-degree centrality
video3_top_out_degree <- sort(degree(video3_yt_comp_subgraph, mode = 'out'), decreasing = TRUE)[1:20]

# Create a data frame for out-degree centrality
video3_out_degree_df <- data.frame(
  node = names(video3_top_out_degree),
  out_degree = as.vector(video3_top_out_degree)
)

# Get the top 20 nodes based on total-degree centrality
video3_top_total_degree <- sort(degree(video3_yt_comp_subgraph, mode = 'total'), decreasing = TRUE)[1:20]

# Create a data frame for total-degree centrality
video3_total_degree_df <- data.frame(
  node = names(video3_top_total_degree),
  total_degree = as.vector(video3_top_total_degree)
)

# Print or view the data frames
print(video3_in_degree_df)
print(video3_out_degree_df)
print(video3_total_degree_df)


# Get the top 20 nodes based on in-closeness centrality
video3_top_in_closeness <- sort(closeness(video3_yt_comp_subgraph, mode = 'in'), decreasing = TRUE)[1:20]

# Create a data frame for in-closeness centrality
video3_in_closeness_df <- data.frame(
  node = names(video3_top_in_closeness),
  in_closeness = as.vector(video3_top_in_closeness)
)

# Get the top 20 nodes based on out-closeness centrality
video3_top_out_closeness <- sort(closeness(video3_yt_comp_subgraph, mode = 'out'), decreasing = TRUE)[1:20]

# Create a data frame for out-closeness centrality
video3_out_closeness_df <- data.frame(
  node = names(video3_top_out_closeness),
  out_closeness = as.vector(video3_top_out_closeness)
)

# Get the top 20 nodes based on total-closeness centrality
video3_top_total_closeness <- sort(closeness(video3_yt_comp_subgraph, mode = 'total'), decreasing = TRUE)[1:20]

# Create a data frame for total-closeness centrality
video3_total_closeness_df <- data.frame(
  node = names(video3_top_total_closeness),
  total_closeness = as.vector(video3_top_total_closeness)
)

# Print or view the data frames
print(video3_in_closeness_df)
print(video3_out_closeness_df)
print(video3_total_closeness_df)

head(video3_rank_actor_network_yt,n=20)

#Compare these scores to the scores for other artists that are related to BlackPink: Rose

#Rose's video 
rose_url <- c('https://www.youtube.com/watch?v=CKZvWhCqx1s')

rose_yt_data <- yt_auth |> Collect(videoIDs = rose_url,
                                     maxComments = 5000,
                                     writeToFile = TRUE,
                                     verbose = TRUE)
# YOUTUBE actor network 
rose_yt_actor_network <- rose_yt_data |>
  Create('actor') |>
  AddText(rose_yt_data,
          replicateFromText = TRUE,
          verbose = TRUE)

# Create graph from the network and change IDs to screen names
rose_yt_actor_graph <- rose_yt_actor_network |> Graph()
V(rose_yt_actor_graph)$name <- V(rose_yt_actor_graph)$screen_name

# Save and write graph to file
saveRDS(rose_yt_actor_graph, file = 'YouTubeActorVideoRose.rds')
write_graph(rose_yt_actor_graph, file = 'YouTubeActorVideoRose.graphml', format = 'graphml')

# Load YouTubeActor
rose_yt_network_graph <- readRDS('YouTubeActorVideoRose.rds')
length(V(rose_yt_network_graph))
V(rose_yt_network_graph)$name[1:20]

# Centrality Analysis
rose_yt_comps <- components(rose_yt_network_graph, mode = 'weak')
# Find out how many components were found, how many nodes are in each component
rose_yt_comps$no
rose_yt_comps$csize
head(rose_yt_comps$membership, n = 30)

# Find the largest component
rose_yt_largest_comp<- which.max(rose_yt_comps$csize)

# Extract a sub-graph from the original graph
rose_yt_comp_subgraph <- rose_yt_network_graph |>
  induced_subgraph(vids = which(rose_yt_comps$membership == rose_yt_largest_comp))

# Remove NA values from vertex names in the subgraph
rose_yt_comp_subgraph <- delete_vertices(rose_yt_comp_subgraph, which(is.na(V(rose_yt_comp_subgraph)$name)))
#Degree Centrality
sort(degree(rose_yt_comp_subgraph, mode = 'in'), decreasing = TRUE) [1:20]
sort(degree(rose_yt_comp_subgraph, mode = 'out'), decreasing = TRUE) [1:20]
sort(degree(rose_yt_comp_subgraph, mode = 'total'), decreasing = TRUE) [1:20]

#Closeness Centrality
sort(closeness(rose_yt_comp_subgraph, mode = 'in'), decreasing = TRUE) [1:20]
sort(closeness(rose_yt_comp_subgraph, mode = 'out'), decreasing = TRUE) [1:20]
sort(closeness(rose_yt_comp_subgraph, mode = 'total'), decreasing = TRUE) [1:20]

#Betweenness Centrality
sort(betweenness(rose_yt_comp_subgraph, directed = FALSE),decreasing = TRUE)[1:20]

#OPTIONAL: CREATE DATAFRAME
# Get the top 20 nodes based on in-degree centrality
rose_top_in_degree <- sort(degree(rose_yt_comp_subgraph, mode = 'in'), decreasing = TRUE)[1:20]

# Create a data frame for in-degree centrality
rose_in_degree_df <- data.frame(
  node = names(rose_top_in_degree),
  in_degree = as.vector(rose_top_in_degree)
)

# Get the top 20 nodes based on out-degree centrality
rose_top_out_degree <- sort(degree(rose_yt_comp_subgraph, mode = 'out'), decreasing = TRUE)[1:20]

# Create a data frame for out-degree centrality
rose_out_degree_df <- data.frame(
  node = names(rose_top_out_degree),
  out_degree = as.vector(rose_top_out_degree)
)

# Get the top 20 nodes based on total-degree centrality
rose_top_total_degree <- sort(degree(rose_yt_comp_subgraph, mode = 'total'), decreasing = TRUE)[1:20]

# Create a data frame for total-degree centrality
rose_total_degree_df <- data.frame(
  node = names(rose_top_total_degree),
  total_degree = as.vector(rose_top_total_degree)
)

# Print or view the data frames
print(rose_in_degree_df)
print(rose_out_degree_df)
print(rose_total_degree_df)


# Get the top 20 nodes based on in-closeness centrality
rose_top_in_closeness <- sort(closeness(rose_yt_comp_subgraph, mode = 'in'), decreasing = TRUE)[1:20]

# Create a data frame for in-closeness centrality
rose_in_closeness_df <- data.frame(
  node = names(rose_top_in_closeness),
  in_closeness = as.vector(rose_top_in_closeness)
)

# Get the top 20 nodes based on out-closeness centrality
rose_top_out_closeness <- sort(closeness(rose_yt_comp_subgraph, mode = 'out'), decreasing = TRUE)[1:20]

# Create a data frame for out-closeness centrality
rose_out_closeness_df <- data.frame(
  node = names(rose_top_out_closeness),
  out_closeness = as.vector(rose_top_out_closeness)
)

# Get the top 20 nodes based on total-closeness centrality
rose_top_total_closeness <- sort(closeness(rose_yt_comp_subgraph, mode = 'total'), decreasing = TRUE)[1:20]

# Create a data frame for total-closeness centrality
rose_total_closeness_df <- data.frame(
  node = names(rose_top_total_closeness),
  total_closeness = as.vector(rose_top_total_closeness)
)

# Print or view the data frames
print(rose_in_closeness_df)
print(rose_out_closeness_df)
print(rose_total_closeness_df)

rose_actor_network_yt <- rose_yt_data |> Create('actor')
rose_actor_network_graph_yt <- rose_actor_network_yt |> Graph()
plot(rose_actor_network_graph_yt, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)
write_graph(rose_actor_network_graph_yt, file = 'YoutubeActorRoseVideo.graphml', format = 'graphml')
rose1_rank_actor_network_yt <- sort(page_rank(rose_actor_network_graph_yt)$vector, decreasing = TRUE)
V(rose_actor_network_graph_yt)$name <- V(rose_actor_network_graph_yt)$screen_name
rose_rank_actor_network_yt <- sort(page_rank(rose_actor_network_graph_yt)$vector, decreasing = TRUE)
head(rose_rank_actor_network_yt, n = 5)
rose_rank_actor_network_yt <- rose_rank_actor_network_yt[!is.na(names(rose_rank_actor_network_yt))]

head(rose_rank_actor_network_yt,n=20)

View(rose_yt_data)
View(video1_yt_data)
View(video2_yt_data)
View(video3_yt_data)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`QUESTION 9:COMMUNITY ANALYSIS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(vosonSML)
library(igraph)

#VIDEO1: "KILL THIS LOVE"

# Load Video 1 network graph
video1_yt_network_graph <- readRDS('YouTubeActorVideo1.rds')
# Transform into an undirected graph
video1_undir_yt_network_graph <- as.undirected(video1_yt_network_graph, mode = "collapse")

# Run Louvain algorithm
video1_yt_louvain_comm <- cluster_louvain(video1_undir_yt_network_graph)
# See sizes of communities
sizes(video1_yt_louvain_comm)

# Run Girvan-Newman (edge-betweenness) algorithm
video1_yt_eb_comm <- cluster_edge_betweenness(video1_undir_yt_network_graph)
# See sizes of communities
sizes(video1_yt_eb_comm)

#VIDEO 2: "PINK VENOM"
# Load Video 2 network graph
video2_yt_network_graph <- readRDS('YouTubeActorVideo2.rds')
# Transform into an undirected graph
video2_undir_yt_network_graph <- as.undirected(video2_yt_network_graph, mode = "collapse")

# Run Louvain algorithm
video2_yt_louvain_comm <- cluster_louvain(video2_undir_yt_network_graph)
# See sizes of communities
sizes(video2_yt_louvain_comm)

# Run Girvan-Newman (edge-betweenness) algorithm
video2_yt_eb_comm <- cluster_edge_betweenness(video2_undir_yt_network_graph)
# See sizes of communities
sizes(video2_yt_eb_comm)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Question 10:Sentiment & Emotion Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
#VIDEO1: "KILL THIS LOVE"
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(syuzhet)
library(ggplot2)
#Clean the text
video1_clean_text <- video1_yt_data$Comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> 
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation()

#Sentiment score
video1_sentiment_scores <- get_sentiment(video1_clean_text, method = "afinn") |> sign()
video1_sentiment_df <- data.frame(text = video1_clean_text, sentiment = video1_sentiment_scores)
View(video1_sentiment_df)
#Convert each score to the name
video1_sentiment_df$sentiment <- factor(video1_sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(video1_sentiment_df)

#Visualisation
ggplot(video1_sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Comments") +
  ggtitle("Sentiment Analysis of Comments on 'Kill This Love' video")

#Assign emotion scores to comments
video1_emo_scores <- get_nrc_sentiment(video1_clean_text)[ , 1:8]
#Create Emotion Data Frame:
video1_emo_scores_df <- data.frame(video1_clean_text, video1_emo_scores)
View(video1_emo_scores_df)
video1_emo_sums <- video1_emo_scores_df[,2:9] |> 
  sign() |> 
  colSums() |> 
  sort(decreasing = TRUE) |> 
  data.frame() / nrow(video1_emo_scores_df) 

#Calculate Proportion of Each Emotion:
names(video1_emo_sums)[1] <- "Proportion" 
View(video1_emo_sums)

#Visualisation
ggplot(video1_emo_sums, aes(x = reorder(rownames(video1_emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(video1_emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Pastel2") +
  labs(x = "Emotion Categories", y = "Proportion of Comments") +
  ggtitle("Emotion Analysis of Comments on 'Kill This Love' video")

#VIDEO 3: "STAY VIDEO"
#Clean the text
video3_clean_text <- video3_yt_data$Comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> 
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation()

#Sentiment score
video3_sentiment_scores <- get_sentiment(video3_clean_text, method = "afinn") |> sign()
video3_sentiment_df <- data.frame(text = video3_clean_text, sentiment = video3_sentiment_scores)
View(video3_sentiment_df)
#Convert each score to the name
video3_sentiment_df$sentiment <- factor(video3_sentiment_df$sentiment, levels = c(1, 0, -1),
                                        labels = c("Positive", "Neutral", "Negative")) 
View(video3_sentiment_df)

ggplot(video3_sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Comments") +
  ggtitle("Sentiment Analysis of Comments on 'Stay' video")

#Assign emotion scores to comments
video3_emo_scores <- get_nrc_sentiment(video3_clean_text)[ , 1:8]
#Create Emotion Data Frame:
video3_emo_scores_df <- data.frame(video3_clean_text, video3_emo_scores)
View(video3_emo_scores_df)
video3_emo_sums <- video3_emo_scores_df[,2:9] |> 
  sign() |> 
  colSums() |> 
  sort(decreasing = TRUE) |> 
  data.frame() / nrow(video3_emo_scores_df) 

#Calculate Proportion of Each Emotion:
names(video3_emo_sums)[1] <- "Proportion" 

#Visualisation
ggplot(video3_emo_sums, aes(x = reorder(rownames(video3_emo_sums), Proportion),
                            y = Proportion,
                            fill = rownames(video3_emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Pastel2") +
  labs(x = "Emotion Categories", y = "Proportion of Comments") +
  ggtitle("Emotion Analysis of Comments on 'Stay' video")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`Question 11: DECISION TREE - SPOTIFY~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`

library(spotifyr)
library(C50)
library(caret)
library(e1071)
library(dplyr)
library(partykit)

# Collecting Data for BlackPink
blackpink_features <- get_artist_audio_features("BlackPink")
View(blackpink_features)
data.frame(colnames(blackpink_features))
blackpink_features_subset <- blackpink_features[ , 9:20]
View(blackpink_features_subset)

# Search for Playlists with BlackPink
search_results <- search_spotify("Blackpink", type = "playlist", limit = 50)
# Extract playlist IDs
playlist_ids <- search_results$id
playlist_names <- search_results$name
# Combine into a data frame
playlists <- data.frame(playlist_name = playlist_names, playlist_id = playlist_ids)
# View the playlists
View(playlists)


# Get playlist audio features for the top 50 playlist
top50_features <- get_playlist_audio_features("spotify", "2SxFbSuvIuTSmKvyTDP2iU")
# View the data structure
View(top50_features)
# Select relevant columns and rename 'track.id' to 'track_id'
top50_features_subset <- top50_features[, 6:17]
top50_features_subset <- top50_features_subset |> rename(track_id = track.id)
# Add binary indicator column
top50_features_subset["isBlackPink"] <- 0
blackpink_features_subset["isBlackPink"] <- 1
# Perform anti-join to exclude Blackpink tracks from the top 50 features
top50_features_noblackpink <- anti_join(top50_features_subset, blackpink_features_subset, by = "track_id")
# Check column names
colnames(top50_features_noblackpink)
colnames(blackpink_features_subset)
# Ensure both data frames have the same column names
common_columns <- intersect(colnames(top50_features_noblackpink), colnames(blackpink_features_subset))
top50_features_noblackpink <- top50_features_noblackpink[, common_columns]
blackpink_features_subset <- blackpink_features_subset[, common_columns]
# Combine the data frames
comb_data <- rbind(top50_features_noblackpink, blackpink_features_subset)
# Convert `isBlackPink` to a factor
comb_data$isBlackPink <- factor(comb_data$isBlackPink)
# Optionally drop the `track_id` column
comb_data <- select(comb_data, -track_id)
# View the combined data
View(comb_data)


#Training Data Preparation:
comb_data <- comb_data[sample(1:nrow(comb_data)), ]
split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]
dt_model <- train(isBlackPink~ ., data = training_set, method = "C5.0")

#Testing the model
prediction_row <- 1 # MUST be smaller than or equal to testing set size
predicted_label <- predict(dt_model, testing_set[prediction_row, ]) # predict the label for this row
predicted_label <- as.numeric(levels(predicted_label))[predicted_label] # transform factor into numeric value

if (predicted_label == testing_set[prediction_row, 12]){
  print(paste0("Prediction is: ", predicted_label, ". Correct!"))
} else {
  paste0("Prediction is: ", predicted_label, ". Wrong.")
}
confusionMatrix(dt_model, reference = testing_set$isBlackPink)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Question 12: LDA TOPIC MODELLING~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``


#VIDEO 1: "KILL THIS LOVE"

library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(topicmodels)
library(reshape2)
library(dplyr)
library(ggplot2)
#Data Preprocessing:
video1_clean_text <- video1_yt_data$Comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> 
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation()
#Convert to corpus
video1_text_corpus <- VCorpus(VectorSource(video1_clean_text))
video1_text_corpus[[1]]$content
video1_text_corpus[[5]]$content
# Text Processing:
video1_text_corpus <- video1_text_corpus |>
  tm_map(content_transformer(tolower)) |> 
  tm_map(removeWords, stopwords(kind = "SMART")) |> 
  # tm_map(stemDocument) |> # optional
  tm_map(stripWhitespace)

video1_text_corpus[[1]]$content
video1_text_corpus[[5]]$content

# Document-Term Matrix (DTM) Creation:
video1_doc_term_matrix <- DocumentTermMatrix(video1_text_corpus)
video1_non_zero_entries = unique(video1_doc_term_matrix$i)
video1_dtm = video1_doc_term_matrix[video1_non_zero_entries,]

save(video1_dtm, file = "doc_term_matrix_video1.RData")
load("doc_term_matrix_video1.RData")

library(topicmodels)
library(tidytext)
install.packages("broom")
install.packages("broom.mixed")
library(broom)
library(broom.mixed)
library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(topicmodels)
library(reshape2)
library(dplyr)
library(ggplot2)


#Perform LDA topic modelling

video1_lda_model <- LDA(video1_dtm, k = 6)

video1_found_topics <- tidy(video1_lda_model, matrix = "beta")
View(video1_found_topics)
library(dplyr)
library(ggplot2)
library(tidytext)
#Visualisation
video1_top_terms <- video1_found_topics |>
  group_by(topic) |>
  slice_max(beta, n = 10) |> 
  ungroup() |>
  arrange(topic, -beta)

video1_top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()



#VIDEO3: "STAY"
#Topic Modelling

library(tidyr)
library(tidytext)
library(textclean)
library(tm)
library(topicmodels)
library(reshape2)
library(dplyr)
library(ggplot2)
#Data Preprocessing:
video3_clean_text <- video3_yt_data$Comment |> 
  replace_url() |> 
  replace_html() |>
  replace_non_ascii() |> 
  replace_word_elongation() |>
  replace_internet_slang() |>
  replace_contraction() |>
  removeNumbers() |> 
  removePunctuation()
#Convert to corpus
video3_text_corpus <- VCorpus(VectorSource(video3_clean_text))
video3_text_corpus[[1]]$content
video3_text_corpus[[5]]$content
# Text Processing:
video3_text_corpus <- video3_text_corpus |>
  tm_map(content_transformer(tolower)) |> 
  tm_map(removeWords, stopwords(kind = "SMART")) |> 
  # tm_map(stemDocument) |> # optional
  tm_map(stripWhitespace)

video3_text_corpus[[1]]$content
video3_text_corpus[[5]]$content

# Document-Term Matrix (DTM) Creation:
video3_doc_term_matrix <- DocumentTermMatrix(video3_text_corpus)
video3_non_zero_entries = unique(video3_doc_term_matrix$i)
video3_dtm = video3_doc_term_matrix[video3_non_zero_entries,]

save(video3_dtm, file = "doc_term_matrix_video3.RData")
load("doc_term_matrix_video3.RData")

install.packages("topicmodels")
install.packages("tidytext")
library(topicmodels)
library(tidytext)

#Perform LDA topic modelling

video3_lda_model <- LDA(video3_dtm, k = 6)

video3_found_topics <- tidy(video3_lda_model, matrix = "beta")
View(video3_found_topics)
library(dplyr)
library(ggplot2)
library(tidytext)
#Visualisation
video3_top_terms <- video3_found_topics |>
  group_by(topic) |>
  slice_max(beta, n = 10) |> 
  ungroup() |>
  arrange(topic, -beta)

video3_top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

library(tidyverse)
library(ggridges)
library(tidytext)
library(mosaic)
library(syuzhet)
library(viridis)
library(wordcloud2)
library(lubridate)

chart2 = subset(rym_chart2, select = -c(1))

#Condense primary_genres to just the first genre in each string (before the first comma)
chart2$primary_genres <- ifelse(substr(chart2$primary_genres, 1, regexpr(",",chart2$primary_genres)-1) == "", #test
       chart2$primary_genres, #value if true
       substr(chart2$primary_genres, 1, regexpr(",",chart2$primary_genres)-1)) #value if false

#extract table of the most popular words in the primary_genres column
#(this will help us categorize primary_genres into the standardized rym_genre bins)
genreText = paste(unlist(chart2$primary_genres), collapse =" ")
text_df = tibble(genreText)
genre_words <- text_df %>% unnest_tokens(output = word, input = genreText)
wordcounts = genre_words %>% count(word, sort = TRUE)

summary(wordcounts)

#Decide how to split up genres using keywords (domain knowledge required)
#Psychedelia: "Neo-Psychedelia"
#Electronic: "Electronic|Techno|Glitch|House|Indietronica|IDM"
#Folk: "Folk"
#Singer-Songwriter: "Singer-Songwriter"
#Jazz: "Jazz"
#Hip Hop: "Hip Hop|Trap|Rap"
#R&B: "R&B|Neo-Soul|Soul|Funk"
#Pop: "Pop|Synthpop|Britpop|Electropop"
#Rock: "Rock|Metal|Punk|Emo|Hardcore|Garage|Shoegaze|Grunge|Slowcore"

#Create new genre variable
chart2$rym_genre = ""
chart2$rym_genre[grep("Neo-Psychedelia", chart2$primary_genres)] <- "Psychedelia"
chart2$rym_genre[grep("Electronic|Techno|Glitch|House|Indietronica|IDM", chart2$primary_genres)] <- "Electronic"
chart2$rym_genre[grep("Folk", chart2$primary_genres)] <- "Folk"
chart2$rym_genre[grep("Singer-Songwriter", chart2$primary_genres)] <- "Singer-Songwriter"
chart2$rym_genre[grep("Jazz", chart2$primary_genres)] <- "Jazz"
chart2$rym_genre[grep("Hip Hop|Trap|Rap", chart2$primary_genres)] <- "Hip Hop"
chart2$rym_genre[grep("R&B|Neo-Soul|Soul|Funk", chart2$primary_genres)] <- "R&B"
chart2$rym_genre[grep("Pop|Synthpop|Britpop|Electropop", chart2$primary_genres)] <- "Pop"
chart2$rym_genre[grep("Rock|Metal|Punk|Emo|Hardcore|Garage|Shoegaze|Grunge|Slowcore", chart2$primary_genres)] <- "Rock"

other <- !(chart2$rym_genre %in% c("Hip Hop", "Rock", "Pop", "R&B", "Jazz", "Psychedelia", "Electronic", "Folk", "Singer-Songwriter"))
chart2$rym_genre[other] <- "Other"

#Summary for each genre
genrestats = favstats(avg_rating~rym_genre, data = chart2); genrestats

#Visualizations
#-color palette generator: https://waldyrious.net/viridis-palette-generator/
#--Bar chart, average album ratings per genre
aggdf = aggregate(rating_count ~ rym_genre, data = chart2, mean)

g = ggplot(aggdf, aes(reorder(rym_genre, rating_count), rating_count)) + 
  geom_col(aes(fill = rating_count)) + 
  scale_fill_gradient2(low = "purple", 
                       high = "red", 
                       midpoint = median(chart2$rating_count),
                       name = "Rating Count") + 
  coord_flip() + 
  labs(y = "Average Ratings per Album", x = "Genre")

ggplotly(g)

#--Histogram, average ratings
g = ggplot(chart2, aes(avg_rating, fill=rym_genre)) + 
  geom_histogram(bins = 100) + 
  scale_fill_viridis_d(option = "D", name = "Genres") +
  labs(title = "Distribution of Albums' Average Ratings", x="Average Rating", y="Count")

ggplotly(g)

#--Histogram, release_Date
year(max(chart2$release_date)) - year(min(chart2$release_date)) #number of bins, 1 bin for each year

g = ggplot(chart2, aes(as.numeric(year(release_date)), fill=rym_genre)) + 
  geom_histogram(bins = 68) +
  labs(x="Year", fill="Genre")

ggplotly(g)

#--Density plots
ggplot(chart2, aes(x = avg_rating, y = rym_genre)) +
  geom_density_ridges(show.legend = FALSE, 
                      scale = 2, 
                      rel_min_height = 0.01, 
                      aes(fill=rym_genre, alpha = .75)) +
  scale_fill_manual(values = c("#000004", "#180f3d", "#440f76", "#721f81", "#9e2f7f", "#cd4071", "#f1605d", "#fd9668", "#feca8d", "#fcfdbf")) +
  labs(title = 'Average Ratings Across the Major Genres', x="Average Rating", y = "Genres") +
  theme_ridges(grid = FALSE)


#--Create WordCloud for descriptors/tags
#---extract table of the descriptor words with their frequency
descText <- paste(unlist(chart2$descriptors), collapse =" ") #descText is descriptors column condensed into 1 row 
descText <- gsub(",", "", descText)
descText_df <- tibble(descText) #tibble of the descText
desc_words <- descText_df %>% unnest_tokens(output = word, input = descText)
desc_counts <- desc_words %>% count(word, sort = TRUE)

wordcloud2(data = desc_counts)






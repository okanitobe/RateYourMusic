#March 11, 2022
#Scraping RateYourMusic.com's custom chart page, filtered by popularity for Albums

#-------------------------Part 1: Load Libraries--------------------------------
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(zoo)


#-------Part 2: Testing, variable definition, selecting CSS elements------------

link = "https://rateyourmusic.com/charts/popular/album/all-time/exc:live,archival/"
page = read_html(link)
page

#CSS elements were found using the SelectorGadget extension in Google Chrome

position = page %>% html_nodes(".topcharts_position") %>% html_text()
release_name = page %>% html_nodes(".release") %>% html_text(); release_name
artist_name = page %>% html_nodes(".topcharts_item_artist") %>% html_text(); artist_name
release_date = page %>% html_nodes(".topcharts_item_releasedate") %>% html_text(); release_date
release_type = page %>% html_nodes(".chart_release_type") %>% html_text()
primary_genres = page %>% html_nodes(".topcharts_item_genres_container") %>% html_text()
secondary_genres = page %>% html_nodes(".topcharts_item_secondarygenres_container") %>% html_text()
descriptors = page %>% html_nodes(".topcharts_item_descriptors_container") %>% html_text()
avg_rating = page %>% html_nodes(".topcharts_avg_rating_stat") %>% html_text()
avg_rating = as.numeric(avg_rating)
avg_rating
rating_count = page %>% html_nodes(".topcharts_ratings_stat") %>% html_text()
rating_count = as.numeric(gsub("," , "" , rating_count))
rating_count #use gsub() to remove the commas from the numbers so you can convert it to numeric
review_count = page %>% html_nodes(".topcharts_reviews_stat") %>% html_text()
review_count = as.numeric(review_count)

#------------------------Part 3: Scrape Chart Pages---------------------------

chart = data.frame()

for (page_num in seq(from = 1, to = 125, by = 1)) {
  link = paste("https://rateyourmusic.com/charts/popular/album/all-time/exc:live,archival/",page_num, "/#results", sep = "")
  page = read_html(link)
  
  position = page %>% html_nodes(".topcharts_position") %>% html_text()
  release_name = page %>% html_nodes(".release") %>% html_text()
  artist_name = page %>% html_nodes(".topcharts_item_artist") %>% html_text()
  release_date = page %>% html_nodes(".topcharts_item_releasedate") %>% html_text()
  release_type = "album"
  primary_genres = page %>% html_nodes(".topcharts_item_genres_container") %>% html_text()
  secondary_genres = page %>% html_nodes(".topcharts_item_secondarygenres_container") %>% html_text()
  descriptors = page %>% html_nodes(".topcharts_item_descriptors_container") %>% html_text()
  avg_rating = page %>% html_nodes(".topcharts_avg_rating_stat") %>% html_text()
  rating_count = page %>% html_nodes(".topcharts_ratings_stat") %>% html_text()
  review_count = page %>% html_nodes(".topcharts_reviews_stat") %>% html_text()
  
  
  chart = rbind(chart, data.frame(position, release_name, artist_name, release_date, release_type, primary_genres, secondary_genres, descriptors, avg_rating, rating_count, review_count, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_num))
}

#Save as CSV
write.csv(chart, "rym_chart1.csv")

#----------------------------Part 4: Scrape Genre Page--------------------------

rym_genre = data.frame()

link2 = "https://rateyourmusic.com/genres/"
page2 = read_html(link2)
  
genre_name = page2 %>% html_nodes("h2 a") %>% html_text()
genre_description = page2 %>% html_nodes(".page_genre_index_hierarchy_item_expanded") %>% html_text()
rym_genre = rbind(rym_genre, data.frame(genre_name, genre_description, stringsAsFactors = FALSE))

write.csv(rym_genre, "rym_genre.csv")

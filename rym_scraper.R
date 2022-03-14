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

#------------------------Part 3: Scrape all the Pages---------------------------

chart = data.frame()

for (page_num in seq(from = 117, to = 125, by = 1)) {
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


#-------------------------Part 4: Cleaning the Data-----------------------------

chart2 = chart #duplicated data frame, for trial and error purposes :)

#Change position, avg_rating, rating_count, and review_count to numeric format
chart2$position = as.numeric(gsub("\\.","",chart2$position)); chart2$position

chart2$avg_rating = as.numeric(chart2$avg_rating)

chart2$rating_count = as.numeric(gsub("," , "" , chart2$rating_count))

chart2$review_count = str_replace_all(chart2$review_count, "-", "0"); chart2$review_count #replace dashes "-" with characterized "0" so R doesn't autofill NA's in the next step
chart2$review_count = as.numeric(chart2$review_count); chart2$review_count

#Clean primary_genres: remove whitespaces, separate genre labels, etc.

chart2$primary_genres = chart$primary_genres #to reset, if necessary
chart2$primary_genres = str_replace_all(chart2$primary_genres, "\n", ""); chart2$primary_genres #remove the "\n"'s
chart2$primary_genres = str_replace_all(chart2$primary_genres, " ", ""); chart2$primary_genres #remove whitespaces
chart2$primary_genres = gsub("([a-z])([A-Z])", "\\1 \\2", chart2$primary_genres); chart2$primary_genres #add spaces back between the condensed names
chart2$primary_genres = gsub(",([A-Za-z])", ", \\1", chart2$primary_genres); chart2$primary_genres #add spaces after the commas
chart2$primary_genres = gsub("([a-z])&([A-Z])", "\\1 & \\2", chart2$primary_genres); chart2$primary_genres #add space between genre labels separated by "&" (except for R&B)
View(chart2)

#Clean secondary_genres: remove whitespaces, separate genre labels, etc.

chart2$secondary_genres = chart$secondary_genres #to reset, if necessary
chart2$secondary_genres = str_replace_all(chart2$secondary_genres, "\n", ""); chart2$secondary_genres #remove the "\n"'s
chart2$secondary_genres = str_replace_all(chart2$secondary_genres, " ", ""); chart2$secondary_genres #remove whitespaces
chart2$secondary_genres = gsub("([a-z])([A-Z])", "\\1 \\2", chart2$secondary_genres); chart2$secondary_genres #add spaces back between the condensed names
chart2$secondary_genres = gsub(",([A-Za-z])", ", \\1", chart2$secondary_genres); chart2$secondary_genres #add spaces after the commas
chart2$secondary_genres = gsub("([a-z])&([A-Z])", "\\1 & \\2", chart2$secondary_genres); chart2$secondary_genres #add space between secondary genre labels separated by "&" (except for R&B)
chart2$secondary_genres[which(chart2$secondary_genres=="")] <- "NA" #fill empty cells with NA
View(chart2)


#Clean descriptors column

chart2$descriptors = chart$descriptors #to reset, if necessary
chart2$descriptors = str_replace_all(chart2$descriptors, "\n", ""); chart2$descriptors #remove the "\n"'s
chart2$descriptors = str_replace_all(chart2$descriptors, " ", ""); chart2$descriptors #remove whitespaces
chart2$descriptors = gsub("([a-z])([A-Z])", "\\1 \\2", chart2$descriptors); chart2$descriptors #add spaces back between the condensed descriptions
chart2$descriptors = gsub(",([A-Za-z])", ", \\1", chart2$descriptors); chart2$descriptors #add spaces after the commas
View(chart2)

#Change albumdate from chr to Date format -- this was a tough one lol
#--dates appear to follow 3 formats: "dd Month YYYY", "Month YYYY", and "YYYY"

chart2$release_date = chart$release_date #to reset, if necessary
chart2$release_date = str_replace_all(chart2$release_date, "\n", ""); chart2$release_date #remove the "\n"'s
chart2$release_date = str_replace_all(chart2$release_date, "Live", ""); chart2$release_date #remove the word "Live" from the date field (data frame includes Live and Archival albums)
chart2$release_date = str_replace_all(chart2$release_date, "Archival", ""); chart2$release_date #remove the word "Archival" from the date field (data frame includes Live and Archival albums)
chart2$release_date = str_replace_all(chart2$release_date, " ", ""); chart2$release_date #remove whitespaces
chart2$release_date = gsub("([0-9])([A-Z])", "\\1 \\2", chart2$release_date); chart2$release_date #add space between day and Month
chart2$release_date = gsub("([a-z])([0-9])", "\\1 \\2", chart2$release_date); chart2$release_date #add space between Month and Year

#--create 2 temporary columns that duplicate the cleaner albumdate variable
chart2$release_date2 = chart2$release_date
chart2$release_date3 = chart2$release_date2

#--convert each albumdate variable into Date format using a different method for the 3 date types specified above
chart2$release_date = as.Date(chart2$release_date, "%d %B %Y"); chart2$release_date #convert all "dd Month YYYY" types to Date format
chart2$release_date2 = myd(chart2$release_date2, truncated = 1) #convert all "Month YYYY" types to Date format (truncate function replaces missing day with 01)
chart2$release_date3 = as.Date(as.yearmon(as.numeric(chart2$release_date3))) #convert all "YYYY" types to Date format (replaces missing day and Month with 01)

chart2$release_date = as.Date(ifelse(is.na(chart2$release_date), chart2$release_date2, chart2$release_date)) #replace NAs in albumdate with value in the same row from albumdate2 
chart2$release_date = as.Date(ifelse(is.na(chart2$release_date), chart2$release_date3, chart2$release_date)) #replace remaining NAs in albumdate with value in the same row from albumdate3

is.na(chart2$release_date) #check to see if there are any NAs left -- they should all say "FALSE"

chart2 = subset(chart2, select = -c(release_date2, release_date3)) #remove albumdate2 and albumdate3

#Save as CSV
write.csv(chart2, "rym_chart2.csv")

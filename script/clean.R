#------------------------Part 5: Cleaning Chart Data----------------------------
chart2 = rym_chart1 #duplicated data frame, for trial and error purposes :)

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

#Change release_date from chr to Date format -- this was a tough one
#--dates appear to follow 3 formats: "dd Month YYYY", "Month YYYY", and "YYYY"

chart2$release_date = chart$release_date #to reset, if necessary
chart2$release_date = str_replace_all(chart2$release_date, "\n", ""); chart2$release_date #remove the "\n"'s
chart2$release_date = str_replace_all(chart2$release_date, " ", ""); chart2$release_date #remove whitespaces
chart2$release_date = gsub("([0-9])([A-Z])", "\\1 \\2", chart2$release_date); chart2$release_date #add space between day and Month
chart2$release_date = gsub("([a-z])([0-9])", "\\1 \\2", chart2$release_date); chart2$release_date #add space between Month and Year

#--create 2 temporary columns that duplicate the cleaner release_date variable
chart2$release_date2 = chart2$release_date
chart2$release_date3 = chart2$release_date2

#--convert each release_date variable into Date format using a different method for the 3 date types specified above
chart2$release_date = as.Date(chart2$release_date, "%d %B %Y"); chart2$release_date #convert all "dd Month YYYY" types to Date format
chart2$release_date2 = myd(chart2$release_date2, truncated = 1) #convert all "Month YYYY" types to Date format (truncate function replaces missing day with 01)
chart2$release_date3 = as.Date(as.yearmon(as.numeric(chart2$release_date3))) #convert all "YYYY" types to Date format (replaces missing day and Month with 01)

chart2$release_date = as.Date(ifelse(is.na(chart2$release_date), chart2$release_date2, chart2$release_date)) #replace NAs in release_date with value in the same row from release_date2 
chart2$release_date = as.Date(ifelse(is.na(chart2$release_date), chart2$release_date3, chart2$release_date)) #replace remaining NAs in release_date with value in the same row from release_date3

is.na(chart2$release_date) #check to see if there are any NAs left -- they should all say "FALSE"

chart2 = subset(chart2, select = -c(release_date2, release_date3)) #remove release_date2 and release_date3

#Save as CSV
write.csv(chart2, "rym_chart2.csv")

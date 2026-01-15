
library(XML)
library(stringr)

xml_file <- xmlParse("~/Downloads/I Do Not Know/music/January_13_Library.xml")

root <- xmlRoot(xml_file)

songs <- xpathApply(root, "//dict/dict", function(node) {
  keys <- xpathSApply(node, "key", xmlValue)
  values <- xpathSApply(node, "key/following-sibling::*[1]", xmlValue)
  return(setNames(as.list(values), keys))
})

all_keys <- unique(unlist(lapply(songs, names)))

sugar_bombs <- lapply(songs, function(song) {
  song[setdiff(all_keys, names(song))] <- NA
  return(song)
})

songs_df <- do.call(rbind, lapply(sugar_bombs, function(x) as.data.frame(as.list(x), stringsAsFactors = FALSE)))

head(songs_df)

songs_df <- songs_df[1,]

library(tidyverse)

melted_songs <- songs_df %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Values")

melted_songs <- melted_songs[,2]

melted_songs <- melted_songs[c(1:3557),]

melted_songs$'Track ID' <- str_extract(melted_songs$Values,"([ID])(\\d{4,5})",group = 2)

melted_songs$Values <- gsub("(Track ID\\d{4,5})(.+)","\\2",melted_songs$Values)

melted_songs$Values <- gsub("(.+)(Normalization.+)","\\1",melted_songs$Values)

melted_songs$Values <- gsub("(.+)(FavoritedLoved)","\\1",melted_songs$Values)

melted_songs$`Release Date` <- str_extract(melted_songs$Values,"(.+)(Release\\sDate.+)",group = 2)

melted_songs$`Release Date` <- gsub("(.+[Z])([A-Z].+)","\\1",melted_songs$`Release Date`)

melted_songs$`Release Date` <- gsub("(Release Date)(.+)","\\2",melted_songs$`Release Date`)

melted_songs$Values <- gsub("(.+)(Skip Date.+)","\\1",melted_songs$Values)

melted_songs$`Skip Total` <- str_extract(melted_songs$Values,"(.+)(Skip Count.+)",group = 2)

melted_songs$`Skip Total` <- gsub("(Skip Count)(.+)","\\1 \\2",melted_songs$`Skip Total`)

melted_songs$`Skip Total` <- gsub("Skip Count ","",melted_songs$`Skip Total`)

melted_songs$`Skip Total` <- as.integer(melted_songs$`Skip Total`)

melted_songs$`Skip Total`[is.na(melted_songs$`Skip Total`)] <- 0

melted_songs$Values <- gsub("(.+)(Skip Count.+)","\\1",melted_songs$Values)

melted_songs$`Last Play Date` <- str_extract(melted_songs$Values,"(.+)(Play Date.+)",group = 2)

melted_songs$`Last Play Date` <- gsub("(.+[Z])(Release Date.+)","\\1",melted_songs$`Last Play Date`)

melted_songs$`Last Play Date` <- gsub("(Play Date )(.+)","\\2",melted_songs$`Last Play Date`)

melted_songs$Values <- gsub("(.+)(Play Date.+)(Play Date.+)","\\1",melted_songs$Values)

melted_songs$'Play Total' <- str_extract(melted_songs$Values,"(.)(Play Count\\d+)", group = 2)

melted_songs$`Play Total` <- gsub("(Play Count)(\\d+)","\\2",melted_songs$`Play Total`)

melted_songs$`Play Total` <- as.integer(melted_songs$`Play Total`)

melted_songs$`Play Total`[is.na(melted_songs$`Play Total`)] <- 0

melted_songs$Values <- gsub("(.+)(Bit Rate.+)","\\1",melted_songs$Values)

melted_songs$`Added to Library` <- str_extract(melted_songs$Values,"(.+)(Date Added.+)",group = 2)

melted_songs$`Added to Library` <- gsub("Date Added","",melted_songs$`Added to Library`)

melted_songs$Values <- gsub("(.+)(Date Modified.+)","\\1",melted_songs$Values)

melted_songs$`Year Released` <- str_extract(melted_songs$Values,"(.+)(Year\\d.+)",group = 2)

melted_songs$`Year Released` <- gsub("Year","",melted_songs$`Year Released`)

melted_songs$Values <- gsub("(.+)(Year\\d{4})","\\1",melted_songs$Values)

melted_songs$`Track Number` <- str_extract(melted_songs$Values,"(.+)(Track Number.+)",group = 2)

melted_songs$`Track Number` <- gsub("(Track Number)(.+)","\\2",melted_songs$`Track Number`)

melted_songs$`Track Number` <- gsub("Track Count"," of ",melted_songs$`Track Number`)

melted_songs$Values <- gsub("(.+)(Disc Number.+)","\\1",melted_songs$Values)

melted_songs$'Total Time' <- str_extract(melted_songs$Values,"(.+)(Total Time\\d+)", group = 2)

melted_songs$`Total Time` <- gsub("(Total Time)(\\d+)","\\2",melted_songs$`Total Time`)

melted_songs$Values <- gsub("(.+)(Total Time\\d+)","\\1",melted_songs$Values)

melted_songs$Values <- gsub("(.+)(Kind.+)","\\1",melted_songs$Values)

melted_songs$Genre <- str_extract(melted_songs$Values,"(.+)(Genre.+)", group = 2)

melted_songs$Genre <- gsub("(Genre)(.+)","\\2",melted_songs$Genre)

melted_songs$Values <- gsub("(.+)(Genre.+)","\\1",melted_songs$Values)

melted_songs$Album <- str_extract(melted_songs$Values,"(.+)(Album Artist.+)(Album.+)", group = 3)

melted_songs$Album <- gsub("(Album)(.+)","\\2",melted_songs$Album)

melted_songs$Values <- gsub("(.+)(Album Artist.+)","\\1",melted_songs$Values)

melted_songs$Artist <- str_extract(melted_songs$Values,"(.+)(Artist.+)", group = 2)

melted_songs$Artist <- gsub("(Artist)(.+)","\\2",melted_songs$Artist)

melted_songs$Values <- gsub("(.+)(Artist.+)","\\1",melted_songs$Values)

melted_songs$Values <- gsub("(^.{4})(.+)","\\2",melted_songs$Values)

melted_songs$Values <- gsub("(^e)(.+)","\\2",melted_songs$Values)

melted_songs$`Total Time` <- as.integer(melted_songs$`Total Time`)

melted_songs$'Total Time Listened' <- as.integer(melted_songs$`Play Total`*melted_songs$`Total Time`)

melted_songs <- melted_songs[,-2]

columns <- c("Song Title","Release Date","Total Skips","Last Time Played", "Play Total", "Date Added", "Year Released", "Track Number", "Total Time","Genre","Album","Artist","Total Time Listened")

colnames(melted_songs) <- columns

moved_columns <- c(1,12,11,8,5,10,6,9,13,4,2,3,7)

melted_songs <- melted_songs[,moved_columns]

melted_songs$`Total Time Listened` <- round(melted_songs$`Total Time Listened`/3600000,2)

melted_songs$`Total Time Listened Minutes` <- str_extract(melted_songs$`Total Time Listened`,"(\\d+)(\\.\\d+)", group = 2)

melted_songs$`Total Time Listened` <- gsub("(\\d+)(\\.\\d+)","\\1",melted_songs$`Total Time Listened`)

melted_songs$`Total Time Listened` <- as.integer(melted_songs$`Total Time Listened`)

melted_songs$`Total Time Listened Minutes` <- gsub("(.+)","0\\1",melted_songs$`Total Time Listened Minutes`)

melted_songs$`Total Time Listened Minutes` <- replace_na(melted_songs$`Total Time Listened Minutes`,"0")
melted_songs$`Total Time Listened Minutes` <- as.numeric(melted_songs$`Total Time Listened Minutes`)
melted_songs$`Total Time Listened Minutes` <- melted_songs$`Total Time Listened Minutes`*60
melted_songs$`Total Time Listened Seconds` <- str_extract(melted_songs$`Total Time Listened Minutes`,"(\\d+)(\\.\\d+)", group = 2)
melted_songs$`Total Time Listened Minutes` <- gsub("(\\d+)(\\.\\d+)","\\1",melted_songs$`Total Time Listened Minutes`)
melted_songs$`Total Time Listened Minutes` <- as.numeric(melted_songs$`Total Time Listened Minutes`)
melted_songs$`Total Time Listened Seconds` <- replace_na(melted_songs$`Total Time Listened Seconds`,"0")
melted_songs$`Total Time Listened Seconds` <- gsub("(\\.\\d+)","0\\1",melted_songs$`Total Time Listened Seconds`)
melted_songs$`Total Time Listened Seconds` <- as.numeric(melted_songs$`Total Time Listened Seconds`)
melted_songs$`Total Time Listened Seconds` <- melted_songs$`Total Time Listened Seconds`*60

new_names <- c("Song Title","Artist","Album","Track Number","Play Total","Genre","Date Added","Total Time","Total Hours Listened","Last Time Played","Release Date","Total Skips","Year Released","Total Minutes Listened","Total Seconds Listened")

colnames(melted_songs) <- new_names

newest_order <- c(1,2,3,4,5,6,7,8,9,14,15,10,11,12,13)

melted_songs <- melted_songs[,newest_order]

# This is where I had to update the album names to the correct titles since whatever regular expression coding
# I was attempting to use was removing everything but a closing parenthesis

melted_songs[520,2] <- "Lil Uzi Vert, Quavo & Travis Scott"
melted_songs[520,3] <- "The Fate of the Furious: The Album"
melted_songs[c(890,1479,1710,1711),3] <- "The Beatles (The White Album)"
melted_songs[c(1036,1233,1723,1724,1725),3] <- "The Beatles 1967-1970 (The Blue Album)"
melted_songs[84,3] <- "Greatest Hits! (Album)"
melted_songs[2818,2] <- "Tomoko Aran"
melted_songs[2818,3] <- "Fuyukukan"
melted_songs[2818,4] <- "J-Pop"
melted_songs[3492,3] <- "Hurry Up Tomorrow (Video Album)"

melted_songs$Album <- gsub(" Music From And Inspired By","Black Panther The Album Music From And Inspired By",melted_songs$Album)

melted_songs$`Song Title` <- as.factor(melted_songs$`Song Title`)
melted_songs$Artist <- as.factor(melted_songs$Artist)
melted_songs$Album <- as.factor(melted_songs$Album)

melted_csv <- write.csv(melted_songs,"~/Downloads/I Do Not Know/music/completely_music.csv")
# I now will save as .csv so I can use the data on Tableau Public to make some visualizations of my data
# write.csv(melted_songs, file = '~/Downloads/I Do Not Know/Dec15.csv')

# I want to include this later to total out the amount of time I have spent listening to music on Apple Music
# account, just need to logically think on the math and coding for it

# sum(melted_songs$`Hours Listened`)
# sum(melted_songs$`Minutes Listened`)/60
# ((sum(melted_songs$`Seconds Listened`)+49.98)/60)/60


## This is for a new idea I had to create a matrix with the RGB values to randomize colors on all parts
## of the ShinyApp, but it will take a different day to work on it!

# colors_matrix <- matrix(data = NA, nrow = 256, ncol = 3)

# column_names <- c("Red","Green","Blue")

# colnames(colors_matrix) <- column_names

# colors_matrix[1:256,] <- seq(from = 0, to = 255, by = 1)

# alpha_values <- 1

# randomizer <- function(matrix){
#  apply(matrix, 2, function(col) sample(col,1))
# }

# random_rgba <- randomizer(colors_matrix)

# color_finals <- paste0("rgba(",random_rgba[1],",",random_rgba[2],",",random_rgba[3],",",alpha_values,")")
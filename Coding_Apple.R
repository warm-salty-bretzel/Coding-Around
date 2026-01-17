
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


melted_music <- read.csv("completely_music.csv", check.names = FALSE)

melted_music <- melted_music[,-1]



melted_music <- melted_music %>% 
  group_by(`Song Title`,Artist) %>% 
  summarise(
    across(where(is.numeric),sum,na.rm = TRUE),
    across(where(~ !is.numeric(.)),first),
    .groups = "drop"
    )

mixed_music <- melted_music[sample(nrow(melted_music)),]

mixed_music$Day <- ceiling(seq_len(nrow(mixed_music))/10)


## This is where I created a dataframe for each day that I would be able to know the exact songs without looking through the whole frame

Big_ol_song_list_1 <- mixed_music[mixed_music$Day == 1,]
Big_ol_song_list_2 <- mixed_music[mixed_music$Day == 2,]
Big_ol_song_list_3 <- mixed_music[mixed_music$Day == 3,]
Big_ol_song_list_4 <- mixed_music[mixed_music$Day == 4,]
Big_ol_song_list_5 <- mixed_music[mixed_music$Day == 5,]
Big_ol_song_list_6 <- mixed_music[mixed_music$Day == 6,]
Big_ol_song_list_7 <- mixed_music[mixed_music$Day == 7,]
Big_ol_song_list_8 <- mixed_music[mixed_music$Day == 8,]
Big_ol_song_list_9 <- mixed_music[mixed_music$Day == 9,]
Big_ol_song_list_10 <- mixed_music[mixed_music$Day == 10,]
Big_ol_song_list_11 <- mixed_music[mixed_music$Day == 11,]
Big_ol_song_list_12 <- mixed_music[mixed_music$Day == 12,]
Big_ol_song_list_13 <- mixed_music[mixed_music$Day == 13,]
Big_ol_song_list_14 <- mixed_music[mixed_music$Day == 14,]
Big_ol_song_list_15 <- mixed_music[mixed_music$Day == 15,]
Big_ol_song_list_16 <- mixed_music[mixed_music$Day == 16,]
Big_ol_song_list_17 <- mixed_music[mixed_music$Day == 17,]
Big_ol_song_list_18 <- mixed_music[mixed_music$Day == 18,]
Big_ol_song_list_19 <- mixed_music[mixed_music$Day == 19,]
Big_ol_song_list_20 <- mixed_music[mixed_music$Day == 20,]
Big_ol_song_list_21 <- mixed_music[mixed_music$Day == 21,]
Big_ol_song_list_22 <- mixed_music[mixed_music$Day == 22,]
Big_ol_song_list_23 <- mixed_music[mixed_music$Day == 23,]
Big_ol_song_list_24 <- mixed_music[mixed_music$Day == 24,]
Big_ol_song_list_25 <- mixed_music[mixed_music$Day == 25,]
Big_ol_song_list_26 <- mixed_music[mixed_music$Day == 26,]
Big_ol_song_list_27 <- mixed_music[mixed_music$Day == 27,]
Big_ol_song_list_28 <- mixed_music[mixed_music$Day == 28,]
Big_ol_song_list_29 <- mixed_music[mixed_music$Day == 29,]
Big_ol_song_list_30 <- mixed_music[mixed_music$Day == 30,]
Big_ol_song_list_31 <- mixed_music[mixed_music$Day == 31,]
Big_ol_song_list_32 <- mixed_music[mixed_music$Day == 32,]
Big_ol_song_list_33 <- mixed_music[mixed_music$Day == 33,]
Big_ol_song_list_34 <- mixed_music[mixed_music$Day == 34,]
Big_ol_song_list_35 <- mixed_music[mixed_music$Day == 35,]
Big_ol_song_list_36 <- mixed_music[mixed_music$Day == 36,]
Big_ol_song_list_37 <- mixed_music[mixed_music$Day == 37,]
Big_ol_song_list_38 <- mixed_music[mixed_music$Day == 38,]
Big_ol_song_list_39 <- mixed_music[mixed_music$Day == 39,]
Big_ol_song_list_40 <- mixed_music[mixed_music$Day == 40,]
Big_ol_song_list_41 <- mixed_music[mixed_music$Day == 41,]
Big_ol_song_list_42 <- mixed_music[mixed_music$Day == 42,]
Big_ol_song_list_43 <- mixed_music[mixed_music$Day == 43,]
Big_ol_song_list_44 <- mixed_music[mixed_music$Day == 44,]
Big_ol_song_list_45 <- mixed_music[mixed_music$Day == 45,]
Big_ol_song_list_46 <- mixed_music[mixed_music$Day == 46,]
Big_ol_song_list_47 <- mixed_music[mixed_music$Day == 47,]
Big_ol_song_list_48 <- mixed_music[mixed_music$Day == 48,]
Big_ol_song_list_49 <- mixed_music[mixed_music$Day == 49,]
Big_ol_song_list_50 <- mixed_music[mixed_music$Day == 50,]
Big_ol_song_list_51 <- mixed_music[mixed_music$Day == 51,]
Big_ol_song_list_52 <- mixed_music[mixed_music$Day == 52,]
Big_ol_song_list_53 <- mixed_music[mixed_music$Day == 53,]
Big_ol_song_list_54 <- mixed_music[mixed_music$Day == 54,]
Big_ol_song_list_55 <- mixed_music[mixed_music$Day == 55,]
Big_ol_song_list_56 <- mixed_music[mixed_music$Day == 56,]
Big_ol_song_list_57 <- mixed_music[mixed_music$Day == 57,]
Big_ol_song_list_58 <- mixed_music[mixed_music$Day == 58,]
Big_ol_song_list_59 <- mixed_music[mixed_music$Day == 59,]
Big_ol_song_list_60 <- mixed_music[mixed_music$Day == 60,]
Big_ol_song_list_61 <- mixed_music[mixed_music$Day == 61,]
Big_ol_song_list_62 <- mixed_music[mixed_music$Day == 62,]
Big_ol_song_list_63 <- mixed_music[mixed_music$Day == 63,]
Big_ol_song_list_64 <- mixed_music[mixed_music$Day == 64,]
Big_ol_song_list_65 <- mixed_music[mixed_music$Day == 65,]
Big_ol_song_list_66 <- mixed_music[mixed_music$Day == 66,]
Big_ol_song_list_67 <- mixed_music[mixed_music$Day == 67,]
Big_ol_song_list_68 <- mixed_music[mixed_music$Day == 68,]
Big_ol_song_list_69 <- mixed_music[mixed_music$Day == 69,]
Big_ol_song_list_70 <- mixed_music[mixed_music$Day == 70,]
Big_ol_song_list_71 <- mixed_music[mixed_music$Day == 71,]
Big_ol_song_list_72 <- mixed_music[mixed_music$Day == 72,]
Big_ol_song_list_73 <- mixed_music[mixed_music$Day == 73,]
Big_ol_song_list_74 <- mixed_music[mixed_music$Day == 74,]
Big_ol_song_list_75 <- mixed_music[mixed_music$Day == 75,]
Big_ol_song_list_76 <- mixed_music[mixed_music$Day == 76,]
Big_ol_song_list_77 <- mixed_music[mixed_music$Day == 77,]
Big_ol_song_list_78 <- mixed_music[mixed_music$Day == 78,]
Big_ol_song_list_79 <- mixed_music[mixed_music$Day == 79,]
Big_ol_song_list_80 <- mixed_music[mixed_music$Day == 80,]
Big_ol_song_list_81 <- mixed_music[mixed_music$Day == 81,]
Big_ol_song_list_82 <- mixed_music[mixed_music$Day == 82,]
Big_ol_song_list_83 <- mixed_music[mixed_music$Day == 83,]
Big_ol_song_list_84 <- mixed_music[mixed_music$Day == 84,]
Big_ol_song_list_85 <- mixed_music[mixed_music$Day == 85,]
Big_ol_song_list_86 <- mixed_music[mixed_music$Day == 86,]
Big_ol_song_list_87 <- mixed_music[mixed_music$Day == 87,]
Big_ol_song_list_88 <- mixed_music[mixed_music$Day == 88,]
Big_ol_song_list_89 <- mixed_music[mixed_music$Day == 89,]
Big_ol_song_list_90 <- mixed_music[mixed_music$Day == 90,]
Big_ol_song_list_91 <- mixed_music[mixed_music$Day == 91,]
Big_ol_song_list_92 <- mixed_music[mixed_music$Day == 92,]
Big_ol_song_list_93 <- mixed_music[mixed_music$Day == 93,]
Big_ol_song_list_94 <- mixed_music[mixed_music$Day == 94,]
Big_ol_song_list_95 <- mixed_music[mixed_music$Day == 95,]
Big_ol_song_list_96 <- mixed_music[mixed_music$Day == 96,]
Big_ol_song_list_97 <- mixed_music[mixed_music$Day == 97,]
Big_ol_song_list_98 <- mixed_music[mixed_music$Day == 98,]
Big_ol_song_list_99 <- mixed_music[mixed_music$Day == 99,]
Big_ol_song_list_100 <- mixed_music[mixed_music$Day == 100,]
Big_ol_song_list_101 <- mixed_music[mixed_music$Day == 101,]
Big_ol_song_list_102 <- mixed_music[mixed_music$Day == 102,]
Big_ol_song_list_103 <- mixed_music[mixed_music$Day == 103,]
Big_ol_song_list_104 <- mixed_music[mixed_music$Day == 104,]
Big_ol_song_list_105 <- mixed_music[mixed_music$Day == 105,]
Big_ol_song_list_106 <- mixed_music[mixed_music$Day == 106,]
Big_ol_song_list_107 <- mixed_music[mixed_music$Day == 107,]
Big_ol_song_list_108 <- mixed_music[mixed_music$Day == 108,]
Big_ol_song_list_109 <- mixed_music[mixed_music$Day == 109,]
Big_ol_song_list_110 <- mixed_music[mixed_music$Day == 110,]
Big_ol_song_list_111 <- mixed_music[mixed_music$Day == 111,]
Big_ol_song_list_112 <- mixed_music[mixed_music$Day == 112,]
Big_ol_song_list_113 <- mixed_music[mixed_music$Day == 113,]
Big_ol_song_list_114 <- mixed_music[mixed_music$Day == 114,]
Big_ol_song_list_115 <- mixed_music[mixed_music$Day == 115,]
Big_ol_song_list_116 <- mixed_music[mixed_music$Day == 116,]
Big_ol_song_list_117 <- mixed_music[mixed_music$Day == 117,]
Big_ol_song_list_118 <- mixed_music[mixed_music$Day == 118,]
Big_ol_song_list_119 <- mixed_music[mixed_music$Day == 119,]
Big_ol_song_list_120 <- mixed_music[mixed_music$Day == 120,]
Big_ol_song_list_121 <- mixed_music[mixed_music$Day == 121,]
Big_ol_song_list_122 <- mixed_music[mixed_music$Day == 122,]
Big_ol_song_list_123 <- mixed_music[mixed_music$Day == 123,]
Big_ol_song_list_124 <- mixed_music[mixed_music$Day == 124,]
Big_ol_song_list_125 <- mixed_music[mixed_music$Day == 125,]
Big_ol_song_list_126 <- mixed_music[mixed_music$Day == 126,]
Big_ol_song_list_127 <- mixed_music[mixed_music$Day == 127,]
Big_ol_song_list_128 <- mixed_music[mixed_music$Day == 128,]
Big_ol_song_list_129 <- mixed_music[mixed_music$Day == 129,]
Big_ol_song_list_130 <- mixed_music[mixed_music$Day == 130,]
Big_ol_song_list_131 <- mixed_music[mixed_music$Day == 131,]
Big_ol_song_list_132 <- mixed_music[mixed_music$Day == 132,]
Big_ol_song_list_133 <- mixed_music[mixed_music$Day == 133,]
Big_ol_song_list_134 <- mixed_music[mixed_music$Day == 134,]
Big_ol_song_list_135 <- mixed_music[mixed_music$Day == 135,]
Big_ol_song_list_136 <- mixed_music[mixed_music$Day == 136,]
Big_ol_song_list_137 <- mixed_music[mixed_music$Day == 137,]
Big_ol_song_list_138 <- mixed_music[mixed_music$Day == 138,]
Big_ol_song_list_139 <- mixed_music[mixed_music$Day == 139,]
Big_ol_song_list_140 <- mixed_music[mixed_music$Day == 140,]
Big_ol_song_list_141 <- mixed_music[mixed_music$Day == 141,]
Big_ol_song_list_142 <- mixed_music[mixed_music$Day == 142,]
Big_ol_song_list_143 <- mixed_music[mixed_music$Day == 143,]
Big_ol_song_list_144 <- mixed_music[mixed_music$Day == 144,]
Big_ol_song_list_145 <- mixed_music[mixed_music$Day == 145,]
Big_ol_song_list_146 <- mixed_music[mixed_music$Day == 146,]
Big_ol_song_list_147 <- mixed_music[mixed_music$Day == 147,]
Big_ol_song_list_148 <- mixed_music[mixed_music$Day == 148,]
Big_ol_song_list_149 <- mixed_music[mixed_music$Day == 149,]
Big_ol_song_list_150 <- mixed_music[mixed_music$Day == 150,]
Big_ol_song_list_151 <- mixed_music[mixed_music$Day == 151,]
Big_ol_song_list_152 <- mixed_music[mixed_music$Day == 152,]
Big_ol_song_list_153 <- mixed_music[mixed_music$Day == 153,]
Big_ol_song_list_154 <- mixed_music[mixed_music$Day == 154,]
Big_ol_song_list_155 <- mixed_music[mixed_music$Day == 155,]
Big_ol_song_list_156 <- mixed_music[mixed_music$Day == 156,]
Big_ol_song_list_157 <- mixed_music[mixed_music$Day == 157,]
Big_ol_song_list_158 <- mixed_music[mixed_music$Day == 158,]
Big_ol_song_list_159 <- mixed_music[mixed_music$Day == 159,]
Big_ol_song_list_160 <- mixed_music[mixed_music$Day == 160,]
Big_ol_song_list_161 <- mixed_music[mixed_music$Day == 161,]
Big_ol_song_list_162 <- mixed_music[mixed_music$Day == 162,]
Big_ol_song_list_163 <- mixed_music[mixed_music$Day == 163,]
Big_ol_song_list_164 <- mixed_music[mixed_music$Day == 164,]
Big_ol_song_list_165 <- mixed_music[mixed_music$Day == 165,]
Big_ol_song_list_166 <- mixed_music[mixed_music$Day == 166,]
Big_ol_song_list_167 <- mixed_music[mixed_music$Day == 167,]
Big_ol_song_list_168 <- mixed_music[mixed_music$Day == 168,]
Big_ol_song_list_169 <- mixed_music[mixed_music$Day == 169,]
Big_ol_song_list_170 <- mixed_music[mixed_music$Day == 170,]
Big_ol_song_list_171 <- mixed_music[mixed_music$Day == 171,]
Big_ol_song_list_172 <- mixed_music[mixed_music$Day == 172,]
Big_ol_song_list_173 <- mixed_music[mixed_music$Day == 173,]
Big_ol_song_list_174 <- mixed_music[mixed_music$Day == 174,]
Big_ol_song_list_175 <- mixed_music[mixed_music$Day == 175,]
Big_ol_song_list_176 <- mixed_music[mixed_music$Day == 176,]
Big_ol_song_list_177 <- mixed_music[mixed_music$Day == 177,]
Big_ol_song_list_178 <- mixed_music[mixed_music$Day == 178,]
Big_ol_song_list_179 <- mixed_music[mixed_music$Day == 179,]
Big_ol_song_list_180 <- mixed_music[mixed_music$Day == 180,]
Big_ol_song_list_181 <- mixed_music[mixed_music$Day == 181,]
Big_ol_song_list_182 <- mixed_music[mixed_music$Day == 182,]
Big_ol_song_list_183 <- mixed_music[mixed_music$Day == 183,]
Big_ol_song_list_184 <- mixed_music[mixed_music$Day == 184,]
Big_ol_song_list_185 <- mixed_music[mixed_music$Day == 185,]
Big_ol_song_list_186 <- mixed_music[mixed_music$Day == 186,]
Big_ol_song_list_187 <- mixed_music[mixed_music$Day == 187,]
Big_ol_song_list_188 <- mixed_music[mixed_music$Day == 188,]
Big_ol_song_list_189 <- mixed_music[mixed_music$Day == 189,]
Big_ol_song_list_190 <- mixed_music[mixed_music$Day == 190,]
Big_ol_song_list_191 <- mixed_music[mixed_music$Day == 191,]
Big_ol_song_list_192 <- mixed_music[mixed_music$Day == 192,]
Big_ol_song_list_193 <- mixed_music[mixed_music$Day == 193,]
Big_ol_song_list_194 <- mixed_music[mixed_music$Day == 194,]
Big_ol_song_list_195 <- mixed_music[mixed_music$Day == 195,]
Big_ol_song_list_196 <- mixed_music[mixed_music$Day == 196,]
Big_ol_song_list_197 <- mixed_music[mixed_music$Day == 197,]
Big_ol_song_list_198 <- mixed_music[mixed_music$Day == 198,]
Big_ol_song_list_199 <- mixed_music[mixed_music$Day == 199,]
Big_ol_song_list_200 <- mixed_music[mixed_music$Day == 200,]
Big_ol_song_list_201 <- mixed_music[mixed_music$Day == 201,]
Big_ol_song_list_202 <- mixed_music[mixed_music$Day == 202,]
Big_ol_song_list_203 <- mixed_music[mixed_music$Day == 203,]
Big_ol_song_list_204 <- mixed_music[mixed_music$Day == 204,]
Big_ol_song_list_205 <- mixed_music[mixed_music$Day == 205,]
Big_ol_song_list_206 <- mixed_music[mixed_music$Day == 206,]
Big_ol_song_list_207 <- mixed_music[mixed_music$Day == 207,]
Big_ol_song_list_208 <- mixed_music[mixed_music$Day == 208,]
Big_ol_song_list_209 <- mixed_music[mixed_music$Day == 209,]
Big_ol_song_list_210 <- mixed_music[mixed_music$Day == 210,]
Big_ol_song_list_211 <- mixed_music[mixed_music$Day == 211,]
Big_ol_song_list_212 <- mixed_music[mixed_music$Day == 212,]
Big_ol_song_list_213 <- mixed_music[mixed_music$Day == 213,]
Big_ol_song_list_214 <- mixed_music[mixed_music$Day == 214,]
Big_ol_song_list_215 <- mixed_music[mixed_music$Day == 215,]
Big_ol_song_list_216 <- mixed_music[mixed_music$Day == 216,]
Big_ol_song_list_217 <- mixed_music[mixed_music$Day == 217,]
Big_ol_song_list_218 <- mixed_music[mixed_music$Day == 218,]
Big_ol_song_list_219 <- mixed_music[mixed_music$Day == 219,]
Big_ol_song_list_220 <- mixed_music[mixed_music$Day == 220,]
Big_ol_song_list_221 <- mixed_music[mixed_music$Day == 221,]
Big_ol_song_list_222 <- mixed_music[mixed_music$Day == 222,]
Big_ol_song_list_223 <- mixed_music[mixed_music$Day == 223,]
Big_ol_song_list_224 <- mixed_music[mixed_music$Day == 224,]
Big_ol_song_list_225 <- mixed_music[mixed_music$Day == 225,]
Big_ol_song_list_226 <- mixed_music[mixed_music$Day == 226,]
Big_ol_song_list_227 <- mixed_music[mixed_music$Day == 227,]
Big_ol_song_list_228 <- mixed_music[mixed_music$Day == 228,]
Big_ol_song_list_229 <- mixed_music[mixed_music$Day == 229,]
Big_ol_song_list_230 <- mixed_music[mixed_music$Day == 230,]
Big_ol_song_list_231 <- mixed_music[mixed_music$Day == 231,]
Big_ol_song_list_232 <- mixed_music[mixed_music$Day == 232,]
Big_ol_song_list_233 <- mixed_music[mixed_music$Day == 233,]
Big_ol_song_list_234 <- mixed_music[mixed_music$Day == 234,]
Big_ol_song_list_235 <- mixed_music[mixed_music$Day == 235,]
Big_ol_song_list_236 <- mixed_music[mixed_music$Day == 236,]
Big_ol_song_list_237 <- mixed_music[mixed_music$Day == 237,]
Big_ol_song_list_238 <- mixed_music[mixed_music$Day == 238,]
Big_ol_song_list_239 <- mixed_music[mixed_music$Day == 239,]
Big_ol_song_list_240 <- mixed_music[mixed_music$Day == 240,]
Big_ol_song_list_241 <- mixed_music[mixed_music$Day == 241,]
Big_ol_song_list_242 <- mixed_music[mixed_music$Day == 242,]
Big_ol_song_list_243 <- mixed_music[mixed_music$Day == 243,]
Big_ol_song_list_244 <- mixed_music[mixed_music$Day == 244,]
Big_ol_song_list_245 <- mixed_music[mixed_music$Day == 245,]
Big_ol_song_list_246 <- mixed_music[mixed_music$Day == 246,]
Big_ol_song_list_247 <- mixed_music[mixed_music$Day == 247,]
Big_ol_song_list_248 <- mixed_music[mixed_music$Day == 248,]
Big_ol_song_list_249 <- mixed_music[mixed_music$Day == 249,]
Big_ol_song_list_250 <- mixed_music[mixed_music$Day == 250,]
Big_ol_song_list_251 <- mixed_music[mixed_music$Day == 251,]
Big_ol_song_list_252 <- mixed_music[mixed_music$Day == 252,]
Big_ol_song_list_253 <- mixed_music[mixed_music$Day == 253,]
Big_ol_song_list_254 <- mixed_music[mixed_music$Day == 254,]
Big_ol_song_list_255 <- mixed_music[mixed_music$Day == 255,]
Big_ol_song_list_256 <- mixed_music[mixed_music$Day == 256,]
Big_ol_song_list_257 <- mixed_music[mixed_music$Day == 257,]
Big_ol_song_list_258 <- mixed_music[mixed_music$Day == 258,]
Big_ol_song_list_259 <- mixed_music[mixed_music$Day == 259,]
Big_ol_song_list_260 <- mixed_music[mixed_music$Day == 260,]
Big_ol_song_list_261 <- mixed_music[mixed_music$Day == 261,]
Big_ol_song_list_262 <- mixed_music[mixed_music$Day == 262,]
Big_ol_song_list_263 <- mixed_music[mixed_music$Day == 263,]
Big_ol_song_list_264 <- mixed_music[mixed_music$Day == 264,]
Big_ol_song_list_265 <- mixed_music[mixed_music$Day == 265,]
Big_ol_song_list_266 <- mixed_music[mixed_music$Day == 266,]
Big_ol_song_list_267 <- mixed_music[mixed_music$Day == 267,]
Big_ol_song_list_268 <- mixed_music[mixed_music$Day == 268,]
Big_ol_song_list_269 <- mixed_music[mixed_music$Day == 269,]
Big_ol_song_list_270 <- mixed_music[mixed_music$Day == 270,]
Big_ol_song_list_271 <- mixed_music[mixed_music$Day == 271,]
Big_ol_song_list_272 <- mixed_music[mixed_music$Day == 272,]
Big_ol_song_list_273 <- mixed_music[mixed_music$Day == 273,]
Big_ol_song_list_274 <- mixed_music[mixed_music$Day == 274,]
Big_ol_song_list_275 <- mixed_music[mixed_music$Day == 275,]
Big_ol_song_list_276 <- mixed_music[mixed_music$Day == 276,]
Big_ol_song_list_277 <- mixed_music[mixed_music$Day == 277,]
Big_ol_song_list_278 <- mixed_music[mixed_music$Day == 278,]
Big_ol_song_list_279 <- mixed_music[mixed_music$Day == 279,]
Big_ol_song_list_280 <- mixed_music[mixed_music$Day == 280,]
Big_ol_song_list_281 <- mixed_music[mixed_music$Day == 281,]
Big_ol_song_list_282 <- mixed_music[mixed_music$Day == 282,]
Big_ol_song_list_283 <- mixed_music[mixed_music$Day == 283,]
Big_ol_song_list_284 <- mixed_music[mixed_music$Day == 284,]
Big_ol_song_list_285 <- mixed_music[mixed_music$Day == 285,]
Big_ol_song_list_286 <- mixed_music[mixed_music$Day == 286,]
Big_ol_song_list_287 <- mixed_music[mixed_music$Day == 287,]
Big_ol_song_list_288 <- mixed_music[mixed_music$Day == 288,]
Big_ol_song_list_289 <- mixed_music[mixed_music$Day == 289,]
Big_ol_song_list_290 <- mixed_music[mixed_music$Day == 290,]
Big_ol_song_list_291 <- mixed_music[mixed_music$Day == 291,]
Big_ol_song_list_292 <- mixed_music[mixed_music$Day == 292,]
Big_ol_song_list_293 <- mixed_music[mixed_music$Day == 293,]
Big_ol_song_list_294 <- mixed_music[mixed_music$Day == 294,]
Big_ol_song_list_295 <- mixed_music[mixed_music$Day == 295,]
Big_ol_song_list_296 <- mixed_music[mixed_music$Day == 296,]
Big_ol_song_list_297 <- mixed_music[mixed_music$Day == 297,]
Big_ol_song_list_298 <- mixed_music[mixed_music$Day == 298,]
Big_ol_song_list_299 <- mixed_music[mixed_music$Day == 299,]
Big_ol_song_list_300 <- mixed_music[mixed_music$Day == 300,]
Big_ol_song_list_301 <- mixed_music[mixed_music$Day == 301,]
Big_ol_song_list_302 <- mixed_music[mixed_music$Day == 302,]
Big_ol_song_list_303 <- mixed_music[mixed_music$Day == 303,]
Big_ol_song_list_304 <- mixed_music[mixed_music$Day == 304,]
Big_ol_song_list_305 <- mixed_music[mixed_music$Day == 305,]
Big_ol_song_list_306 <- mixed_music[mixed_music$Day == 306,]
Big_ol_song_list_307 <- mixed_music[mixed_music$Day == 307,]
Big_ol_song_list_308 <- mixed_music[mixed_music$Day == 308,]
Big_ol_song_list_309 <- mixed_music[mixed_music$Day == 309,]
Big_ol_song_list_310 <- mixed_music[mixed_music$Day == 310,]
Big_ol_song_list_311 <- mixed_music[mixed_music$Day == 311,]
Big_ol_song_list_312 <- mixed_music[mixed_music$Day == 312,]
Big_ol_song_list_313 <- mixed_music[mixed_music$Day == 313,]
Big_ol_song_list_314 <- mixed_music[mixed_music$Day == 314,]
Big_ol_song_list_315 <- mixed_music[mixed_music$Day == 315,]
Big_ol_song_list_316 <- mixed_music[mixed_music$Day == 316,]
Big_ol_song_list_317 <- mixed_music[mixed_music$Day == 317,]
Big_ol_song_list_318 <- mixed_music[mixed_music$Day == 318,]
Big_ol_song_list_319 <- mixed_music[mixed_music$Day == 319,]
Big_ol_song_list_320 <- mixed_music[mixed_music$Day == 320,]
Big_ol_song_list_321 <- mixed_music[mixed_music$Day == 321,]
Big_ol_song_list_322 <- mixed_music[mixed_music$Day == 322,]
Big_ol_song_list_323 <- mixed_music[mixed_music$Day == 323,]
Big_ol_song_list_324 <- mixed_music[mixed_music$Day == 324,]
Big_ol_song_list_325 <- mixed_music[mixed_music$Day == 325,]
Big_ol_song_list_326 <- mixed_music[mixed_music$Day == 326,]
Big_ol_song_list_327 <- mixed_music[mixed_music$Day == 327,]
Big_ol_song_list_328 <- mixed_music[mixed_music$Day == 328,]
Big_ol_song_list_329 <- mixed_music[mixed_music$Day == 329,]
Big_ol_song_list_330 <- mixed_music[mixed_music$Day == 330,]
Big_ol_song_list_331 <- mixed_music[mixed_music$Day == 331,]
Big_ol_song_list_332 <- mixed_music[mixed_music$Day == 332,]
Big_ol_song_list_333 <- mixed_music[mixed_music$Day == 333,]
Big_ol_song_list_334 <- mixed_music[mixed_music$Day == 334,]
Big_ol_song_list_335 <- mixed_music[mixed_music$Day == 335,]

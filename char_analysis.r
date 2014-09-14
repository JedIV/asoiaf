############################
# Analysis of characters
############################
# jediv
############################
# 2014-09-12
############################

# uncomment this if you need to install packages
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("RColorBrewer")

library(ggplot2)
library(scales)
library(RColorBrewer)

# Reads in csv
chars <- read.csv("~/personal_repos/asoiaf/asoiaf_data.csv",stringsAsFactors=FALSE)

# Get chapter List
#########################

chars$book_chaps <- paste(chars$Book, chars$Chapter.Name, sep = ": ")
chap_list        <- unique(chars$book_chaps)

chars$book_chaps <- factor(chars$book_chaps,
                            levels = rev(chap_list))

chars$Book <- factor(chars$Book,
                            levels = c("A Game of Thrones",
                                       "A Storm of Swords",
                                       "A Clash of Kings",
                                       "A Feast for Crows",
                                       "A Dance with Dragons"))

chapter_plot <- ggplot(chars[chars$Chapter.Name != "Appendix" &
                             chars$Book != "The Hedge Knight" &
                             is.na(chars$Book) == FALSE,],
                             aes(book_chaps)) +
                       geom_histogram(aes(fill = Book)) +
                       coord_flip() +
                       theme_bw() + 
                       xlab("Chapter") +
                       ylab("# of New Characters") + 
                       theme(axis.text.y=element_text(size=5)) +
                       scale_fill_brewer(palette = "Set1")

######################
# Chapter list organized descending order
######################
chars_per_chap <- aggregate(Character ~ book_chaps + Book,
                            data = chars[chars$Chapter.Name != "Appendix" &
                                         chars$Book != "The Hedge Knight",], length)

chars_per_chap$book_chaps <- factor(chars_per_chap$book_chaps,
                                    levels = chars_per_chap[order(chars_per_chap$Character),]$book_chaps)

char_plot <- ggplot(chars_per_chap,
                             aes(book_chaps,Character)) +
                       geom_bar(aes(fill = Book),stat = "identity") +
                       coord_flip() +
                       theme_bw() + 
                       xlab("Chapter") +
                       ylab("# of New Characters") + 
                       theme(axis.text.y=element_text(size=5)) +
                       scale_fill_brewer(palette = "Set1")

#l <- mean(chars_per_chap$Character)

#rand_chaps <- data.frame(out = rpois(200,l))

#ggplot(chars_per_chap,aes(Character)) +
         #geom_histogram(binwidth = 1) +
         #geom_histogram(data = rand_chaps,aes(out),
         #binwidth = 1,color = "red", alpha = .1)
######################
# POV list organized descending order
######################
chars_per_chap <- aggregate(Character ~ POV,
                            data = chars[chars$Chapter.Name != "Appendix" &
                                         chars$Book != "The Hedge Knight",], length)

povs <- aggregate(book_chaps ~ POV,
                            data = chars[chars$Chapter.Name != "Appendix" &
                                         chars$Book != "The Hedge Knight",], function(x) length(unique(x)))

pov_chars <- merge(povs,chars_per_chap, by = "POV")
pov_chars$chars_per_chap <- pov_chars$Character/pov_chars$book_chaps

pov_chars$POV <- factor(pov_chars$POV,levels = pov_chars[order(pov_chars$chars_per_chap),]$POV)

char_v_pov <- ggplot(pov_chars,
                    aes(POV,chars_per_chap)) +
                    geom_bar(stat = "identity") +
                    coord_flip() +
                    theme_bw() + 
                    xlab("Chapter") +
                    ylab("# of New Characters") + 
                    theme(axis.text.y=element_text(size=5)) +
                    scale_fill_brewer(palette = "Set1")

ggsave(
  "characters_per_chapter.png",
  chapter_plot,
  width  = 8,
  height = 20,
  dpi = 1200
)

ggsave(
  "characters_per_chapter_desc.png",
  char_plot,
  width  = 8,
  height = 20,
  dpi = 1200
)

ggsave(
  "characters_per_pov.png",
  char_v_pov,
  width  = 8,
  height = 8,
  dpi = 1200
)


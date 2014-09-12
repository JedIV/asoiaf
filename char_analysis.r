############################
# Analysis of characters
############################
# jediv
############################
# 2014-09-12
############################

# uncomment this if you need to install packages
# install.packages("ggplot2")

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
                             chars$Book != "The Hedge Knight",], aes(book_chaps)) +
                       geom_histogram(aes(fill = Book)) +
                       coord_flip() +
                       theme_bw() + 
                       theme(axis.text.x=element_text(size=3))

chars_per_chap <- aggregate(Character ~ book_chaps,
                            data = chars[chars$Chapter.Name != "Appendix" &
                                         chars$Book != "The Hedge Knight",], length)

l <- mean(chars_per_chap$Character)

rand_chaps <- data.frame(out = rpois(200,l))

ggplot(chars_per_chap,aes(Character)) +
         geom_histogram(binwidth = 1) +
         geom_histogram(data = rand_chaps,aes(out),binwidth = 1,color = "red", alpha = .1)

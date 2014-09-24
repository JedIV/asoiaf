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
library(gridExtra)

# Reads in csv
chars <- read.csv("~/personal_repos/asoiaf/asoiaf_data.csv",stringsAsFactors=FALSE)

#########################
# Get chapter List
#########################
add_top_ticks <- function(plot, frame, size){
  breaks = seq(0,size,size/3) 
  num_bars = length(unique(frame))
  if(size == 15){
    a = .6
    b = .8
    c = 1.14} else {
    a = -6
    b = -5.2
    c = -4.1}
  # add tick marks
  p <- plot
  for (i in 1:length(breaks))   {
    p = p + annotation_custom(grob = linesGrob(gp=gpar(col= "black")),  
                         ymin = breaks[i], 
                         ymax = breaks[i], 
                         xmin = num_bars + a, 
                         xmax = num_bars + b)
    }

  # Add tick mark labels
  for (i in 1:length(breaks))   {
    p = p + annotation_custom(grob = textGrob(label = breaks[i], gp=gpar(col= "black", cex = .8)),  
                         ymin = breaks[i], 
                         ymax = breaks[i], 
                         xmin = num_bars + c, 
                         xmax = num_bars + c)
    }

  # Code to override clipping
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name=="panel"] <- "off"
  grid.draw(gt)
  return(gt)
}



chars$book_chaps <- paste(chars$Book, chars$Chapter.Name, sep = ": ")
chap_list        <- unique(chars$book_chaps)

chars$book_chaps <- factor(chars$book_chaps,
                            levels = rev(chap_list))

chars$Book <- factor(chars$Book,
                            levels = c("A Game of Thrones",
                                       "A Clash of Kings",
                                       "A Storm of Swords",
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
                       labs(title = "New Characters (Chapter Order)") +
                       theme(plot.title = element_text(vjust = 2.9),
                             legend.position = c(.8,.8),
                             legend.title = element_blank()) +
                       scale_fill_brewer(palette = "Set1")
chapter_plot <- add_top_ticks(chapter_plot,chars$book_chaps,30)

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
                       labs(title = "New Characters (Rank Order)") +
                       theme(plot.title = element_text(vjust = 2.9),
                             legend.position = c(.8,.8),
                             legend.title = element_blank()) +
                       scale_fill_brewer(palette = "Set1")
char_plot <- add_top_ticks(char_plot,chars$book_chaps,30)

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
                    xlab("Chapter's POV Character") +
                    ylab("# of New Characters") + 
                    theme(axis.text.y=element_text(size=6)) +
                    scale_fill_brewer(palette = "Set1") +
                    labs(title = "Average New Characters Per POV Chapter") +
                    theme(plot.title = element_text(vjust = 2.3))

char_v_pov <- add_top_ticks(char_v_pov,pov_chars$POV,15)

###########################
# K Means Analysis
###########################

chap_chars <- data.frame(scale(pov_chars$book_chaps),
                         scale(pov_chars$chars_per_chap))
pov_chars$k_choice <- kmeans(chap_chars, 3, iter.max = 15)$cluster
kmeans_plot <- ggplot(pov_chars,aes(book_chaps,chars_per_chap,label = POV,colour = factor(k_choice))) +
                       geom_text(size = 3) +
                       theme_bw() +
                       xlab("Total Chapters as POV") +
                       ylab("Characters Introduced Per Chapter") +
                       ggtitle("K-means Clustering of POV Characters in ASOIAF")

###########################
# Output
###########################

svg(
  "characters_per_chapter.svg",
  width  = 8,
  height = 20)
  grid.draw(chapter_plot)
dev.off()

svg(
  "characters_per_chapter_desc.svg",
  width  = 8,
  height = 20)
  grid.draw(char_plot)
dev.off()

svg(
  "characters_per_pov.svg",
  width  = 8,
  height = 8)
  grid.draw(char_v_pov)
dev.off()

svg(
  "kmeans_plot.svg",
  width  = 8,
  height = 8)
  kmeans_plot
dev.off()



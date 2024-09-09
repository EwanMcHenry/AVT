# ploting the number of records per recorder, and its distribution



library(tidyverse)
library(ggpubr)
library(hrbrthemes)


rec.id <- read_csv("Data//recorderID.csv")  %>% 
  count(OriginalRecorderId) %>% 
  # add column of percentile of n
  mutate(rank = row_number(n),
         percentile = 100*rank/max(rank)) 


#histograms ---- 
rec.id %>% 
  # percentile plot of frequency of each id
  ggplot(aes(x = n))+
  geom_histogram( fill = "steelblue")+
  # label the largest n
  annotate("text", x = 1500, y = 300, label = paste("Highest records: \n", rev(sort(rec.id$n))[2], "and", max(rec.id$n)))+
  # scale_x_continuous(limits = c(1,NA))+
  labs(x = "Number of records by recorder", y = "Frequency")+
  theme_pubr()


rec.id %>% 
  # percentile plot of frequency of each id
  ggplot(aes(x = n))+
  geom_histogram( fill = "steelblue")+
  # label the largest n
  annotate("text", x = 1500, y = 300, label = paste("Highest records: \n", rev(sort(rec.id$n))[2], "and", max(rec.id$n)))+
  # scale_x_continuous(limits = c(1,NA))+
  labs(x = "Number of records by recorder", y = "Frequency")+
  theme_pubr()

# hist- filtering out highest 2
rec.id %>% 
  filter(n < rev(sort(rec.id$n))[2]) %>% 
  # percentile plot of frequency of each id
  ggplot(aes(x = n))+
  geom_histogram( fill = "steelblue")+
  # label the largest n
  # annotate("text", x = 1500, y = 300, label = paste("Highest records: \n", rev(sort(rec.id$n))[2], "and", max(rec.id$n)))+
  # scale_x_continuous(limits = c(1,NA))+
  labs(x = "Number of records by recorder", y = "Frequency")+
  theme_pubr()


# hist- filtering out > 100
rec.id %>% 
  filter(n < 100) %>% 
  # percentile plot of frequency of each id
  ggplot(aes(x = n))+
  geom_histogram( fill = "steelblue", binwidth = 1)+
  # label the largest n
  annotate("text", x = 80, y = 300, label = paste( sum(rec.id$n>100), " highest records\nfiltered out ->"))+
  # scale_x_continuous(limits = c(1,NA))+
  labs(x = "Number of records by recorder", y = "Frequency")+
  theme_pubr()

# percentile plots ----

col.pal = c("#61B361", "#1049B3", "#B37F10", "#B3226A" )
anno.heights = seq(from = 300, to = 1200, length = 3)
n.tree.interest = c(1,2,5)

# arranged bar plot 
ggplot() +
  geom_area(data = rec.id, aes(x=percentile, y=n), fill=col.pal[1], alpha=0.4) +
  geom_line(data = rec.id, aes(x=percentile, y=n), color=col.pal[1], size=1) +
  # annotate records of 1
  geom_line(aes(x = rep(max(rec.id$percentile[rec.id$n ==n.tree.interest[1]]),2),
                  y = c(0,anno.heights[1])), color = col.pal[2])+
    annotate("text", 
           x = max(rec.id$percentile[rec.id$n ==n.tree.interest[1]])-2, y = anno.heights[1], 
           label = paste0( max(rec.id$percentile[rec.id$n ==n.tree.interest[1]]) %>% round(), "% of recorders have 1 tree"),
           hjust = 1, vjust = 0, color = col.pal[2])+
  # annotate records of 2
  geom_line(aes(x = rep(max(rec.id$percentile[rec.id$n ==n.tree.interest[2]]),2),
                y = c(0,anno.heights[2])), color = col.pal[3])+
  annotate("text", 
           x = max(rec.id$percentile[rec.id$n ==n.tree.interest[2]])-2, y = anno.heights[2], 
           label = paste0( max(rec.id$percentile[rec.id$n == n.tree.interest[2]]) %>% round(), "% of recorders have 1-",n.tree.interest[2] ," trees"),
           hjust = 1, vjust = 0, color = col.pal[3])+
  # annotate records of 5
  geom_line(aes(x = rep(max(rec.id$percentile[rec.id$n == n.tree.interest[3]]),2),
                y = c(0,anno.heights[3])), color = col.pal[4])+
  annotate("text", 
           x = max(rec.id$percentile[rec.id$n == n.tree.interest[3]])-2, y = anno.heights[3], 
           label = paste0( max(rec.id$percentile[rec.id$n == n.tree.interest[3]]) %>% round(), "% of recorders have 1-",n.tree.interest[3] ," trees"),
           hjust = 1, vjust = 0, color = col.pal[4])+
  labs(x = paste("Percentile rank of", length(rec.id$n), "recorders"), y = "Number of records")+
  theme_pubr()



#day 1
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

plotDat = aggregate(diamonds$cut, by = list(cut = diamonds$cut), 
                    FUN = length)

colnames(plotDat)[2] <- "n"


ggplot(plotDat, aes(x = cut, y = n)) +
  geom_point(aes(size = n)) +
  theme_minimal()

ggplot(mtcars, aes(x = wt, y = mpg, color = am)) + 
  geom_point() +
  theme_minimal()





mtcars$amFactor = as.factor(mtcars$am) 

ggplot(mtcars, aes(x = wt, y = mpg, color = amFactor)) + 
  geom_point() +
  theme_minimal()


diamonds %>% 
  group_by(cut) %>% 
  summarize(n = n()) %>% 
  ggplot(., aes(x = cut, y = n)) +
  geom_point(aes(size = n)) +
  theme_minimal()



mtcars %>% 
  mutate(am = as.factor(am)) %>%  
  ggplot(., aes(x = wt, y = mpg, color = am)) + 
  geom_point() +
  theme_minimal()



#Delimited Files
read.table("https://download.bls.gov/pub/time.series/ce/ce.data.42a.RetailTrade.Employment", 
           header = TRUE, sep = "\t")

read.delim("https://download.bls.gov/pub/time.series/ce/ce.data.42a.RetailTrade.Employment")

library(readr)
xx <- read.table("https://jplalor.github.io/files/sdcTestSmall.txt",
           header = T, sep = "^", quote = "", comment.char = "")


library(rvest)





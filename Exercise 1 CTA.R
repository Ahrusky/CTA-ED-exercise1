---
title: "Exercise_1_CTA"
date: "2/7/2024"
output: html_document
---
#Exercise 1: Word frequency analysis
# 7.1 Introduction
# In this tutorial, you will learn how to summarise, aggregate, and analyze text in R:
# How to tokenize and filter text
# How to clean and preprocess text
# How to visualize results with ggplot
# How to perform automated gender assignment from name data (and think about possible biases these methods may enclose)


#load packages
library(tidyverse) # loads dplyr, ggplot2, and others
library(tidytext) # includes set of functions useful for manipulating text
library(ggthemes) # includes a set of themes to make your visualizations look nice!
library(readr) # more informative and easy way to import data
library(babynames) #for gender predictions

#load data
edbfdata <- read_csv("https://raw.githubusercontent.com/cjbarrie/RDL-Ed/main/02-text-as-data/data/edbookfestall.csv")


##17.4 Inspect and filter data

colnames(edbfdata) #colum names
glimpse(edbfdata) #summary

#seleect columns from edbfdata into new data.frame
#get simplified dataset with only event contents and year
evdes <- edbfdata %>%
  select(description, year)

head(evdes)

#events over time, calc indivdual rows (events) by columns (years)
evtsperyr <- evdes %>%
  mutate(obs=1) %>%
  group_by(year) %>%
  summarise(sum_events = sum(obs))

#plot line graph ggplot
ggplot(evtsperyr) +
  geom_line(aes(year, sum_events)) +
  theme_tufte(base_family = "Helvetica") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))


##17.5 Tidy the text

#get year and word for every word and date pair in the dataset
tidy_des <- evdes %>% 
  mutate(desc = tolower(description)) %>%
  unnest_tokens(word, desc) %>%
  filter(str_detect(word, "[a-z]"))


##17.6 Back to the Fringe

#remove stop words 
tidy_des <- tidy_des %>% 
  filter(!word %in% stop_words$word)
stop_words #check what the stop words are
tidy_des %>% #find most common words in these data, apostrophe still there
  count(word, sort = TRUE)
#remove apostrophes
remove_reg <- c("&amp;","&lt;","&gt;","<p>", "</p>","&rsquo", "&lsquo;",  "&#39;", "<strong>", "</strong>", "rsquo", "em", "ndash", "nbsp", "lsquo", "strong")
tidy_des <- tidy_des %>%
  filter(!word %in% remove_reg)
tidy_des %>% #find most common word again
  count(word, sort = TRUE)
#collect these words into data.frame named edbf_term_counts
edbf_term_counts <- tidy_des %>% 
  group_by(year) %>%
  count(word, sort = TRUE)
head(edbf_term_counts) #get head 6 rows of df


##17.7 Analyze keywords

#tag words related to gender inequality or sexism
edbf_term_counts$womword <- as.integer(grepl("women|feminist|feminism|gender|harassment|sexism|sexist", 
                                             x = edbf_term_counts$word))
head(edbf_term_counts) #chech head of d.f


##17.8 Compute aggregate statistics

#get counts by year and word
edbf_counts <- edbf_term_counts %>%
  group_by(year) %>%
  mutate(year_total = sum(n)) %>%
  filter(womword==1) %>%
  summarise(sum_wom = sum(n),
            year_total= min(year_total))
head(edbf_counts)#head of d.f


##17.9 Plot time trends

#count and plot words related to gender, denoted by total number of words in data per year
ggplot(edbf_counts, aes(year, sum_wom / year_total, group=1)) +
  geom_line() +
  xlab("Year") +
  ylab("% gender-related words") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0), limits = c(0, NA)) +
  theme_tufte(base_family = "Helvetica") 
#add red line the year of #MeToo movement 2017
ggplot(edbf_counts, aes(year, sum_wom / year_total, group=1)) +
  geom_line() +
  geom_vline(xintercept = 2017, col="red") +
  geom_text(aes(x=2017.1, label="#metoo year", y=.0015), #add #metoo label
            colour="black", angle=90, text=element_text(size=8)) +
  xlab("Year") +
  ylab("% gender-related words") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0), limits = c(0, NA)) +
  theme_tufte(base_family = "Helvetica")


##17.10 Bonus: gender prediction

#to impute based on gender we make new df w/ artist name and year
# get columns for artist name and year, omitting NAs
gendes <- edbfdata %>%
  select(artist, year) %>%
  na.omit()
# generate new column with just the artist's (author's) first name
gendes$name <- sub(" .*", "", gendes$artist)
#package gender and genderdata9 (DOESNT WORK IN NEW R)
genpred <- gender(gendes$name,
                  years = c(1920, 2000))
#load in babynames package (for each year # of children born withgiven name)
babynames <- babynames
head(babynames)
#calc totql number of female babies
totals_female <- babynames %>%
  filter(sex=="F") %>%
  group_by(year) %>%
  summarise(total_female = sum(n))
#calc total number of mle babies
totals_male <- babynames %>%
  filter(sex=="M") %>%
  group_by(year) %>%
  summarise(total_male = sum(n))
#merge both back together
totals <- merge(totals_female, totals_male)
#merge by year
totsm <- merge(babynames, totals, by = "year")
head(totsm)
#calc porbability based on number of names and sexwhat % likelihood is that baby is female/male
totprops <- totsm %>%
  filter(year >= 1920) %>%
  group_by(name, year) %>%
  mutate(sumname = sum(n),
         prop = ifelse(sumname==n, 1,
                       n/sumname)) %>%
  filter(prop!=.5) %>%
  group_by(name) %>%
  slice(which.max(prop)) %>%
  summarise(prop = max(prop),
            totaln = sum(n),
            name = max(name),
            sex = unique(sex))
#check head of d.f
head(totprops)
#merge proportions of names back with names of artists from fringe festival 
ednameprops <- merge(totprops, gendes, by = "name")
#plot male vs female
ggplot(ednameprops, aes(x=year, fill = factor(sex))) +
  geom_bar(position = "fill") +
  xlab("Year") +
  ylab("% women authors") +
  labs(fill="") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_tufte(base_family = "Helvetica") +
  geom_abline(slope=0, intercept=0.5,  col = "black",lty=2)
#eamine data that had to be cut bc it didnt match babynames
names1 <- ednameprops$name
names2 <- gendes$name
diffs <- setdiff(names2, names1)
diffs #bias looks like a lot of them are internatinal, babynames has a western bias likely


##17.11 Exercises

# Filter the books by genre (selecting e.g., “Literature” or “Children”) and plot frequency of women-related words over time.
selected_genres <- edbfdata %>%
  filter(genre %in% c("Literature", "Children"))
tidy_genres <- selected_genres %>%
  select(description, year, genre) %>%
  mutate(description = tolower(description)) %>%
  unnest_tokens(word, description) %>%
  filter(!word %in% stop_words$word)
#find women-related words
women_related_words <- c("women", "woman", "female", "feminism", "feminist", "gender")
#frequency of women-related words over time
women_word_counts <- tidy_genres %>%
  filter(word %in% women_related_words) %>%
  count(year, genre, name = "frequency") %>%
  group_by(year, genre) %>%
  summarise(total_frequency = sum(frequency))
#plot frequency over time
ggplot(women_word_counts, aes(x = year, y = total_frequency, color = genre)) +
  geom_line() +
  labs(title = "Frequency of Women-Related Words Over Time by Genre",
       x = "Year",
       y = "Frequency of Women-Related Words",
       color = "Genre") +
  theme_minimal()


#Choose another set of terms by which to filter (e.g., race-related words) and plot their frequency over time.

#race-related words
race_related_words <- c("race", "racism", "racial", "ethnicity", "diversity", "inclusion")
#frequency of race-related words over time
race_word_counts <- tidy_genres %>%
  filter(word %in% race_related_words) %>%
  count(year, genre, name = "frequency") %>%
  group_by(year, genre) %>%
  summarise(total_frequency = sum(frequency))
#plot the frequency over time
ggplot(race_word_counts, aes(x = year, y = total_frequency, color = genre)) +
  geom_line() +
  labs(title = "Frequency of Race-Related Words Over Time by Genre",
       x = "Year",
       y = "Frequency of Race-Related Words",
       color = "Genre") +
  theme_minimal()

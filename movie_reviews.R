# Download the ml-latest-small.zip file from "recommended for education and development" under MovieLens Latest Datasets (https://grouplens.org/datasets/movielens/)
# Tidyverse style guide (https://style.tidyverse.org/)
# How to use dplyr (https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html)
# How to do mixed modeling (http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf)

# Keyboard shortcuts
# Cmd + enter: Run the line of code the cursor is in or the highlighted code
# Cmd + shift + c: Comment out selected code
# Cmd + shift + n: Create new file
# Cmd + shift + m: Create a dplyr pipe

# Load libraries
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # plotting
library(lme4) # mixed models

wkdir = "~/Documents/projects/ds_bookclub/R_tutorials/movie_reviews/ml-latest-small/"

# Read in data
links = read.csv(paste(wkdir, "links.csv", sep=""))
movies = read.csv(paste(wkdir, "movies.csv", sep=""))
ratings = read.csv(paste(wkdir, "ratings.csv", sep=""))
tags = read.csv(paste(wkdir, "tags.csv", sep=""))

# Preview data
links %>% head()
movies %>% head()
ratings %>% head()
tags %>% head()

# Number of rows
links %>% nrow()
movies %>% nrow()
ratings %>% nrow()
tags %>% nrow()

# Merge data
all_data = links %>% 
  merge(movies) %>% 
  merge(ratings) %>% 
  select(-timestamp)

# How many # of ratings per user? Looks like 1 per user
all_data %>% 
  group_by(userId, movieId) %>% 
  count() %>% 
  group_by(n) %>% 
  count()

# An easier way to check... still 1 per user
all_data %>% nrow()
all_data %>% select(userId, movieId) %>% unique() %>% nrow()

# Create a column per genre
all_data2 = all_data %>% 
  mutate(Action = ifelse(grepl("Action", genres), 1, 0),
         Adventure = ifelse(grepl("Adventure", genres), 1, 0),
         Animation = ifelse(grepl("Animation", genres), 1, 0),
         Children = ifelse(grepl("Children", genres), 1, 0),
         Crime = ifelse(grepl("Crime", genres), 1, 0),
         Comedy = ifelse(grepl("Comedy", genres), 1, 0),
         Documentary = ifelse(grepl("Documentary", genres), 1, 0),
         Drama = ifelse(grepl("Drama", genres), 1, 0),
         Fantasy = ifelse(grepl("Fantasy", genres), 1, 0),
         Film_Noir = ifelse(grepl("Film-Noir", genres), 1, 0),
         Horror = ifelse(grepl("Horror", genres), 1, 0),
         IMAX = ifelse(grepl("IMAX", genres), 1, 0),
         Musical = ifelse(grepl("Musical", genres), 1, 0),
         Musical_Documentary = ifelse(grepl("Musical Documentary", genres), 1, 0),
         Mystery = ifelse(grepl("Mystery", genres), 1, 0),
         Romance = ifelse(grepl("Romance", genres), 1, 0),
         Sci_Fi = ifelse(grepl("Sci-Fi", genres), 1, 0),
         Thriller = ifelse(grepl("Thriller", genres), 1, 0),
         War = ifelse(grepl("War", genres), 1, 0),
         Western = ifelse(grepl("Western", genres), 1, 0),
         none = ifelse(grepl("(no genres listed)", genres), 1, 0))

# Count number of categories per movie
all_data2$total_categorized = rowSums(all_data2[,c(8:length(names(all_data2)))])

# Check to see if we got all the genres
# Test out removing some of the above columns and see what shows up below
all_data2 %>% filter(total_categorized == 0) %>% .$genres %>% unique() %>% head()

# Check distribution of ratings, max and min
# Use this to guide transformation of scale to 0-100
quantile(all_data2$rating)
min(all_data2$rating)
max(all_data2$rating)

# More data cleaning
# Transform data from wide to long and remove missing rows
# Transform rating from 0-5 scale to 0-100
# Extract year from title
all_data3 = all_data2 %>% 
  gather(genre, type, Action:none) %>% 
  filter(type == 1) %>% 
  mutate(rating2 = rating*20) %>% 
  mutate(year = case_when(grepl("2006–2007", title) ~ "2006",
                          grepl("\\(", title) ~ gsub(".*\\(|\\)|[a-z]|[A-Z]| |:", "", title)),
         year = as.integer(year)) %>% 
  select(-genres, -type)

# Preview most number of movies per genre per year
all_data3 %>% group_by(year, genre) %>% count() %>% arrange(desc(n)) %>% head()

# Average rating by genre
avg_ratings = all_data3 %>% 
  group_by(genre) %>% 
  summarise(n = n(),
            avg_rating2 = mean(rating2),
            sd_rating2 = sd(rating2),
            se_rating2 = sd_rating2 / sqrt(n))

# Plot average rating and error bars per genre
# Click Zoom to adjust view of the plot
ggplot(data = avg_ratings,
       aes(x = genre,
           y = avg_rating2,
           ymin = avg_rating2 - se_rating2,
           ymax = avg_rating2 + se_rating2,
           fill = genre)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width=.2) +
  geom_text(aes(label = round(avg_rating2, 1),
                y = avg_rating2 - 5)) +
  geom_text(aes(label = n,
                y = 2)) +
  ylab("Average rating") +
  theme_bw() +
  theme(legend.position = "none")

# Does the rating increase as the number of categories assigned to the movie increase?
model = lm(rating2 ~ total_categorized, data = all_data3)
summary(model)

# Scale (mean center) the variables to standardize the coefficients
scaled_model = lm(scale(rating2) ~ scale(total_categorized), data = all_data3)
summary(scaled_model)

# Scatterplot of number categorized by rating
ggplot(data = all_data3,
       aes(x = total_categorized,
           y = rating2)) +
  geom_point() +
  theme_bw()

# Scale (mean center) the variables to standardize the coefficients
scaled_model2 = lm(scale(rating2) ~ scale(total_categorized), data = all_data3 %>% filter(total_categorized <= 7))
summary(scaled_model2)

# Scatterplot of number categorized by rating removing outliers
ggplot(data = all_data3 %>% filter(total_categorized <= 7),
       aes(x = total_categorized,
           y = rating2)) +
  geom_jitter() +
  theme_bw()

# Scatterplot of number categorized by rating removing outliers
ggplot(data = all_data3 %>% filter(total_categorized <= 7),
       aes(x = total_categorized,
           y = rating2)) +
  geom_jitter(aes(colour = genre)) +
  theme_bw()

# Average rating per year and genre
avg_rating_year = all_data3 %>% 
  group_by(year, genre) %>% 
  summarise(n = n(),
            avg_rating2 = mean(rating2),
            sd_rating2 = sd(rating2),
            se_rating2 = sd_rating2 / sqrt(n))

# Number of movies per genre per year
ggplot(data = avg_rating_year,
       aes(x = year,
           y = n,
           group = genre,
           colour = genre)) +
  geom_line() +
  theme_bw()

# Do ratings trends change across time?
year_model = lm(rating2 ~ year, data = all_data3)
summary(year_model)

# Do ratings trends change across time? (mean centered)
scaled_year_model = lm(scale(rating2) ~ scale(year), data = all_data3)
summary(scaled_year_model)

# Do ratings trends change across time (controlling for genre as a random factor)
mm_scaled_year_model = lmer(scale(rating2) ~ scale(year) + (1 | genre), data = all_data3)
summary(mm_scaled_year_model)

# Plot rating by year by genre
ggplot(data = all_data3,
       aes(x = year,
           y = rating2,
           colour = title)) +
  geom_point() +
  facet_wrap(~genre) +
  theme_bw() +
  theme(legend.position = "none")


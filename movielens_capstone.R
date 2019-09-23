################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# START CAPSTONE
# Exploring datasets

#Q1
##How many rows and columns are there in the edx dataset?
cat("Number of Rows:", dim(edx)[1])
cat("Number of Columns:", dim(edx)[2])

#Q2
#How many zeros were given as ratings in the edx dataset?
cat("There are", sum(edx$rating==0), "zeroes" )

#How many threes were given as ratings in the edx dataset?
cat("There are", sum(edx$rating==3), "threes" )

#Q3
#How many different movies are in the edx dataset?
cat("There are", n_distinct(edx$movieId), "different movies" )

#Q4
#How many different users are in the edx dataset?
cat("There are", n_distinct(edx$userId), "different users" )


#Q5
#How many movie ratings are in each of the following genres in the edx dataset?
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


#Q6
#Which movie has the greatest number of ratings?
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Q7
#What are the five most given ratings in order from most to least?

edx %>% group_by(rating) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  top_n(5, n)


edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
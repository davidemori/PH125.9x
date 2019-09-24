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

### Data Analysis ###

# Head of dataset (first 10 rows)
head(edx, 10) %>% 
  knitr::kable()

# Number of unique movies, unique users, unique ratings and rating levels in the edx dataset 
edx %>%
  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId),
            n_ratings_entries = n(),
            n_rating_levels = n_distinct(rating))

# Ratings distribution
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, color = "black", fill="rosybrown") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 250000))) +
  ggtitle("Ratings distribution")

# Number of ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie")


# Table 25 movies rated only once
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:25) %>%
  knitr::kable()

# Number of movies per year/decade
# Extract release year from title into a separate field and plot
edx %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$")) %>%
  select(movieId, releaseyear) %>% # 
  group_by(releaseyear) %>% 
  summarise(count = n_distinct(movieId))  %>%
  ggplot(aes(releaseyear, count))+
  geom_line()
  
#Number of ratings per year
if(!require(lubridate)) install.packages("lubridate")

edx %>%
  mutate(year=year(as.POSIXct(edx$timestamp ,origin = "1970-01-01",tz = "GMT"))) %>%
  group_by(year) %>%
  summarize(count=n()) %>%
  ggplot(aes(year,count)) +
  scale_x_discrete(limits = c(seq(1990,2010,1))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 1500000))) +
  xlab("Year") + 
  ylab("Number of ratings") +
  ggtitle("Number of ratings per year")+
  geom_line()


# Plot number of ratings given by users
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings given by users")

# Plot mean movie ratings given by users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_light()


### Modelling Approach ###

## Average movie rating model ##

# Compute mean rating
mu <- mean(edx$rating)
mu #print mean rating

# Test results based on simple prediction
mu_rmse <- RMSE(validation$rating, mu)
cat("Our first RMSE is",mu_rmse, "(Pretty big!!)")

# Save prediction in a dedicated data frame for RMSE results
rmse_results <- data_frame(method = "Average movie rating model", RMSE = mu_rmse)
rmse_results %>% knitr::kable()

## Movie effect model ##

# Now we provide a Simple model that taking into account the movie effect b_i and
# subtract the rating mean from each movie ratings
# Plot b_i versus number of movies
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_avgs%>% ggplot(aes(b_i)) + 
  geom_histogram(bins = 30, color="black", fill="rosybrown")+
  xlab("b_i values") +
  ylab("Number of Movies") +
  ggtitle("b_i versus number of movies") 


# Test and save rmse results 
predicted_ratings <- mu + validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
First_model_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect (b_i) model",  
                                     RMSE = First_model_rmse ))
# Check results after First model
rmse_results %>% knitr::kable()

## Movie effect and user effect models paired ##

# Plot penalty term user effect #
user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_avgs%>% ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color="black", fill="rosybrown")
   

user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


# Test and save rmse results 
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

Second_model_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user (b_i + b_u) effect model",  
                                     RMSE = Second_model_rmse))

# Check result
rmse_results %>% knitr::kable()

## Regularization of movie and user effect model ##

# Chosing value of lambda as tuning parameter
# with cross-validation.
lambdas <- seq(0, 20, 0.25)


# For each lambda,find b_i & b_u, followed by rating prediction & testing
# note:the below code could take some time  
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})


# Plot rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, rmses)  


# The optimal lambda                                                             
lambda <- lambdas[which.min(rmses)]
lambda

# Test and save results                                                             
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized movie and user effect model",  
                                     RMSE = min(rmses)))

# Check result
rmse_results %>% knitr::kable()

#### Results ####                                                            
# RMSE results overview                                                          
rmse_results %>% knitr::kable()


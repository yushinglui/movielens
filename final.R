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

# RMSE function
RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((predicted_ratings - true_ratings)^2))
}

#The average of all ratings model
mu_hat <- mean(edx$rating)
mu_hat

# Predicting all unknown ratings with Î¼Ë† we obtain the following RMSE
naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse

# Creating a results table with this naive approach
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results

# Movie effects model
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("grey"),
                     ylab = "counts", main = "Number of movies")

# Rredict result
predicted_ratings <- mu_hat + validation %>% 
  left_join(movie_avgs, by='movieId') %>% pull(b_i)

RMSE(predicted_ratings, validation$rating)

# User effect model
edx %>% group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>% ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "grey")

# The user averages
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

# Constructing predictors and see how much the RMSE improves
predicted_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  mutate(pred = mu_hat + b_i + b_u) %>% pull(pred)

RMSE(predicted_ratings, validation$rating)
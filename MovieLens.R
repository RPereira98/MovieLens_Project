##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

##################
#Analisis of Data#
##################
library(ggplot2)
#Number of ratings per movie
edx%>%count(movieId)%>%ggplot(aes(n))+geom_histogram(color="black",fill="#FFFFCC")+
  scale_x_log10()+ggtitle("Number of ratings per movie")+
  theme(plot.title = element_text(hjust = 0.5))
#Some movies are rated more than others

#Number of ratings per user
edx%>%count(userId)%>%ggplot(aes(n))+geom_histogram(color="black",fill="#CCFFCC")+
  scale_x_log10()+ggtitle("Number of ratings per user")+
  theme(plot.title = element_text(hjust = 0.5))
#Some users rate movies more than others

##################################
#STAR OF THE PREDICTION ALGORITHM#
##################################
#Training
#Creating subsets from edx data set
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
temp_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train<-edx[-temp_index]
edx_test<-edx[temp_index]
#Removing users and movies of the test set not present in the training set
edx_test<-edx_test%>%semi_join(edx_train,by="userId")%>%
  semi_join(edx_train,by="movieId")

#Using only Movie Average
avg<-mean(edx_train$rating)
rmse_avg<-RMSE(validation$rating,avg)
rmsedata<-data.frame(Method= "Simple Average",RMSE=rmse_avg)
#RMSE=1.061, not the desired value

#Calculating the effect of the movies
mu<-mean(edx_train$rating)
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
#Predicting with the movie effect
test_bi<-edx_test%>%left_join(movie_avgs,by="movieId")%>%pull(b_i)
pred_me<-mu+test_bi
rmse_me<-RMSE(edx_test$rating,pred_me)
rmsedata<-bind_rows(rmsedata,data.frame(Method="Movie Effect",RMSE=rmse_me))
#RMSE=0.942

#Calculating the effect of the users
user_avgs <- edx_train %>% left_join(movie_avgs,by="movieId")%>%
  group_by(userId) %>% 
  summarize(u_i = mean(rating - mu-b_i))
#Predicting with the user effect
test_ui<-edx_test%>%left_join(user_avgs,by="userId")%>%pull(u_i)
pred_ue<-mu+test_bi+test_ui
rmse_ue<-RMSE(edx_test$rating,pred_ue)
rmsedata<-bind_rows(rmsedata,data.frame(Method="Movie+User Effect",RMSE=rmse_ue))
#RMSE=0.8646

#Calculating the effect of the genres
genre_avgs <- edx_train %>% left_join(movie_avgs,by="movieId")%>%left_join(user_avgs,by="userId")%>%
  group_by(genres) %>% 
  summarize(g_i = mean(rating - mu-b_i-u_i))
#Predicting with the genre effect
test_gi<-edx_test%>%left_join(genre_avgs,by="genres")%>%pull(g_i)
pred_ge<-mu+test_bi+test_ui+test_gi
rmse_ge<-RMSE(edx_test$rating,pred_ge)
rmsedata<-bind_rows(rmsedata,data.frame(Method="Movie+User+Genre Effect",RMSE=rmse_ge))
#RMSE=0.8643

#Testing with the validation set
pred_v<-validation%>%left_join(movie_avgs,by="movieId")%>%left_join(user_avgs,by="userId")%>%
  left_join(genre_avgs,by="genres")%>%mutate(pred=mu+b_i+u_i+g_i)%>%pull(pred)
pred_v[is.na(pred_v)]<-mu #Correcting NAs
pred_v[pred_v>5]<-5#Correcting superior values than the maximun rating(5)
pred_v[pred_v<0.5]<-0.5#Correcting inferior values than the minimun rating(0.5)
rmse_v<-RMSE(validation$rating,pred_v)
rmsedata<-bind_rows(rmsedata,data.frame(Method="Validation RMSE",RMSE=rmse_v))
#FINAL RMSE=0.8652, not desired

#Using regularization to improve RMSE
#Calculating the effect of the movies REGULARIZATED
#Finding lambda
lambdas <- seq(0, 10, 0.25)
only_sum <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  pred <-edx_test  %>% 
    left_join(only_sum, by='movieId') %>% 
    mutate(b_ir = s/(n_i+l)) %>%
    mutate(pred = mu + b_ir) %>%
    pull(pred)
  return(RMSE(pred, edx_test$rating))
})
lambda<-lambdas[which.min(rmses)]#Find the best lambda
#best lambda is 1.5
#Predicting with the movie effect regularized
movie_reg_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_ir = sum(rating - mu)/(n()+lambda), n_i = n()) 
test_bir<-edx_test%>%left_join(movie_reg_avgs,by="movieId")%>%pull(b_ir)
pred_mer<-mu+test_bir+test_ui+test_gi
rmse_mer<-RMSE(edx_test$rating,pred_mer)
rmsedata<-bind_rows(rmsedata,data.frame(Method="Movie Reg+User+Genre",RMSE=rmse_mer))
#RMSE=0.8642

#To see how the estimates shrink
tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_ir, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)
ggtitle("Movie Effect",subtitle = "Original VS Regularized")+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle =element_text(hjust = 0.5))

#Calculating the effect of the users regularized
lambdas <- seq(0, 10, 0.25)
only_sumu <- edx_train  %>%left_join(movie_reg_avgs,by="movieId")%>%
  group_by(userId) %>% 
  summarize(s2 = sum(rating - mu-b_ir), n_i2 = n())
rmses <- sapply(lambdas, function(l){
  pred <-edx_test  %>% left_join(movie_reg_avgs,by="movieId")%>%
    left_join(only_sumu, by='userId') %>% 
    mutate(u_ir = s2/(n_i2+l)) %>%
    mutate(pred = mu + b_ir+u_ir) %>%
    pull(pred)
  return(RMSE(pred, edx_test$rating))
})
lambda<-lambdas[which.min(rmses)]#Find the best lambda
#best lambda is 5
#Predicting with the user effect regularized
user_reg_avgs <- edx_train %>% left_join(movie_reg_avgs,by="movieId")%>%
  group_by(userId) %>% 
  summarize(u_ir = sum(rating - mu-b_ir)/(n()+lambda), n_i = n()) 
test_uir<-edx_test%>%left_join(user_reg_avgs,by="userId")%>%pull(u_ir)
pred_uer<-mu+test_bir+test_uir+test_gi
rmse_uer<-RMSE(edx_test$rating,pred_uer)
rmsedata<-bind_rows(rmsedata,data.frame(Method="Movie Reg+User Reg+Genre",RMSE=rmse_uer))
#RMSE=0.8638
#To see how the estimates shrink
tibble(original = user_avgs$u_i, 
       regularlized = user_reg_avgs$u_ir, 
       n = user_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)
ggtitle("User Effect",subtitle = "Original VS Regularized")+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle =element_text(hjust = 0.5))

#Calculating the effect of the genres regularized
lambdas <- seq(0, 10, 0.25)
only_sumg <- edx_train  %>%left_join(movie_reg_avgs,by="movieId")%>%
  left_join(user_reg_avgs,by="userId")%>%
  group_by(genres) %>% 
  summarize(s3 = sum(rating - mu-b_ir-u_ir), n_i3 = n())
rmses <- sapply(lambdas, function(l){
  pred <-edx_test  %>% left_join(movie_reg_avgs,by="movieId")%>%
    left_join(user_reg_avgs,by="userId")%>%
    left_join(only_sumg, by='genres') %>% 
    mutate(g_ir = s3/(n_i3+l)) %>%
    mutate(pred = mu + b_ir+u_ir+g_ir) %>%
    pull(pred)
  return(RMSE(pred, edx_test$rating))
})
lambda<-lambdas[which.min(rmses)]#Find the best lambda
#best lambda is 0 
#Predicting with the genre effect regularized
genre_reg_avgs <- edx_train %>% left_join(movie_reg_avgs,by="movieId")%>%
  left_join(user_reg_avgs,by="userId")%>%
  group_by(genres) %>% 
  summarize(g_ir = sum(rating - mu-b_ir-u_ir)/(n()+lambda), n_i = n()) 
test_gir<-edx_test%>%left_join(genre_reg_avgs,by="genres")%>%pull(g_ir)
pred_ger<-mu+test_bir+test_uir+test_gir
rmse_ger<-RMSE(edx_test$rating,pred_ger)
rmsedata<-bind_rows(rmsedata,data.frame(Method="Movie Reg+User Reg+Genre Reg",RMSE=rmse_ger))
#RMSE=0.8638, very  small change

#To see how the estimates shrink
tibble(original = genre_avgs$g_i, 
       regularlized = genre_reg_avgs$g_ir, 
       n = genre_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)+  
  ggtitle("Genre Effect",subtitle = "Original VS Regularized")+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle =element_text(hjust = 0.5) )

#Testing with the validation set
pred_vr<-validation%>%left_join(movie_reg_avgs,by="movieId")%>%left_join(user_reg_avgs,by="userId")%>%
  left_join(genre_reg_avgs,by="genres")%>%mutate(pred=mu+b_ir+u_ir+g_ir)%>%pull(pred)
pred_vr[is.na(pred_vr)]<-mu#Correcting NAs
pred_vr[pred_vr>5]<-5#Correcting superior values than the maximun rating(5)
pred_vr[pred_vr<0.5]<-0.5#Correcting inferior values than the minimun rating(0.5)
rmse_vr<-RMSE(validation$rating,pred_vr)
rmsedata<-bind_rows(rmsedata,data.frame(Method="Validation Reg RMSE",RMSE=rmse_vr))
#FINAL RMSE=0.8648047
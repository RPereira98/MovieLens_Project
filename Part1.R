#Training
#Using only Movie Average
avg<-mean(edx$rating)
rmse_avg<-RMSE(validation$rating,avg)
rmsedata<-data.frame(Method= "Simple Average",RMSE=rmse_avg)
#RMSE=1.061, not the desired value
#Creating subsets from edx data set
set.seed(1998, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
temp_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train<-edx[-temp_index]
edx_test<-edx[temp_index]
#Removing users and movies of the test not present in the training set
edx_test<-edx_test%>%semi_join(edx_train,by="userId")%>%
  semi_join(edx_train,by="movieId")
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
#RMSE=0.942,not the desired value
#Calculating the effect of the users







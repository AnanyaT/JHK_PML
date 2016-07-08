
library(ggplot2)
#mtcars dataset
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear) 
mtcars$carb <- as.factor(mtcars$carb)

d <- as.data.frame(mtcars)                 #dataframe
str(d)
rownames(d) <- 1:nrow(d)                   #changing rownames to match ObsID

flt <- as.FLTable(d)                       
head(flt)
class(flt$am)

t<- NULL
tr<-glm(am ~ mpg, data=d,family="binomial",x=TRUE,y=TRUE)
t<-lm(am ~ mpg ,data=flt)
head(flt)
Fl_res <- t$residuals
pre <- predict(t)


FL_pre <- as.vector(pre)
R_pre <- predict(tr) 
FL_coeff <- t$coefficients
R_coeff <- t$coefficients

#check using coefficients
Rpre_manual <- R_coeff[1]+R_coeff[2]*mtcars$mpg
FL_pre_manual <- FL_coeff[1] + FL_coeff[2]*flt$mpg  

#check for residuals
score <- exp(R_pre)/(1+exp(R_pre))
dev_resid <- sqrt(-2*(d$am*(log(score))+(1-d$am)*log(1-score))) #deviance
pear_resid <- ((d$am - score)/(sqrt(score*(1-score)))) #pearson residuals
simple_resid <- d$am - FL_pre    #shown by FL t.residuals
work_resid <- (((d$am==1)*1)-score)/(score*(1-score))
tr$residuals
residuals(tr,type="working")                #used in R
# err_score <- abs(d$am - score)
# R_resid <- log(err_score/(1-err_score))
# Fl_resid <- t$residuals





tr<-glm(am ~ mpg, data=d,family="binomial",x=TRUE,y=TRUE)
t<-glm(am ~ mpg , family="binomial",data=flt)
pre <- predict(t)

##equality of residuals (working residuals in R)
score <- FL_pre                            #probability value given by FL
#flt$am-score giving error
work_resid <- (flt$am-score)/(score*(1-score))
FLexpect_equal(Fl_res,as.vector(tr$residuals), check.attributes = FALSE, tolerance = 1e-7) #complete


## equality of influence factors
#sigma
tr_sigma <- glm(am~mpg, data=mtcars)
sigma(tr_sigma)
sigma(tr)
#influence
inf_FL <- influence(t)
inf_R <-influence(tr)
influence(tr)$sigma
?sigma
#head(data.frame(dfbeta(tr), inf_R$coefficients)) #To check coefficients in influence and dfbeta is equal

?influence
#influence factors
influence(tr)
data.frame(dfbetas(tr),influence(tr)$coefficients)





#NEW




library(bit64)
library(ggplot2)
library(caret)
library(data.table) #reading in the data
library(dplyr) #dataframe manipulation
library(ranger) #the random forest implementation
library(plotly) #3D plotting
library(tidyr) #dataframe manipulation
library(FNN) #k nearest neighbors algorithm
library(xgboost)
setwd("C:\\Users\\admin\\Downloads\\fb")

r <- as.data.frame(fread("new_train.csv")) #NEW DATASET CREATED

#test <- as.data.frame(fread("test.csv"))

#r$accuracySum <- (r$accuracySum - mean(r$accuracySum))/sd(r$accuracySum)

# t_hour <- (test$time)/60
# test$hour <- ceiling(t_hour%%24)
# test$day <- ceiling((t_hour/24))
# test$accuracy <- (test$accuracy - mean(test$accuracy))/sd(test$accuracy)
# read <- r[,-c(1,3)]
# #head(test_final)
# test_final <- test[,-c(1,4)]
# names(test_final) <- c("X","Y","accuracySum","hour","day")
# #head(read)
# #head(r)
# #check <- knn(train=r[,-c(1,3)],test = test[,-c(1,5)],cl=place_id,k=5,prob = TRUE)
# cl <- as.factor(r$place_id)
# ch <- FNN::knn(train = read, test =  test_final, cl = r$place_id, k = 5)    #just using coordinates 
# 
# pred <- ch
# viz <- data.frame(test_final,pred)


#checking on train function
train_set <- fread("train.csv")
train <- as.data.frame(train_set)

#train_hour <- as.numeric((train$time)/60)
hour <- (train$NULLtime/3600) %%(24)

train$hour <- ceiling(hour)

train$day <- ceiling((hour/24))
train$accuracy <- (train$accuracy - mean(train$accuracy))/sd(train$accuracy)

train_test <- train[,-c(1,5)]

#train_test <- train_test[sample(train_test$x,100),]
train_sample0 <- data.frame(train_test$accuracy,train_test$x,train_test$y,train_test$day,train_test$hour,train_test$place_id)
names(train_sample0)<- c("accuracySum","X","Y","day","hour","place_id")

train_sample <- train_sample0[sample(train_sample0$X,1000),]

#check_train <- knn(train = train_sample0[,-6], test = train_sample[,-6], cl = as.factor(train_sample0$place_id), k = 10)    
#check_train_dist <- knn(train = train_sample0[,-c(6,5,4,1)], test = train_sample[,-c(6,5,4,1)], cl = as.factor(train_sample0$place_id), k = 10)    #just using coordinates 
#check_train_dist <- knn(train = train_sample0[,c(3,5)], test = train_sample[,c(3,5)], cl = as.factor(train_sample0$place_id), k = 10)    #just using coordinates
check_train_distXY <- knn(train = train_sample0[,c(2,3)], test = train_sample[,c(2,3)], cl = as.factor(train_sample0$place_id), k = 10)    #just using coordinates  
mean(as.character(check_train_distXY) == train_sample$place_id)
# ACCURACY ABOVE 0.4 IN EVERY SAMPLE TAKEN
#check_train_distXYA <- knn(train = train_sample0[,c(1,2,3)], test = train_sample[,c(1,2,3)], cl = as.factor(train_sample0$place_id), k = 10)    #just using coordinates and acc



mean(as.character(check_train_distXY) == (train_sample$place_id)) #Accuracy relatively high 0.43 for XY 


#range(r$frequency)
r_ref <- r[(r$frequency>250),]      #DEPENDING ON THIS STEP, THE PLACE_IDS TO BE CONSIDERED ARE TAKEN
#sum(r_ref$frequency)               #check to see the number of rows considered
train_plid <- train[(as.character(train$place_id) %in% as.character(r_ref$place_id)),]


makeframe <- function(train_plid){
  train_sample1 <- data.frame(train_plid$accuracy,train_plid$x,train_plid$y,train_plid$day,train_plid$hour,train_plid$place_id)
  names(train_sample1)<- c("accuracySum","X","Y","day","hour","place_id")
  
  train_sample_1 <- train_sample1[sample(train_sample0$X,10000),]
  
  #check_train <- knn(train = train_sample0[,-6], test = train_sample[,-6], cl = as.factor(train_sample0$place_id), k = 10)    
  #check_train_dist <- knn(train = train_sample0[,-c(6,5,4,1)], test = train_sample[,-c(6,5,4,1)], cl = as.factor(train_sample0$place_id), k = 10)    #just using coordinates 
  #check_train_dist <- knn(train = train_sample0[,c(3,5)], test = train_sample[,c(3,5)], cl = as.factor(train_sample0$place_id), k = 10)    #just using coordinates
  check_train_distXY1 <- knn(train = train_sample1[,c(2,3)], test = train_sample_1[,c(2,3)], cl = as.factor(train_sample1$place_id), k = 5)    #just using coordinates   
  mean(as.character(check_train_distXY1) == train_sample_1$place_id)
}

model_percent <- makeframe(train_plid) #accuracy of 0.55  at frequency > 250


#TIME 
r$hour <- ceiling(r$hour)
r$day <-  ceiling(r$day)

time_hour <- aggregate(frequency ~ hour, data=r,sum)
time_day <- aggregate(frequency ~ day, data=r,sum)
#names(time_hour)

g1 <- ggplot(time_hour,aes(hour,frequency))+geom_point()+ labs(x="hour",y="frequency")

#names(time_hour)
g2 <- ggplot(time_day,aes(day,frequency))+geom_point()+labs(x="day",y="frequency")

grid.arrange(g1, g2,nrow =2, top = "Time and frequency")

#GIVING WEIGHT TAKING HOUR AND DAY INTO CONSIDERATION AFTER SEEING THE ABOVE PLOT
time_hour$w_hour <- 365/((time_hour$frequency)/max(time_hour$frequency))  #HOURS TO EACH DAY
time_day$w_day <-  1/(((time_day$frequency)/max(time_day$frequency)))    

#NOW weighted dataset created

train$day <- ceiling(train$day)
train$hour <- ceiling(train$hour)
train_weight_day <- train$day * time_day[train$day,]$w_day
train_weight_hour <- train$hour * time_hour[train$hour,]$w_hour






#TAKES TOO LONG
# xdiff <- NULL
# ydiff <- NULL
# dist <- data.frame()
# place_index <- NULL
# 
# for(i in seq(along= 1:100)){
#   for (j in seq(along=1:length(r$X))){
#     xdiff <- train_test[i,]$X - r[j,]$X
#     ydiff <- train_test[i,]$Y - r[j,]$Y
#     dist[i,j] <- (xdiff^2 + ydiff^2)
#   }
# }
# train_test$place_pred_id <- r[place_index,]$place_id














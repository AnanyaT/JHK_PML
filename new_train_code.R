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
read <- fread("train.csv",sep=",",integer64 = "character")
r <- as.data.frame(read)

#head(r)


#r$place_id <- as.factor(r$place_id)
#fit_acc <- lm(accuracy ~ x+y, data=r)

#data_sample$place_id <- as.factor(data_sample$place_id)

#ggplot(data_sample,aes(x=place_id,y=accuracy))+geom_point(aes(color=time))
#----------sampling disperses the data......difficult to see clusters
data<- r[(r$x<2 & r$y<2),]
# 
# library(caret) 
# head(data[,-c(6,1)])
# 
# pre <- preProcess(data[,-c(6,1)],method="pca")
# pre_data <- predict(pre,data[,-c(6,1)])

#head(pre_data)
data$t_hour <- (data$time)/60
data$d_hour <- ceiling(data$t_hour%%24)
data$day <- ceiling((data$t_hour/24))
#length((data$place_id))
sp <- split(data,data$place_id)
#range(sp[[1]]$x)
#range(sp[[1]]$y)
#length(data$x)
#head(data)


#sp_pl <- lapply(sp,function(x){(length(sp$x)>5)==TRUE})
sp_len <- lapply(sp,function(sp){length(sp$x)})

sp_rel <- sp[sp_len>1400]   #Selecting the top popular place ids

sp_plot <- do.call(rbind,sp_rel)

#plot features 3d
plot_ly(data = sp_plot, x = x , y = y, z = accuracy, color = d_hour, type = "scatter3d", mode = "markers", marker=list(size= 5)) 

#
data$place_id <- as.factor(data$place_id)
mdl <- train(place_id ~.,method="rf", data=data[,-c(5,7,1,9)])
lapply(data[1,],class)

confusionMatrix(predict(mdl),sp_plot$place_id)

#head(sp_plot)
#head(data)
data$place_id <- as.factor(data$p
                           lace_id)
pred_data <- predict(mdl,data)


sum((pred_data == data$place_id)*1)/length(data$place_id)

#test for different place ids....gives peaks at diferent hours of the day
ggplot(sp_rel[[9]],aes(x=d_hour,y=accuracy))+geom_line()

#Accuracy with time of day
ggplot(data,aes(x=d_hour,y=accuracy))+geom_line()

g <- ggplot(data, aes(y,accuracy))+geom_point() #accuracy and y relationship 3 groups prominent when sampling of 100000 done

ggplot(data, aes(x,accuracy))+geom_point()   #accuracy and y relationship 3 groups prominent when sampling of 100000 done

#ggplot(data, aes(x,y))+geom_point(aes(color=accuracy))

#length(unique(data$day))


#AGGREGATE INTO DAY / HOUR
# ag_data <- aggregate(accuracy~d_hour+day,data=data,sum)
# 
# ag_hr <-  aggregate(accuracy~d_hour,data=data,sum)
# 
# names(ag_data)
# 
# km_acc <- kmeans(ag_data$accuracy,10)
# 
# #ggplot(ag_data,aes(x=day,y=accuracy))+geom_line(aes(color=d_hour)) 
#____________ABOVE GRAPH SHOWS THAT ACCURACY PARAMETER MIGHT BE TIME DEPENDENT AS VALUE INCREASES AT SPECIFIC INSTANCES
# #ggplot(ag_hr,aes(x=d_hour,y=accuracy))+geom_line()+geom_smooth()
# 
# ag_data$cluster <- km_acc$cluster
# ag_center <- km_acc$centers
# 
# #head(ag_data)
# 
# ggplot(ag_data,aes(day,accuracy,color=cluster))+geom_point()
# 
# data$place_id <- as.factor(data$place_id)
# d_f <- data.frame(data$x,data$y,data$accuracy,data$time, data$place_id)
# fit  <-lm(data.place_id ~. ,data=d_f)
# #head(d_f)
# #lapply(r[1,],class)
# 
# km <- kmeans(data$time,100)
# center <- as.data.frame(km$centers)
# data$cluster <- km$cluster
# 
# ggplot(r,aes(x,y,color=cluster))+geom_point()+geom_point(data=center,aes(x,y,color=km$centers))
# 
# plot(x=data$x,y=data$y)
# points(km$centers,cex=3)
unique(data$d_hour)

ggplot(data,aes(x,y))+geom_point(aes(color=d_hour))+theme_minimal()

ggplot(data, aes(x, y )) + geom_point(aes(color = d_hour)) + theme_minimal() + theme(legend.position = "none") +
  ggtitle("Check-ins colored by place_id")




#CHECKING VARIABLES
ggplot(r,aes(accuracy))+geom_density()

ggplot(r,aes(x))+geom_density()

ggplot(r,aes(y))+geom_density()

ggplot(r,aes(time))+geom_density()

ggplot(r,aes(as.numeric(r$place_id)))+geom_density()

plot(data$y,data$ccuracy)




## PLACE IDS WITH TIME
r$t_hour <- (r$time)/60
r$d_hour <- ceiling(r$t_hour%%24)
r$day <- ceiling((r$t_hour/24))

data_time <- split(r,r$d_hour)
  
number <-lapply(data_time,function(x){length(unique(x$place_id))})
accu <- lapply(data_time,function(x){sum((x$accuracy))})

#on doing number/accu we see that the accuracy with place is max for 1 hour
#taking data for 1st hour

data_one <- data_time[[1]]
#head(data_one)
#plot_ly(data =data_one, x=x,y=y,z=accuracy,type = "scatter3d", mode = "markers", marker=list(size= 5)) 
#ggplot(data_one,aes(y,accuracy))+geom_point()

sp_one <- split(data_one,data_one$place_id)
num <- lapply(sp_one,function(x){length(x$y)})

range(num)#Most of the place_ids occur only once
#median(as.vector(as.numeric(num)))

library(caret)
library(ranger)
library(randomForest)
data_one$place_id = as.factor(data_one$place_id)




pred_one <- randomForest(place_id ~ x + y + accuracy ,data_one)
                   



#checking for accuracy
data_place_acc <- as.vector(tapply(data$accuracy,data$place_id,sum))
data_place_pl <- as.vector(tapply(data$place_id,data$place_id,length))
df_acc <- data.frame(data_place_acc,data_place_pl)
ggplot(df_acc,aes(data_place_acc, data_place_pl))+geom_point()
#for the whole data
r_place_acc <- as.vector(tapply(r$accuracy,r$place_id,sum))
r_place_pl <-  as.vector(tapply(r$accuracy,r$place_id,length))
r_df_acc <- data.frame(r_place_acc,r_place_pl)
ggplot(r_df_acc,aes(r_place_acc, r_place_pl))+geom_point()
#CONCLUSION
#The above graph shows that sum of accuracy and frequency of occurrence of place_id are linearly related

# 
# #CHECKING FOR ACC AND PLACE_ID DATA WITH TIME
# data_one_place_acc <- as.vector(tapply(data_one$accuracy,data_one$place_id,sum))
# data_one_place_pl <- as.vector(tapply(data_one$place_id,data_one$place_id,length))
# one_df_acc <- data.frame(data_one_place_acc,data_one_place_pl)
# ggplot(one_df_acc,aes(data_one_place_acc, data_one_place_pl))+geom_point()


r_meanx <- as.vector(names(tapply(r$x,r$place_id,mean)))
r_meany <- as.vector(tapply(r$y,r$place_id,mean))
r_place_id <- as.vector(names(tapply(r$x,r$place_id,mean)))
r_day <- as.vector(tapply(r$day,r$place_id,median))   #median
r_hour <- as.vector(tapply(r$d_hour,r$place_id,median))   #median
#checking for any time relation
sp_time <- split(data,data$place_id)
mean(sp_time[[3]]$day)

#new data frame
r_frame <- data.frame(r_place_id,r_place_pl,r_place_acc,r_meanx,r_meany,r_day,r_hour) #data frame according to place id
names(r_frame) <- c("place_id","frequency","accuracySum","X","Y","day","hour")

#length(unique(r_frame$place_id))
#3D PLOT 
#plot_ly(data = r_frame, x = X , y = Y, z = accuracySum, color = frequency,  type = "scatter3d", mode = "markers", marker=list(size= 5))


#test data
test <- fread("test.csv")

write.csv(r_frame,file="new_train.csv")


r_frame$place_id <- as.factor(r_frame$place_id)
model <- randomForest(place_id ~.,data=data.frame(r_frame))

head(r_frame)
lapply(r_frame[1,],class)

















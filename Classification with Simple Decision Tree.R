# Import Data #
movie <- read.csv("E:/My Dictionary/Using R/Data/Movie_classification.csv")
View(movie)
str(movie)

# Data Preprocessing #
summary(movie) #there are missing values in variable Time_taken
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE) #imputasi with mean because it is numerical variable

# Test-Train Split
install.packages('caTools')
library(caTools)
set.seed(0)
split <- sample.split(movie,SplitRatio = 0.8)
train <- subset(movie,split == TRUE)
test <- subset(movie,split == FALSE)

############################### MODELING #################################
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

classtree <- rpart(formula = Start_Tech_Oscar~., data = train, method="class", control = rpart.control(maxdepth = 3))
rpart.plot(classtree, box.palette="RdBu", digits = -3) 
test$pred <- predict(classtree, test, type = "class")
cm <- table(test$Start_Tech_Oscar, test$pred)
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy

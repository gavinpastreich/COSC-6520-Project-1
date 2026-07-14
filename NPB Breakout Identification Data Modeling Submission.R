rm(list=ls())

#Libraries

library(sqldf)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(caret)
library(gains)
library(pROC)
library(rpart)
library(rpart.plot)
library(forecast)
library(randomForest)
library(adabag)

library(ROSE)

library(kknn)

#holy shit this is actually coming together, I have to remove service time of 5. 

ShrinkData <- read.csv("C:/Users/gavin/OneDrive/Desktop/2026 Spring Courses/COSC 6520/Project 1/ShrinkData.csv")

ShrinkData$X <- NULL



#Separate 2025 as validation set

ShrinkData$PreBreakout <- as.factor(ShrinkData$PreBreakout)


ShrinkData2025 <- sqldf("select * from ShrinkData where Season = 2025")

ShrinkData2025raw <- sqldf("select * from ShrinkData where Season = 2025")

#Saving names for later prediction
names_2025 <- ShrinkData2025$Name


ShrinkData <- sqldf("select * from ShrinkData where Season != 2025")

ShrinkData$Name <- NULL

ShrinkData$Season <- NULL

ShrinkData2025$Name <- NULL

ShrinkData2025$Season <- NULL

ShrinkData2025$PreBreakout <- NULL #This is where classification predictions will go

ShrinkData2025raw$PreBreakout <- NULL #This is where classification predictions will go


#There is a massive skew in Breakout
ggplot(ShrinkData, aes(x = PreBreakout, fill = PreBreakout)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Starting Distribution of PreBreakout", x = "Breakout", y = "Count") +
  theme_minimal()

#Ok so I'm going to use ROSE after I do classification tree


#Setting Seed
set.seed(1)


ClassificationTree <- rpart(PreBreakout ~., 
                            data = ShrinkData, 
                            method = "class",
                            control = rpart.control(minsplit = 5, cp = 0.001)
)

rpart.plot(ClassificationTree)

summary(ClassificationTree)

ClassificationTree$frame



# Get leaf assignments
TreeLeaves <- data.frame(
  obs    = seq_len(nrow(ShrinkData)),
  actual = ShrinkData$PreBreakout,
  leaf   = ClassificationTree$where,
  probPreBreakout = predict(ClassificationTree, ShrinkData, type = "prob")[, "1"]
)

# Summarise each leaf
TreeLeafSummary <- TreeLeaves %>%
  group_by(leaf) %>%
  summarise(
    n_total  = n(),
    n_class1 = sum(actual == 1),
    n_class0 = sum(actual == 0),
    probPreBreakout    = max(probPreBreakout),
    .groups  = "drop"
  ) %>%
  arrange(n_class1)

print(TreeLeafSummary)

leaves0 <- TreeLeafSummary %>%
  filter(n_class1 == 0) %>%
  pull(leaf)



ShrinkDataFiltered <- ShrinkData[!(TreeLeaves$leaf %in% leaves0) | TreeLeaves$actual == 1,]

summary(ShrinkDataFiltered$PreBreakout)


#Was able to remove 300 observations based on 0 prebreakout. 
ggplot(ShrinkDataFiltered, aes(x = PreBreakout, fill = PreBreakout)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
labs(title = "Distribution of PreBreakout After Classification Tree", x = "Breakout", y = "Count") +
  theme_minimal()

#Splitting into train and test


set.seed(1)

Split <- createDataPartition(ShrinkDataFiltered$PreBreakout, p=0.7, list=FALSE)

ShrinkTrainSet <- ShrinkDataFiltered[Split,]
ShrinkTestSet <- ShrinkDataFiltered[-Split,]

sqldf("select sum(PreBreakout) from ShrinkTrainSet") #20 positive
sqldf("select sum(PreBreakout) from ShrinkTestSet") #8 positive, got all of them.

#Even with train and test split there is still a big imbalance

#Scaling

Shrinkpreproc  <- preProcess(ShrinkTrainSet, method = c("center", "scale"))
ShrinkTrainSet <- predict(Shrinkpreproc, ShrinkTrainSet)
ShrinkTestSet  <- predict(Shrinkpreproc, ShrinkTestSet)
ShrinkData2025 <- predict(Shrinkpreproc, ShrinkData2025)

# Reattach names to 2025 after scaling
ShrinkData2025$Name <- names_2025


#Going to use ROSE (Random Oversampling estimate) to fill the rest of the data
#install.packages("ROSE")
library(ROSE)

#have to change PreBreakout to Yes or No for KNN
ShrinkTrainSet$PreBreakout <- factor(ifelse(ShrinkTrainSet$PreBreakout == "1", "Yes", "No"))
ShrinkTestSet$PreBreakout  <- factor(ifelse(ShrinkTestSet$PreBreakout  == "1", "Yes", "No"))

#Does setting seed to 1 override the seed in here?
RoseTrainSet <- ROSE(PreBreakout ~ ., data = ShrinkTrainSet, N = 1500, p = 0.4, seed = 1)$data

table(RoseTrainSet$PreBreakout) #a bit closer to even which is good. Time for KNN


#much closer and still allows for priority to the negative class given the rarity of breakout

ggplot(RoseTrainSet, aes(x = PreBreakout, fill = PreBreakout)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
labs(title = "Distribution of PreBreakout", x = "Breakout", y = "Count") +
  theme_minimal()



# Optimal grid for precision
myGrid <- expand.grid(
  .kmax = c(27),
  .distance = c(1),
  .kernel = c("biweight")
)


# Control for model
myCtrl <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = prSummary,
  savePredictions = "final"
)

#training KNN model
KNN_fit <- train(PreBreakout ~ ., data = RoseTrainSet,
                 method    = "kknn",
                 trControl = myCtrl,
                 tuneGrid  = myGrid,
                 metric    = "Precision")



print(KNN_fit)
#plot(KNN_fit)   

KNN_Class      <- predict(KNN_fit, newdata = ShrinkTestSet)
KNN_Class_prob <- predict(KNN_fit, newdata = ShrinkTestSet, type = "prob")

confusionMatrix(KNN_Class, ShrinkTestSet$PreBreakout, positive = "Yes") # Classified as 1 or 0 no probability


confusionMatrix( #KNN_Class_prob
  factor(ifelse(KNN_Class_prob[, "Yes"] >= 0.9, "Yes", "No"), levels = c("No", "Yes")),
  ShrinkTestSet$PreBreakout, positive = "Yes"
)
#28 percent precision much better


##cumulative lift chart

ShrinkTestSet$PreBreakout_num <- ifelse(ShrinkTestSet$PreBreakout == "Yes", 1, 0)

gains_table <- gains(ShrinkTestSet$PreBreakout_num, KNN_Class_prob[, "Yes"])

gains_table

plot(c(0, gains_table$cume.pct.of.total*sum(ShrinkTestSet$PreBreakout_num))~c(0, gains_table$cume.obs), 
     xlab = "# of cases", 
     ylab = "Cumulative", 
     main="Cumulative Lift Chart", 
     type="l")
lines(c(0, sum(ShrinkTestSet$PreBreakout_num))~c(0, dim(ShrinkTestSet)[1]), 
      col="red", 
      lty=2)

##decile-wise lift
barplot(gains_table$mean.resp/mean(ShrinkTestSet$PreBreakout_num), 
        names.arg=gains_table$depth, 
        xlab="Percentile", 
        ylab="Lift", 
        ylim=c(0,7), 
        main="Decile-Wise Lift Chart")

##ROC
roc_object <- roc(ShrinkTestSet$PreBreakout, KNN_Class_prob[,2])
plot.roc(roc_object)
auc(roc_object)


#Prediction time! Going based off probability

probability2025 <- predict(KNN_fit, newdata = ShrinkData2025 %>% select(-Name), type = "prob")

#Applying the 90% threshold
ShrinkData2025$BreakoutProbability <- probability2025[, "Yes"]
ShrinkData2025$Prediction <- ifelse(probability2025[, "Yes"] > 0.9, "Yes", "No")



# Attach predictions and probabilities by name
ShrinkData2025PredictionWithStats <- ShrinkData2025raw %>%
  left_join(
    ShrinkData2025 %>% select(Name, BreakoutProbability, Prediction),
    by = "Name"
  ) %>%
  arrange(desc(BreakoutProbability))

View(ShrinkData2025PredictionWithStats)

sqldf("select *
      from ShrinkData2025PredictionWithStats where Prediction = 'Yes'")




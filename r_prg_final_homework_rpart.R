url = "https://storage.googleapis.com/2017_ithome_ironman/data/kaggle_titanic_train.csv"

titanic <- read.csv(url)

summary(titanic)

titanic$Embarked <- as.character(titanic$Embarked)

titanic$Embarked[titanic$Embarked==""] <- "S"

titanic$Embarked <- factor(titanic$Embarked)

titanic$Survived <- factor(titanic$Survived)

str(titanic)

summary(titanic)


#remove col: c("PassengerId", "Name", "Ticket", "Cabin")

titanic_selected_col <- titanic[,c("Survived", "Pclass", "Sex", "Age", "SibSp","Parch","Fare", "Embarked")]
#用k-Nearest Neighbours填補遺漏值 https://rpubs.com/skydome20/R-Note10-Missing_Value
# knnImputation {DMwR}	R Documentation
# Fill in NA values with the values of the nearest neighbours

library(DMwR)

imputed_titanic <- knnImputation(titanic_selected_col)

summary(imputed_titanic)


n <- nrow(imputed_titanic)

set.seed(1945)

shuffled_imputed_titanic<- imputed_titanic[sample(n),]

train_indices <- round(n * 0.7)

train_set <-shuffled_imputed_titanic[1:train_indices, ]

test_set <- shuffled_imputed_titanic[(train_indices+1):n, ]

library(rpart)

tree_fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train_set, method = "class")

# 計算 accuracy
prediction <- predict(forest_fit, newdata = test_set, type="class")
confusion_matrix <- table(test_set$Survived, prediction)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

url <- "https://storage.googleapis.com/py_ds_basic/kaggle_titanic_test.csv"
to_predict <- read.csv(url)
summary(to_predict)
str(to_predict)

to_predict_selected_col <- to_predict[,c("Pclass", "Sex", "Age", "SibSp","Parch","Fare", "Embarked")]

imputed_to_predict <- knnImputation(to_predict_selected_col)

summary(imputed_to_predict)

predicted_results<- predict(tree_fit, newdata =imputed_to_predict, type="class")

to_submit<- data.frame(to_predict$PassengerId, predicted_results)

names(to_submit) <- c("PassengerId", "Survived")

head(to_submit)

write.csv(to_submit, file = "to_submit2.csv", row.names = FALSE)
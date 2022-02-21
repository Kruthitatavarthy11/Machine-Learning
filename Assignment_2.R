#importing the required packages 
library('caret')
library('ISLR')
library('dplyr')
library('class')

#Importing the data
originaldata <- read.csv("C:/Users/Kruthi Tatavarthy/Documents/Masters in Business Analytics/Sem-1/Machine Learning/UniversalBank.csv", header = TRUE, 
                         sep =",", stringsAsFactors = FALSE)

#Performing a k-NN classification with all predictors except ID and ZIP code i.e., eliminating ID and Zip Code from all the columns
originaldata$ID <- NULL
originaldata$ZIP.Code <- NULL
summary(originaldata)


originaldata$Personal.Loan =  as.factor(originaldata$Personal.Loan)


#use preProcess() from the caret package to normalize the data by dividing into training and validation
Model_norm <- preProcess(originaldata[, -8],method = c("center", "scale"))
originaldata_norm <- predict(Model_norm,originaldata)
summary(originaldata_norm)



Train_index <- createDataPartition(originaldata$Personal.Loan, p = 0.6, list = FALSE)
train.df = originaldata_norm[Train_index,]
validation.df = originaldata_norm[-Train_index,]

print(train.df)

#Prediction of data
library(caret)
library(FNN)

To_Predict = data.frame(Age = 40, Experience = 10, Income = 84, Family = 2,
                        CCAvg = 2, Education = 1, Mortgage = 0, Securities.Account =
                          0, CD.Account = 0, Online = 1, CreditCard = 1)
print(To_Predict)
To_Predict_Norm <- predict(Model_norm,To_Predict)

Prediction <- knn(train= as.data.frame(train.df[,1:7,9:12]),
                  test = as.data.frame(To_Predict_Norm[,1:7,9:12]),
                  cl= train.df$Personal.Loan,
                  k=1)
print(Prediction)



#Question 2 

set.seed(123)
UniversalBankcontrol <- trainControl(method= "repeatedcv", number = 3, repeats = 2)
searchGrid = expand.grid(k=1:10)

knn.model = train(Personal.Loan~., data = train.df, method = 'knn', tuneGrid = searchGrid,trControl = UniversalBankcontrol)

knn.model



#Question 3

predictions_bank <- predict(knn.model,validation.df)

confusionMatrix(predictions_bank,validation.df$Personal.Loan)
#The matrix has a 95.1% accuracy.

#Question 4
#Levels
To_Predict_Norm = data.frame(Age = 40, Experience = 10, Income = 84, Family = 2,
                                   CCAvg = 2, Education = 1, Mortgage = 0,
                                   Securities.Account =0, CD.Account = 0, Online = 1,
                                   CreditCard = 1)
To_Predict_Norm = predict(Model_norm, To_Predict)
predict(knn.model, To_Predict_Norm)



#Question 5
train_size = 0.5 #training(50%)
Train_index = createDataPartition(originaldata$Personal.Loan, p = 0.5, list = FALSE)
train.df = originaldata_norm[Train_index,]


test_size = 0.2 #Test Data(20%)
Test_index = createDataPartition(originaldata$Personal.Loan, p = 0.2, list = FALSE)
Test.df = originaldata_norm[Test_index,]


valid_size = 0.3 #validation(30%)
Validation_index = createDataPartition(originaldata$Personal.Loan, p = 0.3, list = FALSE)
validation.df = originaldata_norm[Validation_index,]



Testingknn <- knn(train = train.df[,-8], test = Test.df[,-8], cl = train.df[,8], k =3)
Validknn <- knn(train = train.df[,-8], test = validation.df[,-8], cl = train.df[,8], k =3)
Trainingknn <- knn(train = train.df[,-8], test = train.df[,-8], cl = train.df[,8], k =3)

confusionMatrix(Testingknn, Test.df[,8])
confusionMatrix(Validknn, validation.df[,8])
confusionMatrix(Trainingknn, train.df[,8])

#Final Conclusion: Difference between the training data accuracy and senitivity is better 
#when compared to testing data and validation data

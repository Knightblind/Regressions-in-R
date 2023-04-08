#@Author: Thomas Sorenson
##Description: This script performs linear regression analysis on the dataset for Titanic 
##and also illustrates some basic analysis such as incorporating interaction terms and regperforming multiple regressions
##This script is a good sample for teaching basic regressions in R.
#First We want to install a package to allow us to perform some data analysis

install.packages("SWIRL")
LIBRARY(SWIRL)

##First, I will load the dataset Titanic-sample.csv
titanic= read.csv("E:/Analysis6100/Data Sets/Titanic-sample.csv")

##display dataset
titanic

#I am going to explore this data a bit and check for the missing values that may exist.
#count missing values
sum(is.na(titanic))

##Looks like we have some missing values, im going to create a new dataset which has removed the missing values.
##create titanic2 which removes na values

titanic2= na.omit(titanic)
#count the na values in titanic2
sum(is.na(titanic2))


##Im interested in seeing which variables hae impact on the fare paid for titanic tickets. So i will be performing a multiple linear regression with fare as my Y variable and the rest of the data as independent variables.
#Run a multiple regression with fare as the y variable and all other variables as x variables
lm.fit= lm(Fare~., data=titanic2)
##Summarize the output of the regression
summary(lm.fit) 

#Now i will perform More multiple regression. Run another regression predicting the effect on fare by the variables survived, class, sex,age, sibsp, parch
##there are two variables being removed from the prior model, therefore they should be removed incrementally
lm.fit2= lm(Fare~.-Embarked, data=titanic2)
##Summarize the output of the regression
summary(lm.fit2) 


##Next we remove PassengerID
lm.fit3= lm(Fare~.-Embarked -PassengerId, data=titanic2)
##Summarize regression output
summary(lm.fit3)


#Now  im interested in seeing what effect there is for sex as age increases.  So i will perform Even more regression: add an interaction term for sex and age
lm.fit4= lm(Fare~.-Embarked -PassengerId +Sex*Age, data=titanic2)
summary(lm.fit4)




# Clear all variables
rm(list=ls(all=TRUE)) 
# Close all figures
graphics.off()

# Importing Data
dataset=read.csv("Data.csv")

# Encoding categorical data
dataset$State=factor(dataset$State,
                       levels=c("New York","California","Florida"),
                       labels=c(1,2,3))

# Splitting the dataset into Training Set and Test Set
split=sample.split(dataset$Profit,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Multiple Linear Regression
regressor=lm(formula=Profit~.,training_set)

#Predicting the test set results
y_pred=predict(regressor,test_set)

# Building optimum model by backward elimination
# Choose only IVs that are highly statistically significant, and
# Remove the IVs that are not statistically significant

regress=lm(formula=Profit~R.D.Spend+Administration+Marketing.Spend+State,
             training_set)
summary(regress)

regress2=lm(formula=Profit~R.D.Spend+Administration+Marketing.Spend,
           training_set)
summary(regress2)

regress3=lm(formula=Profit~R.D.Spend+Marketing.Spend,
            training_set)
summary(regress3)

regress4=lm(formula=Profit~R.D.Spend,
            training_set)
summary(regress4)

#Predicting the test set results
y_pred_optimum=predict(regress4,test_set)










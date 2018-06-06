# Clear all variables
rm(list=ls(all=TRUE)) 
# Close all figures
graphics.off()

# Importing Data
dataset=read.csv("Position_Salaries.csv")
dataset = dataset[2:3]

# Random Forest Regression
#install.packages("randomForest")
#library(randomForest)
regressor=randomForest(x=dataset[1],y=dataset$Salary,ntree=500)
# x argument of randomForest function is a dataframe
# y argument of randomForest function is a vector

summary(regressor)
y_pred=predict(regressor,data=dataset)

# Predicting a new result with Random Forest Regression
y_pred_1 = predict(regressor, data.frame(Level = 6.5))

# Visualising the Random Forest Regression results (higher resolution)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Random Forest Regression)') +
  xlab('Level') +
  ylab('Salary')
# Clear all variables
rm(list=ls(all=TRUE)) 
# Close all figures
graphics.off()

# Importing Data
dataset=read.csv("Position_Salaries.csv")
dataset = dataset[2:3]

# Decision Tree Regression
#install.packages("rpart")
#library(rpart)
regressor=rpart(formula=Salary~.,data=dataset,
                control=rpart.control(minsplit = 1))
#summary(regressor)
y_pred=predict(regressor,data=dataset)

#Ploting
p=ggplot(data=dataset, aes(x=Level,y=Salary))
p+geom_point(colour="Red")+
  geom_line(aes(x=Level,y=y_pred),colour="Blue")+
  xlab("Position Level")+
  ylab("Salary")

# Make a prediction for level=6.5
y_pred_new=predict(regressor,data=data.frame(Level=6.5))

# Create a smoother curve
x_grid=seq(min(dataset$Level),max(dataset$Level),0.1)
y_pred_smooth=predict(regressor,newdata=data.frame(Level=x_grid))

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x=x_grid,y=y_pred_smooth),colour="Blue")+
  xlab("Position Level")+
  ylab("Salary")




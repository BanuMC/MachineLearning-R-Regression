# Clear all variables
rm(list=ls(all=TRUE)) 
# Close all figures
graphics.off()

# Importing Data
dataset=read.csv("Position_Salaries.csv")
dataset = dataset[2:3]

# SVR
#install.packages("e1071")
#library(e1071)
regressor=svm(formula=Salary~.,data=dataset,type = 'eps-regression')
# if u are doing regression type=eps-regression
# if u are doing classification type=C-classification

summary(regressor)
y_pred=predict(regressor,data=dataset)

# Make a prediction for level=6.5
y_pred_new=predict(regressor,data.frame(Level=6.5))

#Ploting
p=ggplot(data=dataset, aes(x=Level,y=Salary))
p+geom_point(colour="Red")+
  geom_line(aes(x=Level,y=y_pred),colour="Blue")+
  xlab("Position Level")+
  ylab("Salary")



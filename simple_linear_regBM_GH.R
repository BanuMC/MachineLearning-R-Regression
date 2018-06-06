# Clear all variables
rm(list=ls(all=TRUE)) 
# Close all figures
graphics.off()

# Importing Data
dataset=read.csv("50_Startups.csv")

#Ploting the data
p=ggplot(data=dataset, aes(x=YearsExperience,y=Salary))
 p+ geom_point()
 
 
# Splitting the dataset into Training Set and Test Set
 split=sample.split(dataset$Salary,SplitRatio = 2/3)
 training_set=subset(dataset,split==TRUE)
 test_set=subset(dataset,split==FALSE)
 
#Simple Linear Regression
regressor=lm(formula=Salary~YearsExperience, data=training_set)
# List the coefficients and statistics such as p-value
summary(regressor)

# Predicting test_set Results
y_pred=predict(regressor, test_set)

# Predicting training_set Results
# This is to draw the regression line
y_pred2=predict(regressor,newdata = training_set)

#Ploting the Results
#install.packages("ggplot2")
#library(ggplot2)
#Plot training set 
r=ggplot(data=training_set, aes(x=YearsExperience,y=Salary,colour="Red"))
r+ geom_point()+geom_line(aes(x=YearsExperience),y=y_pred2,colour="Blue")+
  ggtitle("Salary vs. Experience (Training Set)")+
  xlab("Years of Experience")+
  ylab("Salary")+
  theme(axis.title.x= element_text(size=18),
        axis.title.y= element_text(size=18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18))

#Plot test set 
r=ggplot(data=test_set, aes(x=YearsExperience,y=Salary,colour="Red"))
r+ geom_point()+geom_line(aes(x=YearsExperience),y=y_pred,colour="Blue")+
  ggtitle("Salary vs. Experience (Test Set)")+
  xlab("Years of Experience")+
  ylab("Salary")+
  theme(axis.title.x= element_text(size=18),
        axis.title.y= element_text(size=18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18))





 
# Clear all variables
rm(list=ls(all=TRUE)) 
# Close all figures
graphics.off()

# Importing Data
dataset=read.csv("Position_Salaries.csv")
data_lin=dataset[,2:3]

# Simple Linear Regression
lin_reg=lm(formula=Salary~Level, data=data_lin)
# List the coefficients and statistics such as p-value
summary(lin_reg)
y_pred_lin=predict(lin_reg,data=dataset)

#Polynomial Regression Model
data_poly=dataset[,2:3] # Prep for poly model
data_poly$level2=data_poly$Level^2
data_poly$level3=data_poly$Level^3
data_poly$level4=data_poly$Level^4
poly_reg=lm(formula=Salary~., data=data_poly)
summary(poly_reg)
y_pred_poly=predict(poly_reg,data=data_poly)

#Ploting
p=ggplot(data=dataset, aes(x=Level,y=Salary))
p+geom_point(colour="Red")+
  geom_line(aes(x=Level,y=y_pred_poly),colour="Blue")+
  geom_line(aes(x=Level,y=y_pred_lin),colour="Green")+
  xlab("Position Level")+
  ylab("Salary")

# Create a smoother curve
x_grid=seq(min(dataset$Level),max(dataset$Level),0.1)
y_pred_poly_smooth=predict(poly_reg,data=data.frame(Level=x_grid,level2=x_grid^2,
                                             level3=x_grid^3,level4=x_grid^4))
y_pred_lin_smooth=predict(lin_reg,newdata=data.frame(Level=x_grid))
                                                  
p+geom_point(colour="Red")+
  geom_line(aes(x=x_grid,y=y_pred_poly_smooth),colour="Blue")+
  geom_line(aes(x=x_grid,y=y_pred_lin_smooth),colour="Green")+
  xlab("Position Level")+
  ylab("Salary")

# Make a prediction for level=6.5
y_pred_lin_new=predict(lin_reg,data.frame(Level=6.5))
y_pred_poly_new=predict(poly_reg,data.frame(Level=6.5,level2=6.5^2,
                                            level3=6.5^3,level4=6.5^4))  

  
#Prodigy_DS_Task-2

#Data cleaning and EDA with TITANIC dataset.

# First we will load the data

data2<-read.csv("E:\\EDUCATION\\Prodigy Infotech Internship\\train.csv",header = TRUE)
head(data2)
str(data2)

# We now clean the unnecessary rows and columns and remove the missing values.

data2<-na.omit(data2)
summary(data2)
data2<-data2[,-c(1,4,9,11)]
head(data2)

# For Visualize the dataset we use "Plot" command and plot the whole dataset.

plot(data2)

#EDA

# We now see that how many people of different class would survive and died with barplot and frequency table.

survival_class<-table(data2$Survived,data2$Pclass)
survival_class
barplot(survival_class,xlab = "Class i.e 1= Upper, 2= Middle, 3=Lower",ylab = "Number of people",beside = TRUE,col = c("red","green"),legend=c("Didnot survive","Survived"),main = "Barplot for Survived people of Different class")

# We now see that how many people of different sex would survive and die with barplot and frequency table.

survival_gender<-table(data2$Survived,data2$Sex)
survival_gender
barplot(survival_gender,xlab = "Gender : Female, Male",ylab = "Number of people",beside = TRUE,col = c("red","green"),legend=c("Didnot survive","Survived"),main = "Barplot for Survived people of different gender")

# We now interested to see how many people of different boarding port or different emberkment port would survive with barplot and frequency table.

survival_embarked<-table(data2$Survived,data2$Embarked)
survival_embarked
barplot(survival_embarked,xlab = "Port of embarkation i.e. C = Cherbourg, Q = Queenstown, S = Southampton",ylab = "Number of People",beside = TRUE,col = c("red","green"),legend=c("Didnot Survive","Survived"),main = "Barplot for Survived People boarding from different port")

# We now look into the age ditribution of the passengers.

hist(data2$Age,probability = TRUE,xlab = "Age",ylab = "Density",main = "Histogram of age of Different people in the ship",col = "pink")
lines(density(data2$Age),col="green",lwd=4)

# Looking into the fare distribution of ticket of different passengers.

hist(data2$Fare,probability = TRUE)
lines(density(data2$Fare),col="green",lwd=4)

# We now creating prediction model using glm.

model2<-glm(data2$Survived~.,family = "binomial",data = data2)
model2
summary(model2)
anova(model2,test = "Chisq")
model2.o<-step(model2,direction = "backward")

plot(model2.o)

### Conclusion:
# The survival rate of female and upper class people is so high.
# The survival rate of people who embarked from southampton is very high.
# The predicted model follows normality which shows that our prediction is pretty sure.
# There is a significant effect of class , gender, age group on the survival rate which is clearly shown in the prediction as well as glm model.
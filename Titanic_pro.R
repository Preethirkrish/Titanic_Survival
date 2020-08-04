View(train)
library(Amelia)

#missmap(train,, vars =Age,main="Titanic Training Data - Missings Map", col=c("yellow", "black"), legend=FALSE)
library(visdat)
vis_miss(train)

# Droping Cabin as most of the values are missing
# Droping Embarked, PAssengerId, Fare, Ticket, Name as they do not decide the survival

library(dplyr)
data.frame = select(train, Survived, Age, Pclass, Sex, SibSp, Parch)
data.frame=na.omit(data.frame)
str(data.frame)

# To convert Survived and Pclass as categorical values
data.frame$Survived = factor(data.frame$Survived)
data.frame$Pclass = factor(data.frame$Pclass, order=TRUE, levels=c(3,2,1))

# Visualizing the data

#correlation
library(GGally)
ggcorr(data.frame, nbreaks = 6, label = TRUE, label_size = 3, color = "grey")

#survived count
library(ggplot2)
ggplot(data.frame, aes(x= Survived))+
  geom_bar(width = 0.5, fill = "coral")+
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5)

#Survival count by sex
ggplot(data.frame, aes(x= Survived,fill = Sex))+
  geom_bar(position = position_dodge())+
  geom_text(stat = 'count', 
            aes(label =stat(count)), 
            position = position_dodge(width = 1), vjust= -0.5)

#Survival count by Pclass(Passenger class)
ggplot(data.frame, aes(x= Survived,fill = Pclass))+
  geom_bar(position = position_dodge())+
  geom_text(stat = 'count', 
            aes(label =stat(count)), 
            position = position_dodge(width = 1), vjust= -0.5)

#Age Density
ggplot(data.frame, aes(x= Age))+
  geom_density(fill ='grey')

#Survival count by age
# discritizing age
data.frame$Discritized.age= cut(data.frame$Age, c(0,10,20,30,40,50,60,70,80,100))
ggplot(data.frame, aes(x= Discritized.age,fill = Survived))+
  geom_bar(position = position_dodge())+
  geom_text(stat = 'count', 
            aes(label =stat(count)), 
            position = position_dodge(width = 1), vjust= -0.5)

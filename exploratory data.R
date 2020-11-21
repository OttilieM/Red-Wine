####HEADER ####
## Who: Ottilie Mitchell
## What: EDA
## Last edited:12/11/2020
####

####CONTENTS ####
##1.1 Set working directory
##1.2 Read in data
##1.3 Check for missing data
##1.4 Data manipulation
##2.1 Boxplots all data
##2.2 Boxplot red and white
##3.0 Histogram all data
##3.1 Histogram red and white
##4.0 Correlograms
##4.1 Install library
##4.2 Correlograms all data
##4.3 Experimenting with different methods
##4.4 Corrograph number method with red wine
##4.5 Corrograph number method with white wine
##5.0 Plot showing alcohol and quality in red and white wine

##1.1 Set working directory
setwd("C:/Users/User/Documents/Data Science/C7081 - Statistical analysis for data science/Assessment")

##1.2 Read in data
my_data <- read.csv("winequality-whiteandred.csv")

##1.3 Check for missing data
names(my_data)
dim(my_data)
sum(is.na(my_data)) #no missing data

##1.4 Data manipulation
#Split data by type
type <- split(my_data, my_data$type)
str(type) #structure of data
white <- type$white
white <- type$white[,2:13]
red <- type$red
red <- type$red[,2:13]

##2.0 Boxplots all data
boxplot(my_data$quality, col = "blue", xlab = "Quality", ylab = "Count", main = "Quality count")

##2.1 Boxplot red and white data
par(mfrow=(c(1,2))) #allows 2 plots next to each other

boxplot(white$quality, col = "yellow", xlab = "Quality", main = "White wine quality")
boxplot(red$quality, col = "maroon", xlab = "Quality", main = "Red wine quality")

##3.0 Histograms all data
hist(my_data$quality, col = "blue", xlab = "Quality", main = "Qualilty count")

##3.1 Histogram red and white data

hist(white$quality, col = "yellow", xlab = "Quality", main = "White wine quality")
abline(v= mean(white$quality), col = "black", lty = 2, lwd = 3) #shows the mean

hist(red$quality, col = "maroon", xlab = "Quality", main = "Red wine quality")
abline(v = mean(red$quality), col = "black", lty = 2, lwd = 3)#shows the mean

##4.0 Correlograms

#4.1 Install library
install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1)) #shows one plot at a time

##4.2 Correlogram all data
my_data <- my_data[,2:13] #removes the type variable
cordata <- cor(my_data)
head(round(cordata,2))
##4.3 Experimenting with different methods
#Circle method
corrplot(cordata, method = "circle", type = "upper")

#Colour method
corrplot(cordata, method = "color", type = "upper")

#Number method
corrplot(cordata, method = "number", type = "upper")

#Color method the best as can see easily how clear the correlation is

##4.4 Corrograph number method with red wine

corplotred <- cor(red)
corrplot(corplotred, method = "color", type = "upper")

##4.5 Corrograph number method with white wine
corplotwhite <- cor(white)
corrplot(corplotwhite, method = "color", type = "upper")

#From the corrograph we can see that quite clearly that alcohol correlates the strongest with quality in red and white wine.

##5.0 Plot showing alcohol and quality in red and white wine
par(mfrow=c(1,2))
plot(red$quality,red$alcohol, col = "maroon", xlab = "Quality", ylab = "Alcohol", main = "Red")
plot(white$quality,white$alcohol, col = "yellow", xlab = "Quality", ylab = "Alcohol", main = "White")

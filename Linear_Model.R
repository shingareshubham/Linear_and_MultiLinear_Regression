#Set Working Directory
setwd("C:/R")

#Read the data file
DefaultData<-read.csv("25/data.csv")

#Check what is there in the file
DefaultData
View(DefaultData)

#Check if the data is populated/imported properly
head(DefaultData)
tail(DefaultData)

#Check the summary of the file
summary(DefaultData)

#Generate plot of Dependent variable (Losses)
plot(DefaultData$Losses)

#Check the quantile to find out the outlier limit
quantile(DefaultData$Losses, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.995,1))

#Creating the Capped Losses column with 1200 cap
DefaultData$CappedLosses<-ifelse(DefaultData$Losses>1200,1200,DefaultData$Losses)

#Check if Capped Losses column has been created properly or not
summary(DefaultData)
names(DefaultData)

#Create new object deleting Losses and S.no.
DefaultData3<-DefaultData[,-c(9)]

#Check the headings of the new object
names(DefaultData3)

#Structure of Default data
str(DefaultData3)


#Generate plots to see the relation between the independent variables and the dependent variable
plot(DefaultData3$Age,DefaultData3$CappedLosses)
plot(DefaultData3$Years.of.Driving.Experience,DefaultData3$CappedLosses)
plot(DefaultData3$Number.of.Vehicles,DefaultData3$CappedLosses)
plot(DefaultData3$Gender,DefaultData3$CappedLosses)
plot(DefaultData3$Married,DefaultData3$CappedLosses)
plot(DefaultData3$Vehicle.Age,DefaultData3$CappedLosses)
plot(DefaultData3$Fuel.Type,DefaultData3$CappedLosses)

#Need to see Losses Distribution among all independant variables. Pivot Table in R with melt and cast
install.packages("reshape")
library("reshape")

#First look at the Data names, by which we want to create pivot table
names(DefaultData3)

#Melt the data: Melt will identify items to be summed/average (Called Measures) and separate out id variables by which we want to add/average
data.m<-melt(DefaultData3, id=c(1:8), measure=c(9))

#Let's look at the different values of our new object
head(data.m)
#CappedLosses have been melted

#Let's cast our data
cast(data.m, Age~variable,fun.aggregate=sum)
data.c<-cast(data.m, Age~variable,mean)
data.c
data.c<-cast(data.m, Age~variable,c(sum,mean))
data.c

#Create Age Bands from 16 to 25, 26 to 59 and 60+
DefaultData3$AgeBand<-ifelse(DefaultData3$Age<=25,"16-25",
                             ifelse(DefaultData3$Age>=60,"60+","26-59"))

#Let's see if we are able to do the conversion in a correct way or not
head(DefaultData3)
tail(DefaultData3)

#Create AgeBand - Average
data.ageband<-aggregate(Age~AgeBand,data=DefaultData3,mean)
data.ageband

#Merge two object - Just like vlookup in Excel
DefaultData3<-merge(DefaultData3,data.ageband, by="AgeBand")

#View the data if it has been looked up correctly
View(DefaultData3)

#We can export data from R to excel
write.csv(DefaultData3,"Data1.csv")
#Similarly we can convert Vehicle Age to Vehicle Age Band

#Convert Categorical varibles in Dummy Variable
DefaultData3$GenderDummy<-ifelse(DefaultData3$Gender=="F",1,0)
#Similarly we can covert other categorical variables to Dummy Variables

#Check the headings and the summary
names(DefaultData3)
summary(DefaultData3)

#We will use the data which has been converted into bands and dummy variables. Read the final Data
DefaultData4<-read.csv("25/Linear_Reg_Sample_Data.csv")

#Look at the column Headings
names(DefaultData4)

#Install car Package for cif (Multicollinearity)
install.packages("car")

#Load the car package
library("car")

#Create linear function for vif
vif_data<-lm(Capped_Losses~Years_Drv_Exp+Number_Vehicles+Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)

summary(vif_data)
#Check Vif, vif>2 means presence of multicollinearity
vif(vif_data)

#Compare R-square of Average_Age and Years_Drv_Exp to check which performs better
age1<-lm(Capped_Losses~Average_Age,data=DefaultData4)
drv1<-lm(Capped_Losses~Years_Drv_Exp,data=DefaultData4)
summary(age1)
summary(drv1)
#keep Average_Age and remove Years_Drv_Exp
#In same way we can decide to keep Age band as compared to Age and Vehicle Age Band as compared to Vehicle Age

#Run Linear Regression w/o Years_Drv_Exp
lin_r1<-lm(Capped_Losses~Number_Vehicles+Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)

#Let's look at the results
summary(lin_r1)
#Remove Number_Vehicles
names(DefaultData4)
lin_r<-lm(Capped_Losses~Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)


#Run Linear Regression w/o Number_Vehicles
lin_r2<-lm(Capped_Losses~Average_Age+Gender_Dummy+Married_Dummy+Avg_Veh_Age+Fuel_Type_Dummy,data=DefaultData4)

#Let's look at the results
summary(lin_r2)


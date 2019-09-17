getwd()
setwd("C:\\Users\\pavan\\Downloads\\Project\\SalesForce_Contact")

install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("sqldf")
install.packages("lubridate")
install.packages("forcats")
install.packages("psych")
install.packages("MASS")
install.packages("broom")
install.packages("dummies")
install.packages("nnet")
install.packages("caret")
install.packages("e1071")
library(dplyr)
library(ggplot2)
library(stringr)
library(sqldf)
library(lubridate)
library(forcats)
library(readxl)
library(psych)
library(e1071)
library(MASS)
library(broom)
library(dummies)
library(neuralnet)
library(nnet)
library(caret)

#Reading DataSet HireHeroes
setwd("E:\\Project\\HireHeros\\SalesForce_Contact")
Complete_dataset =read_excel("SalesForce_Contact_csv.xlsx")

names(Complete_dataset)

#Select only the required columns from Dataset
Selected_Data = Complete_dataset[,c("Status__c","Service_Branch__c","MailingState","MailingPostalCode","Service_Rank__c","Alumni_Survey_Completed__c","Military_Spouse_Caregiver__c","Alumni__c","Gender__c","Race__c","Confirmed_Hired_Date__c","Dat_Initial_Assessment_was_Completed__c","Date_of_Service_EntryNew__c","Date_of_SeparationNew__c","CreatedDate","Hire_Heroes_USA_Confirmed_Hire__c")]
names(Selected_Data)
				
table(Selected_Data$Military_Spouse_Caregiver__c)

####################################################################################

#Filtering based on whether the client opted IN to the HHUSA alumni program
Filtered_Data = Selected_Data %>% filter(Alumni__c==1)

#####################################################################################
#Converting character/numeric variables to factors

Filtered_Data$Status__c = as.factor(Filtered_Data$Status__c)

#Filtering insignificant levels and dropping them

Filtered_Data = Filtered_Data[! Filtered_Data$Status__c %in% c('Full time', 'Full time (35 hours a week or more)','Part time (less than 35 hours a week)'), ] %>% droplevels()
table(Filtered_Data$Status__c)

Filtered_Data$Service_Branch__c= as.factor(Filtered_Data$Service_Branch__c)
table(Filtered_Data$Service_Branch__c)

Filtered_Data$MailingState = as.factor(Filtered_Data$MailingState)
table(Filtered_Data$MailingState)

Filtered_Data$MailingPostalCode= as.factor(Filtered_Data$MailingPostalCode)
table(Filtered_Data$MailingPostalCode)

Filtered_Data$Service_Rank__c= as.factor(Filtered_Data$Service_Rank__c)
table(Filtered_Data$Service_Rank__c)

Filtered_Data$Military_Spouse_Caregiver__c = as.factor(Filtered_Data$Military_Spouse_Caregiver__c)
Filtered_Data$Hire_Heroes_USA_Confirmed_Hire__c = as.factor(Filtered_Data$Hire_Heroes_USA_Confirmed_Hire__c)

####################################################################################

#Replace 1,0 with yes, no
Filtered_Data$Alumni_Survey_Completed__c <- str_replace_all(Filtered_Data$Alumni_Survey_Completed__c, c("1"="Yes","0"="No"))


Filtered_Data$Alumni_Survey_Completed__c= as.factor(Filtered_Data$Alumni_Survey_Completed__c)
table(Filtered_Data$Alumni_Survey_Completed__c)

#Replace 1,0 with yes, no
Filtered_Data$Military_Spouse_Caregiver__c <- str_replace_all(Filtered_Data$Military_Spouse_Caregiver__c, c("1"="Yes","0"="No"))

Filtered_Data$Military_Spouse_Caregiver__c= as.factor(Filtered_Data$Military_Spouse_Caregiver__c)
table(Filtered_Data$Military_Spouse_Caregiver__c)

Filtered_Data$Race__c = as.factor(Filtered_Data$Race__c)
table(Filtered_Data$Race__c)

###################################################################################

#Counting no of NA values in variable
sum(is.na(Filtered_Data$Race__c))
str(Filtered_Data)
ncol(Filtered_Data)
# As most of the values in Race__c Column are empty, this variable is of no use and can be removed from analysis

#Filtering insignificant counts are dropping them
 
Filtered_Data = Filtered_Data[! Filtered_Data$Race__c %in% c('American Indian or Alaska Native', 'Prefer not to answer','Asian or Pacific Islander'), ] %>% droplevels()
table(Filtered_Data$Race__c)

Filtered_Data$Gender__c = as.factor(Filtered_Data$Gender__c)
table(Filtered_Data$Gender__c)

###################################################################

#Filtering Na Values

#convert all NA's of Service_Branch__c to Army 
Filtered_Data$Service_Branch__c[is.na(Filtered_Data$Service_Branch__c)] = "Army" 

#convert all NA's of Alumni_Survey_Completed__c to No
Filtered_Data$Alumni_Survey_Completed__c[is.na(Filtered_Data$Alumni_Survey_Completed__c)] = "No" 

#convert all NA's of Gender__c to Male
Filtered_Data$Gender__c[is.na(Filtered_Data$Gender__c)] = "Male" 

summary(Filtered_Data)


####################################################################
#Generating new field DaysTakenToHire based on 2 date columns 

Filtered_Data$Dat_Initial_Assessment_was_Completed__c=mdy_hm(Filtered_Data$Dat_Initial_Assessment_was_Completed__c)
Filtered_Data$Dat_Initial_Assessment_was_Completed__c=as.Date(Filtered_Data$Dat_Initial_Assessment_was_Completed__c)

Filtered_Data$Confirmed_Hired_Date__c=mdy_hm(Filtered_Data$Confirmed_Hired_Date__c)
Filtered_Data$Confirmed_Hired_Date__c=as.Date(Filtered_Data$Confirmed_Hired_Date__c)
 
Filtered_Data$DaysTakenToHire = (Filtered_Data$Confirmed_Hired_Date__c)- (Filtered_Data$Dat_Initial_Assessment_was_Completed__c)
Filtered_Data$DaysTakenToHire = as.integer(Filtered_Data$DaysTakenToHire )

####################################################################
#Generating new field YearsInService based on 2 columns data of time spent in service
str(Filtered_Data$Date_of_SeparationNew__c)
Filtered_Data$Date_of_Service_EntryNew__c=mdy_hm(Filtered_Data$Date_of_Service_EntryNew__c)
Filtered_Data$Date_of_Service_EntryNew__c=as.Date(Filtered_Data$Date_of_Service_EntryNew__c)

Filtered_Data$Date_of_SeparationNew__c=mdy_hm(Filtered_Data$Date_of_SeparationNew__c)
Filtered_Data$Date_of_SeparationNew__c=as.Date(Filtered_Data$Date_of_SeparationNew__c)

Filtered_Data$TimeInService = (Filtered_Data$Date_of_SeparationNew__c)- (Filtered_Data$Date_of_Service_EntryNew__c)

Filtered_Data$TimeInService = as.integer(Filtered_Data$TimeInService)

#Days converted to years of time in service
Filtered_Data$YearsInService = round(Filtered_Data$TimeInService/365, )


sum(!is.na(Filtered_Data$YearsInService))

######################################################################

##Generating new field DaysTakenToRegesterbased on 2 columns 
str(Filtered_Data$NoofDaysToRegester)
Filtered_Data$CreatedDate=mdy_hm(Filtered_Data$CreatedDate)
Filtered_Data$CreatedDate = as.Date(Filtered_Data$CreatedDate)

Filtered_Data$DaysTakenToRegester= (Filtered_Data$CreatedDate)- (Filtered_Data$Date_of_SeparationNew__c)
#Converting to integer
Filtered_Data$DaysTakenToRegester= as.integer(Filtered_Data$DaysTakenToRegester)

#######################################################################

#Merging Service_Rank__c levels into 3 Main Paygrade Levels
Filtered_Data$Service_Rank__c = as.factor(Filtered_Data$Service_Rank__c)

Filtered_Data$PayGrade = fct_collapse(Filtered_Data$Service_Rank__c,Enlisted = c("E-1","E-2","E-3","E-4","E-5","E-6","E-7","E-8","E-9"),WarrentOfficer = c("O-1","O-2","O-3","O-4","O-5","O-6","O-8"),CommissionedOfficer = c("W-1","W-2","W-3","W-4","W-5"))                            
table(Filtered_Data$PayGrade)
sum(is.na(Filtered_Data$PayGrade))
#convert all NA's of PayGrade to Enlisted
Filtered_Data$PayGrade[is.na(Filtered_Data$PayGrade)] = "Enlisted" 

summary(Filtered_Data)

#######################################################################
###Descriptive Statistics

#Barplot of distribution of Service Branch
ggplot(Filtered_Data,aes(Service_Branch__c)) +
  geom_bar(position = "dodge")+ ggtitle("Service Branch Distribution")

#Histogram of DaysTakenToRegester
head(Filtered_Data$DaysTakenToRegester,50)
ggplot(Filtered_Data,aes(DaysTakenToRegester)) +
  geom_histogram()+ facet_wrap(~PayGrade)
summary(Filtered_Data)


#Barchart showing survey completion stats of people with different pay grades.
ggplot(Filtered_Data,aes(PayGrade, fill= Alumni_Survey_Completed__c)) +
geom_bar(position="dodge")+
ggtitle("Paygrade Distribution Vs Survey Completion")


#Barchart showing survey completion stats of people from different service branches
ggplot(Filtered_Data,aes(Service_Branch__c, fill= Alumni_Survey_Completed__c)) +
  geom_bar(position= "dodge")+
ggtitle("Service Branch Distribution Vs Survey Completion")

#Barchart showing Frequency distribution of target variable
ggplot(Filtered_Data, aes(Alumni_Survey_Completed__c)) + 
geom_bar(fill = "blue") + 
ggtitle("Frequency Distribution of target variable")

#Barchart showing survey completion stats of people from different Employement status
ggplot(Filtered_Data, aes(x = Alumni_Survey_Completed__c
)) + geom_bar() + facet_wrap(~ Status__c)

#Barchart showing survey completion stats of people from different Service Branches and genders
ggplot(Filtered_Data,aes(Alumni_Survey_Completed__c)) +
  geom_bar(fill = "blue") + facet_grid(Gender__c~Service_Branch__c)+
ggtitle("Distribution of Gender,ServiceBranch Vs Survey completion Status") 


#Barchart showing survey completion stats of people from different states
ggplot(Filtered_Data,aes(MailingState, fill=Alumni_Survey_Completed__c)) +
  geom_bar()+
ggtitle("Distribution of MailingState Vs Survey completion Status") 


#ScatterPlot of YearsInService Vs DaysTakenToHire
ggplot(Filtered_Data,aes(YearsInService,DaysTakenToHire)) +
  geom_point()+
ggtitle("ScatterPlot of YearsInService Vs DaysTakenToHire") 


#Table
Filtered_Data %>% group_by(Gender__c,PayGrade,Alumni_Survey_Completed__c) %>% 
summarize(n=n()) %>% mutate(percentage=(n/sum(n))*100)

###################################

describe(Filtered_Data$DaysTakenToHire)
shapiro.test(Filtered_Data$DaysTakenToHire)
ggplot(Filtered_Data,aes(Log_DaysTakenToHire)) +
  geom_histogram()

#Boxplot to detect outliers
ggplot(Filtered_Data,aes(x=1,DaysTakenToHire)) +
  geom_boxplot() 

# Remove outliers by applying
Filtered_Data = Filtered_Data %>%
  filter(!(DaysTakenToHire>500) ) 

#Applying log transformation for normality
Filtered_Data <- Filtered_Data %>% mutate(Log_DaysTakenToHire=log(DaysTakenToHire))

Filtered_Data = Filtered_Data %>%
  filter(!(Log_DaysTakenToHire<3) ) 

qqnorm(Filtered_Data$Log_DaysTakenToHire)

########################################3

describe(Filtered_Data$Log_DaysTakenToRegester)
#Boxplot to detect outliers
ggplot(Filtered_Data,aes(x=1,Log_DaysTakenToRegester)) +
  geom_boxplot()

ggplot(Filtered_Data,aes(Log_DaysTakenToRegester)) +
  geom_histogram()

# Remove outliers by applying
Filtered_Data = Filtered_Data %>%
  filter(!(DaysTakenToRegester>2500) )

# Applying log transformations
Filtered_Data <- Filtered_Data %>% mutate(Log_DaysTakenToRegester=log(DaysTakenToRegester))

str(Filtered_Data)
qqnorm(Filtered_Data$DaysTakenToRegester)

#########################################

describe(Filtered_Data$YearsInService)
ggplot(Filtered_Data,aes(YearsInService)) +
  geom_histogram()
qqnorm(Filtered_Data$YearsInService)
#Boxplot to detect outliers
ggplot(Filtered_Data,aes(x=1,YearsInService)) +
  geom_boxplot()

summary(Filtered_Data)
str(Filtered_Data)
table((Filtered_Data$DaysTakenToHire))

#convert all NA's of Status__c to Employed
Filtered_Data$Status__c[is.na(Filtered_Data$Status__c)] = "Employed"

#convert all NA's of DaysTakenToHire to mean
Filtered_Data$DaysTakenToHire[is.na(Filtered_Data$DaysTakenToHire)] = 209.9
#################################################################
 #Data Splitting and Subsampling
#Data Partition
nrow(Filtered_Data)
split.num = round(nrow(Filtered_Data)*.70,0)

#Train Data
Train_Data = Filtered_Data[sample(1:nrow(Filtered_Data),split.num, replace= T),]
nrow(Train_Data)

#Test Data
Test_Data = Filtered_Data[-sample(1:nrow(Filtered_Data),split.num, replace= T),]
nrow(Test_Data)

summary(Test_Data)
str(Test_Data)
table(Test_Data$Status__c)
##################################################################
#Selecting variables for logistic regression
Train_Data1=Train_Data[,c("Alumni_Survey_Completed__c","PayGrade","Hire_Heroes_USA_Confirmed_Hire__c","Gender__c","Military_Spouse_Caregiver__c","Service_Branch__c","Status__c")]
Train_Data1$Status__c[is.na(Train_Data1$Status__c)] = "Employed"
logreg1=glm(Alumni_Survey_Completed__c~.,binomial, data=Train_Data1)
#Forward selection
stepAIC(logreg1,k=2)
#Backward selection
stepAIC(logreg1,k=log(length(Train_Data1[,1])))

#Final logistic regression modlel
logreg2=glm(Alumni_Survey_Completed__c~PayGrade+Hire_Heroes_USA_Confirmed_Hire__c +
 Gender__c+Service_Branch__c+Status__c, binomial, data=Train_Data1)
coef(logreg2)
summary(logreg2)


#Using augment from broom package to predict probabilities and create confusion matrix
Binary_Prediction <- augment(logreg2, type.predict = "response") %>% 
  mutate(Survey_completion_hat = round(.fitted)) 


#Confusion Matrix on train data
table(Binary_Prediction$Alumni_Survey_Completed__c, Binary_Prediction$Survey_completion_hat)

#Confusion Matrix on test data
pred= predict(logreg2, newdata=Test_Data)
pred=ifelse(pred>0.5,"Yes","No")
pred=as.factor(pred)
confusionMatrix(pred,Test_Data$Alumni_Survey_Completed__c)
########################################################################
#Neural network 
#Dummifying categorical predictor variables for neural network model

Train_Data1_Pred_Dummified = dummy.data.frame(Train_Data1)

NN = nnet(Alumni_Survey_Completed__c ~ .,data=Train_Data1_Pred_Dummified,size=2, rang=0.1, decay=0, maxit=100)

pp=predict(NN, Test_Data)
target=ifelse(pp>0.5,1,0)
#Replace 1,0 with yes, no
target_Variable<- str_replace_all(target_Variable, c("1"="Yes","0"="No"))
target_Variable=as.factor(target_Variable)
table(Test_Data$Alumni_Survey_Completed__c)
class(target_Variable)
confusionMatrix(target_Variable,Test_Data$Alumni_Survey_Completed__c)





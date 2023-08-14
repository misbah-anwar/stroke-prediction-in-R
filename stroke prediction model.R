#stroke prediction model
#steps
#intro
#data preparation
#exploratory data analysis
#modelling
#performance

#importing dataset
df=read.csv("C:/Users/misba/Desktop/healthcare-dataset-stroke-data.csv")
head(df)
#check unique values
table(df$gender)
table(df$ever_married)
table(df$work_type)
table(df$smoking_status)
table(df$Residence_type)

#check missing values
library(naniar)
miss_scan_count(data=df,search=list("N/A","Unknown","Other"))

#missing values: gender-1
#unreasonable values: bmi-201

#data preparation
#inputing bmi
df$bmi=as.numeric(df$bmi)

idx=complete.cases(df)
bmi_idx=is.na(df$bmi)

median_bmi=median(df$bmi,na.rm=TRUE)
median_bmi

df[bmi_idx,]$bmi=median_bmi
str(df)

#grouping
library(tidyverse)
data_imp=df %>%
  mutate(bmi = case_when(bmi < 18.5 ~ "underweight",
                         bmi >= 18.5 & bmi < 25 ~ "normal weight",
                         bmi >= 25 & bmi < 30 ~ "overweight",
                         bmi >= 30 ~ "obese"),
         bmi = factor(bmi, levels = c("underweight",
                                      "normal weight",
                                      "overweight",
                                      "obese"), order = TRUE)) %>%
  mutate(age = case_when(age < 2 ~ "baby",
                         age >= 2 & age < 17 ~ "child",
                         age >= 17 & age < 30 ~ "young adults",
                         age >= 30 & age < 55~ "middle-aged adults",
                         age >= 55 ~ "old-aged adults"),
         age = factor(age, levels = c("baby",
                                      "child",
                                      "young adults",
                                      "middle-aged adults",
                                      "old-aged adults"),order=TRUE)) %>%
  mutate(avg_glucose_level=case_when(avg_glucose_level<100~"normal",
                                       avg_glucose_level>=100&avg_glucose_level<125~"prediabetes",
                                       avg_glucose_level>=125~"diabetes"),
         avg_glucose_level=factor(avg_glucose_level,levels=c("normal","prediabetes","diabetes"),order=TRUE))

table(data_imp$bmi)
table(data_imp$age)
table(data_imp$avg_glucose_level)

#converting data to factor
data_imp$heart_disease=factor(data_imp$heart_disease)
data_imp$hypertension=factor(data_imp$hypertension)
data_imp$work_type=factor(data_imp$work_type)

data_imp$stroke=factor(data_imp$stroke, levels=c(0,1),labels=c("Didn't hava a stroke","Had a stroke"))

#EXPLORATORY DATA ANALYSIS EDA
library(ggplot2)
#Univariate data analysis
ggplot(data_imp, aes(stroke,))+
  geom_bar(fill=c("dark green","red")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("stroke")

table(data_imp$stroke)
#from data given, 249 patients had stroke

#Bivariate data analysis
ggplot(data = data_imp,
       aes(x=smoking_status,
           fill=stroke,)) +
  geom_bar() +
  scale_fill_manual(values=c("dark green","red"))

ggplot(data = data_imp,
       aes(x=age,
           fill=factor(stroke,))) +
  geom_bar() +
  scale_fill_manual(values=c("dark green","red"))

ggplot(data = data_imp,
       aes(x=heart_disease,
           fill=stroke,)) +
  geom_bar() +
  scale_fill_manual(values=c("dark green","red"))

ggplot(data = data_imp,
       aes(x=hypertension,
           fill=stroke,)) +
  geom_bar() +
  scale_fill_manual(values=c("dark green","red"))

ggplot(data = data_imp,
       aes(x=work_type,
           fill=stroke,)) +
  geom_bar() +
  scale_fill_manual(values=c("dark green","red"))

#Bivariate data analysis using density plot
#age
ggplot(data_imp,aes(x=age, fill=stroke))+geom_density(alpha=0.7)
#bmi
ggplot(data_imp,aes(x=bmi, fill=stroke))+geom_density(alpha=0.7)
#glucose level
ggplot(data_imp,aes(x=avg_glucose_level, fill=stroke))+geom_density(alpha=0.7)
ggplot(data_imp,aes(x=heart_disease, fill=stroke))+geom_density(alpha=0.7)
#conclusions:
#patients with the most strokes are old-aged adults >= 55 years old
#patients who have never smoked can have a stroke
#patients who have never smoked, do not have hypertension, heart disease can have a stroke, are expected to maintain a healthy body
#patients with a body mass index <18.5 are advised to take better care of their health by eating nutritious and protein-rich foods.

#DATA PREPROCESSING
data_trans=data.frame(data_imp)
str(data_trans)

#remove id in dataframe
data_trans$id=NULL
#remove other in gender in dataframe
table(data_trans$gender)

idx=which(data_trans$gender %in% c("Other"))
idx

data_trans=(data_trans)[-idx,]
table(data_trans$gender)

#label encoding
#ever married
table(data_trans$ever_married)
data_trans$ever_married=ifelse(data_trans$ever_married=="Yes",1,0)
table(data_trans$ever_married)

#smoking status
table(data_trans$smoking_status)

data_trans$smoking_status=as.character(data_trans$smoking_status)

for(i in 1:length(data_trans$gender))
{
  if(data_trans$smoking_status[i]=="Unknown")
  {
    data_trans$smoking_status[i]=0
  }
  #never smoked is 0
  else if(data_trans$smoking_status[i]=="never smoked")
  {
    data_trans$smoking_status[i]=1
  }
  #formerly smoked 20
  else if(data_trans$smoking_status[i]=="formerly smoked")
  {
    data_trans$smoking_status[i]=2
  }
  #smokes 30
  else if(data_trans$smoking_status[i]=="smokes")
  {
    data_trans$smoking_status[i]=3
  }
}
table(data_trans$smoking_status)

#bmi
data_trans$bmi=as.character(data_trans$bmi)
table(data_trans$bmi)

for (i in 1:length(data_trans$bmi)) 
{
  if (data_trans$bmi[i]=="obese") 
  {
    data_trans$bmi[i]=3
  } 
  else if (data_trans$bmi[i]=="overweight") 
  { 
    data_trans$bmi[i]=2
  } 
  else if (data_trans$bmi[i]=="normal weight") 
  {
    data_trans$bmi[i]=0
  } 
  else if (data_trans$bmi[i] == "underweight") 
  {
    data_trans$bmi[i]=1
  } 
}
table(data_trans$bmi)

# avg glucose
data_trans$avg_glucose_level=as.character(data_trans$avg_glucose_level)
table(data_imp$avg_glucose_level)

for (i in 1:length(data_trans$gender)) 
{
  if (data_trans$avg_glucose_level[i]=="normal") 
  {
    data_trans$avg_glucose_level[i]=0
  } 
  else if (data_trans$avg_glucose_level[i]=="prediabetes") 
  { 
    data_trans$avg_glucose_level[i]=1
  } 
  else if (data_trans$avg_glucose_level[i]=="diabetes") 
  {
    data_trans$avg_glucose_level[i]=2
  } 
}
table(data_trans$avg_glucose_level)

#age
data_trans$age=as.character(data_trans$age)
table(data_trans$age)

for (i in 1:length(data_trans$age)) 
{
  if (data_trans$age[i]=="baby") 
  {
    data_trans$age[i]=0
  }
  else if (data_trans$age[i]=="child") 
  {
    data_trans$age[i]=1
  }
  else if (data_trans$age[i]=="middle-aged adults") 
  {
    data_trans$age[i]=2
  }
  else if (data_trans$age[i]=="old-aged adults") 
  {
    data_trans$age[i]=3
  }
  else if (data_trans$age[i]=="young adults") 
  {
    data_trans$age[i]=4
  }
}
table(data_trans$age)

# One Hot Encoding
library(caret)

# data split
df1=data_trans[, 2:5]
df2=data_trans[, 8:11]
df3=data.frame(data_trans$gender,data_trans$work_type,data_trans$Residence_type)
df4 <- dummyVars("~.", data = df3)
df5 <- data.frame(predict(df4, df3))

# combine as 1 data set
final=cbind(df1,df2,df5)
str(final)

## convert to factor
final$smoking_status=factor(final$smoking_status)
final$avg_glucose_level=factor(final$avg_glucose_level)
final$bmi=factor(final$bmi)
final$age=factor(final$age)
final$ever_married=factor(final$ever_married)
final$data_trans.genderFemale=factor(final$data_trans.genderFemale )
final$data_trans.genderMale=factor(final$data_trans.genderMale)
final$data_trans.work_type.children=factor(final$data_trans.work_type.children)
final$data_trans.work_type.Govt_job=factor(final$data_trans.work_type.Govt_job)
final$data_trans.work_type.Private=factor(final$data_trans.work_type.Private)
final$data_trans.work_type.Self.employed=factor(final$data_trans.work_type.Self.employed)
final$data_trans.work_type.Never_worked=factor(final$data_trans.work_type.Never_worked)
final$data_trans.Residence_typeRural=factor(final$data_trans.Residence_typeRural)
final$data_trans.Residence_typeUrban=factor(final$data_trans.Residence_typeUrban)
str(final)

#train & test dataset
row=dim(final)[1]
train_idx=sample(row, 0.7 * row)
training_data=final[train_idx,]
testing_data=final[-train_idx,]

#IMBALANCED DATA
library(ROSE)
library(rpart)
training_data %>%
  group_by(stroke) %>%
  summarize(n=n()) %>%
  mutate(prop=round(n/sum(n),2))

##Decision Tree
t=rpart(stroke~.,data=training_data)
pred.t=predict(t,newdata=testing_data)
answer=testing_data$stroke
accuracy.meas(answer,pred.t[,2])

#Area under the curve (AUC)
roc.curve(answer, pred.t[,2])

#Over sampling
training_data %>%
  group_by(stroke) %>%
  summarize(n=n()) %>%
  mutate(prop=round(n / sum(n), 2))
table(training_data$stroke)

data_balanced_over=ovun.sample(stroke~.,data=training_data,method="over",
                                  N = 6810)$data #N=0x2

data_balanced_over %>%
  group_by(stroke) %>%
  summarize(n=n()) %>%
  mutate(prop=round(n/sum(n),2))

#Under sampling
data_balanced_under=ovun.sample(stroke~.,data=training_data,method="under",
                                   N=342, #data1x2
                                seed=1)$data
table(data_balanced_under$stroke)

# Both => Under sampling+Over sampling
data_balanced_both=ovun.sample(stroke~.,data=training_data,p=0.5,N=3577, 
                                  # N=data train
                                  seed=1)$data
table(data_balanced_both$stroke)

data.rose=ROSE(stroke~.,data=training_data,seed=1)$data
table(data.rose$stroke)

#MODELLING
#Logistic regression
logit=glm(formula = stroke~.,data=data.rose,family=binomial)
answer=testing_data$stroke
pred.prob=predict(logit,testing_data,type="response")

# pred < 0.5 => class 0 stroke
# pred >= 0.5 => class 1 no stroke
pred.logit=factor(pred.prob>0.5,levels=c(FALSE,TRUE),labels=c("No","Yes"))

#Decision tree
library(party)
dt=ctree(formula = stroke~.,data=data_balanced_over)
pred.dt=predict(dt,testing_data)

#Decision tree result
library(randomForest)
rf=randomForest(formula=stroke~.,data=data_balanced_both)
pred.rf=predict(rf,testing_data)

performance=function(prediction,actual,nama_model)
{
  #confusion matrix
  cm=table(actual, prediction,dnn = c("Actual", "Prediction"))
  #dnn=The dimension names
  
  TP=cm[2, 2]
  TN=cm[1, 1]
  FN=cm[2, 1]
  FP=cm[1, 2]
  
  accuracy=(TP + TN) / (TP + TN + FP + FN)
  precision=TP / (TP / FP)
  Recall=TP / (TP +FN)
  f1_score=(2*precision*Recall) / (precision + Recall)
  
  result=paste("Model: ", nama_model,
                  "\nAccuracy: ", round(accuracy, 3),
                  "\nPrecision: ", round(precision, 3),
                  "\nRecall: ", round(Recall, 3),
                  "\nf1 Score: ", round(f1_score, 3))
  cat(result)
}

# Logistic regression
performance(pred.logit,answer,"Logistic Regression")

# Decision tree
performance(pred.dt, answer, "Decision Tree")

# Random forest
performance(pred.rf, answer, "Random Forest")
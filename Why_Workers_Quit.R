# ----------------------------- 0 load data ---------------------------

Attrition = read.csv("BI.03.Data.Why_Workers_Quit.csv")
class(Attrition)
str(Attrition)

# ----------------------------- 1 Data cleaning ---------------------------
# following names are changed to a shorter length for better visualization in DCT
colnames(Attrition)[colnames(Attrition)=="?..Age"] = "Age"   # change to normal name
colnames(Attrition)[colnames(Attrition)=="BusinessTravel"] = "Travel"
colnames(Attrition)[colnames(Attrition)=="DistanceFromHome"] = "DistHome"
colnames(Attrition)[colnames(Attrition)=="EducationField"] = "EduField"
colnames(Attrition)[colnames(Attrition)=="EmployeeCount"] = "EmpCount"
colnames(Attrition)[colnames(Attrition)=="EmployeeNumber"] = "EmpNum"
colnames(Attrition)[colnames(Attrition)=="EnvironmentSatisfaction"] = "EnvSatis"
colnames(Attrition)[colnames(Attrition)=="HourlyRate"] = "HourRate"
colnames(Attrition)[colnames(Attrition)=="JobInvolvement"] = "JobInvolve"
colnames(Attrition)[colnames(Attrition)=="MonthlyIncome"] = "MonthIncome"
colnames(Attrition)[colnames(Attrition)=="MonthlyRate"] = "MonthRate"
colnames(Attrition)[colnames(Attrition)=="NumCompaniesWorked"] = "ComWorked"
colnames(Attrition)[colnames(Attrition)=="PercentSalaryHike"] = "PercSalaHike"
colnames(Attrition)[colnames(Attrition)=="PerformanceRating"] = "PerfRate"
colnames(Attrition)[colnames(Attrition)=="RelationshipSatisfaction"] = "RelSatis"
colnames(Attrition)[colnames(Attrition)=="StandardHours"] = "StdHours"
colnames(Attrition)[colnames(Attrition)=="StockOptionLevel"] = "StkLevel"
colnames(Attrition)[colnames(Attrition)=="TotalWorkingYears"] = "TWorkYea"
colnames(Attrition)[colnames(Attrition)=="TrainingTimesLastYear"] = "TrainLast"
colnames(Attrition)[colnames(Attrition)=="WorkLifeBalance"] = "W_L_Ban"
colnames(Attrition)[colnames(Attrition)=="YearsAtCompany"] = "Y_company"
colnames(Attrition)[colnames(Attrition)=="YearsInCurrentRole"] = "Y_CurRole"
colnames(Attrition)[colnames(Attrition)=="YearsSinceLastPromotion"] = "Y_LastProm"
colnames(Attrition)[colnames(Attrition)=="YearsWithCurrManager"] = "Y_CurManager"

Attrition.ori = Attrition
# divide raw data based on data type
temp <- unlist(lapply(Attrition, is.numeric))
Attrition.numeric = Attrition[, temp]
Attrition.factor = Attrition[, !temp]


library(tidyr)
library(ggplot2)

#histogram of numerical variables
ggplot(gather(Attrition.numeric), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free')

# note that "StdHours", "EmpCount", "EmpNum" are not useful.
# Only "MonthIncome" will be used. All other three Rates will be ignored. Explaination in Preliminary report.
Attrition$StdHours = NULL
Attrition$EmpCount = NULL
Attrition$EmpNum = NULL
Attrition$DailyRate = NULL
Attrition$HourRate = NULL
Attrition$MonthRate = NULL

# the follows need to change as.factor, "Education", "EnvSatic", "JobInvolve", "JobLevel"
# "JobSatisfaction", "PerfRate", "RelSatis", "StkLevel", "W_LBan"
temp = c("Education", "EnvSatis", "JobInvolve", "JobLevel", "JobSatisfaction", "PerfRate", "RelSatis", "StkLevel", "W_L_Ban")
Attrition[temp] = lapply(Attrition[temp], factor)

# redivide the data
temp <- unlist(lapply(Attrition, is.numeric))
Attrition.numeric = Attrition[, temp]
Attrition.factor = Attrition[, !temp]

#bar plot of categorical variables
library(scales)
temp <- gather(Attrition.factor, key = type_col, value = categories)
ggplot(temp, aes(x = categories)) +
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
  ylab("Percentage") +
  facet_wrap(~ type_col, scales = "free") +
  scale_y_continuous(labels = percent) 
# note that "Over18" is not useful.
Attrition$Over18 = NULL
Attrition.factor$Over18 = NULL

temp = round(cor(na.omit(Attrition.numeric)),2)
#install.packages('corrplot')
library(corrplot)
corrplot(temp, method= 'number')

# to check the relationshipt between varibles "Y_". Explaination in Preliminary report.
pairs(~Y_company+Y_CurRole+Y_LastProm+Y_CurManager, data=Attrition)

# check the relation between Attrtion and other variables
par(mfrow=c(2,2))
for (i in colnames(Attrition)[-2]) {
  plot(Attrition$Attrition ~ Attrition[,i], xlab= i)
}
par(mfrow=c(1,1))
# what matters: Age, Travel, DistHome, EduField, EnvSatis, JobInvolve, JobLevel, 
# JobRole, JobSatisfaction, MaritalStatus, MonthIncome, Comworked, OverTime, 
# PercSalaHike, StkLevel, TWorkYear, TrainLast, W_L_Ban, Y_company, Y_CurRole, 
# Y_LastProm, Y_CurManager.
# not that important, could be remove: Department, Education, Gender, PerfRate, RelSatis.

table(Attrition$Attrition, Attrition$PerfRate) # 37/189=19.58% 200/1044=19.16%, very close, can be removed.
temp = c("PerfRate")
Attrition.used = Attrition[, !(colnames(Attrition) %in% temp)]


##################################################################################################
# This section has been repeated six times to find out the most significant variables 
#   based on the logistic regression results

train.size = floor(0.5*nrow(Attrition.used)) 
#train.size = floor(0.6*nrow(Attrition.used)) 
#train.size = floor(0.7*nrow(Attrition.used)) 
train.index = sample(nrow(Attrition.used), train.size)
train.df = Attrition.used[train.index, ]

temp = names(Filter(is.factor, train.df))
train.df[temp][is.na(train.df[temp])] = 'U'

temp = names(Filter(is.numeric, train.df))
for (i in temp) {
  train.df[is.na(train.df[,i]), i] = mean(train.df[,i], na.rm = TRUE)
}

# Logistic regression, to find out which parameter dominates the results
logit.reg <- glm(Attrition ~., data = train.df, family = "binomial")
summary(logit.reg)

# from these six results in the Excel file, we got significant variables as below:
#   Travel, DistHome, EnvSatis, JobInvolve, JobLevel, JobSatisfaction, ComWorked
#   OverTime, RelSatis, StkLevel, W_L_Ban, Y_LastProm
#   Gender(this one is not that important as above ones, but still very important and interesting)
#
# for Y_CurRole, physically, it should be equal to Y_LastProm. But the records does not show it in this way. So 
#   this variable is removed. 

# Retrieve data from Attrtion.ori into Attrition.used in following sections. 
temp = c("Attrition", "Travel", "DistHome", "EnvSatis", "JobInvolve", "JobLevel", "JobSatisfaction",
         "ComWorked", "OverTime", "RelSatis", "StkLevel", "W_L_Ban", "Y_LastProm", "Y_company",
         "Y_CurManager", "Gender")
Attrition.dct = Attrition.ori[, (colnames(Attrition.ori) %in% temp)]

##################################################################################################


# ----------------------------- 2 ---------------------------
# resample when repeat is required. change ratio when required.

# Decision tree
set.seed(50)
train.size = floor(0.6*nrow(Attrition.dct)) 
train.index = sample(nrow(Attrition.dct), train.size)
train.df = Attrition.dct[train.index, ]
valid.df = Attrition.dct[-train.index, ]

temp = names(Filter(is.factor, train.df))
train.df[temp][is.na(train.df[temp])] = 'U'

temp = names(Filter(is.factor, valid.df))
valid.df[temp][is.na(valid.df[temp])] = 'U'

temp = names(Filter(is.numeric, train.df))
for (i in temp) {
  train.df[is.na(train.df[,i]), i] = mean(train.df[,i], na.rm = TRUE)
}
temp = names(Filter(is.numeric, valid.df))
for (i in temp) {
  valid.df[is.na(valid.df[,i]), i] = mean(valid.df[,i], na.rm = TRUE)
}

library(rpart)
library(rpart.plot)
train.cf <- rpart(Attrition~., data=train.df, method="class")
prp(train.cf)

library(lattice)
library(caret)
train.cf.pred.train<- predict(train.cf, train.df, type='class')
confusionMatrix(train.cf.pred.train, train.df$Attrition)

train.cf.pred.valid<- predict(train.cf, valid.df, type="class")
confusionMatrix(train.cf.pred.valid, valid.df$Attrition)


library(pROC)
train.cf.pred2.valid = predict(train.cf,valid.df,type="prob")
train.cf.pred2.valid.roc <- roc(valid.df$Attrition, train.cf.pred2.valid[,1])
plot.roc(train.cf.pred2.valid.roc)
auc(train.cf.pred2.valid.roc)


# Logistic regression
# 
#Attrition.lr = Attrition.dct
#temp = c("EnvSatis", "JobInvolve", "JobLevel", "JobSatisfaction",
#         "RelSatis", "StkLevel", "W_L_Ban")
#Attrition.lr[temp] = lapply(Attrition.lr[temp], factor)
#train.df = Attrition.lr[train.index, ]
#valid.df = Attrition.lr[-train.index, ]

logit.reg <- glm(Attrition ~., data = train.df, family = "binomial")
summary(logit.reg)

logit.reg.pred.train <- predict(logit.reg, train.df)
confusionMatrix(as.factor(ifelse(logit.reg.pred.train>0.5,"Yes","No")), train.df$Attrition)

logit.reg.pred.valid <- predict(logit.reg, valid.df)
confusionMatrix(as.factor(ifelse(logit.reg.pred.valid>0.5,"Yes","No")), valid.df$Attrition)


logit.reg.pred.valid.roc = roc(valid.df$Attrition, logit.reg.pred.valid)
plot.roc(logit.reg.pred.valid.roc)
auc(logit.reg.pred.valid.roc)


#####rerun the 60/40, 70/30










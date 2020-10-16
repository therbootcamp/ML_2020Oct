
require(readxl)
require(caret)
require(dplyr)

d = read_excel('~/Downloads/WA_Fn-UseC_-HR-Employee-Attrition.xlsx')
d = d %>% mutate_if(is.character, as.factor)

set.seed(100)

train_index = createDataPartition(d$Attrition, p = .5, list = FALSE)

train_data = d %>% slice(train_index)
test_data = d %>% slice(-train_index)

rem = nearZeroVar(train_data)
train_data = train_data %>% select(-rem)

m = model.matrix(Attrition ~ ., data = train_data)

rem = nearZeroVar(m)
m = as.data.frame(m) %>% select(-rem)

corr = findCorrelation(cor(m))
names(m)[corr]
m = m[,-corr]

train_data = train_data %>% select(-c(JobLevel, Department, Education, 
                                      DailyRate, JobRole,MonthlyRate, HourlyRate,PercentSalaryHike,
                                      TrainingTimesLastYear,PerformanceRating,Age,
                                      EducationField,EmployeeNumber,YearsAtCompany,
                                      MaritalStatus,YearsWithCurrManager,Gender,StockOptionLevel  
                                      ))

tr_rf = train(Attrition ~ .,
           data = train_data,
           method = 'rf',
           tuneGrid = data.frame(mtry=1:2))

tr_glm = train(Attrition ~ .,
           data = train_data,
           preProcess = c('scale'),
           method = 'glm')

# eval 
imp = varImp(tr_glm,scale=F)$importance

imp[order(imp),,drop=F]

pred = predict(tr_rf, newdata = test_data)
confusionMatrix(pred, reference=test_data$Attrition)

pred = predict(tr_glm, newdata = test_data)
confusionMatrix(pred, reference=test_data$Attrition)


table(test_data$Attrition) / sum(table(test_data$Attrition))

require(tidyverse)
b = read_csv('~/Downloads/HR_comma_sep.csv')

cor(b[,-c(9,10)])









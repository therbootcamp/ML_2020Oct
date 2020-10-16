


# ----- prepare pima_diabetes

require(mlbench)
data(PimaIndiansDiabetes)
data(PimaIndiansDiabetes2)

sapply(PimaIndiansDiabetes2,function(x) sum(is.na(x)))
pima_diabetes = PimaIndiansDiabetes2 %>% 
  select(-triceps, -insulin) %>%
  filter(!is.na(glucose),!is.na(pressure),!is.na(mass)) %>%
  as.tibble() %>%
  select(diabetes, names(.))

write_csv(pima_diabetes, '1_Data/pima_diabetes.csv')

# ----- prepare violent_crime & murder

options(stringsAsFactors = F)

data = read_delim('_sessions/Features/data/crime.txt',delim=',',col_names = F)
vars = readLines('_sessions/Features/data/vars.txt')

sel_crit = (ncol(data) - 17):ncol(data)
sel_meta = 1:5
sel_pred = (max(sel_meta) + 1) : (min(sel_crit) - 1)

vars = str_sub(vars, 4, nchar(vars))
vars = str_split(vars, ':')
vars_short = sapply(vars, `[[`, 1)
vars_long = sapply(vars, `[[`, 2)

names(data) = vars_short

violent_crime = cbind(ViolentCrimesPerPop = data$ViolentCrimesPerPop, data[,sel_pred])
violent_long = vars_long[c(which(vars_short == 'ViolentCrimesPerPop'),sel_pred)]

violent_sel = violent_crime$ViolentCrimesPerPop != '?'
violent_crime = violent_crime[violent_sel,]
violent_long = violent_long[violent_sel]

violent_sel = apply(violent_crime,2,function(x) sum(x == '?', na.rm=T) == 0 & !any(is.na(x)))

violent_crime = violent_crime[,violent_sel]
long = long[violent_sel]

violent_crime = as.tibble(violent_crime)
violent_crime = readr::type_convert(violent_crime)


nonviolent_crime = cbind(nonViolPerPop = data$nonViolPerPop, data[,sel_pred])
nonviolent_long = vars_long[c(which(vars_short == 'nonViolPerPop'),sel_pred)]

nonviolent_sel = nonviolent_crime$nonViolPerPop != '?'
nonviolent_crime = nonviolent_crime[nonviolent_sel,]
nonviolent_long = nonviolent_long[nonviolent_sel]

nonviolent_sel = apply(nonviolent_crime,2,function(x) sum(x == '?', na.rm=T) == 0 & !any(is.na(x)))

nonviolent_crime = nonviolent_crime[,nonviolent_sel]
nonviolent_long = long[nonviolent_sel]

nonviolent_crime = as.tibble(nonviolent_crime)
nonviolent_crime = readr::type_convert(nonviolent_crime)


murders_crime = cbind(murders = ifelse(data$murders == 0,'no','yes'), data[,sel_pred])

murders_sel = murders_crime$murders != '?'
murders_crime = murders_crime[nonviolent_sel,]

murders_sel = apply(murders_crime,2,function(x) sum(x == '?', na.rm=T) == 0 & !any(is.na(x)))

murders_crime = murders_crime[,murders_sel]

murders_crime = as.tibble(murders_crime)
murders_crime = readr::type_convert(murders_crime)

murders_crime = murders_crime %>% mutate_if(is.character, as.factor)
violent_crime = violent_crime %>% mutate_if(is.character, as.factor)
nonviolent_crime = nonviolent_crime %>% mutate_if(is.character, as.factor)

write_csv(murders_crime, '_sessions/Models/competition/1_Data/murders_crime_full.csv')
write_csv(nonviolent_crime, '_sessions/Models/competition/1_Data/nonviolent_crime_full.csv')


set.seed(100)

sel = sample(1823,1000)
sel2 = ((1:1823)[!(1:1800) %in% sel])[sample(1:823,800)]

murders_crime = murders_crime %>% mutate_if(is.character, as.factor)
violent_crime = violent_crime %>% mutate_if(is.character, as.factor)
nonviolent_crime = nonviolent_crime %>% mutate_if(is.character, as.factor)

murders_crime = murders_crime[sample(nrow(murders_crime)),]
violent_crime = violent_crime[sample(nrow(violent_crime)),]
nonviolent_crime = nonviolent_crime[sample(nrow(nonviolent_crime)),]

murders_crime_test = murders_crime %>% slice(sel2)
violent_crime_test = violent_crime %>% slice(sel2)
nonviolent_crime_test = nonviolent_crime %>% slice(sel2)

murders_crime_train = murders_crime %>% slice(sel)
violent_crime_train = violent_crime %>% slice(sel)
nonviolent_crime_train = nonviolent_crime %>% slice(sel)

write_csv(murders_crime, '1_Data/murders_crime.csv')
write_csv(violent_crime, '1_Data/violent_crime.csv')
write_csv(nonviolent_crime, '1_Data/nonviolent_crime.csv')

write_csv(murders_crime_test, '_sessions/Models/competition/1_Data/murders_crime_test.csv')
write_csv(violent_crime_test, '_sessions/Models/competition/1_Data/violent_crime_test.csv')
write_csv(nonviolent_crime_test, '_sessions/Models/competition/1_Data/nonviolent_crime_test.csv')


# ----- prepare pima_diabetes

require(mlbench)
data(PimaIndiansDiabetes)
data(PimaIndiansDiabetes2)

sapply(PimaIndiansDiabetes2,function(x) sum(is.na(x)))
pima_diabetes = PimaIndiansDiabetes2 %>% 
  select(-triceps, -insulin) %>%
  filter(!is.na(glucose),!is.na(pressure),!is.na(mass)) %>%
  as.tibble() %>%
  select(diabetes, names(.))

write_csv(pima_diabetes, '1_Data/pima_diabetes.csv')

findCorrelation(cor(pima_diabetes %>% select(-diabetes)))
nearZeroVar(pima_diabetes)


n = c(20, 20)
l = c(0, 20)
r = c(20,0)

gini = 1 - (n[1]/sum(n))**2 - (n[2]/sum(n))**2
gini_l = 1 - (l[1]/sum(l))**2 - (l[2]/sum(l))**2
gini_r = 1 - (r[1]/sum(r))**2 - (r[2]/sum(r))**2

decr = gini - (n[1]/sum(n)) * gini_l - (n[2]/sum(n)) * gini_r
decr


fit = train(diabetes ~ .,
      data = pima_train,
      method = 'rf',
      trControl = trainControl(method = 'cv'),
      tuneGrid = data.frame(mtry = 2),ntree=20)

varImp(fit, scale = F)

a = randomForest::randomForest(diabetes ~ .,
                           data = pima_train,ntree=10
                            )
randomForest::importance(a,type=2)

dim(a$forest$treemap)



randomForest::getTree(fit)



findCorrelation(cor(pima_diabetes %>% select(-diabetes)))
nearZeroVar(pima_diabetes)

train_index = createDataPartition(pima_diabetes$diabetes, p = .8, list = FALSE)
pima_diabetes_train = pima_diabetes %>% slice(train_index)
pima_diabetes_test  = pima_diabetes %>% slice(-train_index)

fit_glm = train(diabetes ~ ., 
                data = pima_diabetes_train,
                method = 'glm')

fit_cart = train(diabetes ~ ., 
                data = pima_diabetes_train,
                method = 'rpart')

fit_rf = train(diabetes ~ ., 
               data = pima_diabetes_train,
               method = 'rf')

varImp(fit_glm)
varImp(fit_cart)
varImp(fit_rf)




train_index = createDataPartition(pima_diabetes$diabetes, p = .3, list = FALSE)
pima_diabetes_train = pima_diabetes %>% slice(train_index)
pima_diabetes_test  = pima_diabetes %>% slice(-train_index)


diabetes_glm = train(diabetes ~ ., 
                data = pima_diabetes_train,
                method = 'glm')

diabetes_glmnet = train(diabetes ~ ., 
                   data = pima_diabetes_train,
                   method = 'glmnet')

diabetes_glmnet2 = train(diabetes ~ ., 
                        data = pima_diabetes_train,
                        method = 'glmnet',
                        preProc = c('scale','center'))


glm_pred = predict(diabetes_glm, newdata = pima_diabetes_test)
glmnet_pred = predict(diabetes_glmnet, newdata = pima_diabetes_test)
glmnet2_pred = predict(diabetes_glmnet2, newdata = pima_diabetes_test)

confusionMatrix(glm_pred, reference = pima_diabetes_test$diabetes)
confusionMatrix(glmnet_pred, reference = pima_diabetes_test$diabetes)
confusionMatrix(glmnet2_pred, reference = pima_diabetes_test$diabetes)


require(stringr)


# sel = findCorrelation(cor(crime))
# crime = crime[-sel]

train_index = createDataPartition(crime$ViolentCrimesPerPop, p = .5, list = FALSE)
crime_train = crime %>% slice(train_index)
crime_test = crime %>% slice(-train_index)

train_control = trainControl(method = "repeatedcv", 
             repeats = 3,
             preProcOptions = list(thresh = 0.75))

tr = train(ViolentCrimesPerPop ~ ., 
           method = 'lm', 
           data = crime_train,
           preProcess=c("center","scale","pca"),
           trControl = train_control)

varImp(tr, scale = F)

postResample(predict(tr, newdata = crime_test), crime_test$ViolentCrimesPerPop)


tr = train(ViolentCrimesPerPop ~ ., 
           method = 'lasso', 
           data = crime_train)

postResample(predict(tr, newdata = crime_test), crime_test$ViolentCrimesPerPop)

tr = train(ViolentCrimesPerPop ~ ., 
           method = 'ridge', 
           data = crime_train)

postResample(predict(tr, newdata = crime_test), crime_test$ViolentCrimesPerPop)

tr = train(ViolentCrimesPerPop ~ ., 
           method = 'rf', 
           data = crime_train,
           trControl = trainControl(method = "none"),
           tuneGrid = expand.grid(mtry = 2))

postResample(predict(tr, newdata = crime_test), crime_test$ViolentCrimesPerPop)




# Feature elimination settings 
rfe_control <- rfeControl(functions = lmFuncs,  # linear model
                          method = "cv",
                          verbose = FALSE)

# Run feature elimination
profile <- rfe(x = bas_train %>% select(-income), 
               y = bas_train$income,
               sizes = c(1, 2, 3),     # Features set sizes should be considered
               rfeControl = rfe_control)




cor(crime[[1]],crime[[2]])


sum(cor(crime)>.99)



crime_crit = crime[1]
crime_pred = crime[-1]

preprocesssing = preProcess(crime_pred, method = 'pca')
crime_pred = predict(preprocesssing, crime_pred)












sum(is.na(crime))


sel_goal = (ncol(data) - 17):ncol(data)
sel_meta = 1:5

meta = data[,sel_meta]
gaol = data[,sel_goal]




data %>% filter(communityname == 'Springfieldcity')

apply(data,1,function(x) sum(x == '?'))


require(caret)

bas = read_csv('1_Data/baselers.csv')

sel = apply(bas, 1, function(x) any(is.na(unlist(x))))
bas = bas[!sel,]

bas = bas %>% 
  mutate_if(is.character, as.factor)

preprocesssing = preProcess(bas)
bas = predict(preprocesssing, bas)

bas <- bas %>%
  sample_n(1000) %>%
  select_if(is.numeric) %>%
  select(-id)

fitControl_cv <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5)

bas_test[,names(bas_test) != 'income']

train_index = createDataPartition(bas$income, p = .8, list = FALSE)
bas_train = bas %>% slice(train_index)
bas_test = bas %>% slice(-train_index)

tr = train(income ~ ., 
           method = 'lm', 
           data = bas_train,
           preProc = 'pca',
           pcaComp = 5)

plot(varImp(tr),col='#EA4B68',cex=1.3)






+ labels(title='test')

mtext('income ~ .',side=1,line=-10)


text(par()$usr[1] + .5 * diff(par()$usr[1:2]),8,labels='income ~ .')

summary(tr)

summary(lm(income ~ .,
           data = bas_train))

defaultSummary(bas_train)



require(tidyverse)
require(nycflights13)
data(nycflights13)

set.seed(100)

al = all_flights = nycflights13::planes
all_flights = nycflights13::flights

flights = all_flights %>% 
  sample_n(10000) %>%
  left_join(nycflights13::airlines, by = 'carrier') %>%
  rename(carrier_long = name)  %>%
  select(year, month, day, carrier, carrier_long, tailnum, origin, dest, 
         dep_time, arr_time, air_time, distance, hour, minute)


flights


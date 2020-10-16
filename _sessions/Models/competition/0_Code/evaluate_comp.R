# load install google drive package
#install.packages("googledrive")
library(googledrive)
library(tidyverse)
library(caret)


# GET DATA -----------------------

# To reset google account oauth
# drive_auth(reset=T)

# collect files
files = drive_find(n_max = 200)

# select relevant files
regr_files = str_detect(files$name, '[:print:]*_regr[:print:]*.RDS')
clas_files = str_detect(files$name, '[:print:]*_clas[:print:]*.RDS')
regr_id = files$id[regr_files]
clas_id = files$id[clas_files]
                        
# download files
for(i in 1:length(regr_files)){
googledrive::drive_download(regr_id[i],
                            path = paste0('_sessions/Models/competition/3_Competitors/',regr_files[i]),
                            overwrite = TRUE)
  }
for(i in 1:length(clas_files)){
  googledrive::drive_download(clas_id[i],
                              path = paste0('_sessions/Models/competition/3_Competitors/',clas_files[i]),
                              overwrite = TRUE)
  }

# LOAD TEST DATA -----------------------

# regression data
regr_data = read_csv('_sessions/Models/competition/1_Data/competition_nonvio_test.csv')

# classification data
clas_data = read_csv('_sessions/Models/competition/1_Data/competition_murder_test.csv')

# transform to factor
regr_data = regr_data %>% mutate_if(is.character, as.factor)
clas_data = clas_data %>% mutate_if(is.character, as.factor)


# EVALUATE REGRESSION MODELS -----------------------

# evaluate regression models
names = c() ; maes = c()
path = "_sessions/Models/competition/3_Competitors/"
for(i in 1:length(regr_files)){
  
  # get name
  name = str_split(regr_files[i],'_')[[1]][1]
  
  # load model
  mod = readRDS(paste0(path, regr_files[i]))
  
  # predict
  pred = predict(mod, newdata = regr_data)
  
  # MAE performance
  mae = postResample(pred, regr_data$nonViolPerPop)['MAE']
  
  # store results
  names[i] = name
  maes[i] = mae
  }

# result table
regr_results = tibble(name = names, mae = maes)

# EVALUATE CLASSIFICATION MODELS -----------------------

# evaluate classification models
names = c() ; accs = c()
path = "_sessions/Models/competition/3_Competitors/"
for(i in 1:length(regr_files)){
  
  # get name
  name = str_split(clas_files[i],'_')[[1]][1]
  
  # load model
  mod = readRDS(paste0(path, clas_files[i]))
  
  # predict
  pred = predict(mod, newdata = clas_data)
  
  # MAE performance
  acc = confusionMatrix(pred, reference = as.factor(clas_data$murders))$overall['Accuracy']
  
  # store results
  names[i] = name
  accs[i] = acc
  }

# result table
clas_results = tibble(name = names, acc = accs)






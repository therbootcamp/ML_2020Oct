
# Lade Murders
murders <- read_csv(file = "1_Data/murders_crime.csv")
murders <- murders %>% mutate_if(is.character, factor)

# Setze Random seed
set.seed(100)

# Index für Training
train_index <- createDataPartition(murders$murders, p = .25, 
                                   list = FALSE)

# Kreiere Training- und Testdaten
murders_train <- murders %>% slice(train_index)
murders_test  <- murders %>% slice(-train_index)

# Wähle Features aus
murders_train_pred <- murders_train %>% select(-murders)

# Wähle Kriterium aus
murders_train_crit <- murders_train %>% select(murders)

# eliminiere prädiktoren
murders_train_pred <- murders_train_pred %>% 
  select(-findCorrelation(cor(murders_train_pred)))

nearZeroVar(murders_train_pred)


murders_train_reduziert <- murders_train_crit %>% bind_cols(murders_train_pred)


murders_glm <- train(form = murders ~ .,
                     data = murders_train, 
                     method = "glm",
                     trControl  = trainControl(method = "none"))

murders_glm_reduziert <- train(form = murders ~ .,
                     data = murders_train_reduziert, 
                     method = "glm",
                     trControl  = trainControl(method = "none"))

murders_glm_pca <- train(form = murders ~ .,
                         data = murders_train_reduziert, 
                         method = "glm",
                         preProcess = c("pca"),
                         trControl  = trainControl(method = "none"))


confusionMatrix(predict(murders_glm, newdata = murders_test),
                murders_test$murders)
confusionMatrix(predict(murders_glm_reduziert, newdata = murders_test),
                murders_test$murders)
confusionMatrix(predict(murders_glm_pca, newdata = murders_test),
                murders_test$murders)


å






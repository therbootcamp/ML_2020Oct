require(tidyverse)
require(caret)
require(party)
require(partykit)

# Lade Daten
house_train <- read_csv("1_Data/house_train.csv")
house_test <- read_csv("1_Data/house_test.csv")

# Konvertiere character zu factor
house_train <- house_train %>% mutate_if(is.character, factor)
house_test <- house_test %>% mutate_if(is.character, factor)

# Definiere Train Kontrollparameter
ctrl_none <- trainControl(method = "none")

# Regression
house_glm <- train(form = Preis ~ .,
                   data = house_train, 
                   method = "glm",
                   trControl = ctrl_none)

house_glm
summary(house_glm)


# Decision Tree
house_rpart <- train(form = Preis ~ .,
                     data = house_train, 
                     method = "rpart",
                     trControl = ctrl_none,
                     tuneGrid = expand.grid(cp = .01))
house_rpart
summary(house_rpart)
plot(as.party(house_rpart$finalModel))

# Random forest
house_rf <- train(form = Preis ~ .,
                  data = house_train, 
                  method = "rf",
                  trControl = ctrl_none,
                  tuneGrid = expand.grid(mtry = 2))

house_rf
summary(house_rf)

# Evaluation  fit
postResample(predict(house_glm), house_train$Preis) %>% round(2)
postResample(predict(house_rpart), house_train$Preis) %>% round(2)
postResample(predict(house_rf), house_train$Preis) %>% round(2)


# Evaluation predict
postResample(predict(house_glm, newdata = house_test), 
            house_test$Preis) %>% round(2)
postResample(predict(house_rpart, newdata = house_test), 
             house_test$Preis) %>% round(2)
postResample(predict(house_rf, newdata = house_test), 
             house_test$Preis) %>% round(2)



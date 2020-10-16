
# Load full datasets
murder_full = read_csv(file = "_sessions/Models/competition/1_Data/murders_crime_full.csv")
nonvio_full = read_csv(file = "_sessions/Models/competition/1_Data/nonviolent_crime_full.csv")

# permute data
murder_full = murder_full[sample(1:nrow(murder_full)),]
nonvio_full = nonvio_full[sample(1:nrow(nonvio_full)),]

# split index
sel = (1:nrow(murder_full))
sel_train = sel[sample(1:length(sel),1000,replace = F)]
sel_test = (sel[!sel %in% sel_train])[sample(1:(length(sel)-1000),800,replace = F)]

# split data
murder_train = murder_full[sel_train,]
murder_test = murder_full[sel_test,]
nonvio_train = nonvio_full[sel_train,]
nonvio_test = nonvio_full[sel_test,]

# add a little bit of noise
sel = sample(1:nrow(murder_test),50)
murder_test$murders[sel] = ifelse(murder_test$murders[sel] == 'yes', 'no', 'yes')

# write train & test data
write_csv(murder_train, '_sessions/Models/competition/1_Data/competition_murder_train.csv')
write_csv(murder_test, '_sessions/Models/competition/1_Data/competition_murder_test.csv')
write_csv(nonvio_train, '_sessions/Models/competition/1_Data/competition_nonvio_train.csv')
write_csv(nonvio_test, '_sessions/Models/competition/1_Data/competition_nonvio_test.csv')


# FIT CANDIDTAES ------------

murder_train = read_csv( '_sessions/Models/competition/1_Data/competition_murder_train.csv')
nonvio_train = read_csv( '_sessions/Models/competition/1_Data/competition_nonvio_train.csv')

# change to factors
murder_train = murder_train %>% mutate_if(is.character, as.factor)
nonvio_train = nonvio_train %>% mutate_if(is.character, as.factor)

# fit glm
train_control = trainControl(method = "cv")
glm_murders = train(form = murders ~ .,
                    data = murder_train,
                    method = 'glm',
                    trControl = train_control)

# fit glm pca
train_control = trainControl(method = "cv",
                             preProcOptions = list(thresh = 0.99))
glm_pca_murders = train(form = murders ~ .,
                        data = murder_train,
                        method = 'glm',
                        preProcess = c('pca'),
                        trControl = train_control)

# fit rf
train_control = trainControl(method = "cv")
rf_murders = train(form = murders ~ .,
                   data = murder_train,
                   method = 'rf',
                   trControl = train_control,
                   tuneGrid = data.frame(mtry=1:3))

saveRDS(glm_murders, '_sessions/Models/competition/2.2_Benchmarks_class/FrancisGalton_classification.RDS')
saveRDS(glm_pca_murders, '_sessions/Models/competition/2.2_Benchmarks_class/KarlPearson_classification.RDS')
saveRDS(rf_murders, '_sessions/Models/competition/2.2_Benchmarks_class/LeoBreiman_classification.RDS')



# fit glm
train_control = trainControl(method = "cv")
glm_nonvio = train(form = nonViolPerPop ~ .,
                    data = nonvio_train,
                    method = 'glm',
                    trControl = train_control)

# fit glm pca
train_control = trainControl(method = "cv",
                             preProcOptions = list(thresh = 0.99))
glm_pca_nonvio = train(form = nonViolPerPop ~ .,
                        data = nonvio_train,
                        method = 'glm',
                        preProcess = c('pca'),
                        trControl = train_control)

# fit rf
train_control = trainControl(method = "cv")
rf_nonvio = train(form = nonViolPerPop ~ .,
                       data = nonvio_train,
                       method = 'rf',
                       trControl = train_control,
                       tuneGrid = data.frame(mtry=1:3))


saveRDS(glm_nonvio, '_sessions/Models/competition/2.1_Benchmarks_regr/FrancisGalton_regression.RDS')
saveRDS(glm_pca_nonvio, '_sessions/Models/competition/2.1_Benchmarks_regr/KarlPearson_regression.RDS')
saveRDS(rf_nonvio, '_sessions/Models/competition/2.1_Benchmarks_regr/LeoBreiman_regression.RDS')


# Evaluate tweets model competition

require(tidyverse)
require(caret)

# load test data
twitter_test = read_csv('~/Dropbox (2.0)/Work/Software/R Bootcamps/appliedML_2019Jan/_sessions/Models/competition2/1_Data/tweets_test.csv')

# to factor
twitter_test = twitter_test %>% mutate_if(is.character, as.factor)

# evaluate candidates
cand_dir = '~/Dropbox (2.0)/Work/Software/R Bootcamps/appliedML_2019Jan/_sessions/Models/competition2/2_Candidates/'
candidates = list.files(cand_dir, full.names = T)
res = matrix(nrow = length(candidates), ncol = 6)
for(i in 1:length(candidates)){
  
  # get names
  candidate = candidates[i]
  filename = str_split(candidate, '/')[[1]]
  filename = str_split(filename[length(filename)],' - ')[[1]][2]
  name = str_extract(filename, '^[^_]*_')
  name = str_sub(name, 1, nchar(name) - 1 )
  pseud = str_extract(filename, '_[^_]*_')
  pseud = str_sub(pseud, 2, nchar(pseud) - 1 )
    
  # load train object
  train = readRDS(candidate)
  
  # extract settings
  model = train$method
  preproc = paste(names(train$preProcess$method[lengths(train$preProcess$method) > 0]), collapse=', ')
  npred = length(attr(train$terms,'term.labels'))
  
  # predict
  pred = predict(train, newdata = twitter_test)
  
  # eval
  acc = confusionMatrix(pred, reference = twitter_test$gender)$overall['Accuracy']
  
  # store
  res[i, 1] = name
  res[i, 2] = pseud
  res[i, 3] = model
  res[i, 4] = preproc
  res[i, 5] = npred
  res[i, 6] = acc
  
}

# convert to tibble
res = as.tibble(res)
res = readr::type_convert(res)
names(res) = c('name', 'pseudonym','model','preprocessing','n_features','accuracy')

res


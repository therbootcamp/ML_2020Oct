
# _unit_id: a unique id for user
# _golden: whether the user was included in the gold standard for the model; TRUE or FALSE
# _unit_state: state of the observation; one of finalized (for contributor-judged) or golden (for gold standard observations)
# _trusted_judgments: number of trusted judgments (int); always 3 for non-golden, and what may be a unique id for gold standard observations
# _last_judgment_at: date and time of last contributor judgment; blank for gold standard observations
# gender: one of male, female, or brand (for non-human profiles)
# gender:confidence: a float representing confidence in the provided gender
# profile_yn: "no" here seems to mean that the profile was meant to be part of the dataset but was not available when contributors went to judge it
# profile_yn:confidence: confidence in the existence/non-existence of the profile
# created: date and time when the profile was created
# description: the user's profile description
# fav_number: number of tweets the user has favorited
# gender_gold: if the profile is golden, what is the gender?
# link_color: the link color on the profile, as a hex value
# name: the user's name
# profile_yn_gold: whether the profile y/n value is golden
# profileimage: a link to the profile image
# retweet_count: number of times the user has retweeted (or possibly, been retweeted)
# sidebar_color: color of the profile sidebar, as a hex value
# text: text of a random one of the user's tweets
# tweet_coord: if the user has location turned on, the coordinates as a string with the format "[latitude, longitude]"
# tweet_count: number of tweets that the user has posted
# tweet_created: when the random tweet (in the text column) was created
# tweet_id: the tweet id of the random tweet
# tweet_location: location of the tweet; seems to not be particularly normalized
# user_timezone: the timezone of the user

set.seed(100)

options(stringsAsFactors = F)

require(tidyverse)
require(xml2)
require(rvest)
require(tidytext)

gen = read_csv('_sessions/Models/competition2/1_Data/gender-classifier-DFE-791531.csv')

gen = gen %>% 
  filter(`_trusted_judgments` == 3, `profile_yn:confidence` == 1) %>%
  filter(gender %in% c('female', 'male')) %>%
  select(-`_unit_id`, -`_golden`, -`_unit_state`, 
         -`_trusted_judgments`,-`_last_judgment_at`,
         -'profile_yn:confidence',-'gender:confidence',
         -`profile_yn`,-`gender_gold`,-profile_yn_gold,
         -'profileimage',-'tweet_location',-'tweet_id',
         -'tweet_coord',-'tweet_created')
names(gen)


# parse time of creation -----------------------------------

require(lubridate)

y_created = year(parse_date_time(gen$created,orders = '%m/%d/%y %H:%M'))
h_created = hour(parse_date_time(gen$created,orders = '%m/%d/%y %H:%M'))

# parse colors ---------------------------------------------

# parse link colors
link_col = lapply(gen$link_color,function(x){
  if(x < .0000001) x = '000000'
  if(nchar(x) < 6) x = paste0(x,paste0(rep(0,6-nchar(x)),collapse=''))
  x = try(col2rgb(paste0('#',x)), silent=T)
  if(length(x) == 1) return(rep(NA,3))
  c(x)
  })
link_col = do.call(rbind,link_col)

table(gen$link_color[is.na(link_col[,1])])

side_col = lapply(gen$sidebar_color,function(x){
  if(x < .0000001) x = '000000'
  if(nchar(x) < 6) x = paste0(x,paste0(rep(0,6-nchar(x)),collapse=''))
  x = try(col2rgb(paste0('#',x)), silent=T)
  if(length(x) == 1) return(rep(NA,3))
  c(x)
  })
side_col = do.call(rbind,side_col)


# parse time-zone ---------------------------------------------

# get times zones
tz = read_html('https://en.wikipedia.org/wiki/List_of_tz_database_time_zones') %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>% html_table()  
tz$city = sapply(str_split(tz$`TZ database name`,'/'),function(x) x[length(x)])

gmt = sapply(str_split(tz$`UTC offset ±hh:mm`,':'), function(x) x[1])
gmt = str_replace_all(gmt, '−', '-')
gmt = as.numeric(gmt)
tz$gmt = gmt

tz_extra = as.data.frame(matrix(c(
    'West Central Africa',-1,
    'Indiana (East)',-5,
    'New Delhi',+5,
    'Brasilia',-3,
    'Greenland',-3,
    'Edinburgh',0,
    'Pretoria',+2,
    'Mountain Time (US & Canada)',-7,
    'Quito',-5,
    'Atlantic Time (Canada)',-4,
    'Central Time (US & Canada)',-6,
    'Pacific Time (US & Canada)',-8,
    'Eastern Time (US & Canada)',-5),ncol = 2, byrow=T))
names(tz_extra) = c('city','gmt')
tz_extra$gmt = as.numeric(tz_extra$gmt)

zones = rep(NA, nrow(gen))

sel = gen$user_timezone %in% tz$city 
zones[sel] = f.utils::replace_cn(gen$user_timezone[sel], tz$city, tz$gmt)

sel = gen$user_timezone %in% tz_extra$city
zones[sel] = f.utils::replace_cn(gen$user_timezone[sel], tz_extra$city , tz_extra$gmt)


# -------- baby names

baby = read_csv('_sessions/Models/competition2/1_Data/us-baby-names/NationalNames.csv')

baby = baby %>% 
  group_by(Name, Gender) %>%
  summarize(Count = sum(Count)) %>%
  ungroup()

baby_male = baby %>% filter(Gender == 'M') %>%
  arrange(desc(Count)) %>%
  top_n(1000)
baby_male_term = paste(baby_male$Name, collapse='|')

baby_female = baby %>% filter(Gender == 'F') %>%
  arrange(desc(Count)) %>%
  top_n(1000)
baby_female_term = paste(baby_female$Name, collapse='|')

male_detect_name = str_detect(gen$name, baby_male_term)
female_detect_name = str_detect(gen$name, baby_female_term)

male_detect_description = str_detect(gen$description, baby_male_term)
female_detect_description = str_detect(gen$description, baby_female_term)

male_detect_tweet = str_detect(gen$text, baby_male_term)
female_detect_tweet = str_detect(gen$text, baby_female_term)

# male_detect_name[is.na(male_detect_name)] = F
# female_detect_name[is.na(female_detect_name)] = F
# male_detect_description[is.na(male_detect_description)] = F
# female_detect_description[is.na(female_detect_description)] = F
# male_detect_tweet[is.na(male_detect_tweet)] = F
# female_detect_tweet[is.na(female_detect_tweet)] = F

# -------- sentiment

desc_texts = tibble(row = 1:nrow(gen), text = gen$description)
desc_sent = desc_texts %>% 
  unnest_tokens(word, text) %>%
  left_join(tidytext::get_sentiments('afinn')) %>%
  group_by(row) %>%
  summarize(sent = mean(score, na.rm=T),
            na = is.na(word[1]))
desc_sent$sent[desc_sent$na == F & is.na(desc_sent$sent)] = 0
desc_sents = rep(NA, nrow(gen))
desc_sents[desc_sent[[1]]] = desc_sent[[2]]

text_texts = tibble(row = 1:nrow(gen), text = gen$text)
text_sent = text_texts %>% 
  unnest_tokens(word, text) %>%
  left_join(tidytext::get_sentiments('afinn')) %>%
  group_by(row) %>%
  summarize(sent = mean(score, na.rm=T),
            na = is.na(word[1]))
text_sent$sent[text_sent$na == F &is.na(text_sent$sent)] = 0
text_sents = rep(NA, nrow(gen))
text_sents[text_sent[[1]]] = text_sent[[2]]

# Get n char -----

nchar_descr = sapply(gen$description, function(x) {
  n = try(nchar(iconv(x,sub='')),silent=T)
  if(is.character(n)) return(NA)
  n
  })

nchar_text = sapply(gen$text, function(x) {
  n = try(nchar(iconv(x,sub='')),silent=T)
  if(is.character(n)) return(NA)
  n
})

# Final data set --------

twitter = 
  tibble(
    gender = gen$gender,
    year_created = y_created,
    hour_created = h_created,
    tweet_count = gen$tweet_count,
    retweet_count = gen$retweet_count,
    user_timezone = zones,
    name_nchar = nchar(iconv(gen$name,sub='')),
    name_male = male_detect_name,
    name_female = female_detect_name,
    descr_nchar = nchar_descr,
    descr_male = male_detect_description,
    descr_female = female_detect_description,
    descr_sent = desc_sents,
    tweet_nchar = nchar_text,
    tweet_male = male_detect_tweet,
    tweet_female = female_detect_tweet,
    tweet_sent = text_sents,
    linkcol_red = link_col[,1],
    linkcol_green = link_col[,2],
    linkcol_blue = link_col[,3],
    sidecol_red = side_col[,1],
    sidecol_green = side_col[,2],
    sidecol_blue = side_col[,3])

twitter = twitter[!apply(twitter,1,function(x) any(is.na(x))),]
twitter = readr::type_convert(twitter)
twitter$gender = as.factor(twitter$gender)
twitter = twitter %>% mutate_if(is.logical, as.numeric)

# ---- create partition

# set.seed(100)
# 
# # trim to 4000
# twitter_sel = (twitter[sample(1:nrow(twitter)),])[sample(5000),]
# 
# # split
# train_index = createDataPartition(twitter_sel$gender, list = FALSE, p = .4998)
# twitter_train = twitter_sel %>% slice(train_index)
# twitter_test = twitter_sel %>% slice(-train_index)
# 
# # save
# write_csv(twitter_train,'_sessions/1_Data/tweets_train.csv')
# write_csv(twitter_test,'_sessions/Models/competition2/1_Data/tweets_test.csv')


cat(paste0('|',names(twitter_train),'||\n'),sep='')

# ---- run models

m_glm =  train(gender ~ . , data = twitter_train, method = 'glm',       preProcess = c('scale'))
m_knn =  train(gender ~ . , data = twitter_train, method = 'knn',       preProcess = c('scale'), tuneGrid = expand.grid(k = 1:10))
m_cart = train(gender ~ . , data = twitter_train, method = 'rpart',     preProcess = c('scale'), tuneGrid = data.frame(cp = seq(.01,.21,.02)))
m_enet = train(gender ~ . , data = twitter_train, method = 'glmnet',    preProcess = c('scale'), tuneGrid = expand.grid(alpha = c(0,.5,1), lambda = exp((-5):5)))
m_rf =   train(gender ~ . , data = twitter_train, method = 'rf',        preProcess = c('scale'), tuneGrid = data.frame(mtry = c(1, 2, 3)))
m_svm =  train(gender ~ . , data = twitter_train, method = 'svmLinear', preProcess = c('scale'), tuneGrid = expand.grid(C = 2^c(0:5)))

confusionMatrix(predict(m_glm), reference = twitter_train$gender)$overall['Accuracy']
confusionMatrix(predict(m_knn), reference = twitter_train$gender)$overall['Accuracy']
confusionMatrix(predict(m_cart), reference = twitter_train$gender)$overall['Accuracy']
confusionMatrix(predict(m_enet), reference = twitter_train$gender)$overall['Accuracy']
confusionMatrix(predict(m_rf), reference = twitter_train$gender)$overall['Accuracy']
confusionMatrix(predict(m_svm), reference = twitter_train$gender)$overall['Accuracy']

p_glm = predict(m_glm, newdata = twitter_test)
p_knn = predict(m_knn, newdata = twitter_test)
p_enet = predict(m_enet, newdata = twitter_test)
p_rf = predict(m_rf, newdata = twitter_test)
p_svm = predict(m_svm, newdata = twitter_test)

confusionMatrix(p_glm, reference = twitter_test$gender)$overall['Accuracy']
confusionMatrix(p_knn, reference = twitter_test$gender)$overall['Accuracy']
confusionMatrix(p_enet, reference = twitter_test$gender)$overall['Accuracy']
confusionMatrix(p_rf, reference = twitter_test$gender)$overall['Accuracy']
confusionMatrix(p_svm, reference = twitter_test$gender)$overall['Accuracy']




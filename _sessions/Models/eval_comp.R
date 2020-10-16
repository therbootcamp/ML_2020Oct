####### EVAL ML MODEL COMPETITION ######################################### 
require(caret)
require(stringr)

# DATA --------------

data_test = readr::read_csv('../_sessions/Models/competition2/1_Data/tweets_test.csv')

# MODELS --------------

repeat {

# get files
files = list.files('../_sessions/Models/submissions',full.names = T)

# exclude markus
files = files[!str_detect(files, "Markus Steiner")]

# remove non.rds
files = files[str_detect(files, ".RDS")]

# read mdoels
mods = lapply(files, readRDS)

# read mdoels
classes = sapply(mods, function(x) class(x)[1])
sel = classes == 'train'
mods = mods[sel]

# extract names
nams = sapply(str_split(files[sel],'/'),'[[',5)
nams = str_sub(nams, 1, nchar(nams)-4)

# mods[[1]]$finalModel$model$.outcome

# # read mdoels
# sel = sapply(mods, function(x) any(c("female","male") %in% levels(x$finalModel$model$.outcome)))
# mods = mods[sel]
# 
# # extract names
# nams = nams[sel]

# EVALUATE --------------

# evaluate mopdels
eval_mod = function(x){
  pred = predict(x, newdata = data_test)
  conf = confusionMatrix(pred, factor(data_test$gender))
  acc = conf$overall["Accuracy"]
  acc
  }

# get results
ACCs = sapply(mods, eval_mod)
names(ACCs) = nams

# sort results
ACCs = sort(ACCs, decreasing = T)

# PLOT --------------

cols = yarrr::piratepal("ipod")

par(mar=c(17,5,1,1))
plot.new();plot.window(xlim=c(.5,length(ACCs)+.5),ylim=c(0,1))
rect((1:length(ACCs))-.45,0,(1:length(ACCs))+.45,ACCs, col = cols, border=NA)
mtext(seq(0,1,.1),at=seq(0,1,.1),side=2,las=1)
mtext("ACC",side=2,las=1,line=2)
text(1:length(ACCs),rep(.2,length(ACCs)),labels=round(ACCs,3),col='white',cex=1)

#i = 1
#mtext(names(ACCs),side=1,las=2,at=(1:length(ACCs)))

i = 1
mtext(names(ACCs)[i],side=1,las=2,at=(1:length(ACCs))[i])


Sys.sleep(5)

}




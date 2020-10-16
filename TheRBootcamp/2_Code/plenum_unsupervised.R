
require(tidyverse)

gap <- read_csv('1_Data/gap.csv')
gap2017 <- gap %>% 
  filter(Jahr == 2007) %>% 
  select(`BIP pro Kopf`, Lebenserwartung)

plot(gap2017)

gap_km <- kmeans(scale(gap2017), centers = 3)

plot(gap2017, 
     col = gap_km$cluster)

gap_dbscan <- dbscan::dbscan(scale(gap2017), 
                             eps = .25)
plot(gap2017, 
     col = gap_dbscan$cluster + 1)

gap_gm <- mclust::Mclust(gap2017)

plot(gap_gm, what = 'classification')

gap_gm$parameters

plot(gap_gm, what = 'BIC', ylim=c(-3800, -3780))

credit <- read_csv('1_Data/credit.csv')

credit_gm <- mclust::Mclust(credit)

plot(credit_gm, )



source("https://raw.githubusercontent.com/therbootcamp/ML_2019Oct/master/_materials/code/baselrbootcamp_palettes.R")

require(gapminder)
require(gganimate)

gap = gapminder::gapminder %>% filter(country != 'Kuwait') %>%
  rename('BIP pro Kopf' = gdpPercap, Lebenserwartung = lifeExp)

write_csv(gap %>% rename(Land = country, Kontinent = continent, Jahr = year, Population = pop), '_sessions/Unsupervised/1_Data/gap.csv')

cols = c(baselrbootcamp_colors,'#235A97')
names(cols) = unique(gap$continent)
p = ggplot(gap, aes(x = `BIP pro Kopf`, 
                y = `Lebenserwartung`,
                size = pop,
                col = continent, 
                label = year)) +
  geom_point() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  ylim(20, 80) + xlim(0, 50000) +
  geom_text(aes(x = 25000, y = 30), 
            col = 'black', size=8) + 
  scale_color_manual(values = cols)


require(gganimate)
anim = p + transition_manual(year) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/data.gif', anim,
          nframes = length(unique(gap$year)), 
          res = 300, width = 4, height = 4, unit = 'in',fps = 2)


p = ggplot(gap %>% filter(year == 1952), aes(x = `BIP pro Kopf`, 
                    y = `Lebenserwartung`)) +
  geom_text(aes(x = 8000, y = 35, label = 1952), col = 'grey75', size=8) +
  geom_point() + 
  theme_minimal() + 
  theme(legend.position = "none")
ggsave('_sessions/Unsupervised/image/gap1952.png',plot = p,device="png",dpi = 300, width = 4, height = 4, unit = 'in')


###  k-means --------

d = as.matrix(gap1952[,c(4,6)]) ; for(j in 1:2) d[,j] = c(scale(d[,j]))

t = 0
iter = list()
set.seed(100)
for(rep in 1:10){

  k = 3
  d = as.matrix(gap1952[,c(6,4)])
  r_x = range(d[,1]) ; r_y = range(d[,2])
  start_x = runif(k, r_x[1], r_x[2]); start_y = runif(k, r_y[1], r_y[2])
  starts = cbind(start_x, start_y)
  sds = apply(d, 2, sd)
  
  
  clu = rep(0,nrow(d))
  t = t + 1;iter[[t]] = cbind(rbind(starts, d), clu = c(1:3, clu), start = rep(c(T, F),c(3,nrow(d))), t = t)
  t = t + 1;iter[[t]] = cbind(rbind(starts, d), clu = c(1:3, clu), start = rep(c(T, F),c(3,nrow(d))), t = t)
  t = t + 1;iter[[t]] = cbind(rbind(starts, d), clu = c(1:3, clu), start = rep(c(T, F),c(3,nrow(d))), t = t)
  t = t + 1;iter[[t]] = cbind(rbind(starts, d), clu = c(1:3, clu), start = rep(c(T, F),c(3,nrow(d))), t = t)
  
  for(i in 1:12){
    
    # get dists
    dists = matrix(nrow = nrow(d), ncol = k)
    for(j in 1:nrow(starts)){
      dists[, j] = sqrt((d[,1]/sds[1] - starts[j,1]/sds[1])**2  + (d[,2]/sds[2] - starts[j,2]/sds[2])**2)
      }
    
    # get assignment
    clu = apply(dists, 1, which.min)
    
    # store
    t = t + 1;iter[[t]] = cbind(rbind(starts, d), clu = c(1:3, clu), start = rep(c(T, F),c(3,nrow(d))), t = t)
    if(i == 1){
    t = t + 1;iter[[t]] = cbind(rbind(starts, d), clu = c(1:3, clu), start = rep(c(T, F),c(3,nrow(d))), t = t)
    #t = t + 1;iter[[t]] = cbind(rbind(starts, d), clu = c(1:3, clu), start = rep(c(T, F),c(3,nrow(d))), t = t)
    }
    
    # new starts
    for(j in 1:nrow(starts)){
      starts[j, ] = colMeans(d[clu == j,,drop=F]) 
      }
    
    }
  
  }



iters = as_tibble(do.call(rbind, iter))
names(iters)[1:2] = c('BIP pro Kopf','Lebenserwartung')

cols = baselrbootcamp_colors[c(2,1,3,4)]
names(cols) = 0:3

p = ggplot(iters, 
       aes(x = `BIP pro Kopf`, 
           y = `Lebenserwartung`, shape=factor(start), 
           size = start, col = factor(clu))) + 
  geom_point() + theme_minimal() + theme(legend.position = "none") + 
  scale_color_manual(values = cols) + 
  scale_size_continuous(range=c(2,5)) + scale_shape_manual(values = c(15,19))


require(gganimate)
anim = p + transition_states(t, transition_length = 2, state_length = 2) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/kmeans.gif', nframes = max(iters$t), anim, res = 300, width = 4, height = 4, unit = 'in')



# k-selection

gap1952 = gapminder::gapminder %>% filter(country != 'Kuwait') %>%
  rename('BIP pro Kopf' = gdpPercap, Lebenserwartung = lifeExp) %>% 
  filter(year == 1952) %>% select('BIP pro Kopf', Lebenserwartung)

gap1952_sc = scale(gap1952)


iter = list()
for(i in 1:10){
  
  km = kmeans(gap1952_sc, i)
  w = km$tot.withinss * sd(gap1952$`BIP pro Kopf`) * sd(gap1952$Lebenserwartung)
  centers = km$centers
  for(j in 1:2) centers[,j] = centers[,j] * attr(gap1952_sc,'scaled:scale')[j] + attr(gap1952_sc,'scaled:center')[j]
  pts = rbind(centers, as.matrix(gap1952))
  iter[[i]] = cbind(pts, clu = c(1:i, km$cluster), start = rep(c(T,F),c(i,nrow(gap1952_sc))), w, t = i)
  
  }

iters = as_tibble(do.call(rbind, iter))

cols = c(baselrbootcamp_colors[c(2,4,1,3)],'#235A97')
cols = colorRampPalette(cols)(10) ; names(cols) = 1:10

p1 = ggplot(iters, 
            aes(x = `BIP pro Kopf`, 
                y = `Lebenserwartung`, shape=factor(start), 
                size = start, col = factor(clu))) + 
  geom_point() + theme_minimal() + theme(legend.position = "none") + 
  scale_color_manual(values = cols) + 
  scale_size_continuous(range=c(2,5)) + scale_shape_manual(values = c(15,19))

ws = iters %>% group_by(t) %>% slice(1) %>% ungroup()
ws = lapply(1:10, function(x) {tmp = ws[1:x,]; tmp$t = x;tmp$k=1:x;tmp})
ws = do.call(rbind, ws) %>% rename("Binnenvarianz" = w)
p2 = ggplot(ws, 
            aes(x = k, 
                y = Binnenvarianz)) + 
  geom_point(size=3) + geom_line(size=1.3) + theme_minimal() + 
  scale_x_continuous(limits = c(1,10)) + ylim(200000,12000000)


anim = p1 + transition_manual(t) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/k-selection1.gif', anim,
          nframes = length(unique(gap$year)), 
          res = 300, width = 4, height = 2.2, unit = 'in',fps = 2)


anim = p2 + transition_manual(t) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/k-selection2.gif', anim,
          nframes = length(unique(gap$year)), 
          res = 300, width = 4, height = 1.8, unit = 'in',fps = 2)


# DBSCAN

ds = scale(d)
ds = ds[order(ds[,2]),]

iter = list()
col = rep(0, nrow(ds))
eps = .2
minPts = 5
t = 0

for(i in 1:nrow(ds)){
  
  pt = ds[i,]
  dists = sqrt((ds[,1] - pt[1])**2  + (ds[,2] - pt[2])**2)
  nneigh = sum(dists < eps)
  t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == i), col = col, t = t)
  col[i] = ifelse(nneigh == 1, 1, ifelse(nneigh>=minPts + 1, 3, 2))
  t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == i), col = col, t = t)
  
}

clusts = which(col == 3)
comps = igraph::components(igraph::graph_from_adjacency_matrix(as.matrix(dist(ds[clusts,])) < eps))
clusts = split(clusts, comps$membership)

col = iter[[t]][,'col']
cols_nums = c(3,5,4)
for(i in 1:length(clusts)){
  
  clust = clusts[[i]]
  for(j in clust){
    t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == j), col = col, t = t)
    col[j] = cols_nums[1 + (i %% 3)]
    t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == j), col = col, t = t)
  }
}

clusts = unlist(clusts)
col = iter[[t]][,'col']
for(i in which(col == 2)){
  
  pt = ds[i,]
  dists = sqrt((ds[,1] - pt[1])**2  + (ds[,2] - pt[2])**2)
  neighs = which(dists < eps)
  neighs = neighs[neighs %in% clusts]
  t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == i), col = col, t = t)
  if(!any(col[neighs] >= 3)) col[i] = 1  else col[i] = col[neighs][col[neighs] >= 3 ][1] 
  t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == i), col = col, t = t)
  }


for(i in 1:40) {t = t + 1 ; iter[[t]] = cbind(ds, radius = F, col = col, t = t)}

iters = as_tibble(do.call(rbind, iter))
names(iters)[1:2] = c('BIP pro Kopf','Lebenserwartung')

cols = c('black', baselrbootcamp_colors[c(2,4,1,3)],'#235A97')
names(cols) = 0:5

p = ggplot(iters, 
           aes(x = `BIP pro Kopf`, 
               y = `Lebenserwartung`,
               col = factor(col))) + 
  geom_point(size = 2) + 
  geom_point(shape=21, stroke = 1.2,
             aes(size = radius), 
             data = iters %>% filter(radius != 0)) + 
  scale_size_continuous(range = c(2, 15)) +
  theme_minimal() +
  scale_color_manual(values = cols) + 
  theme(legend.position = "none") 

require(gganimate)
anim = p + transition_manual(frames = t) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/dbscan_1.gif', animation = anim, 
          nframes = max(iters$t), res = 300, width = 4, 
          height = 4, unit = 'in', fps = 100, duration = max(iters$t) / 100,
          end_pause = 5)



# DBSCAN

ds = scale(d)
ds = ds[order(ds[,2]),]

iter = list()
col = rep(0, nrow(ds))
eps = .3
minPts = 5
t = 0

for(i in 1:nrow(ds)){
  
  pt = ds[i,]
  dists = sqrt((ds[,1] - pt[1])**2  + (ds[,2] - pt[2])**2)
  nneigh = sum(dists < eps)
  t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == i), col = col, t = t)
  col[i] = ifelse(nneigh == 1, 1, ifelse(nneigh>=minPts + 1, 3, 2))
  t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == i), col = col, t = t)
  
}

clusts = which(col == 3)
comps = igraph::components(igraph::graph_from_adjacency_matrix(as.matrix(dist(ds[clusts,])) < eps))
clusts = split(clusts, comps$membership)

col = iter[[t]][,'col']
cols_nums = c(3,5,4)
for(i in 1:length(clusts)){
  
  clust = clusts[[i]]
  for(j in clust){
    t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == j), col = col, t = t)
    col[j] = cols_nums[1 + (i %% 3)]
    t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == j), col = col, t = t)
  }
}

clusts = unlist(clusts)
col = iter[[t]][,'col']
for(i in which(col == 2)){
  
  pt = ds[i,]
  dists = sqrt((ds[,1] - pt[1])**2  + (ds[,2] - pt[2])**2)
  neighs = which(dists < eps)
  neighs = neighs[neighs %in% clusts]
  t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == i), col = col, t = t)
  if(!any(col[neighs] >= 3)) col[i] = 1  else col[i] = col[neighs][col[neighs] >= 3 ][1] 
  t = t + 1 ; iter[[t]] = cbind(ds, radius = ((1:nrow(ds)) == i), col = col, t = t)
  }


for(i in 1:40) {t = t + 1 ; iter[[t]] = cbind(ds, radius = F, col = col, t = t)}

iters = as_tibble(do.call(rbind, iter))
names(iters)[1:2] = c('BIP pro Kopf','Lebenserwartung')

cols = c('black', baselrbootcamp_colors[c(2,4,1,3)],'#235A97')
names(cols) = 0:5

p = ggplot(iters, 
           aes(x = `BIP pro Kopf`, 
               y = `Lebenserwartung`,
               col = factor(col))) + 
  geom_point(size = 2) + 
  geom_point(shape=21, stroke = 1.2,
             aes(size = radius), 
             data = iters %>% filter(radius != 0)) + 
  scale_size_continuous(range = c(2, 23)) +
  theme_minimal() +
  scale_color_manual(values = cols) + 
  theme(legend.position = "none") 

require(gganimate)
anim = p + transition_manual(frames = t) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/dbscan_2.gif', animation = anim, 
          nframes = max(iters$t), res = 300, width = 4, 
          height = 4, unit = 'in', fps = 100, duration = max(iters$t) / 100,
          end_pause = 5)


# Vorteile DBSCAN

x1 = runif(110,-4.5,4.5)
y1 = runif(110,3,4)
x2 = runif(100,-4.5,-3.5)
y2 = runif(100,-4,3)
x3 = runif(100,-.5,.5)
y3 = runif(100,-4,3)
x4 = runif(100,3.5,4.5)
y4 = runif(100,-4,3)
x5 = runif(100,-2.5,-1.5)
y5 = runif(100,-2,2)
x6 = runif(100,1.5,2.5)
y6 = runif(100,-2,2)
x7 = runif(110,-4.5,4.5)
y7 = runif(110,-4,-3)

d = as_tibble(
  cbind(x = c(x1, x2, x3, x4, x5, x6, x7), 
        y = c(y1, y2, y3, y4, y5, y6, y7)))

km_seq = 2:6
dbscan_seq = seq(.6,1.4,.2)

iter = list()
for(i in 1:5){
  
  km = kmeans(d, centers = km_seq[i])
  db = dbscan(d, eps = dbscan_seq[i], minPts = 10)
  
  iter[[i]] = cbind(d, km = km$cluster, dbscan = db$cluster, 
                    km_lab = paste0("k = ",km_seq[i]), 
                    db_lab = paste0("eps = ",dbscan_seq[i]), t = i)
  }

iters = as_tibble(do.call(rbind, iter))

set.seed(2)
cols = c(baselrbootcamp_colors[c(2,4,1,3)],'#235A97')
sel = seq(1, 19, length = 5)
cols = colorRampPalette(cols)(19)
names(cols)[sel] = 0:4
names(cols)[-sel] = 5:18

p1 = ggplot(iters, aes(x, y, col = factor(km))) + geom_point() + 
  theme_minimal() + theme(legend.position = 'none') + 
  scale_color_manual(values=cols) + 
  geom_text(data = iters, 
             mapping = aes(x = 4.3, y = 4.7, label = km_lab), 
            col = 'black', size=3) + labs(title="k-means")

anim = p1 + transition_manual(t) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/dbscan_adv1.gif', anim,
          nframes = length(unique(gap$year)), 
          res = 300, width = 4, height = 2.3, unit = 'in',fps = 1)

p2 = ggplot(iters, aes(x, y, col = factor(dbscan))) + geom_point() + 
  theme_minimal() + theme(legend.position = 'none') + 
  scale_color_manual(values=cols) + 
  geom_text(data = iters, 
            mapping = aes(x = 4, y = 4.7, label = db_lab), 
            col = 'black', size=3) + labs(title="DBSCAN")

anim = p2 + transition_manual(t) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/dbscan_adv2.gif', anim,
          nframes = length(unique(gap$year)), 
          res = 300, width = 4, height = 2.3, unit = 'in',fps = 1)



# Vorteile DBSCAN

x1 = runif(110,-4.5,4.5)
y1 = runif(110,3,4)
x2 = runif(100,-4.5,-3.5)
y2 = runif(100,-4,3)
x3 = runif(100,-.5,.5)
y3 = runif(100,-4,3)
x4 = runif(100,3.5,4.5)
y4 = runif(100,-4,3)
x5 = runif(100,-2.5,-1.5)
y5 = runif(100,-2,2)
x6 = runif(100,1.5,2.5)
y6 = runif(100,-2,2)
x7 = runif(110,-4.5,4.5)
y7 = runif(110,-4,-3)
x8 = runif(40, -7, 7)
y8 = runif(40, -7, 7)

d = as_tibble(
  cbind(x = c(x1, x2, x4, x7, x8), 
        y = c(y1, y2, y4, y7, y8)))

km_seq = 2:6
dbscan_seq = seq(.6,1.4,.2)

iter = list()
for(i in 1:5){
  
  km = kmeans(d, centers = km_seq[i])
  db = dbscan(d, eps = dbscan_seq[i], minPts = 10)
  
  iter[[i]] = cbind(d, km = km$cluster, dbscan = db$cluster, 
                    km_lab = paste0("k = ",km_seq[i]), 
                    db_lab = paste0("eps = ",dbscan_seq[i]), t = i)
}

iters = as_tibble(do.call(rbind, iter))

set.seed(2)
cols = c(baselrbootcamp_colors[c(2,4,1,3)],'#235A97')
sel = seq(1, 19, length = 5)
cols = colorRampPalette(cols)(19)
names(cols)[sel] = 0:4
names(cols)[-sel] = 5:18

p1 = ggplot(iters, aes(x, y, col = factor(km))) + geom_point() + 
  theme_minimal() + theme(legend.position = 'none') + 
  scale_color_manual(values=cols) + 
  geom_text(data = iters, 
            mapping = aes(x = 4.3, y = 4.7, label = km_lab), 
            col = 'black', size=3) + labs(title="k-means")

anim = p1 + transition_manual(t) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/dbscan_adv3.gif', anim,
          nframes = length(unique(gap$year)), 
          res = 300, width = 4, height = 2.3, unit = 'in',fps = 1)

p2 = ggplot(iters, aes(x, y, col = factor(dbscan))) + geom_point() + 
  theme_minimal() + theme(legend.position = 'none') + 
  scale_color_manual(values=cols) + 
  geom_text(data = iters, 
            mapping = aes(x = 4, y = 4.7, label = db_lab), 
            col = 'black', size=3) + labs(title="DBSCAN")

anim = p2 + transition_manual(t) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/dbscan_adv4.gif', anim,
          nframes = length(unique(gap$year)), 
          res = 300, width = 4, height = 2.3, unit = 'in',fps = 1)


# Vorteile DBSCAN

x1 = runif(200,-4.5,4.5)
y1 = runif(200,3,4)
x2 = runif(50,-4.5,-3.5)
y2 = runif(50,-4,3)
x3 = runif(100,-.5,.5)
y3 = runif(100,-4,3)
x4 = runif(400,3.5,4.5)
y4 = runif(400,-4,3)
x5 = runif(30,-2.5,-1.5)
y5 = runif(30,-2,2)
x6 = runif(150,1.5,2.5)
y6 = runif(150,-2,2)
x7 = runif(70,-4.5,4.5)
y7 = runif(70,-4,-3)

d = as_tibble(
  cbind(x = c(x1, x2, x3, x4, x5, x6, x7), 
        y = c(y1, y2, y3, y4, y5, y6, y7)))

km_seq = 2:6
dbscan_seq = seq(.2,1,.2)

iter = list()
for(i in 1:5){
  
  km = kmeans(d, centers = km_seq[i])
  db = dbscan(d, eps = dbscan_seq[i], minPts = 10)
  
  iter[[i]] = cbind(d, km = km$cluster, dbscan = db$cluster, 
                    km_lab = paste0("k = ",km_seq[i]), 
                    db_lab = paste0("eps = ",dbscan_seq[i]), t = i)
}

iters = as_tibble(do.call(rbind, iter))

set.seed(2)
cols = c(baselrbootcamp_colors[c(2,4,1,3)],'#235A97')
sel = seq(1, 19, length = 5)
cols = colorRampPalette(cols)(19)
names(cols)[sel] = 0:4
names(cols)[-sel] = 5:18

p1 = ggplot(iters, aes(x, y, col = factor(km))) + geom_point() + 
  theme_minimal() + theme(legend.position = 'none') + 
  scale_color_manual(values=cols) + 
  geom_text(data = iters, 
            mapping = aes(x = 4.3, y = 4.7, label = km_lab), 
            col = 'black', size=3) + labs(title="k-means")

anim = p1 + transition_manual(t) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/dbscan_adv5.gif', anim,
          nframes = length(unique(gap$year)), 
          res = 300, width = 4, height = 2.3, unit = 'in',fps = 1)

p2 = ggplot(iters, aes(x, y, col = factor(dbscan))) + geom_point() + 
  theme_minimal() + theme(legend.position = 'none') + 
  scale_color_manual(values=cols) + 
  geom_text(data = iters, 
            mapping = aes(x = 4, y = 4.7, label = db_lab), 
            col = 'black', size=3) + labs(title="DBSCAN")

anim = p2 + transition_manual(t) #& patchwork::plot_annotation()
anim_save('_sessions/Unsupervised/image/dbscan_adv6.gif', anim,
          nframes = length(unique(gap$year)), 
          res = 300, width = 4, height = 2.3, unit = 'in',fps = 1)


# gaussian mixtures ---- 

gap1952 = gapminder::gapminder %>% filter(country != 'Kuwait') %>%
  rename('BIP pro Kopf' = gdpPercap, Lebenserwartung = lifeExp) %>% 
  filter(year == 1952) %>% select('BIP pro Kopf', Lebenserwartung)

gap1952_sc = scale(gap1952)

require(mclust)

gap1952_ = gap1952
names(gap1952_) = c('x1', 'x2')
a = mclust::Mclust(gap1952_)

plot(gap1952_)
mclust2Dplot(gap1952_, parameters = a$parameters, z = a$z, what = 'classification')

png('_sessions/Unsupervised/image/mclust1.png',res=300,width=4,height=4,unit='in')
a = mclust::Mclust(gap1952)
par(mar=c(3,3,1,1))
plot(a, what = 'classification', 
     col = baselrbootcamp_colors[1:3], 
     pch=16,bty='n',xaxt='n',yaxt='n')
grid()
mtext(seq(0,15000,5000),side=1,at=seq(0,15000,5000))
mtext(seq(30,70,10),side=2,las=1,at=seq(30,70,10))
mtext(names(gap1952),side=c(1,2),line=c(1.5,1.5),cex=1.2)
dev.off()

# Vorteile DBSCAN

x1 = runif(110,-4.5,4.5)
y1 = runif(110,3,4)
x2 = runif(100,-4.5,-3.5)
y2 = runif(100,-4,3)
x3 = runif(100,-.5,.5)
y3 = runif(100,-4,3)
x4 = runif(100,3.5,4.5)
y4 = runif(100,-4,3)
x5 = runif(100,-2.5,-1.5)
y5 = runif(100,-2,2)
x6 = runif(100,1.5,2.5)
y6 = runif(100,-2,2)
x7 = runif(110,-4.5,4.5)
y7 = runif(110,-4,-3)

d = as_tibble(
  cbind(x = c(x1, x2, x3, x4, x5, x6, x7), 
        y = c(y1, y2, y3, y4, y5, y6, y7)))

png('_sessions/Unsupervised/image/mclust2.png',res=300,width=4,height=4,unit='in')
a = mclust::Mclust(d)
par(mar=c(3,3,1,1))
plot(a, what = 'classification', 
     col = sample(colorRampPalette(baselrbootcamp_colors)(20)), 
     pch=16,bty='n',xaxt='n',yaxt='n')
grid()
mtext(seq(-4,4,1),side=1,at=seq(-4,4,1))
mtext(seq(-4,4,1),side=2,las=1,at=seq(-4,4,1))
mtext(names(d),side=c(1,2),line=c(1.5,1.5),cex=1.2)
dev.off()


x1 = runif(200,-4.5,4.5)
y1 = runif(200,3,4)
x2 = runif(50,-4.5,-3.5)
y2 = runif(50,-4,3)
x3 = runif(100,-.5,.5)
y3 = runif(100,-4,3)
x4 = runif(400,3.5,4.5)
y4 = runif(400,-4,3)
x5 = runif(30,-2.5,-1.5)
y5 = runif(30,-2,2)
x6 = runif(150,1.5,2.5)
y6 = runif(150,-2,2)
x7 = runif(70,-4.5,4.5)
y7 = runif(70,-4,-3)

d = as_tibble(
  cbind(x = c(x1, x2, x3, x4, x5, x6, x7), 
        y = c(y1, y2, y3, y4, y5, y6, y7)))

png('_sessions/Unsupervised/image/mclust3.png',res=300,width=4,height=4,unit='in')
a = mclust::Mclust(d)
par(mar=c(3,3,1,1))
plot(a, what = 'classification', 
     col = sample(colorRampPalette(baselrbootcamp_colors)(20)), 
     pch=16,bty='n',xaxt='n',yaxt='n')
grid()
mtext(seq(-4,4,1),side=1,at=seq(-4,4,1))
mtext(seq(-4,4,1),side=2,las=1,at=seq(-4,4,1))
mtext(names(d),side=c(1,2),line=c(1.5,1.5),cex=1.2)
dev.off()



vars = c('ONEOFF_PURCHASES_FREQUENCY',
         'BALANCE','BALANCE_FREQUENCY','CREDITLIMIT','TENURE',
         'PURCHASES_FREQUENCY','PURCHASES','INSTALLMENTS_PURCHASES')

a = mclust::Mclust(d)
plot(a, what = 'classification', 
     col = sample(colorRampPalette(baselrbootcamp_colors)(20)), 
     pch=16,bty='n',xaxt='n',yaxt='n')

sel = caret::findCorrelation(cor(cr),cutoff = 0.5,verbose=T)
names(cr)[sel]

hist(cr$ONEOFF_PURCHASES)

cr = read_csv('_sessions/Unsupervised/1_Data/credit_raw.csv')
cr = cr %>% select(BALANCE,BALANCE_FREQUENCY,PURCHASES,CREDIT_LIMIT,ONEOFF_PURCHASES,
                   MINIMUM_PAYMENTS, PRC_FULL_PAYMENT, TENURE) %>% na.omit() 
write_csv(cr, '_sessions/Unsupervised/1_Data/credit.csv')

dim(cr)

cr$cl = a$classification

res = cr %>% mutate_all(scale) %>% group_by(cl) %>% select(-1) %>% summarize_all(mean)


a = Mclust(cr %>% select(-1) %>% na.omit() %>% scale())
kmeans()
dbscan(cr %>% select(-1) %>% na.omit() %>% scale(), eps = .2)





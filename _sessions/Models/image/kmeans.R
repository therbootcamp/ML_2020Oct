
data(iris)

cols = c('#606060','#EA4B68','#6ABA9A','#EACC48','#235A97')

png('_sessions/Models/image/iris_kmeans.png',width=5,height=5,units='in',res=300)

par(mfrow=c(2,2),mar=c(2.5,2.5,2.5,1))

ks = 2:5
for(k in ks){
res = stats::kmeans(iris[,-5], centers = k, nstart = 10)

plot.new();plot.window(xlim=c(1,7),ylim=c(2,4.5))
points(iris[,c(3,2)], pch = 16, col = f.utils::replace_nc(res$cluster,1:5,cols),cex=1.2)
mtext(c('Petal length','Sepal width',paste0('k = ',k)),side=c(1,2,3),line=1,font=c(1,1,2),cex=c(1,1,1.3))
}

dev.off()

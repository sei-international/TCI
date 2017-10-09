# cluster analysis for TCI scores
# Steve Fick
# 8-29-2017

library(vegan)
library(raster)
set.seed(1985)
d <- read.csv(file.path(outdir, 'data.csv'), stringsAsFactors = FALSE, check.names=F)
d$HDI <- as.numeric(d$HDI)
row.names(d) <- d$`Country name`
s <- grep('score', names(d))

sh <- shapefile('data/global_flows_map/COUNTRY_CENTROIDS.shp')
d$area <- sh$SQKM[match(d$`Country code`, sh$ISO)]
d$logArea <- log(d$area+1)

#find top 30%
t70 <- d$TCI > quantile(d$TCI, .7, na.rm = TRUE)
nt70 <- !t70


# omit NAS to allow for distance matrix calculation
x <- na.omit(d[t70,s])
i <- attributes(x)$na.action

#################################################
# Perform wards clustering on euclidean distance
#################################################

nclust <- 7
#di <- vegdist(d[t70,s], method = 'bray', na.rm=TRUE)
di <- dist(x, method = 'euclidean')
tree <- hclust(di, metho = 'ward')
clh <- cutree(tree, k = nclust)

s2 <- c( s, grep('GaIN', names(d)), grep('HDI', names(d)),grep('logArea', names(d)))
a <- aggregate(d[t70,s2][-i,], list(clh), mean, na.rm=TRUE)
aver <- apply(d[,s2],2, function(x) mean(as.numeric(x), na.rm = TRUE))

colz = c(
"#e41a1c",
"#377eb8",
"#4daf4a",
"#984ea3",
"#ff7f00",
"#a65628",
"#f781bf",
"#999999")

# make plot

svg(file.path(outdir, 'figures', 'cluster_dendrogram.svg'), width = 10, height = 10)

layout(matrix(   c(2,3,4,5,6,1,1,7,8,1,1,9, 10,11,12,13),4,4))
plot(tree, labels = row.names(x), xlab= '', main = 'Upper 30% TCI', ylab = '', yaxt = 'n', xaxt = 'n', sub = '')

#j <- rect.hclust(tree, k=nclust, border= rainbow(nclust))
j <- rect.hclust(tree, k=nclust, border= colz[1:nclust])



grps <- do.call(c,lapply(1:length(j), function(x) rep(x, length(j[[x]]))))
ids <- do.call(c, j)

# labels
xs <-aggregate(1:length(grps), by = list(grps), FUN= mean)$x
text(xs, par('usr')[3],   letters[1:nclust], pos =3, offset = .3)


cols <- grps[match(names(clh), names(ids))]
cols <- aggregate(clh, by = list(cols), unique)
#cols <- rainbow(nclust)[order(cols$x)]

for(i in names(d)[s2]){
# for(i in names(d)[s]){
  name <- gsub('_score', '', i)
  name <- gsub('_', ' ', name)
  #barplot(c(a[cols$x,i]), names = letters[1:nclust], main = i, col = rainbow(nclust))
  yl = c(0, max(c( aver[i], max(a[cols$x,i]))))
  if(grepl('log',i)) {
    k = c(aver[i], a[cols$x,i])
    barplot(c(a[cols$x,i]), names = letters[1:nclust], main = name, col = colz[1:nclust], ylim = c(min(k)-.5, max(k)), xpd = FALSE)

  } else {
    barplot(c(a[cols$x,i]), names = letters[1:nclust], main = name, col = colz[1:nclust], ylim = yl)
  }
  abline(h = aver[i], lty = 2)
}


dev.off()



########################################
# NMDS
########################################
layout(matrix(1,1,1))
row.names(d) <- d$`Country name`
y <- na.omit(d[,s])
jj <- attributes(y)$na.action
yt70 <- which(row.names(y) %in% d$`Country name`[t70])
nyt70 <- which(!row.names(y) %in% d$`Country name`[t70])

m <- metaMDS(y, try = 1000)

#m$points['Austria',]<- m$points['Austria']+.05
#m$points['Netherlands',2]<- m$points['Netherlands',2]-.01


svg(file.path(outdir, 'figures','nmds.svg'), height = 10, width = 12)

nnames <- gsub('_score', '', names(d)[s])
nnames <- gsub('_', ' ', nnames)

ordiplot(m,type="n", xlim = c(-.42,.35), ylim = c(-.41,.35))
# ordiplot(m,type="n")
orditorp(m,display="species", labels = nnames ,col="deepskyblue2",air=0.01, cex = 1.1)
orditorp(m,display="sites", select= yt70, col = colz[grps[match(as.character(row.names(y)[yt70]), names(unlist(j)))]], air = .01, cex = (d$TCI[-jj]/max(d$TCI[-jj]))[yt70])
orditorp(m,display="sites", select= nyt70, col = rgb(0,0,0,.5), air = .01
, cex = (d$TCI[-jj]/max(d$TCI[-jj]))[nyt70])

text(-.4,-.4, paste0( 'stress = ', round(m$stress,3)))
dev.off()

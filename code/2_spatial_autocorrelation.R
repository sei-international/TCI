
## spatial autocorrelation

# Steve Fick
# Aug 29 2017

#This script develops an global adjacency matrix for countries of interest and evaluates levels of spatial autocorrelation using Moran's I for a given variable.

# make adjacency / distance matrix for world countries

library(raster)
library(deldir)
library(maps)

# setup
d <- read.csv(file.path(outdir, 'data.csv'), stringsAsFactors = FALSE, check.names=F)


sh <- shapefile('data/global_flows_map/COUNTRY_CENTROIDS.shp')
cc <- d$`Country code`
goog <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

# filter to only countries in dataset

s <- sh[which(sh$ISO %in% cc), ]
d <- d[-which(!d$`Country code` %in% s$ISO),]

#rearrange
s <- s[match(d$`Country code`, s$ISO),]


xg1 <- spTransform(s, CRS(goog))

xg2 <- xg1@data

# to create links across pacific
spin <- function(cr){
  x <- cr[,1]
  y <- cr[,2]
  x[which(x > 0)] <- x[which(x>0)] -20026376.39
  x[which(x <= 0)] <- x[which(x <= 0)] + 20026376.39
  #y[which(y > 0)] <- y[which(y>0)] -20048966.10
  #y[which(y <= 0)] <- y[which(y <= 0)] + 20048966.10
  
  return( cbind(x,y))
}

coordinates(xg2) <- spin(coordinates(xg1))

# find delauney connections from one perspective

dela <- deldir(coordinates(xg1)[,1], coordinates(xg1)[,2])
del1 <- dela$delsgs

# rotate earth and find connections from other perspective

# not including thise because not necessary to keep links between pacific(?)
delb <- deldir(coordinates(xg2)[,1], coordinates(xg2)[,2])
del2 <- delb$delsgs

# create (sparse) adjacency matrix based on delaunay
dis1 <- matrix(0, nrow(s), nrow(s))

dis1[ cbind(del1$ind1, del1$ind2)] <- 1
dis1[ cbind(del1$ind2, del1$ind1)] <- 1

#dis1[ cbind(del2$ind1, del2$ind2)] <- 1
#dis1[ cbind(del2$ind2, del2$ind1)] <- 1

row.names(dis1) <- s$ISO
colnames(dis1) <- s$ISO

# double check
map('world')
title('Spatial Adjacency Matrix')
points(s, cex = .4, col = 'red')

invisible(lapply(1:nrow(dis1), function(v){
  
  adj <- which(dis1[v,] > 0)
  for (a in adj){
    lines(coordinates(s)[c(v,a),1 ] ,coordinates(s)[c(v,a),2], col = rgb(0,0,0,.2))
  }
}))

# some ad-hoc corrections to the adjacency matrix
dadj <- dis1

# remove small islands
tooSmall <- c(

  'Bouvet Island' = "BVT",
  'Kiribati' = 'KIR'
)

ts <- sapply (tooSmall, grep, colnames(dadj))
dadj <- dadj[-ts,-ts]  

# remove these connections

  # all of bermudas connections to europe and africa
  bmu <- c('ISL', 'IRL', 'PRT', 'CPV')
  dadj['BMU', bmu] <- 0
  dadj[bmu, 'BMU'] <- 0

  #all of cape Verde's connections to America
  cpv <- c('PRT', 'ATG', 'BRB')
  dadj['CPV', cpv] <- 0
  dadj[cpv, 'CPV'] <- 0
  
  #uruguay - Namib
  dadj['URY','NAM' ] <- 0
  dadj['NAM', 'URY' ] <- 0
  
  #Kazakstan, east baltic staes
  eb <- c('FIN', 'EST', 'LVA', 'BLR', 'UKR', 'GEO')
  dadj['KAZ' , eb] <- dadj[eb, 'KAZ'] <- 0
  
  # MUS and SEA
  dadj['MUS' , c("AUS", "SGP" )] <- dadj[c("AUS", "SGP" ), 'MUS'] <- 0
  
  # Namibia and liberia, stp
  dadj['NAM' , c("STP", "LBR" )] <- dadj[c("LBR", "STP" ), 'NAM'] <- 0
  
  
# Add these connections
  
  # Tonga - micronesia
  dadj['TON' , c('NZL', 'VUT','MHL')] <- dadj[ c('NZL', 'VUT','MHL'), 'TON'] <- 1

  #Russia - east baltic and china
  eb <- c('CHN','FIN', 'EST', 'LVA', 'BLR', 'UKR', 'GEO')
  dadj['RUS' , eb] <- dadj[eb, 'RUS'] <- 1

  #china <-> vietnam, india, pakistan, tajikistan, kazakstan
  cn <- c('VNM', "TJK", 'PAK', 'IND', 'KAZ', 'NPL')
  dadj['CHN' , cn] <- dadj[cn, 'CHN'] <- 1

# move these centroids
  usa <- grep('USA', s$ISO)
  cr <- coordinates(s)
  cr[usa,] <- c(-97.99, 39.38)
  x <- s@data
  coordinates(x) <- cr
  projection(x) <- projection(s)
  s <- x


# print output

svg(file.path(outdir, 'figures', 'spatial_adjacency.svg'))  
  map('world')
  for (v in 1:nrow(dadj)){
    adj <- which(dadj[v,] > 0)
    for (a in adj){
      ind <- which(s$ISO %in% row.names(dadj)[c(v,a)] )
      lines(coordinates(s)[ind, 1 ] ,coordinates(s)[ind,2], col = rgb(0,0,0,.2))
    }
  }
  
  points(s, cex = .4, col= 'red')

dev.off()
  
dis1 <- dadj

#The choice of spatial adjacency may have a large impact on estimations of autocorrelation. Here I used delaunay triangulation, which connects points in space without 'overconnecting' and creating 'sliver triangles'. Note that I did not make any connections across the Pacific, which may not be reasonable. 


# raw distance matrix

dis2 <- pointDistance(coordinates(s), coordinates(s),lonlat = TRUE, allpairs = TRUE)

# function for calculating moran's I 

my.m <- function(v){
  
  # v must be a named vector with iso codes
  
  require(spdep)
  
  i <- names(v)[which(!is.na(v))]
  
  
  w1 <- mat2listw(dis1[i,i])
  
  v <- v[i]
  
  #moran.mc(v, w1, 999)
  corr <- sp.correlogram(w1$neighbours,v,order = 5, method = 'I', randomisation = FALSE, zero.policy = TRUE)
  #plot(corr) 
  print (length(v))
  return(corr)
  
}

#add names
g <- d$GaIN
tt <- d$TCI
names(g) <- names(tt) <- d$`Country code`

gain <- my.m(g)
tc <- my.m(tt)

svg(file.path(outdir, 'figures', 'moransI.svg'))


plot(0,0, xlim = c(.7,5.3), ylim = c(-.3, .8), type = 'n', xlab = 'Spatial Lag', ylab = "Moran's I", xaxt = 'n')
axis(1, at = 1:5, labels = 1:5)

dtc <- sqrt(tc$res[,3])*2
itc <- tc$res[,1]
dg <- sqrt(gain$res[,3])*2
ig <- gain$res[,1]

arrows((1:5+.1),itc + dtc ,(1:5+.1), itc -dtc, code = 3, angle = 90 , length = .1)
# points((1:5+.1),itc, pch = 21, lwd = 1, bg = 'grey')
points((1:5+.1),itc, pch = 15, lwd = 2, col = 'black')

arrows((1:5-.1),ig + dg , (1:5-.1), ig -dg, code = 3, angle = 90 , length = .1)
points((1:5-.1),ig, pch =16, lwd = 2, col = 'black')

abline(0,0)
legend('topright', legend = c('GaIN', 'TCI'), pch = c(16, 15, lwd = 2))

dev.off()

#High 'I' values here indicate that the variable is strongly spatially clustered, or autocorrelated. Clearly GaIN is much spatially autocorrelated across spatial lags (order of neighbors). 

# calculate whether I's are statistically different
ii <- list()

for(i in 1:5){
  
  # difference in estimate
  dif <- gain$res[i,1]-tc$res[i,1]
  # pooled variance
  vrsd <- sqrt((gain$res[i,3])+(tc$res[i,3]))
  # probability of drawing this difference given a 0 mean, vrsd variance distribution
  p <- pnorm(0,dif, vrsd)
  p # very small 
  cat('\nspatial lag ', i, ' : ', round(p,10))
  
  ii[[paste0('lag ',i)]] <- p
}

out <- as.data.frame(do.call(rbind, ii))
names(out) <- c('p-value')
write.csv(out, file.path(outdir, 'tables', 'moransI_comparison.csv'), row.names=T)

#The TCI and Gain scores are significantly different up to the 5th spatial lag. 

#It may be useful to examine patterns of autocorrelation in other variables, and to measure how sensitive global index autocorrelation is to perturbations of the index components (weights, choice of variables etc.)
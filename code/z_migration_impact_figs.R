

#######################################
library(data.table)

dir.create(file.path(outdir, 'figures', 'presentation', 'migration'), recursive = T)
tdir <- file.path(outdir, 'figures', 'presentation', 'migration')
F <- fread('data/bilateral/migration_nd.csv')
  
x <- c('SEN', 'SWE', 'KEN', 'FJI')

i <- which(is.na(F$to_iso))
j <- which(is.na(F$from_iso))
i <- union(i,j)
if(length(i) > 0 ) F <- F[-i,]

library(raster)
library(RColorBrewer)

#w <- shapefile('../data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')

W <- shapefile("data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

# Kenya

X <- list(
ken = F[to_iso == 'KEN',],
swe = F[to_iso == 'SWE',],
fij = F[to_iso == 'FJI',],
sen = F[to_iso == 'SEN',]
)

quant <- function(v){    as.numeric( cut ( v , quantile( v, seq(0, 1, length.out = 11), na.rm =TRUE)))}

for(ctr in c('KEN', 'SWE', 'FJI', 'SEN')){

x <- F[to_iso == ctr,]

#merge
w <- W
w$id <- 1:nrow(w)

# w@data <- merge(w@data, x, by.x = 'ISO3', by.y = 'to_iso', all.x = TRUE)
w@data <- merge(w@data, x, by.x = 'iso_a3', by.y = 'from_iso', all.x = TRUE)
w@data <- w@data[order(w$id),]

w$NDX[which(w$NDX == 0)]<-NA

wr <- spTransform(w, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))



nbins <- 6

qn <- quantile(w$NDX, seq(0,1, length.out = nbins), na.rm=TRUE)


#qn <- quantile(w$NDX, seq(0,1, length.out = 6),  na.rm=TRUE)
cl <- cut(w$NDX, qn, include =T)
cl <- as.numeric(cl)

cl[which(is.na(cl)) ] <- nbins + 1
focus <- which(w$iso_a3 == ctr)
cl[focus ] <- nbins +2

has <- which(!is.na(w$NDX))

graycol <- grey(.8)
focalcol <- '#d1e2ff'

# cc <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026', graycol, focalcol)
cc <- c( colorRampPalette(brewer.pal(8,"Reds"))(nbins), graycol, focalcol) 







svg(file.path(tdir, paste0('migration_cwi_', ctr, '.svg')), height = 14, width = 20)
  
  plot(wr, col = cc[cl], border = 'white')
  plot(wr[focus,], border = 'blue', add=T, lwd = 3)
  plot(wr[has,], border = 'black', add=T, lwd = 2)
  
  
  pp <- par('usr')
  hpc = .05
  xs <- seq( pp[1], pp[2], length.out = nbins + 1)
  ys <- rep(pp[3], nbins)
  yhs <- rep(pp[3] +  (pp[4]-pp[3])*hpc, nbins)


  rect(xs[-length(xs)], ys, xs[-1], yhs, col = cc[1:nbins], border = 'white')
  text((xs + .5*diff(xs))[-length(xs)], y =mean(c(ys, yhs)), round(qn, 5))

  text( xs[1], mean(yhs) + (pp[4]-pp[3])*hpc*.5, 'Migration Climate Weighted Exposure to NDGaIN', pos = 4, cex = 1.1)

dev.off()
  
shapefile(wr, file.path(tdir, paste0('migration_cwi_', ctr, '.shp')), overwrite=T) 

y <- x[which(x$n > 0),]
y$ndQ <- quant(y$nd)
y$migQ <- quant(y$PERC)
y$ndxmigQ <- y$ndQ * y$migQ
y <- y[base::order(y$PERC, decreasing =TRUE),]

write.csv2(y,file.path(tdir, paste0('migration_cwi_', ctr, '.csv')), row.names=FALSE)
 
}









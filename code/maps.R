# Maps of TCI
# Steve Fick
# 2017 05 03

library(raster)
library(RColorBrewer)

d <- read.csv(file.path(outdir, 'data.csv'), stringsAsFactors = FALSE, check.names=F)
sh <- shapefile('data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')

#merge
sh$id <- 1:nrow(sh)
sh@data <- merge(sh@data, d, by.x = 'ISO3', by.y = 'Country code', all.x = TRUE)
sh@data <- sh@data[order(sh$id),]

shr <- spTransform(sh, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

# cull small geometries 
shr$a <- area(shr)
i <- which(shr$a < quantile(shr$a, .3))
ss <- shr[-i,]


nbins <- 10
cols <- colorRampPalette(brewer.pal(8,"Blues"))(13)

xlims <- c( -13066967, 15921469)
ylims <- c(-7392096 , 8826585)

brks <- quantile(ss$GaIN, seq(0,1, length.out = nbins+1), na.rm=TRUE)
na <- which(is.na(ss$GaIN))


svg(file.path(outdir, 'figures', 'maps_NDGAIN.svg'), height = 7, width = 10)


# ND-GaIN
plot(ss,xlim = xlims, ylim = ylims, col = cols[2+ as.numeric(cut(ss$GaIN, brks))], border = 'white', lwd = .5)
plot(ss[na,], col = grey(.9), add = TRUE, border = 'white')


pp <- par('usr')
hpc = .05
xs <- seq( pp[1], pp[2], length.out = nbins + 2)
ys <- rep(pp[3], nbins+1)
yhs <- rep(pp[3] +  (pp[4]-pp[3])*hpc, nbins+1)


rect(xs[-length(xs)], ys, xs[-1], yhs, col = c(grey(.9),cols[1:nbins + 2]), border = 'white')
text((xs + .5*diff(xs))[-length(xs)], y =mean(c(ys, yhs)), c('No Data', round(brks + diff(brks), 2)[-length(brks)]))

text( xs[1], mean(yhs) + (pp[4]-pp[3])*hpc*.5, 'ND-GaIN Index of the vulnerability of countries to climate change', pos = 4, cex = 1.1)
dev.off()

# TCI

svg(file.path(outdir, 'figures', 'maps_TCI.svg'), height = 7, width = 10)


brks2 <- quantile(ss$TCI, seq(0,1, length.out = nbins+1), na.rm=TRUE)

na <- which(is.na(ss$TCI))
plot(ss,xlim = xlims, ylim = ylims, col = cols[2+ as.numeric(cut(ss$TCI, brks2))], border = 'white', lwd = .5)

plot(ss[na,], col = grey(.9), add = TRUE, border = 'white')


pp <- par('usr')
hpc = .05
xs <- seq( pp[1], pp[2], length.out = nbins + 2)
ys <- rep(pp[3], nbins+1)
yhs <- rep(pp[3] +  (pp[4]-pp[3])*hpc, nbins+1)


rect(xs[-length(xs)], ys, xs[-1], yhs, col = c(grey(.9),cols[1:nbins + 2]), border = 'white')
text((xs + .5*diff(xs))[-length(xs)], y =mean(c(ys, yhs)), c('No Data', round(brks2 + diff(brks2), 2)[-length(brks2)]))

text( xs[1], mean(yhs) + (pp[4]-pp[3])*hpc*.5, 'TCI Index of exposure to transnational climate impacts', pos = 4, cex = 1.1)

dev.off()
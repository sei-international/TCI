
d <- read.csv(file.path(outdir, 'data.csv'), stringsAsFactors = FALSE, check.names=F)

# get region
sh <- shapefile('data/global_flows_map/COUNTRY_CENTROIDS.shp')

d <- merge(d, sh@data[,c('ISO', 'UNREGION1')], by.x = 'Country code', by.y = 'ISO', all.x = TRUE)

#TCI

tc <- d[order(d$TCI, decreasing = TRUE), c('Country name','Country code', 'TCI', 'UNREGION1')]

#ND-GAIN

nd <- d[order(d$GaIN, decreasing = TRUE), c('Country name','Country code', 'GaIN', 'UNREGION1')]


write.csv(tc, file.path(outdir,'tables', 'top_scoring_TCI.csv'), row.names=F)

write.csv(nd, file.path(outdir, 'tables', 'top_scoring_NDGAIN.csv'), row.names=F)


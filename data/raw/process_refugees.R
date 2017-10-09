#
# calculate Asylum indicator values
#

# add positive asylum decisions (as percent of population) and # refuges (as percent of population). If only one value avalailable, ignore.


d <- read.csv('refugees.csv', strings =F, check.names=F)

v <- rowSums(d[, 3:4])
 

out <- d[,1:2]
out$`sum of positive asylum decisions and # refuges, both as percent of population` <- v

write.csv(out, '../refugees.csv', row.names=F)
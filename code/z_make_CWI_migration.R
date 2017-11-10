# make climate weighted indices
#
library(data.table)
# ndgain

nd <- read.csv('../data/ndgain.csv')

# fdi

f <- read.csv('../data/bilateral/migration.csv', check.names=F, stringsAsFactors=F)

#cols <- paste0('X', 2008:2012)


# ignore missing values
f$nd <- nd$GaIN[ match(f$from_iso, nd$Country.code) ]

F <- data.table(f)

# percentage of migrants by country
 F[, PERC := n/sum(n, na.rm =TRUE), by = from]

# percentage times nd
F[, NDX := PERC*nd , by = from]

out <- F[, list( migration_cwi = sum(NDX, na.rm= TRUE)),by = list(from, from_iso) ]

write.csv(F, 'data/bilateral/migration_nd.csv', row.names=FALSE)
write.csv(out , 'data/cwi_migration.csv', row.names=FALSE)

#######################################

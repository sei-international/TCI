# make climate weighted indices for FDI
#
library(data.table)

# ndgain

nd <- read.csv('data/ndgain.csv')

# fdi

f <- read.csv('data/bilateral/fdi.csv', check.names=F, stringsAsFactors=F)

cols <- paste0('X', 2008:2012)

# take absolute value of FDI
for(col in cols){  f[,col] <- abs(as.numeric(f[,col]) )   }

ignore <- c(
"Other Africa", "Unspecified", "CIS","Transition economies" , "World" 
)


regions <- c("European Union", "Other developed Europe", "North Africa", "East Asia", "South-East Asia", "South Asia", "West Asia", "South America", "Central America", "Caribbean",'cis','world',"Other Africa")

i <- which(f$from %in% regions | f$to %in% regions)
if(length(i) > 0) f <- f[-i,]

#2008:2012

# ignore missing values
denom <- 5 - apply(f[, cols], 1, function(x) sum(is.na(x)))

f$avg <- rowSums(f[,cols], na.rm =TRUE) / denom
f$nd <- nd$GaIN[ match(f$to_iso, nd$Country.code) ]

F <- data.table(f)

# average of yearly average FDI   -- Not a good way to do it!
F[, TOTAL := mean( c(sum(X2008, na.rm=TRUE), sum(X2009, na.rm=TRUE), sum(X2010, na.rm=TRUE), sum(X2011, na.rm=TRUE), sum(X2012, na.rm=TRUE)), na.rm=TRUE), by = from]

# F[, PERC := avg /sum(avg), by = from]
 F[, PERC := avg /TOTAL, by = from]


F[, NDX := PERC*nd , by = from]


out <- F[, list( cwi = sum(NDX, na.rm= TRUE)),by = list(from, from_iso) ]


write.csv(out , 'data/cwi_fdi.csv', row.names=FALSE)
write.csv(F , 'data/bilateral/fdi_nd.csv', row.names=FALSE)

#######################################

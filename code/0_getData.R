
# combine data

D <- list()

for (ind in names(indicators)){

  print( ind )
  
  # read dataset
  d <- read.csv( file.path('data', indicators[[ind]]$fname ) , stringsAsFactors=FALSE, check.names= FALSE)

  names(d)[3] <- paste0(ind, '_value')
  
  s <- score(d[,3], num.quantiles)
  d[, paste0(ind, '_score')] <- s

  D[[ind]] <- d

}

df <- Reduce(function(...) merge(..., all=T, by = c('Country code','Country name')), D)


# include covariates

  gdp <- read.csv(file.path('data', 'gdp.csv'), check.names=F, strings=F)
  df <- merge(df, gdp, by= c('Country code', 'Country name'), all=T)
  
  hdi <- read.csv(file.path('data', 'hdi.csv'), check.names=F, strings=F)
  df <- merge(df, hdi, by= c('Country code', 'Country name'), all=T)
  
  pop <- read.csv(file.path('data', 'pop.csv'), check.names=F, strings=F)
  df <- merge(df, pop, by= c('Country code', 'Country name'), all=T)

  gain <- read.csv(file.path('data', 'ndgain.csv'), check.names=F, strings=F)
  df <- merge(df, gain, by= c('Country code', 'Country name'), all=T)

# calculate overall TCI score

  g <- df[, grep('score', names(df))]
  number.ok <- apply(g, 1, function(x) sum(!is.na(x)))

  df$TCI <- ifelse(number.ok >= min.num.inds, rowMeans(g, na.rm=TRUE), NA)

# write data to output file

  write.csv(df, file.path(outdir, 'data.csv'), row.names=F)

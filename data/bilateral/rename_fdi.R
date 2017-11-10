library(readxl)

f <- "BilateralFDIoutflows_raw.xls"

ss <- excel_sheets(f)

out <- list()


for (s in ss){

  d <- read_excel(f, sheet = s, col_names=F)
  
  if(s == 'Malta') next()
  if(nchar(s) < 4){
  a <- d[,1]

  ctr <- grep('Table', a) - 1
  starts <- ctr + 6
  ends <- grep('Source:',a)-3

  for(i in 1:length(ctr)){
  
    ct <- a[ctr[i]]
    k <- d[ starts[i]:ends[i],]
    x <- cbind(ct, k, stringsAsFactors =F)
    names(x) <- c('from', 'to', paste0('X', 2001:2012))

    out [[ct]] <- x
    }
  } else {
  d <- as.data.frame(d)
  e <- d[,5]
  n <- d[,4]
  want <- which(!is.na(e)| !is.na(n))
  
  newn <- ifelse(is.na(e[want]), n[want], e[want])
  x <- d[want,paste0('X',c(7:18))]
  x <- cbind(s,newn, x, stringsAsFactors =F)
  names(x) <- c('from','to', paste0('X', 2001:2012))
  
    
  out[[s]] <- x
  
  
  }
  
}


v <- do.call(rbind, out)
row.names(v) <- NULL


# clean bilateral
library(stringdist)
#d <- read.csv('fdi.csv', strings =F)
d <- v

key <- read.csv('country_key.csv', strings =F)


regions <- c("European Union", "Other developed Europe", "North Africa", "East Asia", "South-East Asia", "South Asia", "West Asia", "South America", "Central America", "Caribbean",'cis','world')

i <- which(d$to %in% regions)
if(length(i) > 0) d <- d[-i,]


d$from_iso <- key$iso[ match(tolower(d$from), key$name)]
d$to_iso <- key$iso[ match(tolower(d$to), key$name)]



write.csv(d, 'fdi.csv', row.names=F)


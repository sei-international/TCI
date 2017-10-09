## Sensitivity and Uncertainty Analysis
# Steve Fick
# 8-29-2017

################################################################
# Effect of removing each indicator in turn on index calculation
################################################################
set.seed(1984)

d <- read.csv(file.path(outdir, 'data.csv'), stringsAsFactors = FALSE)

g <- d[, grep('score', names(d))] 

ss <- list()
for (i in names(indicators)){
  
  # recalculate index without indicator
  j <- grep(i, names(g))
  nas <- apply(g[,-j], 1, function(x) sum(!is.na(x)) >= min.num.inds)
  v <- ifelse(nas,rowMeans(g[,-j], na.rm=TRUE), NA)
  ss[[i]]$spearman <- cor(v, d$TCI, method = 'spearman', use='complete.obs')
  
}

s <- as.data.frame(cbind(names(ss), unlist(ss))) 
row.names(s) <- NULL
names(s) <- c('indicator', 'Spearman Correlation')
s$`Spearman Correlation` <- round(as.numeric(as.character(s[,2])), 4)

dir.create(file.path(outdir, 'tables'))
dir.create(file.path(outdir, 'figures'))

write.csv(s, file.path(outdir, 'tables', 'indicator_removal_effect.csv'), row.names=F)

################################################################
# effect of different bin numbers on index 
################################################################

v <- d[, grep('value', names(d))]
bb <- list()

for (bin in 1:15){

    b <- apply(v, 2, score, bin)
    nas <- apply(b, 1, function(x) sum(!is.na(x)) >= min.num.inds)
    x <- ifelse(nas,rowMeans(b, na.rm=TRUE), NA)
    bb[[as.character(bin)]] <- cor(x, d$TCI, use='complete.obs', method = 'spearman')
    
}
  
sb <- as.data.frame(cbind(names(bb), unlist(bb))) 
row.names(sb) <- NULL
names(sb) <- c('bins', 'Spearman Correlation')
sb$`Spearman Correlation` <- round(as.numeric(as.character(sb[,2])), 4)

write.csv(sb, file.path(outdir, 'tables', 'bin_number_effect.csv'), row.names=F)

########################################################
# indicator data uncertainty
########################################################


noise <- function(vect, percent.error ){
    #generate deviations based on a 0 mean normal distribution with sdev
    # equal to 
    n <- rnorm(length(vect), 0, diff(range(vect, na.rm=TRUE))*percent.error/100*2)
    #cap result to observed variable range
    pmax(min(vect,na.rm=TRUE), pmin(vect + n, max(vect,na.rm=TRUE)))
}

# for calculating change in rank
Rbar <- function(a,b) { mean(abs(rank(a)-rank(b)))     }

nsim <- 999
sims.out <- list()

for( ind in names(indicators)){
  
  cat(' running simulations for ', ind ,'\n')
  flush.console()
  
  sims <- c()
  j <- grep(ind, names(v))
  
  for ( i in 1:nsim){

     #add noise
     vs <- v
     vs[,j] <- noise(vs[,j], percent.error)
     
     #recalculate index
     b <- apply(vs, 2, score, 10)
     nas <- apply(b, 1, function(x) sum(!is.na(x)) >= min.num.inds)
     x <- ifelse(nas,rowMeans(b, na.rm=TRUE), NA)

     #compare correlations
     sims <- c(sims, cor(x, d$TCI, use='complete.obs', method = 'spearman'))
     # sims <- c(sims, Rbar(x, d$TCI))
  }

  sims.out[[ind]] <- sims
  
}


## create summary table
sumry <- lapply(sims.out, function(x) c( mean(x) - sd(x), mean(x), mean(x)+ sd(x)))
smry <- do.call(rbind, sumry)
smry <- as.data.frame(t(as.data.frame(sumry))) #Ugly !
names(smry) <- c('mean - sd', 'avg spearman rho', 'mean + sd')

write.csv(smry, file.path(outdir,'tables', 'indicator_data_permutation.csv'), row.names=F)

## create summary figure


f.out <- file.path(outdir, 'figures', 'uncertainty_indicator_permutation.svg')

svg(f.out)
dotchart( smry[,2], xlim = c( min(unlist(sims.out)), max(unlist(sims.out))), labels = row.names(smry), xlab = 'Spearman rank correlation')

for(i in 1:length(sims.out)){
  den <- (density(sims.out[[i]]))
  den$x <- c(par('usr')[1],den$x, par('usr')[2])
  den$y <- c(0,den$y,0)
  
  den$y <- den$y/max(den$y)*.5
  den$y <- den$y + i
  not <- which(den$x >= par('usr')[1] & den$x <= par('usr')[2])
  den$x <- den$x[not]
  den$y <- den$y[not]
  
  polygon(den, col = '#e2edff')
  
}

segments( smry[,1], 1:nrow(smry), smry[,3], 1:nrow(smry), lwd= 3)
points(smry[,2], 1:nrow(smry), pch = 16, cex = 2)

points(s$`Spearman Correlation`, 1:nrow(s), pch =4, cex = 1.5, lwd =3)

dev.off()

# calculate pseudo P values

# generate all possible combinations of two indicators
e <- combn(names(sims.out),2 )

#object to store results
ps <- matrix(NA, length(sims.out), length(sims.out))
row.names(ps) <- names(sims.out)
colnames(ps) <- names(sims.out)

# for each combo
for (i in 1:ncol(e)){
  
  # difference between correlations
  a <- (sum( (sims.out[[e[1,i]]] - sims.out[[e[2,i]]])>0) + 1 )/ (length(sims.out[[1]])+1) 
  
  # allow for inverse if difference is negative  
  b <- pmin(a ,1-a)
  ps[e[2,i], e[1,i]] <- pmin(1,b )
}

# bonferroni correction

ps.sig <- ps < .05/ncol(e)
ps.sig[ps.sig] <- '*'
ps.sig[ps.sig == 'FALSE'] <- ''
ps.sig[is.na(ps.sig)] <- ''

out <- matrix( paste(round(ps,3), ps.sig), nrow(ps), ncol(ps))
out[out== 'NA '] <- ''
row.names(out) <- row.names(ps)
colnames(out) <- colnames(ps)

write.csv( out, file.path(outdir, 'tables', 'pairwise_indicator_comparisons.csv'), row.names=T)

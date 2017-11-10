# reformat migration
library(readxl)
library(reshape2)
library(stringdist)
f <- 'Bilateralmigration_raw.xls'

d <- read_excel(f, skip = 1)
out <- melt(d , id.vars = names(d)[1])
names(out) <- c('from', 'to', 'n')

key <- read.csv('country_key.csv', stringsAsFactors = F)

ignore <- c("TOTAL", NA, "Sources: Ratha and Shaw (2007) updated with additional data for 71 destination countries as described in the Migration and Remittances Factbook 2011.", 
"Notes:", "(1) Bilateral migration data were created by applying weights based on bilateral migrant stocks (from population censuses of individual countries) to the UN Population Division's estimates of total migrant stocks in 2005. See Ratha, Dilip K., and William Shaw (2006). \"South-South Migration and Remittances,\" Development Prospects Group, World Bank.", 
"(2) Assumptions for allocating unidentified migrants are described in Ratha and Shaw (2007)"
)

i <- which(out$from %in% ignore | out$to %in% ignore)
out <- out[-i,]


out$from_iso <- key$iso[ match(tolower(out$from), key$name)]
out$to_iso <- key$iso[ match(tolower(out$to), key$name)]

write.csv(out, 'migration.csv', row.names=F)



# fix country names

# nd <- read.csv('../ndgain.csv', strings =F)

# key <- data.frame(from = tolower(unique(out$from)), to = NA, stringsAsFactors = F)

# m <- which(!is.na( match(key$from, tolower(nd$Country.name)) ) )

# key$to[m] <- key$from[m]  
# key$step <- NA
# key$step[m] <- 0

# pos <- tolower(nd$Country.name)

# for(i in 1:10){

  # m <- amatch(key$from, pos, method = 'lcs', max =i)
  
  # j <- which(!is.na(m) & is.na(key$to))
  # if(length(j) > 0){
    # key$to[j] <- pos[m][j]
    # key$step[j] <- i
  # }

# }


# nas <- c('andorra', 'american samoa', 'channel islands', 'gibraltar', 'greenland', 'guam', 'isle of man','kosovo', 'mayotte','northern mariana islands', 'samoa','tuvalu')

# fromto = c(
# 'congo, dem. rep.'  = 'demogratic republic of the congo', 
# 'egypt, arab rep.' = 'egypt',
# 'iran, islamic rep.' = 'iran',
 # "korea, dem. rep." = "korea, democratic people's republic of",
# 'kyrgyz republic' = 'kyrgyzstan',
# 'lao pdr' = "lao people's democratic republic",
# "libya" = "libyan arab jamahiriya",
# "micronesia, fed. sts."  = "micronesia, federated states of",
# "myanmar" = "burma",
# "russian federation" = "russia",
# "tanzania" = "united republic of tanzania",
# "macedonia fyr" = "the former yugoslav republic of macedonia")


# z <- match(nas, key$from)
# key$to[z] <- NA
# key <- key[1:213,]

# r <- fromto[key$from]
# key$to[which(!is.na(r))] <- r[which(!is.na(r))]
# key$ISO <- nd$Country.code[match (key$to, tolower(nd$Country.name)) ]

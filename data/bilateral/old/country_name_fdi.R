
# from <- unique(c(d$from, d$to))


# nd <- read.csv('../ndgain.csv', strings =F)

# key <- data.frame(from = tolower(from), to = NA, stringsAsFactors = F)

# m <- which(!is.na( match(tolower(key$from), tolower(nd$Country.name)) ) )

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





# nas <- c(tolower(c("European Union", "Other developed Europe", "North Africa", 
# "East Asia", "South-East Asia", "South Asia", "West Asia", "South America", 
# "Central America", "Caribbean")),c(
# 'niue', 'anguilla', 'nauru','reunion','guernsey', 'andorra', 'cis', 'other africa', 'pitcairn', 'samoa', 'world','tokelau', 'cook islands', 'saint helena', 'macao, china', 'american samoa', 'christmas island', 'unspecified', 'british indian ocean', 'jersey', 'guam', 'gibraltar', 'isle of man'))


# fromto = c(
# "libya" = "libyan arab jamahiriya",
# "myanmar" = "burma",
# 'plurinational state of bolivia ' = 'bolivia',
# 'macedonia' =   "the former yugoslav republic of macedonia",
# 'the fyr of macedonia' =   "the former yugoslav republic of macedonia",
# "moldova" = "republic of moldova",
# "moldova, republic of"= "republic of moldova",
# "korea"= "korea, republic of",
# "bolivarian republic of venezuela" = 'venezuela',
# "venezuela, bolivarian rep. of" = 'venezuela',
# "russian federation" = "russia",
# "czech" = "czech republic",
# "lao people's dem. rep." = "lao people's democratic republic",
# "libya " = "libyan arab jamahiriya",
# "islamic republic of iran " = 'iran',
# " islamic republic of iran" = 'iran',
# "islamic republic of iran" = 'iran',
# "yugoslavia (former)" =  "the former yugoslav republic of macedonia",
# "bolivia, plurinational state of" = "bolivia",
# "taiwan province of china"= 'taiwan')

# z <- match(nas, key$from)
# key$to[z] <- NA

# r <- fromto[key$from]

# key$to[which(!is.na(r))] <- r[which(!is.na(r))]
# key$ISO <- nd$Country.code[match (key$to, tolower(nd$Country.name)) ]

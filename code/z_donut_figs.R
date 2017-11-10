
# load GGplot2
library(ggplot2)
 
dir.create(file.path(outdir, 'figures', 'presentation'), recursive =T)
tdir <- file.path(outdir, 'figures', 'presentation')

# Create test data.
d = read.csv(file.path(outdir, 'data.csv'))

cats = structure(c("Biophysical", "Finance", "Finance", "People", "People", 
"Trade", "Trade", "Trade", "Global"), .Names = c("Transboundary_Water_score", 
"FDI_score", "Remittances_score", "Asylum_score", "Migration_score", 
"Trade_Openness_score", "Cereal_Imports_score", "Embedded_Water_score", 
"Globalization_score"))

cc <- c('Kenya'  = 'KEN', 'Senegal' = 'SEN', 'Sweden' = 'SWE', 'Fiji' = 'FJI' )

for(ctr in cc  ){
 cat(ctr, '\n');flush.console()
 x <- d[d$Country.code == ctr, ]

 cz <- grep('score', names(x))
 dat <- data.frame( count = unlist(x[,cz]), subcategory = gsub('_score', '', names(x))[cz], stringsAsFactors=F)
 dat$category <- cats[paste0(dat$subcategory, "_score")]

 dat <- na.omit(dat)
 # Add addition columns, needed for drawing with geom_rect.
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$category), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))

svg( file.path(tdir, paste0( ctr, "_TCI_donut.svg")), height=10, width = 11)
 
# Make the plot
p1 = ggplot(dat)+
     geom_rect(aes(fill=subcategory, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
     geom_rect(aes(fill =category, ymax = ymax, ymin = ymin, xmin =2, xmax =3)) +
     coord_polar(theta="y") +
     xlim(c(0, 4)) +
     theme(panel.grid=element_blank()) +
     theme(axis.text=element_blank()) +
     theme(axis.ticks=element_blank()) +
     annotate("text", x = 0, y = 0, label = names(cc)[match(ctr,cc)]) +
     labs(title='')
print(p1)

dev.off()

write.csv( dat, file.path(tdir, paste0(ctr, '_TCI_donut_data.csv')), row.names=F)
}

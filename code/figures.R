# setup

d <- read.csv(file.path(outdir, 'data.csv'), stringsAsFactors = FALSE, check.names=F)# create function that recomputes indices.

# indicators <- grep('Indicator [0-9]{1,2}:', names(d))
# scores <- grep('Indicator.*?score', names(d) )
# tci <- d[,"AVERAGE of countries with ?7 indicators"]
# HDI <- as.numeric(gsub(',','.', d[,'HDI']))
# g <- d$GaIN
# country <- d$`Country name`
# kof <- d$`Indicator 11: KOF Globalisation Index`

d$HDI <- as.numeric(d$HDI)


######################################################################
# density figures
######################################################################

#make plots

svg( file.path(outdir, 'figures', 'distributions.svg'),  width = 8, height = 12)
par(mfrow = c(5,2))
plot(0,0,type = 'n',yaxt = 'n', xaxt = 'n',  xlab = '', ylab='')
legend(-1,1,lty = 2, col = c('blue', 'black', 'red'), lwd = 3, legend = c('SWEDEN', 'ETHIOPIA', 'THAILAND'), cex = 2)

for (i in names(indicators)){
  
  j <- paste0(i, '_value')
  name <- gsub('_', ' ', i)
  hist( d[,j], n = 20, col = 'grey', xlab = 'score',main = paste0( substr( name, 1,40 ), '\n', substr(name, 41, 200)))
  swe <- d[grep("Sweden", d$`Country name`), j]
  eth <- d[grep("Ethiopia", d$`Country name`), j]
  thai <- d[grep("Thailand", d$`Country name`), j]
  abline(v = swe, lty = 2, col = 'blue')
  abline(v = eth, lty = 2, col = 'black')
  abline(v = thai, lty = 2, col = 'red')
  
}

dev.off()

######################################################################
# comparison figure 
######################################################################

toFollow <- c('Burundi', 'Sweden', 'Madagascar', 'Netherlands', 'Jordan', 'Indonesia' )
m <- which(d$`Country name` %in% toFollow)

svg( file.path(outdir, 'figures', 'comparison.svg'), width = 11, height = 11)
  par(mar = c(4.1,4.1,1,1))
  par(mfrow = c(2,2))

  plot(d$HDI,d$GaIN, pch = 16, cex = 1.2,col = rgb(255, 182, 193, 230, max = 255) , xlab = 'HDI', ylab = 'NDGaIN')
  #points(HDI,g, pch = 1, lwd = 2,col = rgb(152, 0, 0, 200, max = 255))

  text(d$HDI[m], d$GaIN[m], d$`Country name`[m])
  text(.8,.6, paste0('r = ' , round( cor(d$HDI, d$GaIN, use = 'complete.obs'),2)))



  plot(d$HDI,d$TCI, pch = 16, cex = 1.2,col = rgb(193, 182, 255, 230, max = 255) , xlab = 'HDI', ylab = 'TCI', xlim = c(.3, 1.01))
  #points(HDI,tci, pch = 1, lwd = 2,col = rgb(0, 0, 152, 200, max = 255))


  text(d$HDI[m]-.02, d$TCI[m], d$`Country name`[m])
  text(.9,3, paste0('r = ' , round( cor(d$HDI, d$TCI, use = 'complete.obs'),2)))

  
  
  plot(d$Globalization_value,d$GaIN, pch = 16, cex = 1.2,col = rgb(255, 182, 193, 230, max = 255) , xlab = 'KOF', ylab = 'NDGaIN')
  #points(HDI,tci, pch = 1, lwd = 2,col = rgb(0, 0, 152, 200, max = 255))

  text(d$Globalization_value[m]-.02, d$GaIN[m], d$`Country name`[m])
  text(80,.6, paste0('r = ' , round( cor(d$Globalization_value, d$GaIN, use = 'complete.obs'),2)))

  
  plot(d$Globalization_value,d$TCI, pch = 16, cex = 1.2,col = rgb(193, 182, 255, 230, max = 255) , xlab = 'KOF', ylab = 'TCI')
  #points(HDI,tci, pch = 1, lwd = 2,col = rgb(0, 0, 152, 200, max = 255))

  text(d$Globalization_value[m]-1.9, d$TCI[m], d$`Country name`[m])
  text(80,3, paste0('r = ' , round( cor(d$Globalization_value, d$TCI, use = 'complete.obs'),2)))
  
  
  
dev.off()


  # a <- list(
  
  # x = HDI[m],
  # y = g[m],
  # text = country[m],
  # xref = 'x',
  # yref = 'y',
  # showarrow = TRUE,
  # arrowhead = 7,
  # ax = 20,
  # ay = -40
  

# )
# b <- list(
  
  # x = HDI[m],
  # y = tci[m],
  # text = country[m],
  # xref = 'x2',
  # yref = 'y2',
  # showarrow = TRUE,
  # arrowhead = 7,
  # ax = 20,
  # ay = -40
  

# )

# p1 <- plot_ly(x = HDI, y = g, mode = 'markers',
        # marker = list(size = 10,
                       # color = 'rgba(255, 182, 193, .9)',
                       # line = list(color = 'rgba(152, 0, 0, .8)',
                                   # width = 2))) %>%
  # layout(title = 'Styled Scatter',
         # yaxis = list(zeroline = FALSE),
         # xaxis = list(zeroline = FALSE),
         # annotations = a)

# p2 <- plot_ly(x = HDI, y = tci, 
        # marker = list(size = 10,
                       # color = 'rgba(193, 182, 255, .9)',
                       # line = list(color = 'rgba(0, 0, 155, .8)',
                                   # width = 2))) %>%
  # layout(title = 'Styled Scatter',
         # yaxis = list(zeroline = FALSE),
         # xaxis = list(zeroline = FALSE), annotations = b)

# subplot(p1,p2)

# library(plotly)


# economics <- data.frame(date = HDI, unemploy= g, uempmed = tci )

# m <- economics[which.max(economics$unemploy), ]
# n <- economics[which.max(economics$uempmed), ]

# annotations
# a <- list(
  # x = m$date,
  # y = m$unemploy,
  # text = "annotation a",
  # xref = "x",
  # yref = "y",
  # showarrow = TRUE,
  # arrowhead = 7,
  # ax = 20,
  # ay = -40
# )

# b <- list(
  # x = n$date,
  # y = n$uempmed,
  # text = "annotation b",
  # xref = "x2",
  # yref = "y2",
  # showarrow = TRUE,
  # arrowhead = 7,
  # ax = 20,
  # ay = -40
# )

# figure labels
# f <- list(
  # family = "Courier New, monospace",
  # size = 18,
  # color = "#7f7f7f ")
# x <- list(
  # title = "x Axis",
  # titlefont = f)
# y <- list(
  # title = "y Axis",
  # titlefont = f)

# p1 <- plot_ly(economics, x = ~date, y = ~unemploy) %>%
  # add_markers(name = ~"unemploy") %>%
  # layout(annotations = a, xaxis = x, yaxis = y)
# p2 <- plot_ly(economics, x = ~date, y = ~uempmed) %>%
  # add_markers(name = ~"uempmed") %>%
  # layout(annotations = b, xaxis = x, yaxis = y)
# p <- subplot(p1, p2, titleX = TRUE, titleY = TRUE) %>%
  # layout(showlegend = FALSE)




setwd("~/Smith/Senior Year/Stoudt_Thesis/FINAL_CODE/minDataForThesis")
us<-read.csv("usToUseNoDuplicates.csv")

## manually project 
map("state", proj = "lambert", param = c(33,45), orientation = c(90,0,-100))
projData =  mapproject(us$long, inaa$lat, projection = "")
us$longProj=projData$x
us$latProj=projData$y

## manually assign color
require(RColorBrewer)
rbPal <- colorRampPalette(c(brewer.pal(9, "OrRd"),"black"))

col <- rbPal(10)[as.numeric(cut(us$uranium,
                                breaks = quantile(us$uranium,seq(0,1,by=.1))))]


require(maps)
map("state", proj = "lambert", param = c(33,45), orientation = c(90,0,-100))
points(us$longProj,us$latProj,col=col,cex=.5,pch=19)

require(SDMTools)
legend.gradient(cbind(x=c(-.185,-.175,-.175,-.185),y=c(-.86,-.86,-.905,-.905)), 
                cols = c(brewer.pal(9, "OrRd"),"black"), title="", limits = "",cex=.75)

## manually add legend labels
text(-.165,-.86-.0015,round(quantile(us$uranium,seq(0,1,by=.1))[11],2),cex=.75)
text(-.165,-.86-.0045-.002,round(quantile(us$uranium,seq(0,1,by=.1))[10],2),cex=.75)
text(-.165,-.86-.0045-.0045-.002,round(quantile(us$uranium,seq(0,1,by=.1))[9],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.002,round(quantile(us$uranium,seq(0,1,by=.1))[8],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.002,round(quantile(us$uranium,seq(0,1,by=.1))[7],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0015,round(quantile(us$uranium,seq(0,1,by=.1))[6],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0045-.0015,round(quantile(us$uranium,seq(0,1,by=.1))[5],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0015,round(quantile(us$uranium,seq(0,1,by=.1))[4],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0015,round(quantile(us$uranium,seq(0,1,by=.1))[3],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0015,round(quantile(us$uranium,seq(0,1,by=.1))[2],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0015-.0045,round(quantile(us$uranium,seq(0,1,by=.1))[1],2),cex=.75)
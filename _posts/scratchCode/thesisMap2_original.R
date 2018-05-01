#### data ####
setwd("~/Smith/Senior Year/Stoudt_Thesis/Data")
us<-read.csv("usToUseNoDuplicates.csv")

setwd("~/Smith/Senior Year/Stoudt_Thesis/Data/FINAL")
training<-read.csv("trainingFinal.csv")
testing<-read.csv("testFinal.csv")


#### model ####
require(mgcv)


b1=c("cr","ps","tp")

isPenalized1=c(T,F)
te1Param<-as.data.frame(expand.grid(b1,isPenalized1))
names(te1Param)<-c("basis","isPenalized")
te1Param$basis=as.character(te1Param$basis)

training$uraniumTransform=1/(1+exp(-training$uranium)) # transform to [0,1] for beta
testing$uraniumTransform=1/(1+exp(-testing$uranium))

data=te1Param[3,]
basis=data[[1]]
isPenalized=data[[2]]

uranium.gam=gam(uraniumTransform ~ te(longProj,latProj,bs=basis,fx=isPenalized),
                family=betar(link="logit"), data=training)

x = seq(from=min(us$longProj), to=max(us$longProj), by=.001) # training made from nure
y = seq(from=min(us$latProj), to=max(us$latProj), by=.001)
nx = length(x); ny = length(y)
xy = expand.grid(longProj=x,latProj=y)

zGRID.gam = predict(uranium.gam, newdata=xy)
trainPred=predict(uranium.gam,newdata=training)
testPred=predict(uranium.gam, newdata=testing)

#### plotting hack ####

require(RColorBrewer)

#http://wiki.cbr.washington.edu/qerm/index.php/R/Contour_Plots for filled.contour2
filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = quantile(z,seq(0,1,.1),na.rm=T), nlevels = 10, color.palette = brewer.pal(9, "OrRd"), 
            col =  c(brewer.pal(9, "OrRd"),"black"),
            #color.palette(length(levels) - 1) 
            plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...) 
    #quantile(z,seq(0,1,.1))
    
  {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    par(las = las)
    mar <- mar.orig
    plot.new()
    par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), # here made a change
                    col = col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1)
        Axis(y, side = 2)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }

#### plot ####


require(maps) 
require(SDMTools)
boundaryX<-map("usa",proj = "lambert", param = c(33,45), orientation = c(90,0,-100),plot=F)$x
boundaryY<-map("usa",proj = "lambert", param = c(33,45), orientation = c(90,0,-100),plot=F)$y
xrange <- range(boundaryX, na.rm=TRUE)
yrange <- range(boundaryY, na.rm=TRUE)
xbox <- xrange + c(-2, 2)
ybox <- yrange + c(-2, 2)

#png("gamTE1BestNice.png", width=16, height=12, units="in", res=300)
map("state", proj = "lambert", param = c(33,45), orientation = c(90,0,-100))
filled.contour3(x,y,array(zGRID.gam,dim=c(nx,ny)),plot.axes=map("state",projection="",lwd = 2, add=T))
polypath(c(boundaryX, NA, c(xbox, rev(xbox))),
         c(boundaryY, NA, rep(ybox, each=2)),
         col="white", rule="evenodd")
legend.gradient(cbind(x=c(-.185,-.175,-.175,-.185),y=c(-.86,-.86,-.905,-.905)), 
                cols = c(brewer.pal(9, "OrRd"),"black"), title="", limits = "",cex=.75)

text(-.165,-.86-.0015,round(quantile(zGRID.gam,seq(0,1,by=.1))[11],2),cex=.75)
text(-.165,-.86-.0045-.002,round(quantile(zGRID.gam,seq(0,1,by=.1))[10],2),cex=.75)
text(-.165,-.86-.0045-.0045-.002,round(quantile(zGRID.gam,seq(0,1,by=.1))[9],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.002,round(quantile(zGRID.gam,seq(0,1,by=.1))[8],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.002,round(quantile(zGRID.gam,seq(0,1,by=.1))[7],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0015,round(quantile(zGRID.gam,seq(0,1,by=.1))[6],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0045-.0015,round(quantile(zGRID.gam,seq(0,1,by=.1))[5],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0015,round(quantile(zGRID.gam,seq(0,1,by=.1))[4],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0015,round(quantile(zGRID.gam,seq(0,1,by=.1))[3],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0015,round(quantile(zGRID.gam,seq(0,1,by=.1))[2],2),cex=.75)
text(-.165,-.86-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0045-.0015-.0045,round(quantile(zGRID.gam,seq(0,1,by=.1))[1],2),cex=.75)
#dev.off()

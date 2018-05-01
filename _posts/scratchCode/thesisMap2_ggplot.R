### set up ###
require(ggplot2)
require(mgcv)
require(maps)
require(sp)
require(tigris)
require(RColorBrewer)

setwd("~/Smith/Senior Year/Stoudt_Thesis/Data")
us<-read.csv("usToUseNoDuplicates.csv")

setwd("~/Smith/Senior Year/Stoudt_Thesis/Data/FINAL")
training<-read.csv("trainingFinal.csv")
testing<-read.csv("testFinal.csv")

## do on original coordinates for now
x = seq(from=min(us$long), to=max(us$long), by=.1) 
y = seq(from=min(us$lat), to=max(us$lat), by=.1)
nx = length(x); ny = length(y)
xy = expand.grid(long=x,lat=y)

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

uranium.gam=gam(uraniumTransform ~ te(long,lat,bs=basis,fx=isPenalized),
                family=betar(link="logit"), data=training)

zGRID.gam = predict(uranium.gam, newdata=xy)

coordPred=cbind.data.frame(x,y,zGRID.gam)

#### get within continental US ####

pts = SpatialPoints(coordPred[,1:2])

#https://journal.r-project.org/archive/2016/RJ-2016-043/RJ-2016-043.pdf
us_states <- unique(fips_codes$state)[1:51]
continental_states <- us_states[!us_states %in% c("AK", "HI")]
us_pumas <- rbind_tigris(
  lapply(
    continental_states, function(x) {
      pumas(state = x, cb = TRUE)
    }
  )
)

proj4string(pts)=proj4string(us_pumas)

withinContinental=over(pts,us_pumas)

toPlot=coordPred[which(!is.na(withinContinental[,1])),] ## all the same NA structure
names(toPlot)[3]="z"

#### color coded points ####

all_states <- map_data("state")
p <- ggplot()+ geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="black", fill="white" )
p=p+geom_point(data=toPlot,aes(x=x,y=y,col=cut(z,quantile(z,seq(0,1,by=.1),include.lowest=T))))+scale_colour_manual(name="Predicted Quantile of \n Uranium (ppm)",values=c(brewer.pal(9, "YlOrRd"),"black"))
#https://cran.r-project.org/web/packages/autoimage/vignettes/ggplot2-comparison.html
p+geom_path(aes(x = long, y = lat, group = group), data = all_states)+coord_map("lambert", parameters = c(c(33,45)))
  


#### continuous heat map ####
  #https://stackoverflow.com/questions/37529116/how-to-plot-a-heat-map-with-irregular-data-in-ordinates-in-ggplot?rq=1
p= ggplot(toPlot, aes(x = x, y = y, z = z)) + stat_summary_2d(binwidth = 0.3) +scale_fill_gradientn(colours = c(brewer.pal(9, "YlOrRd"),"black")) 
p
p=p+geom_path(aes(x = long, y = lat, z=NA,group = group), data = all_states) ## doesn't work
p+coord_map("lambert", parameters = c(c(33,45)))
## still want colors by quantile


#### To-Do ###

## I want to be able to model on projected scale and then plot.
## Do I need to back project to plot just to reproject via coord_map?

## https://www.rdocumentation.org/packages/move/versions/3.0.1/topics/spTransform
## https://www.r-bloggers.com/map-projections-for-r-shapefiles-united-states/
  
# Plot Palaeomaps
# credit to Ben Moon for the simple line maps
# https://bcmoon.uk/palaeontology/palaeomaps-in-R/

if (!require("pacman")) install.packages("pacman")
pacman::p_load(GISTools,ggplot2,ggthemes,tidyverse,rgplates,chronosphere,ncdf4,divDyn,rgdal,broom,raster,sf,inlmisc)


# get PBDB data

#pbdb_url <-
"https://paleobiodb.org/data1.2/occs/list.csv?base_name=maniraptora&interval=Middle%20Jurassic&show=coords,paleoloc"

pbdb_url <-
  "https://paleobiodb.org/data1.2/occs/list.csv?base_name=Maniraptoriformes,maniraptora&interval=Mesozoic&show=attr,class,coords,loc,paleoloc"

dat <- readr::read_csv(pbdb_url)

# using GPlates and PaleoMAP
# https://www.evolv-ed.net/courses/tutorials/20191219_reconstruct_tutorial/
# must have GPlates installed on local machine
# https://www.gplates.org/

# time slice the data into 10ma BINS
dat$mid_ma <- plyr::round_any((dat$max_ma + dat$min_ma)/2,5,f=floor) # get midpoints
data(tens)
sliTen <- slice(dat$mid_ma, breaks=tens$bottom, ts=TRUE) # slice the fossil data using the 10 Ma timeslices
dat$timeslice <- sliTen$slc # add time slice number

# get rough stage names which correspond to the slices
stage_names <- subset(tens,ten>28 & ten<44)
stage_names <- stage_names[,c(1,9)]
stage_names$stage <- c("Hettangian to Sinemurian","Hettangian to Sinemurian","Pliensbachian","Toarcian to Aalenian","Bajocian to Bathonian","Callovian to Kimmeridgian","Tithonian","Berriasian to Valanginian","Hauterivian to Barremian","Aptian","Albian","Cenomanian","Turonian to Santonian","Campanian","Maastrichtian")

# now remove the data we don't want
# only keepingslices 33 to 43
dat <- subset(dat, timeslice > 28 & timeslice < 42)
#remove birds
dat <- subset(dat, class != "Aves")
# remove family is NA
dat <- subset(dat, !is.na(family))
#remove oootaxa and footprints
dat <- dat[grep("oolithidae", dat$family, invert=TRUE), ]
dat <- dat[grep("podidae", dat$family, invert=TRUE), ]
#move some columns around for clarity
dat <- relocate(dat,c(max_ma,min_ma),.before=mid_ma)
#delete columns for clarity
dat <- dplyr::select(dat,-c("latlng_basis","latlng_precision","geogscale","paleomodel","geoplate","reference_no","accepted_attr","difference","identified_no","record_type","reid_no","flags"))
#add in timeslice names
names(stage_names)[names(stage_names) == "ten"] <- "timeslice"
dat$ts2 <- dat$timeslice
dat <- merge(dat,stage_names,by.x="timeslice",by.y="timeslice")
names(dat)[names(dat) == "ts2"] <- "timeslice"

# Now export to excel for clean up!!
# REIMPORT AS dat_clean
###############################
# now reconstruct points using GEOPLATE model

dat_clean <- add_column(dat_clean,GPLATES_LONG=NA)
dat_clean <- add_column(dat_clean,GPLATES_LAT=NA)
dat_clean$GPLATES_LONG <- as.numeric(dat_clean$GPLATES_LONG)
dat_clean$GPLATES_LAT <- as.numeric(dat_clean$GPLATES_LAT)

for (i in 1:nrow(dat_clean)) {
  temp_row <- dat_clean[i,]
  pal_coords <- reconstruct(temp_row[,9:10],temp_row$Recon_age,model="PALEOMAP")
  dat_clean[i,]$GPLATES_LONG <- pal_coords[,1]
  dat_clean[i,]$GPLATES_LAT <- pal_coords[,2]
}

write.csv(dat_clean,"Manirap_recon.csv")

#dems <- fetch(dat="paleomap", var="dem") # get PALEOMAP DEMS
#demord <- matchtime(dems, tens$bottom) # reorder dem to match timeslices calculated above

pr <- fetch(dat="paleomap", var="paleoatlas", res=0.1) # get PALEOMAP Atlas

# remove any questionable taxa (family = Disputed)
disputed_taxa <- dat_clean[grep("Disputed", dat_clean$family, invert=FALSE), ]
dat_clean <- dat_clean[grep("Disputed", dat_clean$family, invert=TRUE), ]


# write multi page pdf for inkscape
# accepted taxa in solid red, disputed in open circle

pdf("pal_maps.pdf",width=8.2,height=11.6)
#par(mfrow=c(2,2))

# middle jurassic
# use recon age of 165
mj <- pr["165",]
mapplot(mj,rgb=TRUE)
t <- subset(dat_clean, Recon_age == 165)
t2 <- subset(disputed_taxa, Recon_age == 165)
points(t$GPLATES_LONG,t$GPLATES_LAT,pch=20,col="red",cex=0.75)
points(t2$GPLATES_LONG,t2$GPLATES_LAT,pch=1,col="red",cex=0.75)
rect(-180,-90,180,90,lwd=3,border="white")
title(main="Middle Jurassic",outer = TRUE, adj=0, line=-18.5)

# late jurassic
# use recon age of 150
mj <- pr["150",]
mapplot(mj,rgb=TRUE)
t <- subset(dat_clean, Recon_age == 150)
t2 <- subset(disputed_taxa, Recon_age == 150)
points(t$GPLATES_LONG,t$GPLATES_LAT,pch=20,col="red",cex=0.75)
points(t2$GPLATES_LONG,t2$GPLATES_LAT,pch=1,col="red",cex=0.75)
rect(-180,-90,180,90,lwd=3,border="white")
title(main="Late Jurassic",outer = TRUE, adj=0, line=-18.5)

# early cretaceous
# use recon age of 125
mj <- pr["125",]
mapplot(mj,rgb=TRUE)
t <- subset(dat_clean, Recon_age  > 95 & Recon_age < 150)
t2 <- subset(disputed_taxa, Recon_age  > 95 & Recon_age < 150)
points(t$GPLATES_LONG,t$GPLATES_LAT,pch=20,col="red",cex=0.75)
points(t2$GPLATES_LONG,t2$GPLATES_LAT,pch=1,col="red",cex=0.75)
rect(-180,-90,180,90,lwd=3,border="white")
title(main="Early Cretaceous",outer = TRUE, adj=0, line=-18.5)

# late cretaceous
# use recon age of 80
mj <- pr["80",]
mapplot(mj,rgb=TRUE)
t <- subset(dat_clean, Recon_age  < 110)
t2 <- subset(disputed_taxa, Recon_age  < 110)
points(t$GPLATES_LONG,t$GPLATES_LAT,pch=20,col="red",cex=0.75)
points(t2$GPLATES_LONG,t2$GPLATES_LAT,pch=1,col="red",cex=0.75)
rect(-180,-90,180,90,lwd=3,border="white")
title(main="Late Cretaceous",outer = TRUE, adj=0, line=-18.5)

dev.off()


#pdf("pal_maps.pdf",width=8.2,height=11.6)
#par(mfrow=c(3,3))
#for(i in sort(unique(dat_clean$Recon_age))){
#  print(i)
#  mjR <- pr[i,1] # red 
#  mjG <- pr[i,2]# green
#  mjB <- pr[i,3]# blue
#  mj <- stack(mjR,mjG,mjB) # raster stack
#  t <- subset(dat_clean, Recon_age == i)
#  t2 <- subset(stage_names, ten == i) # for title
#  t3 <- subset(tens, ten == i)
#  t3 <- paste0(t3$bottom," to ", t3$top," Ma")
#  mapplot(mj,rgb=TRUE)
#  points(t$GPLATES_LONG,t$GPLATES_LAT,pch=20,col="red")
#  rect(-180,-90,180,90,lwd=3,border="white")
#  title(main=paste0(t2," ",t3),outer = TRUE, adj=0, line=-18.5) #-18.5 adjusts the y pos of title
#}
#dev.off()

##


###########################
#create palaeocoastlines for the Mesozoic 200 Ma to 66Ma 
#export to shapefiles for use externally
# remember to set working directory
#wd <- getwd()
#export_dir <- setwd(choose.dir(caption="Choose Directory for Export"))
#setwd(export_dir)
#coast <- reconstruct("coastlines", model = "PALEOMAP", age=c(66, seq(70,200,by=5))) # get coastlines which match Mesozoic Altas slices from 200 Ma to 66 Ma

#for (i in 1:length(coast)){
#  for (t in c(66, seq(70,200,by=5))){
#
#writeOGR(obj=coast[[i]],dsn=getwd(),layer=paste0(t,"_Ma_Coastline_PALEOMAP"),driver="ESRI Shapefile")
#}
#}
#setwd(wd)















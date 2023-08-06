library(tidyverse)
library(viridis)
library(hrbrthemes)
library(mapdata)
library(cowplot)
library(ggplot2)
library(mapsf)
library(sf)


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# Get the world polygon
world <- map_data("world")

# assume file infile_training loaded
# get data required 
taxadist <- infile_training[c("Clade2","Period","Lower_Stage","Upper_Stage","Lat","Long")]

# using ggplot

p <- taxadist %>%
  mutate(Lat=round(Lat,1)) %>%
  mutate(Long=round(Long,1)) %>%
  group_by(Lat, Long, Period, Clade2) %>%
  summarise(n=n()) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(x=Long, y=Lat, color=Period, size=n), alpha=0.4) +
  scale_color_manual(values=c("#7FC64E", "#34B2C9", "#812B92"), guide = guide_legend(override.aes = list(size = 4) )) +
  scale_size_continuous(range=c(1,20)) +
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(0,0,0,0), "cm"),
    legend.position="bottom",
    legend.box="vertical", legend.margin=margin()) +
  xlim(-180,180) +
  ylim(-80,80) +
  scale_x_continuous(expand = c(0.006, 0.006)) +
  coord_equal() 

p + theme(legend.position = c(0.87, 0.25),
         legend.background = element_rect(fill = "white", color = "black"),legend.title = element_blank())

pdf("data_distribution.pdf")
p
dev.off()

png(file="data_distribution.png", width = 16, height = 11.8, units="cm", res = 600) 
p
dev.off()


# sep legend
legend <- get_legend(p + theme(legend.position = c(0.5, 0.25),
                                legend.background = element_rect(fill = "white", color = "white"),legend.title = element_blank()))

p_NoL <- p + theme(legend.position="none")

pdf("data_distribution_no_legend.pdf")
p_NoL
dev.off()

png(file="data_distribution_no_Legend.png", width = 16, height = 11.8, units="cm", res = 600) 
p_NoL
dev.off()

png(file="legend.png", width = 10, height = 16, units="cm", res = 600) 
plot_grid(legend,ncol=1)
dev.off()


# using mapsf

world <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

p <- taxadist %>%
  mutate(Lat=round(Lat,1)) %>%
  mutate(Long=round(Long,1)) %>%
  group_by(Lat, Long, Period, Clade2) %>%
  summarise(n=n())


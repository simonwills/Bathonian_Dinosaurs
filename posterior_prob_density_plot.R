# density plots

# read in excel file containing posterior probabilities and taxa


library(ggplot2)
library(tidyverse)
library(plyr)

mu <- ddply(Ensemble_P_Prob, "ML", summarise, grp.mean=mean(P)) # calculate grp means

taxa <- c("Dromaeosaur Morphotype A","Dromaeosaur Morphotype B","Dromaeosaur Morphotype C" )
e2 <- subset(Ensemble_P_Prob, ML %in% taxa)


density_plot <- e2  %>%
  ggplot(data=e2, mapping=aes(x = P, fill= ML)) +
  geom_density(alpha=0.2) +
  geom_vline(data=mu[1:3,], aes(xintercept=grp.mean, color=ML),
             linetype="dashed") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "bottom") +
  xlab("Posterior probability") +
  ylab("Density")


pdf("posterior_distribution.pdf")
density_plot
dev.off()

png(file="posterior_distribution.png", width = 16, height = 11.8, units="cm", res = 600) 
density_plot
dev.off()
# By clade accuracy plots



library(readxl,ggplot2,tidyr)
ByCladeTestAccuracy <- read_excel("TestingCladeAccuracies/ByCladeTestAccuracy.xlsx")

df <- ByCladeTestAccuracy %>% pivot_longer(!Taxon, names_to = "Model", values_to = "Accuracy")

#bar plot
ggplot(df,aes(Taxon, Accuracy, fill=Model)) + geom_col(position="dodge")

bold.12.text <- element_text(face = "bold", size = 12)
normal.12.text <- element_text(size = 12)

#line plot
taxon_line <- ggplot(df, aes(x = Taxon, y = Accuracy,colour = factor(Model),group=Model)) +
geom_point(size=4,shape=15) + 
  geom_line(size=1.2) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(panel.background = element_blank(), # remove plot background
        panel.grid.major = element_blank(), # remove grid
        panel.grid.minor = element_blank(), # remove grid
        plot.margin = ggplot2::margin(10, 20, 10, 10), # set plot margins - top, right, bottom, left
        legend.title = element_blank(), # remove legend title
        legend.position = c(.2, .2), # place legend inside plot boundaries
        legend.key=element_blank(), # remove legend background
        legend.justification = c("right", "top"),
        legend.key.size = unit(1, "cm"), # set legend size / spacing (relies on function above)
        legend.box.just = "right",
        panel.border = element_rect(colour = "black", fill=NA, size=2),axis.text.y = normal.12.text,axis.title.x = bold.12.text,
        axis.title.y = bold.12.text,
        axis.text.x=element_text(color = "black", size=11, angle=90, vjust=0.5, hjust=0.5)) # adjust posiiton and size of x axis tick labels
# add + coord_flip() if needed

tiff("Taxon_Accuracy_By_Model.tiff", units="cm", width=21, height=29, res=600)
taxon_line
dev.off()

png("Taxon_Accuracy_By_Model.png", units="cm", width=21, height=29, res=600)
taxon_line
dev.off()

# FOR PDF ONLY

taxon_line <- ggplot(df, aes(x = Taxon, y = Accuracy,colour = factor(Model),group=Model)) +
  geom_point(size=4,shape=15) + 
  geom_line(size=1.2) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(panel.background = element_blank(), # remove plot background
        panel.grid.major = element_blank(), # remove grid
        panel.grid.minor = element_blank(), # remove grid
        plot.margin = ggplot2::margin(10, 20, 10, 10), # set plot margins - top, right, bottom, left
        legend.title = element_blank(), # remove legend title
        legend.position = c(.2, .4), # place legend inside plot boundaries
        legend.key=element_blank(), # remove legend background
        legend.justification = c("right", "top"),
        legend.key.size = unit(1, "cm"), # set legend size / spacing (relies on function above)
        legend.box.just = "right",
        panel.border = element_rect(colour = "black", fill=NA, size=2),axis.text.y = normal.12.text,axis.title.x = bold.12.text,
        axis.title.y = bold.12.text,
        axis.text.x=element_text(color = "black", size=11, angle=90, vjust=0.5, hjust=0.5)) # adjust posiiton and size of x axis tick labels
# add + coord_flip() if needed

pdf("Taxon_accuracy_by_model.pdf")
taxon_line
dev.off()


# add class numbers per taxon
# for accuracies use a combined accuracy

training_class_numbers <- read_csv("training_class_numbers.csv")
ByCladeTestAccuracy2 <- ByCladeTestAccuracy
ByCladeTestAccuracy2$Comb_Accuracy <- (ByCladeTestAccuracy2$RF + ByCladeTestAccuracy2$MDA + ByCladeTestAccuracy2$C5.0) / 3
ByCladeTestAccuracy2$z <- training_class_numbers$z
ByCladeTestAccuracy2$cases <- training_class_numbers$Cases

write.csv(ByCladeTestAccuracy2,"Clade_Comb_Accuracy.csv")






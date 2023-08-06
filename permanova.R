

# NPMANOVA / PERMANOVA
# https://f-santos.gitlab.io/2020-05-07-npmanova.html
# Don't use MDACV for this, use either PCA or log vars
# adonis and pairwise adonis crreate a distance matrix from the input data
# can also use a pre-esisting distance matrix, just do not reference the method or sim.method
# https://jichapman.files.wordpress.com/2016/05/tutorial-13.pdf

# FIRST FROM LOADED DATA

tsne_train <- read_csv("C:/Users/swill/OneDrive - Natural History Museum/Manirpatora Manuscript/MultivariateTests/train_tsne_3d.csv")
pairwise_permanova <- pairwise.adonis(tsne_train[,1:3],tsne_train$Class, sim.method = "mahalanobis", p.adjust.m = "bonferroni", perm = 10000)


# Also doibg pairwise with RVAideMemoire

Y <- prediction[,c("CBL","CBW","CH","ADM","PDM")]
Y_pca <- prcomp(prediction[,c(4:8)])
Y_pca <- as.data.frame(Y_pca$x)

permanovoa_overall <- adonis(Y ~ prediction$class2, method="mahalanobis", permutations=10000)


# now pairwise

library(pairwiseAdonis)
pairwise_permanova <- pairwise.adonis(Y,prediction$class2, sim.method = "mahalanobis", p.adjust.m = "bonferroni", perm = 10000)

library(RVAideMemoire)

pairwise_permanova_RVA <-  pairwise.perm.manova(vegdist(Y,"mahalanobis"),prediction$class,nperm=10000,test="Hotelling-Lawley",p.method = "bonferroni",F = TRUE,R2 = TRUE)

# do the same on t-SNE data. Structure tsne V1, V2, GroupID
pairwise_permanova_RVA_SNE <-  pairwise.perm.manova(vegdist(train_tsne[,-3],"mahalanobis"),train_tsne$GroupID,nperm=10000,test="Hotelling-Lawley",p.method = "bonferroni",F = TRUE,R2 = TRUE)



# to plot the pairwise p values from the RVA matirx

library(reshape2)
library(sjmisc)
library(dplyr)

# make code easier and repalce row / cols with 3 letters
RVA <- pairwise_permanova_RVA$p.value
RVA_Col <- colnames(RVA)
RVA_Row <- row.names(RVA)
r <- c("ALL","CAR","DMA","DMB","DMC","MEG","MET","NET","NEV","NOA","NAC","NAN","NMM","NTT","PRO","SPI","THZ","TRO","TMA","TMB","TMC","TPM")
r2 <- c("DM2","DM3","OTH","THZ","TRO")
c2 <- c("DM1","DM2","DM3","OTH","THZ")
c <- c("ABE","ALL","CAR","DMA","DMB","DMC","MEG","MET","NET","NEV","NOA","NAC","NAN","NMM","NTT","PRO","SPI","THZ","TRO","TMA","TMB","TMC")
row.names(RVA) <- r
colnames(RVA) <- c

RVA_BAK <- RVA



RVA[is.na(RVA)] <- 0 # repalce NA with 0
longData <- melt(RVA)
longData<-longData[longData$value!=0,]
longData$pointsize <- rec(longData$value, rec="0:0.01 = 1; 0.01:0.05=2; 0.05:0.1 = 3; 0.1:1=4")
longData$pointsize_rev <- rec(longData$value, rec="0:0.01 = 4; 0.01:0.05=3; 0.05:0.1 = 2; 0.1:1=1")
longData$rec_Pvalue <- longData$pointsize

grad <- c("#313695","#4575b4","#74add1","#abd9e9","#e0f3f8","#ffffbf","#fee090","#fdae61","#f46d43","#d73027","#a50026")

pairwise_plot2 <- longData %>%
  ggplot(aes(x=Var2, y=Var1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
#  scale_fill_gradientn(colors = rev(hcl.colors(20, "RdYlGn"))) +
  scale_fill_gradientn(colors = grad) +
  coord_fixed() +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20)) +
#  geom_text((aes(label=sprintf("%0.3f",  value))), color = "white", size = 1.8) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Taxon") +
  ylab("Taxon") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold")) +
  theme(axis.text.x=element_text(angle=60, hjust=1))



pdf("pairwise_plot.pdf")
pairwise_plot2
dev.off()










pairwise_plot2 <- longData %>%
  ggplot(aes(x=Var2, y=Var1, color=value)) +
  geom_point(data = . %>% filter(value < 0.01),size=4,color="#1A9850") +
  geom_point(data = . %>% filter(between(value,0.01,0.05)),size=4,color="#A6D96A") +
  geom_point(data = . %>% filter(between(value,0.05,0.1)),size=4,color="#FDAE61") +
  geom_point(data = . %>% filter(between(value,0.1,1)),size=4,color="#D73027") +
#  geom_text(aes(label=sprintf("%0.3f",  value))) +
  guides(size = "none", alpha = "none") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "bottom") +
  xlab("Taxon") +
  ylab("Taxon")
  
  
pdf("pairwise_plot.pdf")
pairwise_plot2
dev.off()
 




# to do the same on pca of RF prox matrix
# assume training = training data partition and testing = unknown data

library(randomForest)

# create the rf model

f <- as.formula(
  paste("GroupID", 
        paste(predictor.vars, collapse = " + "), 
        sep = " ~ "))

rf_proximitymodel <- randomForest(f,training,inbag=TRUE, importance=TRUE, proximity=TRUE,ntree=2000,nodesize=4,mtry=3,priors=prior)

devtools::install_github("zmjones/edarf", subdir = "pkg")
library(edarf)

prox <- extract_proximity(rf_proximitymodel)
pca = prcomp(prox, scale = TRUE)
Y_pca <- as.data.frame(pca$x)
Y_pca <- Y_pca[,1:10]
permanovoa_overall <- adonis(Y_pca ~ training$GroupID, method="mahalanobis", permutations=10000)

library(pairwiseAdonis)
pairwise_permanova <- pairwise.adonis(Y_pca,training$GroupID, sim.method = "mahalanobis", p.adjust.m = "bonferroni", perm = 10000)

# to extract prox from unknown data
unknown_prox <- extract_proximity(rf_proximitymodel,unknown)
pca_unk <- prcomp(unknown_prox, scale=TRUE)
Y_pca_unk <- as.data.frame(pca_unk$x)
Y_pca_unk <- Y_pca_unk[,1:15]
permanovoa_overall <- adonis(Y_pca_unk ~ unknown$GroupID, method="mahalanobis", permutations=10000)
pairwise_permanova <- pairwise.adonis(Y_pca_unk,unknown$GroupID, sim.method = "mahalanobis", p.adjust.m = "bonferroni", perm = 10000)





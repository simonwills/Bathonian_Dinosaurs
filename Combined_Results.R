# combine data

# combine model results for unknown data
# create alluvial plots


if (!require("pacman")) install.packages("pacman")
pacman::p_load(jsonlite,writexl,easyalluvial,dplyr,RColorBrewer,ggalluvial)



#this bit grabs the dropbox path on local machine
file_name<-list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
if (length(file_name)==0){
  file_name<-list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}

file_content<-fromJSON(txt=file_name)$personal
dropbox.path<-file_content$path

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# C5 RESULTS
C5_completedata_results <- readRDS(paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/C5/SmallTeethForClassification_C5_UnknownModelResults.rds"))
C5_completecases_results <- readRDS(paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/C5/SmallTeethForClassification_CompleteCasesC5_UnknownModelResults.rds"))
C5_imputeddata_results <- readRDS(paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/C5/SmallTeethForClassification_ImputedC5_UnknownModelResults.rds"))

# merge these to produce one data frame with NA where there is no classification for the complete case dataset
unknown.results <- merge(C5_completedata_results[c("SPECID","class")],C5_completecases_results[c("SPECID","class")],all.x = TRUE,by="SPECID")
unknown.results <- merge(unknown.results,C5_imputeddata_results[c("SPECID","class")],all.x=TRUE,by="SPECID")
colnames(unknown.results)[2:4] <- c("CompleteData","CompleteCases","ImputedData")

#calculate majority votes 
unknown.results$majorityvote <- apply(unknown.results[,2:4],1,function(x) names(which.max(table(x))))
taxa <- as.character(unique(unknown.results[,2],incomparables = FALSE, MARGIN = 1))
taxa <- c(taxa,as.character(unique(unknown.results[,3],incomparables = FALSE, MARGIN = 1)))
taxa <- c(taxa,as.character(unique(unknown.results[,4],incomparables = FALSE, MARGIN = 1)))
taxa <- unique(taxa)
tt <- t(apply(unknown.results[,2:4], 1, function(u) table(factor(u, levels=c(taxa)))))
unknown.results$majorityvote_count <- apply(tt,1,max)
unknown.results$majorityvote2 <- unknown.results$majorityvote
levels(unknown.results$majorityvote2) <- c(levels(unknown.results$majorityvote2),"Unknown") # add a factor of type "Unknown" to MV2 column
unknown.results$majorityvote2[unknown.results$majorityvote_count==1] <- "Unknown" # replace MV2 value with unknown if there is no majority vote i.e. value = 1

C5.unknown.results <- unknown.results


# RF RESULTS
RF_completecases_results <- readRDS(paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/RF/SmallTeethForClassification_CompleteCasesRF_UnknownModelResults.rds"))
RF_imputeddata_results <- readRDS(paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/RF/SmallTeethForClassification_ImputedRF_UnknownModelResults.rds"))

# merge these to produce one data frame with NA where there is no classification for the complete case dataset
unknown.results <- merge(RF_imputeddata_results[c("SPECID","class")],RF_completecases_results[c("SPECID","class")],all.x = TRUE,by="SPECID")
colnames(unknown.results)[2:3] <- c("ImputedData","CompleteCases")

#calculate majority votes 
unknown.results$majorityvote <- apply(unknown.results[,2:3],1,function(x) names(which.max(table(x))))
taxa <- as.character(unique(unknown.results[,2],incomparables = FALSE, MARGIN = 1))
taxa <- c(taxa,as.character(unique(unknown.results[,3],incomparables = FALSE, MARGIN = 1)))
taxa <- unique(taxa)
tt <- t(apply(unknown.results[,2:3], 1, function(u) table(factor(u, levels=c(taxa)))))
unknown.results$majorityvote_count <- apply(tt,1,max)
unknown.results$majorityvote2 <- unknown.results$majorityvote
levels(unknown.results$majorityvote2) <- c(levels(unknown.results$majorityvote2),"Unknown") # add a factor of type "Unknown" to MV2 column
unknown.results$majorityvote2[unknown.results$majorityvote_count==1] <- "Unknown" # replace MV2 value with unknown if there is no majority vote i.e. value = 1

RF.unknown.results <- unknown.results

# MDA RESULTS
MDA_completecases_results <- readRDS(paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/MDA/SmallTeethForClassification_CompleteCasesMDA_UnknownModelResults.rds"))
MDA_imputeddata_results <- readRDS(paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/MDA/SmallTeethForClassification_ImputedMDA_UnknownModelResults.rds"))

# merge these to produce one data frame with NA where there is no classification for the complete case dataset
unknown.results <- merge(MDA_imputeddata_results[c("SPECID","class")],MDA_completecases_results[c("SPECID","class")],all.x = TRUE,by="SPECID")
colnames(unknown.results)[2:3] <- c("ImputedData","CompleteCases")

#calculate majority votes 
unknown.results$majorityvote <- apply(unknown.results[,2:3],1,function(x) names(which.max(table(x))))
taxa <- as.character(unique(unknown.results[,2],incomparables = FALSE, MARGIN = 1))
taxa <- c(taxa,as.character(unique(unknown.results[,3],incomparables = FALSE, MARGIN = 1)))
taxa <- unique(taxa)
tt <- t(apply(unknown.results[,2:3], 1, function(u) table(factor(u, levels=c(taxa)))))
unknown.results$majorityvote_count <- apply(tt,1,max)
unknown.results$majorityvote2 <- unknown.results$majorityvote
levels(unknown.results$majorityvote2) <- c(levels(unknown.results$majorityvote2),"Unknown") # add a factor of type "Unknown" to MV2 column
unknown.results$majorityvote2[unknown.results$majorityvote_count==1] <- "Unknown" # replace MV2 value with unknown if there is no majority vote i.e. value = 1

MDA.unknown.results <- unknown.results

# COMBINED MACHINE LEARNING RESULTS

# FIRST THE ENTIRE DATA i.e. INCL NA
MachineLearningCombinedResults  <- merge(C5.unknown.results[c("SPECID","majorityvote2")],RF.unknown.results[c("SPECID","majorityvote2")],all.x = TRUE,by="SPECID")
MachineLearningCombinedResults <- merge(MachineLearningCombinedResults,MDA.unknown.results[c("SPECID","majorityvote2")],all.x = TRUE,by="SPECID")
colnames(MachineLearningCombinedResults)[2:4] <- c("C5.0","RandomForest","MDA")

MachineLearningCombinedResults$majorityvote <- apply(MachineLearningCombinedResults[,2:4],1,function(x) names(which.max(table(x))))
taxa <- as.character(unique(MachineLearningCombinedResults[,2],incomparables = FALSE, MARGIN = 1))
taxa <- c(taxa,as.character(unique(MachineLearningCombinedResults[,3],incomparables = FALSE, MARGIN = 1)))
taxa <- c(taxa,as.character(unique(MachineLearningCombinedResults[,4],incomparables = FALSE, MARGIN = 1)))
taxa <- unique(taxa)
tt <- t(apply(MachineLearningCombinedResults[,2:4], 1, function(u) table(factor(u, levels=c(taxa)))))
MachineLearningCombinedResults$majorityvote_count <- apply(tt,1,max)
MachineLearningCombinedResults$majorityvote2 <- MachineLearningCombinedResults$majorityvote
levels(MachineLearningCombinedResults$majorityvote2) <- c(levels(MachineLearningCombinedResults$majorityvote2),"Unknown") # add a factor of type "Unknown" to MV2 column
MachineLearningCombinedResults$majorityvote2[MachineLearningCombinedResults$majorityvote_count==1] <- "Unknown" # replace MV2 value with unknown if there is no majority vote i.e. value = 1
MachineLearningCombinedResults <- subset(MachineLearningCombinedResults,select= -c(majorityvote))
colnames(MachineLearningCombinedResults)[5:6] <- c("VoteCount","MajorityVote")

write_xlsx(MachineLearningCombinedResults,paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/MachineLearningResults.xlsx"))

# Alluvial Plot


MachineLearningCombinedResults <- as.data.frame(unclass(MachineLearningCombinedResults), stringsAsFactors = TRUE)

flow.order <- as.character(unique(MachineLearningCombinedResults[,2],incomparables = FALSE, MARGIN = 1))
flow.order <- c(flow.order,as.character(unique(MachineLearningCombinedResults[,3],incomparables = FALSE, MARGIN = 1)))
flow.order <- c(flow.order,as.character(unique(MachineLearningCombinedResults[,4],incomparables = FALSE, MARGIN = 1)))
flow.order <- unique(c(flow.order,as.character(unique(MachineLearningCombinedResults[,6],incomparables = FALSE, MARGIN = 1))))

flow.colour <- c("red","blue","blue4","grey","gold4","gold3","beige")

bold.12.text <- element_text(face = "bold", size = 10)
normal.12.text <- element_text(size = 10)

alluvial_plot <-  alluvial_wide(dplyr::select(MachineLearningCombinedResults, C5.0, RandomForest, MDA , MajorityVote), fill_by = 'first_variable', 
                                order_levels = flow.order, 
                                stratum_labels = FALSE, 
                                auto_rotate_xlabs = FALSE, 
                                col_vector_flow = flow.colour, 
                                col_vector_value = flow.colour, 
                                verbose=FALSE) +
  geom_flow(alpha=0.5) +
  scale_x_discrete(expand = c(0, 0),"Classifier",labels = c("C5.0", "Random Forest", "Mixture\ndiscriminant analysis","Majority\nvote"))+
  theme(panel.background = element_blank(),axis.text.x = normal.12.text,axis.text.y = normal.12.text,axis.title.x = bold.12.text,axis.title.y = bold.12.text) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_text(stat = "stratum", size = 3) 

tiff("Alluvial_ML_Results.tiff", units="cm", width=29, height=21, res=600)
alluvial_plot
dev.off()


# now the complete cases ONLY

MachineLearningComplteCasesResults  <- merge(C5.unknown.results[c("SPECID","CompleteCases")],RF.unknown.results[c("SPECID","CompleteCases")],by="SPECID")
MachineLearningComplteCasesResults <- merge(MachineLearningComplteCasesResults,MDA.unknown.results[c("SPECID","CompleteCases")],by="SPECID")
colnames(MachineLearningComplteCasesResults)[2:4] <- c("C5.0","RandomForest","MDA")
MachineLearningComplteCasesResults <- MachineLearningComplteCasesResults[complete.cases(MachineLearningComplteCasesResults),]

MachineLearningComplteCasesResults$majorityvote <- apply(MachineLearningComplteCasesResults[,2:4],1,function(x) names(which.max(table(x))))

taxa <- as.character(unique(MachineLearningComplteCasesResults[,2],incomparables = FALSE, MARGIN = 1))
taxa <- c(taxa,as.character(unique(MachineLearningComplteCasesResults[,3],incomparables = FALSE, MARGIN = 1)))
taxa <- c(taxa,as.character(unique(MachineLearningComplteCasesResults[,4],incomparables = FALSE, MARGIN = 1)))
taxa <- unique(taxa)
tt <- t(apply(MachineLearningComplteCasesResults[,2:4], 1, function(u) table(factor(u, levels=c(taxa)))))
MachineLearningComplteCasesResults$majorityvote_count <- apply(tt,1,max)
MachineLearningComplteCasesResults$majorityvote2 <- MachineLearningComplteCasesResults$majorityvote
levels(MachineLearningComplteCasesResults$majorityvote2) <- c(levels(MachineLearningComplteCasesResults$majorityvote2),"Unknown") # add a factor of type "Unknown" to MV2 column
MachineLearningComplteCasesResults$majorityvote2[MachineLearningComplteCasesResults$majorityvote_count==1] <- "Unknown" # replace MV2 value with unknown if there is no majority vote i.e. value = 1
MachineLearningComplteCasesResults <- subset(MachineLearningComplteCasesResults,select= -c(majorityvote))
colnames(MachineLearningComplteCasesResults)[5:6] <- c("VoteCount","MajorityVote")
write_xlsx(MachineLearningComplteCasesResults,paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/MachineLearningCompleteCasesResults.xlsx"))


MachineLearningComplteCasesResults <- as.data.frame(unclass(MachineLearningComplteCasesResults), stringsAsFactors = TRUE)

flow.order <- as.character(unique(MachineLearningComplteCasesResults[,2],incomparables = FALSE, MARGIN = 1))
flow.order <- c(flow.order,as.character(unique(MachineLearningComplteCasesResults[,3],incomparables = FALSE, MARGIN = 1)))
flow.order <- c(flow.order,as.character(unique(MachineLearningComplteCasesResults[,4],incomparables = FALSE, MARGIN = 1)))
flow.order <- unique(c(flow.order,as.character(unique(MachineLearningComplteCasesResults[,6],incomparables = FALSE, MARGIN = 1))))

flow.colour <- c("red","blue","blue4","grey","gold4","gold3","beige")

bold.12.text <- element_text(face = "bold", size = 10)
normal.12.text <- element_text(size = 10)

alluvial_plot_CompleteCases <-  alluvial_wide(dplyr::select(MachineLearningComplteCasesResults, C5.0, RandomForest, MDA , MajorityVote), fill_by = 'first_variable', 
                                order_levels = flow.order, 
                                stratum_labels = FALSE, 
                                auto_rotate_xlabs = FALSE, 
                                col_vector_flow = flow.colour, 
                                col_vector_value = flow.colour, 
                                verbose=FALSE) +
  geom_flow(alpha=0.5) +
  scale_x_discrete(expand = c(0, 0),"Classifier",labels = c("C5.0", "Random Forest", "Mixture\ndiscriminant analysis","Majority\nvote"))+
  theme(panel.background = element_blank(),axis.text.x = normal.12.text,axis.text.y = normal.12.text,axis.title.x = bold.12.text,axis.title.y = bold.12.text) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_text(stat = "stratum", size = 3) 

tiff("Alluvial_ML_CompleteCasesResults.tiff", units="cm", width=29, height=21, res=600)
alluvial_plot_CompleteCases
dev.off()


#######################################################################
#######################################################################
# now we do the same for the complete cases training model data results

# Training data results
C5_training_completedata_results <- readRDS(paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/C5/CombinedDataForModelsAndClassification20212707_CompleteCasesC5_TrainPartitionModelResults.rds"))
RF_training_completedata_results <- readRDS(paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/RF/CombinedDataForModelsAndClassification20212707_CompleteCasesRF_TrainPartitionModelResults.rds"))
MDA_training_completedata_results <- readRDS(paste0(dropbox.path,"/PhD/Manuscripts/15. The early evolution of manirptoran theropods/Results/SmallTeethUnknowns/MDA/CombinedDataForModelsAndClassification20212707_CompleteCasesMDA_TrainPartitionModelResults.rds"))

C5_training_completedata_results <- C5_training_completedata_results[c("GroupID","SPECID","class")]
RF_training_completedata_results <- RF_training_completedata_results[c("GroupID","SPECID","class")]
MDA_training_completedata_results <- MDA_training_completedata_results[c("GroupID","SPECID","class")]

# merge these to produce one data frame with NA where there is no classification for the complete case dataset
training.results <- C5_training_completedata_results
training.results$RF <- RF_training_completedata_results$class
training.results$MDA <- MDA_training_completedata_results$class
colnames(training.results)[1:3] <- c("Original_Class","SPECID","C5.0")
training.results$C5.0 <- as.character(training.results$C5.0)
training.results$RF <- as.character(training.results$RF)
training.results$MDA <- as.character(training.results$MDA)

#calculate majority votes 
training.results$majorityvote <- apply(training.results[,3:5],1,function(x) names(which.max(table(x))))
taxa <- as.character(unique(training.results[,3],incomparables = FALSE, MARGIN = 1))
taxa <- c(taxa,as.character(unique(training.results[,4],incomparables = FALSE, MARGIN = 1)))
taxa <- c(taxa,as.character(unique(training.results[,5],incomparables = FALSE, MARGIN = 1)))
taxa <- unique(taxa)
tt <- t(apply(training.results[,3:5], 1, function(u) table(factor(u, levels=c(taxa)))))
training.results$majorityvote_count <- apply(tt,1,max)
training.results$majorityvote2 <- training.results$majorityvote
levels(training.results$majorityvote2) <- c(levels(training.results$majorityvote2),"Unknown") # add a factor of type "Unknown" to MV2 column
training.results$majorityvote2[training.results$majorityvote_count==1] <- "Unknown" # replace MV2 value with unknown if there is no majority vote i.e. value = 1



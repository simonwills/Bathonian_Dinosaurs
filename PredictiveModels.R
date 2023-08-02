# Version created using R version 3.6.0 running on R Studio 1.3.1056
# tested on R version 4.0.2


#######################################################################################
# check if required libraries exist. If not then download from CRAN and then load them.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, MASS, mda,randomForest,C50)


######################################################################
# Read the data file and choose variables
# the routine assumes a .CSV file and needs to contain (at least) the following variables / columns:
# columns must have a name as the first row.
# specimenID, grouping variable, morphometric variables.
######################################################################

infile <- read.csv(filename<-file.choose())

################################################################################
#The next section selects various pieces of info from the data file
#Choices are made via a graphical pop-up
#If you cannot use graphical pop-ups e.g. running R studio via a web browser
# then change graphics = TRUE to graphics = FALSE
################################################################################

# choose the grouping column
group.col <- colnames(infile)
group.select <- select.list(group.col, multiple = FALSE,
                            title = "Choose grouping column", graphics = TRUE)
# choose groups to analyze
taxa.list <- as.character(unique(infile[,group.select]))
taxa.select <- select.list(taxa.list, multiple = TRUE, title = "Choose groups to analyse", graphics = TRUE)

# choose the morphometric data
predictor.cols <- colnames(infile)
predictor.vars <- select.list(predictor.cols, multiple = TRUE,
                              title = "Choose morphometric variables", graphics = TRUE)

# choose the specimen ID column
specID.col <- colnames(infile)
specID.select <- select.list(specID.col, multiple = FALSE,
                             title = "Choose Specimen ID column", graphics = TRUE)

#give some consistency in the names
colnames(infile)[colnames(infile) == group.select] <- 'GroupID'
colnames(infile)[colnames(infile) == specID.select] <- 'SPECID'

# this just ensures that all the factor levels are acceptable R names
infile$GroupID <- make.names(infile$GroupID) 
# now make sure they are factors .... 
infile$GroupID <- factor(infile$GroupID) 

# this section checks for missing data and drops those cases by default
# simply comment out the next three lines if not needed.
cat(c("Dropping ",sum(!complete.cases(infile[,predictor.vars])),"cases with missing data"))
infile_na <- infile[!complete.cases(infile[,predictor.vars]), ]
infile <- infile[complete.cases(infile[,predictor.vars]), ]

# now drop any groups where there are less group members than morphometric variables
dropped.taxa <- summary(infile$GroupID)
dropped.taxa <- dropped.taxa[dropped.taxa <= length(predictor.vars)]
cat(c("Dropping ", length(dropped.taxa), "groups as the group membership is less than the number of morphometric variables"))
print("Dropping the following groups ....")
print(dropped.taxa)
infile = infile[infile$GroupID %in% names(table(infile$GroupID)) [table(infile$GroupID) > length(predictor.vars)],]


# log + 1 the data if needed
if (select.list(choices=c("YES","NO"), graphics=TRUE, title="Log predictor variables?", preselect="NO") == "YES") {
  cat("Doing Log transform of variables")
  infile[,predictor.vars] <- log(infile[,predictor.vars]+1)
}

# scale and center the data if needed
if (select.list(choices=c("YES","NO"), graphics=TRUE, title="Scale and centre predictor variables?", preselect="NO") == "YES") {
  cat("Scaling & centering morphometric variables")
  infile[,predictor.vars] <- scale(infile[,predictor.vars],center=TRUE, scale=TRUE)
}


set.seed(3456)
trainIndex <- createDataPartition(infile$GroupID, p = .8, 
                                  list = FALSE, 
                                  times = 1)
training <- infile[ trainIndex,]
training$GroupID <- factor(training$GroupID)
testing  <- infile[-trainIndex,]


# drop columns
training <- training[c("GroupID","SPECID",predictor.vars)]
testing <- testing[c("GroupID","SPECID",predictor.vars)]


# now pull out the known cases specimen ID
training.id <- training[(c("SPECID","GroupID"))]
testing.id <- testing[(c("SPECID","GroupID"))]

#then drop SpecimenID column
training <- training[c("GroupID",predictor.vars)]
testing <- testing[c("GroupID",predictor.vars)]


# set up training control and priors

ctrl <- trainControl(classProbs = TRUE,method="repeatedcv",number=10, repeats=10,savePredictions="final")
prior = rep(1/nlevels(training$Group),nlevels(training$Group))


if (select.list(choices=c("YES","NO"), graphics=TRUE, title="run C.50 model?", preselect="NO") == "YES") {
# C50 model

c50Grid <- expand.grid(.trials = c(1:9, (1:10)*10),
                       .model = c("tree", "rules"),
                       .winnow = c(TRUE, FALSE))

c5Fit1 <- train(x=training[,predictor.vars],
                y=training$GroupID,
                method = "C5.0",
                tuneGrid = c50Grid,
                trControl = ctrl,
                metric = "Accuracy", 
                importance=TRUE, 
                preProc = c("center", "scale"),
                parms=list(priors = prior)) 

# Fit model on partitioned data
training.c5.0.optimized.class <- predict(c5Fit1,training)
training.c5.0.optimized.prob<- predict(c5Fit1,training,type="prob")
testing.c5.0.optimized.class <- predict(c5Fit1,testing)
testing.c5.0.optimized.prob <- predict(c5Fit1,testing,type="prob")

training.c5.0.optimized.results <- training
training.c5.0.optimized.results$class <- training.c5.0.optimized.class
training.c5.0.optimized.results$prob <- training.c5.0.optimized.prob
testing.c5.0.optimized.results <- testing
testing.c5.0.optimized.results$class <- testing.c5.0.optimized.class
testing.c5.0.optimized.results$prob <- testing.c5.0.optimized.prob

training.c5.0.optimized.matrix <- confusionMatrix(data=training.c5.0.optimized.results$class, reference=training.c5.0.optimized.results$GroupID)
testing.c5.0.optimized.matrix <- confusionMatrix(data=testing.c5.0.optimized.results$class, reference=testing.c5.0.optimized.results$GroupID)

# export confusion matrix results to csv files
write.csv(training.c5.0.optimized.matrix$table, "C50.training_CM.csv")
write.csv(training.c5.0.optimized.matrix$overall, "C50.training_overall.csv")
write.csv(training.c5.0.optimized.matrix$byClass, "C50.training_byclass.csv")

write.csv(testing.c5.0.optimized.matrix$table, "C50.testing_CM.csv")
write.csv(testing.c5.0.optimized.matrix$overall, "c50.testing_overall.csv")
write.csv(testing.c5.0.optimized.matrix$byClass, "c50.testing_byclass.csv")

# export test data with predictions and specimen ID to csv

write.csv(merge(testing.id, testing.c5.0.optimized.results, by=0, all=TRUE),"C50_test_predictions.csv")

}

if (select.list(choices=c("YES","NO"), graphics=TRUE, title="run MDA model?", preselect="NO") == "YES") {
# MDA

mdaFit1 <- train(x=training[,predictor.vars],y=training$GroupID,method="mda",trControl = ctrl,preProc=c("center","scale"),tuneGrid = expand.grid(.subclasses=1:8),parms=list(priors = prior))


training.mda.optimized<- predict(mdaFit1,training)
training.mda.optimized.prob<- predict(mdaFit1,training,type="prob")
testing.mda.optimized <- predict(mdaFit1,testing)
testing.mda.optimized.prob <- predict(mdaFit1,testing,type="prob")

training.mda.optimized.results <- training
training.mda.optimized.results$class <- training.mda.optimized
training.mda.optimized.results$prob <- training.mda.optimized.prob

testing.mda.optimized.results <- testing
testing.mda.optimized.results$class <- testing.mda.optimized
testing.mda.optimized.results$prob <- testing.mda.optimized.prob

training.mda.optimized.matrix <- confusionMatrix(data=training.mda.optimized.results$class, reference=training.mda.optimized.results$GroupID)
testing.mda.optimized.matrix <- confusionMatrix(data=testing.mda.optimized.results$class, reference=testing.mda.optimized.results$GroupID)

# export results to csv files
write.csv(training.mda.optimized.matrix$table, "mda.training_CM.csv")
write.csv(training.mda.optimized.matrix$overall, "mda.training_overall.csv")
write.csv(training.mda.optimized.matrix$byClass, "mda.training_byclass.csv")

write.csv(testing.mda.optimized.matrix$table, "mda.testing_CM.csv")
write.csv(testing.mda.optimized.matrix$overall, "mda.testing_overall.csv")
write.csv(testing.mda.optimized.matrix$byClass, "mda.testing_byclass.csv")

write.csv(merge(testing.id, testing.mda.optimized.results, by=0, all=TRUE),"MDA_test_predictions.csv")

}


if (select.list(choices=c("YES","NO"), graphics=TRUE, title="run Random Forests model?", preselect="NO") == "YES") {
# Random Forests
RFFit1 <- train(x=training[,predictor.vars],y=training$GroupID,method="rf",trControl = ctrl,importance=TRUE, ntree=2000,nodesize=4)
training.RF.optimized<- predict(RFFit1,training)
training.RF.optimized.prob<- predict(RFFit1,training,type="prob")
testing.RF.optimized <- predict(RFFit1,testing)
testing.RF.optimized.prob <- predict(RFFit1,testing,type="prob")

training.RF.optimized.results <- training
training.RF.optimized.results$class <- training.RF.optimized
training.RF.optimized.results$prob <- training.RF.optimized.prob

testing.RF.optimized.results <- testing
testing.RF.optimized.results$class <- testing.RF.optimized
testing.RF.optimized.results$prob <- testing.RF.optimized.prob

training.RF.optimized.matrix <- confusionMatrix(data=training.RF.optimized.results$class, reference=training.RF.optimized.results$GroupID)
testing.RF.optimized.matrix <- confusionMatrix(data=testing.RF.optimized.results$class, reference=testing.RF.optimized.results$GroupID)

# export results to csv files
write.csv(training.RF.optimized.matrix$table, "RF.training_CM.csv")
write.csv(training.RF.optimized.matrix$overall, "RF.training_overall.csv")
write.csv(training.RF.optimized.matrix$byClass, "RF.training_byclass.csv")

write.csv(testing.RF.optimized.matrix$table, "RF.testing_CM.csv")
write.csv(testing.RF.optimized.matrix$overall, "RF.testing_overall.csv")
write.csv(testing.RF.optimized.matrix$byClass, "RF.testing_byclass.csv")

write.csv(merge(testing.id, testing.RF.optimized.results, by=0, all=TRUE),"RF_test_predictions.csv")

}

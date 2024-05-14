#=======================================================================

# Rattle is Copyright (c) 2006-2021 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2024-05-14 13:19:57 x86_64-w64-mingw32 

# Rattle version 5.5.1 user 'eddie'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2024-05-14 13:20:33 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/eddie/Desktop/Cardio_Train_USE_FINAL.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2024-05-14 13:20:35 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=69185 train=48430 validate=10378 test=10377

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("age", "gender", "age_year", "age_bins",
                   "Gender_BIN", "height", "weight", "ap_hi",
                   "ap_lo_adj", "ap_lo", "cholesterol", "gluc",
                   "smoke", "alco", "active")

crs$numeric   <- c("age", "gender", "age_year", "Gender_BIN",
                   "height", "weight", "ap_hi", "ap_lo_adj", "ap_lo",
                   "cholesterol", "gluc", "smoke", "alco", "active")

crs$categoric <- "age_bins"

crs$target    <- "cardio"
crs$risk      <- NULL
crs$ident     <- "id"
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2024-05-14 13:20:44 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2024-05-14 13:21:08 x86_64-w64-mingw32 

# The 'gplots' package provides the 'barplot2' function.

library(gplots, quietly=TRUE)

#=======================================================================
# Rattle timestamp: 2024-05-14 13:21:08 x86_64-w64-mingw32 

# Bar Plot 

# Generate the summary data for plotting.

ds <- rbind(summary(na.omit(crs$dataset[crs$train,]$age_bins)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$cardio=="0",]$age_bins)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$cardio=="1",]$age_bins)))

# Sort the entries.

ord <- order(ds[1,], decreasing=TRUE)

# Plot the data.

bp <-  barplot2(ds[,ord], beside=TRUE, ylab="Frequency", xlab="age_bins", ylim=c(0, 29520), col=colorspace::rainbow_hcl(3))

# Add the actual frequencies.

text(bp, ds[,ord]+984, ds[,ord])

# Add a legend to the plot.

legend("topright", bty="n", c("All","0","1"),  fill=colorspace::rainbow_hcl(3))

# Add a title to the plot.

title(main="Distribution of age_bins (sample)\nby cardio",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:24:52 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for height

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(height) %>%
  ggplot2::ggplot(ggplot2::aes(x=height)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("height\n\nRattle 2024-May-14 13:24:52 eddie") +
  ggplot2::ggtitle("Distribution of height (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for weight

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(weight) %>%
  ggplot2::ggplot(ggplot2::aes(x=weight)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("weight\n\nRattle 2024-May-14 13:24:52 eddie") +
  ggplot2::ggtitle("Distribution of weight (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2024-05-14 13:25:19 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'Gender_BIN'.

ds <- rbind(data.frame(dat=crs$dataset[crs$train,][,"Gender_BIN"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="Gender_BIN", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of Gender_BIN (sample)",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:25:40 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for age_year

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(age_bins=as.factor(age_bins)) %>%
  ggplot2::ggplot(ggplot2::aes(y=age_year)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=age_bins, fill=age_bins), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=age_bins), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("age_bins\n\nRattle 2024-May-14 13:25:40 eddie") +
  ggplot2::ggtitle("Distribution of age_year (sample)\nby age_bins") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2024-05-14 13:26:17 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for height

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(age_bins=as.factor(age_bins)) %>%
  ggplot2::ggplot(ggplot2::aes(y=height)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=age_bins, fill=age_bins), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=age_bins), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("age_bins\n\nRattle 2024-May-14 13:26:17 eddie") +
  ggplot2::ggtitle("Distribution of height (sample)\nby age_bins") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for weight

# Generate a box plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(age_bins=as.factor(age_bins)) %>%
  ggplot2::ggplot(ggplot2::aes(y=weight)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=age_bins, fill=age_bins), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=age_bins), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("age_bins\n\nRattle 2024-May-14 13:26:17 eddie") +
  ggplot2::ggtitle("Distribution of weight (sample)\nby age_bins") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2024-05-14 13:26:42 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for ap_hi

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(age_bins=as.factor(age_bins)) %>%
  ggplot2::ggplot(ggplot2::aes(y=ap_hi)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=age_bins, fill=age_bins), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=age_bins), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("age_bins\n\nRattle 2024-May-14 13:26:42 eddie") +
  ggplot2::ggtitle("Distribution of ap_hi (sample)\nby age_bins") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for ap_lo_adj

# Generate a box plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(age_bins=as.factor(age_bins)) %>%
  ggplot2::ggplot(ggplot2::aes(y=ap_lo_adj)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=age_bins, fill=age_bins), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=age_bins), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("age_bins\n\nRattle 2024-May-14 13:26:42 eddie") +
  ggplot2::ggtitle("Distribution of ap_lo_adj (sample)\nby age_bins") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2024-05-14 13:26:51 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for ap_hi

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=ap_hi)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:26:51 eddie") +
  ggplot2::ggtitle("Distribution of ap_hi (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for ap_lo_adj

# Generate a box plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=ap_lo_adj)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:26:51 eddie") +
  ggplot2::ggtitle("Distribution of ap_lo_adj (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2024-05-14 13:26:59 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for age_year

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=age_year)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:26:59 eddie") +
  ggplot2::ggtitle("Distribution of age_year (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#=======================================================================
# Rattle timestamp: 2024-05-14 13:37:30 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=69185 train=48430 validate=10378 test=10377

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("gender", "age_year", "Gender_BIN", "height",
                   "weight", "ap_hi", "ap_lo_adj", "cholesterol",
                   "gluc", "smoke", "alco", "active")

crs$numeric   <- c("gender", "age_year", "Gender_BIN", "height",
                   "weight", "ap_hi", "ap_lo_adj", "cholesterol",
                   "gluc", "smoke", "alco", "active")

crs$categoric <- NULL

crs$target    <- "cardio"
crs$risk      <- NULL
crs$ident     <- "id"
crs$ignore    <- c("age", "age_bins", "ap_lo")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2024-05-14 13:37:34 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(cardio) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    size=0, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.55 secs

#=======================================================================
# Rattle timestamp: 2024-05-14 13:38:01 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:38:23 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(cardio) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    size=1, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.28 secs

#=======================================================================
# Rattle timestamp: 2024-05-14 13:38:31 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:38:52 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on Cardio_Train_USE_FINAL.csv [validate].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net Cardio_Train_USE_FINAL.csv [validate] cardio")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2024-05-14 13:40:06 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=69185 train=48430 validate=10378 test=10377

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("gender", "age_bins", "Gender_BIN", "height",
                   "weight", "ap_hi", "ap_lo_adj", "cholesterol",
                   "gluc", "smoke", "alco", "active")

crs$numeric   <- c("gender", "Gender_BIN", "height", "weight",
                   "ap_hi", "ap_lo_adj", "cholesterol", "gluc",
                   "smoke", "alco", "active")

crs$categoric <- "age_bins"

crs$target    <- "cardio"
crs$risk      <- NULL
crs$ident     <- "id"
crs$ignore    <- c("age", "age_year", "ap_lo")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2024-05-14 13:40:13 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(cardio) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    size=0, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.63 secs

#=======================================================================
# Rattle timestamp: 2024-05-14 13:40:17 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:40:19 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on Cardio_Train_USE_FINAL.csv [validate].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net Cardio_Train_USE_FINAL.csv [validate] cardio")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2024-05-14 13:40:30 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(cardio) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    size=1, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 1.01 secs

#=======================================================================
# Rattle timestamp: 2024-05-14 13:40:33 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:40:36 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on Cardio_Train_USE_FINAL.csv [validate].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net Cardio_Train_USE_FINAL.csv [validate] cardio")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2024-05-14 13:40:55 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=69185 train=48430 validate=10378 test=10377

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("gender", "age_year", "age_bins", "Gender_BIN",
                   "height", "weight", "ap_hi", "ap_lo_adj",
                   "cholesterol", "gluc", "smoke", "alco", "active")

crs$numeric   <- c("gender", "age_year", "Gender_BIN", "height",
                   "weight", "ap_hi", "ap_lo_adj", "cholesterol",
                   "gluc", "smoke", "alco", "active")

crs$categoric <- "age_bins"

crs$target    <- "cardio"
crs$risk      <- NULL
crs$ident     <- "id"
crs$ignore    <- c("age", "ap_lo")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2024-05-14 13:41:07 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(cardio) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    size=0, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.73 secs

#=======================================================================
# Rattle timestamp: 2024-05-14 13:41:09 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:41:10 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on Cardio_Train_USE_FINAL.csv [validate].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net Cardio_Train_USE_FINAL.csv [validate] cardio")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2024-05-14 13:41:15 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(cardio) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    size=1, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.14 secs

#=======================================================================
# Rattle timestamp: 2024-05-14 13:41:20 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:41:23 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on Cardio_Train_USE_FINAL.csv [validate].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net Cardio_Train_USE_FINAL.csv [validate] cardio")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2024-05-14 13:42:02 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=69185 train=48430 validate=10378 test=10377

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("gender", "age_year", "age_bins", "Gender_BIN",
                   "height", "weight", "ap_hi", "ap_lo_adj",
                   "cholesterol", "gluc", "alco", "active")

crs$numeric   <- c("gender", "age_year", "Gender_BIN", "height",
                   "weight", "ap_hi", "ap_lo_adj", "cholesterol",
                   "gluc", "alco", "active")

crs$categoric <- "age_bins"

crs$target    <- "cardio"
crs$risk      <- NULL
crs$ident     <- "id"
crs$ignore    <- c("age", "ap_lo")
crs$weights   <- "crs$dataset$smoke"

#=======================================================================
# Rattle timestamp: 2024-05-14 13:42:12 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(cardio) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    weights=(crs$dataset$smoke)[crs$train],
    size=0, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.28 secs

#=======================================================================
# Rattle timestamp: 2024-05-14 13:42:14 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:42:39 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(cardio) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    weights=(crs$dataset$smoke)[crs$train],
    size=1, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.74 secs

#=======================================================================
# Rattle timestamp: 2024-05-14 13:42:44 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:42:54 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on Cardio_Train_USE_FINAL.csv [validate].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net Cardio_Train_USE_FINAL.csv [validate] cardio")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2024-05-14 13:43:28 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=69185 train=48430 validate=10378 test=10377

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("gender", "age_year", "age_bins", "Gender_BIN",
                   "height", "weight", "ap_hi", "ap_lo_adj",
                   "cholesterol", "gluc", "smoke", "active")

crs$numeric   <- c("gender", "age_year", "Gender_BIN", "height",
                   "weight", "ap_hi", "ap_lo_adj", "cholesterol",
                   "gluc", "smoke", "active")

crs$categoric <- "age_bins"

crs$target    <- "cardio"
crs$risk      <- NULL
crs$ident     <- "id"
crs$ignore    <- c("age", "ap_lo")
crs$weights   <- "crs$dataset$alco"

#=======================================================================
# Rattle timestamp: 2024-05-14 13:43:32 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(cardio) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    weights=(crs$dataset$alco)[crs$train],
    size=1, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.67 secs

#=======================================================================
# Rattle timestamp: 2024-05-14 13:43:35 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:43:38 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on Cardio_Train_USE_FINAL.csv [validate].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net Cardio_Train_USE_FINAL.csv [validate] cardio")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2024-05-14 13:44:19 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for age_year

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=age_year)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:44:19 eddie") +
  ggplot2::ggtitle("Distribution of age_year (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for height

# Generate a box plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=height)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:44:19 eddie") +
  ggplot2::ggtitle("Distribution of height (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for weight

# Generate a box plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=weight)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:44:19 eddie") +
  ggplot2::ggtitle("Distribution of weight (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for ap_hi

# Generate a box plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=ap_hi)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:44:19 eddie") +
  ggplot2::ggtitle("Distribution of ap_hi (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for ap_lo_adj

# Generate a box plot.

p05 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=ap_lo_adj)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:44:19 eddie") +
  ggplot2::ggtitle("Distribution of ap_lo_adj (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for cholesterol

# Generate a box plot.

p06 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=cholesterol)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:44:19 eddie") +
  ggplot2::ggtitle("Distribution of cholesterol (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for gluc

# Generate a box plot.

p07 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=gluc)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:44:19 eddie") +
  ggplot2::ggtitle("Distribution of gluc (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for smoke

# Generate a box plot.

p08 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=smoke)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:44:19 eddie") +
  ggplot2::ggtitle("Distribution of smoke (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for alco

# Generate a box plot.

p09 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=alco)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:44:20 eddie") +
  ggplot2::ggtitle("Distribution of alco (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for active

# Generate a box plot.

p10 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(cardio=as.factor(cardio)) %>%
  ggplot2::ggplot(ggplot2::aes(y=active)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=cardio, fill=cardio), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=cardio), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("cardio\n\nRattle 2024-May-14 13:44:20 eddie") +
  ggplot2::ggtitle("Distribution of active (sample)\nby cardio") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04, p05, p06, p07, p08, p09, p10)

#=======================================================================
# Rattle timestamp: 2024-05-14 13:45:06 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=69185 train=48430 validate=10378 test=10377

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("gender", "age_year", "age_bins", "Gender_BIN",
                   "height", "weight", "ap_hi", "ap_lo_adj", "gluc",
                   "smoke", "alco", "active")

crs$numeric   <- c("gender", "age_year", "Gender_BIN", "height",
                   "weight", "ap_hi", "ap_lo_adj", "gluc", "smoke",
                   "alco", "active")

crs$categoric <- "age_bins"

crs$target    <- "cardio"
crs$risk      <- NULL
crs$ident     <- "id"
crs$ignore    <- c("age", "ap_lo")
crs$weights   <- "crs$dataset$cholesterol"

#=======================================================================
# Rattle timestamp: 2024-05-14 13:45:12 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(cardio) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    weights=(crs$dataset$cholesterol)[crs$train],
    size=1, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.33 secs

#=======================================================================
# Rattle timestamp: 2024-05-14 13:45:14 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2024-05-14 13:45:17 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on Cardio_Train_USE_FINAL.csv [validate].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net Cardio_Train_USE_FINAL.csv [validate] cardio")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$cardio)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2024-05-14 13:46:27 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=69185 train=48430 validate=10378 test=10377

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

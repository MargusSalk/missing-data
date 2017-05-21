library(mice)
library(DMwR)
library(randomForest)

datMNAR <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', header = F, sep = ',')
names(datMNAR) <- c('Sex','Length','Diameter','Ht','Whole_wt','Shucked_wt','Viscera_wt','Shell_wt','Rings')
result_array <- c()

p <- 20
attribute_modified <- 'Diameter'
threshold <- 0.5 
amount_removed <- as.integer(length(which(datMNAR[,attribute_modified] > threshold))*p/100)
original <- datMNAR
numeric_methods <- c('pmm', 'norm', 'norm.nob', 'norm.boot', 'norm.predict', 'mean', 'quadratic', 'cart', 'rf', 'ri', 'sample', 'fastpmm')
for (method in numeric_methods) {
  # init percentage error array
  mape_array <- c()
  for (iteration in 1:10) {
    imputed_data <- NULL
    imp_MNAR <- NULL
    modified_datMNAR <- datMNAR
    # delete values
    modified_datMNAR[sample(which(modified_datMNAR[,attribute_modified] > threshold), amount_removed), attribute_modified] <- NA
    #impute values
    imputed_data <- mice(modified_datMNAR, m = 5, defaultMethod = c(method, "logreg", "polyreg", "polr"))
    imp_MNAR <- complete(imputed_data)
    # comparison
    actuals <- original[,attribute_modified][is.na(modified_datMNAR[,attribute_modified])]
    predicteds <- imp_MNAR[is.na(modified_datMNAR[,attribute_modified]), attribute_modified]
    mape <- regr.eval(actuals, predicteds)["mape"]
    # add mape to array
    names(mape) <- NULL
    mape_array <- c(mape_array, mape)
  }
  # summary of mean absolute percentage error
  mean <- summary(mape_array)['Mean']
  names(mean) <- NULL
  result_array <- c(result_array, mean)
}
# method with smallest mean error
smallest <- min(as.data.frame(result_array))
# get method
best_method <- numeric_methods[match(smallest, result_array)]
names(result_array) <- numeric_methods
result_array
best_method
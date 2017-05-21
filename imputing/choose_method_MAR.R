library(mice)
library(DMwR)
library(pan)
library(randomForest)


datMAR <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', header = F, sep = ',')
names(datMAR) <- c('Sex','Length','Diameter','Ht','Whole_wt','Shucked_wt','Viscera_wt','Shell_wt','Rings')
result_array <- c()

p <- 15 #percentage of missing values added
attribute_modified <- 'Diameter'
related_attribute <- 'Sex'
related_attribute_value <- 'M'
amount_removed <- as.integer(length(which(datMAR[,related_attribute] == related_attribute_value))*p/100)
numeric_methods <- c('pmm', 'norm', 'norm.nob', 'norm.boot', 'norm.predict', 'mean', 'quadratic', 'cart', 'rf', 'ri', 'sample', 'fastpmm')
for (method in numeric_methods) {
  # init percentage error array
  mape_array <- c()
  for (iteration in 1:10) {
    modified_datMAR <- datMAR
    modified_datMAR[sample(which(modified_datMAR[,related_attribute] == related_attribute_value), amount_removed), attribute_modified] <- NA
    imputed_data <- mice(modified_datMAR, m = 5, defaultMethod = c(method, "logreg", "polyreg", "polr"))
    imp_MAR <- complete(imputed_data)
    # comparison
    actuals <- datMAR[,attribute_modified][is.na(modified_datMAR[,attribute_modified])]
    predicteds <- imp_MAR[is.na(modified_datMAR[,attribute_modified]), attribute_modified]
    mape <- regr.eval(actuals, predicteds)["mape"]
    
    names(mape) <- NULL
    mape_array <- c(mape_array, mape)
  }
  # summary of mean absolute percentage error
  mean <- summary(mape_array)['Mean']
  names(mean) <- NULL
  result_array <- c(result_array, mean)
}

smallest <- min(as.data.frame(result_array))
# get method
best_method <- numeric_methods[match(smallest, result_array)]
names(result_array) <- numeric_methods
result_array
best_method
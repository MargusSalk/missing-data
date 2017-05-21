library(mice)

datMAR <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', header = F, sep = ',')
names(datMAR) <- c('Sex','Length','Diameter','Ht','Whole_wt','Shucked_wt','Viscera_wt','Shell_wt','Rings')
p <- 15 #percentage of missing values added
attribute_modified <- 'Diameter'
related_attribute <- 'Sex'
related_attribute_value <- 'M'
amount_removed <- as.integer(length(which(datMAR[,related_attribute] == related_attribute_value))*p/100)
datMAR[sample(which(datMAR[,related_attribute] == related_attribute_value), amount_removed), attribute_modified] <- NA

imputed_data <- mice(datMAR, m = 5, defaultMethod = c("norm.predict", "logreg", "polyreg", "polr"))
# imputed_data <- mice(datMAR, m = 5)
imp_MAR <- complete(imputed_data)

actuals <- original[,attribute_modified][is.na(datMAR[,attribute_modified])]
predicteds <- imp_MAR[is.na(datMAR[,attribute_modified]), attribute_modified]
regr.eval(actuals, predicteds)

actual_diameter <- as.data.frame(original[,attribute_modified][is.na(datMAR[,attribute_modified])])
names(actual_diameter) <- c('Diameter')
predicted_diameter <- as.data.frame(imp_MAR[is.na(datMAR[,attribute_modified]), attribute_modified])
names(predicted_diameter) <- c('Diameter')

ggplot(predicted_diameter, aes(Diameter)) + geom_density(color='blue') + geom_density(data=actual_diameter, aes(Diameter), color='red')
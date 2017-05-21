datMNAR <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', header = F, sep = ',')
names(datMNAR) <- c('Sex','Length','Diameter','Ht','Whole_wt','Shucked_wt','Viscera_wt','Shell_wt','Rings')
p <- 20 #percentage of missing values added
attribute_modified <- 'Diameter'
threshold <- 0.5 # point from which we start removing values
# the plan: remove p% of Diameter values if Diameter > threshold
amount_removed <- as.integer(length(which(datMNAR[,attribute_modified] > threshold))*p/100)
datMNAR[sample(which(datMNAR[,attribute_modified] > threshold), amount_removed), attribute_modified] <- NA

imputed_data <- mice(datMNAR, m = 5, defaultMethod = c("norm.predict", "logreg", "polyreg", "polr"))
# imputed_data <- mice(datMNAR, m = 5)
imp_MNAR <- complete(imputed_data)

actuals <- original[,attribute_modified][is.na(datMNAR[,attribute_modified])]
predicteds <- imp_MNAR[is.na(datMNAR[,attribute_modified]), attribute_modified]
regr.eval(actuals, predicteds)

actual_diameter <- as.data.frame(original[,attribute_modified][is.na(datMNAR[,attribute_modified])])
names(actual_diameter) <- c('Diameter')
predicted_diameter <- as.data.frame(imp_MNAR[is.na(datMNAR[,attribute_modified]), attribute_modified])
names(predicted_diameter) <- c('Diameter')

ggplot(predicted_diameter, aes(Diameter)) + geom_density(color='blue') + geom_density(data=actual_diameter, aes(Diameter), color='red')
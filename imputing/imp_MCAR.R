library(mice)
library(DMwR)
library(ggplot2)

datMCAR <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', header = F, sep = ',')
names(datMCAR) <- c('Sex','Length','Diameter','Ht','Whole_wt','Shucked_wt','Viscera_wt','Shell_wt','Rings')
p <- 5 #percentage of missing values added
attribute_modified <- 'Diameter'
amount_removed <- as.integer(nrow(datMCAR)*p/100)
original <- datMCAR
datMCAR[sample(1:nrow(datMCAR), amount_removed), attribute_modified] <- NA

#md.pattern(datMCAR)
imputed_data <- mice(datMCAR, m = 5, defaultMethod = c("norm.predict", "logreg", "polyreg", "polr"))
#imputed_data <- mice(datMCAR, m = 5)
imp_MCAR <- complete(imputed_data)

actuals <- original[,attribute_modified][is.na(datMCAR[,attribute_modified])]
predicteds <- imp_MCAR[is.na(datMCAR[,attribute_modified]), attribute_modified]
regr.eval(actuals, predicteds)

actual_diameter <- as.data.frame(original[,attribute_modified][is.na(datMCAR[,attribute_modified])])
names(actual_diameter) <- c('Diameter')
predicted_diameter <- as.data.frame(imp_MCAR[is.na(datMCAR[,attribute_modified]), attribute_modified])
names(predicted_diameter) <- c('Diameter')

ggplot(predicted_diameter, aes(Diameter)) + geom_density(color='blue') + geom_density(data=actual_diameter, aes(Diameter), color='red')

# formula for regression model
# Diameter ~ Sex + Length + ... 
form1 <- reformulate(names(datMCAR[, !names(datMCAR) %in% c(attribute_modified)]), attribute_modified)
fit1 <- with(imputed_data, lm(as.formula(Reduce(paste, deparse(form1)))))
# use regression to pool together different iterations (for estimates)
pool1 <- pool(fit1)
# summary(pool1)
# complete dataset with one of the imputations

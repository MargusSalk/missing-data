# https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv
# https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality.names

library(dplyr)
library(dummies)
library(polycor)
datMNAR <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', header = T, sep = ';')
datMNAR$quality <- as.factor(datMNAR$quality)
p <- 10 #percentage of missing values added (11 for around 5% of total)
attribute_modified <- 'quality'
threshold <- c(6, 7, 8) # remove from these values

amount_removed <- as.integer(length(which(!is.na(match(datMNAR[,attribute_modified], threshold))))*p/100)
datMNAR[sample(which(!is.na(match(datMNAR[,attribute_modified], threshold))), amount_removed), attribute_modified] <- NA

# removing attribute_modified from dataset and converting other factor variables to dummies
attr_modified_col <- select(datMNAR, get(attribute_modified))
other_columns <- select(datMNAR, -(get(attribute_modified)))
other_columns <- dummy.data.frame(other_columns, dummy.class="factor", sep=".", fun=as.factor)
# add the two together
datMNARdum <- cbind(attr_modified_col, other_columns)

missing_to_binary <- function(x, attr) {
  if (class(x[,attr]) == 'factor') {
    x[,attr] <- as.character(x[,attr]);
  }
  x[,attr][!is.na(x[,attr])] <- 0;
  x[,attr][is.na(x[,attr])] <- 1;
  x[,attr] <- as.factor(x[,attr]);
  return(x);
}
# convert column attribute_modified to binary (1 value missing, 0 value observed)
datMNAR1 <- missing_to_binary(datMNARdum, attribute_modified)
# correlation matrix with other variables
cr <- hetcor(datMNAR1, std.err = F)$correlations[attribute_modified,]
tmp <- as.data.frame(cr)
tmp <- tmp[rownames(tmp) != attribute_modified,]
tmp <- as.data.frame(abs(tmp))
strong_coef <- 0.15 # set point from which upwards is 'strongly' correlated
cr
cat('There are ',nrow(filter(tmp, tmp > strong_coef)),'columns strongly correlated(more than ',strong_coef,') with missing values of ',attribute_modified)
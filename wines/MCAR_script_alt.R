# script does not perform well because of attr 'quality' distribution.

# https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv
# https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality.names
library(dplyr)
library(dummies)
library(polycor)

datMCAR <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', header = T, sep = ';')
datMCAR$quality <- as.factor(datMCAR$quality)
p <- 5 #percentage of missing values added
attribute_modified <- 'alcohol'
amount_removed <- as.integer(nrow(datMCAR)*p/100)
datMCAR[sample(1:nrow(datMCAR), amount_removed), attribute_modified] <- NA

# removing attribute_modified from dataset and converting other factor variables to dummies
attr_modified_col <- select(datMCAR, get(attribute_modified))
other_columns <- select(datMCAR, -(get(attribute_modified)))
other_columns <- dummy.data.frame(other_columns, dummy.class="factor", sep=".", fun=as.factor)
# add the two together
datMCARdum <- cbind(attr_modified_col, other_columns)

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
datMCAR1 <- missing_to_binary(datMCARdum, attribute_modified)
# correlation matrix with other variables
cr <- hetcor(datMCAR1, std.err = F)$correlations[attribute_modified,]
tmp <- as.data.frame(cr)
tmp <- tmp[rownames(tmp) != attribute_modified,]
tmp <- as.data.frame(abs(tmp))
strong_coef <- 0.15 # set point from which upwards is 'strongly' correlated
cr
cat('There are ',nrow(filter(tmp, tmp > strong_coef)),'columns moderately correlated(more than ',strong_coef,') with missing values of ',attribute_modified)
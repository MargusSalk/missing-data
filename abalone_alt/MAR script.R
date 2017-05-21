library(dplyr)
library(dummies)
library(polycor)
# https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.names - more info on dataset
datMAR <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', header = F, sep = ',')
names(datMAR) <- c('Sex','Length','Diameter','Ht','Whole_wt','Shucked_wt','Viscera_wt','Shell_wt','Rings')
p <- 30 #percentage of missing values added
attribute_modified <- 'Sex'
related_attribute <- 'Length'
related_attribute_value <- 0.4

amount_removed <- as.integer(length(which(datMAR[,related_attribute] < related_attribute_value))*p/100)
datMAR[sample(which(datMAR[,related_attribute] < related_attribute_value), amount_removed), attribute_modified] <- NA

# removing attribute_modified from dataset and converting other factor variables to dummies
attr_modified_col <- select(datMAR, get(attribute_modified))
other_columns <- select(datMAR, -(get(attribute_modified)))
other_columns <- dummy.data.frame(other_columns, dummy.class="factor", sep=".", fun=as.factor)
# add the two together
datMARdum <- cbind(attr_modified_col, other_columns)

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
datMAR1 <- missing_to_binary(datMARdum, attribute_modified)
# correlation matrix with other variables
cr <- hetcor(datMAR1, std.err = F)$correlations[attribute_modified,]
tmp <- as.data.frame(cr)
tmp <- tmp[rownames(tmp) != attribute_modified,]
tmp <- as.data.frame(abs(tmp))
strong_coef <- 0.8 # set point from which upwards is 'strongly' correlated
cr
cat('There are ',nrow(filter(tmp, tmp > strong_coef)),'columns strongly correlated(more than ',strong_coef,') with missing values of ',attribute_modified)
library(dplyr)
library(dummies)
library(polycor)
# https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.names - more info on dataset
datMNAR <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', header = F, sep = ',')
names(datMNAR) <- c('Sex','Length','Diameter','Ht','Whole_wt','Shucked_wt','Viscera_wt','Shell_wt','Rings')
p <- 30 #percentage of missing values added
attribute_modified <- 'Sex'
threshold <- 'I' # point from which we start removing values

# the plan: remove p% of Diameter values if Diameter > threshold

amount_removed <- as.integer(length(which(datMNAR[,attribute_modified] == threshold))*p/100)
datMNAR[sample(which(datMNAR[,attribute_modified] == threshold), amount_removed), attribute_modified] <- NA

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
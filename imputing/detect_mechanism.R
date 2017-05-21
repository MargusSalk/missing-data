library(dplyr)
library(dummies)
library(polycor)
# result codes: 1 - MCAR, 2 - MAR, 3 - MNAR
mcar_threshold <- 0.15
mar_threshold <- 0.8

missing_mechanism <- function(dat, attribute) {
  dat <- missing_to_binary(dat, attribute);
  dat <- factor_to_dummies(dat, attribute);
  cr <- hetcor(dat, std.err = F)$correlations[attribute,];
  correlations <- filter_correlations(cr, attribute);
  max <- max(correlations);
  if (max < mcar_threshold) {
    return(1);
  } else if (max < mar_threshold) {
    return(3);
  } else {
    return(2);
  }
}

missing_to_binary <- function(dat, attr) {
  dat[,attr][!is.na(dat[,attr])] <- 0;
  dat[,attr][is.na(dat[,attr])] <- 1;
  dat[,attr] <- as.factor(dat[,attr]);
  return(dat);
}

factor_to_dummies <- function(dat, attribute_excluded) {
  attr_modified_col <- select(dat, get(attribute_excluded));
  other_columns <- select(dat, -(get(attribute_excluded)));
  other_columns <- dummy.data.frame(other_columns, dummy.class="factor", sep=".", fun=as.factor);
  return(cbind(attr_modified_col, other_columns));
}

filter_correlations <- function(attr_correlations, attribute) {
  correlations <- abs(as.data.frame(attr_correlations));
  correlations <- correlations[rownames(correlations) != attribute,];
  return(as.data.frame(correlations));
}

dat <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', header = F, sep = ',')
names(dat) <- c('Sex','Length','Diameter','Ht','Whole_wt','Shucked_wt','Viscera_wt','Shell_wt','Rings')

# MCAR dataset
mcar <- dat
p <- 5 #percentage of missing values added
attribute_mcar <- 'Diameter'
amount_removed <- as.integer(nrow(mcar)*p/100)
mcar[sample(1:nrow(mcar), amount_removed), attribute_mcar] <- NA

# MNAR dataset
mnar <- dat
p <- 20 #percentage of missing values added
attribute_mnar <- 'Diameter'
threshold <- 0.5 # point from which we start removing values
amount_removed <- as.integer(length(which(mnar[,attribute_mnar] > threshold))*p/100)
mnar[sample(which(mnar[,attribute_mnar] > threshold), amount_removed), attribute_mnar] <- NA

# MAR dataset
mar <- dat
p <- 15 #percentage of missing values added
attribute_mar <- 'Diameter'
related_attribute <- 'Sex'
related_attribute_value <- 'M'
amount_removed <- as.integer(length(which(mar[,related_attribute] == related_attribute_value))*p/100)
mar[sample(which(mar[,related_attribute] == related_attribute_value), amount_removed), attribute_mar] <- NA

# results
res_mcar <- missing_mechanism(mcar, attribute_mcar)
res_mar <- missing_mechanism(mar, attribute_mar)
res_mnar <- missing_mechanism(mnar, attribute_mnar)

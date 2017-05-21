library(dplyr)
library(dummies)
library(polycor)


convert_to_factors <- function(dat) {
  for (n in names(dat)) {
    if (class(dat[,n]) == 'integer') {
      if (length(unique(dat[, n])) < 10) {
        dat[,n] <- as.factor(dat[,n]);
      }  
    }
  }
  return(dat);
}


#SET FULL PATH TO FILE Numeric_var.csv
numeric_var_path <- ""
med <- read.table(numeric_var_path, header=TRUE, sep=",")
med <- med[,2:41]
med <- convert_to_factors(med);
attribute_modified <- 'V2'
# removing attribute_modified from dataset and converting other factor variables to dummies
attr_modified_col <- select(med, get(attribute_modified))
other_columns <- select(med, -(get(attribute_modified)))
other_columns <- dummy.data.frame(other_columns, dummy.class="factor", sep=".", fun=as.factor)
# add the two together
meddum <- cbind(attr_modified_col, other_columns)

missing_to_binary <- function(x, attr) {
  if (class(x[,attr]) == 'factor') {
    x[,attr] <- as.character(x[,attr]);
  }
  x[,attr][!is.na(x[,attr])] <- 0;
  x[,attr][is.na(x[,attr])] <- 1;
  x[,attr] <- as.factor(x[,attr]);
  return(x);
}

# convert column attribute_modified to binary (1 - value missing, 0 - value observed)
med1 <- missing_to_binary(meddum, attribute_modified)
# correlation matrix with other variables
cr <- hetcor(med1, std.err = F, use = 'pairwise.complete.obs')$correlations[attribute_modified,]
tmp <- as.data.frame(cr)
tmp <- tmp[rownames(tmp) != attribute_modified,]
tmp <- as.data.frame(abs(tmp))
strong_coef <- 0.15 # set point from which upwards is 'strongly' correlated
cr
cat('There are ',nrow(filter(tmp, tmp > strong_coef)),'columns strongly correlated(more than ',strong_coef,') with missing values of ',attribute_modified)
######################################################
###  Create a formula for machine learning models  ###
######################################################

# This function allows to create a formula from the columns of a data frame
# arguments:
# data: R data frame
# position: number of the column in the data frame that we want to predict

create_formula <- function(data, position = 1)
{
  
  formula <- paste(colnames(data)[position], " ~ ", paste(colnames(data[-position]), collapse = " + "), sep = "")
  return(formula)
  
}


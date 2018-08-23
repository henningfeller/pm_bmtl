
standardize_continuous_input <- function(data_vector) {
  
  if (!( (is.vector(data_vector)) & (is.numeric(data_vector))) ) {
    stop("Input is not a numerical vector")
  }
  
  # Henning, you can come up with better names
  in.sd <- sd(data_vector)
  
  standardized <- data_vector / (2 * in.sd)
  
  inbetween.mean <- mean(standardized)
  
  standardized <- standardized - inbetween.mean
  
  standardization_function <- function(data_vector) {
    new_standardized <- (data_vector / (2*in.sd)) - inbetween.mean
    return(new_standardized)
  }
  
  backfunction <- function(standardized_vector) {
    if  (!( (is.vector(standardized_vector)) & (is.numeric(standardized_vector))) ) {
      stop("Input is not a numerical vector")
    }
    
    normal_data <- (standardized_vector + inbetween.mean) * 2 * in.sd
    return(normal_data)
  }	
  
  return(list(standardized_data = standardized,
              standardization_func = standardization_function,
              back_standardization_func = backfunction))
}

standardize_mean0_var1 <-function(df){
  
  for (i in 1:length(as.list( names(df)))){
    if (is.numeric(df[,i]) == "TRUE"){
      df[,i] = (df[,i] - mean(df[,i]))/sd(df[,i])
    }
  }
  return(df)
}

one_hot_encoding <-function(df){
	del <- list()
	for (i in 1:length(as.list( names(df)))){
		if (is.character(df[,i]) == "TRUE"){
			for(unique_value in unique(df[,i])){
				df[paste(colnames(df)[i], unique_value, sep = ".")] <- ifelse(df[,i] == unique_value, 1, 0)
			}
			del <- append(del,colnames(df)[i])
		}
	}
	df <- df[,!names(df) %in% del]
	
	return(df)
}

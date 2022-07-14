# read file 
data <- read.delim("Assignment_04_P_values.txt", header = FALSE, sep = "\t", dec = ".")

# convert to numeric
data[, 1] <- sapply(data[,1], as.numeric)

#convert to vector
p_values <- sapply(data[,1], as.vector)

#num p_values
num_p_values <- length(p_values)

# function to apply corrections methods
new.correct_pvalues <- function(pvalues,method="",flag=FALSE) {
   
   #correct#
   if (method == "b")
    {
      result_list <- p.adjust(pvalues, method = 'bonferroni')
    }
  else if (method == "f")
    {
      result_list <- p.adjust(pvalues, method = 'fdr')
    }
  
  # count significant 
  if (flag == TRUE)
  {
    sign <- 0
    for (p in result_list)
    {
      if (p <= 0.05)
      {
        sign <- sign+1
      }
    }
  }
  
  else 
  {
    sign <- 0
    for (p in pvalues)
    {
      if (p <= 0.05)
      {
        sign <- sign+1
      }
    }
  }
  return(sign)
}


sign_org = new.correct_pvalues(p_values,flag=FALSE)
sign_bonf = new.correct_pvalues(p_values,method="b",flag=TRUE)
sign_fdr = new.correct_pvalues(p_values,method="f",flag=TRUE) 


write(paste("The number of the p_values before any correction:", as.character(num_p_values)),file="results_file.txt",append=TRUE)
write(paste("The number of significant p_values before any correction:", as.character(sign_org)),file="results_file.txt",append=TRUE)
write(paste("The number of significant p_values after the Bonferroni method:", as.character(sign_bonf)),file="results_file.txt",append=TRUE)
write(paste("The number of the significant p_values after the FDR method:", as.character(sign_fdr)),file="results_file.txt",append=TRUE)



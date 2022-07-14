new.GE_Data_Normalization <- function(Data) 
{

  cols = 1:ncol(Data)
  rows = 1:nrow(Data)
  genes_df_Normalized<- data.frame(a=rows)
  
  for (val in cols)
  {
    
    col_name <- names(Data)[val]
    list_val <- unlist(Data[col_name], use.names = FALSE)
    
    if (is.numeric(list_val))
    {
      m <- mean(list_val)
      s <- sd(list_val)
      gene_Normalized <- mutate(Data, new = (list_val - m) / s)$new
      genes_df_Normalized[col_name] <- gene_Normalized
    }
    
    else
    {
      genes_df_Normalized[col_name] <-Data[col_name]
    }
    
  }

  genes_df_Normalized<- genes_df_Normalized[-1]

 
  return(genes_df_Normalized)
}

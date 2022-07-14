new.DNA_to_RNA <- function(DNA) 
{
  DNA_split <- strsplit(DNA, "")[[1]] 
  DNA_len = nchar(DNA) 
  my_range = 1:DNA_len
  
  for (val in my_range)
    {
    
       if (DNA_split[val] == "T") 
         { 
      
            DNA_split[val] = "U" 
      
         }
    } 
  RNA <- paste(DNA_split, collapse = '')
  
  return(RNA)
}
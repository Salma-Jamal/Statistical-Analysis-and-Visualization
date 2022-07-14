Genes_df <- read.delim("Assignment_05_GE_Data.txt", header = TRUE,sep="")
healthy <- Genes_df$G_Healthy
cancerous <- Genes_df$G_Cancerous

new.hypotheses_testing <- function(sample_1,sample_2,method="Method_1",num_permu=0,h0,h1)
  {
  
  if (method == "Method_1")
  {
    # First method
    ALPHA = 0.05
    heal_nc <- shapiro.test(sample_1)
    can_nc <- shapiro.test(sample_2)
    
    if ( (heal_nc$p.value > ALPHA) & (can_nc$p.value > ALPHA) )
    {
      var_samples <- var.test(sample_1, sample_2)
      
      # if var are equal
      if (var_samples$p.value > ALPHA)
      {
        result <- t.test(x = sample_1, y=sample_2, paired = FALSE, alternative ='two.sided', var.equal = TRUE)
        justif <- "Both Samples follow Normal Distribution & Variance are equal so we will use t-test"
        
      }
      
      # var not equal
      else
      {
        # Wilch test
        result <- t.test(x= sample_1, y=sample_2, paired = FALSE, alternative ='two.sided', var.equal =FALSE)
        justif <- "Both Samples follow Normal Distribution but Variance are not equal so we will use Wilch test"
      }
    }
    
    # doesn't follow normal
    else
    {
      result <- wilcox.test(x = sample_1, y = sample_2, alternative = 'two.sided')
      justif <- "Samples doesn't follow Normal Distribution so we will use wilcox test"
    }
    
    p_value <- result$p.value
    
  }
  
  else
  {
        # second method
        T_healthy = mean(sample_1) #(max(sample_1) + min(sample_1)) / 2
        T_cancer  = mean(sample_2) #(max(sample_2) + min(sample_2)) / 2
        
        set.seed(NULL) # To regain the randomness of the random generators.
        T_obs = T_cancer - T_healthy
        # perm values vector
        T_perm <- vector(mode = "double", length = num_permu)
        combined_GE = c(sample_1, sample_2)
        L = length(combined_GE)
        
        for (i in 1:num_permu) 
        {
            Ind_healt = sample(L, L/2) 
            healthy_perm = combined_GE[Ind_healt] 
            cancerous_perm = combined_GE[-Ind_healt] 
            
            T_health_perm = mean(healthy_perm) #(max(healthy_perm) + min(healthy_perm)) / 2
            
            T_cancer_perm = mean(cancerous_perm) #(max(cancerous_perm) + min(cancerous_perm)) / 2
            
            T_perm[i] = T_cancer_perm - T_health_perm
        }
        
        p_value <- length(which(T_perm >= T_obs)) / num_permu
      
  }
  
  write(method,file="results_file.txt",append=TRUE)
  write(h0,file="results_file.txt",append=TRUE)
  write(h1,file="results_file.txt",append=TRUE)
  if(method == "Method_1")
    {write(justif,file="results_file.txt",append=TRUE)}
  write(paste("p_value =",p_value),file="results_file.txt",append=TRUE)
  write("----------------------------------------------------",file="results_file.txt",append=TRUE)
  
  return(p_value)
}


m1_pvalue <- new.hypotheses_testing(healthy, cancerous,h0="h0: GE(cancerous) = GE(healthy)",h1="h1: GE(cancerous) != GE(healthy)")

m2_pvalue <- new.hypotheses_testing(healthy, cancerous, method = "Method_2",num_permu= 1000000,h0="h0: T_cancerous - T_healthy = 0",h1="h0: T_cancerous - T_healthy > 0")

if ((m1_pvalue <= 0.05) & (m2_pvalue <= 0.05))
  {
      write("Both pvalues will result in reject null hypothesis",file="results_file.txt",append=TRUE)
  }else if ((m1_pvalue <= 0.05) & (m2_pvalue > 0.05))
  {
    write("method1 pvalue will result to reject null hypothesis And method2 pvalue will result to fail to reject null hypothesis ",file="results_file.txt",append=TRUE)
  }else if ((m1_pvalue > 0.05) & (m2_pvalue <= 0.05))
  {
    write("method1 pvalue will result to fail to reject null hypothesis And method2 pvalue will result to reject null hypothesis ",file="results_file.txt",append=TRUE)
  }

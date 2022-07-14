set.seed(50)

## Gene 1 ##
G1_control   <- rnorm(1000)
G1_condition <- rnorm(1000)


## Gene 2 ##
G2_control   <- rnorm(1000, mean=2, sd=0.5) 
G2_condition <- rnorm(1000, mean=4, sd=0.5)


## Gene 3 ##
G3_control   <-  rnorm(1000, mean=2, sd=0.5)
G3_condition <-  rnorm(1000, mean=0, sd=1.5)

## Gene 4 ##
G4_control   <- rnorm(1000)
G4_condition <- runif(1000, min = 0, max = 1)


### Function ###

new.hypotheses_testing <- function(sample_1,sample_2,paired_flag,altern,single=FALSE,Gene_name,h0,h1) 
{
  ALFA <- 0.05
  if (single == TRUE)
    {
      nc <- shapiro.test(sample_1)
      if (nc$p.value > ALFA)
        {
          result <- t.test(x = sample_1, mu = sample_2, alternative = "two.sided")          
        }
      
      else
        {
         result <- wilcox.test(x = sample_1, mu = sample_2, alternative = "two.sided")
        }
    }
  
  else
    {
      # Paired Samples
      if (paired_flag == TRUE)
          {
          
              # paired diff
              dif = sample_1 - sample_2
              # check if normal or not
              normality_check <- shapiro.test(dif)
          
              # if follow Normal 
              if (normality_check$p.value > ALFA) 
                {
                   # t-test p_value ≤ α reject the null hypothesis
                   result <- t.test(sample_2, sample_1, paired = TRUE, alternative = altern)
                }
          
              # if it doesn't follow normal
              else
                {
                  result <- wilcox.test(sample_2, sample_1, alternative = altern, paired = TRUE)
                }
          }
      
      # Independent Samples
      else
          {
              # check if samples follow normal distribution
              s1_nc <- shapiro.test(sample_1)
              s2_nc <- shapiro.test(sample_2)
            
              # if follow normal
              if ( (s1_nc$p.value > ALFA) & (s2_nc$p.value > ALFA) )
                {
                    var_samples <- var.test(sample_2, sample_1)
                    
                    # if var are equal
                    if (var_samples$p.value > ALFA)
                      {
                        result <- t.test(x= sample_2, y=sample_1, paired = FALSE, alternative =altern, var.equal = TRUE)
                      
                      }
                
                    # var not equal
                    else
                    {
                      # Wilch test
                      result <- t.test(x= sample_2, y=sample_1, paired = FALSE, alternative =altern, var.equal =FALSE)
                    }
                }
        
              # doesn't follow normal
              else
              {
                result <- wilcox.test(x = sample_2, y = sample_1, alternative = altern)
              }
          }
      
    }
  
  p_v <- result$p.value
  if (p_v <= ALFA){
    conc <- "Reject the Null Hypothesis"
  }
  else{
    conc <- "Fail to Reject the Null Hypothesis"
  }
  
  test_output <- capture.output(print(result))
  
  write(Gene_name,file="results_file.txt",append=TRUE)
  write(h0,file="results_file.txt",append=TRUE)
  write(h1,file="results_file.txt",append=TRUE)
  write(test_output,file="results_file.txt",append=TRUE)
  write(conc,file="results_file.txt",append=TRUE)
  write("------------------------------------",file="results_file.txt",append=TRUE)
}

#data <- data.frame(cont= G3_control,cond = G3_condition)
#boxplot(data) 

## Call The Function ##
q_1<- new.hypotheses_testing(G1_control,G1_condition,TRUE,'two.sided',FALSE,'Gene1',"ho: avg G_cond = avg G_cont", "h1: avg G_cond != avg G_cont")


q_2<- new.hypotheses_testing(G2_control,G2_condition,FALSE,'g',FALSE,'Gene2',"ho: G_cond <= G_cont", "h1: G_cond > G_cont")


q_3<- new.hypotheses_testing(G3_control,G3_condition,FALSE,'g',FALSE,'Gene3',"ho: G_cond <= G_cont", "h1: G_cond > G_cont")


q_4<- new.hypotheses_testing(G4_control,G4_condition,FALSE,'two.sided',FALSE,'Gene4 Q1',"ho: avg G_cond = avg G_cont", "h1: avg G_cond != avg G_cont")


q_4_2<- new.hypotheses_testing(G4_control,0.2,FALSE,'two.sided',TRUE,'Gene4 Q2',"ho: avg G_cont = GE-level", "h1: avg G_cont != GE-level")
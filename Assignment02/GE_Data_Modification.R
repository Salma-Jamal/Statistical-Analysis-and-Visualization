## Part 1 ##

install.packages("magrittr") 
install.packages("dplyr")    
library(magrittr) 
library(dplyr)   
source('GE_Data_Normalization.R')

genes_df <- read.csv("Assignment_02_Data.csv")
print(genes_df)


genes_df_cleaned <- genes_df %>% mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

print(genes_df_cleaned)

Genes_df_Normalized <- new.GE_Data_Normalization(genes_df_cleaned)
print(Genes_df_Normalized)


write.table(Genes_df_Normalized, 'Genes_normalized.txt', sep = "\t",row.names = FALSE, col.names = TRUE)


## Part 2 ##

GE_Data <- read.delim('Genes_normalized.txt', header = TRUE, sep = "\t")
GE_Sample_Mean <- c(apply(GE_Data[,-1], 1, mean))
GE_Sample_SD <- c(apply(GE_Data[,-1], 1, sd))
x <- 1:7

print(GE_Data)
print(GE_Sample_Mean)


windows(width=20, height=15)
par(bg= "#F1F1F1")
grid = matrix(c(1,2,1,3),nrow=2,ncol=2,byrow = TRUE)
layout(grid)

## Fig 1 ##
plot(x, GE_Sample_Mean, type = "b", lty = 1, pch = 19, 
     col = "violet", xlab = "Gene", ylab = "Mean & SD",
     main = 'Mean and SD values',cex.main = 0.8)
lines(x, GE_Sample_SD, pch = 19, col = "blue", type = "b", lty = 2,)
legend(x= "topleft",legend=c("Mean", "SD"),
       col=c("violet", "blue"), lty = 1:2,cex=0.7 ,inset = 0.2)

## Fig 2 ##
boxplot(GE_Sample_Mean, GE_Sample_SD, names = c('Mean', 'SD'),
        xlab = 'Descriptor', ylab = 'Value',
        main = 'Distribution of the Mean and SD values',cex.main = 0.8,col=c("violet", "blue"))

## Fig 3 ##
plot(ecdf(GE_Sample_Mean), xlab = 'Data Points', main =
       'Cumulative Distribution Function of Mean', cex.main = 0.8,
     ylab = 'Fraction of data points',col = "violet")





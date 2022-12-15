library(jsonlite)
library(dplyr)
library(ggplot2)

#Import and read csv file as a dataframe
Mecha_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

lm( mpg ~ ., Mecha_table)

summary(lm( mpg ~ ., data= Mecha_table))


#suspension_coil

#Read and import file into table
suspension_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

#Find mean,median,variance and standard deviation of PSI 
total_summary <- suspension_table %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

#Group by each manufacturing lot by mean,median,variance and standard deviation of PSI 
lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups='keep')


#T-Tests

#t-test across all manufacturing lots
t.test(suspension_table$PSI,mu=1500)

#t-test across Lot1 
t.test(PSI ~ 1  , data=suspension_table, subset=Manufacturing_Lot == 'Lot1',mu=1500)
#t-test across Lot2
t.test(PSI ~ 1 , data=suspension_table, subset=Manufacturing_Lot == 'Lot2',mu=1500)
#t-test across Lot3
t.test(PSI ~ 1 , data=suspension_table, subset=Manufacturing_Lot == 'Lot3',mu=1500)


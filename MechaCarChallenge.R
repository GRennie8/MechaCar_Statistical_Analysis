# Deliverable 1
# Load in dplyr package
library(dplyr) 
# Read in csv as DataFrame
MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv', check.names=F,stringsAsFactors = F)
head(MechaCar_mpg)
# Linear regression model for DataFrame
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = MechaCar_mpg) 
# Summary to determine p-value and r-squared value
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = MechaCar_mpg)) 


# Deliverable 2 
# Read in csv as DataFrame
Suspension_Coil <- read.csv(file='Suspension_coil.csv', check.names=F,stringsAsFactors = F) 
#Stats summary with summarize()
total_summary <- Suspension_coil %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')
# Stats summary with group_by(C
lot_summary <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')


# Deliverable 3
# t-test for all the lots, pop mu = 1500 psi
t.test(Suspension_Coil$PSI,mu=mean(Suspension_Coil$PSI))
# t-test for lot 1
t.test(subset(Suspension_Coil$PSI,Suspension_Coil$Manufacturing_Lot == "Lot1"),mu=mean(Suspension_Coil$PSI))
# t-test for lot 2
t.test(subset(Suspension_Coil$PSI,Suspension_Coil$Manufacturing_Lot == "Lot2"),mu=mean(Suspension_Coil$PSI))
# t-test for lot 3
t.test(subset(Suspension_Coil$PSI,Suspension_Coil$Manufacturing_Lot == "Lot3"),mu=mean(Suspension_Coil$PSI))

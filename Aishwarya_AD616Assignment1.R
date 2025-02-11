### ASSINGMENT 1 - ENTERPREISE RISK ANALYTICS


##Solution-1
library(tidyverse)

#Initializing seed & specifying the number of trails
set.seed(123)
n<-100000

#Simulating weekly demand for different travel website
A <-rnorm(n,mean=200,sd=20)
B <-rnorm(n,mean=50,sd=10)
C <-rnorm(n,mean=100,sd=15)
D <-rnorm(n,mean=150,sd=30)
E <-rnorm(n,mean=100,sd=30)
F <-rnorm(n,mean=100,sd=10)

#Calculating total call center demand
total_call_center_demand_simulation <-A+B+C+D+E+F


#Calculating the Mean & Standard Deviation for the call center
mean_total_demand <- mean(total_call_center_demand_simulation)
sd_total_demand <-sd(total_call_center_demand_simulation)
cat("Mean of Total Call Center Demand:", mean_total_demand, "\n")
cat("Mean of Standard Deviation of Total Call Center Demand", sd_total_demand, "\n")

#Histogram for total call center demand
par(oma = c(0,0,0,0))
hist(total_call_center_demand_simulation,
  main= "TOTAL DEMAND IN CALL CENTER",
  col="pink1",
  border = "deeppink3",
  xlim = c(min(total_call_center_demand_simulation), max(total_call_center_demand_simulation)),
  breaks = 40,
  xlab = "Total Hours of Calls per Week",
  ylab = "Frequency of Calls",
  las = 1,
  cex.axis = 0.6,
  cex.lab = 0.8,
  cex.main = 0.9)
grid(col = "white", lty = 1)

## Solution-2
install.packages("e1071")
library(e1071)

# Initializing seed and number of trails
set.seed(123)
n<-100000

# Battery life simulation in months
battery_life_months <- rnorm(n,mean= 7*12,sd=2*12)

# Calculate refund amount for each trial
refund_amount_per_phone <- ifelse(battery_life_months < 5*12, 10 +(5*12 - battery_life_months)*1.50,0)

# a) Expected cost per cell phone to the manufacturer
expected_cost_per_phonne <- mean(refund_amount_per_phone)
cat("Expected cost per cell phone:", expected_cost_per_phonne, "\n")

# b) Probability a refund will be paid
probability_of_refund <- mean(refund_amount_per_phone > 0)
cat("Probability a refund will be paid:", probability_of_refund, "\n")

# c) Average cost per refund
average_cost_per_refund <- mean(refund_amount_per_phone[refund_amount_per_phone>0])
cat("Average cost per refund:", average_cost_per_refund, "\n")

##Solution-3

# Initializing seed and number of trials
set.seed(123)
n <- 100000

# Normal distribution of servings
servings <- round(rnorm(n, mean = 125, sd = 35))

# Initialising the costs
cost_after_purchase <- (0.5 + 0.15)
profit_after_purchase <- (2.75 - cost_after_purchase)
license_cost <- 100

# Function to calculate net profit for a given number of servings
calculate_net_profit <- function(max_servings) {
  minimum_sale <- pmin(servings, max_servings)
  total_sale <- (minimum_sale * profit_after_purchase) - license_cost
  loss_after_sale <- ((max_servings - total_sale) * 0.5)
  net_profit <- sum((total_sale - loss_after_sale)) / n
  return(net_profit)
}

# Calculate net profit for different scenarios using sapply
max_servings_options <- c(75, 100, 120, 140, 160, 180)
net_profits <- sapply(max_servings_options, calculate_net_profit)

# Print net profits
cat("Net Profits for Different Servings:\n")
print(net_profits)

# Histogram for 160 servings
profit_160 <- (pmin(servings, 160) * profit_after_purchase) - license_cost - ((160 - pmin(servings, 160) * profit_after_purchase) * 0.5)

par(oma = c(0, 0, 0, 0))
hist(profit_160,
     main = "Profit from 160 Servings",
     col = "pink1",
     border = "lightcoral",
     xlim = c(-100, 300),
     xlab = "Profit",
     ylab = "Frequency",
     las = 1,
     cex.axis = 0.8,
     cex.lab = 0.9)
    
# grid
      grid(col = "white", lty = 1)
        
# Labels to the bars
      text(hist(profit_160, plot = FALSE)$mids, hist(profit_160, plot = FALSE)$counts,
             labels = hist(profit_160, plot = FALSE)$counts, pos = 3, offset = 0.5, cex = 0.6)

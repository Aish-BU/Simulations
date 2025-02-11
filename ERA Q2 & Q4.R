library(dplyr)
set.seed(1223)

group_size_dist <- c(0, 0.3, 0.1, 0.4, 0.2)  # Percentages
group_sizes <- c(1, 2, 3, 4, 5)
arrival_time_windows <- c(15, 10, 5, 10, 15)/60  # Inter-arrival times for each hour block
simulation_hours <- seq(0, 5)  # Operating hours 5pm-10pm
current_table_mix <- c(0, 21, 3)  # Numeric vector for 2, 4, and 6-top tables
proposed_table_mix <- c(4, 17, 4)  # Numeric vector for 2, 4, and 6-top tables

# Function to simulate arrivals based on inter-arrival time distribution
simulate_arrivals <- function(inter_arrival_times, hours) {
  arrivals <- numeric()
  for (hour in hours) {
    time <- hour
    while (time < hour + 1) {
      inter_arrival <- sample(inter_arrival_times, 1, replace = TRUE)
      time <- time + inter_arrival
      if (time < hour + 1) {
        arrivals <- c(arrivals, time)
      }
    }
  }
  return(arrivals)
}
simulate_night <- function(table_mix, arrival_time_windows, group_size_dist, simulation_hours) {
  tables <- rep(seq_along(table_mix), times = table_mix)
  table_states <- rep(0, length(tables))  # 0 for available, 1 for occupied
  table_release_times <- rep(0, length(tables))  # Track when each table will be free
  wait_times <- numeric()
  arrivals <- simulate_arrivals(arrival_time_windows, simulation_hours)
  
  
  
  
  # Adding variability in meal duration between 50 to 70 minutes
  meal_duration <- runif(n = length(arrivals), min = 50/60, max = 70/60)  # Meal durations in hours
  
  for (i in seq_along(arrivals)) {
    arrival <- arrivals[i]
    group_size <- sample(group_sizes, size = 1, prob = group_size_dist)
    suitable_table_index <- which(tables >= group_size & table_states == 0 & table_release_times <= arrival)
    
    if (length(suitable_table_index) > 0) {
      chosen_table <- suitable_table_index[1]
      table_states[chosen_table] <- 1
      table_release_times[chosen_table] <- arrival + meal_duration[i]
    } else {
      # Find the next available time
      if (length(table_release_times[table_release_times > arrival]) > 0) {
        wait_times <- c(wait_times, min(table_release_times[table_release_times > arrival]) - arrival)
      }
    }
  }
  
  # Convert wait times to minutes
  wait_times_in_minutes <- wait_times * 60
  
  return(list(
    avg_wait_time = mean(wait_times_in_minutes, na.rm = TRUE), 
    utilization = mean(table_states)
  ))
}

# Function to run simulations and summarize results
run_simulation <- function(table_mix, num_simulations = 1000) {
  results <- replicate(num_simulations, simulate_night(table_mix, arrival_time_windows, group_size_dist, simulation_hours), simplify = FALSE)
  
  avg_wait_times <- sapply(results, function(res) res$avg_wait_time)
  avg_utilization <- sapply(results, function(res) res$utilization)
  
  list(
    avg_wait_time = mean(avg_wait_times),
    utilization = mean(avg_utilization) * 100
  )
}

# Run simulations for current and proposed table mixes
results_current <- run_simulation(current_table_mix)
results_proposed <- run_simulation(proposed_table_mix)

# Print the results
cat("Current Table Mix:\n")
cat(results_current$avg_wait_time, "minutes\n")
cat(results_current$utilization, "%\n\n")

cat("Proposed Table Mix:\n")
cat(results_proposed$avg_wait_time, "minutes\n")
cat(results_proposed$utilization, "%\n")


#_____________________________________________________________________________
# QUESTION - 4
# Define alternative table mixes
alternative_table_mixes <- list(
  c(5, 5, 5),
  c(4, 4, 4, 4),
  c(2, 2, 2, 2),
  c(3, 3, 3, 3)
)

#Initialize variables to store results
best_mix <- NULL

#Initialize low and high value
best_avg_wait_time <- Inf
best_utilization <- -Inf

# Loop through alternative mixes
for (alternative_mix in alternative_table_mixes) {
  results_alternative <- run_simulation(alternative_mix)
  
  #Check if this mix is better than the current best mix
  if(results_alternative$avg_wait_time < best_avg_wait_time &&
     results_alternative$utilization > best_utilization) {
    best_mix <- alternative_mix
    best_avg_wait_time <- results_alternative$avg_wait_time
    best_utilization <- results_alternative$utilization
  }
}

#Print the best mix and its results
cat("Best Table Mix:/n")
cat("Table Sizes:", best_mix, "\n")
cat("Average Wait Time:", best_avg_wait_time, "minutes\n")
cat("Utilization:", best_utilization, "%\n")
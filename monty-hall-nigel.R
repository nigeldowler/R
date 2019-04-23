# Monte Carlo Simulations on the Monty Hall game
# Nigel Dowler
# April 2019

# Required libraries
library(dplyr)
library(ggplot2)

# Install/load gganimate library
#devtools::install_github('thomasp85/gganimate')
library(gganimate)

# Set plot theme
theme_set(theme_bw())

# Set the seed if you want to fix the random number generator
set.seed(42)


### 10 simulations -------------------------------------

# How many boxes to choose from? Usually 3.
num_boxes <- 3

# How many times to play the game?
num_trials <- 10

# Create a data frame to hold the cumulative results of every trial
montecarlo <- data.frame(trial  = integer(),
                         result = factor(levels=c("Win","Lose")),
                         total  = integer())

# Initiate totals
totalWin <- 0
totalLose <- 0

# Loop through every trial
for (i in 1:num_trials)
{
  # Randomly assign the prize to a box
  box_prize <- sample(1:num_boxes, 1)

  # Randomly choose a box
  box_choice <- sample(1:num_boxes, 1)

  # Update the win and lose totals
  totalWin <- totalWin + as.numeric(box_choice == box_prize)
  totalLose <- totalLose + as.numeric(box_choice != box_prize)

  # Add the new results to the data frame
  newtrial <- data.frame(trial  = c(i, i),
                         result = c("Win", "Lose"),
                         total  = c(totalWin, totalLose))
  
  montecarlo <- rbind(montecarlo, newtrial)
}

# Check it
tail(montecarlo)

# Create a plot
p <- ggplot(data = montecarlo, aes(x = result, y = total, fill = result)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_x_discrete(limits = c("Win", "Lose")) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = "{closest_state} games") +
  transition_states(trial, 1, 1)

# Render and display the animated plot
animate(p, nframes = nrow(montecarlo), fps = 2, renderer = gifski_renderer(loop = FALSE))

# Print out a summary of the results
montecarlo %>% filter(trial == num_trials) %>% select(result, total)

# Save the results
montecarlo_10    <- montecarlo



### 100 simulations -----------------------------------

# How many boxes to choose from? Usually 3.
num_boxes <- 3

# How many times to play the game?
num_trials <- 100

# Create a data frame to hold the cumulative results of every trial
montecarlo <- data.frame(trial  = integer(),
                         result = factor(levels=c("Win","Lose")),
                         total  = integer())

# Initiate totals
totalWin <- 0
totalLose <- 0

# Loop through every trial
for (i in 1:num_trials)
{
  # Randomly assign the prize to a box
  box_prize <- sample(1:num_boxes, 1)
  
  # Randomly choose a box
  box_choice <- sample(1:num_boxes, 1)
  
  # Update the win and lose totals
  totalWin <- totalWin + as.numeric(box_choice == box_prize)
  totalLose <- totalLose + as.numeric(box_choice != box_prize)
  
  # Add the new results to the data frame
  newtrial <- data.frame(trial  = c(i, i),
                         result = c("Win", "Lose"),
                         total  = c(totalWin, totalLose))
  
  montecarlo <- rbind(montecarlo, newtrial)
}

# Check it
tail(montecarlo)

# Create a plot
p <- ggplot(data = montecarlo, aes(x = result, y = total, fill = result)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_x_discrete(limits = c("Win", "Lose")) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = "{closest_state} games") +
  transition_states(trial, 1, 1)

# Render and display the animated plot
animate(p, nframes = nrow(montecarlo), fps = 20, renderer = gifski_renderer(loop = FALSE))

# Print out a summary of the results
montecarlo %>% filter(trial == num_trials) %>% select(result, total)

# Save the results
montecarlo_100 <- montecarlo



### 10,000 simulations ----------------------------------

# How many boxes to choose from? Usually 3.
num_boxes <- 3

# How many times to play the game?
num_trials <- 10000

# Create a data frame to hold the cumulative results of every trial
montecarlo <- data.frame(trial  = integer(),
                         result = factor(levels=c("Win","Lose")),
                         total  = integer())

# Initiate totals
totalWin <- 0
totalLose <- 0

# Loop through every trial
for (i in 1:num_trials)
{
  # Randomly assign the prize to a box
  box_prize <- sample(1:num_boxes, 1)
  
  # Randomly choose a box
  box_choice <- sample(1:num_boxes, 1)
  
  # Update the win and lose totals
  totalWin <- totalWin + as.numeric(box_choice == box_prize)
  totalLose <- totalLose + as.numeric(box_choice != box_prize)
  
  # Add the new results to the data frame
  newtrial <- data.frame(trial  = c(i, i),
                         result = c("Win", "Lose"),
                         total  = c(totalWin, totalLose))
  
  montecarlo <- rbind(montecarlo, newtrial)
}

# Check it
tail(montecarlo)

# Filter the data to every 50th trial to make plotting easier
montecarlo_orig <- montecarlo
montecarlo <- montecarlo_10000 %>% filter(trial %% 50 == 0)
nrow(montecarlo)

# Create a plot
p <- ggplot(data = montecarlo, aes(x = result, y = total, fill = result)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_x_discrete(limits = c("Win", "Lose")) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = "{closest_state} games") +
  transition_states(trial, 1, 1)

# Render and display the animated plot
animate(p, nframes = nrow(montecarlo), fps = 20, renderer = gifski_renderer(loop = FALSE))

# Print out a summary of the results
montecarlo %>% filter(trial == num_trials) %>% select(result, total)

# Save the results
montecarlo_10000 <- montecarlo_orig

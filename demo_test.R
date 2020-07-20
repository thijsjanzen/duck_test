


start_N <- 100
death_rate <- 0.1
birth_rate <- 1.0
survival_to_adult <- 0.1

N_through_time <- c(start_N)
num_generations <- 100

N_t <- start_N
for(t in 1:num_generations) {
   # first we do death
  num_dead <- rbinom(n = 1, size = N_t, prob = death_rate)
  N_t <- N_t - num_dead

  # now we produce offspring, from a Poisson distribution with mean birth_rate
  num_offspring <- sum(rpois(n = N_t, lambda = birth_rate))

  # from those offspring, most will not make it:
  num_dead_offspring <- rbinom(n = 1, size = num_offspring, prob = 1 - survival_to_adult)
  num_offspring <- num_offspring - num_dead_offspring

  N_t <- N_t + num_offspring
  N_through_time <- c(N_through_time, N_t)
}

plot(N_through_time, type = "l")

# now, let's make this a bit more general, so we can do this many times:
sim_demography <- function(N_0,
                           birth_rate,
                           death_rate,
                           survival_to_adult,
                           num_generations) {

  N_through_time <- c(N_0)
  N_t <- N_0
  for(t in 1:num_generations) {
    # first we do death
    num_dead <- rbinom(n = 1, size = N_t, prob = death_rate)
    N_t <- N_t - num_dead

    # now we produce offspring, from a Poisson distribution with mean birth_rate
    num_offspring <- sum(rpois(n = N_t, lambda = birth_rate))

    # from those offspring, most will not make it:
    num_dead_offspring <- rbinom(n = 1, size = num_offspring, prob = 1 - survival_to_adult)
    num_offspring <- num_offspring - num_dead_offspring

    N_t <- N_t + num_offspring
    N_through_time <- c(N_through_time, N_t)
  }
  return(N_through_time)
}

# now we can do many runs:
to_plot <- c()
for(r in 1:100) {
  simulation_result <- sim_demography(N_0 = 1000,
                                      birth_rate = 1.1,
                                      death_rate = 0.1,
                                      survival_to_adult = 0.1,
                                      num_generations = 100)
  to_add <- cbind(r, 1:(num_generations+1), simulation_result)
  to_plot <- rbind(to_plot, to_add)

  # output progress to screen:
  cat(r, "\n")
}

colnames(to_plot) <- c("r", "t", "N")
to_plot <- tibble::as_tibble(to_plot)
ggplot(to_plot, aes(x = t, y = N, group = r)) +
  geom_line( alpha = 0.5) +
  theme_classic()

# or we can plot 95%:
to_plot %>%
  group_by(t) %>%
  summarise("mean_N" = mean(N),
            "max_N" = quantile(N, 0.975),
            "min_N" = quantile(N, 0.025)) %>%
  ggplot(aes(x = t, y = mean_N)) +
    geom_ribbon(aes(x = t, ymin = min_N, ymax = max_N),
                fill = "grey", alpha = 0.5) +
    theme_classic() +
    geom_line() +
    xlab("Generations") +
    ylab("Population Size")








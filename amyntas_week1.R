require(rethinking)

# 1)

n = 100
# the grid
p_grid <- seq(0, 1, length.out=n)
# the prior
prior <- rep(1, n)
# the likelihood at each value in grid
likelihood <- dbinom(4 , size=15, prob=p_grid)
# compute product of likelihood and prior
unstd.posterior <- likelihood*prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

plot(p_grid, posterior,
     xlab="probability of water", 
     ylab="posterior probability", 
     type="b")

# 2)

# earth surface mostly water
prior <- ifelse(p_grid<0.5, 0, 1)
# the likelihood at each value in grid
likelihood <- dbinom(4 , size=6, prob=p_grid)
# compute product of likelihood and prior
unstd.posterior <- likelihood*prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

plot(p_grid, posterior,
     xlab="probability of water", 
     ylab="posterior probability", 
     type="p")

# 3)

# sampling from the posterior
samples <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior)

# 89% percentile and HPDI intervals
abline(v = PI(samples, prob=.89 ), col = "red")
abline(v = HPDI(samples, prob=.89), col = "blue")

# 4)
#AAAAAARGGGGH

#water.discrimination = rbinom(1, 1, .2)
#water.discrimination = sample(c("W","L"), 1, prob = )
observations = rbinom(20, 1, .7)
observations

for (i in 1:20) { 
observations[i] = ifelse(observations[i] == 1, rbinom(1, 1, .2), 0)
}
observations


water.discrimination = observations
df = data.frame(observations=observations,
                water.discrimination=water.discrimination)
df$water.discrimination = ifelse(df$observations == 1, rbinom(1, 1, .2), 0)

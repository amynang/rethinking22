require(rethinking)

# 1) ====

n = 100
p_grid = seq(0, 1, length.out=n)

# the prior
prior = rep(1, n)
# the likelihood at each value in grid
likelihood = dbinom(4 , size=15, prob=p_grid)
# compute product of likelihood and prior
unstd.posterior = likelihood*prior
# standardize the posterior, so it sums to 1
posterior = unstd.posterior/sum(unstd.posterior)

plot(p_grid, posterior,
     xlab="probability of water", 
     ylab="posterior probability", 
     type="b")

# 2) ====

# earth surface mostly water
prior = ifelse(p_grid<0.5, 0, 1)
# the likelihood at each value in grid
likelihood = dbinom(4 , size=6, prob=p_grid)
# compute product of likelihood and prior
unstd.posterior = likelihood*prior
# standardize the posterior, so it sums to 1
posterior = unstd.posterior/sum(unstd.posterior)

plot(p_grid, posterior,
     xlab="probability of water", 
     ylab="posterior probability", 
     type="p")

# 3) ====

# sampling from the posterior
samples = sample(p_grid, size=1e4, replace=TRUE, prob=posterior)

# 89% percentile and HPDI intervals
abline(v = PI(samples, prob=.89 ), col = "red")
abline(v = HPDI(samples, prob=.89), col = "blue")

# The width and location difference come from the fact that PI will exclude 5.5%
# from either extreme regardless of the density, while HPDI works from the mode 
# outwards until it has 89% of the density, so it includes the rather probable 
# bit that is left out by PI.
# Neither interval would show us that the posterior is truncated below 0.5

# 4) ====

# "Who does not have brains has legs" - Greek folk wisdom
# observations = rbinom(1e4, 1, .7)
# sum(observations)/length(observations)
# for (i in 1:1e4) { 
# observations[i] = ifelse(observations[i] == 1, rbinom(1, 1, .8), 0)
# }
# sum(observations)/length(observations)


# A generative model that is land-biased would weigh the true probability of 
# water by 1-.2=.8 giving .7*.8=.56 instead for a true proportion of .7
tosses = 1e4
true.proportion = .7
observations = rbinom(tosses, 1, true.proportion*.8)
sum(observations)/length(observations)


# back to an unbiased scenario
tosses = 20
observations = rbinom(tosses, 1, true.proportion)

# the prior
prior = rep(1, n)
# the likelihood at each value in grid
likelihood = dbinom(sum(observations), size=tosses, prob=p_grid)
# compute product of likelihood and prior
unstd.posterior = likelihood*prior
# standardize the posterior, so it sums to 1
posterior = unstd.posterior/sum(unstd.posterior)

plot(p_grid, posterior,
     xlab="probability of water", 
     ylab="posterior probability", 
     type="b")

samples = sample(p_grid, size=1e4, replace=TRUE, prob=posterior)

w = rbinom(1e4, size=20, prob=.7) ; simplehist(w)
w = rbinom(1e4, size=20, prob=samples) ; simplehist(w)


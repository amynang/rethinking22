# load data again, since it's a long way back
library(rethinking)
library(ggdist)
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]; d3 <- d[ d$age <= 13 , ]

plot( weight ~ height , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )


# 1) ====

# average height
xbar <- mean(d2$height)
# fit model
m.w <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b*( height - xbar ) ,
    a ~ dnorm( 70 , 15 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 30 )
  ) , data=d2 )
precis( m.h )

post <- extract.samples( m4.3 )
mu_at_140 <- post$a + post$b * ( 140 - xbar )
dens( mu_at_140 , col=rangi2 , lwd=2 , xlab="mu|weight=140" )
PI( mu_at_140 , prob=0.89 )

mu_at_160 <- post$a + post$b * ( 160 - xbar )
dens( mu_at_160 , col=rangi2 , lwd=2 , xlab="mu|weight=160" )
PI( mu_at_160 , prob=0.89 )

mu_at_175 <- post$a + post$b * ( 175 - xbar )
dens( mu_at_175 , col=rangi2 , lwd=2 , xlab="mu|weight=175" )
PI( mu_at_175 , prob=0.89 )
abline(v = PI(mu_at_175, prob=.89 ), col = "black")



# 2) ====
xbar <- mean(d3$age)
f1=
  ggplot(data = tibble(x = seq(from = 0, to = 6, by = .1)), 
         aes(x = x, y = dnorm(x, mean = 3, sd = .5))) +
  geom_line() +
  ylab("density")
f2=
  tibble(x = seq(from = -1, to = 6, by = .1)) %>%
  
  ggplot(aes(x = x, y = dunif(x, min = 0, max = 3))) +
  geom_line() +
  scale_y_continuous(NULL, breaks = NULL)
f1+f2




set.seed(42)
N <- 100 # 100 lines
a <- rnorm( N , 3 , .5 )
b <- rnorm( N , 1 , 1 )
plot( NULL , xlim=range(d3$age) , ylim=c(0,40) ,
      xlab="age" , ylab="weight" )
#abline( h=0 , lty=2 )
#abline( h=272 , lty=1 , lwd=0.5 )
#mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d3$age)
for ( i in 1:N ) curve( a[i] + b[i]*(x) ,
                        from=min(d3$age) , to=max(d3$age) , add=TRUE ,
                        col=col.alpha("black",0.2) )


m.w <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b*age,
    a ~ dnorm( 3 , .5 ) ,
    b ~ dlnorm( 1 , 1 ) ,
    sigma ~ dexp(1)
  ) , data=d3 )
precis( m.w )

# define sequence of weights to compute predictions for 4.54
# these values will be on the horizontal axis
age.seq <- seq( from=0 , to=13 , by=1 )
# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( m.w , data=data.frame(age=age.seq) )
# summarize the distribution of mu
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
# plot raw data
# fading out points to make line and interval more visible
plot( weight ~ age , data=d3 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( age.seq , mu.mean )
# plot a shaded region for 89% PI
shade( mu.PI , age.seq )

sim.weight <- sim( m.w , data=list(age=age.seq) )
str(sim.weight)

weight.PI <- apply( sim.weight , 2 , PI , prob=0.89 )
weight.HPDI <- apply( sim.weight , 2 , HPDI , prob=0.89 )

# plot raw data
plot( weight ~ age , d3 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( age.seq , mu.mean )
# draw HPDI region for line
shade( weight.HPDI , age.seq )
# draw PI region for simulated heights
shade( weight.PI , age.seq )


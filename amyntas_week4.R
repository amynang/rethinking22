library(rethinking)

# 1) ====
d <- sim_happiness( seed=1977 , N_years=1000 )

d2 <- d[ d$age>17 , ] # only adults
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )

d2$mid <- d2$married + 1
m6.9 <- quap(
  alist(
    happiness ~ dnorm( mu , sigma ),
    mu <- a[mid] + bA*A,
    a[mid] ~ dnorm( 0 , 1 ),
    bA ~ dnorm( 0 , 2 ),
    sigma ~ dexp(1)
  ) , data=d2 )
precis(m6.9,depth=2)

m6.10 <- quap(
  alist(
    happiness ~ dnorm( mu , sigma ),
    mu <- a + bA*A,
    a ~ dnorm( 0 , 1 ),
    bA ~ dnorm( 0 , 2 ),
    sigma ~ dexp(1)
  ) , data=d2 )
precis(m6.10)


compare(m6.9, m6.10)
PSIS(m6.9);PSIS(m6.10)

# The first model is expected to make better predictions

# a[1] and a[2] are the expected happines among unmarried and married 
# people. bA is the correlation of hapiness and age stratified by marital 
# status

# 2) ====

data(foxes)
d = data.frame( A = scale(foxes$area), 
                F = scale(foxes$avgfood),
                G = scale(foxes$groupsize),
                W = scale(foxes$weight))

# The total effect of food availability on fox weight
f <- alist(
  W ~ dnorm( mu , sigma ) ,
  mu <- a + b*F ,
  a ~ dnorm( 0 , .3 ) ,
  b ~ dnorm( 0 , .5 ) ,
  sigma ~ dexp( 1 )
)
f.2 <- quap( f , data = d )
precis(f.2)

# The direct effect of food availability on fox weight
f <- alist(
  W ~ dnorm( mu , sigma ) ,
  mu <- a + bF*F + bG*G ,
  a ~ dnorm( 0 , .3 ) ,
  bF ~ dnorm( 0 , .5 ) ,
  bG ~ dnorm( 0 , .5 ) ,
  sigma ~ dexp( 1 )
)
f.3 <- quap( f , data = d )
precis(f.3)

compare(f.2, f.3)
PSIS(f.2);PSIS(f.3)

# f.3 does better at prediction
# a is the expected mean fox weigth, bF is the direct 
# effect of food on weight and bG is the effect of 
# groupsize on weight.

# 3) ====
data("cherry_blossoms")
d <- cherry_blossoms[ complete.cases(cherry_blossoms$temp,
                                     cherry_blossoms$doy) , 2:3] 
d$D <- scale(d$doy)
d$T <- scale(d$temp)

f <- alist(
  D ~ dnorm( mu , sigma ) ,
  mu <- a + b*T ,
  a ~ dnorm( 0 , .2 ) ,
  b ~ dnorm( 0 , .4 ) ,
  sigma ~ dexp( 1 )
)
c.b.1 <- quap( f , data = d )

# prior predictive check
prior <- extract.prior( c.b.1 )
xseq <- c(-2,2)
mu <- link( c.b.1 , post = prior , data=list(T=xseq))
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:100 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )
precis(c.b.1)
plot( d$T , d$D , lwd=1, col=8 )
post <- extract.samples(c.b.1)
for ( i in 1:100 ) abline( post$a[i] , 
                           post$b[i] , 
                           lwd=3 , col=alpha(4, 0.1) )

f <- alist(
  D ~ dnorm( mu , exp(log_sigma) ) ,
  mu <- a + b[1]*T + b[2]*T^2 ,
  a ~ dnorm( 0 , .2 ) ,
  b ~ dnorm( 0 , .4 ) ,
  log_sigma ~ dnorm( 0 , 1 )
)
c.b.2 <- quap( f , data = d, start=list(b=rep(0,2)) )
precis(c.b.2, depth = 2)

post <- extract.samples(c.b.2)
plot( d$T , d$D , lwd=1, col=8 )
for ( i in 1:100 ) curve( post$a[i] + 
                          post$b[i,1]*x +
                          post$b[i,2]*x^2 , 
                           lwd=3 , col=alpha(4, 0.1), add=T)

compare(c.b.1,c.b.2)
# The linear model is better

post <- extract.samples(c.b.1)
mu_9C <- post$a + post$b * ( (9 - mean(d$temp))/sd(d$temp) )
dens( mu_9C , col=rangi2 , lwd=2 , xlab="mu|T=9C" )
abline(v = mean(mu_9C))
# The expected day of blossoming for 9 degrees is the 96 doy


# 4) ====
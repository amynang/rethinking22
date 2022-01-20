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
precis( m.w )


post <- extract.samples( m.w )

# Plot expected weights
par(mfrow=c(1,3))
mu_at_140 <- post$a + post$b * ( 140 - xbar )
dens( mu_at_140 , col=rangi2 , lwd=2 , xlab="mu|height=140" )
abline(v = mean(mu_at_140), col = "red")
abline(v = PI(mu_at_140, prob=.89 ), col = "black")

mu_at_160 <- post$a + post$b * ( 160 - xbar )
dens( mu_at_160 , col=rangi2 , lwd=2 , xlab="mu|height=160" )
abline(v = mean(mu_at_160), col = "red")
abline(v = PI(mu_at_160, prob=.89 ), col = "black")

mu_at_175 <- post$a + post$b * ( 175 - xbar )
dens( mu_at_175 , col=rangi2 , lwd=2 , xlab="mu|height=175" )
abline(v = mean(mu_at_175), col = "red")
abline(v = PI(mu_at_175, prob=.89 ), col = "black")



# 2) ====
xbar <- mean(d3$age)
# well I don't know what a six year old should weight,
# but a newborn baby would be ~3kg, I think

set.seed(42)
N <- 100 
a <- rnorm( N , 3 , .5 )
b <- rnorm( N , 1 , .4 )
par(mfrow=c(1,1))
plot( NULL , xlim=range(d3$age) , ylim=c(-5,40) ,
      xlab="age" , ylab="weight" )

for ( i in 1:N ) { 
  curve(a[i] + b[i]*(x) ,
        from=min(d3$age) , to=max(d3$age) , add=TRUE ,
        col=col.alpha("black",0.2) )
  }
# finally, babies don't shrink with age

m.w <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b*age,
    a ~ dnorm( 3 , .5 ) ,
    b ~ dlnorm( 1 , .4 ) ,
    sigma ~ dexp(1)
  ) , data=d3 )
precis( m.w )
# those are heavy newborns

age.seq <- seq( from=0 , to=13 , by=1 )
mu <- link( m.w , data=data.frame(age=age.seq) )
# summarize the distribution of mu
mu.mean <- apply( mu , 2 , mean )
sim.weight <- sim( m.w , data=list(age=age.seq) )
weight.PI <- apply( sim.weight , 2 , PI , prob=0.89 )

# plot raw data
plot( weight ~ age , d3 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( age.seq , mu.mean )
# draw PI region for simulated weights
shade( weight.PI , age.seq )


# 3====
d3$sex <- ifelse( d3$male==1 , 2 , 1 )

m.WHS <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a[sex] + b[sex]*age,
    a[sex] ~ dnorm( 3 , .5 ) ,
    b[sex] ~ dlnorm( 1 , 1 ) ,
    sigma ~ dexp(1)
  ) , data=d3 )
precis( m.WHS , depth=2 )

xseq = seq( from=0 , to=13 , by=1 )

muF = link(m.WHS,
           data = list(sex=rep(1,14),
                       age=xseq))
muM = link(m.WHS,
           data = list(sex=rep(2,14),
                       age=xseq))

mu.contr = muM-muF
plot(NULL, xlim=range(xseq),
           ylim=c(-1,4),
     xlab = "Age",
     ylab = "weight contrast M-F")
for (p in c(.5,.6,.7,.8,.9)) {
  shade(apply(mu.contr,2,PI,prob=p),xseq)
}
abline(h=0,lty=2)
# boys get increasingly heavier than girls with age
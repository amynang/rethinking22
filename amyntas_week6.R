library(rethinking)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
data(reedfrogs)
d <- reedfrogs
d$tank <- 1:nrow(d)
dat <- list(
  survival = d$surv,
  density = d$density,
  tank = d$tank )
#1) ----

#running any 2 out of 3 models goes fine, the 3rd results in session abort :( 

m.tank_1 <- ulam(
  alist(
    survival ~ dbinom( density , p ) ,
    logit(p) <- a[tank] ,
    a[tank] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ), data=dat , chains=4, cores = 4 , log_lik=TRUE )

#precis(m.tank.1, depth = 3)

# prior predictive check
prior1 <- extract.prior( m.tank_1 )

m.tank_0.1 <- ulam(
  alist(
    survival ~ dbinom( density , p ) ,
    logit(p) <- a[tank] ,
    a[tank] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( .1 )
  ), data=dat , chains=4, cores = 4 , log_lik=TRUE )

# prior predictive check
prior2 <- extract.prior( m.tank_0.1 )

m.tank_10 <- ulam(
  alist(
    survival ~ dbinom( density , p ) ,
    logit(p) <- a[tank] ,
    a[tank] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 10 )
  ), data=dat , chains=4, cores = 4 , log_lik=TRUE )

#precis(m.tank.1, depth = 3)

# prior predictive check
prior3 <- extract.prior( m.tank_10 )

beepr::beep(9)

xseq <- c(1:48)
mu <- link( m.tank_1 , post = prior1 , data=list(tank=xseq))
plot( NULL , xlim=c(1,48) , ylim=0:1 )
for ( i in 1:100 ) points( xseq , mu[i,] , col=col.alpha("black",0.1) )
plot(density(mu[,1]))

mu <- link( m.tank_0.1 , post = prior2 , data=list(tank=xseq))
plot( NULL , xlim=c(1,48) , ylim=c(0,1) )
for ( i in 1:100 ) points( xseq , mu[i,] , col=col.alpha("black",0.1) )
plot(density((mu[,1])))


mu <- link( m.tank_10 , post = prior3 , data=list(tank=xseq))
plot( NULL , xlim=c(1,48) , ylim=0:1 )
for ( i in 1:100 ) points( xseq , mu[i,] , col=col.alpha("black",0.3) )
plot(density((mu[,1])))
# reducing the prior on sigma widens the prior distribution of aj


#2) ----
library(dagitty)
dat <- list(
  S = d$surv,
  D = d$density,
  T = 1:nrow(d),
  P = ifelse(d$pred=="no",1L,2L),
  G = ifelse(d$size=="small",1L,2L)
)

dag <- dagitty('dag {D->S 
                     T->S 
                     P->S 
                     G->S}')
plot(dag)

mSPG <- ulam(
  alist(
    S ~ binomial(D,p),
    logit(p) <- a[T] + b[P,G],
    a[T] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1 ) ,
    matrix[P,G]:b ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data=dat, chains=4, cores = 4, log_lik=TRUE)

precis(mSPG, depth = 3)
# predation reduces survival, size increases survival in the absence of predators
# reduces it in their presence




#3) ----

dat <- list(
  S = d$surv,
  D_ = d$density,
  D = dplyr::case_when(d$density == 10 ~ 1L,
                       d$density == 25 ~ 2L,
                       d$density == 35 ~ 3L),
  T = 1:nrow(d),
  P = ifelse(d$pred=="no",1L,2L),
  G = ifelse(d$size=="small",1L,2L)
)

mSDPG <- ulam(
  alist(
    S ~ binomial(D_,p),
    logit(p) <- a[T] + 
                b1[P,G] + 
                b2[D,P], 
    a[T] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1 ) ,
    matrix[P,G]:b1 ~ dnorm(0,1),
    matrix[D,P]:b2 ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data=dat, chains=4, cores = 4, log_lik=TRUE)

precis(mSDPG, depth = 3)
post.mSPG <- extract.samples(mSPG)
post.mSDPG <- extract.samples(mSDPG)

dens(post.mSPG$sigma)
dens(post.mSDPG$sigma)
# sigma of the last model is lower; density acounts for some of the variation


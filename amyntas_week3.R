library(rethinking)
library(dagitty)
data(foxes)



fx_dag <- dagitty('dag{ A -> F -> G -> W ; F -> W}')
impliedConditionalIndependencies( fx_dag )

d = data.frame( A = scale(foxes$area), 
                F = scale(foxes$avgfood))


# 1) ====
# The effect of Area on Food

# model formula
f <- alist(
  F ~ dnorm( mu , sigma ) ,
  mu <- a + b*A ,
  a ~ dnorm( 0 , .3 ) ,
  b ~ dnorm( 0 , .5 ) ,
  sigma ~ dexp( 1 )
)

#fitting the model
f.1 <- quap( f , data = d )

# prior predictive check
prior <- extract.prior( f.1 )
xseq <- c(-2,2)
mu <- link( f.1 , post = prior , data=list(A=xseq))
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:100 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

# groups with larger territory have more available food
precis(f.1)
plot( d$A , d$F , lwd=1, col=8 )
post <- extract.samples(f.1)
for ( i in 1:100 ) abline( post$a[i] , 
                           post$b[i] , 
                           lwd=3 , col=alpha(4, 0.1) )


# 2) ====
d$G = scale(foxes$groupsize)
d$W = scale(foxes$weight)

# The total effect of food availability on fox weight
f <- alist(
  W ~ dnorm( mu , sigma ) ,
  mu <- a + b*F ,
  a ~ dnorm( 0 , .3 ) ,
  b ~ dnorm( 0 , .5 ) ,
  sigma ~ dexp( 1 )
)
f.2 <- quap( f , data = d )

# change in food availability does not influence fox weight overall
precis(f.2)
plot( d$F , d$W , lwd=1, col=8 )
post <- extract.samples(f.2)
for ( i in 1:100 ) abline( post$a[i] , 
                           post$b[i] , 
                           lwd=3 , col=alpha(4, 0.1) )

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
# the direct effect of food availability on fox weight is positive
plot( d$F , d$W , lwd=1, col=8 )
post <- extract.samples(f.3)
for ( i in 1:100 ) abline( post$a[i] , 
                           post$bF[i] , 
                           lwd=3 , col=alpha(4, 0.1) )
# but the effect of group size on weight is negative
plot( d$G , d$W , lwd=1, col=8 )
for ( i in 1:100 ) abline( post$a[i] , 
                           post$bG[i] , 
                           lwd=3 , col=alpha(4, 0.1) )

# Given this DAG it looks like larger territories with more food 
# allow foxes invest in raising more juveniles. Individual fox weight 
# is lower but fox biomass increases

# BUT an arrow is missing here; large groups should be able to control 
# more territory

fx_dag <- dagitty('dag{ A -> F -> G -> W ; 
                        F -> W ;
                        G -> A }')
plot(fx_dag)
adjustmentSets(fx_dag, "F", "W")
impliedConditionalIndependencies( fx_dag )




# 3) ====

# Adding an unobserved confound on the Stroke DAG
stroke_dag <- dagitty('dag {
bb="0,0,1,1"
A [adjusted,pos="0.4,0.6"]
S [adjusted,pos="0.4,0.3"]
X [exposure,pos="0.5,0.5"]
Y [outcome,pos="0.6,0.5"]
u [latent,pos="0.6,0.3"]
A -> S
A -> X
A -> Y
S -> X
S -> Y
X -> Y
u -> S
u -> Y
}
')
plot(stroke_dag)

adjustmentSets(stroke_dag, "X", "Y")

# the slope of X is the direct effect of HIV on Stroke. the slope of A 
# is the direct effect of Age on Stroke. Given the unobserved confound,
# the slope of S is both any direct effect of Smoking on Stroke AND 
# any association between Smoking and Stroke due to the unobserved 
# confound.
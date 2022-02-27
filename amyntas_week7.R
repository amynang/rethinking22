library(rethinking)
library(tidyverse)
data(bangladesh)

#1)----

d1 = bangladesh %>% 
  group_by(district) %>% 
  summarise(contra = sum(use.contraception),
            n=n()) %>% 
  add_row(district = 54, .before = 54)

d = list(C=d1[-54,]$contra,
         D=d1[-54,]$district,
         N=d1[-54,]$n)

m_cd <- ulam(alist(C ~ dbinom(N, p),
                   logit(p) <- a[D],
                   vector[61]:a ~ dnorm(a_bar, sigma),
                   a_bar ~ dnorm(0, 1),
                   sigma ~ dexp(1)), 
             data=d, 
             chains=4, 
             cores = 4,
             log_lik=TRUE )
trankplot(m_cd, n_cols=2)

precis(m_cd, depth = 2)

post = extract.samples(m_cd)

# raw data
plot(d1$contra/d1$n, xlab="District", ylab="Proportion using contraception")
# model estimates
points(logistic(apply(post$a, 2, mean)), col="brown")
#abline(v=54, lty=2, col=col.alpha(1,0.5))
# samples from the posterior
xseq <- c(1:61)
mu <- link( m_cd , post = post , data=list(D=xseq))
for (i in 1:500) points(xseq, mu[i,], col=col.alpha("red",0.01))
# a_bar
abline(h=mean(logistic(post$a_bar)), col=2)
# Due to partial pooling the model learns not only from the data of each
# region, but from other regions as well, so it pulls all estimates towards the 
# overall mean. This is more extreme for districts with few observations (like 3)
# District 54, having no data is right on the mean with a lot of uncertainty around it

#2)----
library(dagitty)
bangladesh_dag <- dagitty('dag {A->C
                                A->K
                                U->C
                                U->K
                                K->C
                                D->U
                                D->C}')
plot(bangladesh_dag)
adjustmentSets(bangladesh_dag, "U", "C")


#3)----
d = list(C=bangladesh$use.contraception,
         D=as.integer(bangladesh$district),
         U=ifelse(bangladesh$urban==0,1L,2L))

m_cdu <- ulam(alist(C ~ dbern(p),
                    logit(p) <- a[D] + b*U,
                    b ~ dnorm(0,.5),
                    vector[61]:a ~ dnorm(a_bar, sigma),
                    a_bar ~ dnorm(0, 1),
                    sigma ~ dexp(1)), 
              data=d, 
              chains=4, 
              cores = 4,
              log_lik=TRUE )
precis(m_cdu)

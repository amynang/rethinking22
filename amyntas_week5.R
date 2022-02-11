# 1) ====
library(rethinking)
data(NWOGrants)  
d = NWOGrants
d$gid = ifelse( d$gender=="m" , 1 , 2 )

m_tot <- ulam(
  alist(
    awards ~ dbinom( applications , p ) ,
    logit(p) <- a[gid] ,
    a[gid] ~ dnorm( 0 , 1.5 )
  ) , data=d , chains=4 , log_lik=TRUE)
trankplot(m_tot,n_cols=2)
precis( m_tot , depth=2 )
post <- extract.samples(m_tot)
diff_p <- inv_logit(post$a[,2]) - inv_logit(post$a[,1])

# Total effect shows women are less likely to be awarded grant
dens(diff_p, lwd = 2, col=4, xlab = "Women/Men")
abline(v= 0, lty = 3)

# 2) ====
d$discipline <- rep(1:9,each=2)

# number of applications by gender
m_ga <- ulam(
  alist(
    applications ~ dpois( lambda ),
    log(lambda) <- a[gid],
    a[gid] ~ normal(0,1)
  ), data=d , chains=4 , log_lik=TRUE )
precis(m_ga, 2)
post <- extract.samples(m_ga)
diff_ap <- exp(post$a[,2]) - exp(post$a[,1])

# women apply less than men
dens(diff_ap, lwd = 2, col=4, xlab = "Women/Men")

# applications by gender, discipline
m_gda <- ulam(
  alist(
    applications ~ dpois( lambda ),
    log(lambda) <- a[gid,discipline],
    matrix[gid,discipline]:a ~ normal(0,1)
  ), data=d , chains=4 , log_lik=TRUE )
precis(m_gda, depth=3)
post <- extract.samples(m_gda)
PrAp <- exp( post$a ) 
diff_prob_d <- sapply( 1:9 , function(i) PrAp[,2,i] - PrAp[,1,i] )

# this difference is less pronounced or even fliped for some disciplines
plot(NULL,xlim=c(-200,75),ylim=c(0,.05),xlab="Gender contrast (probability)",ylab="Density")
for ( i in 1:9 ) dens( diff_prob_d[,i] , lwd=2 , col=i, add = T)
abline(v=0,lty=3)

# direct effect of gender on awards
m_dir <- ulam(
  alist(
    awards ~ dbinom( applications , p ) ,
    logit(p) <- a[gid,discipline],
    matrix[gid,discipline]:a ~ normal(0,1)
  ) , data=d , chains=4 , iter=2000, log_lik=TRUE)
trankplot(m_dir, n_cols=2)

precis( m_dir , depth=3)
post <- extract.samples(m_dir)
PrA <- inv_logit( post$a ) 
diff_prob_d <- sapply( 1:9 , function(i) PrA[,2,i] - PrA[,1,i] )

total_apps <- sum(d$applications)

apps_per_disc <- sapply( 1:9 , function(i) sum(d$applications[d$discipline==i]) )

# all apps from men
p_G1 <- link(m_dir,data=list(
  discipline=rep(1:9,times=apps_per_disc),
  applications=rep(1,total_apps),
  gid=rep(1,total_apps)))

# all apps from women
p_G2 <- link(m_dir,data=list(
  discipline=rep(1:9,times=apps_per_disc),
  applications=rep(1,total_apps),
  gid=rep(2,total_apps)))

# no clear difference after accounting for department differences
dens( p_G2 - p_G1 , lwd=4 , col=2 , xlab="effect of gender perception" )
abline(v=0,lty=3)


# 3) ====

# show each discipline density with weight as in population
w <- xtabs( d$applications ~ d$discipline ) / sum(d$applications)
w <- w/max(w)
plot(NULL,xlim=c(-0.5,0.5),ylim=c(0,25),xlab="Gender contrast (probability)",ylab="Density")
for ( i in 1:9 ) dens( diff_prob_d[,i] , lwd=2+8*w[i]^3 , col=i, add = T)
abline(v=0,lty=3)

# For most desciplines women are as likely or even more likely to land an
# award IF they apply (but they apply less than men)

# In earth social and medical sciences who are among the 4 disciplines with the 
# largest number of applications, and also the desciplines with the least pronounced 
# differences in number of applications between genders
# they are less likely to receive a grant than men
# so these disciplines are driving the total effect of gender on awards


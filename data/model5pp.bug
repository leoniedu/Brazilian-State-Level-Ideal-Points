model {
  scale <- max(sd(pi),.000001)
  shift <- mean(pi)
  sdp <- 100
  for (i in 1:n.rows) {
        yb[i] ~ dnorm(xb[i], tau.y)
   	logit(xb[i]) <- b0[legis[i]] + b1[legis[i]] * pi[party.j[i]]
  }
  sigma.y ~ dunif(0,sdp)
  tau.y <- pow(sigma.y,-2)
  for (j in 1:n.party) {
    pi[j] ~ dnorm(0, pow(sdp, -2))
    pi.pp[j] <- (pi[j]-shift)/scale
  } 	     
  for (i in 1:n.legis) {
    mu.lr[i] <- (lrclassb[i]-b0[i])/b1[i]
    mu.lrclassb[i] <- b0[i] + b1[i] * pi[party.i[i]]
    logit(lrclassb[i]) ~ dnorm(mu.lrclassb[i], tau.lrclass)
    b0[i] ~ dnorm(mu.0, tau.0)
    b1[i] ~ dnorm(mu.1, tau.1)
  }
  sigma.0 ~ dunif(0, sdp)
  tau.0 <- pow(sigma.0,-2)
  sigma.lrclass ~ dunif(0, sdp)
  tau.lrclass <- pow(sigma.lrclass,-2)
  sigma.1 ~ dunif(0, sdp)
  tau.1 <- pow(sigma.1,-2)
  mu.0 ~ dnorm(0, pow(sdp, -2))
  mu.1 ~ dunif(0, sdp)
}


library(reshape)
load("../data/dall.RData")

## zucco power published estimates
year <- 2009
year.pz <- ifelse (year>2005, 2005, year)
ploc <- read.csv("../data/party.estimates.csv")
ploc <- ploc[, c("X", paste("party.estimates.", year.pz, sep=''),
                 ##paste("within.survey.ses.", year, sep=''))]
                 paste("within.survey.ses.", year.pz, sep=''))]
names(ploc) <- c("party","xparty","xpartysd")
ploc <- na.omit(ploc)
ploc$party <- recode.party.pz(tolower(ploc$party))



## zucco model with random effects
model.bugs <- function() {
    sdp <- 10
    for (i in 1:n.rows) {
        y[i] ~ dnorm(xb[i], tau.y)
   	xb[i] <- b0[legis[i]] + b1[legis[i]] * party[party.i[i]]
    }
    tau.y ~ dgamma(.01, .01)
    sigma.y <- pow(tau.y, -2)
    for (j in 1:n.party) {
        ##party[j] ~ dnorm(0, pow(sdp, -2))
        party[j] ~ dunif(1, 10)
    }
    for (j in 1:n.legis) {
        b0[j] ~ dnorm(mu.0, tau.0)
        b1[j] ~ dnorm(mu.1, tau.1)
    }
    sigma.0 ~ dunif(0, sdp)
    tau.0 <- pow(sigma.0,-2)
    sigma.1 ~ dunif(0, sdp)
    tau.1 <- pow(sigma.1,-2)
    mu.0 ~ dnorm(0, pow(sdp, -2))
    mu.1 ~ dunif(0, sdp)
}



## zucco model with random effects
## + self placement
model.bugs.self <- function() {
    sdp <- 10
    for (i in 1:n.rows) {
        y[i] ~ dnorm(xb[i], tau.y)
   	xb[i] <- b0[legis[i]] + b1[legis[i]] * party[party.i[i]]
    }
    for (i in 1:n.legis) {
        self[i] ~ dnorm(zb[i], tau.self)
   	zb[i] <- b0[i] + b1[i] * party[party.self[i]]
        self.p[i] <- (self[i]-b0[i])/b1[i]
    }
    for (j in 1:n.party) {
        ##party[j] ~ dnorm(0, pow(sdp, -2))
        party[j] ~ dunif(1, 10)
    }
    for (j in 1:n.legis) {
        b0[j] ~ dnorm(mu.0, tau.0)
        b1[j] ~ dnorm(mu.1, tau.1)
    }
    sigma.0 ~ dunif(0, sdp)
    tau.0 <- pow(sigma.0,-2)
    sigma.1 ~ dunif(0, sdp)
    tau.1 <- pow(sigma.1,-2)
    mu.0 ~ dnorm(0, pow(sdp, -2))
    mu.1 ~ dunif(0, sdp)
    tau.self ~ dgamma(.01, .01)
    sigma.self <- sqrt(1/tau.self)
    tau.y ~ dgamma(.01, .01)
    sigma.y <- sqrt(1/tau.y)
}


model.bugs.self.state <- function() {
    sdp <- 10
    for (i in 1:n.rows) {
        y[i] ~ dnorm(xb[i], tau.y)
   	xb[i] <- b0[legis[i]] + b1[legis[i]] * party[party.i[i]]
    }
    for (i in 1:n.legis) {
        self[i] ~ dnorm(zb[i], tau.self)
   	zb[i] <- b0[i] + b1[i] * party.state[party.self[i], state[i]]
        self.p[i] <- (self[i]-b0[i])/b1[i]
    }
    sigma.party ~ dunif(0, 10)
    tau.party <- pow(sigma.party, -2)
    for (i in 1:n.party) {
        party[i] ~ dunif(1, 10)
        for (j in 1:n.state) {
            party.state[i,j] ~ dnorm(party[i], tau.party)
        }
    }
    for (j in 1:n.legis) {
        b0[j] ~ dnorm(mu.0, tau.0)
        b1[j] ~ dnorm(mu.1, tau.1)
    }
    sigma.0 ~ dunif(0, sdp)
    tau.0 <- pow(sigma.0,-2)
    sigma.1 ~ dunif(0, sdp)
    tau.1 <- pow(sigma.1,-2)
    mu.0 ~ dunif(0, sdp)
    mu.1 ~ dunif(0, sdp)
    tau.self ~ dgamma(.01, .01)
    sigma.self <- sqrt(1/tau.self)
    tau.y ~ dgamma(.01, .01)
    sigma.y <- sqrt(1/tau.y)
}







lr <- dall
##lr <- subset(dall, .id==year)
lr <- subset(dall, .id>=1997)
nmin <- 0
tp <- sort(table(dall$party), dec=TRUE)
lp <- names(tp[tp>nmin])
ts <- sort(table(dall$state), dec=TRUE)
##ls <- names(ts)
ls <- names(ts[ts>nmin])
lr <- melt(lr,
           id.var=c(".id", "caseid", "party",  "state", "lrclass"),
           measure.var=grep("lr",names(lr), value=TRUE))
lr$year <- as.numeric(as.character(lr$.id))
## remove NAs (no party id or no party loc)
lr <- subset(lr,(!is.na(value))&(!is.na(party)))
lr <- subset(lr, variable!="lrclass")
lr$party.i.factor <- recode.party.pz(substr(lr$variable,3,100))
lr$party <- recode.party.pz(lr$party)

lr <- subset(lr, party%in%names(tp)[tp>nmin])
lr <- subset(lr, party.i.factor%in%lp)
lr <- subset(lr, state%in%ls)
## all parties
allp <- sort(unique(c(as.character(lr$party.i.factor),
                      as.character(lr$party))))
## party measured
lr$party.i.factor <- factor(lr$party.i.factor, levels=allp)
lr$party.i <- as.numeric(lr$party.i.factor)
## party of the legislator
lr$party.self.factor <- factor(lr$party, levels=allp)
lr$party.self <- as.numeric(lr$party.self.factor)
lr$legis.factor <- with(lr, factor(paste(.id,caseid,sep=";")))
lr$legis <- as.numeric(lr$legis.factor)
lrself <- unique(subset(lr, select=-c(variable,value,party.i.factor,party.i)))
lrself <- lrself[order(lrself$legis),]
lrself <- reshape::rename(lrself, c(lrclass="self"))
lrself$state.factor <- factor(lrself$state)
lrself$state <- as.numeric(lrself$state.factor)



n.rows <- nrow(lr)
y <- lr$value
legis <- lr$legis
party.i <- lr$party.i
n.legis <- max(legis)
n.party <- length(allp)
self <- lrself$self
party.self <- lrself$party.self
state <- lrself$state
n.state <- max(state)
parties <- 1:(n.party+1)

    
inits <- function(){
    b0 <- runif(n.legis)
    b1 <- runif(n.legis)
    ##list(b0=b0, b1=b1, sigma.0=runif(1), sigma.1=runif(1))
    list(b0=b0)
}


## missing data on lrclass. have to predict outside bugs
jags.data <- Hmisc::llist(y, party.i, legis, n.rows, n.party, n.legis
                          , self, party.self, state, n.state,
                          parties
                          )





library(R2WinBUGS)
library(rjags)
source("../utils.R")
write.model(model.bugs,con="model.bug")
write.model(model.bugs.self,con="model.self.bug")
write.model(model.bugs.self.state,con="model.self.state.bug")

parameters.to.save <- c("b0","b1", "sigma.y", "sigma.0", "sigma.1", "party", "mu.0", "mu.1")
parameters.to.save.self <- c(parameters.to.save, "self.p", "sigma.self")
parameters.to.save.self.state <- c(parameters.to.save.self, "party.state", "sigma.party")



system.time(jags.self.state <- jags.model(file="model.self.state.bug",data=jags.data, inits=inits, n.adapt=5000,n.chains=2))
system.time(jags.self.state.s <- coda.samples(jags.self.state, variable.names=parameters.to.save.self.state, n.iter=5000 , thin=2))





system.time(jags.self <- jags.model(file="model.self.bug",data=jags.data, inits=inits, n.adapt=1000,n.chains=2))
system.time(jags.self.s <- coda.samples(jags.self,variable.names=parameters.to.save.self, n.iter=1000 , thin=1))






system.time(jags1 <- jags.model(file="model.bug",data=jags.data, inits=inits, n.adapt=1000,n.chains=2))
system.time(jags1.s <- coda.samples(jags1,variable.names=parameters.to.save, n.iter=1000 , thin=1))



bugs <- coda2bugs(jags1.s)

bugs.self <- coda2bugs(jags.self.state.s)





res <- data.frame(party=levels(lr$party.i.factor),
                  mu.party=bugs$mean$party,
                  sd.party=bugs$sd$party
                  ,mu.party.self=bugs.self$mean$party,
                  sd.party.self=bugs.self$sd$party                  
                  )
res <- merge(res,ploc, all=TRUE)




res.i <- data.frame(lrself
                    ,b0=bugs$mean$b0
                    ,b1=bugs$mean$b1
                    ,b0.self=bugs.self$mean$b0
                    ,b1.self=bugs.self$mean$b1
                    ,b0.se=bugs$sd$b0
                    ,b1.se=bugs$sd$b1
                    ,b0.se.self=bugs.self$sd$b0
                    ,b1.se.self=bugs.self$sd$b1
                    ,self.p=bugs.self$mean$self.p
                    )
res.i <- merge(res.i, res)






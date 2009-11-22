library(ggplot2)
library(Hmisc)
library(foreign)

normalize.df <- function(x) {
    data.frame(lapply(x, function(z) if (is.character(z)|is.factor(z)) {
        tolower(z)
    } else {
        z
    }))
}



years <- c(1990, 1993, 1997, 2001, 2005)
dnow <- lapply(years, function(i) {
  res <- read.spss(paste(i, "-survey-weighted.sav", sep="")
                   , to.data.frame=TRUE)
  names(res) <- tolower(names(res))
  res
})
years <- c(years, 2009)
dnow <- c(dnow, list(read.dta("plio139-23aug.dta")))
dnow[[1]] <- within(dnow[[1]], {
    resp <- ave(ppweight, pp90, FUN=length)
    seats <- ((ppweight*resp)/length(ppweight))*570
    rs <- resp/seats
})
dnow[[2]] <- within(dnow[[2]], {
    resp <- ave(ppweight, pp93, FUN=length)
    seats <- ((ppweight*resp)/length(ppweight))*584
    rs <- resp/seats
})
dnow[[3]] <- within(dnow[[3]], {
    resp <- ave(ppweight, pp97, FUN=length)
  seats <- ((ppweight*resp)/length(ppweight))*594
    rs <- resp/seats
})
dnow[[4]] <- within(dnow[[4]], {
    resp <- ave(ppweight, pp2001, FUN=length)
    seats <- ((ppweight*resp)/length(ppweight))*594
    rs <- resp/seats
})
dnow[[5]] <- within(dnow[[5]], {
    resp <- ave(ppweight, pp2005, FUN=length)
    seats <- ((ppweight*resp)/length(ppweight))*594
    rs <- resp/seats
})
dnow[[6]] <- within(dnow[[6]], {
    resp <- ave(pweight, pp2009, FUN=length)
    seats <- ((pweight*resp)/length(pweight))*594
    rs <- resp/seats
})
names(dnow) <- years
dnow <- lapply(dnow,
               function(x) {
                   if (!"ppvsreg"%in%names(x)) {
                       x$ppvsreg <- NA
                   }
                   x$ppvsreg <- tolower(x$ppvsreg)
                   x
               }
               )

dnow[[1]]$party <- dnow[[1]]$pp90
dnow[[2]]$party <- dnow[[2]]$pp93
dnow[[3]]$party <- dnow[[3]]$pp97
dnow[[4]]$party <- dnow[[4]]$pp2001
dnow[[5]]$party <- dnow[[5]]$pp2005
dnow[[6]]$party <- dnow[[6]]$pp2009


library(plyr)
dall <- ldply(dnow,
              function(x) {
                  res <- data.frame(x[,c("caseid", "party", "ppvsreg", "resp", "seats", "rs", "state",grep("^lr.*", names(x), value=TRUE))])
                  res
              }
              )
dall <- normalize.df(dall)
dall$lrdiap <- NULL



recode.party <- function(x) car::recode(x, "'dem'='pfl'; c('pcdb','pc do b')='pcdob'; c('pp','ppr-pds-pdc','ppb-ppr','ppb','pds')='pp'")

dall$partyrec <- recode.party(dall$party)




dall$localism <- factor(dall$ppvsreg, levels=c("votes party interests",
                                      "splits party and local interests",
                                      "votes local interests"))

save(dall, file="dall.RData")

lr <- dall
##lr <- subset(dall, .id==2009)
lr$year <- as.numeric(as.character(lr$.id))
lr$.id <- NULL
mv <- grep("lr",names(lr), value=TRUE)
mv <- mv[!mv%in%"lrclass"]
lr <- melt(lr,
           id.var=c("year", "caseid", "party",  "state", "lrclass"),
           measure.var=mv)
lr <- subset(lr,(!is.na(value)))
## party of the legislator
lr$party.i.factor <- factor(recode.party(lr$party))
lr$party.i.factor <- relevel(lr$party.i.factor, "pfl")#2
lr$party.i.factor <- relevel(lr$party.i.factor, "pt")#1
## party measured
lr$party.j.factor <- factor(recode.party(substr(lr$variable,3,100)), levels=levels(lr$party.i.factor))
lr$party.i <- as.numeric(lr$party.i.factor)
lr$party.j <- as.numeric(lr$party.j.factor)
lr1 <- subset(lr, (party.i.factor==party.j.factor) & (!is.na(lrclass)), select=-c(party.j,party.j.factor, variable))


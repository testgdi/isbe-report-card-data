source("rep-card-util.R")

layout.d <- make.data.layout( data.dir="datadir2014/",
                             layout.fn="rc14-layout-2014.txt")

## what file

file.lookup.d <- file.column.lookup( file.lookup.fn="file-lookup.txt")

## assemble the fields into a data frame

low.inc.d <- fld.name.2.df(fld.name="LOW-INCOME SCHOOL %",
                           colname="low.inc.pct")

psae.2014.d <- fld.name.2.df(fld.name="2014 SCHOOL PERCENT FOR MEETS & EXCEEDS IN READING",
                             colname="psae.2014",                             
                             fac1="PSAE")

sch.name.d <- fld.name.2.df(fld.name="SCHOOL NAME",
                            colname="sch.name")

hs.d <- fld.name.2.df(fld.name="SCHOOL TYPE CODE",
                      colname="hs")


## merge the data, then filter on high schools
m <- merge(low.inc.d$df, sch.name.d$df)
m <- merge(m, psae.2014.d$df)
m <- merge(m, hs.d$df)
m <- m[ m$hs == 0,]

## plot school level data

## get indices for the proviso schools
proviso.idx <- grep( "Proviso", m$sch.name)

## x11()
png("proviso-reading-2014-psae-vs-low-income.png")
plot( m$low.inc.pct, m$psae.2014, col="gray",
     ylab="% meet and exceeds in reading",
     xlab="low income school %",
     main="psae vs low income")

points(m[proviso.idx,"low.inc.pct"], m[proviso.idx,"psae.2014"],col=1)

## make a faint grid
abline(h=seq(0,100,10), lty="dotted", col="gray")
abline(v=seq(0,100,10), lty="dotted", col="gray")

## do some regressions so we can add a
## regression line to the plot
library(MASS) ## need this for rlm

rownames(m) <- m$id
lm.out <- lm( psae.2014 ~ low.inc.pct, data=m)
rlm.out <- rlm( psae.2014 ~ low.inc.pct, data=m)
abline( lm.out, col="red")
## abline( rlm.out, col="blue")

dev.off()

## sort on residuals
resids <- as.numeric( lm.out$residuals)
names( resids) <- as.character(m[names(lm.out$residuals), "sch.name"])

## best corrected performance
sort.out <- sort( resids, decreasing=T)
head( names(sort.out),20)


## worst corrected
sort.out <- sort( resids)
head( names(sort.out),30)



### district level

dist.name.d <- fld.name.2.df(fld.name="DISTRICT NAME",
                            colname="dist.name")

low.inc.dist.d <- fld.name.2.df(fld.name="LOW-INCOME DISTRICT %",
                           colname="low.inc.dist.pct")

dist.psae.2014.d <- fld.name.2.df(fld.name="2014 DISTRICT PERCENT FOR MEETS & EXCEEDS IN READING",
                             colname="dist.psae.2014",                             
                             fac1="PSAE")

dist.m <- merge(low.inc.dist.d$df, dist.name.d$df)
dist.m <- merge(dist.m, dist.psae.2014.d$df)
dist.m <- merge(dist.m, hs.d$df)
dist.m <- dist.m[ dist.m$hs == 0,]

u.dist.m <- unique(dist.m[,c(2,3,4)])

png("district-proviso-reading-2014-psae-vs-low-income.png")
plot( u.dist.m$low.inc.dist.pct, u.dist.m$dist.psae.2014, col="gray",
     ylab="% meet and exceeds in reading",
     xlab="low income district %",
     main="psae vs low income: district")

lm.out <- lm( dist.psae.2014 ~ low.inc.dist.pct, data=u.dist.m)
rlm.out <- rlm( dist.psae.2014 ~ low.inc.dist.pct, data=u.dist.m)
abline( lm.out, col="red")

proviso.idx <- grep( "Proviso", u.dist.m$dist.name)
points( u.dist.m[ proviso.idx,"low.inc.dist.pct"],
        u.dist.m[ proviso.idx,"dist.psae.2014"])
abline(h=seq(0,100,10), lty="dotted", col="gray")
abline(v=seq(0,100,10), lty="dotted", col="gray")
dev.off()

# district residuals
## sort on residuals
resids <- as.numeric( lm.out$residuals)
names( resids) <- as.character(u.dist.m[names(lm.out$residuals), "dist.name"])

## best corrected performance
sort.out <- sort( resids, decreasing=T)
head( names(sort.out),20)


## worst corrected
sort.out <- sort( resids)
head( names(sort.out),30)

## sort.out[45:50]
## Brooklyn UD 188                   East Peoria CHSD 309              
##                         -12.00143                         -11.55851 
## Proviso Twp HSD 209               Mulberry Grove CUSD 1             
##                         -11.55735                         -11.40823 
## Ridgewood CHSD 234                Sangamon Valley CUSD 9            
##                         -11.26181                         -11.26022 
## 
## > length( sort.out)
## [1] 476
## >
## 
## 47/476 = .098, i.e. bottom 10%


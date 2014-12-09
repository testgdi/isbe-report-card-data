source("rep-card-util.R")

## setup where the data reside
layout.d <- make.data.layout( data.dir="data2014/",
                             layout.fn="rc14-layout-2014.txt")

file.lookup.d <- file.column.lookup( file.lookup.fn="file-lookup.txt")

## assemble the fields into a data frame
## we want variables for:
##    low income
##    psae 2014 performance
##    school name
##    school type so we can filter on high schools
##    eva per student -- this format is '$123,456' so need to convert string to number
##    pupil-teacher ratio
##    % classes not taught by highly qualified teachers
##    retention rate
##
##    if a "(" appears in the field name, it has to be escaped for the lookup

## lets look at operational spending - instruction
## that is overhead

low.inc.d <- fld.name.2.df(fld.name="LOW-INCOME SCHOOL %",
                           colname="low.inc.pct")

psae.2014.d <- fld.name.2.df(fld.name="2014 SCHOOL PERCENT FOR MEETS & EXCEEDS IN READING",
                             colname="psae.2014",                             
                             fac1="PSAE")

sch.name.d <- fld.name.2.df(fld.name="SCHOOL NAME",
                            colname="sch.name")

hs.d <- fld.name.2.df(fld.name="SCHOOL TYPE CODE",
                      colname="hs")

eav.d <- fld.name.2.df(fld.name="EAV PER PUPIL  DISTRICT",
                       colname="eav")
## > head( eav.d$df)
##                id        eav
## 1 010010010260001    $84,679
## 2 010010010262002    $84,679
eav.d$df$eav <- as.numeric(gsub( ",", "",gsub("\\$","",as.character(eav.d$df$eav))))


pt.ratio.d <- fld.name.2.df(fld.name="PUPIL-TEACHER RATIO -HS DISTRICT",
                             colname="pt.ratio")

not.high.d <- fld.name.2.df(fld.name="% CLASSES NOT TAUGHT BY HIGHLY QUALIFIED TEACHERS\\(SCHOOL",
                            colname="not.high")

ret.rate.d <- fld.name.2.df(fld.name="TEACHER RETENTION RATE \\(SCHOOL",
                            colname="ret.rate")
                                                  
## merge the data, then filter on high schools
m <- merge(low.inc.d$df, sch.name.d$df)
m <- merge(m, psae.2014.d$df)
m <- merge(m, hs.d$df)
m <- merge(m, eav.d$df)
m <- merge(m, pt.ratio.d$df)
m <- merge(m, not.high.d$df)
m <- merge(m, ret.rate.d$df)
m <- m[ m$hs == 0,]

##lm.out <- lm( psae.2014 ~ low.inc.pct + eav + pt.ratio + not.high + ret.rate, data=m)
lm.out <- lm( psae.2014 ~  eav + pt.ratio, data=m)
summary(lm.out)

lm.out <- lm( psae.2014 ~  low.inc.pct + pt.ratio, data=m)
summary(lm.out)

lm.out <- lm( psae.2014 ~  low.inc.pct + not.high, data=m)
summary(lm.out)

lm.out <- lm( psae.2014 ~  low.inc.pct + ret.rate, data=m)
summary(lm.out)


proviso.idx <- grep( "Proviso", m$sch.name)




### district level

dist.name.d <- fld.name.2.df(fld.name="DISTRICT NAME",
                            colname="dist.name")

low.inc.dist.d <- fld.name.2.df(fld.name="LOW-INCOME DISTRICT %",
                           colname="low.inc.dist.pct")

dist.psae.2014.d <- fld.name.2.df(fld.name="2014 DISTRICT PERCENT FOR MEETS & EXCEEDS IN READING",
                             colname="dist.psae.2014",                             
                             fac1="PSAE")

dist.not.high.d <- fld.name.2.df(fld.name="% CLASSES NOT TAUGHT BY HIGHLY QUALIFIED TEACHERS\\(DISTRICT",
                            colname="not.high")

dist.ret.rate.d <- fld.name.2.df(fld.name="TEACHER RETENTION RATE \\(DISTRICT",
                            colname="ret.rate")

dist.inst.expend.d <- fld.name.2.df(fld.name="INSTRUCT EXPEND PER PUPIL  DISTRICT",
                            colname="inst.spend")

dist.inst.expend.d$df$inst.spend <-
  as.numeric(gsub( ",", "",gsub("\\$","",as.character(dist.inst.expend.d$df$inst.spend))))

dist.op.expend.d <- fld.name.2.df(fld.name="OPER EXPEND PP - DISTRICT",
                            colname="op.spend")

dist.op.expend.d$df$op.spend <-
  as.numeric(gsub( ",", "",gsub("\\$","",as.character(dist.op.expend.d$df$op.spend))))

dist.enroll.d <- fld.name.2.df(fld.name="DISTRICT TOTAL ENROLLMENT",
                            colname="enrollment")
dist.enroll.d$df$enrollment <-
  as.numeric(gsub( ",", "",gsub("\\$","",as.character(dist.enroll.d$df$enrollment))))

dist.county.d <- fld.name.2.df(fld.name="COUNTY",
                            colname="county")
dist.county.d$df$county <- trim( as.character( dist.county.d$df$county))

dist.m <- merge(low.inc.dist.d$df, dist.name.d$df)
dist.m <- merge(dist.m, dist.county.d$df)
dist.m <- merge(dist.m, dist.psae.2014.d$df)
dist.m <- merge(dist.m, hs.d$df)
dist.m <- merge(dist.m, pt.ratio.d$df)
dist.m <- merge(dist.m, dist.not.high.d$df)
dist.m <- merge(dist.m, dist.ret.rate.d$df)
dist.m <- merge(dist.m, dist.inst.expend.d$df)
dist.m <- merge(dist.m, dist.op.expend.d$df)
dist.m <- merge(dist.m, dist.enroll.d$df)
dist.m <- dist.m[ dist.m$hs == 0,]

u.dist.m <- unique(dist.m[,2:dim(dist.m)[2]])

# some districts are dupped because of missing data
# Chicago, New Trier, J S Morton. then 4 other dist with NA
# complete cases eliminates the dups, throws out 4 little dists

u.dist.m <- u.dist.m[ complete.cases( u.dist.m),]
rownames( u.dist.m) <- as.character(u.dist.m$dist.name)

u.dist.m$overhead <- u.dist.m$op.spend - u.dist.m$inst.spend

cor.order.me <- c("dist.psae.2014", "low.inc.dist.pct",
              "pt.ratio", "not.high", "ret.rate", "inst.spend", "op.spend",
                  "overhead", "enrollment")


round( cor( u.dist.m[, cor.order.me]), 2)

proviso.idx <- grep( "Proviso", u.dist.m$dist.name)

## raw overhead


## x11()
png("hist.overhead.png")
hist( u.dist.m$overhead, n=40,
     main="Overhead")
abline(v=u.dist.m$overhead[proviso.idx], col="red")
dev.off()

ecdf( u.dist.m$overhead)( u.dist.m$overhead[proviso.idx])


## we might think that overhead, since it's linear in spending
## might scale with inversely % low income
## this turns out to be true for %low.inc below about 60%
## the result is the regressionline is flat, but loess
## shows expected rend at high income schools.

y <-  u.dist.m$overhead
x <-  u.dist.m$low.inc.dist.pct

##x11()
png("overhead.vs.low.inc.png")
plot( x, y, col="gray",
     ylab="overhead", xlab="% low inc")
points( x[proviso.idx], y[proviso.idx], col="red")
lo.out <- loess( y ~ x, span=.95)
pred.x <- seq(5,95)
pred.out <- predict( lo.out, newdata=data.frame( x=pred.x))
lines( pred.x, pred.out, col="blue")
dev.off()

## library( MASS)
### abline( rlm( y ~ x))

## So looking at residual overhead spending

##x11()
png("hist.overhead.vs.lowinc.resid.png")
hist( lo.out$residuals, n=40,
     main="Overhead Residuals [against % low inc]")
abline(v=lo.out$residuals[proviso.idx], col="red"
)

dev.off()

## does overhead have anything to do with psae
##x11()
png("psae.vs.oh.resid.png")
plot( lo.out$residuals, u.dist.m$dist.psae.2014, col="gray")
points( lo.out$residuals[proviso.idx],
       u.dist.m$dist.psae.2014[proviso.idx], col="red")

dev.off()
## to be fair, should look at residual of psae against pct low inc

##x11()
png("psae.vs.low.inc.png")
plot( u.dist.m$low.inc.dist.pct, u.dist.m$dist.psae.2014, col="gray",
     xlab="% low inc", ylab="psae.2014")
points( u.dist.m$low.inc.dist.pct[proviso.idx],
       u.dist.m$dist.psae.2014[proviso.idx],col="red")

x <- u.dist.m$low.inc.dist.pct
y <- u.dist.m$dist.psae.2014

lo.out.inc <- loess( y ~ x)
pred.x <- seq(5,95)
pred.out <- predict( lo.out.inc, newdata=data.frame( x=pred.x))
lines( pred.x, pred.out, col="blue")

dev.off()

##x11()
png("oh.resid.vs.psae.resid.png")
plot( lo.out$residuals, lo.out.inc$residuals, col="gray",
       xlab="overhead residual [against % low inc]",
       ylab="psae residual [against % low inc]")
     
points( lo.out$residuals[proviso.idx],
       lo.out.inc$residuals[proviso.idx], col="red")

dev.off()
## picture is about the same


# other financial oversight
east.stl.idx <- grep( "East St Louis", u.dist.m$dist.name)
north.ch.idx <- grep( "North Chicago", u.dist.m$dist.name)
cairo.idx <- grep( "Cairo", u.dist.m$dist.name)
round.lake.idx <- grep( "Round Lake", u.dist.m$dist.name)

others <- c(east.stl.idx, north.ch.idx, cairo.idx, round.lake.idx)

##x11()
png("other.oversigt.png")
plot( lo.out$residuals, lo.out.inc$residuals, col="gray",
       xlab="overhead residual [against % low inc]",
       ylab="psae residual [against % low inc]")
cols <- c("darkred","darkred","orange","orange")
points( lo.out$residuals[others],
       lo.out.inc$residuals[others], col=cols)

points( lo.out$residuals[proviso.idx],
       lo.out.inc$residuals[proviso.idx], col="red")

dev.off()

## total excess overhead
u.dist.m$tot.ex.oh <- lo.out$residuals * u.dist.m$enrollment
prov.tot.ex <- u.dist.m$tot.ex.oh[proviso.idx]
png("tot.ex.oh.png")
hist( u.dist.m$tot.ex.oh, n=100,
     main="total excess overhead",
     xlab="total excess overhead $")
dev.off()
png("tot.ex.oh.zoom.png")
hist( u.dist.m$tot.ex.oh, n=100, xlim=c(-20e6,20e6),
     main="total excess overhead",
     xlab="total excess overhead $")
abline(v=prov.tot.ex, col="red")
dev.off()

ecdf(u.dist.m$tot.ex.oh)( u.dist.m$tot.ex.oh[proviso.idx])
1-ecdf(u.dist.m$tot.ex.oh)( u.dist.m$tot.ex.oh[proviso.idx])

## raw total excess overhead

raw.exc.oh <- (u.dist.m$overhead - mean(u.dist.m$overhead ))*u.dist.m$enrollment

x11()
png("simple.total.exc.oh.png")
hist(raw.exc.oh,n=100)
abline(v=raw.exc.oh[proviso.idx], col="red")
dev.off()

ecdf(raw.exc.oh)( raw.exc.oh[proviso.idx])
1-ecdf(u.dist.m$tot.ex.oh)( u.dist.m$tot.ex.oh[proviso.idx])

so.out <- sort( raw.exc.oh, index.return=T)
tail( data.frame( dist=u.dist.m[so.out$ix,"dist.name"],
                 raw.exc.oh=raw.exc.oh[so.out$ix]),
     30)

png("simple.total.exc.oh.zoom.png")
hist(raw.exc.oh,n=100, xlim=c(-40e6,40e6))
abline(v=raw.exc.oh[proviso.idx], col="red")
dev.off()


x11()
plot( u.dist.m$low.inc.dist.pct, raw.exc.oh)

####
##
## take in col by county
##
####
col.d <- read.table( "data2014/county-col.csv", header=T, sep=";")
col.d$county <- trim( as.character( col.d$county))

m <- merge( u.dist.m, col.d) # , join="left")
prov.idx <- grep( "Proviso", m$dist.name)

plot( m$low.inc.dist.pct, m$raw.index, col="gray")
points( m$low.inc.dist.pct[prov.idx], m$raw.index[prov.idx], col="red")

## regress out low.inc.dist.pct and county norm
lm.out.2 <- lm( overhead ~ normalized.index.mean + low.inc.dist.pct, data=m)

hist( lm.out.2$residuals, n=100)
abline(v= lm.out.2$residuals[ prov.idx])

ecdf( lm.out.2$residuals)(lm.out.2$residuals[ prov.idx])

tot.oh.2 <- lm.out.2$residuals * m$enrollment
x11()
hist( tot.oh.2, n=200, xlim=c(-3e7,3e7))
abline(v= tot.oh.2[ prov.idx])
ecdf( tot.oh.2)(tot.oh.2[ prov.idx])
## tot.oh.2[ prov.idx]
##      56 
## $4,655,334 
## 

## loess
lo.out.2 <- loess( overhead ~ normalized.index.mean + low.inc.dist.pct, data=m)

prov.idx <- grep( "Proviso", m$dist.name)
hist( lo.out.2$residuals, n=100)
abline(v= lo.out.2$residuals[ prov.idx])

ecdf( lo.out.2$residuals)(lo.out.2$residuals[ prov.idx])

tot.oh.3 <- lo.out.2$residuals * m$enrollment
x11()
hist( tot.oh.3, n=200, xlim=c(-3e7,3e7))
abline(v= tot.oh.3[ prov.idx])
ecdf( tot.oh.3)(tot.oh.3[ prov.idx])
tot.oh.3[ prov.idx]
## > tot.oh.3[ prov.idx]
##      56 
## $6,407,673 

## restrict to Cook
## shows Proviso oh is unremarkable in Cook County
cook.bool <- m$county == "Cook"
m.cook <- m[cook.bool,]
png("overhead.vs.low.inc.cook.png")
plot( m.cook$low.inc.dist.pct, m.cook$overhead, col="gray")
points( m$low.inc.dist.pct[prov.idx], m$overhead[prov.idx], col="red")
abline(lm( m.cook$overhead ~ m.cook$low.inc.dist.pct), col="blue")
dev.off()


## county as dummy
library(MASS)
rlm.out.3 <- rlm( overhead ~ as.factor( county) + low.inc.dist.pct, data=m)
summary( rlm.out.3)

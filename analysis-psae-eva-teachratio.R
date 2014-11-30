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


dist.m <- merge(low.inc.dist.d$df, dist.name.d$df)
dist.m <- merge(dist.m, dist.psae.2014.d$df)
dist.m <- merge(dist.m, hs.d$df)
dist.m <- merge(dist.m, pt.ratio.d$df)
dist.m <- merge(dist.m, dist.not.high.d$df)
dist.m <- merge(dist.m, dist.ret.rate.d$df)
dist.m <- dist.m[ dist.m$hs == 0,]

u.dist.m <- unique(dist.m[,2:8])

# some districts are dupped because of missing data
# Chicago, New Trier, J S Morton. then 4 other dist with NA
# complete cases eliminates the dups, throws out 4 little dists

u.dist.m <- u.dist.m[ complete.cases( u.dist.m),]
rownames( u.dist.m) <- as.character(u.dist.m$dist.name)

cor.order.me <- c("dist.psae.2014", "low.inc.dist.pct",
              "pt.ratio", "not.high", "ret.rate")
round( cor( u.dist.m[, cor.order.me]), 2)

##
## Do a bunch of regressions, see which variables matter in combination
##

form <- "dist.psae.2014 ~ low.inc.dist.pct + pt.ratio + ret.rate + not.high"
lm.out <- lm( as.formula(form), data=u.dist.m)
summary( lm.out)

form <- "dist.psae.2014 ~ low.inc.dist.pct + ret.rate"
lm.out <- lm( as.formula(form), data=u.dist.m)
summary( lm.out)

## form <- "dist.psae.2014 ~ low.inc.dist.pct"
## lm.out <- lm( as.formula(form), data=u.dist.m)
## summary( lm.out)

## look at residuals


proviso.idx <- grep( "Proviso", u.dist.m$dist.name)


ret.lm.out <- lm( ret.rate ~ low.inc.dist.pct, data=u.dist.m)
head(as.data.frame(sort( ret.lm.out$residuals)),30)
ret.lm.out$residuals[ grep("Proviso", names( ret.lm.out$residuals))]

png("ret-rate-vs-low-income-dist.png")
plot(u.dist.m$low.inc.dist.pct, u.dist.m$ret.rate, col="gray",
     ylab="teacher retention",
     xlab="% low income",
     main="low income explains some teacher turnover [District]")
abline( ret.lm.out, col="red")
points(u.dist.m$low.inc.dist.pct[proviso.idx], u.dist.m$ret.rate[proviso.idx],col=1)
dev.off()

pt.lm.out <- lm( pt.ratio ~ low.inc.dist.pct, data=u.dist.m)
head(as.data.frame(sort( pt.lm.out$residuals)),30)
pt.lm.out$residuals[ grep("Proviso", names( pt.lm.out$residuals))]

png("pupil-teach-ratio-vs-low-income-dist.png")
plot(u.dist.m$low.inc.dist.pct, u.dist.m$pt.ratio, col="gray",
     ylab="pupil-teacher ratio",
     xlab="% low income",
     main="low income and pupil teacher ratio [District]")
abline( pt.lm.out, col="red")
points(u.dist.m$low.inc.dist.pct[proviso.idx], u.dist.m$pt.ratio[proviso.idx],col=1)
dev.off()

# district residuals
## sort on residuals
resids <- as.numeric( lm.out$residuals)
names( resids) <- as.character(u.dist.m[names(lm.out$residuals), "dist.name"])
resids[ grep("Proviso", names( resids))]
hist( resids, n=40)
abline( v=resids[ grep("Proviso", names( resids))], col="red")
ecdf( resids)(resids[ grep("Proviso", names( resids))])
## [1] 0.1374207


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


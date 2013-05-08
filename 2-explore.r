
## Medians
(RR.correct.median <- with(RR, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
(RR.eqweight.median <- summary(RR.equalwt.mean))
(RR.correct.ex.median <- with(RR, tapply( dRGDP, dgcat2, median, na.rm=TRUE )))
(RR.selective.spreadsheet.median <- with(RR.selective.spreadsheet, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
(RR.published.median <- apply(RR.selective.spreadsheet.transcription,2,median,na.rm=TRUE))



## Counts of years
with(RR, table(Country,dgcat))
apply(with(RR,table( Country,dgcat)),2,sum)

with(RR.selective,table( Country,dgcat))
apply(with(RR.selective,table( Country,dgcat)),2,sum)

with(RR.selective.spreadsheet,table( Country,dgcat))
apply(with(RR.selective.spreadsheet,table( Country,dgcat)),2,sum)


RR.newzealand.1951 <- subset(RR.selective.spreadsheet,Country=="New Zealand" & Year==1951)


## Categorical scatterplot
n <- ggplot(RR, aes(x=dgcat,y=dRGDP)) + geom_point(shape=3,color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category")
n <- n + geom_point(RR.published.mean.df, mapping=aes(x=dgcat,y=RR.published.mean), shape=5,  size=5 )
n <- n + geom_text(RR.published.mean.df, mapping=aes(x=dgcat,y=RR.published.mean,label=round(RR.published.mean,1)),hjust=-0.7,size=3,color='darkgray')
n <- n + geom_point(RR.correct.mean.df,  mapping=aes(x=dgcat,y=RR.correct.mean,label=RR.correct.mean), shape=16, size=4 )  + theme_bw()
n <- n + geom_text(RR.correct.mean.df,  mapping=aes(x=dgcat,y=RR.correct.mean,label=round(RR.correct.mean,1)), hjust=1.7,size=3,color='darkgray')
n <- n + geom_point(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP), shape=0, size=3 )
n <- n + geom_text(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP,label=paste(round(dRGDP,1))), hjust=-0.7,size=3,color='darkgray')
n <- n + geom_text(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP,label=paste("NZ",Year)), hjust=1.2,size=3,color='darkgray')
print(n)

## Create legend for categorical scatterplot
plot(3,10,pch=0,ylim=c(0,70),xlim=c(0,5.5))
text(3.2,10,"New Zealand 1951",adj=0)
points(0,15,pch=16)
text(0.2,15,"Correct average real GDP growth",adj=0)
points(0,10,pch=5,cex=1.5)
text(0.2,10,"RR average real GDP growth",adj=0)
points(3,15,pch=3,col='darkgray')
text(3.2,15,"Country-Year real GDP growth",adj=0)

## Expanded categories
o <- ggplot(RR, aes(x=dgcat2,y=dRGDP)) + geom_point(shape=3,color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category")
o <- o + geom_point(RR.correct.mean.2.df,  mapping=aes(x=dgcat,y=RR.correct.mean.2), shape=16, size=4 )  + theme_bw()
o <- o + geom_text(RR.correct.mean.2.df, mapping=aes(x=dgcat,y=RR.correct.mean.2,label=round(RR.correct.mean.2,1)), hjust=1.7, size=3,color='darkgray')
print(o)

## Scatterplot
library(mgcv)
RR.gam <- gam(dRGDP ~ s(debtgdp, bs="cs"),data=RR)

## Cross-validation technique for loess parameters
## http://stats.stackexchange.com/questions/2002/how-do-i-decide-what-span-to-use-in-loess-regression-in-r
m <- ggplot(RR, aes(x=debtgdp,y=dRGDP))
m1 <- m + geom_vline(xintercept=90,color='lightgray',size=1.5)
m1 <- m1 + geom_point(color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Ratio") + scale_x_continuous(breaks=seq(0,240,30)) + theme_bw()
## m1 <- m1 + geom_smooth(method='loess',span=1.0,color='black') + geom_smooth(method='loess',span=0.2,color='black')
m1 <- m1 + geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs"))
## m1 <- m1 + geom_smooth(method='auto', color='black')
print(m1)
## Scatterplot closeup
pdf("closeup.pdf",height=4,width=7)
m2 <- m + geom_point(color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Ratio") + scale_x_continuous(breaks=seq(0,240,30)) + theme_bw() +  geom_vline(xintercept=90,color='lightgray',size=1.5)
## m2 <- m2 + geom_smooth(method='loess',span=0.75,color='black') + geom_smooth(method='loess',span=0.4,color='black')
## m2 <- m2 + geom_smooth(method='auto',color='black')
m2 <- m2 + geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs"))
m2 <- m2 + coord_cartesian(ylim=c(0, 7),xlim=c(0,150)) + scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7)) + theme_bw()
print(m2)

subset(ggplot_build(m1)$data[[3]],  (ymin<3.1 & ymin>2.9) | (ymax<3.1 & ymax>2.9 ))

subset(RR,
       Country %in% c("Australia","Belgium","Canada","Greece","Ireland","Italy","Japan","New Zealand","UK","US"),
       select=c(Country,Year,dgcat,debtgdp,dRGDP))

subset(RR,
       debtgdp>90,
       select=c(Country,Year,dgcat,debtgdp,dRGDP))




## p <- ggplot(RR, aes(x=Year,y=debtgdp,color=Country)) + geom_point() +  facet_grid(. ~ Country) + opts(legend.position="bottom")
## print(p)

## Country-Year average by debtgdp for more recent samples
with(subset(RR, Year>=1950), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
with(subset(RR, Year>=1960), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
with(subset(RR, Year>=1970), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
with(subset(RR, Year>=1980), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
with(subset(RR, Year>=1990), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
with(subset(RR, Year>=2000), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

with(subset(RR, Year>=1950 & Year<1980), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))
with(subset(RR, Year>=1980), tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

subset(RR,dRGDP>10,select=c(Country,Year,dRGDP,dgcat,debtgdp))
subset(RR,dRGDP< -7,select=c(Country,Year,dRGDP,dgcat,debtgdp))



library(plyr)
library(ggplot2)
library(car)
options(scipen = 10000, digits = 4)

RR <- read.csv("RR-processed.csv")
count(RR, "Year")
count(RR, "Country")

# Regression analysis ----------------------------------------------------------
RR$dgcat.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,Inf))
RR$dgcat <- factor(RR$dgcat.lm, labels = c("0-30%","30-60%","60-90%","Above 90%"),ordered=TRUE)

RR$dgcat2.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,120,Inf))
RR$dgcat2 <- factor(RR$dgcat2.lm, labels = c("0-30%","30-60%","60-90%","90-120%","Above 120%"),ordered=TRUE)

base.lm <- lm(dRGDP ~ 1, data = RR)
summary(dgcat.lm <- lm(dRGDP ~ dgcat.lm, data=RR))
anova(base.lm, dgcat.lm)

summary(dgcat2.lm <- lm(dRGDP ~ dgcat2.lm, data=RR))
anova(base.lm, dgcat2.lm)
linearHypothesis(dgcat2.lm, c(
  "dgcat2.lm(30,60]=dgcat2.lm(60,90]",
  "dgcat2.lm(30,60]=dgcat2.lm(90,120]",
  "dgcat2.lm(30,60]=dgcat2.lm(120,Inf]"))
linearHypothesis(dgcat2.lm, c(
  "dgcat2.lm(30,60]=dgcat2.lm(60,90]",
  "dgcat2.lm(30,60]=dgcat2.lm(90,120]"))

# Country-Year average by debtgdp ("correct weights") --------------------------

compare_means <- function(df, by) {
  overall <- ddply(df, by, summarise,
    mean = mean(dRGDP),
    n = length(dRGDP))

  ddply(overall, by[1], summarise,
    correct = weighted.mean(mean, n),
    equal_wt = mean(mean))
}

# Base categories
compare_means(RR, c("dgcat", "Country"))
# Expanded categories
compare_means(RR, c("dgcat2", "Country"))

# Selective treatment of early years -------------------------------------------
RR.selective <- subset(RR, !(
  (Year < 1950 & Country == "New Zealand") |
  (Year < 1951 & Country == "Australia") |
  (Year < 1951 & Country == "Canada")))

compare_means(RR.selective, c("dgcat", "Country"))

# And dropping because of spreadsheet error ------------------------------------

RR.selective.spreadsheet <- subset(RR.selective,
  !Country %in% c("Australia", "Austria", "Belgium", "Canada", "Denmark"))
compare_means(RR.selective.spreadsheet, c("dgcat", "Country"))

## And New Zealand transcription error
## selective.spreadsheet.transcription <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE ))
RR.selective.spreadsheet.transcription["New Zealand",4] <- -7.9
summary(RR.selective.spreadsheet.transcription)
## Table 3 Weights,Exclusion,Spreadsheet Error,Transcription
(RR.published.mean <- apply(RR.selective.spreadsheet.transcription,2,mean,na.rm=TRUE))
RR.published.mean.df <- data.frame(RR.published.mean , dgcat=names(RR.published.mean) )


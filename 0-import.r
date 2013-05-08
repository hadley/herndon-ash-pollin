library(plyr)

csvs <- dir("RR-country-csv", full.name = TRUE)
all <- ldply(csvs, read.csv, stringsAsFactors = FALSE)

count(all, "Country")
write.csv(all, "RR-basic.csv", row.names = FALSE)

# Pick gdp value to use for debt ratio calculation -----------------------------
# It's not obvious what rule governs which of GDP1, GDP2 and GNI to use
# It looks like use GDP2 where available, otherwise falling back to GDP1.
# I don't see how GNI fits in, given that it has very different units.
gdp <- ifelse(!is.na(all$GDP2), all$GDP2, all$GDP1)
all$debtgdp <- 100 * all$Debt / gdp

# Compute lagged gdps
lg <- function(x) c(NA, x[-length(x)])
all <- ddply(all, "Country", mutate,
  lRGDP = lg(RGDP),
  lRGDP1 = lg(RGDP1),
  lRGDP2 = lg(RGDP2)
)

# Fill in missing values of dRGDP ----------------------------------------------
# Compute dRGDP, trying first RGDP, then RGDP2, then RGDP1
dRGDP_poss <- with(all, cbind(
  dRGDP,
  RGDP / lRGDP - 1,
  RGDP2 / lRGDP2 - 1,
  RGDP1 / lRGDP1 - 1
))
colSums(is.na(dRGDP_poss))

first_not_missing <- function(x) x[!is.na(x)][1]
all$dRGDP <- apply(dRGDP_poss, 1, first_not_missing)

# Final tweaking ---------------------------------------------------------------

all <- subset(all, Year >= 1946 & Year <= 2009 & !is.na(dRGDP) & !is.na(debtgdp))
# Italy has another data series through 1946 and is excluded from GITD until 1951
all <- subset(all, !(Year < 1951 & Country == "Italy"))

# Remove columns that only consist of missing values
col_na <- sapply(all, function(x) all(is.na(x)))
all <- all[!col_na]

write.csv(all, "RR-processed.csv", row.names = FALSE)

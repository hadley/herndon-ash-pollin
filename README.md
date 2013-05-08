This repository contains a rewrite of the R code used by Thomas Herndon, Michael Ash and Robert Pollin from their paper [Does High Public Debt Consistently Stifle Economic Growth? A Critique of Reinhart and Rogoff](http://www.peri.umass.edu/236/hash/31e2ff374b6377b2ddec04deaa6388b1/publication/566/
)

(I have no affiliation with the authors, I just thought it would be fun to play around with the data)

# Changes

* Greece had commas in the RGDP column for 1991-2006. Previouly `as.numeric` was used to get rid of them which just turned them into NAs

* It's unclear exactly how to pick between GDP, GDP1 and GDP2. I choose to use GDP2 where available, falling back to GDP1 where not available. This may be different from the original paper.


# Changes

* Greece had commas in the RGDP column for 1991-2006. Previouly `as.numeric` was used to get rid of them which just turned them into NAs

* It's unclear exactly how to pick between GDP, GDP1 and GDP2. I choose to use GDP2 where available, falling back to GDP1 where not available. This may be different from the original paper.


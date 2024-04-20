
library(epitools)

## 2x4
nfo <- c(874, 3669, 698, 7732)
pyears <- c(4750, 26569, 3081, 49454)

# dd <- matrix(c(1361, 322, 526, 2303, 22694, 2975, 2723, 38818), 4, 2)
# dimnames(dd) <- list(Exposure = c("in > 4", "in 1 - 4", "out 1-4", "out > 4"), Outcome = c("NFO", "PYears"))
# dd

rateratio.wald(nfo, pyears)


## 2x2
nfo <- c(236498, 539931) # control group goes first
pyears <- c(12423, 26518)

rateratio.wald(nfo, pyears)

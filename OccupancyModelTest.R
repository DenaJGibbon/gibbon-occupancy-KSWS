library(spOccupancy)

data(hbef2015)

str(hbef2015)

sp.names <- dimnames(hbef2015$y)[[1]]

btbwHBEF <- hbef2015

btbwHBEF$y <- btbwHBEF$y[sp.names == "BTBW", , ]

str(btbwHBEF)

# Specify model formulas
btbw.occ.formula <- ~ scale(Elevation) + I(scale(Elevation)^2)
btbw.det.formula <- ~ scale(day) + scale(tod) + I(scale(day)^2)

# Run the model
out <- spPGOcc(occ.formula = btbw.occ.formula,
               det.formula = btbw.det.formula,
               data = btbwHBEF, n.batch = 800, batch.length = 25,
               accept.rate = 0.43, cov.model = "exponential",
               NNGP = TRUE, n.neighbors = 5, n.burn = 8000,
               n.thin = 4, n.chains = 3, verbose = FALSE,
               k.fold = 2, k.fold.threads = 2)
summary(out)

ppc.out <- ppcOcc(out, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out)

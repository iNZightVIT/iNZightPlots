context("Post stratification weights")

data(api)
dclus1 <- svydesign(id = ~dnum, weights = ~pw, 
    data = apiclus1, fpc = ~fpc)
pop.types <- data.frame(stype = c("E","H","M"), Freq = c(4421,755,1018))
dclus1p <- postStratify(dclus1, ~stype, pop.types)

load_all()
iNZightPlot(api00, api99, design = dclus1)
iNZightPlot(api00, api99, design = dclus1p)

eval(dclus1p$call[[2]])
source("dcov_screening.R")

dat = read.csv("GSS2018_clean.csv", header = T)
GETAHEAD = as.factor(dat$GETAHEAD)
dat = dat[,-which(colnames(dat) == "GETAHEAD")]
var.lst = colnames(dat)

cutpoint = dcov_screening(Y = GETAHEAD , X = dat, var.names = var.lst)[[1]]
cutpoint

identified.var.list = dcov_screening(Y = GETAHEAD , X = dat, var.names = var.lst)[[2]]
identified.var.list



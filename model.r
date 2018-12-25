library("openxlsx")
library('tseries')
library('xtable')
data <- read.xlsx('dataset.xlsx')
adf = adf.test(data$GDP)
pp= pp.test(data$GDP)
kpss=kpss.test(data$GDP)
funs = c(adf.test, pp.test, kpss.test) #tests to be mapped to columns
#how to vectorize this?
gdp = sapply(funs, function(fun,x) fun(x)$p.value, x=data$GDP)
pop = sapply(funs, function(fun,x) fun(x)$p.value, x=data$Population)
elec = sapply(funs, function(fun,x) fun(x)$p.value, x=data$Electricity.consumption)
unempl = sapply(funs, function(fun,x) fun(x)$p.value, x=data$Unemployment)
#put together latex table with results of the test
results = rbind(unempl, pop, elec, gdp)
colnames(results) <- c('ADF', 'PP', 'KPSS')
latex = xtable(results) #the latex unit roots table
#add logs to the dataset
logs = apply(data[, -1], 2, log)
colnames(logs) <- c('logUnempl', 'logPop', 'logElec', 'logGDP')
data=cbind(data, logs)


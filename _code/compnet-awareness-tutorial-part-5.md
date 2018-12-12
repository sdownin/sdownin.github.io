---
title: "Competition Network Analysis Tutorial - Part 5"
collection: code
permalink: /code/compnet-awareness-tutorial-part-5
excerpt: "Part 5: Analyzing Your Network with Updated Data   <br/><a href='/code/compnet-awareness-tutorial-part-5'><img src='/data/compnet-awareness-tutorial-part-5-thumbnail.png' style='max-height:150px; border:0.5px solid #d3d3d3'></a>"
---


#### Contents
- [Part 1: Analyzing Example Network Data Sample](/code/compnet-awareness-tutorial-part-1  "Part 1")
- [Part 2: Goodness of Fit, Degeneracy, Estimation Algorithm](/code/compnet-awareness-tutorial-part-2  "Part 2")
- [Part 3: Adding and Cleaning New data](/code/compnet-awareness-tutorial-part-3 "Part 3")
- [Part 4: Computing Period Networks and Covariate Lists](/code/compnet-awareness-tutorial-part-4  "Part 4")
- Part 5: Analyzing Your Network with Updated Data  <br>(You are here.) 


# Part 5: Analyzing Your Network with Updated Data


Finally, compute a new TERGM with the updated data from Owler and CrunchBase using a model that suits your particular hypotheses.  This may take a while to run (a few minutes to a couple hours)  depending upon:
- network size (number of nodes per network)
- number of periods (number of network panels)
- complexity of change statistics to compute for the predictors in the model 


```r
##================================
##
## Compute TERGM with New Data (Owler + CrunchBase)
##
##--------------------------------

## cache edge covariates list
mmc <- lapply(nets, function(net) net %n% 'mmc')
sim <- lapply(nets, function(net) net %n% 'similarity')

## Set model based upon hypotheses and controls
m2 <-   nets ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) + 
  memory(type = "stability", lag = 1) + 
  timecov(transform = function(t)t) + 
  nodematch("ipo_status", diff = F) + 
  nodecov("age") + absdiff("age") + 
  nodecov("genidx_multilevel") + 
  nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + 
  # edgecov(sim) +  
  #edgecov(mmc) + 
  cycle(3) #+ cycle(4)  
## need to add state_code or match regions between Owler & CrunchBase to include in model
## nodematch("state_code", diff = F)



## number of bootstrap replicates
R <- 100

## set pseudorandom number generator seed for reproducibility
set.seed(1111)

## number of periods in network list
nPeriods <- length(nets)

## estimate the TERGM with bootstrapped PMLE
fit2 <- btergm(m2, R=R, parallel = "multicore", ncpus = detectCores())
```

```
## Mean transformed timecov values:
##  t=1 t=2 t=3 t=4 t=5 t=6 t=7 t=8 t=9 t=10 t=11
##    1   2   3   4   5   6   7   8   9   10   11
## 
## Initial dimensions of the network and covariates:
##                t=2 t=3 t=4 t=5 t=6 t=7 t=8 t=9 t=10 t=11
## nets (row)     116 116 116 116 116 116 116 116  116  116
## nets (col)     116 116 116 116 116 116 116 116  116  116
## memory (row)   116 116 116 116 116 116 116 116  116  116
## memory (col)   116 116 116 116 116 116 116 116  116  116
## timecov1 (row) 116 116 116 116 116 116 116 116  116  116
## timecov1 (col) 116 116 116 116 116 116 116 116  116  116
## 
## All networks are conformable.
## 
## Dimensions of the network and covariates after adjustment:
##                t=2 t=3 t=4 t=5 t=6 t=7 t=8 t=9 t=10 t=11
## nets (row)     116 116 116 116 116 116 116 116  116  116
## nets (col)     116 116 116 116 116 116 116 116  116  116
## memory (row)   116 116 116 116 116 116 116 116  116  116
## memory (col)   116 116 116 116 116 116 116 116  116  116
## timecov1 (row) 116 116 116 116 116 116 116 116  116  116
## timecov1 (col) 116 116 116 116 116 116 116 116  116  116
## 
## Starting pseudolikelihood estimation with 100 bootstrapping replications using multicore forking on 4 cores...
## Done.
```

```r
print(screenreg(fit2, digits = 3))
```

```
## 
## ============================================
##                            Model 1          
## --------------------------------------------
## edges                          2.819        
##                            [ -4.481; 27.975]
## gwesp.fixed.0                  0.003        
##                            [-15.669;  0.191]
## gwdegree                      -1.696        
##                            [ -3.375;  4.314]
## edgecov.memory[[i]]            6.855 *      
##                            [  5.858; 43.118]
## edgecov.timecov1[[i]]         -0.320        
##                            [ -0.511;  1.125]
## nodematch.ipo_status           0.117        
##                            [ -1.050;  0.920]
## nodecov.age                   -0.008 *      
##                            [ -0.233; -0.001]
## absdiff.age                    0.003        
##                            [ -0.003;  0.236]
## nodecov.genidx_multilevel      0.587 *      
##                            [  0.310;  4.792]
## nodecov.cent_pow_n0_4         -0.022        
##                            [ -0.058;  0.677]
## absdiff.cent_pow_n0_4          0.355 *      
##                            [  0.231;  1.299]
## cycle3                         0.982        
##                            [ -0.050; 13.240]
## --------------------------------------------
## Num. obs.                  35642            
## ============================================
## * 0 outside the confidence interval
```

```r
## SAVE SERIALIZED DATA
fit2_file <- file.path(data_dir,sprintf('fit_%s_pd%s_R%s_%s.rds', focal_firm, nPeriods, R, 'm2'))
saveRDS(fit2, file=fit2_file)
```

This completes the tutorial.

> [Back to Contents](#contents  "Back")


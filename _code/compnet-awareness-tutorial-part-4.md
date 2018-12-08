---
title: "Competition Network Analysis Tutorial - Part 4"
collection: code
permalink: /code/compnet-awareness-tutorial-part-4
excerpt: "Part 4: Computing Period Networks and Covariate Lists   <br/><img src='/data/compnet-awareness-tutorial-part-3-thumbnail.png' height='200'>"
---


#### Contents
- [Part 1: Analyzing Example Network Data Sample](#part-1-analyzing-example-network-data-sample  "Part 1")
- [Part 2: Goodness of Fit, Degeneracy, Estimation Algorithm](#part-2-goodness-of-fit-degeneracy-estimation-algorithm  "Part 2")
- [Part 3: Adding and Cleaning New data](#part-3-adding-and-cleaning-new-data  "Part 3")
- Part 4: Computing Period Networks and Covariate Lists <br>(You are here.) 


# Part 4: Computing Period Networks and Covariate Lists

> [Back to Contents](#contents  "Back")


```r
##===============================
## SET YOUR DIRECTORIES:
##   This is the path to the folder where you saved the data file.
##   If you are using a Windows PC, use double backslash path separators "..\\dir\\subdir\\.."
##-------------------------------
## working dir
work_dir <- '/set/working/dir'

## new data directory name
cb_data_dir <- '/set/data/dir'

## owler data directory name
owler_data_dir_name <- 'owler_data'

## SET FOCAL FIRM
focal_firm <- 'ford' ## your focal firm
```





Set relative paths for data


```r
##================================
## Relative paths based on above directories
##--------------------------------
## data_dir <- '/set/your/data/directory/here'
data_dir <- file.path(work_dir, 'data')

## new data directory name
owler_data_dir <- file.path(data_dir, owler_data_dir_name)
```

Load `R` scripts:
- `amj_awareness_functions.R` loads functions for data processing; cached in environment as [list] `aaf`
- `amj_tutorial_cb_data_prep.R` loads data tables; cached in environment as [list] `cb`

This will take several minutes to complete.


```r
##==================================================
## Run data loading and prep scripts
##--------------------------------------------------
source(file.path(work_dir,'R','amj_awareness_functions.R'))    ## aaf: compnet awareness functions
```

```r
source(file.path(work_dir,'R','amj_tutorial_cb_data_prep.R'))  ## cb:  CrunchBase dataframes object
```

```
## 
## Attaching package: 'magrittr'
## The following object is masked from 'package:texreg':
## 
##     extract
## 
## loading dataframes...done.
## adding Owler data
## 1. adding new Owler firms to CrunchBase
##    adding UUIDs to owler firms...
## 2. adding attributes from Owler to CrunchBase firms
## 3. adding acquisitions from Owler to CrunchBase acquisitions table
## 4. adding IPOs from Owler to CrunchBase IPOs table
## 5. adding competitors from Owler to CrunchBase competitors table
## preparing CrunchBase data
## Warning: All formats failed to parse. No formats found.
## Warning: All formats failed to parse. No formats found.
## Warning: All formats failed to parse. No formats found.
## reshaping acquisitions dataframe...done.
## clearing environment...
```

```r
print(summary(aaf))
```

```
##                       Length Class  Mode    
## makeGraph             1      -none- function
## generalistIndex       1      -none- function
## jobsToBeDone          1      -none- function
## mmcMarketsDf          1      -none- function
## mmcfromMarketConcat   1      -none- function
## coopConcatDf          1      -none- function
## coopFromConcat        1      -none- function
## .cov.coop             1      -none- function
## .cov.coopPast         1      -none- function
## .cov.age              1      -none- function
## .cov.mmc              1      -none- function
## .cov.dist             1      -none- function
## .cov.ipo              1      -none- function
## .cov.constraint       1      -none- function
## .cov.similarity       1      -none- function
## .cov.centrality       1      -none- function
## .cov.generalistIndex  1      -none- function
## setCovariates         1      -none- function
## nodeCollapseGraph     1      -none- function
## makePdNetwork         1      -none- function
## makePdGraph           1      -none- function
## getNetEcount          1      -none- function
## plotCompNetColPredict 1      -none- function
```

```r
print(summary(cb))
```

```
##                 Length Class      Mode    
## uuid             1     -none-     function
## match            1     -none-     function
## parseNonNa       1     -none-     function
## falsy            1     -none-     function
## relationBeganOn  1     -none-     function
## relationEndedOn  1     -none-     function
## readCsv          1     -none-     function
## fixDateYMD       1     -none-     function
## csv             13     -none-     list    
## co              33     data.frame list    
## co_comp         15     data.frame list    
## co_cust          7     data.frame list    
## co_parent        7     data.frame list    
## co_prod         12     data.frame list    
## co_acq          23     data.frame list    
## co_br           19     data.frame list    
## co_rou          21     data.frame list    
## co_ipo          20     data.frame list    
## fund             9     data.frame list    
## inv             14     data.frame list    
## inv_rou          3     data.frame list    
## inv_part         3     data.frame list
```

Create the full competition network (graph) for all competitive relations at all times. The following step after this will then create competition network panels with one competition network per time period by removing the relations and firms that didn't exist during that period.


```r
##==================================================
##
##  Make Full Graph
##
##--------------------------------------------------

cat('\nmaking full graph...')
```

```
## 
## making full graph...
```

```r
max.year <- 2016

## delete edges at or later than this date (the year after max.year)
exclude.date <- sprintf('%d-01-01', max.year+1)

## make graph
g.full <- aaf$makeGraph(comp = cb$co_comp, vertdf = cb$co)

## cut out confirmed dates >= 2016
g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[which(V(g.full)$founded_year <= max.year
                                                                | is.na(V(g.full)$founded_year)
                                                                | V(g.full)$founded_year=='' ) ] )
g.full <- igraph::delete.edges(g.full, E(g.full)[which(E(g.full)$relation_created_at >= exclude.date)])

## SIMPLIFY
g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T,
                           edge.attr.comb = list(weight='sum',
                                                 relation_began_on='max',
                                                 relation_ended_on='min'))

## save graph file
igraph::write.graph(graph = g.full, file=file.path(data_dir, "g_full.graphml"), format = 'graphml')

cat('done.\n')
```

```
## done.
```

The main data preparation step involves using the full competition network and running a 3-step procedure to compute temporal panel data of competition networks and covariate arrays for each period.

The steps apply functions loaded in the `aaf` object for each period in the analysis time frame:
1. `aaf$nodeCollapseGraph(...)` Process acquisitions by transferring competitive relations from acquisition target to acquiring firm for each period
2. `aaf$makePdNetwork(...)` Filter the competitive relations and firms that existed within each period
3. `aaf$setCovariates(...)`  Compute node and edge covariates from the updated period competition network and set the covariates in this period's `network` object


```r
##==================================================
##
##  Create Focal Firm Networks per time period
##     and compute covariate arrays lists 
##     from network in each period 
##
##--------------------------------------------------

## -- settings --
d <- 3                   ## distance threshold for cohort selection
yrpd <- 1                ## length of period in years
startYr <- 2005          ## starting year (including 1 previous year for lag)
endYr <- 2017            ## dropping first for memory term; actual dates 2007-2016
lg.cutoff <- 1100        ## large network size cutoff to save periods seprately 
force.overwrite <- FALSE ## if network files in directory should be overwritten
## --------------  


##====================================
## run main network period creation loop
##-------------------------------------
# for (i in 1:length(firms.todo)) {

name_i <- focal_firm

periods <- seq(startYr,endYr,yrpd)
company.name <- 'company_name_unique'
g.base <- g.full  

## focal firm ego network sample
g.d.sub <- igraph::make_ego_graph(graph = g.base, nodes = V(g.base)[V(g.base)$name==name_i], order = d, mode = 'all')[[1]]

## convert to network object
net.d.sub <- asNetwork(g.d.sub)
net <- net.d.sub
net %n% 'ego' <- name_i

##-------process pre-start-year acquisitions----------
acqs.pd <- cb$co_acq[cb$co_acq$acquired_on <= sprintf('%d-12-31',startYr-1), ]
g.d.sub <- aaf$nodeCollapseGraph(g.d.sub, acqs.pd, remove.isolates=T, verbose = T)
```

```
## processing acquisitions: 0 ...
## done.
```

```r
net.d.sub <- asNetwork(g.d.sub)
cat(sprintf('v = %d, e = %d\n',vcount(g.d.sub),ecount(g.d.sub)))
```

```
## v = 116, e = 437
```

```r
##------------Network Time Period List--------------------
nl <- list()

for (t in 2:length(periods)) 
{
  ## period dates
  cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
  t1 <- sprintf('%d-01-01',periods[t-1]) ## inclusive start date 'YYYY-MM-DD'
  t2 <- sprintf('%d-12-31',periods[t-1]) ## inclusive end date 'YYYY-MM-DD'
  
  ## check if period network file exists (skip if not force overwrite)
  file.rds <- sprintf('firm_nets_rnr/%s_d%d_y%s.rds',name_i,d,periods[t-1])
  if (!force.overwrite & file.exists(file.rds)) {
    cat(sprintf('file exists: %s\nskipping.\n', file.rds))
    next
  }
  
  ## 1. Node Collapse acquisitions within period
  acqs.pd <- cb$co_acq[cb$co_acq$acquired_on >= t1 & cb$co_acq$acquired_on <= t2, ]
  g.d.sub <- aaf$nodeCollapseGraph(g.d.sub, acqs.pd, verbose = T)
  
  ## 2. Subset Period Network
  nl[[t]] <- aaf$makePdNetwork(asNetwork(g.d.sub), periods[t-1], periods[t], isolates.remove = F) 
  
  ## 3. Set Covariates for updated Period Network
  covlist <- c('age','mmc','dist','ipo_status','constraint','similarity','centrality','generalist')
  nl[[t]] <- aaf$setCovariates(nl[[t]], periods[t-1], periods[t], covlist=covlist,
                               acq=cb$co_acq,br=cb$co_br,rou=cb$co_rou,ipo=cb$co_ipo)
  
}
```

```
## 
## making period 2005-2006:
## processing acquisitions: 0 ...
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...no firm branches in period. creating empty mmc matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2006-2007:
## processing acquisitions: 0 ...
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...no firm branches in period. creating empty mmc matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2007-2008:
## processing acquisitions: 0 ...
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...no firm branches in period. creating empty mmc matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2008-2009:
## processing acquisitions: 0 ...
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...concatenating firm branch markets...
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |================                                                 |  25%
  |                                                                       
  |================================                                 |  50%
  |                                                                       
  |=================================================                |  75%
  |                                                                       
  |=================================================================| 100%
## computing MMC outer product matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2009-2010:
## processing acquisitions: 0 ...
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...concatenating firm branch markets...
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |=============                                                    |  20%
  |                                                                       
  |==========================                                       |  40%
  |                                                                       
  |=======================================                          |  60%
  |                                                                       
  |====================================================             |  80%
  |                                                                       
  |=================================================================| 100%
## computing MMC outer product matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2010-2011:
## processing acquisitions: 0 ...
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...concatenating firm branch markets...
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |=============                                                    |  20%
  |                                                                       
  |==========================                                       |  40%
  |                                                                       
  |=======================================                          |  60%
  |                                                                       
  |====================================================             |  80%
  |                                                                       
  |=================================================================| 100%
## computing MMC outer product matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2011-2012:
## processing acquisitions: 0 ...
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...concatenating firm branch markets...
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |=============                                                    |  20%
  |                                                                       
  |==========================                                       |  40%
  |                                                                       
  |=======================================                          |  60%
  |                                                                       
  |====================================================             |  80%
  |                                                                       
  |=================================================================| 100%
## computing MMC outer product matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2012-2013:
## processing acquisitions: 1 ...
## simplifying edges...done.
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...concatenating firm branch markets...
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |=============                                                    |  20%
  |                                                                       
  |==========================                                       |  40%
  |                                                                       
  |=======================================                          |  60%
  |                                                                       
  |====================================================             |  80%
  |                                                                       
  |=================================================================| 100%
## computing MMC outer product matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2013-2014:
## processing acquisitions: 0 ...
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...concatenating firm branch markets...
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |=============                                                    |  20%
  |                                                                       
  |==========================                                       |  40%
  |                                                                       
  |=======================================                          |  60%
  |                                                                       
  |====================================================             |  80%
  |                                                                       
  |=================================================================| 100%
## computing MMC outer product matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2014-2015:
## processing acquisitions: 1 ...
## simplifying edges...done.
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...concatenating firm branch markets...
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |=============                                                    |  20%
  |                                                                       
  |==========================                                       |  40%
  |                                                                       
  |=======================================                          |  60%
  |                                                                       
  |====================================================             |  80%
  |                                                                       
  |=================================================================| 100%
## computing MMC outer product matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2015-2016:
## processing acquisitions: 0 ...
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...concatenating firm branch markets...
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |=========                                                        |  14%
  |                                                                       
  |===================                                              |  29%
  |                                                                       
  |============================                                     |  43%
  |                                                                       
  |=====================================                            |  57%
  |                                                                       
  |==============================================                   |  71%
  |                                                                       
  |========================================================         |  86%
  |                                                                       
  |=================================================================| 100%
## computing MMC outer product matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
## 
## making period 2016-2017:
## processing acquisitions: 0 ...
## done.
## collecting edges and vertices to remove...done.
## computing age ...done
## computing multi-market contact (branch geographic overlap)...concatenating firm branch markets...
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |=========                                                        |  14%
  |                                                                       
  |===================                                              |  29%
  |                                                                       
  |============================                                     |  43%
  |                                                                       
  |=====================================                            |  57%
  |                                                                       
  |==============================================                   |  71%
  |                                                                       
  |========================================================         |  86%
  |                                                                       
  |=================================================================| 100%
## computing MMC outer product matrix...done.
## done
## computing distances lag contact...done
## computing IPO status contact...done
## computing constraint...done
## computing inv.log.w.similarity...done
## computing centralities...done
## computing Generalist (vs Specialist) Index...done
```

```r
## ----drop null and skipped periods----
nl.bak <- nl
nl <- nl[which(sapply(nl, length)>0)]

if (length(nl) > 1) {
  names(nl) <- periods[2:length(periods)]
}

##--------------- GET TERGM NETS LIST -----------
## only nets with edges > 0
if (length(nl) > 1) {
  nets.all <- nl[2:length(nl)]
} else {
  nets.all <- nl
}
nets <- nets.all[ which(sapply(nets.all, aaf$getNetEcount) > 0) ]
## record network sizes
write.csv(sapply(nets,function(x)length(x$val)), file = file.path(data_dir,sprintf('%s_d%s.csv',name_i,d)))

#-------------------------------------------------

## Save serialized data file of all networks and covariates lists 
file.rds <- file.path(data_dir,sprintf('%s_d%d.rds',name_i,d))
saveRDS(nets, file = file.rds)
```


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


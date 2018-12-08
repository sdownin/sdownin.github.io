---
title: "Compnet Awareness Tutorial - Part 3"
collection: code
permalink: /code/compnet-awareness-tutorial-part-3
excerpt: "Part 3: Adding and Cleaning New data   <br/><img src='/data/compnet-awareness-tutorial-part-3-thumbnail.png' height="200">"
---


## Tutorial Parts
- [Part 1](/code/compnet-awareness-tutorial-part-1 "Part 1")
- [Part 2](/code/compnet-awareness-tutorial-part-2 "Part 2")
- Part 3 (You are here.)
- [Part 4](/code/compnet-awareness-tutorial-part-4 "Part 4")

#### Contents
- [Part 1: Analyzing Example Network Data Sample](#part-1-analyzing-example-network-data-sample  "Part 1")
- [Part 2: Goodness of Fit, Degeneracy, Estimation Algorithm](#part-2-goodness-of-fit-degeneracy-estimation-algorithm  "Part 2")
- Part 3: Adding and Cleaning New data (You are here.) 
- [Part 4: Computing Period Networks and Covariate Lists](#part-4-computing-period-networks-and-covariate-lists  "Part 4")



# Part 3: Adding and Cleaning New data

> [Back to Contents](#contents  "Back")



In this repository's `R` directory, download the R script `amj_run_TERGM_tutorial_3.R`. 

Create a new folder in your `data_dir` and title it `owler_dir`. If you want to change the name of this new folder, then don't forget to update the `owler_data` variable in the `amj_run_TERGM_tutorial_3.R` script.

Download your owler data file(s) as a CSV file(s) from the Google Doc

- [COMPNET-MASTER Owler Data](https://docs.google.com/spreadsheets/d/1jmOI_byTPlznzbbkIcgCRZ2SbvLzs2q9SI6rI8xJFE0/edit?usp=sharing "Google Doc") 

and save them in your `owler_dir` directory. 

You can download 1 or multiple CSV files; the script below can process 1 file and also combine multiple files.

Before working on the data let's take a look at a single period network data structure
```r
net <- nets[[1]]
print(net)

##  Network attributes:
##   vertices = 180 
##   directed = FALSE 
##   hyper = FALSE 
##   loops = FALSE 
##   multiple = FALSE 
##   bipartite = FALSE 
##   mmc:
##                  0 0.0833333333333333              0.125 
##              32344                  2                  2 
##  0.166666666666667               0.25  0.333333333333333 
##                  2                 20                  4 
##                0.5 
##                 26 
##   dist: 180x180 matrix
##   similarity: 180x180 matrix
##   coop:
##     0 
## 32400 
##   coop_bin:
##     0 
## 32400 
##   coop_past:
##     0 
## 32400 
##   coop_past_bin:
##     0 
## 32400 
##   total edges= 92 
##     missing edges= 0 
##     non-missing edges= 92 
## 
##  Vertex attribute names: 
##     acquired_name acquired_on acquired_uuid acquired_vids age category_group_list category_list cent_deg cent_eig cent_pow_n0_0 cent_pow_n0_1 cent_pow_n0_2 cent_pow_n0_3 cent_pow_n0_4 cent_pow_n0_5 city closed_on closed_year com_edgebetween com_fastgreedy com_infomap com_labelprop com_multilevel com_walktrap company_cusip company_cusip_6 company_gvkey company_name company_sic company_uuid constraint country_code domain employee_count founded_on founded_year genidx_edgebetween genidx_fastgreedy genidx_infomap genidx_labelprop genidx_multilevel genidx_walktrap id ipo_status njobs_edgebetween njobs_fastgreedy njobs_infomap njobs_labelprop njobs_multilevel njobs_walktrap orig.vid region state_code status_update vertex.names weight 
## 
##  Edge attribute names: 
##     relation_began_on relation_ended_on weight

dim(net[,])

## [1] 180 180

print(net[1:4,1:4])

##                123contactform abroad101 actuate allegiance
## 123contactform              0         0       0          0
## abroad101                   0         0       0          0
## actuate                     0         0       0          0
## allegiance                  0         0       0          0
```

After saving the CSV data files in the the new `owler_dir`, you can run the `s` script to process and combine the file(s). 

This script will create (or overwrite) 2 new files in your `data_dir`
 - `owler_edge_list` Edge list of competitive relations
 - `owler_vertex_list` Vertex list of firm attributes


```r
## missing value strings to convert to <NA> type in imported data file
na.strings <- c('NA', 'na', '')

##
#  Assign company_name_unique if missing
#     from two input columns (company_name, company_name_unique)
#  @param [string[]|NA[]] x  Vector of two strings|NAs (company_name, company_name_unique)
#  @return [string company_name_unique
##
assignCompanyNameUnique <- function(x=NA)
{
  # cat(sprintf('1: %s, 2: %s\n', x[1], x[2]))
  if (all(is.na(x)))
    return(NA)
  name <- ifelse(!is.na(x[2]), x[2], x[1]) ## use company_name_unique if exists, else company_name
  name <- str_to_lower(name)  ## to lowercase 
  ## replace non-alphanumeric sequences with a dash "-" and then return
  return(str_replace_all(name, pattern = '[^A-Za-z0-9]+', replacement = '-'))
}

##====================================
## Add New Data from Owler Data Files
##------------------------------------


## new data directory
owler_dir <- file.path(data_dir,owler_data_dir)

## init new dataframes
vt <- data.frame()  #vertices
el <- data.frame()  #edge list

## loop over data files in data directory
for (file in dir(owler_dir, pattern = '\\.csv$')) {
  cat(sprintf('\n\ndata file %s\n', file))  ## echo progress
  
  ## load data
  full_file_path <- file.path(owler_dir, file)
  df <- read.csv(full_file_path, stringsAsFactors = F, na.strings = na.strings)
  
  ## competitor columns
  compcols <- names(df)[grep('competitor_\\d{1,}',x = names(df))]

  ## keep columns that are NOT missing all competiors 
  rows.keep <- apply(df[,compcols], 1, function(x) !all(is.na(x)))
  df <- df[rows.keep,]
  
  ## assign company_name_unique if not exists
  tmp_names_df <- df[, c('name', 'company_name_unique')]
  df$company_name_unique <- apply(tmp_names_df, 1, assignCompanyNameUnique)
  
  ## if no competitor data columns or missing company_name_unique column, 
  ## then skip to next data file
  if (!('company_name_unique' %in% names(df)))
    next
  
  ## append verices
  vt <- rbind(vt, df)
  
  ## loop over firms in data file
  for (i in 1:nrow(df)) {
    
    ## firm i 
    firm_i <- df[i, 'company_name_unique']

    ## select competitors of firm i 
    firm_i_comps <- unlist(df[i, compcols]) ## unlist from data.frame to vector
    
    ## skip rows with no competitors included
    if (all(sapply(firm_i_comps, is.na)))
      next
    
    ## loop over each  competitor j of firm i
    for (j in 1:length(firm_i_comps)) {
      
      ## competitor j's name
      comp_j_name <- unname(firm_i_comps[j])
      ## competitor j's company_name_unique
      comp_j <- vt$company_name_unique[vt$name==comp_j_name]
      
      ## skip is missing
      if (length(comp_j)==0) 
        next
      if(all(is.na(comp_j_name)) | all(is.na(comp_j))) 
        next
      
      # ## echo progress check 
      # cat(sprintf('%s firm %s, competitor %s\n', i, firm_i, comp_j))  ## echo progress
      
      ## append competitor relation      
      tmp_el <- data.frame(source=firm_i, target=comp_j, rank=j, weight=1)
      el <- rbind(el, tmp_el)          
      
    }
    
    if (i %% 50 == 0) cat(sprintf('firm %s %s\n', i, firm_i))  ## echo progress
    
  }
  
}



## check vertices
dim(vt)
head(vt)

## check edge list
dim(el)
head(el, 20)

## 
## 
## data file Ford.csv
## firm 100 dodgeofftworth
## firm 150 physioroom
## firm 200 alldata
## 
## 
## data file NYTimes.csv
## firm 50 lex-machina
## firm 100 linkedin
## firm 150 marketaxess
## firm 200 electra-information-systems
## firm 250 iri
## firm 300 cable-one
## firm 350 salem-media-group
```

Save the vertex list and edge list to CSV files.

```r
## write edge list to csv file
el_file <- file.path(data_dir, 'owler_edge_list.csv')
write.csv(el, file = el_file, row.names = F)

## write vertex list to csv file
vt_file <- file.path(data_dir, 'owler_vertex_list.csv')
write.csv(vt, file = vt_file, row.names = F)
```

This completes Part 3.


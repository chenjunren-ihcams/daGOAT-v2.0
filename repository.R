rm(list=ls())
library(readxl)
setwd("E:/work/project/GOAT/data/GOAT-main")


convert_dl2df <- function(input_list) {
  # Converts the data received by the api to data.frame
  # The data format of input_list is list($col_name, list($values))
  # eg. input_list <- list(Age=c(12,), Sex=c("Male",), ...)
  df <- data.frame(input_list[1])
  for (i in 2:length(input_list)) {
    df <- cbind(df, data.frame(input_list[i]))
  }
  df[df == "null"] <- NA
  return (df)
}


convert_dl2df_t <- function(input_list) {
  # Converts the data received by the api to data.frame
  # The data format of input_list is list("s234": c(stationary.odds.234.t1, stationary.odds.234.t2,,...), 
  #                          "d234": c(dynamic.odds.234.t1, dynamic.odds.234.t2,,...),
  #                          "s34": c(stationary.odds.34.t1, stationary.odds.34.t2,,...),
  #                          "d34": c(dynamic.odds.34.t1, dynamic.odds.34.t2,,...))
  df <- data.frame(input_list[1])
  for (i in 2:length(input_list)) {
    df <- cbind(df, data.frame(input_list[i]))
  }
  df[df == "NA"] <- NA
  df <- t(df)
  return (df)
}


## preset - parameters
get.file <- function(file){
  file <- read.csv(paste(file, '.csv', sep=''))
  file
}



## Step 1
## Input stationary.file and dynamic.file. The population is 'adults' or 'children'.
## Output four lines of odds, dynamic and static model parameters for the day.
present.odds <- function(stationary.data.list,
                         dynamic.data.list,
                         population){
  
  library(readxl)
  library(e1071)
  
  ## ################################################################################# 1. stationary
  cols <- c("Sex", "Age",	"BMI",	"Primary.disease", "Conditioning.regimen",
            "Stem.cell.source",	"Sex.match", "ABO.match",	"TNC.count", "CD34.count",
            "Donor.type",	"GVHD.prophylaxis",	"Antithymocyte.globulin.in.conditioning",
            "HLA.mismatch")
  
  factor.cols <- c("Sex", "Primary.disease", "Conditioning.regimen",
                   "Stem.cell.source", "Sex.match", "ABO.match",
                   "Donor.type", "GVHD.prophylaxis", "Antithymocyte.globulin.in.conditioning")
  
  num.cols <- setdiff(cols, factor.cols)
  
  stationary.file <- convert_dl2df(stationary.data.list)
  stationary <- stationary.file[, c(cols)]
  for(col in factor.cols){
    stationary[, c(col)] <- as.factor(stationary[, c(col)])
  }
  for(col in num.cols){
    stationary[, c(col)] <- as.numeric(stationary[, c(col)])
  }
  
  
  ## grade 234
  nb.234 <- list()
  stationary.odds.234 <- array(NA, 100)
  for (t in 1: 100){
    nb.rds <- paste("preset/", population, "/grade234/NB_RDS/T", t, "NB_model.rds", sep="")
    model.nb <- readRDS(nb.rds)
    prob.nb <- predict(model.nb, stationary[, c(cols)], type = c("raw"),)
    stationary.odds.234[t] <- log( (prob.nb[2]+0.01) / (prob.nb[1]+0.01) )
    nb.234[[t]] <- model.nb$tables
  }
  
  
  ## grade 34
  nb.34 <- list()
  stationary.odds.34 <- array(NA, 100)
  for (t in 1: 100){
    nb.rds <- paste("preset/", population, "/grade34/NB_RDS/T", t, "NB_model.rds", sep="")
    model.nb <- readRDS(nb.rds)
    prob.nb <- predict(model.nb, stationary[, c(cols)], type = c("raw"),)
    stationary.odds.34[t] <- log( (prob.nb[2]+0.01) / (prob.nb[1]+0.01) )
    nb.34[[t]] <- model.nb$tables
  }
  
  
  ## ################################################################################# 2. dynamic
  #keys <- get.file(paste("preset/", population, '/keys', sep=''))$keys
  keys1 <- get.file(paste("preset/", population, '/keys', sep=''))$keys
  keys2 <- setdiff(keys1, keys1[grep('vitalsigns_[0-9]{3}', keys1)])#########Delete nursing records
  keys <- setdiff(keys2, keys2[grep('antibodies_[0-9]{3}', keys2)])#########Delete antibody records
  
  dynamic.file <- convert_dl2df(dynamic.data.list)
  dynamic <- dynamic.file[, c(keys)]
  for (iter in 1:3) {
    hasValue <- list()
    for (t in 1: 100) {
      hasValue[[t]] <- !is.na(dynamic[t, ])
    }
    for (t in 100:(1+1)) {
      dynamic[t, !hasValue[[t]]] <- dynamic[t - 1, !hasValue[[t]]]
    }
  }
  
  
  ## grade 234
  thresholds.high.234 <- get.file(file=paste('preset/', population, '/grade234/thresholds-high', sep=''))
  thresholds.low.234 <- get.file(file=paste('preset/', population, '/grade234/thresholds-low', sep=''))
  odds.ratio.high.234 <- get.file(file=paste('preset/', population, '/grade234/odds-ratio-high', sep=''))
  odds.ratio.medium.234 <- get.file(file=paste('preset/', population, '/grade234/odds-ratio-medium', sep=''))
  odds.ratio.low.234 <- get.file(file=paste('preset/', population, '/grade234/odds-ratio-low', sep=''))
  dynamic.odds.234 <- array(NA, 100)
  for (t in 1:100) {
    dynamic.odds.234[t] <- 0
    for (k in 1:length(keys)) {
      key <- keys[k]
      val <- dynamic[t, names(dynamic) == key]
      if (!is.na(val) & !is.na(thresholds.high.234[t, key])) {
        val <- as.numeric(val)
        if (val > thresholds.high.234[t, key]) {
          dynamic.odds.234[t] <- dynamic.odds.234[t] + log(odds.ratio.high.234[t, key])
        }
      }
      if (!is.na(val) & !is.na(thresholds.low.234[t, key])) {
        val <- as.numeric(val)
        if (val < thresholds.low.234[t, key]) {
          dynamic.odds.234[t] <- dynamic.odds.234[t] + log(odds.ratio.low.234[t, key])
        }
      }
      if (!is.na(val) & !is.na(thresholds.low.234[t, key]) & !is.na(thresholds.high.234[t, key])) {
        val <- as.numeric(val)
        if (val >= thresholds.low.234[t, key] & val <= thresholds.high.234[t, key]) {
          dynamic.odds.234[t] <- dynamic.odds.234[t] + log(odds.ratio.medium.234[t, key])
        }
      }
    }
  }
  
  
  ## grade 34
  thresholds.high.34 <- get.file(file=paste('preset/', population, '/grade34/thresholds-high', sep=''))
  thresholds.low.34 <- get.file(file=paste('preset/', population, '/grade34/thresholds-low', sep=''))
  odds.ratio.high.34 <- get.file(file=paste('preset/', population, '/grade34/odds-ratio-high', sep=''))
  odds.ratio.medium.34 <- get.file(file=paste('preset/', population, '/grade34/odds-ratio-medium', sep=''))
  odds.ratio.low.34 <- get.file(file=paste('preset/', population, '/grade34/odds-ratio-low', sep=''))
  dynamic.odds.34 <- array(NA, 100)
  for (t in 1: 100) {
    dynamic.odds.34[t] <- 0
    for (k in 1:length(keys)) {
      key <- keys[k]
      val <- dynamic[t, names(dynamic) == key]
      if (!is.na(val) & !is.na(thresholds.high.34[t, key])) {
        val <- as.numeric(val)
        if (val > thresholds.high.34[t, key]) {
          dynamic.odds.34[t] <- dynamic.odds.34[t] + log(odds.ratio.high.34[t, key])
        }
      }
      if (!is.na(val) & !is.na(thresholds.low.34[t, key])) {
        val <- as.numeric(val)
        if (val < thresholds.low.34[t, key]) {
          dynamic.odds.34[t] <- dynamic.odds.34[t] + log(odds.ratio.low.34[t, key])
        }
      }
      if (!is.na(val) & !is.na(thresholds.low.34[t, key]) & !is.na(thresholds.high.34[t, key])) {
        val <- as.numeric(val)
        if (val >= thresholds.low.34[t, key] & val <= thresholds.high.34[t, key]) {
          dynamic.odds.34[t] <- dynamic.odds.34[t] + log(odds.ratio.medium.34[t, key])
        }
      }
    }
  }
  
  
  ## ################################################################################# 3. t stationary odds and t dynamic odds
  out.1 <- rbind('s234'=stationary.odds.234, 'd234'=dynamic.odds.234, 's34'=stationary.odds.34, 'd34'=dynamic.odds.34)
  out <- list("solo"=out.1,
              "dynamic" = dynamic,
              "population"=population,
              "nb234"=as.data.frame(unlist(nb.234)),
              "thresholds_high_234"=thresholds.high.234[1,], 
              "thresholds_low_234"=thresholds.low.234[1,], 
              "odds_ratio_high_234"=odds.ratio.high.234, 
              "odds_ratio_medium_234"=odds.ratio.medium.234, 
              "odds_ratio_low_234"=odds.ratio.low.234,
              "nb34"=as.data.frame(unlist(nb.34)),
              "thresholds_high_34"=thresholds.high.34[1,], 
              "thresholds_low_34"=thresholds.low.34[1,], 
              "odds_ratio_high_34"=odds.ratio.high.34, 
              "odds_ratio_medium_34"=odds.ratio.medium.34, 
              "odds_ratio_low_34"=odds.ratio.low.34)
  
  
}



## Step 1 test
population <- 'adults'
stationary.data.list <- get.file(file=paste('test/', population, '-stationary', sep=''))
dynamic.data.list <- get.file(file=paste('test/', population, '-dynamic', sep=''))
dynamic.data.list[, grep('vitalsigns_[0-9]{3}', names(dynamic.data.list))] <- NA 
res.1 <- present.odds(stationary.data.list=stationary.data.list,
                      dynamic.data.list=dynamic.data.list,
                      population=population)
res.1.out <- res.1[[1]]



## Step 2
## Input solo.odds (res.1)
## Output 2 rows of odds and pre.window.size accumulated over 3 days
cumulation.odds <- function(solo.odds.list,
                            pre.window.size=3){
  
  solo.odds <- convert_dl2df_t(solo.odds.list)
  
  dynamic.odds.cum.234 <- array(NA, 100)
  stationary.odds <- solo.odds[1,]
  dynamic.odds <- solo.odds[2,]
  
  for (t in 1: 100){
    dynamic.odds.cum.234[t] <- sum(dynamic.odds[(-min(pre.window.size, (t)) : 0) + t], na.rm = T)
  }
  cum.odds.234 <- stationary.odds + dynamic.odds.cum.234
  
  dynamic.odds.cum.34 <- array(NA, 100)
  stationary.odds <- solo.odds[3,]
  dynamic.odds <- solo.odds[4,]
  for (t in 1: 100){
    dynamic.odds.cum.34[t] <- sum(dynamic.odds[(-min(pre.window.size, (t)) : 0) + t], na.rm = T)
  }
  cum.odds.34 <- stationary.odds + dynamic.odds.cum.34
  
  out.1 <- rbind("234"=cum.odds.234, "34"=cum.odds.34)
  out <- list("cumulation"=out.1,
              "window"=pre.window.size)
  
}


## Step 2 test
solo.odds.list <- list((res.1.out[1, ]), c(res.1.out[2, ]), c(res.1.out[3, ]),c(res.1.out[4, ]) )
res.2 <- cumulation.odds(solo.odds.list=solo.odds.list,
                         pre.window.size = 3)
res.2.out <- res.2[[1]]

## Step 3
## Input cum.odds (res.2)
## Output risk level and 4 cutoff
risk.level <- function(cum.odds.list,
                       population){
  
  cum.odds <- convert_dl2df_t(cum.odds.list)
  
  
  if(population=='adults'){
    st <- 17
    et <- 23
    
    cutoff.234.low <- get.file(paste("preset/", population, '/cutoff', sep=''))$cutoff.234.low
    cutoff.234.high <- get.file(paste("preset/", population, '/cutoff', sep=''))$cutoff.234.high
    cum.odds.234 <- cum.odds[1, ]
    risk.level.234 <- array(NA, 100)
    for(t in st:et){
      if(cum.odds.234[t]>cutoff.234.high[t-16]){
        risk.level.234[t] <- 'high risk'
      }else{
        if(cum.odds.234[t]>cutoff.234.low[t-16]){
          risk.level.234[t] <- 'intermediate risk'
        }else{
          risk.level.234[t] <- 'low risk'
        }
      }
    }
    
    cutoff.34.low <- get.file(paste("preset/", population, '/cutoff', sep=''))$cutoff.34.low
    cutoff.34.high <- get.file(paste("preset/", population, '/cutoff', sep=''))$cutoff.34.high
    cum.odds.34 <- cum.odds[2, ]
    risk.level.34 <- array(NA, 100)
    for(t in st:et){
      if(cum.odds.34[t]>cutoff.34.high[t-16]){
        risk.level.34[t] <- 'high risk'
      }else{
        if(cum.odds.34[t]>cutoff.34.low[t-16]){
          risk.level.34[t] <- 'intermediate risk'
        }else{
          risk.level.34[t] <- 'low risk'
        }
      }
    }
    
    risk.level <- array(NA, 100)
    for(t in st:et){
      if( (risk.level.34[t]=='high risk') | (risk.level.234[t]=='high risk') ){
        risk.level[t] <- 'high risk'
      }else{
        if( (risk.level.34[t]=='intermediate risk') & (risk.level.234[t]=='intermediate risk') ){
          risk.level[t] <- 'intermediate risk'
        }else{
          risk.level[t] <- 'low risk'
        }
      }
    }
    
  }
  
  
  
  out <- list("risk_level"=risk.level,
              "risk_level_234"=risk.level.234, 
              "cutoff_low_234"=cutoff.234.low, 
              "cutoff_high_234"=cutoff.234.high,
              "risk_level_34"=risk.level.34, 
              "cutoff_low_34"=cutoff.34.low, 
              "cutoff_high_34"=cutoff.34.high)
}


## Step 3 test
cum.odds.list <- list((res.2.out[1, ]), c(res.2.out[2, ]))

res.3 <- risk.level(cum.odds.list=cum.odds.list,
                    population=population)
res.3.out <- res.3[[1]][17 : 23]

res.3.out

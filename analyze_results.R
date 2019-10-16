# This script should be run after main. It computes the area under the ROC curve
# based on the true outlier labels and the anomaly scores.

require(ROCR) # for auc function (area under the curve)

(load(file='Goldstein.Rdata'))

aecsvs2df <- function(PARS, RESPATH){
  # This function reads the output-csv-files of the autoencoder and
  # combines the outlier score that is within each file into one big
  # data frame. Note each csv-files contains the outlier score that corresponds
  # with a particular parameter setting.
  # PARS    - A named list containing the parameter settings of lambda, n hidden,
  #           seed, as well as the abbrevaiation of the dataset, and the number of
  #           observations in the dataset
  # RESPATH - Result Path. Path where the csv files are located and the output
  #           data frame will be written to
  # The output is a dataframe containing the outlier scores. This df is 
  # written to the RESPATH as well as returned by the function.
  ncolout <- length(PARS$lam)*length(PARS$nhid)*length(PARS$see)
  dfout <- data.frame(matrix(0, nrow = PARS$nobs, ncol = ncolout))
  colnum <- 1
  for(lam in PARS$lam){
    for(nhid in PARS$nhid){
      for(see in PARS$see){
        dftemp <- read.csv(file = paste0(RESPATH, 'out_ds', PARS$name, '_',
                                         lam,'_',nhid,'_',see,'.csv'))
        dfout[,colnum] <- dftemp[,2]
        names(dfout)[colnum] <- paste0('V_',lam,'_',nhid,'_',see)
        colnum <- colnum + 1
      }
    }
  }
  assign(paste0("dfae", PARS$name), dfout)
  save(list = paste0("dfae", PARS$name), file = paste0(RESPATH, 'dfae', PARS$name, '.Rdata'))
  return(dfout)
}


auc <- function(PRED, REAL){
  pred2 <- prediction(PRED, REAL)
  preauc <- performance(pred2,"auc")
  auc <- unlist(slot(preauc, "y.values"))
  return(auc)
}

get_auc <- function(REAL, PREDS){
  # REAL    - binary vector indicating an outlier (=1) or a normal observation (=0)
  #           the length of REAL equals the number of observations.
  # PREDS   - data frame containing in the columns the outlier scores for various
  #           parameter settings.
  aucvec <- apply(PREDS, 2, function(x) auc(x, REAL))
  print(paste('mean auc: ', round(mean(aucvec),4)))
  print(paste('conf int: ', round(sd(aucvec)/sqrt(length(aucvec)),5)))
  return(aucvec)
}
  
## bc
if(FALSE){ # I commented out these lines since Wojtek first wants to see RBM
# autoencoders
parsbc <- list(
         lam = c(0.001), 
         nhid = c(5, 10 , 15, 30),
         see = 1:10,
         name = "bc",
         nobs = 367)

outdf <- aecsvs2df(parsbc, './')
aucsae <- get_auc(ifelse(dfbc[,ncol(dfbc)]== 'o',1, 0), outdf)
plot(aucsae, pch = 20)
# iforest 
dfifbc <- read.csv(file = 'dfifbc.Rdata')
aucsif <- get_auc(ifelse(dfbc[,ncol(dfbc)]== 'o',1, 0), dfifbc)
}
# rbm
dfrbmbc <- read.csv(file = 'dfrbmbc.Rdata')
aucsrbm <- get_auc(ifelse(dfbc[,ncol(dfbc)]== 'o',1, 0), dfrbmbc)

#auc4plot <- apply(dfrbmbc, 2, function(x) get_auc(ifelse(dfbc[,ncol(dfbc)]== 'o',1, 0),data.frame(x)))
# plot(auc4plot, pch = 20)

if(FALSE){ # I commented out these lines in order not to
  # have long run times when trying this script for the first time

## pg
# autoencoders
parspg <- list(lam = 0.001, # c(0.0005, 0.001, 0.005, 0.01),
               nhid = c(5, 10 , 8, 16),
               see = 1:10,
               name = "pg",
               nobs = 809)
outdf <- aecsvs2df(parspg, './')
aucs <- get_auc(ifelse(dfpg[,ncol(dfpg)]== 'o',1, 0), outdf)
plot(aucs)
# iforest
dfifpg <- read.csv(file = paste0(respath,'dspg/iforest/dfifpg.Rdata'))
aucsif <- get_auc(ifelse(dfpg[,ncol(dfpg)]== 'o',1, 0), dfifpg)
# rbm
dfrbmpg <- read.csv(file = 'dfrbmpg.Rdata')
aucsrbm <- get_auc(ifelse(dfpg[,ncol(dfpg)]== 'o',1, 0), dfrbmpg)

#auc4plot <- apply(dfrbmpg, 2, function(x) get_auc(ifelse(dfpg[,ncol(dfpg)]== 'o',1, 0),data.frame(x)))
#plot(auc4plot, pch = 20)


## le
# # autoencoders
parsle <- list(#lam = c(0.0005, 0.001, 0.005, 0.01),
               lam = c(0.001),
               nhid = c(5, 10, 16, 32),
               see = 1:10,
               name = "le",
               nobs = 1600)
outdf <- aecsvs2df(parsle, './')
aucs <- get_auc(ifelse(dfle[,ncol(dfle)]== 'o',1, 0), outdf)
plot(aucs, pch = 20)
# iforest
dfifle <- read.csv(file = paste0(respath,'dsle/iforest/dfifle.Rdata'))
aucsif <- get_auc(ifelse(dfle[,ncol(dfle)]== 'o',1, 0), dfifle)
# rbm
dfrbmle <- read.csv(file = 'dfrbmle.Rdata')
aucsrbm <- get_auc(ifelse(dfle[,ncol(dfle)]== 'o',1, 0), dfrbmle)

#auc4plot <- apply(dfrbmle, 2, function(x) get_auc(ifelse(dfle[,ncol(dfle)]== 'o',1, 0),data.frame(x)))
#plot(auc4plot, pch = 20)


## pl
# autoencoders
parspl <- list(lam = 0.001,#c(0.0005, 0.001, 0.005, 0.01),
               nhid = c(5, 10, 8, 16),
               see = 1:10,
               name = "pl",
               nobs = 6724)
outdf <- aecsvs2df(parspl, './')
aucs <- get_auc(ifelse(dfpl[,ncol(dfpl)]== 'o',1, 0), outdf)
plot(aucs, pch = 20)
# iforest
dfifpl <- read.csv(file = paste0(respath,'dspl/iforest/dfifpl.Rdata'))
aucsif <- get_auc(ifelse(dfpl[,ncol(dfpl)]== 'o',1, 0), dfifpl)
# rbm
dfrbmpl <- read.csv(file = 'dfrbmpl.Rdata')
aucsrbm <- get_auc(ifelse(dfpl[,ncol(dfpl)]== 'o',1, 0), dfrbmpl)

#auc4plot <- apply(dfrbmpl, 2, function(x) get_auc(ifelse(dfpl[,ncol(dfpl)]== 'o',1, 0),data.frame(x)))
#plot(auc4plot, pch = 20)

## an
# autoencoders
parsan <- list(lam = 0.001, #c(0.0005, 0.001, 0.005, 0.01),
               nhid = c(5, 10, 21),
               see = 1:10,
               name = "an",
               nobs = 6916)
outdf <- aecsvs2df(parsan, './')
aucs <- get_auc(ifelse(dfan[,ncol(dfan)]== 'o',1, 0), outdf)
plot(aucs)
# iforest
dfifan <- read.csv(file = paste0(respath,'dsan/iforest/dfifan.Rdata'))
aucsif <- get_auc(ifelse(dfan[,ncol(dfan)]== 'o',1, 0), dfifan)
# rbm
dfrbman <- read.csv(file = 'dfrbman.Rdata')
aucsrbm <- get_auc(ifelse(dfan[,ncol(dfan)]== 'o',1, 0), dfrbman)

#auc4plot <- apply(dfrbman, 2, function(x) get_auc(ifelse(dfan[,ncol(dfan)]== 'o',1, 0),data.frame(x)))
#plot(auc4plot, pch = 20)


## sa
# autoencoders
parssa <- list(lam = 0.001, #c(0.0005, 0.001, 0.005, 0.01),
               nhid = c(5, 10, 18, 36),
               see = 1:10,
               name = "sa",
               nobs = 5100)
outdf <- aecsvs2df(parssa, './')
aucs <- get_auc(ifelse(dfsa[,ncol(dfsa)]== 'o',1, 0), outdf)
# iforest
dfifsa <- read.csv(file = paste0(respath,'dssa/iforest/dfifsa.Rdata'))
aucsif <- get_auc(ifelse(dfsa[,ncol(dfsa)]== 'o',1, 0), dfifsa)
# rbm
dfrbmsa <- read.csv(file = 'dfrbmsa.Rdata')
aucsrbm <- get_auc(ifelse(dfsa[,ncol(dfsa)]== 'o',1, 0), dfrbmsa)

#auc4plot <- apply(dfrbmsa, 2, function(x) get_auc(ifelse(dfsa[,ncol(dfsa)]== 'o',1, 0),data.frame(x)))
#plot(auc4plot, pch = 20)

## sh
# autoencoders
parssh <- list(lam = 0.001, #c(0.0005, 0.001, 0.005, 0.01),
               nhid = c(5, 10, 4, 9),
               see = 1:10,
               name = "sh",
               nobs = 46464)
outdf <- aecsvs2df(parssh, './')
aucs <- get_auc(ifelse(dfsh[,ncol(dfsh)]== 'o',1, 0), outdf)
# iforest
dfifsh <- read.csv(file = paste0(respath,'dssh/iforest/dfifsh.Rdata'))
aucsif <- get_auc(ifelse(dfsh[,ncol(dfsh)]== 'o',1, 0), dfifsh)
# rbm
dfrbmsh <- read.csv(file = 'dfrbmsh.Rdata')
aucsrbm <- get_auc(ifelse(dfsh[,ncol(dfsh)]== 'o',1, 0), dfrbmsh)

#auc4plot <- apply(dfrbmsh, 2, function(x) get_auc(ifelse(dfsh[,ncol(dfsh)]== 'o',1, 0),data.frame(x)))
#plot(auc4plot, pch = 20)

## sp
# autoencoders
parssp <- list(lam = 0.001, #c(0.0005, 0.001, 0.005, 0.01),
               nhid = c(5, 10, 25, 50),
               see = 1:10,
               name = "sp",
               nobs = 3686)
outdf <- aecsvs2df(parssp, './')
aucs <- get_auc(ifelse(dfsp[,ncol(dfsp)]== 'o',1, 0), outdf)
# iforest
dfifsp <- read.csv(file = paste0(respath,'dssp/iforest/dfifsp.Rdata'))
aucsif <- get_auc(ifelse(dfsp[,ncol(dfsp)]== 'o',1, 0), dfifsp)
# rbm
dfrbmsp <- read.csv(file = 'dfrbmsp.Rdata')
aucsrbm <- get_auc(ifelse(dfsp[,ncol(dfsp)]== 'o',1, 0), dfrbmsp)

#auc4plot <- apply(dfrbmsp, 2, function(x) get_auc(ifelse(dfsp[,ncol(dfsp)]== 'o',1, 0),data.frame(x)))
#plot(auc4plot, pch = 20)


## al
# autoencoders
parsal <- list(lam = 0.001, #c(0.0005, 0.001, 0.005, 0.01),
               nhid = c(5, 10, 13, 27),
               see = 1:10,
               name = "al",
               nobs = 50000)
outdf <- aecsvs2df(parsal, './')
aucs <- get_auc(ifelse(dfal[,ncol(dfal)]== 'o',1, 0), outdf)
plot(aucs)
# iforest
dfifal <- read.csv(file = paste0(respath,'dsal/iforest/dfifal.Rdata'))
aucsif <- get_auc(ifelse(dfal[,ncol(dfal)]== 'o',1, 0), dfifal)
# rbm
dfrbmal <- read.csv(file = 'dfrbmal.Rdata')
aucsrbm <- get_auc(ifelse(dfal[,ncol(dfal)]== 'o',1, 0), dfrbmal)

#auc4plot <- apply(dfrbmal, 2, function(x) get_auc(ifelse(dfal[,ncol(dfal)]== 'o',1, 0),data.frame(x)))
#plot(auc4plot, pch = 20)

## kd
# autoencoders
parskd <- list(lam = 0.001, #c(0.0005, 0.001, 0.005, 0.01),
               nhid = c(5, 10, 14, 29),
               see = 1:10,
               name = "kd",
               nobs = 620098)
outdf <- aecsvs2df(parskd, './')
aucs <- get_auc(ifelse(dfkd[,ncol(dfkd)]== 'o',1, 0), outdf)
plot(aucs)
# iforest
dfifkd <- read.csv(file = paste0(respath,'dskd/iforest/dfifkd.Rdata'))
aucsif <- get_auc(ifelse(dfkd[,ncol(dfkd)]== 'o',1, 0), dfifkd)
# rbm
dfrbmkd <- read.csv(file = 'dfrbmkd.Rdata')
aucsrbm <- get_auc(ifelse(dfkd[,ncol(dfkd)]== 'o',1, 0), dfrbmkd)

#auc4plot <- apply(dfrbmkd, 2, function(x) get_auc(ifelse(dfkd[,ncol(dfkd)]== 'o',1, 0),data.frame(x)))
#plot(auc4plot, pch = 20)

}

# This script is used in the experiments with isolation forest 

source('general.R') # some general functions, used for RBM, autoencoder and iforest
(load(file='Goldstein.Rdata'))


require(IsolationForest)

exp_ifor <- function(DF){
  # Experiments with iForest. The code takes 10 different seeds and 
  # runs an isolation forest on the data for each seed. 
  # DF - dataframe of the input data set, including the target.
  # Output is a dataframe with 10 columns that contain the anomaly scores
  # for each of the 10 seeds.
  ds <- as.data.frame(scaleminmax(DF))
  dfifout <- data.frame(matrix(0, nrow = nrow(ds), ncol = 10))
  for(cnt in 1:10){
    set.seed(cnt)
    isof <- IsolationTrees(ds, ntree = 100,
                           hlim = 8,
                           rowSamp = TRUE,
                           nRowSamp = 256,
                           nmin = 1,
                           rFactor=1,
                           colSamp = FALSE,
                           nColSamp = ncol(ds))
    dfifout[,cnt] <- AnomalyScore(ds, isof)$outF
    # plot(dfifout[,cnt], main = paste0('seed: ', cnt), pch = 20)
  }
  return(dfifout)
}

# bc
dfifbc <- exp_ifor(dfbc)
write.csv(dfifbc, file = './dfifbc.Rdata', row.names = FALSE)

# pg
dfifpg <- exp_ifor(dfpg)
write.csv(dfifpg, file = './dfifpg.Rdata', row.names = FALSE)

# le
dfifle <- exp_ifor(dfle)
write.csv(dfifle, file = './dfifle.Rdata', row.names = FALSE)

# pl
dfifpl <- exp_ifor(dfpl)
write.csv(dfifpl, file = './dfifpl.Rdata', row.names = FALSE)

# an
dfifan <- exp_ifor(dfan)
write.csv(dfifan, file = './dfifan.Rdata', row.names = FALSE)

# sa
dfifsa <- exp_ifor(dfsa)
write.csv(dfifsa, file = './dfifsa.Rdata', row.names = FALSE)

# sh
dfifsh <- exp_ifor(dfsh)
write.csv(dfifsh, file = './dfifsh.Rdata', row.names = FALSE)

# sp
dfifsp <- exp_ifor(dfsp)
write.csv(dfifsp, file = './dfifsp.Rdata', row.names = FALSE)

# al
dfifal <- exp_ifor(dfal)
write.csv(dfifal, file = './dfifal.Rdata', row.names = FALSE)

# kd
dfifkd <- exp_ifor(dfkd)
write.csv(dfifkd, file = './dfifkd.Rdata', row.names = FALSE)

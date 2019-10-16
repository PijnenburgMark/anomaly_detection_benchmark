# This script is used in the experiments with auto-encoder 
outpath = './' # current directory

source('general.R') # some general functions, used for RBM, autoencoder and iforest
(load(file='Goldstein.Rdata'))

require(autoencoder)

autenc <- function(DS, LAMBDA, NHIDDEN, SEED, PATH){
  # function that calls the auto-encode function that trains the
  # auto-encoder. Then uses this trained auto-encoder to 
  # compute the reconstruction error. This is for us the 
  # outlier score. This score is saved in a csv-file.
  # DS      - a matrix with training data
  # LAMBDA  - learning rate 
  # NHIDDEN - number of hidden nodes
  # SEED    - seed that initializes the random generator
  # PATH    - path where the csv-file should be written
  set.seed(SEED)
  ae.fit <- autoencode(X.train = DS,
                       nl = 3, # 3 means input, hidden en output
                       N.hidden = NHIDDEN, 
                       unit.type = "logistic", 
                       lambda = LAMBDA,
                       beta = 0.05,
                       rho = 0.1,
                       epsilon = 0.01,
                       rescale.flag = FALSE # i prefer to do it outside the algorithm
                       ,max.iterations = 2000
  ) 
  
  ds.out <- predict.autoencoder(object = ae.fit, 
                                X.input = DS,
                                hidden.output = FALSE)
  
  outliersc <- rowSums(abs(ds.out$X.output - DS))
  write.csv(outliersc, file = paste0(PATH, 'out_', deparse(substitute(DS)),'_', LAMBDA, '_',
                                     NHIDDEN, '_', SEED, '.csv'))
}


# bc
dsbc <- scaleminmax(dfbc)
print(dim(dsbc))

timevec <- rep(0.0, times = 4*10)

colnum <- 1
for(lam in c(0.001)){
  for(nhid in c(5, 10 , floor(ncol(dsbc)/2), ncol(dsbc))){
    for(see in 1:10){
      print(paste('dsbc',lam, nhid, see))
      tic()
      autenc(dsbc, lam, nhid, see, outpath)
      timevec[colnum] <- toc()
      names(timevec)[colnum] <- paste0('V_', lam, '_', nhid, '_', see)
      colnum <- colnum + 1
    }
  }
}
write.csv(timevec, file = paste0(outpath, 'timevec_bc.csv'))

# pg
dspg <- scaleminmax(dfpg)
print(dim(dspg))

timevec <- rep(0.0, times = 4*10)

colnum <- 1
for(lam in c(0.001)){
  for(nhid in c(5, 10 , floor(ncol(dspg)/2), ncol(dspg))){
    for(see in 1:10){
      print(paste('dspg',lam, nhid, see))
      tic()
      autenc(dspg, lam, nhid, see, outpath)
      timevec[colnum] <- toc()
      names(timevec)[colnum] <- paste0('V_', lam, '_', nhid, '_', see)
      colnum <- colnum + 1
    }
  }
}
write.csv(timevec, file = paste0(outpath, 'timevec_pg.csv'))

# le
dsle <- scaleminmax(dfle)
print(dim(dsle))

timevec <- rep(0.0, times = 4*10)

colnum <- 1
for(lam in c(0.001)){
  for(nhid in c(5, 10 , floor(ncol(dsle)/2), ncol(dsle))){
    for(see in 1:10){
      print(paste('dsle',lam, nhid, see))
      tic()
      autenc(dsle, lam, nhid, see, outpath)
      timevec[colnum] <- toc()
      names(timevec)[colnum] <- paste0('V_', lam, '_', nhid, '_', see)
      colnum <- colnum + 1
    }
  }
}
write.csv(timevec, file = paste0(outpath, 'timevec_le.csv'))

# sp
dssp <- scaleminmax(dfsp)
print(dim(dssp))

timevec <- rep(0.0, times = 4*10)

colnum <- 1
for(lam in c(0.001)){
  for(nhid in c(5, 10 , 25, 50)){
    for(see in 1:10){
      print(paste('dssp',lam, nhid, see))
      tic()
      autenc(dssp, lam, nhid, see, outpath)
      timevec[colnum] <- toc()
      names(timevec)[colnum] <- paste0('V_', lam, '_', nhid, '_', see)
      colnum <- colnum + 1
    }
  }
}
write.csv(timevec, file = paste0(outpath, 'timevec_sp.csv'))

# sa
dssa <- scaleminmax(dfsa)
print(dim(dssa))

timevec <- rep(0.0, times = 4*10)

colnum <- 1
for(lam in c(0.001)){
  for(nhid in c(5, 10 , floor(ncol(dssa)/2), ncol(dssa))){
    for(see in 1:10){
      print(paste('dssa',lam, nhid, see))
      tic()
      autenc(dssa, lam, nhid, see, outpath)
      timevec[colnum] <- toc()
      names(timevec)[colnum] <- paste0('V_', lam, '_', nhid, '_', see)
      colnum <- colnum + 1
    }
  }
}
write.csv(timevec, file = paste0(outpath, 'timevec_sa.csv'))

# pl
dspl <- scaleminmax(dfpl)
print(dim(dspl))

timevec <- rep(0.0, times = 4*10)

colnum <- 1
for(lam in c(0.001)){
  for(nhid in c(5, 10 , floor(ncol(dspl)/2), ncol(dspl))){
    for(see in 1:10){
      print(paste('dspl',lam, nhid, see))
      tic()
      autenc(dspl, lam, nhid, see, outpath)
      timevec[colnum] <- toc()
      names(timevec)[colnum] <- paste0('V_', lam, '_', nhid, '_', see)
      colnum <- colnum + 1
    }
  }
}
write.csv(timevec, file = paste0(outpath, 'timevec_pl.csv'))

# an
dsan <- scaleminmax(dfan)
print(dim(dsan))

timevec <- rep(0.0, times = 4*10)

colnum <- 1
for(lam in c(0.001)){
  for(nhid in c(5, 10 , floor(ncol(dsan)/2), ncol(dsan))){
    for(see in 1:10){
      print(paste('dsan',lam, nhid, see))
      tic()
      autenc(dsan, lam, nhid, see, outpath)
      timevec[colnum] <- toc()
      names(timevec)[colnum] <- paste0('V_', lam, '_', nhid, '_', see)
      colnum <- colnum + 1
    }
  }
}
write.csv(timevec, file = paste0(outpath, 'timevec_an.csv'))

# sh
dssh <- scaleminmax(dfsh)
print(dim(dssh))

timevec <- rep(0.0, times = 4*10)

colnum <- 1
for(lam in c(0.001)){
  for(nhid in c(5, 10 , floor(ncol(dssh)/2), ncol(dssh))){
    for(see in 1:10){
      print(paste('dssh',lam, nhid, see))
      tic()
      autenc(dssh, lam, nhid, see, outpath)
      timevec[colnum] <- toc()
      names(timevec)[colnum] <- paste0('V_', lam, '_', nhid, '_', see)
      colnum <- colnum + 1
    }
  }
}
write.csv(timevec, file = paste0(outpath, 'timevec_sh.csv'))

# al
dsal <- scaleminmax(dfal)
print(dim(dsal))

timevec <- rep(0.0, times = 4*10)

colnum <- 1
for(lam in c(0.001)){
  for(nhid in c(5, 10 , floor(ncol(dsal)/2), ncol(dsal))){
    for(see in 1:10){
      print(paste('dsal',lam, nhid, see))
      tic()
      autenc(dsal, lam, nhid, see, outpath)
      timevec[colnum] <- toc()
      names(timevec)[colnum] <- paste0('V_', lam, '_', nhid, '_', see)
      colnum <- colnum + 1
    }
  }
}
write.csv(timevec, file = paste0(outpath, 'timevec_al.csv'))

# kd
dskd <- scaleminmax(dfkd)
print(dim(dskd))

timevec <- rep(0.0, times = 4*10)

colnum <- 1
for(lam in c(0.001)){
  for(nhid in c(5, 10 , floor(ncol(dskd)/2), ncol(dskd))){
    for(see in 1:10){
      print(paste('dskd',lam, nhid, see))
      tic()
      autenc(dskd, lam, nhid, see, outpath)
      timevec[colnum] <- toc()
      names(timevec)[colnum] <- paste0('V_', lam, '_', nhid, '_', see)
      colnum <- colnum + 1
    }
  }
}
write.csv(timevec, file = paste0(outpath, 'timevec_kd.csv'))





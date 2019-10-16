# This script is used in the experiments with RBM 


source('general.R') # some general functions, used for RBM, autoencoder and iforest
source('my_rbm.R') # contains the rbm function
(load(file='Goldstein.Rdata'))


exp_rbm <- function(DF, NHID){
  # The code takes 10 different seeds, 4
  # different bin sizes, and a vector with the number of hidden nodes and 
  # runs an RBM on the data for each seed. 
  # DF   - dataframe of the input data set, including the target.
  # NHID - an integer vector containing the number of hidden nodes that have to be tried
  # Output is a dataframe with 160 columns that contain the anomaly scores
  # for each of the 160 parameter settings.
  dsname <- deparse(substitute(DF))
  
  timevec <- rep(0.0, times = 4*4*10)
  dfrbmout <- data.frame(matrix(0, nrow = nrow(DF), ncol = 4*4*10))
  
  colnum <- 1
  for(nbin in c(3, 5, 7, 10)){ 
    ds <- discr_thermometer(DF, nbin)
    for(nhid in NHID){ 
      for(see in 1:10){ 
        set.seed(see)
        print(paste(dsname, 'experiment:', colnum,'nbin:', nbin, 'nhid:', nhid, 'seed:', see))
        tic()
        out_rbm <- rbm.train(x = ds, hidden = nhid, maxepochs = 100,
                             batchsize = 10, initial_learning_rate = 0.01, momentum = 0.95,
                             visible_type = "bin", hidden_type = "bin", cd = 1)
        rbm_learned <- out_rbm[[1]]
        totener <- out_rbm[[2]]
        #plot(totener, main = paste('nbin:', nbin, 'nhid:', nhid, 'seed:', see), 
        #     xlab = 'epoch number', ylab = 'total free energy',type = 'l')
        
        dfrbmout[, colnum] <- free_energy(ds, rbm_learned)
        timevec[colnum] <- toc()
        names(timevec)[colnum] <- paste0('V_', nbin, '_', nhid, '_', see)
        colnum <- colnum + 1
      }
    }
  }
  return(list(dfrbmout, timevec))
}


# bc
result <- exp_rbm(dfbc, c(5,10, floor(ncol(dfbc)/2), ncol(dfbc)))
dfrbmbc <- result[[1]]
timevec <- result[[2]]  
write.csv(dfrbmbc, file = 'dfrbmbc.Rdata', row.names = FALSE)
write.csv(timevec, file = 'timevec_bc.csv')

if(FALSE){ # I commented out these lines in order not to
           # have long run times when trying this script for the first time
# pg
result <- exp_rbm(dfpg, c(5,10, floor(ncol(dfpg)/2), ncol(dfpg)))
dfrbmpg <- result[[1]]
timevec <- result[[2]]  
write.csv(dfrbmpg, file = 'dfrbmpg.Rdata', row.names = FALSE)
write.csv(timevec, file = 'timevec_pg.csv')

# le
result <- exp_rbm(dfle, c(5,10, floor(ncol(dfle)/2), ncol(dfle)))
dfrbmle <- result[[1]]
timevec <- result[[2]]  
write.csv(dfrbmle, file = 'dfrbmle.Rdata', row.names = FALSE)
write.csv(timevec, file = 'timevec_le.csv')

# pl
result <- exp_rbm(dfpl, c(5,10, floor(ncol(dfpl)/2), ncol(dfpl)))
dfrbmpl <- result[[1]]
timevec <- result[[2]]  
write.csv(dfrbmpl, file = 'dfrbmpl.Rdata', row.names = FALSE)
write.csv(timevec, file = 'timevec_pl.csv')

# an
result <- exp_rbm(dfan, c(5,10, floor(ncol(dfan)/2), ncol(dfan)))
dfrbman <- result[[1]]
timevec <- result[[2]]  
write.csv(dfrbman, file = 'dfrbman.Rdata', row.names = FALSE)
write.csv(timevec, file = 'timevec_an.csv')

# sa
result <- exp_rbm(dfsa, c(5,10, floor(ncol(dfsa)/2), ncol(dfsa)))
dfrbmsa <- result[[1]]
timevec <- result[[2]]  
write.csv(dfrbmsa, file = 'dfrbmsa.Rdata', row.names = FALSE)
write.csv(timevec, file = 'timevec_sa.csv')

# sh
result <- exp_rbm(dfsh, c(5,10, floor(ncol(dfsh)/2), ncol(dfsh)))
dfrbmsh <- result[[1]]
timevec <- result[[2]]  
write.csv(dfrbmsh, file = 'dfrbmsh.Rdata', row.names = FALSE)
write.csv(timevec, file = 'timevec_sh.csv')

# sp
result <- exp_rbm(dfsp, c(5,10, 25, 50))
dfrbmsp <- result[[1]]
timevec <- result[[2]]  
write.csv(dfrbmsp, file = 'dfrbmsp.Rdata', row.names = FALSE)
write.csv(timevec, file = 'timevec_sp.csv')

# al 
result <- exp_rbm(dfal, c(5,10, floor(ncol(dfal)/2), ncol(dfal)))
dfrbmal <- result[[1]]
timevec <- result[[2]]  
write.csv(dfrbmal, file = 'dfrbmal.Rdata', row.names = FALSE)
write.csv(timevec, file = 'timevec_al.csv')

# kdd 
result <- exp_rbm(dfkd, c(5,10, floor(ncol(dfkd)/2), ncol(dfkd)))
dfrbmkd <- result[[1]]
timevec <- result[[2]]  
write.csv(dfrbmkd, file = 'dfrbmkd.Rdata', row.names = FALSE)
write.csv(timevec, file = 'timevec_kd.csv')

}

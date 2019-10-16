##  This is my adaptation of the code provided in the R-package "deepnet".
##  I adapted the stopping criterion and the adjustment of the learning rate,
##  both based on the total free energy change between epochs.
##
##' Training an RBM(restricted Boltzmann Machine)
##'
##' Training a RBM(restricted Boltzmann Machine)
##' @param x matrix of x values for examples
##' @param hidden number of hidden units
##' @param visible_type activation function of input unit.Only support "sigm" now
##' @param hidden_type activation function of hidden unit.Only support "sigm" now
##' @param learningrate learning rate for gradient descent. Default is 0.8.
##' @param momentum momentum for gradient descent. Default is 0.5 .
##' @param learningrate_scale  learning rate will be mutiplied by this scale after every iteration. Default is 1 .
##' @param numepochs number of iteration for samples  Default is 3.
##' @param batchsize size of mini-batch. Default is 100.
##' @param cd number of iteration for Gibbs sample of CD algorithm.
##' @examples
##' Var1 <- c(rep(1,50),rep(0,50))
##' Var2 <- c(rep(0,50),rep(1,50))
##' x3 <- matrix(c(Var1,Var2),nrow=100,ncol=2)
##' r1 <- rbm.train(x3,10,numepochs=20,cd=10)
##' @author Xiao Rong
##' @export

require(qgam) # for the function 'log1pexp'

rbm.train <- function(x, hidden,
                      maxepochs=3, batchsize=100,
                      initial_learning_rate=0.1, momentum=0.5,
                      visible_type="bin",hidden_type="bin",cd=1){
  
  if (!is.matrix(x)) 
    stop("x must be a matrix!")
  input_dim <- ncol(x)
  
  rbm <- list(
      size = c(input_dim, hidden),
      W = matrix(runif(hidden*input_dim,min=-0.1,max=0.1), c(hidden,input_dim)),
      vW = matrix(rep(0,hidden*input_dim), c(hidden,input_dim)),
      B = runif(input_dim,min=-0.1,max=0.1),
      vB = rep(0,input_dim),
      C = runif(hidden,min=-0.1,max=0.1),
      vC = rep(0,hidden),
      learningrate = initial_learning_rate,
      momentum = momentum,
      hidden_type = hidden_type, visible_type = visible_type,
      cd=cd
    )
  m <- nrow(x);
  numbatches <- m / batchsize;
  s <- 0
  totener <- 0
  stop = FALSE
  epochnr = 0
  while(!stop){
    epochnr = epochnr + 1
    randperm <- sample(1:m,m)
    if(numbatches >= 1){
      for(l in 1 : numbatches){
        s <- s + 1
        batch_x <- x[randperm[((l-1)*batchsize+1):(l*batchsize)], ] 
        rbm <- do.rbm.train(rbm,batch_x,s)
      }
    }
    #last fraction of sample
    if(numbatches > as.integer(numbatches)){
      batch_x <- x[randperm[(as.integer(numbatches)*batchsize):m], ]      
      s <- s + 1
      rbm <- do.rbm.train(rbm,batch_x,s)
    }
    
    totener <- c(totener, sum(free_energy(x, rbm)))
    
    # stopping criterion
    if(abs(totener[length(totener) - 1]) > 0.00000001){ # avoid dividing by 0
      if(abs((totener[length(totener)] - totener[length(totener) - 1])/ totener[length(totener) - 1]) < 0.001 | 
         epochnr >= maxepochs){
        stop = TRUE  
      }
    }
    
    # criterion for lowering learning rate
    if((totener[length(totener)] - totener[length(totener) - 1])/ totener[length(totener) - 1] > 0.01){
      rbm$learningrate <- rbm$learningrate / 10; 
      print(paste('Learning rate adjusted to: ', rbm$learningrate))
    } 
    
  }
  
  return(list(rbm, totener))
}

do.rbm.train <- function(rbm,batch_x,s){
  m <- nrow(batch_x)
  v1 <- batch_x
  h1 <- binary.state(rbm.up(rbm, v1))

  vn <- v1
  hn <- h1
  for(i in 1:rbm$cd){
    vn <- rbm.down(rbm, hn)
    hn <- rbm.up(rbm, vn)  
    #only last hidden state use probability real value
    if(i < rbm$cd){
      hn <- binary.state(hn);
    }
  }
  
  
  dW <- (t(h1) %*% v1 - t(hn)  %*% vn) / m
  # Mark: add next line to build in penalization, instead of the line after that
  # dW <- rbm$learningrate * dW  -0.001*rbm$W)
  dW <- rbm$learningrate * dW
  rbm$vW <- rbm$vW * rbm$momentum + dW
  dw <- rbm$vW
  rbm$W <- rbm$W + dW
  
  dB <- colMeans(v1 - vn)
  dB <- rbm$learningrate * dB
  rbm$vB <- rbm$vB * rbm$momentum + dB
  dB <- rbm$vB
  rbm$B <- rbm$B + dB
  
  dC <- colMeans(h1 - hn) 
  dC <- rbm$learningrate * dC
  rbm$vC <- rbm$vC * rbm$momentum + dC
  dC <- rbm$vC
  rbm$C <- rbm$C + dC
  
  rbm$e[s] <- sum((v1 - vn)^2)/m
  rbm
}


##' Infer hidden units state by visible units
##'
##' Infer hidden units states by visible units
##' @param rbm an rbm object trained by function train.rbm
##' @param v visible units states
##' @return hidden units states
##' @examples
##' Var1 <- c(rep(1,50),rep(0,50))
##' Var2 <- c(rep(0,50),rep(1,50))
##' x3 <- matrix(c(Var1,Var2),nrow=100,ncol=2)
##' r1 <- rbm.train(x3,3,numepochs=20,cd=10)
##' v <- c(0.2,0.8)
##' h <- rbm.up(r1,v)
##' @author Xiao Rong
##' @export
rbm.up <- function(rbm,v){
  m <- nrow(v)
  if(rbm$hidden_type == "bin"){
    sum <- t( t(v %*% t(rbm$W)) + rbm$C )
    h <- sigm( sum )
  }else{
    stop("only support binary state for rbm hidden layer!")
  }  
  h
}

##' Generate visible vector by hidden units states
##'
##' Generate visible vector by hidden units states
##' @param rbm an rbm object trained by function train.rbm
##' @param h hidden units states
##' @return generated visible vector
##' @examples
##' Var1 <- c(rep(1,50),rep(0,50))
##' Var2 <- c(rep(0,50),rep(1,50))
##' x3 <- matrix(c(Var1,Var2),nrow=100,ncol=2)
##' r1 <- rbm.train(x3,3,numepochs=20,cd=10)
##' h <- c(0.2,0.8,0.1)
##' v <- rbm.down(r1,h)
##' @author Xiao Rong
##' @export
rbm.down <- function(rbm,h){
  m <- nrow(h)
  if(rbm$visible_type == "bin"){
    sum <- t( t(h %*% rbm$W) + rbm$B )
    v <- sigm( sum )
  }else{
    stop("only support binary state for rbm hidden layer!")
  } 
  v
}

binary.state <- function(h){
  p <- matrix( runif(length(h),min=0,max=1)
               ,nrow=nrow(h),ncol=ncol(h))
  h[h>p] <- 1
  h[h<=p] <- 0
  h
}

sigm <- function(x){
  1/(1+exp(-x))
}

free_energy <- function(DF, RBM){
  # Function added by Mark.
  # This function computes the total free energy of the observations under the 
  # current model. We use of equation (24.25) of the Practical Guide
  # of Hinton.
  # DF   - dataframe where the rows are the input for the visible layer
  # RBM  - the current state of the RBM
  # Output is the free energy of the observations.

  HBmat <- matrix(rep(RBM["C"][[1]], nrow(DF)), nrow = nrow(DF), byrow=TRUE)
  x <- HBmat + DF %*% t(RBM["W"][[1]]) 
  return(-1 * DF %*% RBM["B"][[1]] - rowSums(log1pexp(x)))
}

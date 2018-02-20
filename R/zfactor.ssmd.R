zfactor.ssmd = function(dat, pos.cont, neg.cont, MainPlate, replicate){
  neg.cont0 = dat[[paste("S0", "_R", replicate, sep="")]][grep(neg.cont, as.character(dat[["welltype"]]))]
  pos.cont0 = dat[[paste("S0", "_R", replicate, sep="")]][grep(pos.cont, as.character(dat[["welltype"]]))]

  neg.cont1 = dat[[paste("S1", "_R", replicate, sep="")]][grep(neg.cont, as.character(dat[["welltype"]]))]
  pos.cont1 = dat[[paste("S1", "_R", replicate, sep="")]][grep(pos.cont, as.character(dat[["welltype"]]))]

  mean.neg.cont0 = mean(neg.cont0)
  var.neg.cont0 = var(neg.cont0)
  mean.pos.cont0 = mean(pos.cont0)
  var.pos.cont0 = var(pos.cont0)  
  cov.neg.cont.pos.cont0 = cov(neg.cont0, pos.cont0) 

  mean.neg.cont1 = mean(neg.cont1)
  var.neg.cont1 = var(neg.cont1)
  mean.pos.cont1 = mean(pos.cont1)
  var.pos.cont1 = var(pos.cont1)
  cov.neg.cont.pos.cont1 = cov(neg.cont1, pos.cont1) 
  
  SSMD0 = (mean.neg.cont0-mean.pos.cont0) / sqrt(var.neg.cont0+var.pos.cont0-2*cov.neg.cont.pos.cont0)
  SSMD1 = (mean.neg.cont1-mean.pos.cont1) / sqrt(var.neg.cont1+var.pos.cont1-2*cov.neg.cont.pos.cont1)

  ZFactor0 = 1-3*(sqrt(var.neg.cont0)+sqrt(var.pos.cont0))/abs(mean.pos.cont0-mean.neg.cont0)
  ZFactor1 = 1-3*(sqrt(var.neg.cont1)+sqrt(var.pos.cont1))/abs(mean.pos.cont1-mean.neg.cont1)

  res = data.frame(MainPlate, replicate, ZFactor0, ZFactor1, SSMD0, SSMD1)
  return(res)
}
  

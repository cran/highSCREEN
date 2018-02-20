normplate = function(mainplate, dat0, dat1, cmap, plate, replicate, norm="bscore", poscont=NULL, negcont=NULL){
  cnamesb = c(as.character(cmap[,1]), as.character(cmap[,2]))
  cnamesa = c(as.character(cmap[,1]), as.character(cmap[,2]))

  if (!is.null(poscont)){
    posb = mean(unlist(dat0[,c(1,ncol(dat0))])[grep(poscont, cnamesb)])
    posa = mean(unlist(dat1[,c(1,ncol(dat1))])[grep(poscont, cnamesa)])
  }

  if (!is.null(negcont)){
    negb = mean(unlist(dat0[,c(1,ncol(dat0))])[grep(negcont, cnamesb)])
    nega = mean(unlist(dat1[,c(1,ncol(dat1))])[grep(negcont, cnamesa)])
  }

  if (norm == "bscore"){
    dat0[,-c(1,ncol(dat0))] = medpolish(dat0[,-c(1,ncol(dat0))])[["residuals"]]/mad(dat0[,-c(1,ncol(dat0))])
    dat1[,-c(1,ncol(dat1))] = medpolish(dat1[,-c(1,ncol(dat1))])[["residuals"]]/mad(dat1[,-c(1,ncol(dat1))])
  }
  else if (norm == "zscore"){
    dat0[,-c(1,ncol(dat0))] = (dat0[,-c(1,ncol(dat0))]-mean(unlist(dat0[,-c(1,ncol(dat0))])))/sd(unlist(dat0[,-c(1,ncol(dat0))]))
    dat1[,-c(1,ncol(dat1))] = (dat1[,-c(1,ncol(dat1))]-mean(unlist(dat1[,-c(1,ncol(dat1))])))/sd(unlist(dat1[,-c(1,ncol(dat1))]))
  }
  else if (norm == "cscore"){
    dat0[,-c(1,ncol(dat0))] = (dat0[,-c(1,ncol(dat0))]-negb)/(posb-negb)*100
    dat1[,-c(1,ncol(dat1))] = (dat1[,-c(1,ncol(dat1))]-nega)/(posa-nega)*100
  }
  else if (norm == "raw"){
    print("raw")
  }
  else
    stop ("unknown normalization.")

  dat = rbind(dat0, dat1)

  dat = as.vector(t(dat))
  dat = data.frame(Time=c(rep(0, length(dat)/2),rep(1, length(dat)/2)), Plate=plate, Replicate=replicate, Norm=norm, well=rep(c(as.vector(sapply(LETTERS[seq(1:8)],function(x){sapply(seq(1,12), function(y){paste(x,y,sep="")})}))),2), row=rep(c(as.vector(sapply(LETTERS[seq(1:8)],function(x){rep(x,12)}))),2), col=rep(c(as.vector(sapply(seq(1:8),function(x){seq(1,12)}))),2), S=c(dat[1:(length(dat)/2)], dat[(length(dat)/2+1):length(dat)]))

  MainPlate = rep(mainplate, nrow(dat))
  welltype = rep("Compound", nrow(dat))
  welltype[seq(1,nrow(dat),12)] = c(as.character(cmap[,1]), as.character(cmap[,1]))
  welltype[seq(12,nrow(dat),12)] = c(as.character(cmap[,2]), as.character(cmap[,2]))
  dat = data.frame(MainPlate, dat, welltype)

  return(dat)
}

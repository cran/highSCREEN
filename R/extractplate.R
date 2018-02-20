extractplate = function(dat0, dat1, plate, replicate){
  dat0 = dat0[[replicate]]
  dat1 = dat1[[replicate]]

  if (plate == 1){
    dat0 = dat0[seq(1,nrow(dat0),2),]
    dat0 = dat0[,seq(1,24,2)]

    dat1 = dat1[seq(1,nrow(dat1),2),]
    dat1 = dat1[,seq(1,24,2)]
  }
  else if (plate == 2){
    dat0 = dat0[seq(1,nrow(dat0),2),]
    dat0 = dat0[,seq(2,24,2)]

    dat1 = dat1[seq(1,nrow(dat1),2),]
    dat1 = dat1[,seq(2,24,2)]
  }
  else if (plate == 3){
    dat0 = dat0[seq(2,nrow(dat0),2),]
    dat0 = dat0[,seq(1,24,2)]

    dat1 = dat1[seq(2,nrow(dat1),2),]
    dat1 = dat1[,seq(1,24,2)]
  }
  else if (plate == 4){
    dat0 = dat0[seq(2,nrow(dat0),2),]
    dat0 = dat0[,seq(2,24,2)]

    dat1 = dat1[seq(2,nrow(dat1),2),]
    dat1 = dat1[,seq(2,24,2)]
  }
  else 
    stop ("unknown plate.")

  datall = list(dat0=dat0, dat1=dat1)
  return(datall)
}

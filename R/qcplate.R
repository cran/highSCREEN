qcplate = function(dat, s0="S0", s1="S1", poscont="Control P", negcont="Control N", qc1.val=0.225, qc2.val=2, addcont, welltype="welltype"){
  addcont1 = sapply(addcont, function(x){mean(unlist(dat[grep(x, as.character(dat[[welltype]])), grep(s1, colnames(dat))]), na.rm=TRUE)})
  poscont1 = mean(unlist(dat[grep(poscont, as.character(dat[[welltype]])), grep(s1, colnames(dat))]), na.rm=TRUE)
  negcont1 = mean(unlist(dat[grep(negcont, as.character(dat[[welltype]])), grep(s1, colnames(dat))]), na.rm=TRUE)

  qc1 = subset(dat, welltype != "Compound")[,grep(s0, colnames(dat))]
  if (sum(qc1, na.rm=TRUE)==0)
    qc1 = NA
  else if(sum(qc1>=qc1.val, na.rm=TRUE)==0)
    qc1 = TRUE
  else
    qc1 = FALSE

  qc2 = dat[grep(poscont, as.character(dat[["welltype"]])),][,grep(s1, colnames(dat))]
  qc2 = colMeans(qc2)

  if (sum(qc2, na.rm=TRUE)==0)
    qc2 = NA
  else if(sum(qc2<=qc2.val, na.rm=TRUE)==0)
    qc2 = TRUE
  else
    qc2 = FALSE
 
  qc3 = all(diff(c(negcont1, addcont1, poscont1)) > 0)

  qc = ifelse(qc1 & qc2 & qc3, TRUE, FALSE)

  res = data.frame(passQC1=qc1, passQC2=qc2, passQC3=qc3, passQC=qc)

  return(res)
}

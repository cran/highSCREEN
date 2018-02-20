rankhits = function(dat, score.before="S0", score.after="S1", var="m1"){
  b = dat[,grep(score.before, colnames(dat))]
  m0 = rowMeans(b, na.rm=TRUE)
  s0 = apply(b, 1, sd, TRUE)
  rs0 = s0 / m0
  
  a = dat[,grep(score.after, colnames(dat))]
  m1 = rowMeans(a, na.rm=TRUE)
  s1 = apply(a, 1, sd, TRUE)
  rs1 = s1 / m1

  #compute IQR based on rs1
  rs1med = summary(rs1)[3]
  rsaq1 = summary(rs1)[2]
  rsaq3 = summary(rs1)[5]

  ind_below = rs1 > (rs1med - 1.5*(rsaq3-rsaq1))
  ind_above = rs1 < (rs1med + 1.5*(rsaq3-rsaq1))
  ind = ind_below & ind_above

  diff = m1 - m0
  res = data.frame(dat, diff, m0, s0, rs0, m1, s1, rs1, ind_below, ind_above, ind)
  ind = sort(res[[var]], index.return=TRUE, decreasing=TRUE)$ix
  res = res[ind,]

  return(res)
}
  

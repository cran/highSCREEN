plotplate = function(dat, score="S", main){
  x = as.matrix(dat[grep(score, colnames(dat))])
  rownames(x) = dat[["well"]]
  heatmap.2(x, cexRow=0.35, cexCol=0.6, srtCol=0, density.info="none", main=main)
}

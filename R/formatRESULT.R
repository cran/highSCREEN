formatRESULT = function(dat, replicate="Replicate", score="S", t="Time"){
dat1 = dat[dat[[replicate]]==1,]

urep = sort(unique(dat[[replicate]]))
dat.scores = sapply(urep, function(x){dat[dat[[replicate]]==x,][[score]]})
colnames(dat.scores) = paste(score, urep, sep="")
dat = data.frame(dat1[,colnames(dat1)!=replicate & colnames(dat1)!=score], dat.scores)

dat0 = dat[as.character(dat[[t]])==0,]
colnames(dat0)[grep(score, colnames(dat0))] = paste(score, "0_", "R", urep, sep="")

dat1 = dat[as.character(dat[[t]])==1,]
colnames(dat1)[grep(score, colnames(dat1))] = paste(score, "1_", "R", urep, sep="")

res = data.frame(dat0[,c("MainPlate","Plate", "Norm", "well", "row", "col", "welltype")], dat0[,paste(score, "0_", "R", urep, sep="")], dat1[,paste(score, "1_", "R", urep, sep="")])

res = data.frame(ID=paste(res[["MainPlate"]], res[["Plate"]], res[["well"]], sep="_"), res)
return(res)
}

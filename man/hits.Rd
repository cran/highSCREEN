\name{hits}
\alias{hits}

\title{
 Identify hits
}
\description{
 description
}
\usage{
hits(dat.raw, dat.norm, s0="S0", s1="S1", 
qc.mainplates, qc1.val=0.225, hit.val=3)
}

\arguments{
  \item{dat.raw}{
  Data frame containing raw data as an output from \eqn{formatRESULT()}. 
}
  \item{dat.norm}{
  Data frame containing normalized data set as an output from \eqn{formatRESULT()}. 
}
  \item{s0}{
  Specifies the name of the columns containing t0-specific scores.
}
  \item{s1}{
  Specifies the name of the columns containing t1-specific scores.
}
  \item{qc.mainplates}{
  A vector containing names of main plates that passed QC.
}
  \item{qc1.val}{
  Threshold value for QC1.
}
  \item{hit.val}{
  Threshold value for identifying candidate hits, based on the mean of t1-specific scores.
}
}

\value{
  The function returns a data frame. Each row corresponds to a compound that passed QC1 and belongs to a plate that passed overall QC. The data frame contains the following columns:
  \item{ID}{Has the complete information to identify a compound or control. It contains information about the main plate, the quadrant/plate and the well.}
  \item{MainPlate}{Specifies the main plate to wich the compound/control belongs.}
  \item{Plate}{Specifies the quadrant/plate to wich the compound/control belongs.}
  \item{Norm}{Specifies the normalization method that was applied for the specific compound.}
  \item{well}{Specifies the location (row and column) of the compound/control in the quadrant.}
  \item{row}{Specifies the row location in the quadrant.}
  \item{col}{Specifies the column location in the quadrant.}
  \item{welltype}{Specifies if the well is compound or control.}
  \item{S0}{Replicates of the score from the t0-specific data set.}
  \item{S1}{Replicates of the score from the t1-specific data set.}
  \item{IND2}{Indicator variable specifying if the compound passes (\eqn{TRUE}) or fails (\eqn{FALSE}) QC2.}
  \item{IND3}{Indicator variable specifying if the compound passes (\eqn{TRUE}) or fails (\eqn{FALSE}) QC3.}
}

\examples{
set.seed(1234)
nc = 24
nr = 16

# create 1st replicate of data matrix with compounds and controls
r1 = matrix(abs(rnorm(nr*nc)*0.01), nr, nc)

# create control map
cmap = data.frame(X1=c(rep("Control P", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control N", 
floor(nr/3))), X2=c(rep("Control N", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control P", floor(nr/3))))
cmap = cmap[seq(1,nr,2),]

# create 2nd replicate of data matrix with compounds and controls
r2 = matrix(abs(rnorm(nr*nc)*0.01), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
r3 = matrix(abs(rnorm(nr*nc)*0.01), nr, nc)

# combine all replicates for the t0-specific data
replicates_t0 = list(r1, r2, r3)
names(replicates_t0) = c("R1", "R2", "R3")

# create 1st replicate of data matrix with compounds and controls
r1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 2nd replicate of data matrix with compounds and controls
r2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
r3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all replicates for the t1-specific data
replicates_t1 = list(r1, r2, r3)
names(replicates_t1) = c("R1", "R2", "R3")

# extract plate 1, replicate 1
dat1 = extractplate(replicates_t0, replicates_t1, plate=1, replicate=1)

# extract plate 1, replicate 2
dat2 = extractplate(replicates_t0, replicates_t1, plate=1, replicate=2)

# extract plate 1, replicate 3
dat3 = extractplate(replicates_t0, replicates_t1, plate=1, replicate=3)

# no normalizion
datraw1 = normplate("Main Plate 1", dat1[["dat0"]], dat1[["dat1"]], cmap,
 plate=1, replicate=1, norm="raw")
datraw2 = normplate("Main Plate 1", dat2[["dat0"]], dat2[["dat1"]], cmap,
 plate=1, replicate=2, norm="raw")
datraw3 = normplate("Main Pltae 1", dat3[["dat0"]], dat3[["dat1"]], cmap,
 plate=1, replicate=3, norm="raw")

# combine 3 replicate
datraw = rbind(datraw1, datraw2, datraw3)

# reformat result
datraw = formatRESULT(datraw, replicate="Replicate", t="Time")

# c-score normalization
datnorm1 = normplate("Main Plate 1", dat1[["dat0"]], dat1[["dat1"]], cmap,
 plate=1, replicate=1, norm="cscore", 
poscont="Control P", negcont="Control N")
datnorm2 = normplate("Main Plate 1", dat2[["dat0"]], dat2[["dat1"]], cmap,
 plate=1, replicate=2, norm="cscore", 
poscont="Control P", negcont="Control N")
datnorm3 = normplate("Main Pltae 1", dat3[["dat0"]], dat3[["dat1"]], cmap,
 plate=1, replicate=3, norm="cscore",
poscont="Control P", negcont="Control N")

# combine 3 replicates
datnorm = rbind(datnorm1, datnorm2, datnorm3)

# reformat result
datnorm = formatRESULT(datnorm, replicate="Replicate", t="Time")

# identify hits
head(hits(datraw, datnorm, qc.mainplates="Main Plate 1", qc1.val=0.225, hit.val=3))
}

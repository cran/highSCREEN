\name{rankhits}
\alias{rankhits}

\title{
 Rank hits
}
\description{
 This function ranks the candidate compound hits based on the mean of t1-specific scores.
}
\usage{
rankhits(dat, score.before="S0", score.after="S1", var="m1")
}

\arguments{
  \item{dat}{
  Data frane as the output of the function \eqn{formatRESULT}.
}
  \item{score.before}{
  Specifies the name of the columns containing t0-specific scores.
}
  \item{score.after}{
  Specifies the name of the columns containing t1-specific scores.
}
  \item{var}{
  Specifies the variable according to which the ranking is done.
}
}

\value{
  Data frame with a row for each hit. The columns are as follows:
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
  \item{diff}{Difference between mean of t1 and t0 replicates.}
  \item{m0}{Mean of t0-specific replicates (MB).}
  \item{s0}{Standard deviation of t0-specific replicates (SB).}
  \item{rs0}{Coefficient of variation of t0-specific replicates. It is the ratio of S0 and M0.}
  \item{m1}{Mean of t1-specific replicates (MA).}
  \item{s1}{Standard deviation of t1-specific replicates (SA)}
  \item{rs1}{Coefficient of variation of t1-specific replicates. It is the ratio of S1 and M1.}
  \item{ind_below}{see \eqn{ind} below}
  \item{ind_above}{see \eqn{ind} below}
  \item{ind}{\eqn{TRUE} (if both \eqn{ind_below} and \eqn{ind_above} are \eqn{TRUE}) if RS1 is within the \eqn{1.5 \times IQR}, where interquartile range (\eqn{IQR}) is computed based on all hits.}
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

# combine 3 replicates
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
h = hits(datraw, datnorm, qc.mainplates="Main Plate 1", qc1.val=0.225, hit.val=3)

# rank hits in disending order of mean of t1-specific replicate scores "m1"
head(rankhits(h))
}

\name{hits}
\alias{hits}

\title{
 title
}
\description{
 description
}
\usage{
hits(dat.raw, dat.norm, score.before="scorebefore", score.after="scoreafter", 
qc.mainplates, qc1.val=0.225, hit.val=3)
}

\arguments{
  \item{dat.raw}{
  Data frame containing raw data as an output from \eqn{formatRESULT()}. 
}
  \item{dat.norm}{
  Data frame containing normalized data set as an output from \eqn{formatRESULT()}. 
}
  \item{score.before}{
  Specifies the name of the columns containing "Before" scores.
}
  \item{score.after}{
  Specifies the name of the columns containing "After" scores.
}
  \item{qc.mainplates}{
  A vector containing names of main plates that passed QC.
}
  \item{qc1.val}{
  Threshold value for QC1.
}
  \item{hit.val}{
  Threshold value for identifying candidate hits, based on the mean of "After" scores.
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
  \item{scorebefore}{Replicates of the score from the "Before" data set.}
  \item{scoreafter}{Replicates of the score from the "After" data set.}
  \item{IND2}{Indicator variable specifying if the compound passes (\eqn{TRUE}) or fails (\eqn{FALSE}) QC2.}
  \item{IND3}{Indicator variable specifying if the compound passes (\eqn{TRUE}) or fails (\eqn{FALSE}) QC3.}
}

\examples{
set.seed(1234)
nc = 24
nr = 16

# create 1st triplicate of data matrix with compounds and controls
replicate1 = matrix(abs(rnorm(nr*nc)*0.01), nr, nc)

# create control map
cmap = data.frame(X1=c(rep("Control P", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control N", 
floor(nr/3))), X2=c(rep("Control N", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control P", floor(nr/3))))
cmap = cmap[seq(1,nr,2),]

# create 2nd triplicate of data matrix with compounds and controls
replicate2 = matrix(abs(rnorm(nr*nc)*0.01), nr, nc)

# create 3rd triplicate of data matrix with compounds and controls
replicate3 = matrix(abs(rnorm(nr*nc)*0.01), nr, nc)

# combine all triplicates for the before data
replicates_before = list(replicate1, replicate2, replicate3)
names(replicates_before) = c("Replicate1", "Replicate2", "Replicate3")

# create 1st triplicate of data matrix with compounds and controls
replicate1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 2nd triplicate of data matrix with compounds and controls
replicate2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd triplicate of data matrix with compounds and controls
replicate3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all triplicates for the after data
replicates_after = list(replicate1, replicate2, replicate3)
names(replicates_after) = c("Replicate1", "Replicate2", "Replicate3")

# extract plate 1, triplicate 1
dat1 = extractplate(replicates_before, replicates_after, plate=1, replicate=1)

# extract plate 1, triplicate 2
dat2 = extractplate(replicates_before, replicates_after, plate=1, replicate=2)

# extract plate 1, triplicate 3
dat3 = extractplate(replicates_before, replicates_after, plate=1, replicate=3)

# no normalizion
datraw1 = normplate("Main Plate 1", dat1[["datbefore"]], dat1[["datafter"]], cmap,
 plate=1, triplicate=1, norm="raw")
datraw2 = normplate("Main Plate 1", dat2[["datbefore"]], dat2[["datafter"]], cmap,
 plate=1, triplicate=2, norm="raw")
datraw3 = normplate("Main Pltae 1", dat3[["datbefore"]], dat3[["datafter"]], cmap,
 plate=1, triplicate=3, norm="raw")

# combine 3 triplicates
datraw = rbind(datraw1, datraw2, datraw3)

# reformat result
datraw = formatRESULT(datraw, triplicate="Triplicate", score="score", t="Time")

# c-score normalization
datnorm1 = normplate("Main Plate 1", dat1[["datbefore"]], dat1[["datafter"]], cmap,
 plate=1, triplicate=1, norm="cscore", 
poscont="Control P", negcont="Control N")
datnorm2 = normplate("Main Plate 1", dat2[["datbefore"]], dat2[["datafter"]], cmap,
 plate=1, triplicate=2, norm="cscore", 
poscont="Control P", negcont="Control N")
datnorm3 = normplate("Main Pltae 1", dat3[["datbefore"]], dat3[["datafter"]], cmap,
 plate=1, triplicate=3, norm="cscore",
poscont="Control P", negcont="Control N")

# combine 3 triplicates
datnorm = rbind(datnorm1, datnorm2, datnorm3)

# reformat result
datnorm = formatRESULT(datnorm, triplicate="Triplicate", score="score", t="Time")

# identify hits
head(hits(datraw, datnorm, qc.mainplates="Main Plate 1", qc1.val=0.225, hit.val=3))
}

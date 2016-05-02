\name{rankhits}
\alias{rankhits}

\title{
 title
}
\description{
 This function ranks the candidate compound hits based on the mean of "After" scores.
}
\usage{
rankhits(dat, score.before="scorebefore", score.after="scoreafter", var="ma")
}

\arguments{
  \item{dat}{
  Data frane as the output of the function \eqn{formatRESULT}.
}
  \item{score.before}{
  Specifies the name of the columns containing "Before" scores.
}
  \item{score.after}{
  Specifies the name of the columns containing "After" scores.
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
  \item{scorebefore}{Replicates of the score from the "Before" data set.}
  \item{scoreafter}{Replicates of the score from the "After" data set.}
  \item{IND2}{Indicator variable specifying if the compound passes (\eqn{TRUE}) or fails (\eqn{FALSE}) QC2.}
  \item{IND3}{Indicator variable specifying if the compound passes (\eqn{TRUE}) or fails (\eqn{FALSE}) QC3.}
  \item{diff}{Difference between mean of "After" and "Before" replicates.}
  \item{mb}{Mean of "Before" replicates (MB).}
  \item{sb}{Standard deviation of "Before" replicates (SB).}
  \item{rsb}{Coefficient of variation of "Before" replicates. It is the ratio of SB and MB.}
  \item{ma}{Mean of "After" replicates (MA).}
  \item{sa}{Standard deviation of "After" replicates (SA)}
  \item{rsa}{Coefficient of variation of "After" replicates. It is the ratio of SB and MB.}
  \item{ind_below}{see \eqn{ind} below}
  \item{ind_above}{see \eqn{ind} below}
  \item{ind}{\eqn{TRUE} (if both \eqn{ind_below} and \eqn{ind_above} are \eqn{TRUE}) if RSA is within the \eqn{1.5 \times IQR}, where interquartile range (\eqn{IQR}) is computed based on all hits.}
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

# extract plate 1, replicate 1
dat1 = extractplate(replicates_before, replicates_after, plate=1, replicate=1)
# extract plate 1, replicate 2
dat2 = extractplate(replicates_before, replicates_after, plate=1, replicate=2)
# extract plate 1, replicate 3
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
h = hits(datraw, datnorm, qc.mainplates="Main Plate 1", qc1.val=0.225, hit.val=3)

# rank hits in disending order of mean of "after" replicate scores "ma"
head(rankhits(h))
}

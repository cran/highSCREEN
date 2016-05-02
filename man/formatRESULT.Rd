\name{formatRESULT}
\alias{formatRESULT}

\title{
 title
}
\description{
 description
}
\usage{
formatRESULT(dat, triplicate="Triplicate", score="score", t="Time")
}

\arguments{
  \item{dat}{
  Data frame such as the return data frame of the function \eqn{normplate()}.
}
  \item{triplicate}{
  Specifies the column name of the input data frame that contains the triplicate numbers.
}
  \item{score}{
  Specifies the column name of the input data frame that contains the normalized score values.
}
  \item{t}{
  Specifies the column name of the input data frame that contains the time designations ("Before" and "After").
}
}

\value{
  This function returns a data frame with each row corresponding to a compound/control and the following columns:
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
}

\examples{
set.seed(1234)
nc = 24
nr = 16

# create 1st triplicate of data matrix with compounds and controls
replicate1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create control map
cmap = data.frame(X1=c(rep("Control P", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control N", 
floor(nr/3))), X2=c(rep("Control N", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control P", floor(nr/3))))
cmap = cmap[seq(1,nr,2),]

# create 2nd triplicate of data matrix with compounds and controls
replicate2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd triplicate of data matrix with compounds and controls
replicate3 = matrix(abs(rnorm(nr*nc)), nr, nc)

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

# extract plate 1, all triplicates
dat1 = extractplate(replicates_before, replicates_after, plate=1, replicate=1)
dat2 = extractplate(replicates_before, replicates_after, plate=1, replicate=2)
dat3 = extractplate(replicates_before, replicates_after, plate=1, replicate=3)

# normalize data of all triplicates
res1 = normplate("Main Plate 1", dat1[["datbefore"]], dat1[["datafter"]], cmap, 
plate=1, triplicate=1, norm="zscore") 
res2 = normplate("Main Plate 1", dat2[["datbefore"]], dat2[["datafter"]], cmap, 
plate=1, triplicate=2, norm="zscore")
res3 = normplate("Main Plate 1", dat3[["datbefore"]], dat3[["datafter"]], cmap, 
plate=1, triplicate=3, norm="zscore")

# reformat data of all triplicates
head(formatRESULT(rbind(res1, res2, res3), triplicate="Triplicate", score="score", t="Time"))
}

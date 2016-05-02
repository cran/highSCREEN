\name{normplate}
\alias{normplate}

\title{
 title
}
\description{
 description
}
\usage{
normplate(mainplate, datbefore, datafter, cmap, plate, triplicate, norm="bscore",
 poscont=NULL, negcont=NULL)
}

\arguments{
  \item{mainplate}{
  Main Plate designation.
}
  \item{datbefore}{
  Data frame containing the 96-well "Before" plate/quadrant to be normalized.
}
  \item{datafter}{
  Data frame containing the 96-well "After" plate/quadrant to be normalized.
}
  \item{cmap}{
  96-well plate control map.
}
  \item{plate}{
  Plate/quadrant designation. Ranges from 1 to 4.
}
  \item{triplicate}{
  Triplicate designation. Ranges from 1 to 3.
}
  \item{norm}{
  Normalization method to be applied. Currenlty implemented methods include C-score (\eqn{cscore}), B-score (\eqn{bscore}), Z-score (eqn{zscore}) and no normalization (\eqn{raw}).
}
  \item{poscont}{
  Specifies positive control designation (used in C-score normalization). 
}
  \item{negcont}{
  Specifies negative control designation (used in C-score normalization). 
}
}

\value{
  Data frame containing the following columns:
  \item{MainPlate}{Specifies the main plate to wich the compound/control belongs.}
  \item{Time}{Specifies "Before" or "After".}
  \item{Plate}{Specifies the quadrant/plate to wich the compound/control belongs.}
  \item{Triplicate}{Specifies triplicate.}
  \item{Norm}{Specifies the normalization method that was applied for the specific compound.}
  \item{well}{Specifies the location (row and column) of the compound/control in the quadrant.}
  \item{row}{Specifies the row location in the quadrant.}
  \item{col}{Specifies the column location in the quadrant.}
  \item{score}{Shows the score value.}
  \item{welltype}{Specifies if the well is compound or control.}
}

\examples{
set.seed(1234)
nc = 24
nr = 16

# create control map
cmap = data.frame(X1=c(rep("Control P", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control N", 
floor(nr/3))), X2=c(rep("Control N", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control P", floor(nr/3))))
cmap = cmap[seq(1,nr,2),]
cmap

# create 1st replicate of data matrix with compounds and controls
replicate1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 2nd replicate of data matrix with compounds and controls
replicate2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
replicate3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all replicate for the before data
replicates_before = list(replicate1, replicate2, replicate3)
names(replicates_before) = c("Replicate1", "Replicate2", "Replicate3")

# create 1st replicate of data matrix with compounds and controls
replicate1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 2nd replicate of data matrix with compounds and controls
replicate2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
replicate3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all replicate for the after data
replicates_after = list(replicate1, replicate2, replicate3)
names(replicates_after) = c("Replicate1", "Replicate2", "Replicate3")

# extract plate 1, replicate 1
dat = extractplate(replicates_before, replicates_after, plate=1, replicate=1)

# normalize using c-score
head(normplate("Main Plate 1", dat[["datbefore"]], dat[["datafter"]], cmap, 
plate=1, triplicate=1, norm="cscore",
 poscont="Control P", negcont="Control N"))

# normalize using b-score (medpolish)
head(normplate("Main Plate 1", dat[["datbefore"]], dat[["datafter"]], cmap, 
plate=1, triplicate=1, norm="bscore"))

# normalize using z-score
head(normplate("Main Plate 1", dat[["datbefore"]], dat[["datafter"]], cmap, 
plate=1, triplicate=1, norm="zscore"))
}

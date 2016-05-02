\name{qcplate}
\alias{qcplate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 title
}
\description{
 description
}
\usage{
qcplate(dat, score.before="scorebefore", score.after="scoreafter", poscont="Control P",
 negcont="Control N", qc1.val=0.225, qc2.val=2, addcont, welltype="welltype")
}

\arguments{
  \item{dat}{

}
  \item{score.before}{
  Specifies the name of the columns containing "Before" scores.
}
  \item{score.after}{
  Specifies the name of the columns containing "After" scores.
}
  \item{poscont}{
  Specifies positive control designation (used in C-score normalization). 
}
  \item{negcont}{
  Specifies negative control designation (used in C-score normalization). 
}
  \item{qc1.val}{
  Pre-defined threshold value for QC1.
}
  \item{qc2.val}{
  Pre-defined threshold value for QC2.
}
  \item{addcont}{
  A vector character names specifying the additional controls for QC3. 
  The vignette provides more details on the particular order of specifying the additional controls.
}
  \item{welltype}{
  Specifies the name for the column defining the well type.
}
}
\details{
 details
}
\value{
  value
}
\references{
 references
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
dat11 = extractplate(replicates_before, replicates_after, plate=1, replicate=1)

# extract plate 1, triplicate 2
dat12 = extractplate(replicates_before, replicates_after, plate=1, replicate=2)

# extract plate 1, triplicate 3
dat13 = extractplate(replicates_before, replicates_after, plate=1, replicate=3)

# no normalizion (norm="raw")
res11 = normplate("Main Plate 1", dat11[["datbefore"]], dat11[["datafter"]], cmap,
 plate=1, triplicate=1, norm="raw")
res12 = normplate("Main Plate 1", dat12[["datbefore"]], dat12[["datafter"]], cmap,
 plate=1, triplicate=2, norm="raw")
res13 = normplate("Main Pltae 1", dat13[["datbefore"]], dat13[["datafter"]], cmap,
 plate=1, triplicate=3, norm="raw")

# combine 3 replicates
res1 = rbind(res11, res12, res13)

# reformat result
res1 = formatRESULT(res1, triplicate="Triplicate", score="score", t="Time")

# perform QC
qcplate(res1, poscont="Control P", negcont="Control N", qc1.val=0.225, qc2.val=2, 
addcont=c("Control low", "Control med", "Control high"), welltype="welltype")
}

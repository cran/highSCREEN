\name{qcplate}
\alias{qcplate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Quality control
}
\description{
 description
}
\usage{
qcplate(dat, s0="S0", s1="S1", poscont="Control P",
 negcont="Control N", qc1.val=0.225, qc2.val=2, addcont, welltype="welltype")
}

\arguments{
  \item{dat}{

}
  \item{s0}{
  Specifies the name of the columns containing t0-specific scores.
}
  \item{s1}{
  Specifies the name of the columns containing t1-specific scores.
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

# create 1st replicate of data matrix with compounds and controls
r1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create control map
cmap = data.frame(X1=c(rep("Control P", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control N", 
floor(nr/3))), X2=c(rep("Control N", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control P", floor(nr/3))))
cmap = cmap[seq(1,nr,2),]

# create 2nd replicate of data matrix with compounds and controls
r2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
r3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all replicates for the t0-specific data
replicates_t0 = list(r1, r2, r3)

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
dat11 = extractplate(replicates_t0, replicates_t1, plate=1, replicate=1)

# extract plate 1, replicate 2
dat12 = extractplate(replicates_t0, replicates_t1, plate=1, replicate=2)

# extract plate 1, replicate 3
dat13 = extractplate(replicates_t0, replicates_t1, plate=1, replicate=3)

# no normalizion (norm="raw")
res11 = normplate("Main Plate 1", dat11[["dat0"]], dat11[["dat1"]], cmap,
 plate=1, replicate=1, norm="raw")
res12 = normplate("Main Plate 1", dat12[["dat0"]], dat12[["dat1"]], cmap,
 plate=1, replicate=2, norm="raw")
res13 = normplate("Main Pltae 1", dat13[["dat0"]], dat13[["dat1"]], cmap,
 plate=1, replicate=3, norm="raw")

# combine 3 replicates
res1 = rbind(res11, res12, res13)

# reformat result
res1 = formatRESULT(res1, replicate="Replicate", t="Time")

# perform QC
qcplate(res1, poscont="Control P", negcont="Control N", qc1.val=0.225, qc2.val=2, 
addcont=c("Control low", "Control med", "Control high"), welltype="welltype")
}

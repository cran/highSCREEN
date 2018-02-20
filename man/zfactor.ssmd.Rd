\name{zfactor.ssmd}
\alias{zfactor.ssmd}

\title{
 Compute Z-factor and SSMD
}
\description{
 This function computes the Z-factor and strictly standardized mean difference (SSMD) of a given 96-well plate.
}
\usage{
zfactor.ssmd(dat, pos.cont, neg.cont, MainPlate, replicate)
}

\arguments{
  \item{dat}{
  Data frame as an output of the function \eqn{formatRESULT()}.
}
  \item{pos.cont}{
  Designation of positive control.
}
  \item{neg.cont}{
  Designation of negative control.
}
  \item{MainPlate}{
  Specifies main plate.
}
  \item{replicate}{
  Specifies the replicate.
}
}

\value{
  Returns a data frame with one row and the following columns:
  \item{MainPlate}{Specifies the main plate.}
  \item{replicate}{Specifies the replicate.}
  \item{ZFactor_Before}{Specifies the Z-factor computed based on the t0-specific data.}
  \item{ZFactor_After}{Specifies the Z-factor computed based on the t1-specific data.}
  \item{SSMD_Before}{Specifies SSMD computed based on the t0-specific data.}
  \item{SSMD_After}{Specifies SSMD computed based on the t1-specific data.}
}

\examples{
set.seed(1234)
nc = 24
nr = 16

# create 1st replicate of data matrix with compounds and controls
replicate1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create control map
cmap = data.frame(X1=c(rep("Control P", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control N", 
floor(nr/3))), X2=c(rep("Control N", floor(nr/3)), 
rep(c("Control low", "Control med", "Control high"), 
(floor(nr/3)+nr-3*floor(nr/3))/3), rep("Control P", floor(nr/3))))
cmap = cmap[seq(1,nr,2),]

# create 2nd replicate of data matrix with compounds and controls
replicate2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
replicate3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all replicates for the t0-specific data
replicates_before = list(replicate1, replicate2, replicate3)
names(replicates_before) = c("Replicate1", "Replicate2", "Replicate3")

# create 1st replicate of data matrix with compounds and controls
replicate1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 2nd replicate of data matrix with compounds and controls
replicate2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
replicate3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all replicates for the t1-specific data
replicates_after = list(replicate1, replicate2, replicate3)
names(replicates_after) = c("Replicate1", "Replicate2", "Replicate3")

# extract plate 1, replicate 1
dat1 = extractplate(replicates_before, replicates_after, plate=1, replicate=1)
# extract plate 1, replicate 2
dat2 = extractplate(replicates_before, replicates_after, plate=1, replicate=2)
# extract plate 1, replicate 3
dat3 = extractplate(replicates_before, replicates_after, plate=1, replicate=3)

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

# compute z-factor and ssmd for each raw compound, replicate 1
zfactor.ssmd(datraw, "Control P", "Control N", "Main Plate 1", 1)
}

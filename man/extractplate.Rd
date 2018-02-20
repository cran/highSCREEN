\name{extractplate}
\alias{extractplate}

\title{
 Extract plate
}
\description{
 description
}
\usage{
extractplate(dat0, dat1, plate, replicate)
}

\arguments{
  \item{dat0}{
  A list consisting of the following elements for the t0-specific data set:
  \eqn{Replicate1}: Data frame containing the first triplicate and 4 additional columns specifying the controls and their plate locations.
  \eqn{Replicate2}: Data frame containing the second triplicate and 4 additional columns specifying the controls and their plate locations.
  \eqn{Replicate3}: Data frame containing the third triplicate and 4 additional columns specifying the controls and their plate locations.
}
  \item{dat1}{
  A list consisting of the following elements for the t1-specific data set:
  \eqn{Replicate1}: Data frame containing the first triplicate and 4 additional columns specifying the controls and their plate locations.
  \eqn{Replicate2}: Data frame containing the second triplicate and 4 additional columns specifying the controls and their plate locations.
  \eqn{Replicate3}: Data frame containing the third triplicate and 4 additional columns specifying the controls and their plate locations.
}
  \item{plate}{
  Specifies which of the 4 plates to be extracted.
}
  \item{replicate}{
  Specfies which replicate to be extracted.
}
}

\value{
  This function returns a list consisting of the following elements:
  \item{dat0}{Data frame containing the t0-specific plate of compounds and controls.}
  \item{dat1}{Data frame containing the t1-specific plate of compounds and controls.}
}

\examples{
set.seed(1234)
nc = 24
nr = 16

# create 1st replicate of data matrix with compounds and controls
r1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 2nd replicate of data matrix with compounds and controls
r2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
r3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all replicate to create t0-specific data set
replicates_t0 = list(r1, r2, r3)
names(replicates_t0) = c("R1", "R2", "R3")

# create 1st replicate of data matrix with compounds and controls
r1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 2nd replicate of data matrix with compounds and controls
r2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
r3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all replicate to create t1-specific data set
replicates_t1 = list(r1, r2, r3)
names(replicates_t1) = c("R1", "R2", "R3")

# extract plate 3, replicate 2
extractplate(replicates_t0, replicates_t1, plate=3, replicate=2)
}

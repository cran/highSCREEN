\name{extractplate}
\alias{extractplate}

\title{
 title
}
\description{
 description
}
\usage{
extractplate(datbefore, datafter, plate, replicate)
}

\arguments{
  \item{datbefore}{
  A list consisting of the following elements for the "Before" data set:
  \eqn{Replicate1}: Data frame containing the first triplicate and 4 additional columns specifying the controls and their plate locations.
  \eqn{Replicate2}: Data frame containing the second triplicate and 4 additional columns specifying the controls and their plate locations.
  \eqn{Replicate3}: Data frame containing the third triplicate and 4 additional columns specifying the controls and their plate locations.
}
  \item{datafter}{
  A list consisting of the following elements for the "After" data set:
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
  \item{datbefore}{Data frame containing the "Before" plate of compounds and controls.}
  \item{datafter}{Data frame containing the "After" plate of compounds and controls.}
}

\examples{
set.seed(1234)
nc = 24
nr = 16

# create 1st replicate of data matrix with compounds and controls
replicate1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 2nd replicate of data matrix with compounds and controls
replicate2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
replicate3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all replicate to create "before" data set
replicates_before = list(replicate1, replicate2, replicate3)
names(replicates_before) = c("Replicate1", "Replicate2", "Replicate3")

# create 1st replicate of data matrix with compounds and controls
replicate1 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 2nd replicate of data matrix with compounds and controls
replicate2 = matrix(abs(rnorm(nr*nc)), nr, nc)

# create 3rd replicate of data matrix with compounds and controls
replicate3 = matrix(abs(rnorm(nr*nc)), nr, nc)

# combine all replicate to create "after" data set
replicates_after = list(replicate1, replicate2, replicate3)
names(replicates_after) = c("Replicate1", "Replicate2", "Replicate3")

# extract plate 3, replicate 2
extractplate(replicates_before, replicates_after, plate=3, replicate=2)
}

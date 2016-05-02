\name{plotcont}
\alias{plotcont}

\title{
 title
}
\description{
 description
}
\usage{
plotcont(dat, score="score", main, xaxis.marks=seq(0,5,0.025))
}

\arguments{
  \item{dat}{
  Data frame as an output from \eqn{normplate()}.
}
  \item{score}{Specifies the columns containing the data.
}
  \item{main}{
  Main title for the plot.
}
  \item{xaxis.marks}{
  Specifies x-axis mark ticks.
}
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

# extract plate 1, triplicate 1
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

# combine 3 triplicates
res1 = rbind(res11, res12, res13)
# reformat result
res1 = formatRESULT(res1, triplicate="Triplicate", score="score", t="Time")

layout(matrix(c(1,2,3), 3, 1, byrow = TRUE))

# plot density of all positive controls
plotcont(subset(res1, welltype=="Control P"), main="Density of Positive Controls",
 xaxis.marks=seq(-1,5,0.025))

# plot density of all negative controls
plotcont(subset(res1, welltype=="Control N"), main="Density of Negative Controls",
 xaxis.marks=seq(-1,5,0.025))

# plot density of controls with low, medium and high concentrations
plotcont(subset(res1, welltype=="Control low" | welltype=="Control med" | 
welltype=="Control high"), main="Density of Controls with Low, 
Medium and High Concentrations", xaxis.marks=seq(-1,5,0.025))
}

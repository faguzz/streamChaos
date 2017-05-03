# streamChaos - Analysis of Nonlinear Data Streams - R package

A framework for analyzing nonlinear data streams (and nonlinear time series).

Based on the package:

* [stream](http://github.com/mhahsler/stream).


## Installation


## Usage

Load the package and create a data stream based on a transient Logistic Map:

![Transient Logistic Map](http://latex.codecogs.com/gif.latex?x_t%20%3D%20r_t%20%5Ccdot%20x_%7Bt-1%7D%20%281%20-%20x_%7Bt-1%7D%29), in which (http://latex.codecogs.com/gif.latex?r_t%20%5Cin%20%5B2.0%2C%204.0%5D)$.


```R
options(warn=-1)

library("streamChaos")

dsd <- NLDSD_TransientLogisticMap(N=12000)

```

Process such stream using the Permutation Entropy algorithm with a sliding window of 800 observations, sliding 10 observations by iteration.

```R
pe <- DSCDD_PermutationEntropy(m=5, d=1, window.length=800)

ret <- processStream(dsd, pe, window.step=10)
```

Plot the results.

```R
options(repr.plot.width=7, repr.plot.height=5)

plot(ret$ds[,1], pch='.')
par(new=T, xaxt='n', yaxt='n')
ts.plot(ret$measures, col=2, xlab='', ylab='')
```

## References

Algorithms:
* [Permutation Entropy]()
* [Recurrence Quantification Analysis]()
* [Multidimensional Fourier Transform]()
* [Permutation-Invariant]()

Data streams:
* [Transient Logistic Map]()
* [Transient Lorenz Attractor]()

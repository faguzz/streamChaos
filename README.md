# streamChaos - Analysis of Nonlinear Data Streams - R package

A framework for analyzing nonlinear data streams (and nonlinear time series).

Based on the package:

* [stream](http://github.com/mhahsler/stream).


## Installation


## Usage

Load the package and create a data stream based on a transient Logistic Map.

```R
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
plot(ret$ds[,1], pch='.')
par(new=T, xaxt='n', yaxt='n')
ts.plot(ret$measures, col=2, xlab='', ylab='')
```

## References

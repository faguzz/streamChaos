# streamChaos - Analysis of Nonlinear Data Streams - R package

A framework for analyzing nonlinear data streams (and nonlinear time series).

Based on the package:

* [stream](http://github.com/mhahsler/stream).


## Installation


## Usage

Load the package and create a data stream based on a transient Logistic Map.

```R
library("streamChaos")

dsd <- NLDSD_TransientLogisticMap(N=10000)

```

Process such stream using the Permutation Entropy algorithm.

```R
pe <- DSCDD_PermutationEntropy()

ret <- processStream(dsd, pe)
```

Plot the results.

```R
plot(ret$ds[,1], pch='.')
par(new=T)
ts.plot(ret$measures, col=2)
```

## References

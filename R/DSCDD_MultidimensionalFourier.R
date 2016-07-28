#######################################################################
# stream-chaos - R library for managing chaotic data streams
# Copyright (C) 2015 Fausto G. da Costa
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

# based on:
#   Proposal of a new stability concept to detect changes in unsupervised
#   data streams. Rosane M.M. Vallim and Rodrigo F. de Mello. ESWA Vol 41
#   p.7350-7360 - Published 15 November 2014

### constructor
DSCDD_MultidimensionalFourier <- function(m=2, d=1, nbins=7, window.length=800) {

	description <- "Multidimensional Fourier"

	multidimensionalFourier <- multidimensionalFourier$new(m=m, d=d, nbins=nbins, window.length=window.length)

	l <- list(description = description, RObj = multidimensionalFourier)

	class(l) <- c("DSCDD_MultidimensionalFourier", "DSCDD_R", "DSCDD")
	l
}

# initializer
multidimensionalFourier <- setRefClass("multidimensionalFourier", 
	fields = list(
		m = "numeric",
		d = "numeric",
		nbins = "numeric",
		window.length = "numeric",
		window.data   = "ANY",
		coordinates   = "ANY",
		modelA = "ANY",
		modelB = "ANY",
		verbose = "logical"
	), 

	methods = list(
		initialize = function(
			m=NULL,
			d=NULL,
			nbins=NULL,
			window.length=NULL
		) {
			m <<- m
			d <<- d
			nbins <<- nbins
			window.length <<- window.length

			resetMDF()

			verbose <<- F

			.self
		},

		process = function(newdata, ...) {
			# BUG
			newdata <- newdata[[1]]

			window.data <<- update(window.data, newdata) 

			if (window.data$count == window.data$max.length) {
				grid <- multidimensionalQuantize(window.data$embedded.dat)
				coefs <- multidimensionalFourierTransform(grid)

				if (is.null(modelA)) {
					modelA <<- as.complex(coefs)
				} else {
					modelB <<- modelA
					modelA <<- as.complex(coefs)
				}
			}
		},

		getMeasure = function(...) {
			if (window.data$count != window.data$max.length) {
				if (verbose) {
					print("Sliding window not filled")
				}
				return(NA)
			}

			if (is.null(modelA) || is.null(modelB)) {
				if (verbose) {
					print("Need two fill windows for processing")
				}
				return(NA)
			}

			magA <- NULL
			magB <- NULL
			for (i in 1:length(modelB)) {
				magA <- cbind(magA, sqrt(Re(modelA[[i]])^2 + Im(modelA[[i]])^2))
				magB <- cbind(magB, sqrt(Re(modelB[[i]])^2 + Im(modelB[[i]])^2))
			}

			eigA <- svd(magA)$d
			eigB <- svd(magB)$d
			diff <- 0
			value <- 0
			prob <- c()
			for (ev in 1:length(eigA)) {
				prob <- c(prob, abs(eigA[ev] - eigB[ev]) /
								max(c(eigA[ev], eigB[ev])))
			}

			ret <- -sum(prob[prob>0] * log2(prob[prob>0]))

			return(ret)
		},

		multidimensionalQuantize = function(data) {
			discretized.data <- data

			for (i in 1:ncol(data)) {
				discretized.data[,i] <- findInterval(data[,i],
										seq(from=min(data[,i], na.rm=T), to=max(data[,i], na.rm=T),
											by=(max(data[,i], na.rm=T)-min(data[,i], na.rm=T))/(nbins-i)))
			}

			grid <- list()

			for (i in 1:nrow(discretized.data)) {
				key <- strcat(as.character(discretized.data[i,]), collapse="#")

				if (is.null(grid[[key]])) {
					grid[[key]] <- 1
				} else {
					grid[[key]] <- grid[[key]] + 1
				}
			}

			grid
		},

		assimilateToGrid = function(grid, key) {
			key = strcat(as.character(key), collapse="#")
			if (is.null(grid[[key]])) return (0)
			return (grid[[key]])
		},

		multidimensionalFourierTransform = function(grid) {
			aux.grid <- grid

			# i = [1, m]
			for (i in 1:ncol(coordinates)) {
				# dim = [1, nbins]
				for (dim in min(coordinates[,i]):max(coordinates[,i], na.rm=T)) {

					indexes <- which(coordinates[,i]==dim)
					series <- c()

					for (index in indexes) {
						key <- strcat(as.character(coordinates[index,]), collapse='#')
						qtt <- assimilateToGrid(aux.grid, key)
						series <- c(series, qtt)
					}

					ret <- fft(series)

					#ret has all Fourier's coefficients
					for (j in 1:length(indexes)) {
						key <- strcat(as.character(coordinates[indexes[j],]), collapse='#')
						if (is.null(aux.grid[[key]]))
							aux.grid[[key]] <- ret[j]
						else
							aux.grid[[key]] <- aux.grid[[key]] + ret[j]
					}
				}
			}

			return(aux.grid)
		},

		printParameters = function() {
			cat("m=", m, ", d=", d, ", nbins=", nbins, "\n", sep='')
		},

		resetMDF = function() {
			window.data <<- SlidingWindow(window.length=window.length,
										embedding.dimension=m, delay.dimension=d)

			coordinates <<- expand.grid(replicate(m, 1:nbins, simplify=F))

			modelA <<- NULL
			modelB <<- NULL
		}
	)
)


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
#   Detecting dynamical changes in time series using the permutation entropy
#   Yinhe Cao, Wen-wen Tung, J. B. Gao, V. A. Protopopescu, and L. M. Hively
#   Phys. Rev. E 70, 046217 - Published 27 October 2004

### constructor
DSCDD_PermutationEntropy <- function(m=5, d=1, window.length=800) {

	permutationEntropy <- permutationEntropy$new(m=m, d=d, window.length=window.length)

	description <- "Permutation Entropy"

	l <- list(description = description, RObj = permutationEntropy)

	class(l) <- c("DSCDD_PermutationEntropy", "DSCDD_R", "DSCDD")
	l
}

# initializer
permutationEntropy <- setRefClass("permutationEntropy", 
	fields = list(
		m = "numeric",
		d = "numeric",
		window.length     = "numeric",
		window.data       = "ANY",
		window.states     = "ANY",
		states.hash.table = "ANY",
		verbose = "logical"
	), 

	methods = list(
		initialize = function(
			m=NULL,
			d=NULL,
			window.length=NULL
		) {
			m <<- m
			d <<- d
			window.length <<- window.length

			reset()

			verbose <<- F

			.self
		},

		process = function(newdata, ...) {
			# BUG
			newdata <- newdata[[1]]

			window.data <<- update(window.data, newdata)

			if (!is.null(window.data$embedded.data)) {
				new.states <- processWindowStates(tail(window.data$embedded.data, n=length(newdata)))
				window.states <<- update(window.states, new.states)
			}
		},

		getMeasure = function(...) {
			if (window.states$count != window.states$max.length) {
				if (verbose) {
					print("Sliding window not filled")
				}
				return(NA)
			}

			return(calculateWindowEntropy(window.states$data))
		},

		mapState = function(state) {
			# sort dimensions of a state row and discretize it by the hash.table
			sorted.index <- sort(state, index.return=T)$ix
			
			states.hash.table[[toString(sorted.index)]]
		},

		processWindowStates = function(window.embedded) {
			w <- c()
			for (i in 1:nrow(window.embedded)) {
				w <- c(w, mapState(window.embedded[i,]))
			}

			w
		},

		shannonEntropy = function(p) {
			if (min(p) < 0 || sum(p) <= 0) {
				return(NA)
			}

			p.norm <- p[p>0]/sum(p)
			-sum(log2(p.norm) * p.norm)
		},

		normalizedShannonEntropy = function(p) {
			shannonEntropy(p) / log2(factorial(m))
		},

		calculateWindowEntropy = function(window.states) {
			window.states.probabilities <- table(window.states) / length(window.states)

			entropy <- normalizedShannonEntropy(window.states.probabilities)

			entropy
		},

		printParameters = function() {
			cat("m=", m, ", d=", d, "\n", sep='')
		},

		reset = function() {
			window.data <<- SlidingWindow(window.length=window.length,
										embedding.dimension=m, delay.dimension=d)
			window.states <<- SlidingWindow(window.length=window.length,
											compute.embedded.data=F)

			# generate all m! permutations and create a hash table like:
			CTRL <- how(within=Within("free"), complete=T, observed=T)
			permutations <- allPerms(m, control=CTRL)

			states.hash.table <<- list()
			for(i in 1:nrow(permutations)) {
				states.hash.table[[toString(permutations[i,])]] <<- i
			}
		}
	)
)


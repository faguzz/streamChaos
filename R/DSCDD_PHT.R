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

DSCDD_PHT <- function(lambda, delta) {

	pht <- pht$new(lambda=lambda, delta=delta)

	description <- "Page-Hinkley Test"

	l <- list(description = description, RObj = pht)

	class(l) <- c("DSCDD_PHT", "DSCDD_R", "DSCDD")
	l
}

# initializer
pht <- setRefClass("pht", 
	fields = list(
		lambda = "numeric",
		delta  = "numeric",
		n      = "numeric",
		m      = "numeric",
		mi     = "numeric",
		Mi     = "numeric",
		PHi    = "numeric",
		md     = "numeric",
		Md     = "numeric",
		PHd    = "numeric",
		verbose = "logical"
	), 

	methods = list(
		initialize = function(
			lambda=NULL,
			delta=NULL
		) {
			lambda <<- lambda
			delta <<- delta

			reset()

			verbose <<- F

			.self
		},

		process = function(newdata, ...) {
			# BUG
			newdata <- newdata[[1]]

			for (x in newdata) {
				# online update mean and variance
				n <<- n + 1
				daux <- x - m
				m <<- m + daux/n

				# PHT
				# increasing
				mi  <<- mi + (x - m - delta)
				Mi  <<- min(Mi, mi)
				PHi <<- mi - Mi

				# decreasing
				md  <<- md - (x - m + delta)
				Md  <<- min(Md, md)
				PHd <<- md - Md
			}
		},

		getMeasure = function(...) {
			if (PHi > lambda || PHd > lambda) {
				reset()
				return(1)
			} else {
				return(0)
			}
		},

		printParameters = function() {
			cat("lambda=", lambda, ", delta=", delta, "\n", sep='')
		},

		reset = function() {
			n <<- 0
			m <<- 0

			mi  <<- 0
			Mi  <<- Inf
			PHi <<- 0
			md  <<- 0
			Md  <<- Inf
			PHd <<- 0
		}
	)
)


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
#   The CUSUM algorithm: a small review
#   Granjon, Pierre
#   2012

DSCDD_CUSUM <- function(h, delta) {

	cusum <- cusum$new(h=h, delta=delta)

	description <- "Cumulative Sum"

	l <- list(description = description, RObj = cusum)

	class(l) <- c("DSCDD_CUSUM", "DSCDD_R", "DSCDD")
	l
}

# initializer
cusum <- setRefClass("cusum", 
	fields = list(
		h     = "numeric",
		delta = "numeric",
		n     = "numeric",
		m     = "numeric",
		variance = "numeric",
		M2    = "numeric",
		Si    = "numeric",
		Gi    = "numeric",
		Sd    = "numeric",
		Gd    = "numeric",
		verbose = "logical"
	), 

	methods = list(
		initialize = function(
			h=NULL,
			delta=NULL
		) {
			h <<- h
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
				M2 <<- M2 + daux*(x - m)

				variance <<- 0

				if (n < 2) {
					variance <<- NaN
				} else {
					variance <<- M2 / (n - 1)
				}

				# CUSUM
				if (!is.nan(variance)) {
					# increasing
					si <- (abs(delta)/variance) * (x - m - abs(delta)/2)
					Si <<- Si + si
					Gi <<- max(Gi + si, 0)

					# decreasing
					sd <- -(abs(delta)/variance) * (x - m + abs(delta)/2)
					Sd <<- Sd + sd
					Gd <<- max(Gd + sd, 0)
				}
			}
		},

		getMeasure = function(...) {
			if (Gi > h || Gd > h) {
				reset()
				return(1)
			} else {
				return(0)
			}
		},

		printParameters = function() {
			cat("h=", h, ", delta=", delta, "\n", sep='')
		},

		reset = function() {
			n  <<- 0
			m  <<- 0
			M2 <<- 0

			Si <<- 0
			Gi <<- 0
			Sd <<- 0
			Gd <<- 0
		}
	)
)


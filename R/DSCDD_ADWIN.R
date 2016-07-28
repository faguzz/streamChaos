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
#   Learning from Time-Changing Data with Adaptive Windowing
#   Bifet, Albert and Gavalda, Ricard
#   SIAM 7, 2007

DSCDD_ADWIN <- function(delta=0.01, window.length=400) {

	adwin <- adwin$new(delta=delta, window.length=window.length)

	description <- "Adaptive Windowing"

	l <- list(description = description, RObj = adwin)

	class(l) <- c("DSCDD_ADWIN", "DSCDD_R", "DSCDD")
	l
}

# initializer
adwin <- setRefClass("adwin", 
	fields = list(
		delta = "numeric",
		window.length     = "numeric",
		window.data       = "ANY",
		drift = "logical",
		verbose = "logical"
	), 

	methods = list(
		initialize = function(
			delta=NULL,
			window.length=NULL
		) {
			delta <<- delta
			window.length <<- window.length

			window.data <<- SlidingWindow(window.length=window.length,
											compute.embedded.data=F)

			reset()

			verbose <<- F

			.self
		},

		process = function(newdata, ...) {
			# BUG
			newdata <- newdata[[1]]

			window.data <<- update(window.data, newdata)

			v <- var(window.data$data)

			for (t in seq(1, window.data$count, by=1)) {
				W1 <- window.data$data[1:t]
				W2 <- window.data$data[t:window.data$count]

				n1 <- t
				n2 <- window.data$count - t + 1

				m1 <- mean(W1)
				m2 <- mean(W2)

				m <- 1/((1/n1) + (1/n2))
				d <- delta/window.data$count

				ecut <- sqrt((2/m) * v * log(2/d)) + (2/(3 * m)) * log(2/d)

				if (abs(m1 - m2) >= ecut) {
					drift <<- T

					# drop W1 from the sliding window
					window.data$data <<- window.data$data[t:window.data$count]

					break
				}
			}
		},

		getMeasure = function(...) {
			if (drift) {
				driftaux <- 1
			} else {
				driftaux <- 0
			}
			reset()
			return(driftaux)
		},

		printParameters = function() {
			cat("delta=", delta, "\n", sep='')
		},

		reset = function() {
			drift <<- F
		}
	)
)


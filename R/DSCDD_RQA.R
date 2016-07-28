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
#   L.L. Trulla, A. Giuliant, J.P. Zbilut, and C.L. Webber
#   Phys. Lett. A 223, 255 (1996)

### constructor
DSCDD_RQA <- function(m=2, d=1, window.length=800, radius,
	measure.type=c("RR", "DET", "LMAX")) {

	rqa <- rqa$new(m=m, d=d, radius=radius, window.length=window.length, measure.type=measure.type)

	description <- "Recurrence Quantification Analysis"

	l <- list(description = description, RObj = rqa)

	class(l) <- c("DSCDD_RQA", "DSCDD_R", "DSCDD")
	l
}

# initializer
rqa <- setRefClass("rqa", 
	fields = list(
		m = "numeric",
		d = "numeric",
		radius = "numeric",
		measure.type      = "character",
		window.length     = "numeric",
		window.data       = "ANY",
		results.file	  = "character"
	), 

	methods = list(
		initialize = function(
			m=NULL,
			d=NULL,
			radius=NULL,
			measure.type=NULL,
			window.length=NULL
		) {
			m <<- m
			d <<- d
			radius <<- radius
			measure.type <<- measure.type
			window.length <<- window.length

			reset()

			.self
		},

		process = function(newdata, ...) {
			# BUG
			newdata <- newdata[[1]]

			window.data <<- update(window.data, newdata)
		},

		getMeasure = function(...) {
            if (window.data$count != window.data$max.length) {
                return(NA)
            }

			return(computeRQA())
		},

		computeRQA = function(...) {
			rqa.measures <- crqa(window.data$data, window.data$data,
								 delay=m, embed=d, radius=radius,
								 normalize=0, rescale=0,
								 mindiagline=2, minvertline=2)[1:9]

			# LMAX = 1/maxL
			rqa.measures$LMAX <- 1/rqa.measures$maxL

			return(rqa.measures[[measure.type]])
		},

		computeR = function() {
			r <- matrix(0, nrow=nrow(window.data$distances), ncol=ncol(window.data$distances))
			r[which(window.data$distances < radius, arr.ind=T)] <- 1

			r
		},

		printParameters = function() {
			cat("m=", m, ", d=", d, ", measure.type=", measure.type, "\n", sep='')
		},

		reset = function() {
			results.file <<- tempfile()

			window.data <<- SlidingWindow(window.length=window.length,
										compute.embedded.data=F)
		}

	)
)


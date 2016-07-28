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

### constructor
DSCDD_GromovHausdorff <- function(m=2, d=1, window.length=800,
								  landmarking=c("none", "random", "kmeans"),
								  landmarks.num=window.length/10) {

	gromovHausdorff <- gromovHausdorff$new(m=m, d=d,
										   window.length=window.length,
										   landmarking=landmarking,
										   landmarks.num=landmarks.num)

	description <- "Gromov-Hausdorff distance"

	l <- list(description = description, RObj = gromovHausdorff)

	class(l) <- c("DSCDD_GromovHausdorff", "DSCDD_R", "DSCDD")
	l
}

# initializer
gromovHausdorff <- setRefClass("gromovHausdorff", 
	fields = list(
		m = "numeric",
		d = "numeric",
		window.length   = "numeric",
		landmarking     = "character",
		landmarks.num   = "numeric",
		window.data     = "ANY",
		old.window.data = "ANY",
		verbose = "logical"
	), 

	methods = list(
		initialize = function(
			m=NULL,
			d=NULL,
			window.length=NULL,
			landmarking=NULL,
			landmarks.num=NULL
		) {
			m <<- m
			d <<- d
			window.length <<- window.length
			landmarking <<- landmarking
			landmarks.num <<- landmarks.num

			reset()

			verbose <<- F

			.self
		},

		process = function(newdata, ...) {
			# BUG
			newdata <- newdata[[1]]

			old.window.data <<- window.data

			window.data <<- update(window.data, newdata) 
		},

		getMeasure = function(...) {
			if (window.data$count == window.data$max.length &
				old.window.data$count == window.data$max.length) {

				# no landmarking
				if (landmarking == "none") {
					a <- window.data$embedded.data
					b <- old.window.data$embedded.data
				}

				# random landmarking
				if (landmarking == "random") {
					a.sample <- sample(1:nrow(window.data$embedded.data), landmarks.num)
					b.sample <- sample(1:nrow(old.window.data$embedded.data), landmarks.num)
					a <- window.data$embedded.data[a.sample,]
					b <- old.window.data$embedded.data[b.sample,]
				}

				# kmeans landmarking
				if (landmarking == "kmeans") {
					centers.a <- kmeans(window.data$embedded.data, landmarks.num)
					centers.b <- kmeans(old.window.data$embedded.data, landmarks.num)
					a <- centers.a$centers
					b <- centers.b$centers
				}

				ret <- gromovdist(dist(a),
								  dist(b))
				return(ret)
			}

			print("Sliding window not filled")
			return(NA)
		},

		printParameters = function() {
			cat("m=", m, ", d=", d, "landmarking=", landmarking,
				"landmarks.num=", landmarks.num, "\n", sep='')
		},

		reset = function() {
			window.data <<- SlidingWindow(window.length=window.length,
										  embedding.dimension=m,
										  delay.dimension=d)

			old.window.data <<- NULL
		}
	)
)


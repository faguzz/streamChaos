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

SlidingWindow <- function(window.length=500, lambda=1,
    compute.embedded.data=T, embedding.dimension=2, delay.dimension=1,
	compute.distances=F) {

	# error handling
	if (window.length < 1) {
		stop("Window too small")
	}

	if (lambda > 1 || lambda < 0) {
		stop("Lambda not acceptable")
	}

	if (compute.embedded.data == T && embedding.dimension < 2) {
		stop("Cannot embedd data with this embedding dimension")
	}

	if (compute.embedded.data == T && delay.dimension < 1) {
		stop("Cannot embedd data with this time delay")
	}

	if (compute.distances == T && compute.embedded.data == F) {
		stop("Cannot compute distances without computing embedded data")
	}

	# parameter setting
	max.length <- window.length
	lambda <- lambda
	m <- embedding.dimension
	d <- delay.dimension
	compute.embedded.data <- compute.embedded.data
	compute.distances <- compute.distances
	embedded.data <- NULL
	count <- 0
	data <- c()

	# storing data
	l <- list(description = "Sliding Window",
			max.length=max.length,
			lambda=lambda,
            m=m,
            d=d,
			compute.embedded.data=compute.embedded.data,
			compute.distances=compute.distances,
			count=count,
			data=data,
			embedded.data=embedded.data)

	class(l) <- c("SlidingWindow")

	l
}

update.SlidingWindow <- function(window, new.data) {
	num.itens <- length(new.data)

	if (num.itens > window$max.length) stop("Updated more points than the size of the window")

	# shift data
	excess.data <- window$count + num.itens - window$max.length
	if (excess.data > 0) {
		window$data <- window$data[-1:-excess.data]
	}
	window$data <- c(window$data, new.data)

	window$count <- length(window$data)

	if (window$compute.embedded.data) {
		# first window for being embedded
		if (is.null(window$embedded.data)) {
			# only if have enough points
			if (window$count >= (window$m - 1) * window$d + 1) {
				window$embedded.data <- embedd(window$data, window$m, window$d)

				if (window$compute.distances) {
					window$distances <- as.matrix(dist(window$embedded.data))
				}
			}
		} else {
			# shift embedded data
			if (excess.data > 0) {
				window$embedded.data <- window$embedded.data[-1:-excess.data,]
			}

			range.embbed.data <- c((window$count - num.itens - ((window$m - 1) * window$d) + 1), window$count)

			if (range.embbed.data[1] < 1) {
				range.embbed.data[1] <- 1
			}

			# new embedded points
			data.to.embedd <- window$data[range.embbed.data[1]:range.embbed.data[2]]
			new.embedded.data <- embedd(data.to.embedd, window$m, window$d)

			# distance between new and old embedded points
			if (window$compute.distances) {
				distance.between.points <- proxy::dist(window$embedded.dat, new.embedded.data)
			}

			# concatenate
			window$embedded.data <- rbind(window$embedded.data, new.embedded.data)

			# shift distance matrix
			if (window$compute.distances) {
				if (excess.data > 0) {
					old.distances <- window$distances[-1:-excess.data, -1:-excess.data]
				} else {
					old.distances <- window$distances
				}
				new.distances <- as.matrix(dist(new.embedded.data))
				window$distances <- rbind(cbind(old.distances, distance.between.points),  # top part
										cbind(t(distance.between.points), new.distances))  # bottom part
			}
		}
	}

	window
}


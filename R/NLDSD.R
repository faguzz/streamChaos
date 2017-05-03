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

NLDSD <- function(...) stop("NLDSD is an abstract class and cannot be instantiated!")

produce_points <- function(dsd, n=1) {
	UseMethod("produce_points")
}


get_points.NLDSD <- function(dsd, n=1, outofpoints=c("stop", "warn", "ignore"),
							 assignment=F, ...) {
    n <- as.integer(n)
    outofpoints <- match.arg(outofpoints)

    # If have not warmed up
    if (!dsd$state$warmed.up) {
        warm_up(dsd)
        dsd$state$warmed.up <- T
    }

    # If have produced enough points
    if (dsd$state$counter >= (dsd$N * dsd$dt)) {
        if(outofpoints == "stop") stop("The stream is at its end!")
        if(outofpoints == "warn") warning("The stream is at its end! No more points available!")
    }

    # If asked more points than its limit
	# XXX when n = 1 and end of the stream...
    if ((dsd$state$counter + n * dsd$dt) > (dsd$N * dsd$dt)) {
        n <- ((dsd$N + 1) * dsd$dt - dsd$state$counter) / dsd$dt
    }

    # Produce points
    data <- produce_points(dsd, n)

    # Add noise
    data <- add_noise(dsd, data)

    # Assignment of changes
    if (assignment) {
		time <- as.numeric(rownames(data))
        assigments <- rep(0, n)
		if (!is.null(dsd$time.changes)) {
			change.times <- intersect(time, as.integer(dsd$time.changes))
			assigments[change.times - head(time, n=1)] <- 1
		}
        data <- cbind(data, assigments)
    }

    data
}

produce_points.default <- function(dsd, n=1) {
	n <- as.integer(n)

	# obtain and normalize data
	if (!dsd$is.transient) {
		obs <- MapIterator(dsd$syst, 0, n*dsd$dt, dsd$dt,
						   dsd$state$start.x, dsd$state$parms,
						   runge.kutta=dsd$runge.kutta)
	} else {
		obs <- TransientMapIterator(dsd$syst, 0, n*dsd$dt, dsd$dt,
									dsd$state$start.x, dsd$state$parms,
									dsd$parms.mod, runge.kutta=dsd$runge.kutta)
	}

	# XXX here is the error!
	last_obs <- tail(obs, n=1)
	current_obs <- head(obs, n=n)
	data <- data.frame(X1=current_obs[,1])
	data <- NormalizeData(data, dsd$range)

	# row and col names
	time <- seq(dsd$state$counter, dsd$state$counter + (n - 1) * dsd$dt, by=dsd$dt)
	rownames(data) <- time

	# update state
	dsd$state$counter <- dsd$state$counter + n * dsd$dt
	dsd$state$start.x <- last_obs
	if (dsd$is.transient) {
		dsd$state$parms <- dsd$state$parms + dsd$parms.mod * n
	}

	data
}

add_noise <- function(dsd, data) {
	UseMethod("add_noise")
}

add_noise.default <- function(dsd, data) {
	n <- nrow(data)

	# adding noise
	noise <- NoiseGenerator(dsd$noise.type, dsd$noise.parms, n)
	data <- data + noise

	data
}

warm_up <- function(dsd, data) {
	UseMethod("warm_up")
}

warm_up.default <- function(dsd) {
	if (dsd$warming.up.num > 0) {
		parms.backup <- dsd$state$parms
		produce_points(dsd, dsd$warming.up.num)
		dsd$state$counter <- 0
		dsd$state$parms <- parms.backup
	}
}

plot.NLDSD <- function(dsd, n=500, assignment=T) {
    points <- get_points(dsd, n, assignment=assignment)

    plot(rownames(points), points[,1], ylim=c(0,1), type='p', pch='.')

    if (assignment) {
        abline(v=(which(points[,2] == 1) + as.numeric(head(rownames(points), n=1))))
    }
}

reset_stream.NLDSD <- function(dsd) {
    dsd$state$counter <- 0
	dsd$state$warm_up <- F
#    dsd$state$start.x <-
#    dsd$state$parms <-

    dsd
}


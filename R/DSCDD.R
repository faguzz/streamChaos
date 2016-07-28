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

### DSCDD - Data Stream Concept Drift Detector interface

DSCDD <- function(...) stop("DSCDD is an abstract class and cannot be instantiated!")

reset <- function(x, ...) { x$RObj$reset() }

getMeasure <- function(x, ...) { x$RObj$getMeasure() }

print.DSCDD <- function(x, ...) {
	cat(.line_break(paste(x$description)))
	cat(.line_break(paste(x$RObj$printParameters())))
	cat("Class:", paste(class(x), collapse=", "), "\n")
}

processStream <- function(dsd, alg, N, window.step=10, plot=F) {
	ds <- NULL
	measures <- c()

	begin.time <- Sys.time()

	while (T) {
		ret <- tryCatch(
			p <- get_points(dsd, n=window.step,
							outofpoints="warn", assignment=T),
			warning = function(w) { w })

		if ("warning" %in% class(ret)) {
			break
		}

		ds <- rbind(ds, p)

		update(alg, p)

		val <- getMeasure(alg)
		measures <- c(measures, val)
	}

	if (plot) {
		plot(ds[,1], pch='.')
		par(new=T)
		times <- seq(window.step, nrow(ds), by=window.step)
		plot(times, measures, type='l', lwd=2, col=2, axes=F)
		abline(v=which(ds[,2] == 1))
		axis(4, at=pretty(measures))
	}

	ret = list()
	ret$ds <- ds
	ret$measures <- measures
	ret$total.time <- as.numeric(Sys.time() - begin.time, units="secs")

	ret
}


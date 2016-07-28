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

NLDSD_LogisticMap <- function(N, start.x=c(0.6), parms=c(3.8),
							dt=1, warming.up.num=1000, range=NULL,
							noise.type="None", noise.parms=list()) {
	if (is.null(range)) {
		range <- matrix(c(0, 1), ncol=2, byrow=T)
	}

	# error checking
	if (N < 1)
		stop("invalid number of observations")

	counter <- 0
	warmed.up <- F

	# stateful
	state <- new.env()
	assign("counter", counter, envir=state)
	assign("start.x", start.x, envir=state)
	assign("parms", parms, envir=state)
	assign("warmed.up", warmed.up, envir=state)

	l <- list(description = "Logistic Map Stream",
			N = N,
			range = range,
			dt = dt,
			syst = LogisticMap,
			is.transient = F,
			runge.kutta = F,
			warming.up.num = warming.up.num,
			noise.type = noise.type,
			noise.parms = noise.parms,
			state = state)
	class(l) <- c("NLDSD_LogisticMap", "NLDSD", "DSD_R", "DSD_data.frame", "DSD")
	l
}

# Logistic Map
LogisticMap <- function(t, input, parms) {
	x <- input[1]
	r <- parms[1]
	dx <- r * x * (1 - x)
	list(c(dx))
}


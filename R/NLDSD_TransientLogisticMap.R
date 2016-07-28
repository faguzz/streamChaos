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

NLDSD_TransientLogisticMap <- function(N, start.x=c(0.6),
									 parm.range=list(t0=c(2.8), tn=c(4.0)),
									 dt=1, warming.up.num=0, range=NULL,
									 noise.type="None", noise.parms=list()) {
	if (is.null(range)) {
		range <- matrix(c(0, 1), ncol=2, byrow=T)
	}

	# error checking
	if (N < 1)
		stop("invalid number of observations")

	counter <- 0
	warmed.up <- F
	parms <- parm.range$t0
	parms.mod <- (parm.range$tn - parm.range$t0)/N

	# stateful
	state <- new.env()
	assign("counter", counter, envir=state)
	assign("start.x", start.x, envir=state)
	assign("parms", parms, envir=state)
	assign("warmed.up", warmed.up, envir=state)

	# changes on dynamics
	change.times <- c(3.022, 3.464, 3.554, 3.571, 3.628, 3.635, 3.739, 3.744, 3.829, 3.850)
	time.changes <- c((change.times - parms)/parms.mod)

	l <- list(description = "Transient Logistic Map Stream",
			N = N,
			range = range,
			dt = dt,
			syst = LogisticMap,
			is.transient = T,
			runge.kutta = F,
			warming.up.num = warming.up.num,
			noise.type = noise.type,
			noise.parms = noise.parms,
			parms.mod = parms.mod,
			time.changes = time.changes,
			state = state)
	class(l) <- c("NLDSD_TransientLogisticMap", "NLDSD", "DSD_R", "DSD_data.frame", "DSD")
	l
}

# Logistic Map
LogisticMap <- function(t, input, parms) {
	x <- input[1]
	r <- parms[1]
	dx <- r * x * (1 - x)
	list(c(dx))
}


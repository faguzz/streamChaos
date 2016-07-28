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
#   J.S. Iwanski and E. Bradley, Chaos 8, 861 (1998)

NLDSD_TransientLorenzAttractor <- function(N, start.x=c(5, 5, 5),
										 parm.range=list(t0=c(10, 28, 8/3), tn=c(10, 268, 8/3)),
										 dt=0.1, warming.up.num=0, range=NULL,
										 noise.type="None", noise.parms=list()) {
	if (is.null(range)) {
		range <- matrix(c(-54, 56), ncol=2, byrow=T)
	}

	# error checking
	if (N < 1)
		stop("invalid number of observations")

	counter <- 0
	warmed.up <- F
	parms <- parm.range$t0
	parms.mod <- (parm.range$tn - parm.range$t0) / N

	# stateful
	state <- new.env()
	assign("counter", counter, envir=state)
	assign("start.x", start.x, envir=state)
	assign("parms", parms, envir=state)
	assign("warmed.up", warmed.up, envir=state)

	# changes on dynamics
	change.times <- c(100, 145, 166, 214.4)
	time.changes <- c((change.times - parms[2])/parms.mod[2]) * dt

	l <- list(description = "Transient Lorenz Attractor Stream",
			N = N,
			range = range,
			dt = dt,
			syst = LorenzAttractor,
			is.transient = T,
			runge.kutta = T,
			warming.up.num = warming.up.num,
			noise.type = noise.type,
			noise.parms = noise.parms,
			parms.mod = parms.mod,
			time.changes = time.changes,
			state = state)
	class(l) <- c("NLDSD_TransientLorenzAttractor", "NLDSD", "DSD_R", "DSD_data.frame", "DSD")
	l
}

# Lorenz Attractor
LorenzAttractor <- function (t, input, parms) {
    # Lorenz Attractor ODE
    #   *copy from tseriesChaos:lorenz.syst
    #
    # Args:
    #   t: actual time
    #   input: actual values
    #   parms: parameters of ODE system
    #
    # Returns:
    #   An iteration of the ODE system
    x <- input[1]
    y <- input[2]
    z <- input[3]
    a <- parms[1]
    b <- parms[2]
    c <- parms[3]
    dx <- a * (y - x)
    dy <- - x * z + b * x - y
    dz <- x * y - c * z
    list(c(dx, dy, dz)) 
}


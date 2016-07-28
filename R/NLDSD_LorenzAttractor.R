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
#   Deterministic Nonperiodic Flow. E.N. Lorenz. J. Atmos. Sci. 20 1963 130.

NLDSD_LorenzAttractor <- function(N, start.x=c(5, 5, 5), parms=c(10, 28, 8/3),
								dt=0.1, warming.up.num=1000, range=NULL,
								noise.type="None", noise.parms=list()) {
	if (is.null(range)) {
		range <- matrix(c(-19, 19), ncol=2, nrow=1, byrow=T)
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

	l <- list(description = "Lorenz Attractor Stream",
			  N = N,
			  range = range,
			  dt = dt,
			  syst = LorenzAttractor,
			  is.transient = F,
			  runge.kutta = T,
			  warming.up.num = warming.up.num,
			  noise.type = noise.type,
			  noise.parms = noise.parms,
			  state = state)
	class(l) <- c("NLDSD_LorenzAttractor", "NLDSD", "DSD_R", "DSD_data.frame", "DSD")
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


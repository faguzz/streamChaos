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
#    XXX

NLDSD_RosslerAttractor <- function(N, start.x=c(0, 0, 0), parms=c(0.15, 0.2, 10.0),
								 dt=0.3, warming.up.num=1000, range=matrix(c(-15, 18), ncol=2, nrow=1, byrow=T),
								 noise.type="None", noise.parms=list()) {
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

	l <- list(description = "Rossler Attractor Stream",
			  N = N,
			  range = range,
			  dt = dt,
			  syst = RosslerAttractor,
			  is.transient = F,
			  runge.kutta = T,
			  warming.up.num = warming.up.num,
			  noise.type = noise.type,
			  noise.parms = noise.parms,
			  state = state)
	class(l) <- c("NLDSD_RosslerAttractor", "NLDSD", "DSD_R", "DSD_data.frame", "DSD")
	l
}

# Rossler Attractor
RosslerAttractor <- function (t, input, parms) {
    # Rossler Attractor ODE
    #   *copy from tseriesChaos:rossler.syst
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
    dx <- -(y + z)
    dy <- x + a * y
    dz <- b + z * (x - c)
    list(c(dx, dy, dz))
}


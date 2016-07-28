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

NoiseGenerator <- function(noise.type, noise.parms, N) {
	if (!is.null(noise.parms$seed)) {
		set.seed(noise.parms$seed)
	}

    noise <- switch(noise.type,
                None    = rep(0, N),
                Normal  = rnorm(N, noise.parms$mean, noise.parms$sd),
                Uniform = runif(N, min=noise.parms$min, max=noise.parms$max),
                ARIMA   = arima.sim(list(order=noise.parms$order, ar=noise.parms$ar), sd=noise.parms$sd, n=N))

	noise
}


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

RK4 <- function(t, syst, y, dt, parms) {
    # Runge-Kutta fourth-order method
    #
    # Args:
    #   t: actual time
    #   syst: ODE system
    #   y: actual values
    #   dt: time step
    #   parms: parameters of ODE system
    #
    # Returns:
    #   Temporal discretizaion for the approximation of ODE
	k1 <- syst(t, y, parms)[[1]] * dt
	k2 <- syst(t + dt/2, y + dt/2 * k1, parms)[[1]] * dt
	k3 <- syst(t + dt/2, y + dt/2 * k2, parms)[[1]] * dt
	k4 <- syst(t + dt, y + dt * k3, parms)[[1]] * dt

	dy <- y + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)

	list(c(dy))
}


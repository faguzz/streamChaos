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

MapIterator <- function(syst, start.time, end.time, dt, start.x, parms=NULL, runge.kutta=F) {
    times <- seq(start.time, end.time, by=dt)
	y <- start.x

	ret <- NULL
	for (t in times) {
		ret <- rbind(ret, y)
		if (runge.kutta) {
			y <- RK4(t, syst, y, dt, parms)[[1]]
		} else {
			y <- syst(t, y, parms)[[1]] * dt
		}
	}

	ret
}


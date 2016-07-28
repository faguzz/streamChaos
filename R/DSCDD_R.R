#######################################################################
# 
# 
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

### Implement a new clusterer
### Create an S3 class with elements description and RObj
### RObj needs to be a reference class with methods
###  * cluster(newdata, ...)

DSCDD_R <- function(...) stop("DSCDD_R is an abstract class and cannot be instantiated!")

### helper for doing things in blocks
## TODO remove it from here!
.make_block <- function(n, block) {
	if(n<block) return(n)

	b <- rep(block, times=as.integer(n/block))
	if(n%%block) b<- c(b, n%%block)
	b
}

update.DSCDD_R <- function(object, dsd, n=1, verbose=FALSE,
	block=10000L, ...) {
	### object contains an RObj which is a reference object with a cluster method

	### for data frame/matrix we do it all at once
	if(is.data.frame(dsd) || is.matrix(dsd)) {
		if(verbose) cat("Clustering all data at once for matrix/data.frame.")
		object$RObj$process(dsd, ...)
		return(invisible(object))
	}

	n <- as.integer(n)
	block <- as.integer(block)
	if(n>0) {
		if(!is(dsd, "DSD_data.frame"))
			stop("Cannot cluster stream (need a DSD_data.frame.)")

		### TODO: Check data
		if(verbose) total <- 0L
		for(bl in .make_block(n, block)) {
			object$RObj$process(get_points(dsd, bl), ...)
			if(verbose) {
				total <- total + bl
				cat("Processed", total, "points\n")
			}
		}
	}

	invisible(object)
}


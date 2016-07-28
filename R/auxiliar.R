
### helper for doing things in blocks
.make_block <- function(n, block) {
    if(n<block) return(n)
    
    b <- rep(block, times=as.integer(n/block))
    if(n%%block) b<- c(b, n%%block)
    b
}

### line break helper
.line_break <- function(x, width=options("width")) {
  form <- paste('(.{1,', width,'})(\\s|$)', sep='')
  gsub(form, '\\1\n', x)
}

NormalizeData <- function(x, range=NULL) {
    if (is.null(range)) {
        min.x <- min(x, na.rm=T)
        max.x <- max(x, na.rm=T)
    } else {
        min.x <- range[1]
        max.x <- range[2]
    }
    return ((x - min.x)/(max.x - min.x))
}



# ===================================================================================================================
# Helper function
# ===================================================================================================================

mid2 <- function(x, na.rm=T) {
    quantile(x, .5, na.rm=na.rm, type = 3);
}

tail2 <- function(x, n) {
    x <- x[!is.na(x)];
    if ( length(x)==0 ) {
        return( NA );
    }
    return( tail(x, n) );
}









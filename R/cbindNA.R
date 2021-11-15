#' @title Combine R Objects with different length by Rows or Columns
#'
#' @description Take a sequence of vector, matrix or data-frame arguments and combine by
#' columns or rows, respectively. If the length of the R objects differ
#' they are filled with NAs.
#'
#' @usage naRbind(..., deparse.level = 1)
#' naCbind(..., deparse.level = 1)
#'
#' @param ... A sequence of vector, matrix or data-frame to bind
#' @param deparse.level Integer controlling the construction of labels in the case of
#' non-matrix-like arguments (for the default method):
#' deparse.level = 0 constructs no labels; the default, deparse.level = 1 or 2 constructs
#' labels from the argument names, see the 'Value' section below.
#'
#' @details The code is based on the  \code{\link{cbind}()} function.
#'
#' @return For the default method, a matrix combining the ... arguments column-wise or
#'  row-wise. (Exception: if there are no inputs or all the inputs are NULL, the value
#'  is NULL.)
#'
#'  The type of a matrix result determined from the highest type of any of the inputs
#'  in the hierarchy raw < logical < integer < double < complex < character < list .
#'
#' @seealso \code{\link[base]{cbind}} or \code{\link[base]{rbind}} documentation.
#'
#' @examples
#' # Vector
#' naCbind(1:10, 1:20)
#'
#' # Matrix
#' naCbind(matrix(1:10, ncol = 2), matrix(1:20, ncol = 2))
#'
#' # Data.frame
#' naCbind(data.frame(x=1:10, y=1:10), data.frame(c=1:20, d=1:20))
#'
#' @export
naCbind = function (..., deparse.level = 1) {
  has.dl <- !missing(deparse.level)
  deparse.level <- as.integer(deparse.level)
  if(identical(deparse.level, -1L)) deparse.level <- 0L # our hack
  stopifnot(0 <= deparse.level, deparse.level <= 2)

  argl <- list(...)
  ## remove trailing 'NULL's:
  na <- nargs() - has.dl
  while(na > 0L && is.null(argl[[na]])) { argl <- argl[-na]; na <- na - 1L }
  if (na == 0) return(NULL)
  symarg <- as.list(substitute(list(...)))[-1L] # symbolic argument (names)
  nmsym <- names(symarg)
  ## Give *names* depending on deparse.level {for non-matrix}:
  nm <- c( ## 0:
    function(i) NULL,
    ## 1:
    function(i) if(is.symbol(s <- symarg[[i]])) deparse(s) else NULL,
    ## 2:
    function(i) deparse(symarg[[i]])[[1L]])[[ 1L + deparse.level ]]
  Nms <- function(i) { if(!is.null(s <- nmsym[i]) && nzchar(s)) s else nm(i) }
  if(na == 1) {
    if(isS4(..1)) {
      r <- cbind2(..1)
      if(length(dim(..1)) < 2L && length(dim(r)) == 2L)
        colnames(r) <- Nms(1)
      return(r)
    }
    else return(base::cbind(..., deparse.level = deparse.level))
  }

  ## else :  na >= 2

  if (na == 2) {
    fix.na <- FALSE
  }
  else { ## na >= 3 arguments
    ## determine nrow(<result>)  for e.g.,	cbind(diag(2), 1, 2)
    ## only when the last two argument have *no* dim attribute:
    nrs <- unname(lapply(argl, nrow)) # of length na
    iV <- sapply(nrs, is.null) # is 'vector'
    fix.na <- identical(nrs[(na - 1):na], list(NULL, NULL))
    if (fix.na) {
      ## "fix" last argument, using 1-column `matrix' of proper nrow():
       nr <- max(if (all(iV)) sapply(argl, length) else unlist(nrs[!iV]))
       argl[[na]] <- cbind(rep(argl[[na]], length.out = nr),
           deparse.level = 0)
       ## and since it's a 'matrix' now, cbind() below may not name it
    }
    ## if(deparse.level) {
      if(fix.na)
        fix.na <- !is.null(Nna <- Nms(na))
    ## }
  }

  Ncol <- function(x) {
    d <- dim(x); if(length(d) == 2L) d[2L] else as.integer(length(x) > 0L) }
  setN <- function(i, nams)
    colnames(r)[i] <<- if(is.null(nams)) "" else nams

  # Fill with NA's
  mrow = max(sapply(argl, function(x) NROW(x)))
  argl <- lapply(argl, function(x) {
    if (is.matrix(x) | is.data.frame(x))
      rbind(as.matrix(x), matrix(rep(NA, (mrow - nrow(x)) * ncol(x)), ncol = ncol(x)))
    else
      c(x, rep(NA, mrow - length(x)))
  })

  r <- argl[[na]]
  for(i in (na-1L):1L) {
    d2 <- dim(r)
    r <- cbind2(argl[[i]], r)
    ## if(deparse.level == 0)
    ##     if(i == 1L) return(r) else next
    ism1 <- !is.null(d1 <- dim(argl[[i]])) && length(d1) == 2L
    ism2 <- !is.null(d2)                   && length(d2) == 2L
    if(ism1 && ism2) ## two matrices
      next

    ## else -- Setting colnames correctly
    ##	       when one was not a matrix [needs some diligence!]
    nn1 <- !is.null(N1 <- if(       (l1 <- Ncol(argl[[i]])) && !ism1) Nms(i)) # else NULL
    nn2 <- !is.null(N2 <- if(i == na-1L && Ncol(argl[[na]]) && !ism2) Nms(na))
    if(nn1 || nn2) {
      if(is.null(colnames(r)))
        colnames(r) <- rep.int("", ncol(r))
      if(nn1) setN(1,	 N1)
      if(nn2) setN(1+l1, N2)
    }
  }

  if(fix.na) {
    if(is.null(colnames(r)))
      colnames(r) <- rep.int("", ncol(r))
    setN(ncol(r), Nna)
  }
  r
}

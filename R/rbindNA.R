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
#' @details The code is based on the  \code{\link{rbind}()} function.
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
#' naRbind(1:10, 1:20)
#'
#' # Matrix
#' naRbind(matrix(1:10, ncol = 2), matrix(1:20, ncol = 2), matrix(1:30, ncol = 3))
#'
#' # Data.frame
#' naRbind(data.frame(x=1:10, y=1:10, c=1:10), data.frame(c=1:20, d=1:20))
#'
#' @export
naRbind = function(..., deparse.level = 1) {
  has.dl <- !missing(deparse.level)
  deparse.level <- as.integer(deparse.level)
  if(identical(deparse.level, -1L)) deparse.level <- 0L # our hack
  stopifnot(0 <= deparse.level, deparse.level <= 2)

  argl <- list(...)
  ## remove trailing 'NULL's:
  na <- nargs() - has.dl
  while(na > 0L && is.null(argl[[na]])) { argl <- argl[-na]; na <- na - 1L }
  if(na == 0) return(NULL)
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
      r <- rbind2(..1)
      if(length(dim(..1)) < 2L && length(dim(r)) == 2L)
        rownames(r) <- Nms(1)
      return(r)
    }
    else return(base::rbind(..., deparse.level = deparse.level))
  }

  ## else :  na >= 2

  if(na == 2) {
    fix.na <- FALSE
  }
  else { ## na >= 3 arguments
    ## determine ncol(<result>)  for e.g.,	rbind(diag(2), 1, 2)
    ## only when the last two argument have *no* dim attribute:
    nrs <- unname(lapply(argl, ncol)) # of length na
    iV <- vapply(nrs, is.null, NA)# is 'vector'
    fix.na <- identical(nrs[(na-1L):na], list(NULL,NULL))
    if(fix.na) {
      ## "fix" last argument, using 1-row `matrix' of proper ncol():
      nr <- max(if(all(iV)) lengths(argl) else unlist(nrs[!iV]))
      argl[[na]] <- rbind(rep(argl[[na]], length.out = nr),
                          deparse.level = 0)
      ## and since it's a 'matrix' now, rbind() below may not name it
    }
    ## if(deparse.level) {
    if(fix.na)
      fix.na <- !is.null(Nna <- Nms(na))
    ## }
  }

  Nrow <- function(x) {
    d <- dim(x); if(length(d) == 2L) d[1L] else as.integer(length(x) > 0L) }
  setN <- function(i, nams)
    rownames(r)[i] <<- if(is.null(nams)) "" else nams

  # Fill with NA's
  mcol = max(sapply(argl, function(x) {
    if (is.matrix(x) | is.data.frame(x)) NCOL(x)
    else length(x)
  }))
  argl <- lapply(argl, function(x) {
    if (is.matrix(x) | is.data.frame(x))
      cbind(as.matrix(x), matrix(NA, nrow(x), ncol = mcol - ncol(x)))
    else
      c(x, rep(NA, mcol - length(x)))
  })

  r <- argl[[na]]
  for(i in (na-1L):1L) {
    d2 <- dim(r)
    r <- rbind2(argl[[i]], r)
    ## if(deparse.level == 0)
    ##     if(i == 1L) return(r) else next
    ism1 <- !is.null(d1 <- dim(argl[[i]])) && length(d1) == 2L
    ism2 <- !is.null(d2)                   && length(d2) == 2L
    if(ism1 && ism2) ## two matrices
      next

    ## else -- Setting rownames correctly
    ##	       when one was not a matrix [needs some diligence!]
    nn1 <- !is.null(N1 <- if(       (l1 <- Nrow(argl[[i]])) && !ism1) Nms(i)) # else NULL
    nn2 <- !is.null(N2 <- if(i == na-1L && Nrow(argl[[na]]) && !ism2) Nms(na))
    if(nn1 || nn2) {
      if(is.null(rownames(r)))
        rownames(r) <- rep.int("", nrow(r))
      if(nn1) setN(1,	 N1)
      if(nn2) setN(1+l1, N2)
    }
  }

  if(fix.na) {
    if(is.null(rownames(r)))
      rownames(r) <- rep.int("", nrow(r))
    setN(nrow(r), Nna)
  }
  r
}

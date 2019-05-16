# Generic form
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)

  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")

  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }

  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)

  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin

  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}

# Grouping the left hand side
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}

# Then to execute:
# Group the left hand side using the new function g() The right hand side should be a vector or a list Use the newly-created binary operator %=%

# # Example Call;  Note the use of g()  AND  `%=%`
# #     Right-hand side can be a list or vector
# g(a, b, c)  %=%  list("hello", 123, list("apples, oranges"))

# g(d, e, f) %=%  101:103

# # Results: 
# > a
# [1] "hello"
# > b
# [1] 123
# > c
# [[1]]
# [1] "apples, oranges"

# > d
# [1] 101
# > e
# [1] 102
# > f
# [1] 103

# git checkout --track origin/daves_branch
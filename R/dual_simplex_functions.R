### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' construct_dual_feasible_tableau
#'
#' @param A \code{matrix}, coefficient matrix of the LP
#' @param b \code{vector}, RHS
#' @param c \code{vector}, coefficients of the objective function
#' @param sense \code{scalar}, max = 1, min = -1 (default = 1)
#'
#' @return \code{matrix}, initial (dual feasible) simplex tableau
#' @export
#'
construct_dual_feasible_tableau <- function(A, b, c, sense) {
  # to do # construct a dual feasible tableau
  return(tableau)
}

### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' is_dual_optimal
#'
#' @param t \code{matrix}, a simplex tableau
#'
#' @return \code{logical}, TRUE = tableau is (dual) optimal, FALSE = tableau is not (dual) optimal
#' @export
#'
is_dual_optimal <- function(t) {
  if (min(t[1:(nrow(t) - 1), ncol(t)]) < 0) {
    return(FALSE) # not (dual) optimal, not (primal) feasible
  }
  else {
    return(TRUE) # (dual) optimal, (primal) feasible
  }
}

### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' get_dual_pivot_row
#'
#' @param t \code{matrix}, a simplex tableau
#'
#' @return \code{scalar}, index of the pivot row
#' @export
#'
get_dual_pivot_row <- function(t) {
  return(which.min(t[1:(nrow(t) - 1), ncol(t)]))
}

### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' get_dual_pivot_colum
#'
#' @param t \code{matrix}, a simplex tableau
#' @param dual_pivot_row \code{scalar}, index of the pivot row
#'
#' @return \code{scalar}, index of the pivot column
#' @export
#'
get_dual_pivot_column <- function(t, dual_pivot_row) {
  a_row <- t[dual_pivot_row, 1:(ncol(t) - 1)]
  z_row <- t[nrow(t), 1:(ncol(t) - 1)]
  if (min(a_row) > 0) {
    print("No (primal) feasible solution exists -> stop execution")
    return(-1)
  }
  else {
    delta <- vector(mode = "double", length = (ncol(t) - 1))
    for (j in 1:(ncol(t) - 1)) {
      if (a_row[j] < 0) {
        delta[j] <- z_row[j] / a_row[j]
      } else {
        delta[j] <- -Inf
      }
    }
    return(which.max(delta))
  }
}

### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' dual_simplex
#'
#' @param tableau \code{matrix}, a simplex tableau
#' @param max_iter \code{scalar}, maximum number of iterations
#'
#' @return (primal and dual) optimal simplex tableau or stops execution in case of an unbounded problem
#' @export
#'
dual_simplex <- function(tableau, max_iter = 100) {
  print("Initial Tableau (Tableau 0)")
  print(tableau)
  iter <- 1
  while (!is_dual_optimal(tableau) & iter < max_iter) {
    print("--------------------------------------------------------------------")
    print(paste("Iteration", iter))
    print("--------------------------------------------------------------------")
    dual_pivot_row <- get_dual_pivot_row(tableau)
    if (dual_pivot_row == -1) {
      break()
    }
    print(paste("Dual pivot row:", dual_pivot_row))
    get_dual_pivot_column <- get_dual_pivot_column(tableau, dual_pivot_row)
    print(paste("Dual pivot column:", get_dual_pivot_column))
    tableau <- pivot(tableau, dual_pivot_row, get_dual_pivot_column)
    print(paste("New tableau at the end of (dual simplex) iteration", iter))
    print(tableau)
    iter <- iter + 1
  }
  print("--------------------------------------------------------------------")
  print("Status: End")
  print("--------------------------------------------------------------------")
}

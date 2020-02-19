#' get_arg_min_conditional
#'
#' @param lhs \code{vector}, LHS
#' @param rhs \code{vector}, RHS
#'
#' @return \code{saclar}, index of rhs[i] / lhs[i] for lhs[i] > 0
#' @export
#'
get_arg_min_conditional <- function(lhs, rhs) {
  theta <- vector(mode = "double", length = length(lhs))
  for (i in 1:length(lhs)) {
    if (lhs[i] > 0) {
      theta[i] <- rhs[i] / lhs[i]
    } else {
      theta[i] <- Inf
    }
  }
  return(which.min(theta))
}



#' reviced_primal_simplex
#'
#' Reviced primal simplex alorithm.
#'
#' @param A \code{matrix}, coefficient matrix of the LP
#' @param b \code{vector}, RHS
#' @param obj \code{vector}, coefficients of the objective function
#' @param basic \code{vector}, indices of a (primal feasible) basis
#'
#' @return optimal basic feasible solution (BFS)
#' @export
#'
#' @examples
reviced_primal_simplex <- function(A, b, obj, basic = c((ncol(A) - nrow(A)):ncol(A))) {
  print("Initialize")

  idx <- c(1:ncol(A))
  BI <- solve(A[, basic])
  b_ <- BI %*% b
  non_basic <- idx[-basic]

  print("Initial Basis B")
  print(basic)
  print("Initial non-Basis N")
  print(non_basic)

  i <- 1L
  while (i < 5 & TRUE) {
    print("------------------------------------------------------------------------------------")
    print(paste0("Iteration ", i))

    # 1) BTRAN (backward transformation)
    pi <- obj[basic] %*% BI

    # 2) price
    c_ <- -obj[non_basic] + obj[basic] %*% BI %*% A[, non_basic]
    print("delta c = ")
    print(c_)
    t_ <- which.min(c_) # need to get the value in basic
    print(paste0("position of min in non-Basis = ", t_))
    pivot_column <- non_basic[t_]
    print(paste0("Pivot column = ", pivot_column))

    if (all(c_ >= 0)) {
      print("Optimal basic feasible solution found!")
      break()
    }

    # 3) FTRAN (forward transformation)
    lhs <- BI %*% A[, pivot_column]

    if (all(lhs <= 0)) {
      print("Problem is unbounded -> stop execution")
      break()
    }

    # 4) CHUZR (Pivotzeilenauswahl)
    pivot_row <- get_arg_min_conditional(lhs, b_)

    # 5) WRETA
    # an der Stelle an der in der Nicht-Basis der Pivotzeilenindex stand wird der Inder der in der Basis an der Stelle Pivotzeile steht geschrieben
    non_basic[non_basic == pivot_column] <- basic[pivot_row]
    basic[pivot_row] <- pivot_column

    print("New basis B")
    print(basic)
    print("New non-basis after iteration N")
    print(non_basic)

    # the following step need to be improved by using the product form of the basis inverse
    BI <- solve(A[, basic])
    b_ <- BI %*% b

    print("basic feasible solution (BFS)")
    print(b_)

    print(paste0("End of iteration ", i))

    i <- i + 1
  }

  z <- obj[basic] %*% b_

  print(paste0("Optimal solution value z = ", z))

  print("End")
}

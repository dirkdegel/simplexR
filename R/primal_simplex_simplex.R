### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' construct_tableau
#'
#' Construct an initial simplex tableau of the linear program for given A, b, c, sense (max = 1, min = -1), relation ({"<=", "=", "=>"}).
#'
#' @param A \code{matrix}, coefficient matrix of the LP
#' @param b \code{vector}, RHS
#' @param c \code{vector}, coefficients of the objective function
#' @param sense \code{scalar}, max = 1, min = -1 (default = 1)
#' @param relation \code{vector}, {"<=", "=", "=>"} (default = "<=")
#' @param bigM \code{scalar}, M used in bigM-method (default = 1000)
#'
#' @return \code{matrix}, initial (primal feasible) simplex tableau
#' @export
#'
construct_tableau <- function(A, b, c, sense = 1, relation = rep("<=", length(b)), bigM = 1000) {
  #
  print("Start constructing Simplex Tableau")
  #
  n <- ncol(A)
  m <- nrow(A)
  #
  # negative RHS
  #
  for (i in 1:m) {
    if (b[i] < 0) {
      A[i, ] <- -1 * A[i, ]
      b[i] <- -1 * b[i]
      if (relation[i] == "<=") {
        relation[i] <- ">="
      }
      else if (relation[i] == ">=") {
        relation[i] <- "<="
      }
    }
  }
  #
  # transform all constraints in [=]-form with a slack or artificial variable
  # "<=" -> add slack variable
  # "="  -> add artificial variable
  # ">=" -> add -1 * slack and artificial variable
  #
  artificial_variable_index_set <- c()
  artificial_variable_row_index_set <- c()
  for (i in 1:m) {
    if (relation[i] == "<=") {
      # add slack variable s >= 0
      c <- c(c, 0)
      A <- cbind(A, s = diag(m)[, i])
    }
    else if (relation[i] == ">=") {
      # add -slack variable s >= 0
      c <- c(c, 0)
      A <- cbind(A, s = -1 * diag(m)[, i])
      # add artificial variable v >= 0
      if (sense == 1){
        c <- c(c, -bigM)
      } else {
        c <- c(c, bigM)
      }
      A <- cbind(A, v = diag(m)[, i])
      artificial_variable_index_set <- c(artificial_variable_index_set, ncol(A))
      artificial_variable_row_index_set <- c(artificial_variable_row_index_set, i)
    }
    else if (relation[i] == "=") {
      # add artificial variable v >= 0
      if (sense == 1){
        c <- c(c, -bigM)
      } else {
        c <- c(c, bigM)
      }
      A <- cbind(A, v = diag(m)[, i])
      artificial_variable_index_set <- c(artificial_variable_index_set, ncol(A))
      artificial_variable_row_index_set <- c(artificial_variable_row_index_set, i)
    }
    else {
      print("Error")
    }
  }
  # sense = 1 ~ max, sense = -1 ~ min
  #
  if (sense == 1) { # max # check if min and big-M case is captured!!!
    c <- -1 * c
  }
  else {
    print("Minimization problem: remember to multiply the optimal objective value by (-1)!")
  }
  # combine c,A,b
  tableau <- rbind(cbind(A, b), z = c(c, 0))
  #
  print("Tableau constructed")
  #
  if (length(artificial_variable_index_set) > 0) {
    print(
      paste("Artificial variable 'v' added at column ", artificial_variable_index_set, "and row ", artificial_variable_row_index_set, " -> phase I algorithm required")
    )
    #
    print(
      paste0("BigM-method is used, with bigM = ", bigM)
    )
    #
    # transform the objective function (depending on the method) but only bigM-method considered atm
    for (k in artificial_variable_row_index_set) {
      tableau[m + 1, ] <- tableau[m + 1, ] - bigM * tableau[k, ]
    }
  }
  #
  return(tableau)
}

### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' optimality_check
#'
#' Checks if the current tableau is (primal) optimality.
#'
#' @param t \code{matrix}, a simplex tableau
#'
#' @return \code{logical}, TRUE = tableau is optimal, FALSE = tableau is not optimal
#' @export
#'
optimality_check <- function(t) {
  if (min(t[nrow(t), 1:(ncol(t) - 1)]) < 0) {
    return(FALSE) # not optimal
  }
  else {
    return(TRUE) # optimal
  }
}

### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' get_pivot_column
#'
#' Choose the first nonbasic column with the lowest negative (reduced) cost.
#'
#' @param t \code{matrix}, a simplex tableau
#'
#' @return \code{scalar}, index of the pivot column
#' @export
#'
get_pivot_column <- function(t) { # pricing
  return(which.min(t[nrow(t), 1:(ncol(t) - 1)]))
}

### ------------------------------------------------------------------------------------------------

#' get_pivot_column_Bland
#'
#' Choose the lowest-numbered (i.e., leftmost) nonbasic column with a negative (reduced) cost.
#'
#' @param t \code{matrix}, a simplex tableau
#'
#' @return \code{scalar}, index of the pivot column
#' @export
#'
get_pivot_column_Bland <- function(t) { # pricing
  which(t[nrow(t), 1:(ncol(t) - 1)] <= 0)[1]
}

### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' get_pivot_row
#'
#' @param t \code{matrix}, a simplex tableau
#' @param pivot_column \code{scalar}, index of the pivot column
#'
#' @return \code{scalar}, index of the pivot row
#' @export
#'
get_pivot_row <- function(t, pivot_column) {
  lhs <- t[1:(nrow(t) - 1), pivot_column]
  rhs <- t[1:(nrow(t) - 1), ncol(t)]
  if (max(lhs) < 0) {
    print("Problem is unbounded -> stop execution")
    # break
    return(-1)
  }
  else {
    theta <- vector(mode = "double", length = (nrow(t) - 1))
    for (i in 1:(nrow(t) - 1)) {
      if (lhs[i] > 0) {
        theta[i] <- rhs[i] / lhs[i]
      } else {
        theta[i] <- Inf
      }
    }
    return(which.min(theta))
  }
}

### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' pivot
#'
#' Performs a pivot operation an a given simplex tableau
#'
#' @param t \code{matrix}, a simplex tableau
#' @param pivot_row \code{scalar}, index of the pivot column
#' @param pivot_column \code{scalar}, index of the pivot row
#'
#' @return \code{matrix}, new simplex tableau
#' @export
#'
pivot <- function(t, pivot_row, pivot_column) {
  pivot_element <- t[pivot_row, pivot_column]
  t[pivot_row, ] <- t[pivot_row, ] / pivot_element
  for (r in 1:nrow(t)) {
    if (r != pivot_row) {
      t[r, ] <- t[r, ] - t[r, pivot_column] * t[pivot_row, ]
    }
  }
  return(t)
}

### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' simplex
#'
#' @param tableau \code{matrix}, simplex tableau in canonical form
#' @param max_iter \code{scalar}, maximum number of iterations
#'
#' @return optimal simplex tableau or stops execution in case of an unbounded problem
#' @export
#'
simplex <- function(tableau, max_iter = 100) {
  print("Initial Tableau (Tableau 0)")
  print(tableau)
  iter <- 1
  while (!optimality_check(tableau) & iter < max_iter) {
    print("--------------------------------------------------------------------")
    print(paste("Iteration", iter))
    print("--------------------------------------------------------------------")
    pivot_column <- get_pivot_column(tableau)
    print(paste("Pivot column:", pivot_column))
    pivot_row <- get_pivot_row(tableau, pivot_column)
    if (pivot_row == -1) {
      break()
    }
    print(paste("Pivot row:", pivot_row))
    tableau <- pivot(tableau, pivot_row, pivot_column)
    print(paste("New tableau at the end of iteration", iter))
    print(tableau)
    iter <- iter + 1
  }
  print("--------------------------------------------------------------------")
  print("Status: End")
  print("--------------------------------------------------------------------")
}

### ------------------------------------------------------------------------------------------------
###
### ------------------------------------------------------------------------------------------------

#' simplexR
#'
#' Solves (find an optimal solution or reports that the problem is not solveable) a generic linear program. Calls \code{construct_tableau} and \code{simplex}.
#'
#' @param A \code{matrix}, coefficient matrix of the LP
#' @param b \code{vector}, RHS
#' @param c \code{vector}, coefficients of the objective function
#' @param sense \code{scalar}, max = 1, min = -1 (default = 1)
#' @param relation \code{vector}, {"<=", "=", "=>"} (default = "<=")
#' @param max_iter \code{scalar}, maximum number of iterations (default = 100)
#'
#' @return Solves (find an optimal solution or reports that the problem is not solveable) a generic linear program. Calls \code{construct_tableau} and \code{simplex}.
#' @export
#'
simplexR <- function(A, b, c, sense = 1, relation = rep("<=", length(b)), max_iter = 100) {
  tableau <- construct_tableau(A, b, c, sense, relation)
  simplex(tableau = tableau, max_iter = max_iter)
}

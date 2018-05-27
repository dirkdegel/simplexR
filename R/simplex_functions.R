###------------------------------------------------------------------------------------------------
###
###------------------------------------------------------------------------------------------------


#' construct_tableau
#'
#' Constrcut an initial Simplex tableau of the linear program in canonical form for given A, b, c, sense (max = 1)
#'
#' @param A a matrix, the coefficient matrix of the LP
#' @param b a vector, RHS
#' @param c a vector, objective function coefficients
#' @param sense max = 1
#'
#' @return a matrix that represents the initial Simplax tableau
#' @export
#'
construct_tableau <- function(A, b, c, sense){
  m <- nrow(A)
  #dimnames(A) <- list(1:m)
  if(sense == 1){ # max
    c <- -1 * c
  }
  t <- rbind(cbind(A, diag(m), b), c(c, rep(0, m + 1)))
  return(t)
}

###------------------------------------------------------------------------------------------------
###
###------------------------------------------------------------------------------------------------


#' optimality_check
#'
#' @param t a Simplex tableau
#'
#' @return logical (TRUE = optimal, FALSE not optimal)
#' @export
#'
optimality_check <- function(t){
  if(min(t[nrow(t), 1:ncol(t) - 1]) < 0){
    return(FALSE) # not optimal
  }
  else{
    return(TRUE) # optimal
  }
}

###------------------------------------------------------------------------------------------------
###
###------------------------------------------------------------------------------------------------


#' get_pivot_column
#'
#' @param t a Simplex tableau
#'
#' @return pivot column
#' @export
#'
get_pivot_column <- function(t){
  return(which.min(t[nrow(t), 1:ncol(t) - 1]))
}

###------------------------------------------------------------------------------------------------
###
###------------------------------------------------------------------------------------------------


#' get_pivot_row
#'
#' @param t a Simplex tableau
#' @param pivot_column the pivot column
#'
#' @return pivot row
#' @export
#'
get_pivot_row <- function(t, pivot_column){
  lhs <- t[1:nrow(t) - 1, pivot_column]
  rhs <- t[1:nrow(t) - 1, ncol(t)]
  if(max(lhs) < 0){
    print("Problem is unbounded -> stop execution")
    #break
    return(-1)
  }
  else{
    theta <- rhs / lhs
    temp <- length(theta)
    for(i in 1:temp){
      if(theta[i] <= 0) # < ???
        theta[i] <- Inf
    }
    return(which.min(theta))
  }
}

###------------------------------------------------------------------------------------------------
###
###------------------------------------------------------------------------------------------------


#' pivot
#'
#' @param t a Simplex tableau
#' @param pivot_row the pivot row
#' @param pivot_column the pivot column
#'
#' @return new Simplex tableau
#' @export
#'
pivot <- function(t, pivot_row, pivot_column){
  pivot_elemnt <- t[pivot_row, pivot_column]
  t[pivot_row, ] <- t[pivot_row, ] / pivot_elemnt
  nr <- nrow(t)
  for(r in 1:nr){
    if(r != pivot_row){
      t[r, ] <- t[r, ] - t[r, pivot_column] * t[pivot_row, ]
    }
  }
  return(t)
}

###------------------------------------------------------------------------------------------------
###
###------------------------------------------------------------------------------------------------


#' simplex
#'
#' @param A a matrix (coefficient matrix)
#' @param b a vector, RHS
#' @param c a vector, objective function coefficients
#' @param sense max = 1
#'
#' @return optimal Simplex tableau or stops execution in case of an unbounded problem
#' @export
#'
simplex <- function(A, b, c, sense){
  tableau <- construct_tableau(A, b, c, sense)
  print("Initial Tableau (Tableau 0)")
  print(tableau)
  iter <- 1
  while(!optimality_check(tableau) & iter < 100){
    print("----------------------------------------------")
    print(paste("Iteration", iter))
    print("----------------------------------------------")
    pivot_column <- get_pivot_column(tableau)
    print(paste("Pivot column:", pivot_column))
    pivot_row <- get_pivot_row(tableau, pivot_column)
    if(pivot_row == -1){break()}
    print(paste("Pivot row:", pivot_row))
    tableau <- pivot(tableau, pivot_row, pivot_column)
    print(paste("New tableau at the end of iteration", iter))
    print(tableau)
    iter <- iter + 1
  }
  print("----------------------------------------------")
  print("Status: ")
  print("----------------------------------------------")
}

# Linear Programming (HW4)


library(lpSolve)
library(linprog) #More detailed linear programming results


## Problem 1&2

# Define the coefficients of the objective function
f.obj <- c(15,9,8,8,11,26)

# Define the constraint matrix
f.con <- matrix(c(1,1,1,0,0,0,
                  0,0,0,1,1,1,
                  1,0,0,1,0,0,
                  0,1,0,0,1,0,
                  0,0,1,0,0,1
                  ), nrow = 5, byrow = TRUE)

# Define the constraint direction
f.dir <- c("<=", "<=", "=", "=", "=")

# Define the right-hand side values of the constraints
f.rhs <- c(45,25,30,10,30)

#Solve
prod.sol <- lp("min", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          FALSE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values








## Problem 3

# Define the coefficients of the objective function
f.obj <- c(8,6,3,8,9,3,44,34,34,32,57,35,28,24)

# Define the constraint matrix
f.con <- matrix(c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,1,1,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,1,1,0,0,0,0,0,0,0,0,
                  -1,0,-1,0,-1,0,1,1,1,1,0,0,0,0,
                  0,-1,0,-1,0,-1,0,0,0,0,1,1,1,1,
                  0,0,0,0,0,0,1,0,0,0,1,0,0,0,
                  0,0,0,0,0,0,0,1,0,0,0,1,0,0,
                  0,0,0,0,0,0,0,0,1,0,0,0,1,0,
                  0,0,0,0,0,0,0,0,0,1,0,0,0,1),
                nrow=9,
                byrow=TRUE)

# Define the constraint direction
f.dir <- c("<=", "<=", "<=", "=", "=", "=", "=", "=", "=")

# Define the right-hand side values of the constraints
f.rhs <- c(5,6,3,0,0,3,4,3,2)

#Solve
prod.sol <- lp("min", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          FALSE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values










## Problem 5

# Define the coefficients of the objective function
f.obj <- c(3.0,3.5,3.4,3.4,2.4,3.4,3.2,3.0,3.5,3.7,3.8,2.7,3.2,3.7,3.8)

# Define the constraint matrix
f.con <- matrix(c(1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,
                  0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,
                  0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,
                  0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,
                  1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,1,1,1
), nrow = 8, byrow = TRUE)

# Define the constraint direction
f.dir <- c("=", "=", "=", "=", "=", "=", "=", "=")

# Define the right-hand side values of the constraints
f.rhs <- c(1,1,1,1,1,1,1,1)

#Solve
prod.sol <- lp("max", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          TRUE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values












## Problem 6

# Define the coefficients of the objective function
f.obj <- c(44,39,29,17,21,17,18,19,29,18,24,21,19,14,29,24,29,14)

# Define the constraint matrix
f.con <- matrix(c(1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  -1,0,0,1,1,-1,0,0,0,0,0,-1,0,0,0,0,0,0,
                  0,-1,0,-1,0,1,1,1,1,-1,0,0,-1,0,0,0,0,0,
                  0,0,-1,0,0,0,-1,0,0,1,1,0,0,0,0,-1,0,0,
                  0,0,0,0,-1,0,0,-1,0,0,0,1,1,1,1,0,-1,0,
                  0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,
                  0,0,0,0,0,0,0,0,0,0,-1,0,0,0,-1,1,1,1), nrow = 7, byrow = TRUE)

# Define the constraint direction
f.dir <- c("=", "=", "=", "=", "=", "=", "=")

# Define the right-hand side values of the constraints
f.rhs <- c(1,0,0,0,0,1,0)

#Solve
prod.sol <- lp("min", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          FALSE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values



## Problem 6

# Define the coefficients of the objective function
f.obj <- c(0,2,5,3,3,0.25,0.25,0.25,0.25)

# Define the constraint matrix
f.con <- matrix(c(1,0,0,0,0,0,0,0,0,
                  0,1,0,0,0,0,0,0,0,
                  0,0,1,0,0,0,0,0,0,
                  0,0,0,1,0,0,0,0,0,
                  0,0,0,0,1,0,0,0,0,
                  1,1,0,0,0,-1,0,0,0,
                  0,0,1,0,0,1,-1,0,0,
                  0,0,0,1,0,0,1,-1,0,
                  0,0,0,0,1,0,0,1,-1,
                  0,0,0,0,0,0,0,0,1), nrow = 10, byrow = TRUE)

# Define the constraint direction
f.dir <- c("=", "<=", "<=", "<=", "<=", "=", "=", "=", "=", "=")

# Define the right-hand side values of the constraints
f.rhs <- c(50,600,300,500,400,400,500,400,400,100)

#Solve
prod.sol <- lp("min", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          FALSE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values


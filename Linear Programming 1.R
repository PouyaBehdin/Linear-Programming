# Linear Programming

library(lpSolve)
library(linprog) #More detailed linear programming results


# Problem 1

# Define the coefficients of the objective function
f.obj <- c(39,52,12.50,16,7.50,8.50)

# Define the constraint matrix
f.con <- matrix(c(3.5,0,1.3,0,0.8,0,
                  2.2,0,1.7,0,0,0,
                  3.1,0,2.6,0,1.7,0,
                  1,1,0,0,0,0,
                  0,0,1,1,0,0,
                  0,0,0,0,1,1), nrow = 6, byrow = TRUE)

# Define the constraint direction
f.dir <- c("<=", "<=", "<=", ">=", ">=", ">=")

# Define the right-hand side values of the constraints
f.rhs <- c(21000,25200,40800,4500,9000,4500)

#Solve
prod.sol <- lp("min", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          FALSE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values
prod.sol$duals #includes duals of constrants and reduced costs of variables (first duals then reduced costs)

options(scipen = 999)

# list outputs in column
matrix(prod.sol$solution)
matrix(prod.sol$duals)

# sensibility analysis results
prod.sol$duals.to #max range of feasibility
prod.sol$duals.from #min range of feasibility
prod.sol$sens.coef.to #max range of optimality
prod.sol$sens.coef.from #min range of optimality



# Convert Minutes To Hours
minutes_to_hours <- function(minutes) {
  hours <- minutes / 60
  rounded_hours <- round(hours, 2)
  return(rounded_hours)
}

# Convert the given minutes values to hours and round to two decimals
minutes1 <- 21000.0
minutes2 <- 16765.40
minutes3 <- 24450.0

hours1 <- minutes_to_hours(minutes1)
hours2 <- minutes_to_hours(minutes2)
hours3 <- minutes_to_hours(minutes3)

# Print the results
cat("21000.0 minutes = ", hours1, " hours\n")
cat("16765.40 minutes = ", hours2, " hours\n")
cat("24450.0 minutes = ", hours3, " hours\n")

##########################NEXT PROBLEM#############################

# Problem 7

# Define the coefficients of the objective function
f.obj <- c(0.07,0.09,0.10,0.11,0.08)

# Define the constraint matrix
f.con <- matrix(c(0,0,0,0,1,
                  -.10,-.10,-.10,.90,0,
                  -1,1,1,0,0,
                  0,0,1,1,-1,
                  1,1,1,1,1), nrow = 5, byrow = TRUE)

# Define the constraint direction
f.dir <- c("<=", "<=", "<=", "<=", "=")

# Define the right-hand side values of the constraints
f.rhs <- c(720000,0,0,0,2400000)

#Solve
prod.sol <- lp("max", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          TRUE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values
prod.sol$duals #includes duals of constrants and reduced costs of variables (first duals then reduced costs)

options(scipen = 999)

# list outputs in column
matrix(prod.sol$solution)
matrix(prod.sol$duals)

# sensibility analysis results
prod.sol$duals.to #max range of feasibility
prod.sol$duals.from #min range of feasibility
prod.sol$sens.coef.to #max range of optimality
prod.sol$sens.coef.from #min range of optimality




##########################NEXT PROBLEM#############################

# Problem 9

# Define the coefficients of the objective function
f.obj <- c(1,1,1,1,1,1,1)

# Define the constraint matrix
f.con <- matrix(c(1,0,0,1,1,1,1,
                  1,1,0,0,1,1,1,
                  1,1,1,0,0,1,1,
                  1,1,1,1,0,0,1,
                  1,1,1,1,1,0,0,
                  0,1,1,1,1,1,0,
                  0,0,1,1,1,1,1), nrow = 7, byrow = TRUE)

# Define the constraint direction
f.dir <- c(">=", ">=", ">=", ">=", ">=", ">=", ">=")

# Define the right-hand side values of the constraints
f.rhs <- c(75,45,40,70,95,90,60)

#Solve
prod.sol <- lp("min", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          FALSE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values
prod.sol$duals #includes duals of constrants and reduced costs of variables (first duals then reduced costs)

options(scipen = 999)

# list outputs in column
matrix(prod.sol$solution)
matrix(prod.sol$duals)

# sensibility analysis results
prod.sol$duals.to #max range of feasibility
prod.sol$duals.from #min range of feasibility
prod.sol$sens.coef.to #max range of optimality
prod.sol$sens.coef.from #min range of optimality



##########################NEXT PROBLEM#############################

# Problem 12

# Define the coefficients of the objective function
f.obj <- c(.002,.006,.007,.003,.007,.005,.005,.002,.003)

# Define the constraint matrix
f.con <- matrix(c(1,1,1,0,0,0,0,0,0,
                  0,0,0,1,1,1,0,0,0,
                  0,0,0,0,0,0,1,1,1,
                  1,0,0,1,0,0,1,0,0,
                  1,0,0,1,0,0,1,0,0,
                  0,1,0,0,1,0,0,1,0,
                  0,1,0,0,1,0,0,1,0,
                  0,0,1,0,0,1,0,0,1,
                  0,0,1,0,0,1,0,0,1), nrow = 9, byrow = TRUE)

# Define the constraint direction
f.dir <- c("<=", "<=", "<=", "<=", ">=", "<=", ">=", "<=", ">=")

# Define the right-hand side values of the constraints
f.rhs <- c(2600000,1000000,1900000,2000000,1000000,2000000,1000000,2000000,1000000)

#Solve
prod.sol <- lp("max", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          TRUE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values
prod.sol$duals #includes duals of constrants and reduced costs of variables (first duals then reduced costs)

options(scipen = 999)

# list outputs in column
matrix(prod.sol$solution)
matrix(prod.sol$duals)

# sensibility analysis results
prod.sol$duals.to #max range of feasibility
prod.sol$duals.from #min range of feasibility
prod.sol$sens.coef.to #max range of optimality
prod.sol$sens.coef.from #min range of optimality






##########################NEXT PROBLEM#############################

# Problem 14

# Define the coefficients of the objective function
f.obj <- c(1,1,1,1,1,1)

# Define the constraint matrix
f.con <- matrix(c(1,0,0,0,0,1,
                  1,1,0,0,0,0,
                  0,1,1,0,0,0,
                  0,0,1,1,0,0,
                  0,0,0,1,1,0,
                  0,0,0,0,1,1), nrow = 6, byrow = TRUE)

# Define the constraint direction
f.dir <- c(">=", ">=", ">=", ">=", ">=", ">=")

# Define the right-hand side values of the constraints
f.rhs <- c(6,5,9,6,5,5)

#Solve
prod.sol <- lp("min", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          FALSE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values
prod.sol$duals #includes duals of constrants and reduced costs of variables (first duals then reduced costs)

options(scipen = 999)

# list outputs in column
matrix(prod.sol$solution)
matrix(prod.sol$duals)

# sensibility analysis results
prod.sol$duals.to #max range of feasibility
prod.sol$duals.from #min range of feasibility
prod.sol$sens.coef.to #max range of optimality
prod.sol$sens.coef.from #min range of optimality





##########################NEXT PROBLEM#############################

# Problem 12

# Define the coefficients of the objective function
f.obj <- c(1,0,0,0,0,0,0,0,0,0)

# Define the constraint matrix
f.con <- matrix(c(1,-1.055,-1.0,-1,0,0,0,0,0,0,
                  0,.0675,.05125,1.04,-1,0,0,0,0,0,
                  0,.0675,.05125,0,1.04,-1,0,0,0,0,
                  0,1.0675,.05125,0,0,1.04,-1,0,0,0,
                  0,0,1.05125,0,0,0,1.04,-1,0,0,
                  0,0,0,0,0,0,0,1.04,-1,0,
                  0,0,0,0,0,0,0,0,1.04,-1), nrow = 7, byrow = TRUE)

# Define the constraint direction
f.dir <- c("=","=","=","=","=","=","=")

# Define the right-hand side values of the constraints
f.rhs <- c(0,160,185,210,255,285,430)

#Solve
prod.sol <- lp("min", f.obj, f.con, f.dir, f.rhs,
               compute.sens=TRUE)
solveLP(cvec = f.obj, bvec = f.rhs, Amat = f.con, maximum =
          FALSE, const.dir = f.dir, lpSolve=TRUE)

# Results
prod.sol$objval #objective function value
prod.sol$solution #decision variables values
prod.sol$duals #includes duals of constrants and reduced costs of variables (first duals then reduced costs)

options(scipen = 999)

# list outputs in column
matrix(prod.sol$solution)
matrix(prod.sol$duals)

# sensibility analysis results
prod.sol$duals.to #max range of feasibility
prod.sol$duals.from #min range of feasibility
prod.sol$sens.coef.to #max range of optimality
prod.sol$sens.coef.from #min range of optimality






















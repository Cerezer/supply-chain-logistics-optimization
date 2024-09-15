# Step 1: Install and load necessary libraries
# ---------------------------------------------
#install.packages("lpSolve")          # For solving linear programming problems
#install.packages("ompr")             # Mathematical optimization model package
#install.packages("ompr.roi")         # OMPR optimization interface
#install.packages("ROI.plugin.glpk")  # Solver plugin for GLPK (GNU Linear Programming Kit)
#install.packages("ggplot2")          # For data visualization
#install.packages("magrittr")         # For the pipe operator %>%

# Load libraries
library(lpSolve)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(ggplot2)
library(reshape2)   # To reshape data for visualization
library(magrittr)   # For the %>% pipe operator

# Step 2: Define the optimization problem
# --------------------------------------
costs <- matrix(c(2, 4, 5, 3,
                  3, 1, 6, 4,
                  4, 2, 7, 5), 
                nrow = 3, byrow = TRUE)  # Costs of transportation from warehouses to stores

supply <- c(20, 30, 25)                 # Supply available at each warehouse
demand <- c(10, 15, 25, 25)             # Demand required at each store

# Step 3: Solve the problem using lpSolve
# --------------------------------------
solution_lp <- lp.transport(costs, direction = "min", 
                            row.signs = rep("<=", 3), row.rhs = supply,
                            col.signs = rep(">=", 4), col.rhs = demand)

# Print the optimal solution using lpSolve
print("Optimal transportation plan using lpSolve:")
print(solution_lp$solution)

# Step 4: Solve the same problem using ompr for larger scalability
# ---------------------------------------------------------------
model <- MIPModel() %>%
  add_variable(x[i, j], i = 1:3, j = 1:4, type = "continuous", lb = 0) %>%
  set_objective(sum_expr(costs[i, j] * x[i, j], i = 1:3, j = 1:4), "min") %>%
  add_constraint(sum_expr(x[i, j], j = 1:4) <= supply[i], i = 1:3) %>%
  add_constraint(sum_expr(x[i, j], i = 1:3) >= demand[j], j = 1:4)

# Solve the model using GLPK solver
result_ompr <- solve_model(model, with_ROI(solver = "glpk"))

# Extract the solution values from the ompr result
solution_ompr <- get_solution(result_ompr, x[i, j])

# Print the optimal transportation plan from ompr
print("Optimal transportation plan using ompr:")
print(solution_ompr)

# Step 5: Visualize the results
# -----------------------------
# Create a heatmap-like visualization to show how much is transported from each warehouse to each store

# Convert the solution into a matrix format suitable for visualization
solution_matrix <- acast(solution_ompr, i ~ j, value.var = "value")

# Plot the optimized transportation plan
ggplot(melt(solution_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(x = "Stores", y = "Warehouses", title = "Optimized Transportation Plan") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal()


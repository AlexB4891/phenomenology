# The idea is generate the function for N number of firms and solve using cournot model.
# By now just obtaining de solution for an excercise:
# After solving the first order condition and the reaction functions found, it leads to the
# next equation system:

matrix <- rbind(c(2,1,1),
                c(1,2,1),
                c(1,1,2))

vector <- c(200,200,200)

solve(matrix, vector)

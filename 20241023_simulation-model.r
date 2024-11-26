
# N_t = N_t-1 + bN_t-1 - mN_t-1
# m = M + aa*N_a = 0.1 + 0.00025*N_a
# b = B - bb*N_s = 2 - 0.0005*N_s

## Ns <- Na - m*Na
## Na <- Ns + b*Ns

# N: number of individuals at start
# b: per capita birth rate
# m: mortality rate
# T: how many times the function should run

m <- function(N_a) {
    return(0.1 + 0.00025 * N_a)
}
b <- function(N_s) {
    return(2 - 0.0005 * N_s)
}
autmn <- function(N_s, b) {
    return(N_s + b * N_s)
}
spring <- function(N_a, m) {
    return(N_a - m * N_a)
}

pred <- function(N, T) {

    results <- data.frame(iteration=integer(), N_s=numeric(), N_a=numeric(), b=numeric(), m=numeric())

    m <- b <- N_a <- N_s <- numeric()

    # setting number of indivuals at first spring to be that of the start

    for (i in 1:T) {

        if(i == 1) {
            N_s <- N
            print(paste("Iteration:", i, "Spring population (N_s):", N_s))
        } else {
            N_s <- spring(N_a, m)
            print(paste("Iteration:", i, "Spring population (N_s):", N_s))
        }

        # calculate b for spring population
        b <- b(N_s)
        print(paste("Iteration:", i, "Spring birth rate (b):", b))

        #calculate autmn population
        N_a <- autmn(N_s, b)
        print(paste("Iteration:", i, "Autumn population (N_a):", N_a))

        # calculate m for autmn population
        m <- m(N_a)
        print(paste("Iteration:", i, "Winter mortality rate (m):", m))

        results <- rbind(results, data.frame(iteration=i, N_s=N_s, N_a=N_a, b=b, m=m))

    }
    return(results)
}

results <- pred(10, 20)

# Plot the results using ggplot2
library(ggplot2)

# Example plot for Spring population (N_s) over iterations
ggplot(results, aes(x=iteration, y=N_s)) +
    geom_line() +
    labs(title="Spring Population Over Time", x="Iteration", y="Spring Population (N_s)")


# Na = Ns + b*Ns
# Ns = Na - m*Na

# Na: autmn population size
# Ns: spring population size
# b: per capita birth rate
# m: mortality rate 

N <- 10

# Code for IBM modelling fish migration past a hindrance, i.e. a dam

#' Install and load packages *dplyr*, *ggplot2*, *stringr*, *lubridate* and *httpgd*.
setup_packages <- function() {
    if (!require(dplyr)) {
        print("Installing dplyr")
        install.packages("gridExtra")
    }
    if (!require(ggplot2)) {
        print("Installing ggplot2")
        install.packages("gridExtra")
    }
    if (!require(stringr)) {
        print("Installing stringr")
        install.packages("stringr")
    }
    if (!require(lubridate)) {
        print("Installing lubridate")
        install.packages("lubridate")
    }
    if (!require(httpgd)) {
        print("Installing httpgd")
        install.packages("httpgd")
    }
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(httpgd) # Pakke for plots i VS Code
    print("Packages loaded")
}

setup_packages()
hgd()
hgd_view()

### Simulation ###

# Parameters

### Parameters
#### Number of Fish at Start, individuals
num_fish <- 1000
#### Simulation Runtime, years
max_time <- 10
#### Juvenile Vinter Mortality, factor
juv_mort <- 0.5
#### Migrants Vinter Mortality, factor
mig_mort <- 0.1
#### Base Mortality for Migrations, factor
mig_mort_base <- 0.2
#### Base Mortality at Sea, factor
sea_mort_base <- 0.2
#### Growth of Juveniles, length in cm
growth_juv <- function() {
    return(sample(2:7, 1))
}
#### Growth of Migrants, length in cm
growth_mig <- function() {
    return(sample(5:15, 1))
}

#### Flow Threshold for Ice Hatch Migrations, flow-rate
flow_th <- 10
#### Turbine Mortatily, factor
turb_mort <- 0.5

initialize_population <- function(n) {
    stages <- sample(c("juvenile", "migrant"), n, replace = TRUE, prob = c(0.8, 0.2))
    lengths <- ifelse(
        stages == "juvenile", pmax(rnorm(n, mean = 12.5, sd = 5), 5), rnorm(n, mean = 62.5, sd = 15)
    )

    data.frame(
        id = 1:n,
        stage = stages,
        length = lengths,
        alive = TRUE
    )
}

winter_update <- function(population) {
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]

    juveniles$alive <- runif(nrow(juveniles)) > juv_mort
    migrants$alive <- runif(nrow(migrants)) > mig_mort_base

    juveniles$length <- juveniles$length + growth_juv()

    population <- rbind(juveniles, migrants)
    return(population)
}



population <- initialize_population(100)
population2 <- winter_update(population)

print(population)
hist(population$length)
hist(population2$length)

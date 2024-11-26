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
#### Juvenile Mortality, factor
juv_mort <- 0.3
#### Base Mortality for Migrations, factor
mig_mort_base <- 0.2
#### Base Mortality at Sea, factor
sea_mort_base <- 0.2
#### Growth of Juveniles, length in cm
growth_juv <- sample(2:7)
#### Growth of Migrants, length in cm
growth_mig <- sample(5:15)
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



population <- initialize_population(100)
print(population)
hist(population$length)

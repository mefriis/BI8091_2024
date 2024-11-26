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
#### Base Mortality for Migrants, factor
mig_mort_base <- 0.05
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
#### Turbine Base Mortatily, function
turb_mort_base <- 0.2

#' Initialize the population
#' @return data.frame with columns id, stage, length and alive
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

#' Update the population during winter: from spawning to downstream migration
winter_update <- function(population) {
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]

    juveniles$alive <- runif(nrow(juveniles)) > juv_mort
    migrants$alive <- runif(nrow(migrants)) > mig_mort_base

    juveniles$length <- juveniles$length + growth_juv()

    population <- rbind(juveniles, migrants)
    return(population)
}
#' Update the population during spring migration
spring_migration <- function(population, flow) {
    # Identify old migrants
    old_migrants <- population[population$stage == "migrant" & population$alive, ]
    # Identify new migrants, which are juveniles above a certain length
    new_migrants <- population[population$stage == "juvenile" & population$length > 15 & population$alive, ]

    # Identify remaining juveniles
    remaining_juveniles <-
        population[population$stage == "juvenile" & population$length <= 15 & population$alive, ]
    # Remove new migrants from juveniles
    remaining_juveniles <-
        remaining_juveniles[!remaining_juveniles$id %in% new_migrants$id, ]

    # Combine old and new migrants
    migrants <- rbind(old_migrants, new_migrants)

    #### Calculate mortality rate for migrants
    # Mortality rate is a function of base mortality and length
    migrants$mortality_rate <- mig_mort_base + turb_mort_base + 0.005 * migrants$length
    # Limit mortality rate to 1
    migrants$mortality_rate <- pmin(migrants$mortality_rate, 1)

    #### Scenarios for migrants depending on flow
    # If flow is below threshold, all migrants must pass the turbine
    # If flow is above threshold, each fish only has a 30% chance of passing the turbine
    if (flow < flow_th) {
        migrants$alive <- runif(nrow(migrants)) > migrants$mortality_rate

    } else {
        # Determine which fish pass the turbine
        turb <- runif(nrow(migrants)) < 0.3

        # Mortality due to turbine passage
        by_turb <- turb & (runif(nrow(migrants)) < migrants$mortality_rate)

        # Mortality due to base mortality when not passing the turbine
        by_base <- !turb & (runif(nrow(migrants)) < mig_mort_base)

        # Identify migrants that survive
        migrants$alive <- !(by_turb | by_base)
    }

    # Combine remaining juveniles and migrants back into the population
    population <- rbind(remaining_juveniles, migrants)
    return(population)
}


population <- initialize_population(100)
population2 <- winter_update(population)
population3 <- spring_migration(population2, 6)

print(population)
hist(population$length)
hist(population2$length)
hist(population3$length)
print(population3)

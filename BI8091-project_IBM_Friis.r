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


#' Initialize the population
#' @return data.frame with columns id, stage, length and alive
initialize_population <- function(n) {
    stages <- sample(c("juvenile", "migrant"), n, replace = TRUE, prob = c(0.8, 0.2))
    lengths <- ifelse(
        stages == "juvenile", pmax(rnorm(n, mean = 12.5, sd = 5), 5), rnorm(n, mean = 62.5, sd = 15)
    )
    females <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.5, 0.5))

    data.frame(
        id = 1:n,
        stage = stages,
        length = lengths,
        female = females,
        alive = TRUE
    )
}

#' Update the population during winter: from spawning to downstream migration
winter_update <- function(population, juv_mort, mig_winter_mort, growth_juv) {
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]

    juveniles$alive <- runif(nrow(juveniles)) > juv_mort
    migrants$alive <- runif(nrow(migrants)) > mig_winter_mort

    juveniles$length <- juveniles$length + growth_juv()

    population <- rbind(juveniles, migrants)
    return(population)
}

#' Update the population during spring migration
spring_migration <- function(population, flow, mig_mort_base, turb_mort_base, flow_th) {
    # Identify old migrants
    old_migrants <- population[population$stage == "migrant" & population$alive, ]
    # Identify new migrants, which are juveniles above a certain length
    new_migrants <- population[population$stage == "juvenile" & population$length > 15 & population$alive, ]
    # Change stage to migrant
    new_migrants$stage <- "migrant"

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

    # Remove mortality rate
    migrants$mortality_rate <- NULL

    # Combine remaining juveniles and migrants back into the population
    population <- rbind(remaining_juveniles, migrants)
    return(population)
}

#' Update the population during summer
summer_update <- function(population, juv_mort, sea_mort, growth_juv, growth_mig) {
    # Identify juveniles and migrants
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]

    # Assess mortality
    juveniles$alive <- runif(nrow(juveniles)) > juv_mort
    migrants$alive <- runif(nrow(migrants)) > sea_mort

    # Update lengths
    juveniles$length <- juveniles$length + growth_juv()
    migrants$length <- migrants$length + growth_mig()

    # Combine juveniles and migrants
    population <- rbind(juveniles, migrants)
    return(population)
}

#' Autmn Migration
autmn_migration <- function(population, mig_mort_base) {
    # Identify juveniles and migrants
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]

    # Assess mortality
    migrants$alive <- runif(nrow(migrants)) > mig_mort_base

    # Combine juveniles and migrants
    population <- rbind(juveniles, migrants)
    return(population)
}

#' Spawning
spawning <- function(population, juv_per_female) {
    # Identify juveniles and migrants
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]

    #### Assess which migrants can spawn
    females <- migrants[migrants$female, ]
    males <- migrants[!migrants$female, ]

    # Only spawn if there are both females and males alive
    if (nrow(females) > 0 && nrow(males) > 0) {
        print("Spawning")
        # Define the number of new juveniles
        num_new_juveniles <- nrow(females) * juv_per_female

        # Create new juveniles data frame
        new_juveniles <- data.frame(
            id = max(population$id) + 1:num_new_juveniles,
            stage = rep("juvenile", num_new_juveniles),
            length = rnorm(num_new_juveniles, mean = 2, sd = 0.25),
            female = sample(c(TRUE, FALSE), num_new_juveniles, replace = TRUE),
            alive = rep(TRUE, num_new_juveniles)
        )

        # Combine new juveniles, juveniles and migrants
        population <- rbind(new_juveniles, juveniles, migrants)

    } else {
        print("No spawning")
        # Combine juveniles and migrants
        population <- rbind(juveniles, migrants)
    }

    return(population)
}

#' Simulation

simulation <- function(
    ### Parameters
    #### Number of Fish at Start, individuals
    num_fish,
    #### Simulation Runtime, years
    max_time,
    #### Juvenile Bi-Annual Mortality, factor
    juv_mort,
    #### Migrants Winter Mortality, factor
    mig_winter_mort,
    #### Base Mortality for Migrants, factor
    mig_mort_base,
    #### Mortality at Sea, factor
    sea_mort,
    #### Growth of Juveniles, Bi-Annual, length in cm
    growth_juv,
    #### Growth of Migrants, length in cm
    growth_mig,
    #### Flow in River, flow-rate
    flow,
    #### Flow Threshold for Ice Hatch Migrations, flow-rate
    flow_th,
    #### Turbine Base Mortatily, function
    turb_mort_base,
    #### Juvenile Fish per Spawning Female
    juv_per_female
) {
    # Initialize population
    population <- initialize_population(num_fish)

    # Run simulation
    for (t in 1:max_time) {
        print(paste("Year", t))

        # Winter update
        population <- winter_update(population, juv_mort, mig_winter_mort, growth_juv)

        # Spring migration
        population <- spring_migration(population, flow, mig_mort_base, turb_mort_base, flow_th)   

        # Summer update
        population <- summer_update(population, juv_mort, sea_mort, growth_juv, growth_mig)

        # Autmn migration
        population <- autmn_migration(population, mig_mort_base)

        # Spawning
        population <- spawning(population, juv_per_female)
    }
    return(population)
}

pop <- data.frame()
pop <- simulation(
    num_fish = 1000,
    max_time = 6,
    juv_mort = 0.25,
    mig_winter_mort = 0.1,
    mig_mort_base = 0.05,
    sea_mort = 0.2,
    growth_juv = function() {
        return(sample(1:3, 1))
    },
    growth_mig = function() {
        return(sample(5:15, 1))
    },
    flow = 5,
    flow_th = 10,
    turb_mort_base = 0.2,
    juv_per_female = 100
)

hist(pop$length, breaks = 20, main = "Fish Length Distribution", xlab = "Length (cm)")

# Parameters

# ### Parameters
# #### Number of Fish at Start, individuals
# num_fish <- 1000
# #### Simulation Runtime, years
# max_time <- 10
# #### Juvenile Bi-Annual Mortality, factor
# juv_mort <- 0.25
# #### Migrants Winter Mortality, factor
# mig_winter_mort <- 0.1
# #### Base Mortality for Migrants, factor
# mig_mort_base <- 0.05
# #### Mortality at Sea, factor
# sea_mort <- 0.2
# #### Growth of Juveniles, Bi-Annual, length in cm
# growth_juv <- function() {
#     return(sample(1:3, 1))
# }
# #### Growth of Migrants, length in cm
# growth_mig <- function() {
#     return(sample(5:15, 1))
# }
# #### Flow Threshold for Ice Hatch Migrations, flow-rate
# flow_th <- 10
# #### Turbine Base Mortatily, function
# turb_mort_base <- 0.2
# #### Juvenile Fish per Spawning Female
# juv_per_female <- 100


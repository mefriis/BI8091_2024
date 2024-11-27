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
#' @param n Number of fish
initialize_population <- function(n) {
    stages <- sample(c("juvenile", "migrant"), n, replace = TRUE, prob = c(0.95, 0.05))
    lengths <- ifelse(
        stages == "juvenile", pmax(rnorm(n, mean = 5, sd = 7), 5), rnorm(n, mean = 62.5, sd = 30)
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

#' Juvenile growth
#' @param n Number of juvenile fish
#' @return Growth of juvenile fish
juvnile_growth <- function(n) {
    base_growth <- rnorm(1, mean = 3, sd = 1)
    base_growth <- pmax(base_growth, 0) # Growth cannot be negative
    density_dependent <- 1 - (1 / 10000) * n # 1 at n=0, and 0 at n=10000
    return(base_growth + density_dependent)
}

#' Update the population during winter: from spawning to downstream migration
#' @param population data.frame with columns id, stage, length and alive
#' @param juv_mort Juvenile mortality rate
#' @param mig_winter_mort Migrant winter mortality rate
#' @return Updated population
winter_update <- function(population, juv_mort, mig_winter_mort) {

    # Remove dead fish
    juveniles <- population[population$stage == "juvenile" & population$alive,]
    migrants <- population[population$stage == "migrant" & population$alive,]

    # Assess mortality
    juveniles$alive <- runif(nrow(juveniles)) > juv_mort
    migrants$alive <- runif(nrow(migrants)) > mig_winter_mort

    n_juv <- sum(juveniles$alive)
    print(paste("Number of juveniles:", n_juv))
    # Update lengths
    juveniles$length <- juveniles$length + juvnile_growth(n_juv)
    # Combine juveniles and migrants
    population <- rbind(juveniles, migrants)
    return(population)
}

#' Update the population during spring migration
#' @param population data.frame with columns id, stage, length and alive
#' @param flow Flow in the river
#' @param mig_mort_base Base mortality rate for migrants
#' @param turb_mort_base Turbine mortality rate
#' @param flow_th Flow threshold for ice hatch migrations
spring_migration <- function(population, flow, mig_mort_base, turb_mort_base, flow_th) {
    # Identify old migrants
    old_migrants <- population[population$stage == "migrant" & population$alive, ]
    # Identify new migrants, which are juveniles above a certain length
    new_migrants <- population[population$stage == "juvenile" & population$length > 15 & population$alive, ]

    if (nrow(new_migrants) == 0) {
        print("No new migrants")

        # Migrants are only the old migrants
        migrants <- old_migrants
    } else {
        print(paste("Number of new migrants:", nrow(new_migrants)))
        # Change stage to migrant
        new_migrants$stage <- "migrant"

        # Combine old and new migrants
        migrants <- rbind(old_migrants, new_migrants)
    }
    print(paste("Number of migrants:", nrow(migrants)))

    # Identify remaining juveniles
    remaining_juveniles <-
        population[population$stage == "juvenile" & population$length <= 15 & population$alive, ]
    # Remove new migrants from juveniles
    remaining_juveniles <-
        remaining_juveniles[!remaining_juveniles$id %in% new_migrants$id, ]


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
#' @param population data.frame with columns id, stage, length and alive
#' @param juv_mort Juvenile mortality rate
#' @param sea_mort Mortality at sea
#' @param growth_mig Function for migrant growth
#' @return Updated population
summer_update <- function(population, juv_mort, sea_mort, growth_mig) {
    # Identify juveniles and migrants
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]

    # Assess mortality
    juveniles$alive <- runif(nrow(juveniles)) > juv_mort
    migrants$alive <- runif(nrow(migrants)) > sea_mort

    n_juv <- sum(juveniles$alive)
    (print(paste("Number of juveniles:", n_juv)))

    # Update lengths
    juveniles$length <- juveniles$length + juvnile_growth(n_juv)
    migrants$length <- migrants$length + growth_mig()

    # Combine juveniles and migrants
    population <- rbind(juveniles, migrants)
    return(population)
}

#' Autmn Migration
#' @param population data.frame with columns id, stage, length and alive
#' @param mig_mort_base Base Mortality for Migrants, factor
#' @return Updated population
autmn_migration <- function(population, mig_mort_base) {
    # Identify juveniles and migrants
    juveniles <- population[population$stage == "juvenile" & population$alive,]
    migrants <- population[population$stage == "migrant" & population$alive,]

    # Assess mortality
    migrants$alive <- runif(nrow(migrants)) > mig_mort_base

    # Combine juveniles and migrants
    population <- rbind(juveniles, migrants)
    return(population)
}

#' Spawning
#' @param population data.frame with columns id, stage, length and alive
#' @param juv_per_female Juvenile Fish per Spawning Female
#' @return Updated population
spawning <- function(mID, population, juv_per_female) {
    # Identify juveniles and migrants
    juveniles <- population[population$stage == "juvenile" & population$alive,]
    migrants <- population[population$stage == "migrant" & population$alive,]

    #### Assess which migrants can spawn
    females <- migrants[migrants$female,]
    males <- migrants[!migrants$female,]

    # Only spawn if there are both females and males alive
    if (nrow(females) > 0 && nrow(males) > 0) {
        print("Spawning")
        # Define the number of new juveniles
        num_new_juveniles <- nrow(females) * juv_per_female

        # Create new juveniles data frame
        new_juveniles <- data.frame(
            id = mID + 1:num_new_juveniles,
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

#' Assess Mortality
#' @param population data.frame with columns id, stage, length and alive
#' @param year Year in simulation
#' @param season Season during year in simulation
asses_mortality <- function(population, year, season) {
    unalived <- population[!population$alive,]
    if (nrow(unalived) > 0) {
        unalived$year <- year
        unalived$season <- season
    }
    return(unalived)
}

#' Assess Count
#' @param population data.frame with columns id, stage, length and alive
#' @param year Year in simulation
#' @param season Season during year in simulation
#' @return List with year, season, juvenile, migrant and number of female migrants
asses_count <- function(population, year, season) {
    result <- list()
    result$year <- year
    result$season <- season
    result$juveniles <- sum(population$stage == "juvenile")
    result$migrants <- sum(population$stage == "migrant")
    result$migrants_female <- sum(population$stage == "migrant" & population$female)
    return(result)
}

#' Simulation
#' @param num_fish Number of Fish at Start, individuals
#' @param max_time Simulation Runtime, years
#' @param juv_mort Juvenile Bi-Annual Mortality, factor
#' @param mig_winter_mort Migrants Winter Mortality, factor
#' @param mig_mort_base Base Mortality for Migrants, factor
#' @param sea_mort Mortality at Sea, factor
#' @param growth_juv Growth of Juveniles, Bi-Annual, length in cm
#' @param growth_mig Growth of Migrants, length in cm
#' @param flow Flow in River, flow-rate
#' @param flow_th Flow Threshold for Ice Hatch Migrations, flow-rate
#' @param turb_mort_base Turbine Base Mortatily, function
#' @param juv_per_female Juvenile Fish per Spawning Female
#' @param undertaker Save the unalived Fish, data.frame
#' @param snap Take a snapshot of the population after each winter if TRUE
#' @param count Count the number of fish in each stage if TRUE
#' @return
#' **population**, data.frame with columns id, stage, length and alive
simulation <- function(num_fish, max_time, juv_mort, mig_winter_mort, mig_mort_base,
    sea_mort, growth_juv, growth_mig, flow, flow_th, turb_mort_base, juv_per_female,
    undertaker, snap, count
) {
    # Initialize population
    population <- initialize_population(num_fish)
    # Set mID to the highest ID in the population
    mID <- max(population$id)
    # Initialize unalived
    unalived <- data.frame()
    # Initialize history, which takes a snapshot of the population after each winter
    history <- data.frame()
    # Initialize count, which counts the number of fish in each stage
    result <- data.frame()

    # Run simulation
    for (t in 1:max_time) {
        print(paste("Year", t))

        ### Winter update
        population <- winter_update(population, juv_mort, mig_winter_mort)

        # Assess mortality
        if  (undertaker == TRUE) {
            unalived <- rbind(unalived, asses_mortality(population, t, "winter"))
        }

        # Take a snapshot of the population if snap is TRUE
        if (snap == TRUE) {
            # Saves living individuals in snapshot
            snapshot <- population[population$alive == TRUE,]
            snapshot$year <- t
            history <- rbind(history, snapshot)
        }

        # Count the number of fish in each stage if count is TRUE
        if (count == TRUE) {
            result <- rbind(result, asses_count(population, t, "winter"))
        }

        ### Spring migration
        population <- spring_migration(population, flow, mig_mort_base, turb_mort_base, flow_th)
        # Assess mortality
        if  (undertaker == TRUE) {
            unalived <- rbind(unalived, asses_mortality(population, t, "spring"))
        }

        # Count the number of fish in each stage if count is TRUE
        if (count == TRUE) {
            result <- rbind(result, asses_count(population, t, "spring"))
        }

        ### Summer update
        population <- summer_update(population, juv_mort, sea_mort, growth_mig)
        # Assess mortality
        if  (undertaker == TRUE) {
            unalived <- rbind(unalived, asses_mortality(population, t, "summer"))
        }
        # Count the number of fish in each stage if count is TRUE
        if (count == TRUE) {
            result <- rbind(result, asses_count(population, t, "summer"))
        }

        ### Autmn migration
        population <- autmn_migration(population, mig_mort_base)
        # Assess mortality
        if  (undertaker == TRUE) {
            unalived <- rbind(unalived, asses_mortality(population, t, "autmn"))
        }

        # Count the number of fish in each stage if count is TRUE
        if (count == TRUE) {
            result <- rbind(result, asses_count(population, t, "autmn"))
        }

        ### Spawning
        population <- spawning(mID, population, juv_per_female)

        # Check if population is extinct
        if (nrow(population) == 0) {
            print("Population extinct")
            return(list(population = population, unalived = unalived, history = history, result = result))
        }
        # Update mID
        mID <- max(population$id)
    }
    return(list(population = population, unalived = unalived, history = history, result = result))
}

pops <- simulation(
    num_fish = 10000,
    max_time = 1000,
    juv_mort = 0.574,
    mig_winter_mort = 0.1,
    mig_mort_base = 0.05,
    sea_mort = 0.2,
    growth_mig = function() {
        return(sample(5:15, 1))
    },
    flow = 11,
    flow_th = 10,
    turb_mort_base = 0.2,
    juv_per_female = 70,
    undertaker = FALSE,
    snap = FALSE,
    count = TRUE
)

result <- pops$result
plot(result$year, result$migrants, type = "l", col = "blue", xlab = "Year", ylab = "Number of Fish", main = "Fish Population")
plot(result$year, result$juveniles, type = "l", col = "blue", xlab = "Year", ylab = "Number of Fish", main = "Fish Population")


pop <- pops$population
ded <- pops$unalived
history <- pops$history
result <- pops$result
result

hist

plot(result$year, result$migrants, type = "l", col = "blue", xlab = "Year", ylab = "Number of Fish", main = "Fish Population")

sum(pop$stage == "migrant")
sum(pop$stage == "juvenile")

boxplot(ded$length ~ ded$season, main = "Dead Fish Length Distribution", xlab = "Season", ylab = "Length (cm)")
sum(ded$season == "winter")
sum(ded$season == "spring")
sum(ded$season == "summer")
sum(ded$season == "autmn")
hist(pop$length, breaks = 100, main = "Fish Length Distribution", xlab = "Length (cm)")
str(pop)

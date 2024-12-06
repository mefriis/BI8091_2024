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
    stages <- sample(c("juvenile", "migrant"), n, replace = TRUE, prob = c(0.96, 0.04))
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
#' @return Growth of juvenile fish dependent on density
juvnile_growth <- function(n) {
    base_growth <- rnorm(1, mean = 3, sd = 1)
    base_growth <- pmax(base_growth, 0) # Growth cannot be negative
    # density_dependent <- (750 / n)^3
    density_dependent <- 1 - (1 / 10000) * n
    density_dependent <- pmin(density_dependent, 1) # not more than 1
    return(base_growth + density_dependent)
}

#' Juvenile mortality
#' @param n Number of juvenile fish
#' @return Mortality of juvenile fish depending on the number of density
juvenile_density_mortality <- function(n) {
    base_mortality <- rnorm(1, mean = 0.2, sd = 0.1)
    base_mortality <- pmax(base_mortality, 0) # Mortality cannot be negative
    density_dependent <- (n / 30000)^2 # 25 000
    return(base_mortality + density_dependent)
}

update_living_juveniles <- function(juveniles) {
    # Calculate current mortality rate
    current_mortality_rate <- juvenile_density_mortality(sum(juveniles$alive))
    # Apply mortality
    juveniles$alive <- runif(nrow(juveniles)) > current_mortality_rate

    return(juveniles)
}

#' Migrant density mortality
#' @param n Number of juvenile fish
#' @return Mortality of juvenile fish depending on the number of density
migrant_density_mortality <- function(n) {
    base_mortality <- rnorm(1, mean = 0.05, sd = 0.01)
    base_mortality <- pmax(base_mortality, 0) # Mortality cannot be negative
    density_dependent <- (n / 800)^2
    return(base_mortality + density_dependent)
}

# Update the migrants' alive status dynamically
update_living_migrants <- function(migrants) {
    for (i in seq_len(nrow(migrants))) {
        current_mortality_rate <- migrant_density_mortality(sum(migrants$alive))
        migrants$alive[i] <- runif(1) > current_mortality_rate
    }
    return(migrants)
}

#' Update the population during winter: from spawning to downstream migration
#' @param population data.frame with columns id, stage, length and alive
#' @param juv_mort Juvenile mortality rate
#' @param mig_winter_mort Migrant winter mortality rate
#' @return Updated population
winter_update <- function(population, juv_mort, mig_winter_mort) {

    # Remove dead fish
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]

    n_juv <- sum(juveniles$alive)
    print(paste("Number of juveniles:", n_juv))

    # Assess mortality
    #juveniles$alive <- runif(nrow(juveniles)) > juvenile_density_mortality(n_juv)
    juveniles <- update_living_juveniles(juveniles)
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
    new_migrants <- population[population$stage == "juvenile" & population$length >= 17 & population$alive, ]

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

    ### Juvenile mortality
    remaining_juveniles <- update_living_juveniles(remaining_juveniles)

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

    n_juv <- sum(juveniles$alive)
    print(paste("Number of juveniles:", n_juv))

    # Assess mortality
    #juveniles$alive <- runif(nrow(juveniles)) > juvenile_density_mortality(n_juv)
    juveniles <- update_living_juveniles(juveniles)
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
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]

    # Assess mortality
    # migrants$alive <- runif(nrow(migrants)) > migrant_density_mortality(nrow(migrants))
    juveniles <- update_living_juveniles(juveniles)
    migrants <- update_living_migrants(migrants)

    # Combine juveniles and migrants
    population <- rbind(juveniles, migrants)
    return(population)
}

#' Spawning
#' @param population data.frame with columns id, stage, length and alive
#' @param juv_per_female Juvenile Fish per Spawning Female
#' @param redd_cap Maximum number of reds avaible for spawning
#' @return Updated population
spawning <- function(mID, population, juv_per_female, redd_cap) {
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
        number_of_females <- nrow(females)

        if (number_of_females > redd_cap) {
            num_new_juveniles <- redd_cap * juv_per_female
        } else {
            num_new_juveniles <- nrow(females) * juv_per_female

        }

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
    unalived <- population[!population$alive, ]
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

#' Flooding
#' @param population data.frame with columns id, stage, length and alive
#' @return Updated population
flood <- function(population) {
    population <- population[population$alive, ]
    population$alive <- runif(nrow(population)) > 0.9
    return(population)
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
#' @param flow_flux True if flow is to vary, boolean.
#' If False, flow is constant. If True, flow is normal distributed around value of **flow**
#' @param turb_mort_base Turbine Base Mortatily, function
#' @param juv_per_female Juvenile Fish per Spawning Female
#' @param redd_cap Maximum number of reds avaible for spawning
#' @param flood_interval Interval for Flooding, years
#' @param undertaker Save the unalived Fish, data.frame
#' @param snap Take a snapshot of the population after each winter if TRUE
#' @param count Count the number of fish in each stage if TRUE
#' @return
#' **population**, data.frame with columns id, stage, length and alive
simulation <- function(num_fish, max_time, juv_mort, mig_winter_mort, mig_mort_base,
    sea_mort, growth_juv, growth_mig, flow, flow_th, flow_flux, turb_mort_base,
    juv_per_female, redd_cap, flood_interval, flood_interval_th, undertaker, snap, count
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
            snapshot <- population[population$alive == TRUE, ]
            snapshot$year <- t
            history <- rbind(history, snapshot)
        }

        # Count the number of fish in each stage if count is TRUE
        if (count == TRUE) {
            result <- rbind(result, asses_count(population, t, "winter"))
        }

        ### Spring migration
        if (flood_interval > 0 && flood_interval_th <= t && t %% flood_interval == 0) {
            print("Flood")
            print(sum(population$alive == TRUE))
            population <- flood(population)
            print(sum(population$alive == TRUE))
        }
        if (flow_flux == TRUE) {
            fluxed_flow <- rnorm(1, mean = flow, sd = 3)
            population <- spring_migration(population, fluxed_flow, mig_mort_base, turb_mort_base, flow_th)
            print(paste("Flow:", fluxed_flow))
            print(nrow(population))

        } else {
            population <- spring_migration(population, flow, mig_mort_base, turb_mort_base, flow_th)
            print(nrow(population))
        }

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
        population <- spawning(mID, population, juv_per_female, redd_cap)

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
    juv_mort = 0.4,
    mig_winter_mort = 0.1,
    mig_mort_base = 0.1,
    sea_mort = 0.1,
    growth_mig = function() {
        return(rnorm(1, mean = 7, sd = 3))
    },
    flow = 11,
    flow_th = 10,
    flow_flux = FALSE,
    turb_mort_base = 0.1,
    juv_per_female = 30,
    redd_cap = 100,
    flood_interval = 0,
    flood_interval_th = 500,
    undertaker = FALSE,
    snap = FALSE,
    count = TRUE
)

result <- pops$result
result  %>%
    filter(season == "winter") %>%
    ggplot(aes(x = year, y = juveniles)) +
    geom_line() + geom_line(aes(y = migrants), color = "blue") +
    labs(title = "Fish Population", x = "Year", y = "Number of Fish") +
    theme_minimal()

result %>%
    filter(season == "winter", year >= 700, year <= 800) %>%
    ggplot(aes(x = year, y = juveniles)) +
    geom_line() + 
    geom_line(aes(y = migrants), color = "blue") +
    labs(title = "Fish Population from Year 700 to 900", x = "Year", y = "Number of Fish") +
    theme_minimal()

result  %>% filter(season == "winter") %>%
    ggplot(aes(x = year, y = juveniles)) + geom_line(aes(y = juveniles))
result  %>% filter(season == "winter") %>%
    ggplot(aes(x = year, y = migrants)) + geom_line(aes(y = migrants))

hist(pop$length[pop$stage == "juvenile"])
hist(pop$length[pop$stage == "migrant"])

pop <- pops$population
ded <- pops$unalived
history <- pops$history
result <- pops$result

## TODO
# Undertaker for migrants only
# Snapshots for migrants only
# Function of extreme flow taking out fish at a given time interval

### Evo-modell
# Legge til en genetisk komponent som gir fisk en sannsynelighet for å velge en av to migrasjonstidspunkter
# Høsten: Større dødelighet som er fast
# Våren: Lavere dødelighet, men som med et tidsintervall er veldig mye høyere enn ved høsten

# Gjøre det om til en aseksuell modell hvor det kun modelleres hunner
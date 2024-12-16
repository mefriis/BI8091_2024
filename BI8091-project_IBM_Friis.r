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
    if (!require(broom)) {
        print("Installing broom")
        install.packages("broom")
    }
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(httpgd) # Pakke for plots i VS Code
    library(broom)
    print("Packages loaded")
}

setup_packages()
hgd()
hgd_view()

### PRINTS ###

#' Print Histograms for Genes Every 100 Years
#' @param history Data frame containing the population history
print_gene_histograms <- function(history, facet) {
    # Ensure ggplot2 is loaded
    if (!require(ggplot2)) {
        install.packages("ggplot2")
        library(ggplot2)
    }

    # Find snapshot interval
    years <- unique(history$year)

    # If facet is TRUE, create a single histogram for all years
    if (facet) {
        p <- ggplot(history, aes(x = gene)) +
            geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
            labs(title = "Distribution of Gene Value per Interval", x = "Gene Value", y = "Frequency") +
            theme_minimal() +
            facet_wrap(~year) +
            theme(
                strip.text = element_text(size = 10, margin = margin(t = 10, b = 10)),
                strip.background = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(hjust = 0.5)
            )
        print(p)
    } else {
        for (year in years) {
            year_data <- history[history$year == year, ]
            p <- ggplot(year_data, aes(x = gene)) +
                geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
                labs(title = paste("Gene Distribution in Year", year), x = "Gene Value", y = "Frequency") +
                theme_minimal()
            print(p)
        }
    }
}


### Simulation ###

#' Initialize the population
#' @return data.frame with columns id, stage, length and alive
#' @param n Number of fish
initialize_population <- function(n) {
    stages <- sample(c("juvenile", "migrant"), n, replace = TRUE, prob = c(0.96, 0.04))
    lengths <- ifelse(
        stages == "juvenile", pmax(rnorm(n, mean = 5, sd = 7), 5), pmax(rnorm(n, mean = 55, sd = 20), 15)
    )
    genes <- runif(n, min = 0, max = 1)

    data.frame(
        id = 1:n,
        stage = stages,
        length = lengths,
        gene = genes,
        alive = TRUE,
        migrating = FALSE
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

# Update the migrants' alive status dynamically based on density
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

    # Assess mortality
    #juveniles$alive <- runif(nrow(juveniles)) > juvenile_density_mortality(n_juv)
    juveniles <- update_living_juveniles(juveniles)
    migrants$alive <- runif(nrow(migrants)) > mig_winter_mort

    # Number of living juveniles
    n_juv <- sum(juveniles$alive)

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
    print(paste("Number of migrants total:", nrow(migrants)))

    # Identify remaining juveniles
    remaining_juveniles <-
        population[population$stage == "juvenile" & population$length <= 15 & population$alive, ]
    # Remove new migrants from juveniles
    remaining_juveniles <-
        remaining_juveniles[!remaining_juveniles$id %in% new_migrants$id, ]

    ### Juvenile mortality
    remaining_juveniles <- update_living_juveniles(remaining_juveniles)

    #### Calculate mortality rate for migrants
    # Identify migrants that are already migrating and currently not in the river
    aldready_migrating_migrants <- migrants[migrants$migrating, ]
    # Identify migrants that are in the river
    migrants <- migrants[!migrants$migrating, ]
    # Check if migrants data frame is not empty before assigning mortality_rate
    if (nrow(migrants) > 0) {
        # Mortality rate is a function of base mortality and length
        # Calculate mortality rate for migrants
        migrants$mortality_rate <- mig_mort_base + turb_mort_base + 0.005 * migrants$length
        # Limit mortality rate to 1
        migrants$mortality_rate <- pmin(migrants$mortality_rate, 1)
    }

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
    population <- rbind(remaining_juveniles, migrants, aldready_migrating_migrants)

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

    # Check if migrants data frame is not empty before assigning migration status
    if (nrow(migrants) > 0) {
        # Set migration status for migrants
        migrants$migrating <- FALSE
    } else {
        print("No migrants to update migration status")
    }

    # Assess mortality
    juveniles <- update_living_juveniles(juveniles)
    if (nrow(migrants) > 0) {
        migrants$alive <- runif(nrow(migrants)) > sea_mort
    } else {
        print("No migrants to assess mortality")
    }

    # Number of living juveniles
    n_juv <- sum(juveniles$alive)

    # Update lengths
    juveniles$length <- juveniles$length + juvnile_growth(n_juv)
    if (nrow(migrants) > 0) {
        migrants$length <- migrants$length + growth_mig()
    }

    # Combine juveniles and migrants
    population <- rbind(juveniles, migrants)
    return(population)
}

#' Autmn Migration for Migrants returning to the river
#' @param population data.frame with columns id, stage, length and alive
#' @param mig_mort_base Base Mortality for Migrants, factor
#' @return Updated population
autmn_migration <- function(population, mig_mort_base) {
    # Identify juveniles and migrants
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]

    # Assess mortality
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
spawning <- function(mID, population, juv_per_female, redd_cap, mutation_sd) {
    # Identify juveniles and migrants
    juveniles <- population[population$stage == "juvenile" & population$alive, ]
    migrants <- population[population$stage == "migrant" & population$alive, ]
    migrants <- migrants[order(migrants$length, decreasing = TRUE), ] # Sort by length, biggest first


    # Only spawn if there are both females and males alive
    if (nrow(migrants) > 0) {
        print("Spawning")
        # Define the number of new juveniles

        if (nrow(migrants) > redd_cap) {
            num_new_juveniles <- redd_cap * juv_per_female
        } else {
            num_new_juveniles <- nrow(migrants) * juv_per_female
        }

        # Get genes for new juveniles
        genes <- next_gen_genes(migrants, redd_cap, juv_per_female, num_new_juveniles, mutation_sd)

        # Create new juveniles data frame
        new_juveniles <- data.frame(
            id = mID + 1:num_new_juveniles,
            stage = rep("juvenile", num_new_juveniles),
            length = rnorm(num_new_juveniles, mean = 2, sd = 0.25),
            gene = genes,
            alive = TRUE,
            migrating = FALSE
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

#' Next Generation Genes
#' @param migrants data.frame with columns id, stage, length and alive
#' @param redd_cap Maximum number of reds avaible for spawning
#' @param juv_per_female Juvenile Fish per Spawning
#' @param num_new_juveniles Number of new juveniles
#' @param mutation_sd Standard deviation of mutation
next_gen_genes <- function(migrants, redd_cap, juv_per_female, num_new_juveniles, mutation_sd) {
    genes <- migrants$gene[1:redd_cap]
    genes <- rep(genes, each = juv_per_female)
    genes <- genes[1:num_new_juveniles] # Slice to correct length if to long
    # Cap mutated genes at 0 and 1
    genes <- pmax(pmin(mutate_genes(genes, mutation_sd), 1), 0)

    return(genes)
}

#' Mutate Genes
#' @param genes Genes to mutate
#' @param mutation_sd Standard deviation of mutation
#' @return Mutated genes
mutate_genes <- function(genes, mutation_sd) {
    mutation <- rnorm(length(genes), mean = 0, sd = mutation_sd)
    genes <- genes + mutation
    return(genes)
}

#' Post Spawning Migration for some migrants returning to sea after spawning
#' Changing migration status for post spawning migration migrants and updating alive status
#' @param population data.frame with columns id, stage, length and alive
#' @param post_spawn_mig_mort_base Base Mortality for Migrants, factor
#' @return Updated population
post_spawning_migration <- function(population, post_spawn_mig_mort_base) {
    # Identify juveniles and migrants
    migrants <- population[population$stage == "migrant" & population$alive, ]
    juveniles <- population[population$stage == "juvenile" & population$alive, ]

    # Fish with gene value below 0.5 have 75% chance of migrating, else 25%
    migrate_prob <- ifelse(migrants$gene < 0.5, 0.75, 0.25)
    migrants$migrating <- runif(nrow(migrants)) < migrate_prob

    # Identify migrating and non-migrating migrants
    migrating_migrants <- migrants[migrants$migrating, ]
    non_migrating_migrants <- migrants[!migrants$migrating, ]

    # Update alive status for migrating migrants
    migrating_migrants$alive <- runif(nrow(migrating_migrants)) > post_spawn_mig_mort_base

    # Combine juveniles and migrants
    population <- rbind(juveniles, migrating_migrants, non_migrating_migrants)

    # Return only living fish
    population <- population[population$alive, ]
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
    #result$season <- season
    result$juveniles <- sum(population$stage == "juvenile")
    result$migrants <- sum(population$stage == "migrant")
    result$migrants_length <- mean(population$length[population$stage == "migrant"], na.rm = TRUE)
    result$gene <- mean(population$gene, na.rm = TRUE)
    return(result)
}

#' Flooding
#' @param population data.frame with columns id, stage, length and alive
#' @return Updated population
flood <- function(population, flood_removal) {
    in_river <- population[population$alive & !population$migrating, ]
    migrating <- population[population$alive & population$migrating, ]

    print(paste("Migrants in the river before flood: ", sum(in_river$alive)))
    print(paste("Migrants in sea: ", sum(migrating$alive)))

    if (nrow(in_river) > 0) {
        in_river$alive <- runif(nrow(in_river)) > flood_removal
    }
    print(paste("Migrants in the river after flood: ", sum(in_river$alive)))

    population <- rbind(in_river, migrating)
    return(population)
}

#' Simulation
#' @param num_fish Number of Fish at Start, individuals
#' @param max_time Simulation Runtime, years
#' @param juv_mort Juvenile Bi-Annual Mortality, factor
#' @param mig_winter_mort Migrants Winter Mortality, factor
#' @param mig_mort_base Base Mortality for Migrants, factor
#' @param post_spawn_mig_mort_base Base Mortality for Post Spawning Migrants, factor
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
#' @param flood_interval_th Threshold for when flooding starts, years
#' @param flood_removal Proportion of fish in the river removed by flooding
#' @param undertaker Save the unalived Fish, data.frame
#' @param snap Take a snapshot of the population after each winter if TRUE
#' @param snap_interval Interval for taking snapshots, years
#' @param count Count the number of fish in each stage if TRUE
#' @return
#' **population**, data.frame with columns id, stage, length and alive
simulation <- function(num_fish, max_time, juv_mort, mig_winter_mort, mig_mort_base, post_spawn_mig_mort_base,
    sea_mort, growth_juv, growth_mig, flow, flow_th, flow_flux, turb_mort_base,
    juv_per_female, mutation_sd, redd_cap, flood_interval, flood_interval_th, flood_removal, undertaker, snap,
    snap_interval, count
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
        if (snap == TRUE && t %% snap_interval == 0) {
            # Saves living individuals in snapshot
            snapshot <- population[population$alive == TRUE, ]
            snapshot$year <- t
            history <- rbind(history, snapshot)
        }

        # # Count the number of fish in each stage if count is TRUE
        # if (count == TRUE) {
        #     result <- rbind(result, asses_count(population, t, "winter"))
        # }

        ### Spring migration
        if (flood_interval > 0 && flood_interval_th <= t && t %% flood_interval == 0) {
            print("Flood")
            print(paste("Before flood: ", sum(population$alive == TRUE)))
            population <- flood(population, flood_removal)
            print(paste("After flood: ",  sum(population$alive == TRUE)))
        }
        if (flow_flux == TRUE) {
            fluxed_flow <- rnorm(1, mean = flow, sd = 3)
            population <- spring_migration(population, fluxed_flow, mig_mort_base, turb_mort_base, flow_th)
            print(paste("Flow:", fluxed_flow))

        } else {
            population <- spring_migration(population, flow, mig_mort_base, turb_mort_base, flow_th)
        }

        # Assess mortality
        if  (undertaker == TRUE) {
            unalived <- rbind(unalived, asses_mortality(population, t, "spring"))
        }

        # # Count the number of fish in each stage if count is TRUE
        # if (count == TRUE) {
        #     result <- rbind(result, asses_count(population, t, "spring"))
        # }

        ### Summer update
        population <- summer_update(population, juv_mort, sea_mort, growth_mig)
        # Assess mortality
        if  (undertaker == TRUE) {
            unalived <- rbind(unalived, asses_mortality(population, t, "summer"))
        }
        # # Count the number of fish in each stage if count is TRUE
        # if (count == TRUE) {
        #     result <- rbind(result, asses_count(population, t, "summer"))
        # }

        ### Autmn migration
        population <- autmn_migration(population, mig_mort_base)

        ### Spawning
        population <- spawning(mID, population, juv_per_female, redd_cap, mutation_sd)

        ### Post Spawning Migration
        population <- post_spawning_migration(population, post_spawn_mig_mort_base)

        # Assess mortality
        if  (undertaker == TRUE) {
            unalived <- rbind(unalived, asses_mortality(population, t, "autmn"))
        }
        # Count the number of fish in each stage if count is TRUE
        if (count == TRUE) {
            result <- rbind(result, asses_count(population, t, "autmn"))
        }

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

# Fish with gene value below 0.5 have 75% chance of migrating post spawning,
# Fish with gene value above 0.5 will have 75% chance of staying in the river and migrating at spring
# Post Spawning Migrants: post_spawn_mig_bort_base
# Spring Migrants: mig_mort_base + turb_mort_base + 0.005 * migrants$length
# Both: mig_winter_mort and sea_mort

pops <- simulation(
    num_fish = 10000,
    max_time = 20000,
    snap = TRUE,
    snap_interval = 2000,
    juv_mort = 0.4,
    mig_winter_mort = 0.1,
    mig_mort_base = 0.1,
    post_spawn_mig_mort_base = 0.3,
    sea_mort = 0.1,
    growth_mig = function() {
        return(rnorm(1, mean = 7, sd = 3))
    },
    flow = 11,
    flow_th = 10,
    flow_flux = FALSE,
    turb_mort_base = 0.1,
    juv_per_female = 10,
    mutation_sd = 0.05,
    redd_cap = 300,
    flood_interval = 25,
    flood_interval_th = 10000,
    flood_removal = 0.8,
    undertaker = FALSE,
    count = TRUE
)

result <- pops$result
pop <- pops$population
history <- pops$history
print_gene_histograms(history, facet = TRUE)
mean_gene <- result %>%
    filter(season == "autmn") %>%
    group_by(year) %>%
    summarize(mean_gene = mean(gene, na.rm = TRUE))

### Plot population at autmn counts
result %>%
    filter(season == "autmn") %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = juveniles, color = "Juveniles")) +
    geom_line(aes(y = migrants, color = "Migrants")) +
    geom_line(data = mean_gene, aes(y = mean_gene * 2000, color = "Mean Gene"), linetype = "dashed") +
    geom_vline(xintercept = 10000, linetype = "longdash", color = "salmon", size = 1) + 
    labs(title = "Population in Autmn", x = "Year", y = "Number of Individuals") +
    scale_color_manual(values = c("Juveniles" = "black", "Migrants" = "blue", "Mean Gene" = "red"),
                       name = "Stage",
                       labels = c("Juveniles", "Mean Gene Value", "Migrants")) +
    scale_y_continuous(
        sec.axis = sec_axis(~ . / 2000, name = "Mean Gene Value", breaks = seq(0, 1, by = 0.1))
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

### Plot yearly mean gene value
result %>%
    ggplot(aes(x = year)) +
    geom_line(data = mean_gene, aes(y = mean_gene), linetype = "dashed") +
    geom_vline(xintercept = 10000, linetype = "longdash", color = "salmon", size = 1) + 
    labs(title = "Average Gene Value", x = "Year", y = "Number of Individuals") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

hist(history$gene)
hist(history$length[history$stage == "juvenile"])
hist(history$length[history$stage == "migrant"])

### Plot at time interval
result %>%
    filter(season == "autmn", year >= 400, year <= 600) %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = juveniles, color = "Juveniles")) +
    geom_line(aes(y = migrants, color = "Migrants")) +
    labs(title = "Population in Autmn", x = "Year", y = "Number of Individuals") +
    scale_color_manual(values = c("Juveniles" = "black", "Migrants" = "blue"),
                        name = "Stage",
                        labels = c("Juveniles", "Migrants")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))


ded <- pops$unalived
history <- pops$history
result <- pops$result

## TODO
# Function to run the simulation multiple times and and store the counts and snapshots of each run

### Evo-modell
# Legge til en genetisk komponent som gir fisk en sannsynelighet for å velge en av to migrasjonstidspunkter
# Høsten: Større dødelighet som er fast
# Våren: Lavere dødelighet, men som med et tidsintervall er veldig mye høyere enn ved høsten
# Endre flood funksjonen til å kun ta ut migranter som er i elven på våren
# Gjøre det om til en aseksuell modell hvor det kun modelleres hunner
# Cap gene at 0 and 1

big_run <- function(n) {
    big_res <- list()
    big_hist <- list()
    for (i in 1:n) {
        print(paste("Run", i))
        tryCatch({
            pops <- simulation(
                num_fish = 10000,
                max_time = 20000, # 20000
                snap = TRUE,
                snap_interval = 2000, # 2000
                juv_mort = 0.4,
                mig_winter_mort = 0.1,
                mig_mort_base = 0.1,
                post_spawn_mig_mort_base = 0.3,
                sea_mort = 0.1,
                growth_mig = function() {
                    return(rnorm(1, mean = 7, sd = 3))
                },
                flow = 11,
                flow_th = 10,
                flow_flux = FALSE,
                turb_mort_base = 0.1,
                juv_per_female = 10,
                mutation_sd = 0.05,
                redd_cap = 300,
                flood_interval = 25,
                flood_interval_th = 10000, # 10000
                flood_removal = 0.8,
                undertaker = FALSE,
                count = TRUE
            )
            big_res[[i]] <- pops$result
            big_hist[[i]] <- pops$history
        }, error = function(e) {
            print(paste("Error in run", i, ":", e$message))
        })
        time <- format(Sys.time(), "%Y%m%d_%H%M%S")
        file.create(paste0("number-", i, "_", time, ".txt"))
    }
    time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    saveRDS(big_res, file = paste0("big_res_", time, ".rds"))
    saveRDS(big_hist, file = paste0("big_hist_", time, ".rds"))
    return(list(result = big_res, history = big_hist))
}

big <- big_run(20)

big_res <- big$result
big_hist <- big$history


#' Calculate the average mean gene value, number of juveniles, and number of migrants for each year across multiple runs
#' @param big_res List of data frames containing the results of each simulation run
#' @return Data frame with the average mean gene value, number of juveniles, and number of migrants for each year
average_simulation_results <- function(big_res) {
    # remove runs with no results
    big_res <- big_res[sapply(big_res, function(x) !is.null(x))]
    # Combine the mean gene values, number of juveniles, and number of migrants from all runs
    combined_results <- do.call(rbind, lapply(big_res, function(x) {
        x %>%
            group_by(year) %>%
            summarize(
                mean_gene = mean(gene, na.rm = TRUE),
                num_juveniles = mean(juveniles, na.rm = TRUE),
                num_migrants = mean(migrants, na.rm = TRUE),
                migrants_length = mean(migrants_length, na.rm = TRUE)
            )
    }))

    # Calculate the average mean gene value, number of juveniles, and number of migrants for each year
    average_results <- combined_results %>%
        group_by(year) %>%
        summarize(
            avg_mean_gene = mean(mean_gene, na.rm = TRUE),
            avg_num_juveniles = mean(num_juveniles, na.rm = TRUE),
            avg_num_migrants = mean(num_migrants, na.rm = TRUE),
            avg_mig_length = mean(migrants_length, na.rm = TRUE)

        )

    return(average_results)
}

average_results <- average_simulation_results(big_res)

# Set the period variable based on the specified year ranges
average_results <- average_results %>%
    mutate(period = case_when(
        year >= 1000 & year < 10000 ~ "Before floodings",
        year > 11000 & year <= 20000 ~ "After floodings",
        TRUE ~ "None"
    ))

average_results$period <- factor(average_results$period, levels = c("Before floodings", "After floodings"))


### Plot population at autmn counts
average_results %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = avg_num_juveniles, color = "Juveniles")) +
    geom_line(aes(y = avg_num_migrants, color = "Migrants")) +
    geom_line(aes(y = avg_mean_gene * 2000, color = "Mean Gene"), linetype = "dashed") +
    geom_vline(xintercept = 10000, linetype = "longdash", color = "salmon", size = 1) + 
    labs(title = "Population in Autmn per Year", x = "Year", y = "Average Number of Individuals") +
    scale_color_manual(values = c("Juveniles" = "black", "Migrants" = "blue", "Mean Gene" = "red"),
                       name = "",
                       labels = c("Juveniles", "Average Mean Gene Value", "Migrants")) +
    scale_y_continuous(
        sec.axis = sec_axis(~ . / 2000, name = "Average Mean Gene Value", breaks = seq(0, 1, by = 0.1))
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.title.y.right = element_text(hjust = 0.89))

### Plot yearly mean gene value
average_results %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = avg_mean_gene), linetype = "dashed") +
    geom_vline(xintercept = 10000, linetype = "longdash", color = "salmon", size = 1) + 
    labs(title = "Average Gene Value per Year", x = "Year", y = "Average Number of Individuals") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))


# Split the data into before and after 10000 years
before_f_gene <- average_results %>%
    filter(period == "Before floodings") %>%
    pull(avg_mean_gene)

after_f_gene <- average_results %>%
    filter(period == "After floodings") %>%
    pull(avg_mean_gene)

# Perform a t-test
t_test_gene <- t.test(before_f_gene, after_f_gene)

# Split the data into before and after 10000 years and test for juv counts
before_f_juv <- average_results %>%
    filter(period == "Before floodings") %>%
    pull(avg_num_juveniles)

after_f_juv <- average_results %>%
    filter(period == "After floodings") %>%
    pull(avg_num_juveniles)

# Perform a t-test
t_test_juv <- t.test(before_f_juv, after_f_juv)

# Split the data into before and after 10000 years and test for migrant counts
before_f_mig <- average_results %>%
    filter(period == "Before floodings") %>%
    pull(avg_num_migrants)

after_f_mig <- average_results %>%
    filter(period == "After floodings") %>%
    pull(avg_num_migrants)

# Perform a t-test
t_test_mig <- t.test(before_f_mig, after_f_mig)

# Split the data into before and after 10000 years and test for migrant length
before_f_mig_length <- average_results %>%
    filter(period == "Before floodings") %>%
    pull(avg_mig_length)

after_f_mig_length <- average_results %>%
    filter(period == "After floodings") %>%
    pull(avg_mig_length)

# Perform a t-test
t_test_mig_length <- t.test(before_f_mig_length, after_f_mig_length)

# Create a data frame to store the t-test results
t_test_results <- bind_rows(
    tidy(t_test_gene) %>% mutate(test = "Gene Values"),
    tidy(t_test_juv) %>% mutate(test = "Juvenile Counts"),
    tidy(t_test_mig) %>% mutate(test = "Migrant Counts"),
    tidy(t_test_mig_length) %>% mutate(test = "Migrant Length")
)

# Print the t-test results table
print(t_test_results)

# Boxplot and Violin plot for gene values
ggplot(average_results %>%
    filter(year >= 1000) %>%
    filter(year < 10000 | year > 11000), aes(x = period, y = avg_mean_gene, fill = period)) +
    geom_violin(trim = FALSE, alpha = 0.5) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    labs(title = "Gene Values Before and After Floodings", x = "Period", y = "Average Mean Gene Value") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

# Boxplot and Violin plot for juvenile counts
ggplot(average_results %>%
    filter(year >= 1000) %>%
    filter(year < 10000 | year > 11000), aes(x = period, y = avg_num_juveniles, fill = period)) +
    geom_violin(trim = FALSE, alpha = 0.5) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    labs(title = "Juvenile Counts Before and After Floodings", x = "Period", y = "Average Number of Juveniles") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

# Boxplot and Violin plot for migrant counts
ggplot(average_results %>%
    filter(year >= 1000) %>%
    filter(year < 10000 | year > 11000), aes(x = period, y = avg_num_migrants, fill = period)) +
    geom_violin(trim = FALSE, alpha = 0.5) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    labs(title = "Migrant Counts Before and After Floodings", x = "Period", y = "Average Number of Migrants") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

# Boxplot and Violin plot for migrant length
ggplot(average_results %>%
    filter(year >= 1000) %>%
    filter(year < 10000 | year > 11000), aes(x = period, y = avg_mig_length, fill = period)) +
    geom_violin(trim = FALSE, alpha = 0.5) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    labs(title = "Migrant Length Before and After Floodings", x = "Period", y = "Average Migrant Length") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

### Population Snapshots

# Combine all data frames into one and add a 'run' variable
combined_hist <- bind_rows(lapply(seq_along(big_hist), function(i) {
    big_hist[[i]] %>%
        mutate(run = i)
}))

print_gene_histograms(combined_hist, facet = TRUE)

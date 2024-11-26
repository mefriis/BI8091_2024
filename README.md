# BI8091_2024
Repo for BI8091 autmn 2024.
This repo contains the code for an Individual-Based Model, **IBM**, simulating the success of a migrating fish population.
The code for the IBM is located in **BI8091-project_IBM_Friis.r**.

### Pseudo Code 

### Parameters
##### Number of Fish at Start, individuals
*num_fish*
##### Simulation Runtime, years
*max_time*
##### Juvenile Mortality, factor
*juv_mort*
##### Base Mortality for Migrations, factor
*mig_mort_base*
##### Base Mortality at Sea, factor 
*sea_mort_base*
##### Growth of Juveniles, length in cm
*growth_juv*
##### Growth of Residents, length in cm
*growth_res*
##### Growth of Migrants, length in cm
*growth_mig*
##### Flow Threshold for Ice Hatch Migrations, flow-rate
*flow_th*
##### Turbine Mortatily, factor
*turb_mort*

### Model Limitations

* Does not account for variations in migratory patterns.
* Does not account for differences between sexes.
* Does not account for variations in reproduction success.
* Does not account for the individuals ability to wait for (more) suitable environmental conditions before migrating. 

### Kvalene

#### Use of **data frame** *vs.* **vectors**
I choose to prioritze ease of development and readability over computational speed as I aim to make a simple model, and modelling one population of migratory fish in a river realistically does not reach such large sizes that vectorizations would be necessary. 
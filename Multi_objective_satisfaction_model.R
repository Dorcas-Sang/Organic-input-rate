# Optimum Organic Inputs (OI) along crop and toposequence combination
# over a 25-year horizon in Northern Ghana Guinea Savannah 

# This study asks: For maize and soybean at three toposequence positions (upper / mid /
##  lower)
# what annual OI rate provides near-maximum probability of simultaneously
# achieving:
#   (i)  SOC target,
#   (ii)  a crop-specific attainable-yield target, and
#   (iii) an acceptable 25-year discounted return on investment (DROI) target?

## Because the OI rate depends largely on the set threshold,
# we did a Robustness test by changing the objective thresholds to
# more stringent set of SOC, yield and DROI targets.

# The OI rate this model returns is in combination with 
# the mineral fertilizer already in the ISFM budget

# Required packages 

# install.packages(c("decisionSupport", "readxl", "dplyr", "tidyr",
                  # "stringr", "purrr", "ggplot2", "patchwork", "scales", "tibble"),
                  # dependencies = TRUE)


#### Load already installed packages ####

library(decisionSupport)  
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(patchwork)
library(scales)
library(tibble)

## 1. load input estimates

input_file <- "input_estimates.xlsx"

#Local place to keep the outputs

output_dir <- "outputs"
dir.create(file.path(output_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "tables"),  showWarnings = FALSE, recursive = TRUE)

#Load parameters

parameters <- readxl::read_excel(input_file, sheet = "parameters") |>
  dplyr::mutate(lower = as.numeric(lower), upper = as.numeric(upper))


# Creating a estimate object for Decision Support package 

input_estimates <- decisionSupport::as.estimate(
  variable     = parameters$variable,
  distribution = parameters$distribution,
  lower        = parameters$lower,
  upper        = parameters$upper
) 

##Development helper during model building to test chuncks of code in the function 

#Make_variables draws one parameter set into the Global
# Environment so the body of built function can be run line by line

make_variables <- function(input_estimates) {
  sampled_values <- decisionSupport::random(
    rho = input_estimates,
    n = 1)
  list2env(
    as.list(sampled_values[1, ]),
    envir = .GlobalEnv)
  invisible(sampled_values)
}

set.seed(2026) # Setting this for stability

make_variables(input_estimates)

## 2. Study design 

#### SCENARIO & TARGET SPECIFICATIONS ########################################

# Design choices most of which are constant

get_const <- function(v) parameters$lower[parameters$variable == v]
years         <- get_const("years")
discount_rate <- get_const("discount_rate")
oi_sweep      <- get_const("oi_sweep")   # maximum OI tested (t DM/ha/yr) 
#Because of the robustness test we extend the OI rate 10t DM/ha
# but 5t DM is the ressource efficient we wanted to test

oi_step       <- get_const("oi_step")
oi_grid       <- seq(0, oi_sweep, by = oi_step)
cat("OI grid: 0 to", oi_sweep, "by", oi_step, "->", length(oi_grid), "levels\n")
topos         <- c("upper", "mid", "lower") #Toposequence 
crops         <- c("Maize", "Soybean") #Crops assessed 

##Performance targets 
#Viable targets define the primary analysis
#Base and stringent targets are used for robustness scenario 
#The viability target asks only whether the farm remains economically sustainable; 
#the base target requires meaningful system performance; 
#and the stringent target asks whether the recommendation holds when 
#considerably more demanding expectations are imposed (aspirational targets)

target_spec <- tibble::tibble(
  scenario             = factor(c("Viability", "Base", "Stringent"),
                                levels = c("Viability", "Base", "Stringent")),
  soc_target_ratio     = (1 + c(0.001, 0.002, 0.004))^years, # Additional SOC from initial SOC
  maize_yield_target   = c(3.0, 4.0, 5.0), #Observable, achievable and potential yield
  soybean_yield_target = c(1.8, 2.0, 3.0),
  droi_target          = c(0.10, 0.25, 0.50) #Percentage Discounted return on investment 
)

#Ressource efficient rule
# We choose the lowest OI rate whose joint target achievemnt probability is 
#at least 95% of the maximum joint probability observed within the decision range 

near_max_fraction <- 0.95     #at 95 % near maximum achievement

## Output names encode the OI rate as an integer of hundredths (OI000 ... OI500)
## so that column names stay syntactically valid and machine-parseable
#Basically this helper converts an organic-input rate into a safe and
#consistently formatted label for output column names

oi_code <- function(x) sprintf("OI%03d", round(x * 100))

####3.Model function converting the conceptual model ####

oi_optimum_model <- function() {
  
# creating a flat named vector, converted to a list on return
# This creates an empty numeric vector called out
#The model gradually adds named results to it before MC simulation
# At the end of the model function, it is converted to a list
#This provides the named scalar outputs expected by MC simulation  
  
out <- numeric(0)   
  
##Organic inputs blend and nutrient balance
#This is composition of one tonne of dry matter 
  
# Farmers assemble organic input from crop residue, manure and compost in
# varying proportions. The three shares are normalized to sum to 1 
# and the blend of nutrients contributions are the share-weighted sum of each
  
organic_inputs <- blend_w_residue + blend_w_manure + blend_w_compost #total organic input blend
residue_share <- blend_w_residue / organic_inputs
manure_share <- blend_w_manure / organic_inputs
compost_share <- blend_w_compost / organic_inputs
  
# Composite Carbon per ton of OI (t C per t DM)
  
oi_carbon <- (residue_share * C_residue) +
    (manure_share * C_manure) + 
    (compost_share * C_compost)
  
## Plant-available nutrient supply of the blend (kg per t DM) i.e. the
## the concentration of each material multiplied by its first-season
## availability coefficient then weighted by the blend shares
## Here they are still per-tonne quantities, but they will be 
## a per-hectare supply only when multiplied by oi_rate inside simulate_system()
#Here Nitrogen, Phosphorous and Potassium
  
oi_N_total <- (residue_share * N_residue) +    ## Nitrogen
    (manure_share * N_manure) +
    (compost_share * N_compost)
  
oi_N_available <- (residue_share * N_residue * Navail_residue) + 
    (manure_share  * N_manure * Navail_manure) + 
    (compost_share * N_compost * Navail_compost)
  
oi_P_total <- (residue_share * P_residue) +     ## Phosphorus 
    (manure_share * P_manure) +
    (compost_share * P_compost)
  
oi_P_available <- (residue_share * P_residue * Pavail_residue) + 
    (manure_share  * P_manure * Pavail_manure) + 
    (compost_share * P_compost * Pavail_compost)
  
oi_K_total <- (residue_share * K_residue) +     ## potassium
    (manure_share * K_manure) +
    (compost_share * K_compost)
  
  oi_K_available <- (residue_share * K_residue * Kavail_residue)+
    (manure_share * K_manure * Kavail_manure) +
    (compost_share * K_compost * Kavail_compost)
  
  
#### 1. Uncertainities, risks and exogenous shocks ####
  
market_shock <- chance_event(p_market_disruption, 
                               value_if = market_disrupt_factor, 
                               value_if_not = 0,
                               n= years)
  
striga_year <- chance_event(p_striga, 
                              value_if = 1,
                              value_if_not = 0, 
                              n = years) #For maize only
  
faw_year <- chance_event( p_faw_outbreak,
                            value_if = 1,
                            value_if_not = 0,
                            n = years) #Fall army worm for maize only
  
drought_year <- chance_event(p_drought, 
                               value_if = 1, 
                               value_if_not = 0,
                               n = years)
  
flood_year <- chance_event(p_flood, 
                             value_if = 1, 
                             value_if_not = 0, 
                             n = years) # Affects lower slope
  
cart_access   <- chance_event(p_cart_access, #cart/tricycle availability
                                value_if = 1, 
                                value_if_not = 0,
                                n = years) #Transport of manure/compost to fields
  
# Credit: most northern-Ghana smallholders need finance upfront to get
# inputs so they get credit either formal or informal
# Whether a farmer is credit-constrained is a chance event,and if so then the input
# cost carries seasonal interest
  
credit_constrained <- chance_event(p_credit_constrained,
                                     value_if = 1,
                                     value_if_not = 0, 
                                     n= years)
  
#If farmers do not worn land, they may need to rent it or pay some token of appreciation to the land owner

land_rented <- chance_event(p_land_rented,
                              value_if = 1,
                              value_if_not = 0,
                              n = years)
  
#Inter annual climate anomalies 
  
annual_temp <- vv(var_mean= mean_temp, 
                    var_CV =  temp_cv, n= years)
  
rain_index <- pmax(rain_floor, 
                     vv(var_mean= 1, # for normal season
                        var_CV = rain_cv, n= years)) 
  
#### 2. INPUT COSTS ####
  
# Handling cost depends on how the material reaches the field
#residue left where it fell needs only spreading, while manure, compost and gathered
#residue must also be collected and hauled. Cart or tricycle access, the
#endowment most often reported to enable organic input use, substitutes cash
#hire for much of that carting labour
#The carted fraction reaches the field either by manual head-loading (labour only)
# OR by hired cart/tricycle (a cash fee that replaces that labour) - a substitution
# of cash for labour, not both at once. Spreading labour is paid either way.
  
# Labour is handled separately because labour
# requirement depends on the OI rate and because the household supplies most of it first
  
cash_cart_per_t <- ifelse(cart_access == 1,
                            oi_carted_fraction * c_cart_hire, 0)
  
labour_oi_per_t <- labour_spread +
    ifelse(cart_access == 1, 0, oi_carted_fraction * labour_carting)  # person-days per t DM per season or year
  
## Rent is a cash cost only for rented land but in Northern Ghana it is more complex than that
#because farmers give token of appreciation to the owner of land 
#But also this cost of renting land is included because even for a farmer who owns the land outright, 
# the opportunity cost of using it stands as a costs
# as farmers could rent it out, or use it for something else
  
paid_land_cost <- land_rented * land_rent
  
nonlabour_cash_cost <- vv(var_mean = c_land_prep,
                            var_CV = cost_cv, n = years) + paid_land_cost
  
## Field labour requirement, person-days per hectare per season.
  
  labour_field_days <- labour_planting + labour_weeding +
    labour_harvesting + labour_threshing
  
## Purchased inputs
#A credit-constrained year carries seasonal interest
  
## Fertilizer cost is derived from the same crop-specific N, P and K rates
## used in the biophysical model. This prevents low nutrient rates from being
## paired randomly with a high per-hectare fertilizer bill
  
maize_fertilizer_cost <- maize_N_rate * fertilizer_cost_N +
    maize_P_rate * fertilizer_cost_P + maize_K_rate * fertilizer_cost_K

soybean_fertilizer_cost <- soybean_N_rate * fertilizer_cost_N +
    soybean_P_rate * fertilizer_cost_P + soybean_K_rate * fertilizer_cost_K
  
maize_var_cash_cost <-
    vv(var_mean = maize_seed + maize_fertilizer_cost + maize_pesticide,
       var_CV = cost_cv, n = years) *
    (1 + credit_constrained * interest_rate_input)
  
soybean_var_cash_cost <-
    vv(var_mean = soybean_seed + soybean_fertilizer_cost +
         inoculant_cost + soybean_pesticide,
       var_CV = cost_cv, n = years) *
    (1 + credit_constrained * interest_rate_input)
  
## Common random numbers that will be called once per Monte Carlo run and then reused
## at every OI rate. This makes each profit comparison genuinely paired

labour_cost_multiplier <- vv(var_mean = 1, var_CV = cost_cv, n = years)
oi_cost_multiplier <- vv(var_mean = 1, var_CV = cost_cv, n = years)
  
## Effective farm-gate prices (GHS t-1)
#A market disruption will affect the price at which the crop is sold
## hence if there is market shock,the farm-gate discount is applied multiplicatively
  
maize_price_eff   <- maize_price   * (1 - market_shock)
soybean_price_eff <- soybean_price * (1 - market_shock)
  
#### 4. Biophysical responses ####
  
## DYNAMIC BIOPHYSICAL & YIELD SIMULATION (Across 3 Toposequence Positions and crop type)
  
## Helper of crop, and soil specific parameter list that will be passed in many lines
  
crop_pars <- list(
    
Maize = list(
      ypot = maize_ypot, N_half = maize_N_half, N_rate = maize_N_rate,
      P_rate = maize_P_rate, K_rate = maize_K_rate,
      P_demand = maize_P_demand, K_demand = maize_K_demand,
      water_sens = maize_water_sens, T_opt = maize_T_opt, heat_sens = maize_heat_sens,
      pH_opt = maize_pH_opt, pH_sens = maize_pH_sens,
      residue_ratio = maize_residue_ratio, root_shoot = maize_root_shoot,
      grain_dm_fraction = maize_grain_dm_fraction,
      y_base = maize_baseline_yield,
      price = maize_price_eff, var_cash_cost = maize_var_cash_cost),
    
Soybean = list(
      ypot = soybean_ypot, N_half = soybean_N_half, N_rate = soybean_N_rate,
      P_rate = soybean_P_rate, K_rate = soybean_K_rate,
      P_demand = soybean_P_demand, K_demand = soybean_K_demand,
      water_sens = soybean_water_sens, T_opt = soybean_T_opt, heat_sens = soybean_heat_sens,
      pH_opt = soybean_pH_opt, pH_sens = soybean_pH_sens,
      residue_ratio = soybean_residue_ratio, root_shoot = soybean_root_shoot,
      grain_dm_fraction = soybean_grain_dm_fraction,
      y_base = soybean_baseline_yield,
      price = soybean_price_eff, var_cash_cost = soybean_var_cash_cost)
  )
  
soc_stock <- function(soc_fraction, bulk_density) {
    soc_fraction * bulk_density * (soil_depth_cm / 100) * 10000
  }
  
soil_pars <- list(
    
upper = list(soc_init = soc_stock(soc_fraction_upper, bulk_density_upper), clay = clay_upper, awc = awc_upper,
                 runoff = runoff_upper, waterlog_sens = waterlog_sens_upper,
                 pH = pH_upper, P_soil = P_soil_upper, K_soil = K_soil_upper),
    
mid   = list(soc_init = soc_stock(soc_fraction_mid, bulk_density_mid),   clay = clay_mid,   awc = awc_mid,
                 runoff = runoff_mid,   waterlog_sens = waterlog_sens_mid,
                 pH = pH_mid,   P_soil = P_soil_mid,   K_soil = K_soil_mid),
    
lower = list(soc_init = soc_stock(soc_fraction_lower, bulk_density_lower), clay = clay_lower, awc = awc_lower,
                 runoff = runoff_lower, waterlog_sens = waterlog_sens_lower,
                 pH = pH_lower, P_soil = P_soil_lower, K_soil = K_soil_lower)
  )
  
## 5. 25-YEAR SIMULATION FOR ONE (crop, position, OI rate) COMBINATION
  
simulate_system <- function(crop_name, topo, oi_rate) {
    
    cp <- crop_pars[[crop_name]] #crop helper
    sp <- soil_pars[[topo]] #soil helper
    
## Clay-dependent humification efficiency: the fraction of added carbon
## that survives into the stable pool (finer soils protect more C)
    
hum_eff_max <- pmin(hum_max, hum_base + hum_clay_sens * sp$clay) # humification
    
#Simulating different nutrients pools
soc_inert  <- sp$soc_init * soc_inert_fraction
soc_active <- sp$soc_init * soc_active_fraction
soc_stable <- sp$soc_init - soc_inert - soc_active
soc <- sp$soc_init
soil_N_active <- soc_active * 1000 / CN_active
soil_N_stable <- soc_stable * 1000 / CN_stable
organic_P_pool <- 0
organic_K_pool <- 0

soc_vec <- numeric(years)
y_vec   <- numeric(years)
    
    for (yr in 1:years) {  
      
## Existing SOC pools decompose first. Nitrogen mineralisation is derived from
## the same active and stable pools and their C:N ratios; inert SOC releases no N
      
temp_modifier <- Q10_decomp ^ ((annual_temp[yr] - ref_temp) / 10)
active_loss_fraction <- 1 - exp(-k_active * temp_modifier)
stable_loss_fraction <- 1 - exp(-k_stable * temp_modifier)
      
N_soil <- soil_N_active * active_loss_fraction +
     soil_N_stable * stable_loss_fraction

soc_active <- soc_active * (1 - active_loss_fraction)
soc_stable <- soc_stable * (1 - stable_loss_fraction)
soil_N_active <- soil_N_active * (1 - active_loss_fraction)
soil_N_stable <- soil_N_stable * (1 - stable_loss_fraction)
soc <- soc_inert + soc_active + soc_stable
      
## Residual organic P and K from previous years become gradually available
P_carryover <- organic_P_pool * organic_P_release_rate
K_carryover <- organic_K_pool * organic_K_release_rate
organic_P_pool <- organic_P_pool - P_carryover
organic_K_pool <- organic_K_pool - K_carryover

## Water
# extra SOC raises available water-holding capacity
      
awc_eff <- sp$awc + awc_per_tC * (soc - sp$soc_init)  
water_supply <- rain_index[yr] * (1 - sp$runoff) +
awc_water_sens * (awc_eff - sp$awc) / sp$awc
water_index  <- min(1, max(0, water_supply))
      
## water_sens to account for the crop sensitivity to a deficit
water_factor <- 1 - cp$water_sens * (1 - water_index)
      
## ISFM complementarity: organic matter can improve the recovery of the fixed crop specific
## suboptimal mineral-fertilizer package
#The response saturates with OI and all recovery fractions remain bounded by one
      
isfm_multiplier <- 1 + isfm_recovery_gain * oi_rate /
        (isfm_half_rate + oi_rate)
N_recovery_eff <- min(1, N_recovery * isfm_multiplier)
P_recovery_eff <- min(1, P_recovery * isfm_multiplier)
K_recovery_eff <- min(1, K_recovery * isfm_multiplier)
      
N_org <- oi_rate * oi_N_available
P_org <- oi_rate * oi_P_available + P_carryover
K_org <- oi_rate * oi_K_available + K_carryover
N_fert <- cp$N_rate * N_recovery_eff
P_fert <- cp$P_rate * P_recovery_eff
K_fert <- cp$K_rate * K_recovery_eff
      
P_tot <- sp$P_soil * P_soil_availability + P_org + P_fert
K_tot <- sp$K_soil * K_soil_availability + K_org + K_fert
      
P_index <- min(1, P_tot / (cp$ypot * cp$P_demand))
      
K_index <- min(1, K_tot / (cp$ypot * cp$K_demand))
      
## Soybean BNF requires adequate moisture and phosphorus
#Inoculation raises potential fixation but cannot remove those environmental constraints

N_bnf <- if (crop_name == "Soybean")
      bnf_N * (1 + inoculant_boost) * water_index * P_index else 0
      N_tot <- N_soil + N_org + N_fert + N_bnf
      N_index <- N_tot / (cp$N_half + N_tot)
      
## To account for the scarcest nutrients that sets the ceiling 
      # we use the von Liebig conversion factoor
      
nutrient_index <- min(N_index, P_index, K_index)    
      
## Heat, PH and extreme events 
      
heat_factor <- max(0.1, 1 - cp$heat_sens * max(0, annual_temp[yr] - cp$T_opt))
pH_factor   <- max(0.1, 1 - cp$pH_sens   * abs(sp$pH - cp$pH_opt))
      
drought_factor <- if (drought_year[yr] == 1) 1 - dryspell_damage else 1
      
flood_factor   <- if (flood_year[yr] == 1)
        1 - waterlog_penalty * sp$waterlog_sens else 1    
      
## Biotic stresses,Maize specific 
## Striga pressure falls as N supply rises, which is one of the main
 ## agronomic pay-offs of organic inputs in the Guinea Savannah
      
pest_factor <- 1
      if (crop_name == "Maize") {
  striga_eff  <- striga_damage * max(0, 1 - striga_N_relief * N_tot / cp$N_half)
  pest_factor <- (1 - striga_year[yr] * striga_eff) *
  (1 - faw_year[yr] * faw_damage)
      }
      
## Yield conversion with all the factors that affect it
      
y_vec[yr] <- max(0, cp$ypot * nutrient_index * water_factor *
                         drought_factor * flood_factor *
                         heat_factor * pH_factor * pest_factor)
      
## SOIL ORGANIC CARBON 
## Carbon entering the field from organic inputs plus the residues actually
## left on the field plus roots. 
# We add a residue_retained_fraction so we can avoid
## double-counting residue that was removed to be used as OI or fodder
      
C_in_oi   <- oi_rate * oi_carbon
      
grain_dm <- y_vec[yr] * cp$grain_dm_fraction
shoot_dm <- grain_dm * cp$residue_ratio
root_dm  <- shoot_dm * cp$root_shoot
      
C_in_crop <- (shoot_dm * residue_retained_fraction + root_dm) * C_conc_crop
      
## Saturation when there is enough
#stabilisation efficiency declines as the soil fills up
      
saturation <- max(0, 1 - soc / (sp$soc_init * soc_saturation_ratio))
      hum_eff    <- hum_eff_max * saturation
      
## Pool-consistent SOC accounting. C_residue, C_manure, C_compost and
## C_conc_crop are all entering TOTAL carbon
## Humification is applied exactly once to total new C
##Existing SOC is divided into active, stable and inert pools; 
# to avoid having the whole SOC stock at 0-20cm depth to decompose at the rate of the active pool 
      
C_input_total <- C_in_oi + C_in_crop
C_to_stable <- hum_eff * C_input_total
C_to_active <- C_input_total - C_to_stable
      
## The portion of fresh organic N not immediately crop-available remains in
## the soil organic pools
# Post-harvest crop N is inferred from retained/root C

N_pool_input <- max(0, oi_rate * (oi_N_total - oi_N_available)) +
        C_in_crop * 1000 / CN_crop
N_to_stable <- hum_eff * N_pool_input
N_to_active <- N_pool_input - N_to_stable
      
soc_active <- soc_active + C_to_active
soc_stable <- soc_stable + C_to_stable
soil_N_active <- soil_N_active + N_to_active
soil_N_stable <- soil_N_stable + N_to_stable
      
## Unavailable P and K for plant remain as residual pools
organic_P_pool <- organic_P_pool +
        max(0, oi_rate * (oi_P_total - oi_P_available))

organic_K_pool <- organic_K_pool +
        max(0, oi_rate * (oi_K_total - oi_K_available))
      soc <- soc_inert + soc_active + soc_stable
      soc_vec[yr] <- soc
    }
    
    
## 6. ECONOMICS 
    
## In northern Ghana the field labour force is the household
# Hired labour is engaged only when the task exceeds what the household can supply,
## typically at the weeding and harvest peaks

labour_days <- labour_field_days + oi_rate * labour_oi_per_t   # person-days ha-1
hired_days  <- pmax(0, labour_days - hh_labour_available)
cash_labour_cost <- hired_days * wage_rate * labour_cost_multiplier
    
### Economic evaluation
    
oi_cash_cost <- oi_rate * (cash_cart_per_t + oi_material_cash_cost_per_t) *
      oi_cost_multiplier
    
revenue <- y_vec * cp$price       # GHS ha-1 yr-1
    
cash_cost <- cp$var_cash_cost + nonlabour_cash_cost +
      cash_labour_cost + oi_cash_cost
    
annual_cash_profit <- revenue - cash_cost

discount_factor <- 1/ (1 + discount_rate) ^ seq_len(years)

pv_benefits_25 <- sum(revenue * discount_factor)
pv_costs_25 <- sum(cash_cost * discount_factor)
npv_25_ghs <- sum(annual_cash_profit * discount_factor)

# Discounted return on investement (DROI) 
#DROI of 0 is discounted break even 
#DROI of 0.10 means net discount benefits equal 10% of discounted costs, 
#DROI of 1.00 means net discount benefits equal discounted costs 

droi_25 <- if (pv_costs_25 > 0) npv_25_ghs / pv_costs_25 else NA_real_

list(
  soc_initial    = sp$soc_init,
  soc_final      = soc_vec[years],
  soc_change_pct = 100 * (soc_vec[years] / sp$soc_init - 1),
  yield_mean     = mean(y_vec),
  npv_25_ghs     = npv_25_ghs,
  droi_25        = droi_25
)
  }

#### Target evaluation for every OI rate ####

#The same 25 years trajectory is evaluated againts the three nested target set
# Base = primary decision analysis 
# Minimum, Stringent = robustness analysis 

#No objective is weighed or averaged, but joint success equals 1
#Only when SOC, yield and Discounted Return on Investement all reach 
#their scenario-specific thresholds in the same monte carlo run

for (crop_name in crops) {
  for (topo in topos) {
    for (oi_rate in oi_grid) {
      
  s <- simulate_system(crop_name, topo, oi_rate)
  tag <- paste(crop_name, topo, oi_code(oi_rate), sep= "_" )
  
  
# Writing continuous outcomes once so we can use them

  out[paste0(tag, "_SOCinitial")]   <- s$soc_initial
  out[paste0(tag, "_SOCfinal")]     <- s$soc_final
  out[paste0(tag, "_SOCchangePct")] <- s$soc_change_pct
  out[paste0(tag, "_YieldMean")]    <- s$yield_mean
  out[paste0(tag, "_NPV25GHS")]     <- s$npv_25_ghs
  out[paste0(tag, "_DROI25")]       <- s$droi_25
  
# Evaluating the same trajectory underbase, minimum and stringent targets 
  
  for (scenario_name in as.character(target_spec$scenario)) {
    
    tr <- target_spec[ target_spec$scenario == scenario_name, ]
    
    yield_target <- if (crop_name == "Maize") {
      tr$maize_yield_target
    } else { 
      tr$soybean_yield_target  
      }

    soc_ok    <- as.numeric(s$soc_final >= s$soc_initial * tr$soc_target_ratio)
    yield_ok  <- as.numeric(s$yield_mean >= yield_target)
    profit_ok <- as.numeric(is.finite(s$droi_25) && s$droi_25 >= tr$droi_target)
    joint_ok  <- as.numeric(soc_ok == 1 && yield_ok == 1 && profit_ok == 1)
    
    scenario_tag <- paste0(tag, "_", scenario_name)
    out[paste0(scenario_tag, "_SOCok")]    <- soc_ok
    out[paste0(scenario_tag, "_Yieldok")]  <- yield_ok
    out[paste0(scenario_tag, "_Profitok")] <- profit_ok
    out[paste0(scenario_tag, "_Jointok")]  <- joint_ok
  }
    }
  }
}
  
return(as.list(out))  
}

#### 5. PAIRED MONTE CARLO SWEEP ####

n_runs <- 200L  #just for testing i will later change to 10k

set.seed(2026)

mc_sweep <- decisionSupport::mcSimulation(
  estimate          = input_estimates,
  model_function    = oi_optimum_model,
  numberOfModelRuns = n_runs,
  functionSyntax    = "plainNames"
)


#### 6. RESHAPE MODEL OUTPUTS AND WRANGLING ##############################################

sweep_y     <- as.data.frame(mc_sweep$y)
sweep_y$run <- seq_len(nrow(sweep_y))

outcome_cols  <- names(sweep_y)[!stringr::str_detect(names(sweep_y), "_(Viability|Base|Stringent)_")]
scenario_cols <- names(sweep_y)[stringr::str_detect(names(sweep_y), "_(Viability|Base|Stringent)_")]
outcome_cols  <- setdiff(outcome_cols, "run")

outcomes_long <- sweep_y |>
  select(run, all_of(outcome_cols)) |>
  pivot_longer(-run, names_to = "output", values_to = "value") |>
  tidyr::extract(
    output,
    into   = c("crop", "soil", "oi_int", "metric"),
    regex  = "^(Maize|Soybean)_(upper|mid|lower)_OI(\\d{3,4})_(.+)$",
    remove = TRUE
  ) |>
  mutate(oi_rate = as.numeric(oi_int) / 100)

scenarios_long <- sweep_y |>
  select(run, all_of(scenario_cols)) |>
  pivot_longer(-run, names_to = "output", values_to = "value") |>
  tidyr::extract(
    output,
    into   = c("crop", "soil", "oi_int", "scenario", "indicator"),
    regex  = "^(Maize|Soybean)_(upper|mid|lower)_OI(\\d{3,4})_(Viability|Base|Stringent)_(SOCok|Yieldok|Profitok|Jointok)$",
    remove = TRUE
  ) |>
  mutate(oi_rate = as.numeric(oi_int) / 100)

#### 7. PROBABILITY COMPUTATIONS ####

obj_levels <- c("SOC target", "Yield target", "Profitability (DROI)", "Joint achievement")

probability_df <- scenarios_long |>
  group_by(scenario, crop, soil, oi_rate, indicator) |>
  summarise(probability = mean(value == 1, na.rm = TRUE), .groups = "drop") |>
  filter(is.finite(probability)) |>
  mutate(
    scenario  = factor(scenario, levels = c("Viability", "Base", "Stringent")),
    soil      = factor(soil, levels = c("upper", "mid", "lower"),
                       labels = c("Upper slope", "Mid slope", "Lower slope")),
    crop      = factor(crop, levels = c("Maize", "Soybean")),
    objective = recode(indicator,
                       SOCok    = "SOC target",
                       Yieldok  = "Yield target",
                       Profitok = "Profitability (DROI)",
                       Jointok  = "Joint achievement"),
    objective = factor(objective, levels = obj_levels)
  )


#### 7. ROBUST OI* SELECTION #####
# OI* = smallest OI whose joint probability is >= 95% of the maximum joint probability

select_oi_robust <- function(df) {
  max_p <- max(df$probability, na.rm = TRUE)
  if (!is.finite(max_p) || max_p == 0) {
    return(tibble::tibble(oi_star = 0.0, max_prob = 0.0))
  }
  candidates <- df |>
    filter(probability >= near_max_fraction * max_p - 1e-12) |>
    arrange(oi_rate)
  tibble::tibble(oi_star = candidates$oi_rate[1], max_prob = max_p)
}

oi_optima <- probability_df |>
  filter(objective == "Joint achievement") |>
  group_by(scenario, crop, soil) |>
  group_modify(~ select_oi_robust(.x)) |>
  ungroup() |>
  mutate(feasible = max_prob > 0)

#### 8. REALISED OUTCOMES AT OI* #############

outcomes_summary <- outcomes_long |>
  mutate(
    soil = factor(soil, levels = c("upper", "mid", "lower"),
                  labels = c("Upper slope", "Mid slope", "Lower slope")),
    crop = factor(crop, levels = c("Maize", "Soybean"))
  ) |>
  inner_join(oi_optima, by = c("crop", "soil"), relationship = "many-to-many") |>
  filter(abs(oi_rate - oi_star) < 1e-6) |>
  group_by(scenario, crop, soil, oi_star, max_prob, feasible, metric) |>
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    ci_lower   = quantile(value, 0.025, na.rm = TRUE),
    ci_upper   = quantile(value, 0.975, na.rm = TRUE),
    .groups    = "drop"
  )

#### 9. PRINT THEME & PALETTES ####

theme_print <- function(base_size = 15) {
  theme_bw(base_size = base_size, base_family = "sans") +
    theme(
      text               = element_text(colour = "black"),
      axis.title         = element_text(face = "bold", size = rel(1.0)),
      axis.text          = element_text(size = rel(0.85), colour = "black"),
      strip.text         = element_text(face = "bold", size = rel(0.95), colour = "black",
                                        margin = margin(4, 4, 4, 4)),
      strip.background   = element_rect(fill = "#F0F0F0", colour = NA),
      panel.border       = element_rect(colour = "grey35", fill = NA, linewidth = 0.5),
      panel.grid.major   = element_line(colour = "grey92", linewidth = 0.35),
      panel.grid.minor   = element_line(colour = "grey96", linewidth = 0.25),
      panel.spacing      = unit(0.9, "lines"),
      legend.position    = "bottom",
      legend.title       = element_text(face = "bold"),
      legend.text        = element_text(size = rel(0.95)),
      legend.key.width   = unit(2.4, "lines"),
      plot.title         = element_text(face = "bold", size = rel(1.15), hjust = 0),
      plot.caption       = element_text(size = rel(0.75), hjust = 0, colour = "grey25"),
      plot.margin        = margin(8, 12, 6, 8)
    )
}

# Palette for the three objectives
objectives_colors <- c(
  "SOC target"           = "#009E73",
  "Yield target"         = "#D55E00",
  "Profitability (DROI)" = "#0072B2",
  "Joint achievement"    = "black",
  "OI* (joint target met)" = "#C62828"
)
objectives_linetypes <- c(
  "SOC target"           = "solid",
  "Yield target"         = "solid",
  "Profitability (DROI)" = "solid",
  "Joint achievement"    = "dashed",
  "OI* (joint target met)" = "solid"
)
key_levels <- names(objectives_colors)

# Sequential label for target levels

scen_colors <- c("Viability" = "#74A9CF", "Base" = "#2B8CBE", "Stringent" = "#023858")

#### VISUALIZATION ####

# Figure 1 shows how OI* emerges from target probabilities, while
# Figure 2 shows what the selected OI rate delivers for each objectives

#### 10. FIGURE 1: OBJECTIVES AND JOINT ACHIEVEMENT ##

plot_joint_curves <- function(crop_name, panel_letter, show_xlab = TRUE) {
  
  d_curves <- probability_df |>
    filter(crop == crop_name) |>
    mutate(key = factor(as.character(objective), levels = key_levels))
  
  d_star <- oi_optima |>
    filter(crop == crop_name, feasible) |>
    mutate(
      key = factor("OI* (joint target met)", levels = key_levels)
    )
  
  ggplot() +
    geom_line(
      data = filter(d_curves, objective != "Joint achievement"),
      aes(x = oi_rate, y = probability, colour = key, linetype = key),
      linewidth = 0.9
    ) +
    geom_line(
      data = filter(d_curves, objective == "Joint achievement"),
      aes(x = oi_rate, y = probability, colour = key, linetype = key),
      linewidth = 1.5
    ) +
    geom_vline(
      data = d_star,
      aes(xintercept = oi_star, colour = key, linetype = key),
      linewidth = 0.7
    ) +
    facet_grid(soil ~ scenario,
               labeller = labeller(scenario = function(x) paste(x, "targets"))) +
    scale_colour_manual(values = objectives_colors, breaks = key_levels, name = NULL) +
    scale_linetype_manual(values = objectives_linetypes, breaks = key_levels, name = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = c(0, 0.5, 1), limits = c(0, 1.04), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, oi_sweep, by = 2), minor_breaks = seq(0, oi_sweep, by = 0.5),
                       limits = c(0, oi_sweep),
                       expand = expansion(mult = c(0.02, 0.02))) +
    labs(
      title = paste0(panel_letter, "  ", crop_name),
      x     = if (show_xlab) expression(bold("Organic input rate (t DM ha"^-1 * " yr"^-1 * ")")) else NULL,
      y     = "Probability of\nachieving target"
    ) +
    guides(colour   = guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)),
           linetype = guide_legend(nrow = 1)) +
    theme_print()
}

fig1 <- (plot_joint_curves("Maize",   "(a)", show_xlab = FALSE) /
           plot_joint_curves("Soybean", "(b)", show_xlab = TRUE)) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "figures", "Figure_1_Joint_Achievement.png"),
       fig1, width = 12, height = 15, dpi = 300, bg = "white")

ggsave(file.path(output_dir, "figures", "Figure_1_Joint_Achievement.pdf"),
       fig1, width = 12, height = 15, bg = "white")

#### 10. FIGURE 2: OUTCOMES AT OI* (SOC, YIELD, NPV) ####
#   SOC  is 25-year percentage change from initial SOC
#   Yield is 25-year mean crop yield (t ha-1)
#   Profitability is represented here by the 25-year NPV (GHS ha-1)

soil_levels <- c("Upper slope", "Mid slope", "Lower slope")

metric_labels <- c(
  SOCfinal     = "SOC stock after 25 yr (t C/ha)",
  YieldMean    = "Mean yield (t/ha)",
  NPV25GHS     = "NPV over 25 yr (GHS/ha)"
)

scen_offset <- c("Viability" = 0.25, "Base" = 0, "Stringent" = -0.25)

add_layout <- function(df) {
  df |>
    mutate(
      crop     = factor(crop, levels = c("Maize", "Soybean")),
      soil     = factor(soil, levels = soil_levels),
      scenario = factor(scenario, levels = names(scen_colors)),
      metric   = factor(metric, levels = names(metric_labels), labels = unname(metric_labels)),
      ypos     = (4 - as.numeric(soil)) + unname(scen_offset[as.character(scenario)])
    )
}

feas_levels <- c("Joint target attainable", "Joint target not attainable (OI* = 0)")

plot_df <- outcomes_summary |>
  filter(metric %in% names(metric_labels)) |>
  mutate(feasible_lab = factor(ifelse(feasible, feas_levels[1], feas_levels[2]), levels = feas_levels)) |>
  add_layout()

fig2 <- ggplot(plot_df, aes(y = ypos, colour = scenario)) +
  geom_segment(aes(x = ci_lower, xend = ci_upper, yend = ypos), linewidth = 1.3, lineend = "round") +
  geom_point(aes(x = mean_value, shape = feasible_lab), size = 3.8, stroke = 1.3, fill = "white") +
  facet_grid(crop ~ metric, scales = "free_x", switch = "y") +
  scale_colour_manual(values = scen_colors, name = "Target level") +
  scale_shape_manual(values = c(16, 1), name = NULL) +
  scale_y_continuous(breaks = 3:1, labels = soil_levels, limits = c(0.45, 3.55), expand = c(0, 0)) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.08))) +
  labs(x = NULL, y = NULL,
       caption = "Values are those obtained at the OI* of each scenario.") +
  guides(colour = guide_legend(order = 1, override.aes = list(shape = 16, linewidth = 1.3)),
         shape  = guide_legend(order = 2)) +
  theme_print() +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 90),
        panel.grid.major.y = element_blank())

ggsave(file.path(output_dir, "figures", "Figure_2_Outcomes_at_OI_star.png"),
       fig2, width = 14, height = 8, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "figures", "Figure_2_Outcomes_at_OI_star.pdf"),
       fig2, width = 14, height = 8, bg = "white")

#### 11. EXPORTING the TABLES #####

write.csv(probability_df,    file.path(output_dir, "tables", "probability_df.csv"),    row.names = FALSE)
write.csv(oi_optima,         file.path(output_dir, "tables", "oi_optima.csv"),         row.names = FALSE)
write.csv(outcomes_summary,  file.path(output_dir, "tables", "outcomes_summary.csv"),  row.names = FALSE)

cat("Analysis complete. Figures and tables saved in 'outputs/'.\n")





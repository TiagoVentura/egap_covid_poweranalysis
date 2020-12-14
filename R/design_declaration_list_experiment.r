# Power ANalysis, List Experiment
# Author: Tiago Ventura

library(DeclareDesign)
library(tidyverse)

# Model
list_experiment <- function(N, eff1, eff2){
  declare_population(
    N = N,
    U = rnorm(N),
    Y_star_violence = rbinom(N, 1, prob=eff1),
    Y_star_goods = rbinom(N, 1, prob=eff2), # Two treatments
    X = rbinom(N, size = 3, prob = 0.5)
  ) +
    declare_estimand(violence = mean(Y_star_violence),
                     goods= mean(Y_star_goods)) +
    declare_assignment(num_arms = 3, conditions = c("control", "z_v", "z_g"),
                       assignment_variable = "Z") +
    declare_potential_outcomes(Y_list ~ Y_star_violence*(Z=="z_v") + Y_star_goods*(Z=="z_g")+ X ,
                               conditions = c("control", "z_v", "z_g"),
                               assignment_variable = "Z") +
    declare_measurement(goods=ifelse(Z=="z_g", 1, 0),
                        violence=ifelse(Z=="z_v", 1, 0)) +
    declare_estimator(Y_list ~ violence + goods,
                      model = lm_robust, term=TRUE, estimand=c("intercept", "violence", "goods"))
}



#Diagnosis
grid <- expand_grid(N=seq(500, 2500, by =100),
                      eff1=seq(0.05, 0.25, by =0.05),
                      eff2=seq(0.15, 0.35, by =0.05))



# Diagnosands
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05)
)
# Expand 


designs <- expand_design(list_experiment, N=seq(500, 2500, by =100), 
              eff1=seq(0.05, 0.25, by =0.05), 
              eff2=seq(0.15, 0.35, by =0.05))


grid <- grid %>%
  rowwise() %>%
  mutate(diagnosands=list(diagnosands),
         design=list(list_experiment(N, eff1, eff2)),
         diagnosands_res=list(diagnose_design(design,
                                              diagnosands=diagnosands,
                                              sims=500, bootstrap_sims = 500))) 


results = grid %>%
  ungroup() %>%
  mutate(res = map(diagnosands_res,"diagnosands_df")) %>%
  unnest(res) %>%
  select(-diagnosands, -design, -diagnosands_res) %>%
  filter(!is.na(bias))

save(results, file="./data/results_simulations_power.Rdata")

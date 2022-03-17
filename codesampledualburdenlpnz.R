# Dual Burden of zoonoses
## Code sample
### Liz P. Noguera Z.

# Packages
library(sensitivity)

# Mozambique: Cysticercosis (Taenia solium) https://doi.org/10.1186/s12879-018-3030-z

set.seed(1000)
mozambique <- list(
  daly = c(1433,2762),           # lower and upper bound 95% UI
  num_infected = c(2098,3133),   # Number of pigs with cysticercosis (0.025, 0.975) quantile
  animal_value = c(30,79),       # Value of an adult pig (0.025, 0.975) quantile
  sold = 0.33,                   # Proportion of adult pigs sold per year
  reduced_value = 0.5,           # Value reduction of infected pork
  gni=460)                      # Gross national income




## fit a gamma distribution to a 95% confidence interval
fit_gamma <-
  function(lwr, upr) {
    f <-
      function(par, target) {
        ## calculate sum of squared deviations
        sum((qgamma(c(.025, .975), par[1], par[2]) - target) ^ 2)
      } 
    
    optim(c(1, 1), f, target = c(lwr, upr))
  }


n = 10000

gamma_paramater_daly <- fit_gamma(mozambique$daly[1], mozambique$daly[2])$par
gamma_parameter_num_infected <- fit_gamma(mozambique$num_infected[1], mozambique$num_infected[2])$par 
gamma_parameter_animal_value <- fit_gamma(mozambique$animal_value[1], mozambique$animal_value[2])$par 

mozambique_total_animal_loss <- unlist(lapply(rgamma(n, gamma_parameter_num_infected[1], gamma_parameter_num_infected[2]), 
                                              function(x)  sum(rgamma(x, gamma_parameter_animal_value[1], gamma_parameter_animal_value[2]))
                                              * mozambique$reduced_value * mozambique$sold))

quantile(mozambique_total_animal_loss , c(0.025,0.5,0.975))



mozambique_ale <- mozambique_total_animal_loss / mozambique$gni
quantile(mozambique_ale , c(0.025,0.5,0.975))



# zDALY -> 
mozambique_zdaly <- mozambique_ale + rgamma(n, gamma_paramater_daly[1], gamma_paramater_daly[2])
quantile(mozambique_zdaly, c(0.025,0.5,0.975))



# Sensitivity analysis...









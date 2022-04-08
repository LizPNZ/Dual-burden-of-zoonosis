

## Helper function

fit_gamma <-
  function(lwr, upr) {
    f <-
      function(par, target) {
        ## calculate sum of squared deviations
        sum((qgamma(c(.025, .975), par[1], par[2]) - target) ^ 2)
      } 
    
    optim(c(1, 1), f, target = c(lwr, upr))
  }

### Source of fit_gamma function: https://github.com/brechtdv/tsol-mozambique

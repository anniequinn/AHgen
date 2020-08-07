melt2 <- # Helper function to use pivot_longer in a way similar to reshape2::melt
  function(input, measure.vars, variable.name = "variable", value.name = "value") { 
    input %>% 
      pivot_longer(cols = all_of(measure.vars), names_to = variable.name, values_to = value.name) 
  }
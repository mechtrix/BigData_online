source(here("data","msa1_data.R"))

x_mean <- measured_data %>% 
  pull(measured_data) %>% 
  mean() %>% 
  round(.,digits = 4)

x_sd <- measured_data %>% 
  pull(measured_data) %>% 
  sd()%>% 
  round(.,digits = 4)

Cg <- round(((20/100)*Tol)/(6*x_sd),digits = 2)
  

Cgk <-  round(((0.5*20/100)*Tol-abs(x_mean-x_true))/(3*x_sd),digits = 2)

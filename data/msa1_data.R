measured_data <- c(20.3030,
                   20.2960,
                   20.3110,
                   20.2980,
                   20.3110,
                   20.3080,
                   20.3130,
                   20.3030,
                   20.3060,
                   20.3020,
                   20.3010,
                   20.3010,
                   20.2970,
                   20.2950,
                   20.3090,
                   20.3020,
                   20.3030,
                   20.3100,
                   20.2960,
                   20.3030,
                   20.3040,
                   20.3000,
                   20.2950,
                   20.3010,
                   20.3080,
                   20.2940,
                   20.3080,
                   20.3040,
                   20.3060,
                   20.3070,
                   20.3030,
                   20.3070,
                   20.3020,
                   20.3070,
                   20.3040,
                   20.3020,
                   20.2980,
                   20.3090,
                   20.2990,
                   20.3030,
                   20.3060,
                   20.3050,
                   20.3040,
                   20.3120,
                   20.2980,
                   20.3040,
                   20.3060,
                   20.3050,
                   20.3000,
                   20.3050)

measured_data <- data.frame(measured_data = measured_data) %>% 
  rowid_to_column(var = "No")

x_true <- 20.3020

Tol <- 0.3

measured_data %>% 
  shapiro_test(measured_data)

binwidth <- 0.001

p_hist <- measured_data %>% 
  ggplot(
    aes(
      x = measured_data
    )
  )+
  geom_histogram(
    binwidth = binwidth,
    color = "white"
  )+
  scale_x_continuous(
    expand = c(0,0,0,0),
    breaks = scales::pretty_breaks(n = 10)
  )+
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 5)
  )+
  labs(
    title = "Distribution of measured values",
    y = "count",
    x = "measured data"
  )+
  theme_minimal()

p_dens1 <- p_hist+geom_density(
    aes(
      y = after_stat(count*binwidth)
    ),
    linewidth = 1.5,
    color = "red"
  )

p_dens2 <- p_dens1+stat_theodensity(
    aes(
      y = after_stat(count*binwidth)
    ),
    geom = "area",
    fill = "gray",
    alpha = 0.6
  )







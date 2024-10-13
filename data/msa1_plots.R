source(here("data","msa1_data.R"))

plt_msa <- measured_data %>% 
  ggplot(
    aes(
      x = No,
      y = measured_data
    )
  )+
  geom_point()+
  geom_line()+
  geom_hline(
    aes(
      yintercept = mean(measured_data),
      linetype = "mean"
    )
  )+
  geom_hline(
    aes(
      yintercept = mean(measured_data)+0.1*Tol/2,
      linetype = "20% Tol"
    ),
    color = "green4",
    linewidth = 1.5
  )+
  geom_hline(
    aes(
      yintercept = mean(measured_data)-0.1*Tol/2,
      linetype = "20% Tol"
    ),
    color = "green4",
    linewidth = 1.5
  )+
  geom_hline(
    aes(
      yintercept = mean(measured_data)+sd(measured_data),
      linetype = "+/-sd"
    )
  )+
  geom_hline(
    aes(
      yintercept = mean(measured_data)-sd(measured_data),
      linetype = "+/-sd"
    )
  )+
  geom_hline(
    aes(
      yintercept = x_true,
      linetype = "x_true"
    ),
    linewidth = 1
  )+
  labs(
    title = "MSA 1 Data vs. index",
    x = "No",
    y = "measured data",
    linetype = ""
  )+
  scale_x_continuous(
    breaks = seq(0,100,5)
  )+
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10)
  )+
  theme_minimal(base_size = 15)+
  theme(
    legend.position = "bottom"
  )




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







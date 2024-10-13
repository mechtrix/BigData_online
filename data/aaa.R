aaa_raw <- data.frame(
  appraiser = c(1,1,1,2,2,2,1,1,1,2,2,2),
  runs = c(1,1,1,1,1,1,2,2,2,2,2,2),
  units = c(3,1,2,3,1,2,3,1,2,3,1,2)
)

reference <- c(
    "bad",
    "good",
    "bad",
    "bad",
    "good",
    "bad",
    "good",
    "bad",
    "bad",
    "good",
    "bad",
    "bad"
  )

results <- c(
  "bad",
  "good",
  "good",
  "good",
  "good",
  "good",
  "good",
  "bad",
  "bad",
  "bad",
  "bad",
  "good"
)


aaa_df <- 
  bind_cols(
    aaa_raw,
    reference = reference,
    results = results
  )

counts <- aaa_df %>% 
  mutate(ref_res = if_else(reference == results, 1, 0))

overall_agreement <- 
  counts %>% 
  summarise(
    sum_ref_res = sum(ref_res),
    count_n = n()
  ) %>% 
  mutate(
    overall_agreement = (sum_ref_res/count_n)*100 
  )%>% pull(overall_agreement) %>% round(.,digits = 1)

single_appraiser_agreement <- 
  counts %>% 
  group_by(
    appraiser
  ) %>% 
  summarise(
    sum_ref_res = sum(ref_res),
    count_n = n()
  ) %>% 
  mutate(
    overall_agreement = (sum_ref_res/count_n)*100 %>% round(.,digits = 1)
  ) 

reference_agreement <- 
  counts %>% 
  group_by(
    reference
  ) %>% 
  summarise(
    sum_ref_res = sum(ref_res),
    count_n = n()
  ) %>% 
  mutate(
    overall_agreement = (sum_ref_res/count_n)*100 %>% round(.,digits = 1)
  ) 

run_agreement <- 
  counts %>% 
  group_by(
    runs
  ) %>% 
  summarise(
    sum_ref_res = sum(ref_res),
    count_n = n()
  ) %>% 
  mutate(
    overall_agreement = (sum_ref_res/count_n)*100 %>% round(.,digits = 1)
  ) 


appraiser_ref_agreement <- 
  counts %>% 
  group_by(
    appraiser,
    reference
  ) %>% 
  summarise(
    sum_ref_res = sum(ref_res),
    count_n = n()
  ) %>% 
  mutate(
    overall_agreement = (sum_ref_res/count_n)*100 %>% round(.,digits = 1)
  ) 

kappa_appraiser <- aaa_df %>% 
  pivot_wider(
    names_from = appraiser,
    values_from = results
  ) %>% 
  janitor::clean_names() %>% 
  select(x1,x2) %>% 
  kappa2()


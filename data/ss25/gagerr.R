library(tidyverse)
library(SixSigma)
library(readxl)
library(qqplotr)


runs <- seq(1,10)
appraiser <- seq(1,3)
part <- seq(1,3)

df <- expand_grid(
  runs = runs,
  appraiser = appraiser,
  part = part
) %>% 
  rowid_to_column(
    var = "idx"
  )


df_shuffle = df[sample(1:nrow(df)), ] 

# write.csv(df_shuffle,file = "data/ss25/gagerr.csv")

### gage rr done


gagerr <- read_excel("data/ss25/data.xlsx") 
                     

gagerr <- gagerr |> 
  mutate(
    result = parse_number(result))

gagerr |> 
  ggplot(
    aes(x = as.factor(appraiser),
        y = result,
        # color = as.factor(appraiser),
        )
  )+
  geom_boxplot(
    outlier.shape = NA
  )+
  facet_wrap(~part)
  # geom_jitter(
  #   aes(
  #     shape = as.factor(appraiser)
  #   )
  # )+
  # geom_hline(yintercept = median(gagerr$result))+
  geom_jitter(width = 0.4)





gagerr |> 
  ggplot(
    aes(
        sample = result,
        # color = as.factor(appraiser),
        
    )
  )+
  stat_qq_band()+
  stat_qq_point()+
  stat_qq_line()+
  facet_wrap(
    part ~ appraiser,
    labeller = label_both,
    scales = "free"
  )
  
  geom_boxplot(
    outlier.shape = NA
  )+
  facet_wrap(~appraiser)+
  geom_hline(yintercept = median(gagerr$result))+
  geom_jitter(width = 0.4)

ss.rr(var = result, 
      part = part, 
      appr = appraiser, 
      data = gagerr, 
      method = "crossed"
      )
  
  
tmp <- gagerr %>% 
  filter(runs %in% c(1,2,2,3,4,5)) %>% 
  select(
    runs, appraiser, part, result
  )

ss.rr(var = result, part = part, appr = appraiser, data = tmp, method = "crossed")

gagerr %>% 
  ggplot(
    aes(
      x = result
    )
  )+
  geom_density()+
  facet_grid(
    appraiser~part,
    labeller = label_both
  )

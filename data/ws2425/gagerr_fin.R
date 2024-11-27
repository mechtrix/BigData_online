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

write.csv(df_shuffle,file = "gagerr.csv")

### gage rr done

library(tidyverse)
library(SixSigma)
library(readxl)

gagerr <- read_excel("data/ws2425/gagerr.xlsx") 
                     

gagerr <- gagerr |> 
  mutate(
    result = parse_number(result))

gagerr |> 
  ggplot(
    aes(x = as.factor(part),
        y = result,
        # color = as.factor(appraiser),
        
        )
  )+
  geom_boxplot(
    outlier.shape = NA
  )+
  facet_wrap(~appraiser)+
  geom_hline(yintercept = median(gagerr$result))+
  geom_jitter(width = 0.4)


library(qqplotr)


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
  facet_grid(
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

tmp <- gagerr %>% 
  filter(runs %in% c(1,2,2,3,4,5)) %>% 
  select(
    runs, appraiser, part, results
  )

ss.rr(var = result, part = part, appr = appraiser, data = gagerr, method = "crossed")

gagerr %>% 
  ggplot(
    aes(
      x = results
    )
  )+
  geom_density()+
  facet_grid(
    appraiser~part,
    labeller = label_both
  )

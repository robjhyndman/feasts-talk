library(tidyverse)
library(tsibble)
library(feasts)

# Convert M3 data to a tsibble
m3_to_tsibble <- function(z) {
  as_tsibble(ts(c(z$x, z$xx),
                start=start(z$x), frequency=frequency(z$x))) %>%
    mutate(
      sn = z$sn,
      st = z$st,
      type = z$type,
      description = z$description
    ) %>%
    as_tibble()
}

# Change 2498 to 2829 when bug is fixed
m3monthly <- map_dfr(Mcomp::M3[1402:2498], m3_to_tsibble) %>%
  as_tsibble(key=c(sn, type, description)) %>%
  mutate(index = yearmonth(index))

m3_features <- m3monthly %>%
  features(value, list(features_stl,features_acf,features_pacf))
pcs <- m3_features %>% select(-sn,-type,-description) %>%
  prcomp(scale=TRUE) %>% augment(m3_features)
pcs %>% ggplot(aes(x=.fittedPC1, y=.fittedPC2)) + geom_point()

outliers <- pcs %>% filter(.fittedPC2 == min(.fittedPC2))
m3monthly %>%
  right_join(outliers, by = c("sn","type","description")) %>%
  ggplot(aes(x = index, y = value)) + geom_line() +
  facet_grid(vars(sn,description)) +
  ggtitle("Outlying time series in PC space")

# In which dimensions is it strange?

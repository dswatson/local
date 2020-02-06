# Load libraries
library(ggplot2)

# Functions
hist_fn <- function(i, j) {
  df %>%
    filter(idx == i) %>%
    select(all_of(j)) %>% 
    rename(theta = j) %>%
    ggplot(aes(theta)) +
    geom_histogram(bins = 50, color = 'grey') + 
    theme_bw() + 
    ggtitle(paste0('Sample = ', i, ', Feature = ', j))
}

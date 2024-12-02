#Script - Cliente Leonardo

rm(list=ls())

library(openair)

windRose(mydata)

x <- circular(Angle)
rose.diag(x, pch = 16, cex = 1, axes = TRUE, shrink = 1, col=3, prop = 2, 
          bins=36, upper=TRUE, ticks=TRUE, units="degrees")





library(tidyverse)
library(HistData)
Nightingale %>% 
  select(Date, Month, Year, contains("rate")) %>% 
  pivot_longer(cols = 4:6, names_to = "Cause", values_to = "Rate") %>% 
  mutate(Cause = gsub(".rate", "", Cause),
         period = ifelse(Date <= as.Date("1855-03-01"), "Abril de 1854 a Março de 1855", "Abril de 1855 a Março de 1856"),
         Month = fct_relevel(Month, "Jul", "Ago", "Set", "Out", "Nov", "Dez", "Jan", "Fev", "Mar", "Abr", "Mai", "Jun")) %>% 
  ggplot(aes(Month, Rate)) + 
  geom_col(aes(fill = Cause), width = 1, position = "identity") + 
  coord_polar() + 
  facet_wrap(~period) +
  scale_fill_manual(values = c("skyblue3", "grey30", "firebrick")) +
  scale_y_sqrt() +
  theme_void() +
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        plot.background = element_rect(fill = alpha("cornsilk", 0.5)),
        plot.margin = unit(c(10, 10, 10, 10), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Gráfico Alice - Diagrama das Causas de Mortalidade no Exército do Leste")









library(tidyverse)
library(HistData)
Nightingale %>% 
  select(Date, Month, Year, contains("rate")) %>% 
  pivot_longer(cols = 4:6, names_to = "Cause", values_to = "Rate") %>% 
  mutate(Cause = gsub(".rate", "", Cause),
         period = ifelse(Date <= as.Date("1855-03-01"), "April 1854 to March 1855", "April de 1855 a March 1856"),
         Month = fct_relevel(Month, "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")) %>% 
  ggplot(aes(Month, Rate)) + 
  geom_col(aes(fill = Cause), width = 1, position = "identity") + 
  coord_polar() + 
  facet_wrap(~period) +
  scale_fill_manual(values = c("aquamarine", "coral", "darkorchid1")) +
  scale_y_sqrt() +
  theme_void() +
  theme(axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        plot.background = element_rect(fill = alpha("black", 0)),
        plot.margin = unit(c(10, 10, 10, 10), "pt"),
        plot.title = element_text(vjust = 5)) +
  ggtitle("Gráfico Alice - Dados originais")






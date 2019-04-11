# Carencia por rezago educativo


# Paquetes ----------------------------------------------------------------

library(tidyverse)

# I. Indicadores de Privación Social

# Indicador de carencia por REZAGO EDUCATIVO ------------------------------

poblacion_brut <- readRDS("data/poblacion.rds")

poblacion_brut  <- rename_all(poblacion_brut, tolower)

# Quitar de la población a huéspedes y trabajadores domésticos

poblacion <- poblacion_brut %>% 
  mutate(parentesco = as.numeric(parentesco)) %>% 
  filter(!((parentesco >= 400 & parentesco < 500) | 
             parentesco >= 700 & parentesco < 800))

rm(poblacion_brut)
gc()
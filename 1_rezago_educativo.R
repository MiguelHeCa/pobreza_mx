# Carencia por rezago educativo


# Paquetes ----------------------------------------------------------------

library(tidyverse)

# I. Indicadores de Privación Social

# Indicador de carencia por REZAGO EDUCATIVO ------------------------------

poblacion1 <- readRDS("data/poblacion.rds")

poblacion1 <- rename_all(poblacion1, tolower)

#### Convirtiendo dbf a rds

# Esta conversión responde a la necesidad de comprimir los archivos para 
# facilitar su uso y exporitación

# Paquetes ----------------------------------------------------------------


library(foreign)
library(tidyverse)

# Transformación ----------------------------------------------------------


nombres <- list.files(path = "R_2016/Bases de datos", pattern = "*.dbf") %>% 
  str_replace_all(pattern = "\\.dbf", replacement = "")

enigh <- nombres %>%
  map(~ read.dbf(file.path("R_2016/Bases de datos", paste0(.,".dbf")), as.is = TRUE))

enigh <- set_names(enigh, nombres)

nombres %>% 
  map(~ saveRDS(enigh[[.]], file.path("R_2016/Bases de datos", paste0(., ".rds"))))


# Comprobación ------------------------------------------------------------


rm(list = ls())
gc()

poblacion1 <- read.dbf("R_2016/Bases de datos/poblacion.dbf", as.is = TRUE)

poblacion2 <- readRDS("R_2016/Bases de datos/poblacion.rds")

setequal(poblacion1, poblacion2)

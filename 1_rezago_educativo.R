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

# Año de nacimiento -------------------------------------------------------

# Se resta 2016 de la edad porque es el año en que se levantó la encuesta
poblacion <- mutate(poblacion, anac_e = 2016 - edad)

# Inasistencia escolar ----------------------------------------------------

# Se calcula para personas con 3 años en adelante

poblacion <- poblacion %>% 
  mutate(inas_esc = case_when(
    asis_esc == "1" ~ 0,
    asis_esc == "2" ~ 1
  ))

# Nivel educativo ---------------------------------------------------------

poblacion <- poblacion %>% 
  mutate_at(c("nivelaprob", "gradoaprob", "antec_esc"), as.numeric)

poblacion <- poblacion %>% 
  mutate(niv_ed = case_when(
    
    # Con primaria incompleta o menos
    (nivelaprob < 2) | (nivelaprob == 2 & gradoaprob < 6)     ~ 0,
    
    # Con primaria completa o secundaria incompleta
    (nivelaprob == 2 & gradoaprob == 6) |
      (nivelaprob == 3 & gradoaprob < 3) |
      (nivelaprob == 5 | nivelaprob == 6) &
      gradoaprob < 3 &
      antec_esc == 1                                          ~ 1,
    
    # Secundaria completa o mayor nivel educativo
    ((nivelaprob == 3 & gradoaprob == 3) |
       (nivelaprob == 4) |
       (nivelaprob == 5 & antec_esc == 1 & gradoaprob >= 3) |  
       (nivelaprob == 6 & antec_esc == 1 & gradoaprob >= 3) |
       (nivelaprob == 5 & antec_esc >= 2) |
       (nivelaprob == 6 & antec_esc >= 2) |
       (nivelaprob >= 7))                                     ~ 2,
    
    # Todo lo demás
    TRUE                                                      ~ NA_real_
  ))

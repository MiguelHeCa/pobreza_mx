# Carencia por rezago educativo

# Paquetes ----------------------------------------------------------------

library(tidyverse)

# I. Indicadores de Privación Social ======================================


# I.1. Rezago educativo ---------------------------------------------------

poblacion_brut <- readRDS("data/poblacion.rds")

poblacion <- poblacion_brut %>% 
  
  # Nombres de variables en minúsculas
  rename_all(tolower) %>% 
  
  # Transformar variables de interés a numéricas
  mutate_at(c("nivelaprob",
              "gradoaprob",
              "antec_esc",
              "hablaind",
              "parentesco"),
            as.numeric) %>% 
  
  # Quitar de la población a huéspedes y trabajadores domésticos
  filter(!((parentesco >= 400 & parentesco < 500) | 
             parentesco >= 700 & parentesco < 800)) %>% 
  mutate(
    # Año de nacimiento
    # Se resta 2016 de la edad porque es el año en que se levantó la encuesta
    anac_e = 2016 - edad,
    
    # Inasistencia escolar
    # Se calcula para personas con 3 años en adelante
    inas_esc = case_when(
      asis_esc == "1" ~ 0,
      asis_esc == "2" ~ 1
    ),
    
    # Nivel educativo
    niv_ed = case_when(
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
         (nivelaprob >= 7)
      )                                                         ~ 2,
      
      # Todo lo demás
      TRUE                                                      ~ NA_real_
    ),
    
    # Indicador de carencia por rezago educativo
    # Se considera en situación de carencia por rezago educativo 
    # a la población que cumpla con alguno de los siguientes criterios:
    
    # 1. Se encuentra entre los 3 y los 15 años y no ha terminado la educación 
    # obligatoria (secundaria terminada) o no asiste a la escuela.
    # 2. Tiene una edad de 16 años o más, su año de nacimiento aproximado es 
    # 1981 o anterior, y no dispone de primaria completa.
    # 3. Tiene una edad de 16 años o más, su año de nacimiento aproximado es
    # 1982 en adelante, y no dispone de primaria secundaria completa.
    ic_rezedu = case_when(
      (edad >= 3 & edad <= 15) &  inas_esc == 1 &  niv_ed <= 1 ~ 1,
      edad >= 16 & anac_e >= 1982 & niv_ed <= 1                ~ 1,
      edad >= 16 & anac_e <= 1981 & niv_ed == 0                ~ 1,
      edad >= 0  & edad <= 2                                   ~ 0,
      (edad >= 3 & edad <= 15) & inas_esc == 0                 ~ 0,
      (edad >= 3 & edad <= 15) & inas_esc == 1 & niv_ed == 2   ~ 0,
      edad >= 16 & anac_e >= 1982 & niv_ed == 2                ~ 0,
      edad >= 16 & anac_e <= 1981 & niv_ed > 0                 ~ 0
    ),
    
    # Población indígena
    hli = case_when(
      edad >= 3 & hablaind == 1 ~ 1,
      edad >= 3 & hablaind == 2 ~ 0,
      TRUE ~ NA_real_ 
    )
  ) %>% 
  
  # Depurando variables
  select(folioviv, foliohog, numren, edad, anac_e,
         inas_esc, antec_esc, niv_ed, ic_rezedu, hli) %>% 
  arrange(folioviv, foliohog, numren)

# Exportando
saveRDS(poblacion, "data/ic_rezedu16.rds")

rm(list = ls())
gc()



# I.2. Acceso a servicios de salud ----------------------------------------

trabajos <- readRDS("data/trabajos.rds")

# Tipo de trabajor: identifica la población subordinada e independiente

trabajos <- trabajos %>% 
  mutate(tipo_trab = case_when(
    
    # Subordinados
    subor == 1 ~ 1,
    
    # Independientes que reciben un pago
    subor == 2 & indep == 1 & tiene_suel == 1 ~ 2,
    subor == 2 & indep == 2 & pago == 1 ~ 2,
    
    # Independientes que no reciben un pago
    subor == 2 & indep == 1 & tiene_suel == 2 ~ 3,
    subor == 2 & indep == 2 & (pago == 2 | pago == 3) ~ 3,
    
    # Todo lo demás
    TRUE ~ NA_real_
  ))

trabajos <- trabajos %>% 
  
  # Ocupación principal o secundaria
  mutate(ocupa = if_else(condition = id_trabajo == 1, true = 1, false = 0))














# I.3. Acceso a la seguridad social ---------------------------------------


# I.4. Calidad y espacios en la vivienda ----------------------------------


# I.5. Acceso en los servicios básicos en la vivienda ---------------------


# I.6. Acceso a la alimentación -------------------------------------------


# I.7. Bienestar (ingresos) -----------------------------------------------


# II. Pobreza =============================================================



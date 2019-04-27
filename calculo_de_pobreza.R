# PROGRAMA PARA LA MEDICIÓN DE LA POBREZA

# Todas las bases de datos del Modelo Estadístico 2016 para la continuidad del 
# MCS-ENIGH pueden ser obtenidas en la página de Internet del INEGI,
# www.inegi.org.mx. Originalmente todas fueron descargadas en formato DBF, 
# pero fueron convertidas a RDS en el script `conversion_dbf.R`. Los archivos
# originales se encuentran en `raw/` y los generados en `data/`

# Se utilizan las siguientes bases:
# Hogares:      hogares.rds
# Población:    poblacion.rds
# Ingresos:     ingresos.rds
# Concentrado:  concentradohogar.rds
# Trabajos:     trabajos.rds
# Viviendas:    viviendas.rds
# No monetario: gastospersona.rds y gastoshogar.rds


# Paquetes ----------------------------------------------------------------

library(tidyverse)

# I. Indicadores de Privación Social ======================================

# I.1 Rezago educativo ----------------------------------------------------

poblacion <- readRDS("raw/poblacion.rds")

poblacion <- poblacion %>% 
  
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
      asis_esc == 1 ~ 0,
      asis_esc == 2 ~ 1
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
    
    #** Indicador de carencia por rezago educativo
    
    # Se considera en situación de carencia por rezago educativo 
    # a la población que cumpla con alguno de los siguientes criterios:
    
    # 1. Se encuentra entre los 3 y los 15 años y no ha terminado la educación 
    # obligatoria (secundaria terminada) o no asiste a la escuela.
    
    # 2. Tiene una edad de 16 años o más, su año de nacimiento aproximado es 
    # 1981 o anterior, y no dispone de primaria completa.
    
    # 3. Tiene una edad de 16 años o más, su año de nacimiento aproximado es
    # 1982 en adelante, y no dispone de primaria secundaria completa.
    ic_rezedu = case_when(
      edad >= 3 & edad <= 15 &  inas_esc == 1 &  niv_ed <= 1 |
        edad >= 16 & anac_e >= 1982 & niv_ed <= 1 |
        edad >= 16 & anac_e <= 1981 & niv_ed == 0              ~ 1,
      edad >= 0  & edad <= 2 |
        edad >= 3 & edad <= 15 & inas_esc == 0 |
        edad >= 3 & edad <= 15 & inas_esc == 1 & niv_ed == 2 |
        edad >= 16 & anac_e >= 1982 & niv_ed == 2 |
        edad >= 16 & anac_e <= 1981 & niv_ed > 0               ~ 0
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

rm(list = ls()); gc()

# I.2 Acceso a servicios de salud -----------------------------------------

poblacion <- readRDS("raw/poblacion.rds")
trabajos <- readRDS("raw/trabajos.rds")

# Tipo de trabajor: identifica la población subordinada e independiente
ocupados <- trabajos %>% 
  mutate(tipo_trab = case_when(
    
    # Subordinados
    subor == 1                                                             ~ 1,
    
    # Independientes que reciben un pago
    subor == 2 & (indep == 1 & tiene_suel == 1) | (indep == 2 & pago == 1) ~ 2,
    
    # Independientes que no reciben un pago
    subor == 2 & (indep == 1 & tiene_suel == 2) | (indep == 2 & pago > 1)  ~ 3,
    
    # Todo lo demás
    TRUE ~ NA_real_
  )) %>% 
  
  # Ocupación principal o secundaria
  mutate(ocupa = if_else(condition = id_trabajo == 1, true = 1, false = 0)) %>% 
  
  # Distinción de prestaciones en trabajo principal y secundario
  select(folioviv, foliohog, numren, id_trabajo, tipo_trab, ocupa) %>% 
  
  # Separar tipos de trabajo
  gather(variable, valor, -(folioviv:id_trabajo)) %>% 
  unite(temporal, variable, id_trabajo, sep = "") %>% 
  spread(temporal, valor) %>% 
  
  # Población ocupada
  mutate(
    ocupa2 = if_else(condition = is.na(ocupa2), true = 0, false = 1),
    
    # Identificar de población trabajadora (que reporta al menos un empleo)
    trab = 1
    ) %>% 
  
  # Acomodar variables
  select(folioviv:numren, tipo_trab1, ocupa1, tipo_trab2, ocupa2, trab) %>% 
  arrange(folioviv, foliohog, numren)

#** Unión de `ocupados` con `población` 
asalud <- poblacion %>% 
  
  # Nombres de variables en minúsculas
  rename_all(tolower) %>% 
  
  # Transformar variables de interés a numéricas
  mutate_at(c("parentesco", "act_pnea1", "act_pnea2"), as.numeric) %>% 
  
  # Quitar de la población a huéspedes y trabajadores domésticos
  filter(!((parentesco >= 400 & parentesco < 500) | 
             parentesco >= 700 & parentesco < 800)) %>% 
  
  arrange(folioviv, foliohog, numren) %>% 
  
  # Agregar variables de ocupados a población
  left_join(ocupados, by = c("folioviv", "foliohog", "numren")) %>% 
  
  #** Creación de variables para el indicador de carencia 

  # Población económicamente activa
  mutate(
  pea = case_when(
    trab == 1 & edad >= 16 & !is.na(edad)                          ~ 1,
    (act_pnea1 == 1 | act_pnea2 == 1) &  edad >= 16 & !is.na(edad) ~ 2,
    (act_pnea1 > 1 | act_pnea2 > 1) & edad >= 16 & !is.na(edad)    ~ 0,
    TRUE                                                           ~ NA_real_
    ),
  
  #** Tipo de trabajo
  
  # Ocupación principal
  tipo_trab1 = case_when(
    pea == 0 | pea == 2 | is.na(pea) ~ NA_real_,
    TRUE ~ tipo_trab1
    ),
  
  # Ocupación secundaria
  tipo_trab2 = case_when(
    pea == 0 | pea == 2 | is.na(pea) ~ NA_real_,
    TRUE ~ tipo_trab2
    ),
  
  #** Prestaciones básicas
  
  #** Prestaciones laborales (servicios médicos)
  
  # Ocupación principal
  smlab1 = case_when(
    ocupa1 == 1 & atemed == 1 &
      !(is.na(inst_1) & is.na(inst_2) & is.na(inst_3) & is.na(inst_4)) &
      !is.na(inscr_1) ~ 1,
    ocupa1 == 1       ~ 0,
    TRUE              ~ NA_real_
    ),
  
  # Ocupación secundaria
  smlab2 = case_when(
    ocupa2 == 1 & atemed == 1 &
      !(is.na(inst_1) & is.na(inst_2) & is.na(inst_3) & is.na(inst_4)) &
      !is.na(inscr_1) ~ 1,
    ocupa2 == 1       ~ 0,
    TRUE ~ NA_real_
    ),
  
  # Contratación voluntaria: servicios médicos
  smcv = case_when(
    edad >= 12 & edad <= 97 & atemed == 1 &
      !(is.na(inst_1) & is.na(inst_2) & is.na(inst_3) & is.na(inst_4)) &
      !is.na(inscr_6)         ~ 1,
    edad >= 12 & edad <= 97 ~ 0,
    TRUE                      ~ NA_real_
    ),
  
  # Acceso directo a servicios de salud
  sa_dir = case_when(
    # Ocupación principal
    tipo_trab1 == 1 & smlab1 == 1 |
      tipo_trab1 > 1 & (smlab1 == 1 | smcv == 1) |
      # Ocupación secundaria
      tipo_trab2 == 1 & smlab2 == 1 |
      tipo_trab2 > 1 & (smlab2 == 1 | smcv == 1) ~ 1,
    TRUE ~ NA_real_
    ),
  
  # Núcleos familiares
  par = case_when(
    parentesco >= 100 & parentesco < 200 ~ 1,
    parentesco >= 200 & parentesco < 300 ~ 2,
    parentesco >= 300 & parentesco < 400 ~ 3,
    parentesco == 601                    ~ 4,
    parentesco == 615                    ~ 5,
    TRUE                                 ~ 6
    ),
  
  # Información relativa a la asistencia escolar
  inas_esc = case_when(
    asis_esc == 1 ~ 0,
    asis_esc == 2 ~ 1
    ),
  
  #** Identificar los principales parentescos respecto a la jefatura del hogar
  
  # # Jefatura del hogar
  jef = case_when(
    par == 1 & sa_dir == 1                                &
      (!is.na(inst_2) | !is.na(inst_3)) & !is.na(inscr_6) &
      is.na(inst_1)  & is.na(inst_4)  & is.na(inst_6)     &
      is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3)    &
      is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7)    ~ NA_real_,
    par == 1 & sa_dir == 1                                ~ 1,
    TRUE                                                  ~ 0
    ),

  # Cónyuge
  cony = case_when(
    par == 2 & sa_dir == 1                                &
      (!is.na(inst_2) | !is.na(inst_3)) & !is.na(inscr_6) &
      is.na(inst_1)  & is.na(inst_4)  & is.na(inst_6)     &
      is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3)    &
      is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7)    ~ NA_real_,
    par == 2 & sa_dir == 1                                ~ 1,
    TRUE                                                  ~ 0
    ),

  # Descendientes
  hijo = case_when(
    par == 3 & sa_dir == 1                                &
      (!is.na(inst_2) | !is.na(inst_3)) & !is.na(inscr_6) &
      is.na(inst_1)  & is.na(inst_4)  & is.na(inst_6)     &
      is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3)    &
      is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7)    ~ NA_real_,
    par == 3 & sa_dir == 1                                ~ 1,
    TRUE                                                  ~ 0
    )
  )

#** Crear suma de las indicadoras de parentesco
suma_poblacion <- asalud %>% 
  group_by(folioviv, foliohog) %>% 
  summarise(jef_1 = sum(jef),
            cony_1 = sum(cony),
            hijo_1 = sum(hijo)) %>% 
  ungroup()

#** Unir la suma a los datos de población
asalud <- asalud %>% 
  left_join(suma_poblacion, by = c("folioviv", "foliohog")) %>% 
  
  # Acceso directo a los servicios de salud de ...
  mutate(
    
    # Jefatura del hogar
    jef_sa = jef_1,
    
    # Conyuge
    cony_sa = case_when(
      cony_1 > 0 ~ 1,
      TRUE       ~ 0
    ),
    
    # Descendientes
    hijo_sa = case_when(
      hijo_1 > 0 ~ 1,
      TRUE       ~ 0
    ),
    
    # Otros núcleos familiares. Se identifica a través de la afiliación o
    # inscripción a servicios de salud por algún familiar dentro o fuera del
    # hogar, muerte del asegurado o por contratación propia
    s_salud = case_when(
      atemed == 1 &
        (
          !is.na(inst_1) | !is.na(inst_2) | !is.na(inst_3) | !is.na(inst_4)
          ) &
        (
          !is.na(inscr_3) | !is.na(inscr_4) | !is.na(inscr_6) | !is.na(inscr_7)
          )                            ~ 1,
      !(is.na(segpop) & is.na(atemed)) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Indicador de carencia por acceso a los servicios de salud
    
    ic_asalud = case_when(
      
      # Acceso directo
      sa_dir |
        
        # Parentesco directo: jefatura
        par == 1 & cony_sa == 1 |
        par == 1 & pea == 0 & hijo_sa == 1 |
        
        # Parentesco directo: cónyuge
        par == 2 & jef_sa == 1 |
        par == 2 & pea == 0 & hijo_sa == 1 |
        
        # Parentesco directo: descendientes
        par == 3 & edad < 16 & jef_sa == 1 |
        par == 3 & edad < 16 & cony_sa == 1 |
        par == 3 & (edad >= 16 & edad <= 25) & inas_esc == 0 & jef_sa == 1 |
        par == 3 & (edad >= 16 & edad <= 25) & inas_esc == 0 & cony_sa == 1 |
        
        # Parentesco directo: ascendentes
        par == 4 & pea == 0 & jef_sa == 1 |
        par == 5 & pea == 0 & cony_sa == 1 |
        
        # Otros núcleos familiares
        s_salud == 1 |
        
        # Acceso reportado
        segpop == 1 |
        segpop == 2 & atemed == 1 &
        !(is.na(inst_1) & is.na(inst_2) & is.na(inst_3) & 
            is.na(inst_4) & is.na(inst_5) & is.na(inst_6)) |
        segvol_2 == "2" ~ 0,
      
      # Se considera en esta situación a la población que:
      # 1. No se encuentra afiliada o inscrita al Seguro Popular o alguna
      # alguna institución que proporcione servicios médicos, ya sea por 
      # prestación laboral, contratación voluntaria o afiliación de un 
      # familiar por parentesco directo.
      TRUE              ~ 1
    ),
    
    # Población con al menos alguna discapacidad, sea física o mental
    discap = case_when(
      disc1 >= 1 & disc1 <= 7 |
        disc2 >= 2 & disc2 <= 7 |
        disc3 >= 3 & disc3 <= 7 |
        disc4 >= 4 & disc4 <= 7 |
        disc5 >= 5 & disc5 <= 7 |
        disc6 >= 6 & disc6 <= 7 |
        disc7 == 7 ~ 1,
      disc1 == 8 | disc1 == "&" | is.na(disc1) ~ 0,
      TRUE ~ 0
    )
  ) %>% 
  
  # Depurar variables
  select(folioviv, foliohog, numren, sexo, discap, ic_asalud) %>% 
  arrange(folioviv, foliohog, numren)

# Exportar
saveRDS(ocupados, "data/ocupados16.rds")
saveRDS(asalud, "data/ic_asalud16.rds")

rm(list = ls()); gc()

# I.3 Acceso a la seguridad social ----------------------------------------

trabajos <- readRDS("raw/trabajos.rds")

prestaciones <- trabajos %>% 
  mutate(
    
    # Tipo de trabajador
    tipo_trab = case_when(
      
      # Subordinados
      subor == 1                                          ~ 1,
      
      # Independientes que reciben un pago
      (subor == 2 & indep == 1 & tiene_suel == 1) |
      (subor == 2 & indep == 2 & pago == 1)               ~ 2,
      
      # Independientes que no reciben un pago
      (subor == 2 & indep == 1 & tiene_suel == 2) |
      (subor == 2 & indep == 2 & (pago == 2 | pago == 3)) ~ 3
      ),
    
    # Prestaciones laborales: incapacidad en caso de enfermedad o maternidad
    # con goce de sueldo y SAR o Afore
    inclab = if_else(is.na(pres_7), true = 0, false = 1),
    aforlab = if_else(is.na(pres_14), true = 0, false = 1),
    
    # Ocupación principal o secundaria
    ocupa = case_when(
      id_trabajo == 1 ~ 1,
      id_trabajo == 2 ~ 0,
      TRUE ~ NA_real_
      )
  ) %>% 

  # Seleccionar variables relevantes
  select(folioviv:numren,
         id_trabajo,
         tipo_trab,
         inclab,
         aforlab,
         ocupa) %>% 
  # Separar tipos de trabajo
  gather(variable, valor, -(folioviv:id_trabajo)) %>% 
  unite(temporal, variable, id_trabajo, sep = "") %>% 
  spread(temporal, valor) %>% 
  
  # Recodificar ocupa2
  mutate(ocupa2 = if_else(is.na(ocupa2), true = 0, false = 1),
         trab = 1) %>% 
  
  # Eliminar variables "inclab1" y "aforlab1" y ordenar el resto
  select(
    folioviv:numren,
    tipo_trab1,
    ocupa1,
    tipo_trab2,
    inclab2,
    aforlab2,
    ocupa2,
    trab
  ) %>% 
  arrange(folioviv, foliohog, numren)

# Ingresos por jubilaciones o pensiones

ingresos <- readRDS("raw/ingresos.rds")

pensiones <- ingresos %>% 
  rename_all(tolower) %>% 
  filter(clave == "P032" |
           clave == "P033" |
           clave == "P044" |
           clave == "P045") %>% 
  mutate(
    
    # Jubilaciones y/o pensiones originadas dentro o fuera del país
    ing_pens = case_when(
      clave == "P032" | clave == "P033" ~ rowMeans(select(., ing_1:ing_6)),
      TRUE ~ 0
    ),
    
    # Beneficio del programa 65 o más u otros programas para adultos mayores
    ing_pam = case_when(
      clave == "P044" | clave == "P045" ~ rowMeans(select(., ing_1:ing_6)),
      TRUE ~ 0
    )
  ) %>% 
  select(folioviv:numren, ing_pens:ing_pam) %>% 
  group_by(folioviv, foliohog, numren) %>% 
  summarise(ing_pens = sum(ing_pens),
            ing_pam = sum(ing_pam)) %>% 
  ungroup() %>% 
  arrange(folioviv, foliohog, numren)

# Construcción del indicador

poblacion <- readRDS("raw/poblacion.rds")

segsoc <- poblacion %>% 
  
  # Nombres de variables en minúsculas
  rename_all(tolower) %>% 
  
  # Transformar variables de interés a numéricas
  mutate_at("parentesco", as.numeric) %>% 
  
  # Quitar de la población a huéspedes y trabajadores domésticos
  filter(!((parentesco >= 400 & parentesco < 500) | 
             parentesco >= 700 & parentesco < 800)) %>% 
  left_join(prestaciones, by = c("folioviv", "foliohog", "numren")) %>% 
  left_join(pensiones, by = c("folioviv", "foliohog", "numren"))

# Exportar prestaciones y pensiones, limpiar espacio de trabajo

saveRDS(prestaciones, "data/prestaciones16.rds")
saveRDS(pensiones, "data/pensiones16.rds")

rm(list = setdiff(ls(), "segsoc")); gc()
###
poblacion <- segsoc
###

  
poblacion2 <- segsoc %>% 
  mutate(
    
    # Población económicamente activa (PEA). Personas de 16 años o más.
    pea = case_when(
      trab == 1 & edad >= 16 & !is.na(edad)                          ~ 1,
      (act_pnea1 == 1 | act_pnea2 == 1) &  edad >= 16 & !is.na(edad) ~ 2,
      (act_pnea1 > 1 | act_pnea2 > 1) & edad >= 16 & !is.na(edad)    ~ 0,
      TRUE                                                           ~ NA_real_
    ),
    
    #** Tipo de trabajo
    
    # Ocupación principal
    tipo_trab1 = case_when(
      pea == 0 | pea == 2 | is.na(pea) ~ NA_real_,
      TRUE ~ tipo_trab1
    ),
    
    # Ocupación secundaria
    tipo_trab2 = case_when(
      pea == 0 | pea == 2 | is.na(pea) ~ NA_real_,
      TRUE ~ tipo_trab2
    ),
    
    # Jubilados o pensionados
    jub = case_when(
      trabajo_mp == 2 & (act_pnea1 == 2 | act_pnea2 == 2) |
        ing_pens > 0 |
        inscr_2 == 2 ~ 1,
      TRUE ~ 0
    ),
    
    #** Prestaciones básicas
    
    #** Prestaciones laborales (servicios médicos)
    
    # Ocupación principal
    smlab1 = case_when(
      ocupa1 == 1 & atemed == 1 &
        !(is.na(inst_1) & is.na(inst_2) & is.na(inst_3) & is.na(inst_4)) &
        !is.na(inscr_1) ~ 1,
      ocupa1 == 1       ~ 0,
      TRUE              ~ NA_real_
    ),
    
    # Ocupación secundaria
    smlab2 = case_when(
      ocupa2 == 1 & atemed == 1 &
        !(is.na(inst_1) & is.na(inst_2) & is.na(inst_3) & is.na(inst_4)) &
        !is.na(inscr_1) ~ 1,
      ocupa2 == 1       ~ 0,
      TRUE ~ NA_real_
    ),
    
    #** Contratación voluntaria
    
    # Servicios médicos
    smcv = case_when(
      edad >= 12 & edad <= 97 & atemed == 1 &
        !(is.na(inst_1) & is.na(inst_2) & is.na(inst_3) & is.na(inst_4)) &
        !is.na(inscr_6)         ~ 1,
      edad >= 12 & edad <= 97 ~ 0,
      TRUE                      ~ NA_real_
    ),
    
    # SAR o Afore
    aforecv = case_when(
      segvol_1 == 1 & edad >= 12 ~ 1,
      is.na(segvol_1) & edad >= 12 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% 
  rename(aforlab1 = aforlab1_f,
         inclab1 = inclab1_fi)

table(poblacion$pea, exclude = NULL)
table(poblacion$tipo_trab1, exclude = NULL)
table(poblacion$tipo_trab2, exclude = NULL)
table(poblacion$jub, exclude = NULL)
table(poblacion$smlab1, exclude = NULL)
table(poblacion$smlab2, exclude = NULL)
table(poblacion$smcv, exclude = NULL)
table(poblacion$aforecv, exclude = NULL)

table(poblacion2$pea, exclude = NULL)
table(poblacion2$tipo_trab1, exclude = NULL)
table(poblacion2$tipo_trab2, exclude = NULL)
table(poblacion2$jub, exclude = NULL)
table(poblacion2$smlab1, exclude = NULL)
table(poblacion2$smlab2, exclude = NULL)
table(poblacion2$smcv, exclude = NULL)
table(poblacion2$aforecv, exclude = NULL)

poblacion2 <- poblacion2 %>% 
  mutate(
    
    #** Acceso a la directo a la seguridad social
    
    ss_dir = case_when(
      
      # Ocupación principal
      tipo_trab1 == 1 & 
        (smlab1 == 1 & inclab1 == 1 & aforlab1 == 1) |
        tipo_trab1 == 2 &
        (smlab1 == 1 | smcv == 1) &
        (aforlab1 == 1 | aforecv == 1) |
        tipo_trab1 == 3 &
        (smlab1 == 1 | smcv == 1) &
        aforecv == 1 |
        
      # Ocupación secundaria
        tipo_trab2 == 1 &
        (smlab2 == 1 & inclab2 == 1 & aforlab2 == 1) |
        tipo_trab2 == 2 &
        (smlab2 == 1 | smcv == 1) &
        (aforlab2 == 1 | aforecv == 1) |
        tipo_trab2 == 3 &
        (smlab2 == 1 | smcv == 1) &
        aforecv == 1 |
        
      # Jubilados y pensionados
        jub == 1 ~ 1,
      TRUE ~ 0
    ),
    
    # Núcleos familiares
    par = case_when(
      parentesco >= 100 & parentesco < 200 ~ 1,
      parentesco >= 200 & parentesco < 300 ~ 2,
      parentesco >= 300 & parentesco < 400 ~ 3,
      parentesco == 601                    ~ 4,
      parentesco == 615                    ~ 5,
      TRUE                                 ~ 6
    ),
    
    # Información relativa a la asistencia escolar
    inas_esc = case_when(
      asis_esc == 1 ~ 0,
      asis_esc == 2 ~ 1
    )
  )

table(poblacion$ss_dir, exclude = NULL)
table(poblacion$par, exclude = NULL)
table(poblacion$inas_esc, exclude = NULL)

table(poblacion2$ss_dir, exclude = NULL)
table(poblacion2$par, exclude = NULL)
table(poblacion2$inas_esc, exclude = NULL)

poblacion2 <- poblacion2 %>% 
  mutate(
    #** Identificar los principales parentescos respecto a la jefatura del hogar
    
    # Jefatura del hogar
    jef = case_when(
      par == 1 & ss_dir == 1                                &
        (!is.na(inst_2) | !is.na(inst_3)) & !is.na(inscr_6) &
        (is.na(inst_1)  & is.na(inst_4)  & is.na(inst_6))   &
        (is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3)   &
        is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7))   ~ NA_real_,
      par == 1 & ss_dir == 1                                ~ 1,
      TRUE                                                  ~ 0
    ),
    
    # Cónyuge
    cony = case_when(
      par == 2 & ss_dir == 1                                &
        (!is.na(inst_2) | !is.na(inst_3)) & !is.na(inscr_6) &
        is.na(inst_1)  & is.na(inst_4)  & is.na(inst_6)     &
        is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3)    &
        is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7)    ~ NA_real_,
      par == 2 & ss_dir == 1                                ~ 1,
      TRUE                                                  ~ 0
    ),
    
    # Descendientes
    hijo = case_when(
      par == 3 & ss_dir == 1                                &
        (!is.na(inst_2) | !is.na(inst_3)) & !is.na(inscr_6) &
        is.na(inst_1)  & is.na(inst_4)  & is.na(inst_6)     &
        is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3)    &
        is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7)    ~ NA_real_,
      par == 3 & ss_dir == 1 & jub == 0                     |
        par == 3 & ss_dir == 1 & jub == 1 & edad > 25       ~ 1,
      TRUE                                                  ~ 0
    )
  )

table(poblacion$jef, exclude = NULL)
table(poblacion$cony, exclude = NULL)
table(poblacion$hijo, exclude = NULL)

table(poblacion2$jef, exclude = NULL)
table(poblacion2$cony, exclude = NULL)
table(poblacion2$hijo, exclude = NULL)

suma_poblacion <- poblacion2 %>% 
  group_by(folioviv, foliohog) %>% 
  summarise(jef_1 = sum(jef),
            cony_1 = sum(cony),
            hijo_1 = sum(hijo)) %>% 
  ungroup()

poblacion2 <- poblacion2 %>% 
  left_join(suma_poblacion, by = c("folioviv", "foliohog"))



poblacion2 <- poblacion2 %>% 
  # Acceso directo a la seguridad social de ...
  mutate(
    
    # Jefatura del hogar
    jef_ss = jef_1,
    
    # Conyuge
    cony_ss = case_when(
      cony_1 > 0 ~ 1,
      TRUE       ~ 0
    ),
    
    # Descendientes
    hijo_ss = case_when(
      hijo_1 > 0 ~ 1,
      TRUE       ~ 0
    ),
    
    # Otros núcleos familiares. Se identifica a la población con acceso a la 
    # seguridad social mediante otros núcleos familiares a través de la 
    # afiliación o inscripción a servicios de salud por algún familiar dentro o
    # fuera del hogar, muerte del asegurado o por contratación propia
    s_salud = case_when(
      atemed == 1 &
        (
          !is.na(inst_1) | !is.na(inst_2) | !is.na(inst_3) | !is.na(inst_4)
        ) &
        (
          !is.na(inscr_3) | !is.na(inscr_4) | !is.na(inscr_6) | !is.na(inscr_7)
        ) ~ 1,
      !(is.na(segpop) & is.na(atemed)) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Programas sociales de pensiones para adultos mayores
    pam = case_when(
      edad >= 65 & ing_pam > 0 ~ 1,
      edad >= 65               ~ 0
    )
  )


table(poblacion$jef_ss, exclude = NULL)
table(poblacion$cony_ss, exclude = NULL)
table(poblacion$hijo_ss, exclude = NULL)
table(poblacion$s_salud, exclude = NULL)
table(poblacion$pam, exclude = NULL)

table(poblacion2$jef_ss, exclude = NULL)
table(poblacion2$cony_ss, exclude = NULL)
table(poblacion2$hijo_ss, exclude = NULL)
table(poblacion2$s_salud, exclude = NULL)
table(poblacion2$pam, exclude = NULL)


poblacion2 <- poblacion2 %>% 
  
  #** Indicador de carencia por acceso a la seguridad social
  
  # No se considera a la población que:
  # 1. Disponga de acceso directo a la seguridad social,
  # 2. Cuente con parentesco directo con alguna persona dentro del hogar
  # que tenga acceso directo,
  # 3. Reciba servicios médicos por parte de algún familiar dentro o
  # fuera del hogar, por muerte del asegurado o por contratación propia, o,
  # 4. Reciba ingresos por parte de un programa de adultos mayores.
  
  # Se considera en situación de carencia aquella población en 
  # cualquier otra situación.
  mutate(
    ic_segsoc = case_when(
      # Acceso directo
      ss_dir == 1 |
        
        # Parentesto directo: jefatura
        (par == 1 & (cony_ss == 1 | pea == 0 & hijo_ss == 1)) |
        
        # Parentesto directo: cónyuge
        (par == 2 & (jef_ss == 1 | pea == 0 & hijo_ss == 1)) |
        
        # Parentesto directo: descendientes
        (par == 3 & 
           (jef_ss == 1 | cony_ss == 1) &
           (edad < 16 | edad >= 16 & edad <= 25 & inas_esc == 0)) |
        
        # Parentesto directo: ascendientes
        (pea == 0 & (par == 4 & jef_ss == 1 | par == 5 & cony_ss == 1)) |
        
        # Otros núcleos familiares
        s_salud == 1 |
        
        # Programa de adultos mayores
        pam == 1 ~ 0,
      
      # Carencia
      TRUE       ~ 1
    )
  )



table(poblacion$ss_dir, exclude = NULL)
table(poblacion$ic_segsoc, exclude = NULL)

table(poblacion2$ss_dir, exclude = NULL)
table(poblacion2$ic_segsoc, exclude = NULL)


setequal(poblacion, poblacion2)

segsoc <- segsoc %>% 
  mutate(
    
    # Población económicamente activa (PEA). Personas de 16 años o más.
    pea = case_when(
      trab == 1 & edad >= 16 & !is.na(edad)                          ~ 1,
      (act_pnea1 == 1 | act_pnea2 == 1) &  edad >= 16 & !is.na(edad) ~ 2,
      (act_pnea1 > 1 | act_pnea2 > 1) & edad >= 16 & !is.na(edad)    ~ 0,
      TRUE                                                           ~ NA_real_
    ),
    
    #** Tipo de trabajo
    
    # Ocupación principal
    tipo_trab1 = case_when(
      pea == 0 | pea == 2 | is.na(pea) ~ NA_real_,
      TRUE ~ tipo_trab1
    ),
    
    # Ocupación secundaria
    tipo_trab2 = case_when(
      pea == 0 | pea == 2 | is.na(pea) ~ NA_real_,
      TRUE ~ tipo_trab2
    ),
    
    # Jubilados o pensionados
    jub = case_when(
      trabajo_mp == 2 & (act_pnea1 == 2 | act_pnea2 == 2) |
        ing_pens > 0 |
        inscr_2 == 2 ~ 1,
      TRUE ~ 0
    ),
    
    #** Prestaciones básicas
    
    #** Prestaciones laborales (servicios médicos)
    
    # Ocupación principal
    smlab1 = case_when(
      ocupa1 == 1 & atemed == 1 &
        !(is.na(inst_1) & is.na(inst_2) & is.na(inst_3) & is.na(inst_4)) &
        !is.na(inscr_1) ~ 1,
      ocupa1 == 1       ~ 0,
      TRUE              ~ NA_real_
    ),
    
    # Ocupación secundaria
    smlab2 = case_when(
      ocupa2 == 1 & atemed == 1 &
        !(is.na(inst_1) & is.na(inst_2) & is.na(inst_3) & is.na(inst_4)) &
        !is.na(inscr_1) ~ 1,
      ocupa2 == 1       ~ 0,
      TRUE ~ NA_real_
    ),
    
    #** Contratación voluntaria
    
    # Servicios médicos
    smcv = case_when(
      edad >= 12 & edad <= 97 & atemed == 1 &
        !(is.na(inst_1) & is.na(inst_2) & is.na(inst_3) & is.na(inst_4)) &
        !is.na(inscr_6)         ~ 1,
      edad >= 12 & edad <= 97 ~ 0,
      TRUE                      ~ NA_real_
    ),
    
    # SAR o Afore
    aforecv = case_when(
      segvol_1 == 1 & edad >= 12 ~ 1,
      is.na(segvol_1) & edad >= 12 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% 
  rename(aforlab1 = aforlab1_f,
         inclab1 = inclab1_fi) %>% 
  mutate(
    
    #** Acceso a la directo a la seguridad social
    
    ss_dir = case_when(
      
      # Ocupación principal
      tipo_trab1 == 1 & 
        (smlab1 == 1 & inclab1 == 1 & aforlab1 == 1) |
        tipo_trab1 == 2 &
        (smlab1 == 1 | smcv == 1) &
        (aforlab1 == 1 | aforecv == 1) |
        tipo_trab1 == 3 &
        (smlab1 == 1 | smcv == 1) &
        aforecv == 1 |
        
        # Ocupación secundaria
        tipo_trab2 == 1 &
        (smlab2 == 1 & inclab2 == 1 & aforlab2 == 1) |
        tipo_trab2 == 2 &
        (smlab2 == 1 | smcv == 1) &
        (aforlab2 == 1 | aforecv == 1) |
        tipo_trab2 == 3 &
        (smlab2 == 1 | smcv == 1) &
        aforecv == 1 |
        
        # Jubilados y pensionados
        jub == 1 ~ 1,
      TRUE ~ 0
    ),
    
    # Núcleos familiares
    par = case_when(
      parentesco >= 100 & parentesco < 200 ~ 1,
      parentesco >= 200 & parentesco < 300 ~ 2,
      parentesco >= 300 & parentesco < 400 ~ 3,
      parentesco == 601                    ~ 4,
      parentesco == 615                    ~ 5,
      TRUE                                 ~ 6
    ),
    
    # Información relativa a la asistencia escolar
    inas_esc = case_when(
      asis_esc == 1 ~ 0,
      asis_esc == 2 ~ 1
    ),
    
    #** Identificar los principales parentescos respecto a la jefatura del hogar
    
    # Jefatura del hogar
    jef = case_when(
      par == 1 & ss_dir == 1                                &
        (!is.na(inst_2) | !is.na(inst_3)) & !is.na(inscr_6) &
        (is.na(inst_1)  & is.na(inst_4)  & is.na(inst_6))   &
        (is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3)   &
           is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7))   ~ NA_real_,
      par == 1 & ss_dir == 1                                ~ 1,
      TRUE                                                  ~ 0
    ),
    
    # Cónyuge
    cony = case_when(
      par == 2 & ss_dir == 1                                &
        (!is.na(inst_2) | !is.na(inst_3)) & !is.na(inscr_6) &
        is.na(inst_1)  & is.na(inst_4)  & is.na(inst_6)     &
        is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3)    &
        is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7)    ~ NA_real_,
      par == 2 & ss_dir == 1                                ~ 1,
      TRUE                                                  ~ 0
    ),
    
    # Descendientes
    hijo = case_when(
      par == 3 & ss_dir == 1                                &
        (!is.na(inst_2) | !is.na(inst_3)) & !is.na(inscr_6) &
        is.na(inst_1)  & is.na(inst_4)  & is.na(inst_6)     &
        is.na(inscr_1) & is.na(inscr_2) & is.na(inscr_3)    &
        is.na(inscr_4) & is.na(inscr_5) & is.na(inscr_7)    ~ NA_real_,
      par == 3 & ss_dir == 1 & jub == 0                     |
        par == 3 & ss_dir == 1 & jub == 1 & edad > 25       ~ 1,
      TRUE                                                  ~ 0
    )
  )

suma_poblacion <- segsoc %>% 
  group_by(folioviv, foliohog) %>% 
  summarise(jef_1 = sum(jef),
            cony_1 = sum(cony),
            hijo_1 = sum(hijo)) %>% 
  ungroup()

segsoc <- segsoc %>% 
  left_join(suma_poblacion, by = c("folioviv", "foliohog")) %>% 
  
  # Acceso directo a la seguridad social de ...
  mutate(
    
    # Jefatura del hogar
    jef_ss = jef_1,
    
    # Conyuge
    cony_ss = case_when(
      cony_1 > 0 ~ 1,
      TRUE       ~ 0
    ),
    
    # Descendientes
    hijo_ss = case_when(
      hijo_1 > 0 ~ 1,
      TRUE       ~ 0
    ),
    
    # Otros núcleos familiares. Se identifica a la población con acceso a la 
    # seguridad social mediante otros núcleos familiares a través de la 
    # afiliación o inscripción a servicios de salud por algún familiar dentro o
    # fuera del hogar, muerte del asegurado o por contratación propia
    s_salud = case_when(
      atemed == 1 &
        (
          !is.na(inst_1) | !is.na(inst_2) | !is.na(inst_3) | !is.na(inst_4)
        ) &
        (
          !is.na(inscr_3) | !is.na(inscr_4) | !is.na(inscr_6) | !is.na(inscr_7)
        ) ~ 1,
      !(is.na(segpop) & is.na(atemed)) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Programas sociales de pensiones para adultos mayores
    pam = case_when(
      edad >= 65 & ing_pam > 0 ~ 1,
      edad >= 65               ~ 0
    ),
    
    #** Indicador de carencia por acceso a la seguridad social
    
    # No se considera a la población que:
    # 1. Disponga de acceso directo a la seguridad social,
    # 2. Cuente con parentesco directo con alguna persona dentro del hogar
    # que tenga acceso directo,
    # 3. Reciba servicios médicos por parte de algún familiar dentro o
    # fuera del hogar, por muerte del asegurado o por contratación propia, o,
    # 4. Reciba ingresos por parte de un programa de adultos mayores.
    
    # Se considera en situación de carencia aquella población en 
    # cualquier otra situación.
    ic_segsoc = case_when(
      # Acceso directo
      ss_dir == 1 |
        
        # Parentesto directo: jefatura
        (par == 1 & (cony_ss == 1 | pea == 0 & hijo_ss == 1)) |
        
        # Parentesto directo: cónyuge
        (par == 2 & (jef_ss == 1 | pea == 0 & hijo_ss == 1)) |
        
        # Parentesto directo: descendientes
        (par == 3 & 
           (jef_ss == 1 | cony_ss == 1) &
           (edad < 16 | edad >= 16 & edad <= 25 & inas_esc == 0)) |
        
        # Parentesto directo: ascendientes
        (pea == 0 & (par == 4 & jef_ss == 1 | par == 5 & cony_ss == 1)) |
        
        # Otros núcleos familiares
        s_salud == 1 |
        
        # Programa de adultos mayores
        pam == 1 ~ 0,
      
      # Carencia
      TRUE       ~ 1
    )
  )

setequal(poblacion, segsoc)

# I.4 Calidad y espacios en la vivienda -----------------------------------


# I.5 Acceso en los servicios básicos en la vivienda ----------------------


# I.6 Acceso a la alimentación --------------------------------------------


# I.7 Bienestar (ingresos) ------------------------------------------------


# II. Pobreza =============================================================


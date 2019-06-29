# PROGRAMA PARA LA MEDICIÓN DE LA POBREZA

# Todas las bases de datos del Modelo Estadístico 2016 para la continuidad del 
# MCS-ENIGH pueden ser obtenidas en la página de Internet del INEGI,
# www.inegi.org.mx. Originalmente todas fueron descargadas en formato DBF, 
# pero fueron convertidas a RDS en el script `conversion_dbf.R`. Los archivos
# originales se encuentran en `original/` y los generados en `data/`

# Se utilizan las siguientes bases:
# Hogares:      hogares.rds
# Población:    poblacion.rds
# Ingresos:     ingresos.rds
# Concentrado:  concentradohogar.rds
# Trabajos:     trabajos.rds
# Viviendas:    viviendas.rds
# No monetario: gastospersona.rds
#               gastoshogar.rds

# Preparación de carpetas -------------------------------------------------

# Crea carpeta de destino en caso de que no se encuentre en folder principal
if (!dir.exists("./data")) {dir.create("./data")}

# Paquetes ----------------------------------------------------------------

library(tidyverse)

# I. Indicadores de Privación Social ======================================

# I.1 Rezago educativo ----------------------------------------------------

poblacion <- readRDS("original/poblacion.rds")

rezedu <- poblacion %>% 
  
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
  filter(!(parentesco %in% c(400:499, 700:799))) %>% 
  
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
      nivelaprob < 2 | (nivelaprob == 2 & gradoaprob < 6)            ~ 0,
      
      # Con primaria completa o secundaria incompleta
      
      (nivelaprob == 2 & gradoaprob == 6)                            |
        (nivelaprob == 3 & gradoaprob < 3)                           |
        (nivelaprob %in% c(5:6) & gradoaprob < 3 & antec_esc == 1)   ~ 1,
      
      # Secundaria completa o mayor nivel educativo
      ((nivelaprob == 3 & gradoaprob == 3)                           |
         (nivelaprob == 4)                                           |
         (nivelaprob %in% c(5:6) & antec_esc == 1 & gradoaprob >= 3) |
         (nivelaprob %in% c(5:6) & antec_esc >= 2)                   |
         (nivelaprob >= 7)
      )                                                              ~ 2,
      
      # Todo lo demás
      TRUE                                                           ~ NA_real_
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
      edad %in% c(3:15) & inas_esc == 1 & niv_ed <= 1          |
        edad >= 16 & anac_e >= 1982 & niv_ed <= 1              |
        edad >= 16 & anac_e <= 1981 & niv_ed == 0              ~ 1,
      edad %in% c(0:2)                                         |
        edad %in% c(3:15) & inas_esc == 0                      |
        edad %in% c(3:15) & inas_esc == 1 & niv_ed == 2        |
        edad >= 16 & anac_e >= 1982 & niv_ed == 2              |
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
  select(folioviv:numren, edad, anac_e:inas_esc, antec_esc, niv_ed:hli) %>% 
  arrange(folioviv, foliohog, numren)

# Exportando
saveRDS(rezedu, "data/ic_rezedu16.rds")
rm(list = ls()); gc()

# I.2 Acceso a servicios de salud -----------------------------------------

poblacion <- readRDS("original/poblacion.rds")
trabajos <- readRDS("original/trabajos.rds")

# Selección de criterios para instituciones y prestaciones médicas
ins_med1 <- c("inst_1", "inst_2", "inst_3", "inst_4")
ins_med2 <- c("inst_2", "inst_3")
prestaciones <- c("inst_1",  "inst_4",  "inst_6", "inscr_1", "inscr_2",
                  "inscr_3", "inscr_4", "inscr_5", "inscr_7")

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
asal <- poblacion %>% 
  
  # Nombres de variables en minúsculas
  rename_all(tolower) %>% 
  
  # Transformar variables de interés a numéricas
  mutate_at(vars(parentesco, act_pnea1, act_pnea2), as.numeric) %>% 
  
  # Quitar de la población a huéspedes y trabajadores domésticos
  filter(!(parentesco %in% c(400:499, 700:799))) %>% 
  
  arrange(folioviv, foliohog, numren) %>% 
  
  # Agregar variables de ocupados a población
  left_join(ocupados2, by = c("folioviv", "foliohog", "numren")) %>% 
  
  #** Creación de variables para el indicador de carencia 
  
  # Población económicamente activa
  mutate(
    pea = case_when(
      trab == 1 & edad >= 16 & !is.na(edad)                          ~ 1,
      (act_pnea1 == 1 | act_pnea2 == 1) & edad >= 16 & !is.na(edad)  ~ 2,
      (act_pnea1 >  1 | act_pnea2 >  1) & edad >= 16 & !is.na(edad)  ~ 0,
      TRUE                                                           ~ NA_real_
    ),
    
    #** Tipo de trabajo
    
    # Ocupación principal
    tipo_trab1 = case_when(
      pea %in% c(0, 2) | is.na(pea) ~ NA_real_,
      TRUE                          ~ tipo_trab1
    ),
    
    # Ocupación secundaria
    tipo_trab2 = case_when(
      pea %in% c(0, 2) | is.na(pea) ~ NA_real_,
      TRUE                          ~ tipo_trab2
    ),
    
    #** Prestaciones básicas
    
    #** Prestaciones laborales (servicios médicos)
    
    # Ocupación principal
    smlab1 = case_when(
      ocupa1 == 1 & atemed == 1 & 
        rowSums(!is.na(select(., ins_med1))) > 0 &
        !is.na(inscr_1) ~ 1,
      ocupa1 == 1       ~ 0,
      TRUE              ~ NA_real_
    ),
    
    # Ocupación secundaria
    smlab2 = case_when(
      ocupa2 == 1 & atemed == 1 & 
        rowSums(!is.na(select(., ins_med1))) > 0 &
        !is.na(inscr_1) ~ 1,
      ocupa2 == 1       ~ 0,
      TRUE              ~ NA_real_
    ),
    
    # Contratación voluntaria: servicios médicos
    smcv = case_when(
      edad %in% 12:97 & atemed == 1 &
        rowSums(!is.na(select(., ins_med1))) > 0 &
        !is.na(inscr_6)       ~ 1,
      edad %in% 12:97         ~ 0,
      TRUE                    ~ NA_real_
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
      parentesco %in% 100:199 ~ 1,
      parentesco %in% 200:299 ~ 2,
      parentesco %in% 300:399 ~ 3,
      parentesco == 601       ~ 4,
      parentesco == 615       ~ 5,
      TRUE                    ~ 6
    ),
    
    # Información relativa a la asistencia escolar
    inas_esc = case_when(
      asis_esc == 1 ~ 0,
      asis_esc == 2 ~ 1
    ),
    
    #** Identificar los principales parentescos respecto a la jefatura del hogar
    
    # # Jefatura del hogar
    jef = case_when(
      par == 1 & sa_dir == 1 & !is.na(inscr_6)       &
        rowSums(!is.na(select(., ins_med2))) > 0     &
        rowSums(is.na(select(., prestaciones))) == 9 ~ NA_real_,
      par == 1 & sa_dir == 1                         ~ 1,
      TRUE                                           ~ 0
    ),
    
    # Cónyuge
    cony = case_when(
      par == 2 & sa_dir == 1 & !is.na(inscr_6)       &
        rowSums(!is.na(select(., ins_med2))) > 0     &
        rowSums(is.na(select(., prestaciones))) == 9 ~ NA_real_,
      par == 2 & sa_dir == 1                         ~ 1,
      TRUE                                           ~ 0
    ),
    
    # Descendientes
    hijo = case_when(
      par == 3 & sa_dir == 1 & !is.na(inscr_6)       &
        rowSums(!is.na(select(., ins_med2))) > 0     &
        rowSums(is.na(select(., prestaciones))) == 9 ~ NA_real_,
      par == 3 & sa_dir == 1                         ~ 1,
      TRUE                                           ~ 0
    )
  )

#** Crear suma de las indicadoras de parentesco
suma_poblacion <- asal %>% 
  group_by(folioviv, foliohog) %>% 
  summarise(jef_1 = sum(jef),
            cony_1 = sum(cony),
            hijo_1 = sum(hijo)) %>% 
  ungroup()

#** Unir la suma a los datos de población
asalud <- asal %>% 
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
        rowSums(!is.na(select(., inst_1, inst_2, inst_3, inst_4))) > 0 & 
        rowSums(!is.na(select(., inscr_3, inscr_4, inscr_6, inscr_7))) > 0 ~ 1,
      rowSums(!is.na(select(., segpop, atemed))) > 0                       ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Indicador de carencia por acceso a los servicios de salud
    
    ic_asalud = case_when(
      
      # Acceso directo
      sa_dir == 1 |
        
        # Parentesco directo: jefatura
        par == 1 & (cony_sa == 1 | pea == 0 & hijo_sa == 1) |
        
        # Parentesco directo: cónyuge
        par == 2 & (jef_sa == 1 | pea == 0 & hijo_sa == 1) |
        
        # Parentesco directo: descendientes
        par == 3 & 
        (edad < 16 & (jef_sa == 1 | cony_sa == 1) |
           (edad %in% 16:25) & inas_esc == 0 & (jef_sa == 1 | cony_sa == 1)) |
        
        # Parentesco directo: ascendentes
        pea == 0 & (par == 4 & jef_sa == 1 | par == 5 & cony_sa == 1) |
        
        # Otros núcleos familiares
        s_salud == 1 |
        
        # Acceso reportado
        segpop == 1 |
        segpop == 2 & atemed == 1 & 
        rowSums(!is.na(select(., inst_1:inst_6))) > 0 |
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
      disc1 %in% 1:7   |
        disc2 %in% 2:7 |
        disc3 %in% 3:7 |
        disc4 %in% 4:7 |
        disc5 %in% 5:7 |
        disc6 %in% 6:7 |
        disc7 == 7              ~ 1,
      disc1 == 8 | disc1 == "&" ~ 0
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

trabajos <- readRDS("original/trabajos.rds")

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

ingresos <- readRDS("original/ingresos.rds")

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

poblacion <- readRDS("original/poblacion.rds")

segsoc <- poblacion %>% 
  
  # Nombres de variables en minúsculas
  rename_all(tolower) %>% 
  
  # Transformar variables de interés a numéricas
  mutate_at("parentesco", as.numeric) %>% 
  
  # Quitar de la población a huéspedes y trabajadores domésticos
  filter(!((parentesco >= 400 & parentesco < 500) | 
             parentesco >= 700 & parentesco < 800)) %>% 
  left_join(prestaciones, by = c("folioviv", "foliohog", "numren")) %>% 
  left_join(pensiones, by = c("folioviv", "foliohog", "numren")) %>% 
  mutate(
    
    # Población económicamente activa (PEA). Personas de 16 años o más.
    pea = case_when(
      trab == 1 & edad >= 16                          ~ 1,
      (act_pnea1 == 1 | act_pnea2 == 1) &  edad >= 16 ~ 2,
      (act_pnea1 > 1 | act_pnea2 > 1) & edad >= 16    ~ 0,
      TRUE                                            ~ NA_real_
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
  ) %>% 
  select(folioviv,
         foliohog,
         numren,
         pea,
         jub,
         ss_dir,
         par,
         jef_ss,
         cony_ss,
         hijo_ss,
         s_salud,
         pam,
         ic_segsoc) %>% 
  arrange(folioviv, foliohog, numren)

# Exportar prestaciones y pensiones, limpiar espacio de trabajo

saveRDS(prestaciones, "data/prestaciones16.rds")
saveRDS(pensiones, "data/pensiones16.rds")
saveRDS(segsoc, "data/ic_segsoc16.rds")

rm(list = ls()); gc()



# I.4 Calidad y espacios en la vivienda -----------------------------------

hogares <- readRDS("original/hogares.rds")

vivienda <- readRDS("original/viviendas.rds")

cev <- hogares %>% 
  left_join(vivienda, by = "folioviv") %>% 
  arrange(folioviv) %>% 

  # Material de construcción de la vivienda
  mutate_at(
    c("mat_pisos",
      "mat_techos",
      "tot_resid",
      "num_cuarto"),
    as.numeric) %>% 
  mutate(
    mat_muros = as.numeric(mat_pared),
    
    # Índice de hacinamiento
    cv_hac = tot_resid / num_cuarto,
    
    # Indicadores de carencia:
    # Del material de piso
    icv_pisos = case_when(
      mat_pisos == 1 ~ 1,
      mat_pisos > 1  ~ 0,
      TRUE           ~ NA_real_
    ),
    
    # Material de techos
    icv_techos = case_when(
      mat_techos <= 2 ~ 1,
      mat_techos > 2  ~ 0,
      TRUE            ~ NA_real_
    ),
    
    # Material de muros
    icv_muros = case_when(
      mat_muros <= 5 ~ 1,
      mat_muros > 5  ~ 0,
      TRUE           ~ NA_real_
    ),
    
    # Índice de hacinamiento de la vivienda
    icv_hac = case_when(
      cv_hac > 2.5  ~ 1,
      cv_hac <= 2.5 ~ 0,
      TRUE          ~ NA_real_
    ),
    
    # Indicador de carencia por calidad y espacios de la vivienda
    
    # Se considera en situación de carencia a la población que:
    # 1. Presente carencia en cualquiera de los subindicadores 
    # de esta dimensión
    
    # No se considera en situación de carencia a la población que:
    # 1. Habite en una vivienda sin carencia en todos los subindicadores
    # de esta dimensión
    
    ic_cv = case_when(
      is.na(icv_pisos)    |
        is.na(icv_techos) |
        is.na(icv_muros)  |
        is.na(icv_hac)    ~ NA_real_,
      icv_pisos == 1    |
        icv_techos == 1 |
        icv_muros == 1  |
        icv_hac == 1      ~ 1,
      icv_pisos == 0    &
        icv_techos == 0 &
        icv_muros == 0  &
        icv_hac == 0      ~ 0
    )
  
  ) %>% 
  
  # Depurar variables
  
  select(folioviv, foliohog, tot_resid, num_cuarto, icv_pisos:ic_cv) %>% 
  arrange(folioviv, foliohog)

# Exportar prestaciones y pensiones, limpiar espacio de trabajo

saveRDS(cev, "data/ic_cev16.rds")

rm(list = ls()); gc()

# I.5 Acceso en los servicios básicos en la vivienda ----------------------

hogares <- readRDS("original/viviendas.rds")

sbv <- hogares %>% 
  mutate(
    aguav = as.numeric(disp_agua),
    drenajev = as.numeric(drenaje),
    elecv = as.numeric(disp_elect),
    combusv = as.numeric(combustibl),
    
    # Disponibilidad de:
    # agua
    sb_agua = recode(
      aguav, `7` = 1, `6` = 2, `5` = 3, `4` = 4, `3` = 5, `2` = 6, `1` = 7
    ),
    
    # drenaje
    sb_dren = recode(
      drenajev, `5` = 1, `4` = 2, `3` = 3, `2` = 4, `1` = 5
    ),
    
    # Electricidad
    sb_luz = recode(
      elecv, `5` = 1, `4` = 2, `3` = 2, `2` = 3, `1` = 4
    ),
    
    # Indicador de carencia de servicio de:
    # agua
    isb_agua = case_when(
      sb_agua <= 5                                              ~ 1,
      !is.na(sb_agua)                                           & 
        (sb_agua > 5 | sb_agua == 4 & ubica_geo == "200580016") ~ 0
    ),
    
    # drenaje
    isb_dren = case_when(
      sb_dren <= 3                  ~ 1,
      sb_dren > 3 & !is.na(sb_dren) ~ 0
    ),
    
    # electricidad
    isb_luz = case_when(
      sb_luz == 1                 ~ 1,
      sb_luz > 1 & !is.na(sb_luz) ~ 0
    ),
    
    # Indicador de combustible
    isb_combus = case_when(
      !is.na(combusv) & combusv <= 2 & estufa_chi == 2                 ~ 1,
      !is.na(combusv) & (combusv <= 2 & estufa_chi == 1 | combusv > 2) ~ 0
    ),
    
    # Indicador de carencia por acceso a los servicios básicos en la vivienda
    
    # Se considera en situación de carencia a la población que:
    # 1. Presente carencia en cualquiera de los subindicadores de esta 
    # dimensión
    
    # No se considera en situación de carencia a la población que:
    # 1. Habite en una vivienda sin carencia en todos los subindicadores
    # de esta dimensión
    
    ic_sbv = case_when(
      is.na(isb_agua)     |
        is.na(isb_dren)   |
        is.na(isb_luz)    |
        is.na(isb_combus) ~ NA_real_,
      isb_agua == 1       |
        isb_dren == 1     |
        isb_luz == 1      |
        isb_combus == 1   ~ 1,
      isb_agua == 0       &
        isb_dren == 0     &
        isb_luz == 0      &
        isb_combus == 0   ~ 0
    )
  ) %>% 
  
  # Depurar variables
  select(folioviv, isb_agua:ic_sbv) %>% 
  arrange(folioviv)

# Exportar
saveRDS(sbv, "data/ic_sbv16.rds")

rm(list = ls())

gc()

# I.6 Acceso a la alimentación --------------------------------------------

ali_pob <- readRDS("original/poblacion.rds")

ali_pob <- ali_pob %>% 
  
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
  
  # Indicador de hogares con menores de 18 años
  mutate(men = if_else(edad > 17, true = 0, false = 1))

# Idenficar menores en hogar
suma_poblacion <- ali_pob %>% 
  group_by(folioviv, foliohog) %>% 
  summarise(men = sum(men)) %>% 
  ungroup() %>% 
  mutate(id_men = if_else(men == 0, true = 0, false = 1)) %>% 
  select(-men)

# Identificación de población en hogares
ali_hog <- readRDS("original/hogares.rds")

ali_hog <- ali_hog %>% 
  mutate(
    
    #** Preguntas para hogares SIN población menor a 18 años. 
    # Algún adulto:
    
    # tuvo una alimentación en muy poca variedad de accceso a 
    # alimentos
    ia_1ad = if_else(acc_alim4 == 1, true = 1, false = 0, missing = 0),
    
    # dejó de desayunar, comer o cenar
    ia_2ad = if_else(acc_alim5 == 1, true = 1, false = 0, missing = 0),
    
    # comió menos de lo que debía comer
    ia_3ad = if_else(acc_alim6 == 1, true = 1, false = 0, missing = 0),
    
    # se quedó sin comida
    ia_4ad = if_else(acc_alim2 == 1, true = 1, false = 0, missing = 0),
    
    # sintió hambre pero no comió
    ia_5ad = if_else(acc_alim7 == 1, true = 1, false = 0, missing = 0),
    
    # solo comió una vez la día o dejó de comer todo un día
    ia_6ad = if_else(acc_alim8 == 1, true = 1, false = 0, missing = 0),
    
    #** Preguntas para hogares CON población menor a 18 años. 
    # Alguien de 0 a 17 años:
    
    # tuvo una alimentación en muy poca variedad de accceso a 
    # alimentos
    ia_7men = if_else(acc_alim11 == 1, true = 1, false = 0, missing = 0),
    
    # comió menos de lo que debía
    ia_8men = if_else(acc_alim12 == 1, true = 1, false = 0, missing = 0),
    
    # se le tuvo que disminuir la cantidad servida en las comidas
    ia_9men = if_else(acc_alim13 == 1, true = 1, false = 0, missing = 0),
    
    # sintió hambre pero no comió
    ia_10men = if_else(acc_alim14 == 1, true = 1, false = 0, missing = 0),
    
    # se acostó con hambre
    ia_11men = if_else(acc_alim15 == 1, true = 1, false = 0, missing = 0),
    
    # comió una vez al día o dejó de comer todo un día
    ia_12men = if_else(acc_alim16 == 1, true = 1, false = 0, missing = 0)
  )

# Construcción de la escala de inseguridad alimentaria
ali <- ali_hog %>% 
  left_join(suma_poblacion, by = c("folioviv", "foliohog")) %>% 
  arrange(folioviv, foliohog)

ali <- ali %>% 
  mutate(
    
    # Escala de hogares SIN menores de 18 años
    tot_iaad = if_else(
      id_men == 0,
      true = ia_1ad + ia_2ad + ia_3ad + ia_4ad + ia_5ad + ia_6ad,
      false = NA_real_
    ),
    
    # Escala de hogares CON menores de 18 años
    tot_iamen = if_else(
      id_men == 1,
      true = ia_1ad + ia_2ad + ia_3ad + ia_4ad + ia_5ad + ia_6ad +
        ia_7men + ia_8men + ia_9men + ia_10men + ia_11men + ia_12men,
      false = NA_real_
    )
  ) %>% 
  
  # Grado de inseguridad alimentaria
  
  # Este subindicador genera cuatro clasificaciones de la siguiente forma:
  # 0 Sin inseguridad alimentaria
  # 1 Inseguridad alimentaria leve
  # 2 Inseguridad alimentaria moderada
  # 3 Inseguridad alimentaria severa
  mutate(
    ins_ali = case_when(
      tot_iaad == 0 | tot_iamen == 0                                ~ 0,
      tot_iaad >= 1 & tot_iaad < 3 | tot_iamen >= 1 & tot_iamen < 4 ~ 1,
      tot_iaad >= 3 & tot_iaad < 5 | tot_iamen >= 4 & tot_iamen < 8 ~ 2,
      tot_iaad >= 5 | tot_iamen >= 8 & !is.na(tot_iamen)            ~ 3,
      TRUE                                                          ~ NA_real_
    ),
    
    # Indicador de carencia por acceso a la alimentación;
    
    # Se considera a la población en hogares que:
    # 1. Presenten inseguridad alimentaria moderada o severa.
    
    # No se considera a la población que:
    # 1. No presente inseguridad alimentaria o presente un grado de inseguridad
    # alimentaria leve.
    ic_ali = if_else(ins_ali == 2 | ins_ali == 3, true = 1, false = 0)
  ) %>% 
  
  # Depurar variables
  select(folioviv:foliohog,
         id_men,
         ia_1ad:ia_12men,
         tot_iaad:tot_iamen,
         ins_ali:ic_ali) %>% 
  arrange(folioviv, foliohog)

# Exportar
saveRDS(suma_poblacion, "data/menores16.rds")
saveRDS(ali, "data/ic_ali16.rds")

rm(list = ls())

gc()

# I.7 Bienestar (ingresos) ------------------------------------------------

# Para la construcción del ingreso corriente del hogar es necesario utilizar
# información sobre la condición de ocupación y los ingresos de los individuos.
# Se utiliza la información contenida en la base "trabajos" para identificar
# a la población ocupada que declara tener como prestación laboral aguinaldo,
# ya sea por su trabajo principal o secundario, a fin de incorporar
# los ingresos por este concepto en la medición.

# Creación del ingreso monetario deflactado a pesos de agosto del 2016.

# Ingresos

trabajos_original <- readRDS("original/trabajos.rds")
ingresos_original <- readRDS("original/ingresos.rds")

aguinaldo_ <- trabajos_original %>% 
  select(folioviv:numren, id_trabajo, pres_8) %>% 
  gather(variable, valor, -(folioviv:id_trabajo)) %>% 
  unite(temporal, variable, id_trabajo, sep = "") %>% 
  spread(temporal, valor) %>% 
  mutate(

  # Población con al menos un empleo  
    trab = 1,
  # Aguinaldo de trabajo principal
    aguinaldo1 = case_when(pres_81 == "08" ~ 1, TRUE ~ 0),
  # Aguinaldo de trabajo segundario
    aguinaldo2 = case_when(pres_82 == "08" ~ 1, TRUE ~ 0)
  ) %>% 
  select(folioviv:numren, aguinaldo1, aguinaldo2, trab) %>% 
  arrange(folioviv, foliohog, numren)

ingresos_ <- ingresos_original %>% 
  rename_all(tolower) %>% 
  arrange(folioviv, foliohog, numren, clave) %>% 
  full_join(aguinaldo_, by = c("folioviv", "foliohog", "numren"))  %>% 
  filter(
    !(clave == "P009" & aguinaldo1 != 1 & !is.na(clave)),
    !(clave == "P016" & aguinaldo2 != 1 & !is.na(clave))
    )

#saveRDS(aguinaldo_, "data/aguinaldo.rds")
#rm(list = setdiff(ls(), "ingresos_"))

# Ahora se deflacta el ingreso recibido por los hogares a precios de agosto de
# 2016. Para ello se utilizan las variables de los meses, las cuales toman los
# valores 2 a 10 e indican el mes en que se recibió el ingreso respectivo.

# Definición de los deflactores 2016
deflactores <- unlist(list(
  dic15	=	0.9915096155, ene16	=	0.9952905552, feb16	=	0.9996486737,
  mar16	=	1.0011208981, abr16	=	0.9979505968, may16	=	0.9935004643,
  jun16	=	0.9945962676, jul16	=	0.9971893899, ago16	=	1.0000000000,
  sep16	=	1.0061063849, oct16	=	1.0122127699, nov16	=	1.0201259756,
  dic16	=	1.0248270555))

ingresos_ <- ingresos_ %>% 
  mutate_at(vars(mes_1:mes_6), as.numeric) %>% 
  mutate(
    ing_6 = case_when(
      mes_6 == 2 ~ ing_6 / deflactores["feb16"],
      mes_6 == 3 ~ ing_6 / deflactores["mar16"],
      mes_6 == 4 ~ ing_6 / deflactores["abr16"],
      mes_6 == 5 ~ ing_6 / deflactores["may16"],
      mes_6 == 6 ~ ing_6 / deflactores["jun16"],
      TRUE       ~ ing_6
    ),
    ing_5 = case_when(
      mes_5 == 3 ~ ing_5 / deflactores["mar16"],
      mes_5 == 4 ~ ing_5 / deflactores["abr16"],
      mes_5 == 5 ~ ing_5 / deflactores["may16"],
      mes_5 == 6 ~ ing_5 / deflactores["jun16"],
      mes_5 == 7 ~ ing_5 / deflactores["jul16"],
      TRUE       ~ ing_5
    ),
    ing_4 = case_when(
      mes_4 == 4 ~ ing_4 / deflactores["abr16"],
      mes_4 == 5 ~ ing_4 / deflactores["may16"],
      mes_4 == 6 ~ ing_4 / deflactores["jun16"],
      mes_4 == 7 ~ ing_4 / deflactores["jul16"],
      mes_4 == 8 ~ ing_4 / deflactores["ago16"],
      TRUE       ~ ing_4
    ),
    ing_3 = case_when(
      mes_3 == 5 ~ ing_3 / deflactores["may16"],
      mes_3 == 6 ~ ing_3 / deflactores["jun16"],
      mes_3 == 7 ~ ing_3 / deflactores["jul16"],
      mes_3 == 8 ~ ing_3 / deflactores["ago16"],
      mes_3 == 9 ~ ing_3 / deflactores["sep16"],
      TRUE       ~ ing_3
    ),
    ing_2 = case_when(
      mes_2 == 6  ~ ing_2 / deflactores["jun16"],
      mes_2 == 7  ~ ing_2 / deflactores["jul16"],
      mes_2 == 8  ~ ing_2 / deflactores["ago16"],
      mes_2 == 9  ~ ing_2 / deflactores["sep16"],
      mes_2 == 10 ~ ing_2 / deflactores["oct16"],
      TRUE        ~ ing_2
    ),
    ing_1 = case_when(
      mes_1 == 7  ~ ing_1 / deflactores["jul16"],
      mes_1 == 8  ~ ing_1 / deflactores["ago16"],
      mes_1 == 9  ~ ing_1 / deflactores["sep16"],
      mes_1 == 10 ~ ing_1 / deflactores["oct16"],
      mes_1 == 11 ~ ing_1 / deflactores["nov16"],
      
      # Se deflactan las claves P008 y P015 (Reparto de utilidades) y 
      # P009 y P016 (aguinaldo) con los deflactores de mayo a agosto 2016 
      # y de diciembre de 2015 a agosto 2016, respectivamente, y se obtiene 
      # el promedio mensual.
      clave %in% c("P008", "P015") ~ ing_1 / deflactores["may16"] / 12,
      clave %in% c("P009", "P016") ~ ing_1 / deflactores["dic15"] / 12,
      TRUE                         ~ ing_1
    )
  ) %>% 
  mutate(
    # Una vez realizada la deflactación, se procede a obtener el ingreso
    # mensual promedio en los últimos seis meses, para cada persona y
    # clave de ingreso.
    ing_mens = rowMeans(select(., ing_1:ing_6), na.rm = TRUE),
    
    # Para obtener el ingreso corriente monetario del hogar, se seleccionan 
    # las claves de ingreso correspondientes.
    ing_mon = if_else(
      clave %in% paste0("P", sprintf("%03d", c(1:9, 11:16, 18:48, 67:81))),
      true = ing_mens,
      false = NA_real_
    )
  ) %>% 
  select(folioviv, foliohog, ing_mon) %>% 
  group_by(folioviv, foliohog) %>% 
  summarise(ing_mon = sum(ing_mon, na.rm = TRUE)) %>% 
  arrange(folioviv, foliohog, ing_mon)

# saveRDS(aguinaldo, "data/aguinaldo.rds")
# saveRDS(ingresos_, "data/ingreso_deflactado16.rds")
rm(list = setdiff(ls(), "ingresos_"))
gc()

# Creación del ingreso no monetario deflactado a pesos de agosto del 2016.
gastos_persona_original <- readRDS("original/gastospersona.rds")
gastos_hogar_original <- readRDS("original/gastoshogar.rds")

no_monetario_persona <- gastos_persona_original %>% 
  rename_all(tolower) %>% 
  rename(frecuenciap = frec_rem) %>% 
  arrange(folioviv, foliohog)

no_monetario_hogar <- gastos_hogar_original %>% 
  rename_all(tolower) %>% 
  arrange(folioviv, foliohog) %>% 
  mutate(numren = NA, origen_rem = NA, inst = NA)


# II. Pobreza =============================================================


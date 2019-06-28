##################################################################

#PROGRAMA PARA LA MEDICI?N DE LA POBREZA 2016

##################################################################

#Todas las bases de datos del Modelo Estad?stico 2016 para la continuidad del MCS-ENIGH pueden ser obtenidas en la p?gina de
#Internet del INEGI, www.inegi.org.mx, y deben estar en formato  *.dbf.

#Este programa debe ser utilizado con la versi?n 3.4.1.

#En este programa se utilizan las siguientes bases:
#Base hogares: hogares.dbf
#Base poblaci?n:poblacion.dbf
#Base de ingresos: ingresos.dbf
#Base de concentrado: concentradohogar.dbf
#Base de trabajos: trabajos.dbf
#Base de viviendas: viviendas.dbf
#Bases de no monetario: gastospersona.dbf y gastoshogar.dbf

#En este programa se utilizan dos tipos de archivos: bases originales y bases generadas.
#El primero es el directorio donde se encuentran las bases del Modelo Estad?stico 2016 para la continuidad del MCS-ENIGH
#En el segundo se guardan las bases de datos generadas por el programa
#Estos se ubican en las siguientes carpetas:

#1) Bases originales: "C:/Bases de datos"
#2) Bases generadas: "C:/Bases"

#Se instalan los paquetes y librer?as a utilizar en el programa
install.packages(
  c(
    "multcomp",
    "mvtnorm",
    "survival",
    "splines",
    "car",
    "doBy",
    "reshape",
    "data.table",
    "rgl",
    "relimp",
    "lmtest",
    "leaps",
    "Hmisc",
    "effects",
    "colorspace",
    "aplpack",
    "abind",
    "RODBC",
    "mvtnorm"
  )
)
library(foreign)
library(car)
library(doBy)
library(reshape)
library(data.table)
library(stats)

rm(list = ls())

#Se define el directorio ra?z de las bases de datos
setwd("C:/")

memory.size()

#########PROGRAMA PARA LA MEDICI?N DE LA POBREZA 2016###########


##Parte I Indicadores de Privaci?n Social:
##INDICADOR DE CARENCIA POR REZAGO EDUCATIVO ----

poblacion1 <- read.dbf("Bases de datos/poblacion.dbf", as.is = TRUE)
names(poblacion1) =  tolower(names(poblacion1))
#Poblaci?n objeto: no se incluye a hu?spedes ni trabajadores dom?sticos
poblacion2 <- as.numeric(poblacion1$parentesco)
index = ((poblacion2 >= 400 &
            poblacion2 < 500) | (poblacion2 >= 700 & poblacion2 < 800))
index = (index == FALSE)
poblacion = poblacion1[index, ]

#A?o de nacimiento
anio_med = 2016

attach(poblacion)

poblacion$anac_e = anio_med - edad


#Inasistencia escolar (se reporta para personas de 3 a?os o m?s)
poblacion$inas_esc = NA
poblacion$inas_esc = recode(asis_esc, "1=0; 2=1; else=NA")

##Nivel educativo
poblacion$nivelaprob <- as.numeric(nivelaprob)
poblacion$gradoaprob <- as.numeric(gradoaprob)
poblacion$antec_esc <- as.numeric(antec_esc)
detach(poblacion)

attach(poblacion)

poblacion$niv_ed = NA
poblacion$niv_ed[(nivelaprob < 2) |
                   (nivelaprob == 2 &
                      gradoaprob < 6)] = 0 #"Con primaria incompleta o menos"

poblacion$niv_ed[(nivelaprob == 2 &
                    gradoaprob == 6) | (nivelaprob == 3 & gradoaprob < 3) |
                   (nivelaprob == 5 |
                      nivelaprob == 6) &
                   gradoaprob < 3 &
                   antec_esc == 1] = 1 # "Primaria completa o secundaria incompleta"

poblacion$niv_ed[((nivelaprob == 3 &
                     gradoaprob == 3) |
                    (nivelaprob == 4) |
                    (nivelaprob == 5 & antec_esc == 1 & gradoaprob >= 3) |
                    (nivelaprob == 6 &
                       antec_esc == 1 &
                       gradoaprob >= 3) |
                    (nivelaprob == 5 &
                       antec_esc >= 2) |
                    (nivelaprob == 6 &
                       antec_esc >= 2) |
                    (nivelaprob >= 7)
)] = 2 #"Secundaria completa o mayor nivel educativo"

detach(poblacion)
table(poblacion$niv_ed)

##Indicador de carencia por rezago educativo

#Se considera en situaci?n de carencia por rezago educativo
#a la poblaci?n que cumpla con alguno de los siguientes criterios:

#1. Se encuentra entre los 3 y los 15 a?os y no ha terminado la educaci?n
#obligatoria (secundaria terminada) o no asiste a la escuela.
#2. Tiene una edad de 16 a?os o m?s, su a?o de nacimiento aproximado es 1981
#o anterior, y no dispone de primaria completa.
#3. Tiene una edad de 16 a?os o m?s, su a?o de nacimiento aproximado es 1982
#en adelante, y no dispone de primaria secundaria completa.

attach(poblacion)
poblacion$ic_rezedu = NA
poblacion$ic_rezedu[(edad >= 3 &
                       edad <= 15) & inas_esc == 1 & (niv_ed == 0 | niv_ed == 1)] = 1
poblacion$ic_rezedu[(edad >= 16) &
                      (anac_e >= 1982) & (niv_ed == 0 | niv_ed == 1)] = 1
poblacion$ic_rezedu[(edad >= 16) & (anac_e <= 1981) & (niv_ed == 0)] = 1
poblacion$ic_rezedu[(edad >= 0 & edad <= 2)] = 0
poblacion$ic_rezedu[(edad >= 3 & edad <= 15) & inas_esc == 0] = 0
poblacion$ic_rezedu[(edad >= 3 &
                       edad <= 15) & inas_esc == 1 & (niv_ed == 2)] = 0
poblacion$ic_rezedu[(edad >= 16) & (anac_e >= 1982) & (niv_ed == 2)] = 0
poblacion$ic_rezedu[(edad >= 16) &
                      (anac_e <= 1981) & (niv_ed == 1 | niv_ed == 2)] = 0
#detach(poblacion)

##Poblaci?n ind?gena
poblacion$hablaind <- as.numeric(hablaind)

poblacion$hli = NA
poblacion$hli[(edad >= 3 & hablaind == 1)] = 1
poblacion$hli[(edad >= 3 & hablaind == 2)] = 0

table(poblacion$hli)
table(poblacion$ic_rezedu)

poblacion = poblacion[, c(
  "folioviv",
  "foliohog",
  "numren",
  "edad",
  "anac_e",
  "inas_esc",
  "antec_esc",
  "niv_ed",
  "ic_rezedu",
  "hli"
)]
poblacion <- orderBy( ~ +folioviv + foliohog + numren, data = poblacion)

write.csv(poblacion, 'Bases/ic_rezedu16.csv', row.names = FALSE)
write.dbf(poblacion, 'Bases/ic_rezedu16.dbf')
detach(poblacion)

##Parte II Indicadores de Privaci?n Social:
##INDICADOR DE CARENCIA POR ACCESO A LOS SERVICIOS DE SALUD ----

rm(list = ls())
trabajos <- read.dbf("Bases de datos/trabajos.dbf", as.is = TRUE)

##Tipo de trabajador: identifica la poblaci?n subordinada e independiente
attach(trabajos)

#Subordinados
trabajos$tipo_trab = NA
trabajos$tipo_trab[subor == 1] = 1

#Independientes que reciben un pago
trabajos$tipo_trab[subor == 2 & indep == 1 & tiene_suel == 1] = 2
trabajos$tipo_trab[subor == 2 & indep == 2 & pago == 1] = 2

#Independientes que no reciben un pago
trabajos$tipo_trab[subor == 2 & indep == 1 & tiene_suel == 2] = 3
trabajos$tipo_trab[subor == 2 & indep == 2 & (pago == 2 | pago == 3)] = 3

#Ocupaci?n principal o secundaria
trabajos$ocupa = NA
trabajos$ocupa[id_trabajo == 1] = 1
trabajos$ocupa[id_trabajo == 2] = 0

#Distinci?n de prestaciones en trabajo principal y secundario
trabajos = trabajos[, c("folioviv",
                        "foliohog",
                        "numren",
                        "id_trabajo",
                        "tipo_trab",
                        "ocupa")]
trabajos2 = reshape(
  trabajos,
  idvar = c("folioviv", "foliohog", "numren"),
  timevar = "id_trabajo",
  direction = "wide"
)
detach(trabajos)

attach(trabajos2)
trabajos2$ocupa2 = NA
trabajos2$ocupa2[is.na(ocupa.2) == TRUE] = 0
trabajos2$ocupa2[ocupa.2 == 0] = 1
detach(trabajos2)

names(trabajos2)[4:6] <- c("tipo_trab1", "ocupa1", "tipo_trab2")

#Identificaci?n de la poblaci?n trabajadora (toda la que reporta al menos un empleo en la base de trabajos.dbf)
trabajos2$trab = 1
table(trabajos2$trab)

trabajos2 = trabajos2[, c(
  "folioviv",
  "foliohog",
  "numren",
  "tipo_trab1",
  "ocupa1",
  "tipo_trab2",
  "ocupa2",
  "trab"
)]
trabajos2 <-
  orderBy(~ +folioviv + foliohog + numren, data = trabajos2)
write.csv(trabajos2, 'Bases/ocupados16.csv', row.names = FALSE)
write.dbf(trabajos2, 'Bases/ocupados16.dbf')

rm(list = ls())
#Integraci?n de la bases
poblacion1 <- read.dbf("Bases de datos/poblacion.dbf", as.is = TRUE)
names(poblacion1) =  tolower(names(poblacion1))
#Poblaci?n objeto: no se incluye a hu?spedes ni trabajadores dom?sticos
poblacion2 <- as.numeric(poblacion1$parentesco)
index = ((poblacion2 >= 400 &
            poblacion2 < 500) |
           (poblacion2 >= 700 & poblacion2 < 800))
index = (index == FALSE)
poblacion = poblacion1[index,]

poblacion <-
  orderBy(~ +folioviv + foliohog + numren, data = poblacion)
ocupados = read.dbf('Bases/ocupados16.dbf', as.is = TRUE)

poblacion = merge(
  poblacion,
  ocupados,
  by.x = c("folioviv", "foliohog", "numren"),
  all.x = TRUE
)
rm(ocupados)

#PEA (personas de 16 a?os o m?s)
attach(poblacion)
poblacion$pea = NA
poblacion$pea[(trab == 1 & (edad >= 16 & is.na(edad) == FALSE))] = 1
poblacion$pea[((act_pnea1 == "1" |
                  act_pnea2 == "1") &
                 (edad >= 16 & is.na(edad) == FALSE))] = 2
poblacion$pea[(edad >= 16 &
                 is.na(edad) == FALSE) &
                (act_pnea1 == "2" |
                   act_pnea1 == "3" |
                   act_pnea1 == "4" |
                   act_pnea1 == "5" | act_pnea1 == "6") &
                is.na(poblacion$pea) == TRUE] = 0
poblacion$pea[(edad >= 16 &
                 is.na(edad) == FALSE) &
                (act_pnea2 == "2" |
                   act_pnea2 == "3" |
                   act_pnea2 == "4" |
                   act_pnea2 == "5" | act_pnea2 == "6") &
                is.na(poblacion$pea) == TRUE] = 0

table(poblacion$pea)
detach(poblacion)


#Tipo de trabajo

#Ocupaci?n principal
attach(poblacion)
poblacion$tipo_trab1[pea == 1 &
                       is.na(tipo_trab1) == FALSE &
                       is.na(pea) == FALSE] = poblacion$tipo_trab1[pea == 1 &
                                                                     is.na(tipo_trab1) == FALSE &
                                                                     is.na(pea) == FALSE]
poblacion$tipo_trab1[(pea == 0 | pea == 2)] = NA
poblacion$tipo_trab1[is.na(pea) == TRUE] = NA


#Ocupaci?n secundaria
poblacion$tipo_trab2[pea == 1 &
                       is.na(tipo_trab2) == FALSE &
                       is.na(pea) == FALSE] = poblacion$tipo_trab2[pea == 1 &
                                                                     is.na(tipo_trab2) == FALSE &
                                                                     is.na(pea) == FALSE]
poblacion$tipo_trab2[(pea == 0 | pea == 2)] = NA
poblacion$tipo_trab2[is.na(pea) == TRUE] = NA



#Prestaciones b?sicas

#Prestaciones laborales (Servicios m?dicos)

#Ocupaci?n principal
poblacion$smlab1 = NA
poblacion$smlab1[ocupa1 == 1 &
                   atemed == 1 &
                   (inst_1 == 1 | inst_2 == 2 |
                      inst_3 == 3 |
                      inst_4 == 4) & (inscr_1 == 1)] = 1
poblacion$smlab1[ocupa1 == 1 & is.na(poblacion$smlab1) == TRUE] = 0


#Ocupaci?n secundaria
poblacion$smlab2 = NA
poblacion$smlab2[ocupa2 == 1 &
                   atemed == 1 &
                   (inst_1 == 1 | inst_2 == 2 |
                      inst_3 == 3 |
                      inst_4 == 4) & (inscr_1 == 1)] = 1
poblacion$smlab2[ocupa2 == 1 & is.na(poblacion$smlab2) == TRUE] = 0


#Contrataci?n voluntaria: servicios m?dicos

#Servicios m?dicos
poblacion$smcv = NA
poblacion$smcv[atemed == 1 &
                 (inst_1 == 1 |
                    inst_2 == 2 |
                    inst_3 == 3 | inst_4 == 4) &
                 (inscr_6 == 6) & (edad >= 12 & edad <= 97)] = 1
poblacion$smcv[(edad >= 12 &
                  edad <= 97) & is.na(poblacion$smcv) == TRUE] = 0
detach(poblacion)

#Acceso directo a servicios de salud

#Ocupaci?n principal
attach(poblacion)
poblacion$sa_dir = NA
poblacion$sa_dir[tipo_trab1 == 1 & smlab1 == 1] = 1
poblacion$sa_dir[tipo_trab1 == 2 & (smlab1 == 1 | smcv == 1)] = 1
poblacion$sa_dir[tipo_trab1 == 3 & (smlab1 == 1 | smcv == 1)] = 1

#Ocupaci?n secundaria
poblacion$sa_dir[tipo_trab2 == 1 & smlab2 == 1] = 1
poblacion$sa_dir[tipo_trab2 == 2 & (smlab2 == 1 | smcv == 1)] = 1
poblacion$sa_dir[tipo_trab2 == 3 & (smlab2 == 1 | smcv == 1)] = 1

#N?cleos familiares
parent <- as.numeric(poblacion$parentesco)
poblacion$par = 6
poblacion$par[(parent >= 100 & parent < 200)] = 1
poblacion$par[(parent >= 200 & parent < 300)] = 2
poblacion$par[(parent >= 300 & parent < 400)] = 3
poblacion$par[parent == 601] = 4
poblacion$par[parent == 615] = 5

#Asimismo, se utilizar? la informaci?n relativa a la asistencia a la escuela
poblacion$inas_esc = NA
poblacion$inas_esc[asis_esc == 1] = 0
poblacion$inas_esc[asis_esc == 2] = 1
detach(poblacion)

#En primer lugar se identifican los principales parentescos respecto
#a la jefatura del hogar;

attach(poblacion)

poblacion$jef = 0
poblacion$jef[par == 1 & sa_dir == 1] = 1
poblacion$jef[par == 1 &
                sa_dir == 1 &
                (((inst_2 == "2" |
                     inst_3 == "3") &
                    inscr_6 == "6") &
                   (
                     is.na(inst_1) == TRUE &
                       is.na(inst_4) == TRUE &
                       is.na(inst_6) == TRUE
                   ) &
                   (
                     is.na(inscr_1) == TRUE &
                       is.na(inscr_2) == TRUE &
                       is.na(inscr_3) == TRUE &
                       is.na(inscr_4) == TRUE &
                       is.na(inscr_5) == TRUE &
                       is.na(inscr_7) == TRUE
                   )
                )] = NA
table(poblacion$jef)

poblacion$cony = 0
poblacion$cony[par == 2 & sa_dir == 1] = 1
poblacion$cony[par == 2 &
                 sa_dir == 1 &
                 (((inst_2 == "2" |
                      inst_3 == "3") &
                     inscr_6 == "6") &
                    (
                      is.na(inst_1) == TRUE &
                        is.na(inst_4) == TRUE &
                        is.na(inst_6) == TRUE
                    ) &
                    (
                      is.na(inscr_1) == TRUE &
                        is.na(inscr_2) == TRUE &
                        is.na(inscr_3) == TRUE &
                        is.na(inscr_4) == TRUE &
                        is.na(inscr_5) == TRUE &
                        is.na(inscr_7) == TRUE
                    )
                 )] = NA
table(poblacion$cony)

poblacion$hijo = 0
poblacion$hijo[par == 3 & sa_dir == 1] = 1
poblacion$hijo[par == 3 &
                 sa_dir == 1 &
                 (((inst_2 == "2" |
                      inst_3 == "3") &
                     inscr_6 == "6") &
                    (
                      is.na(inst_1) == TRUE &
                        is.na(inst_4) == TRUE &
                        is.na(inst_6) == TRUE
                    ) &
                    (
                      is.na(inscr_1) == TRUE &
                        is.na(inscr_2) == TRUE &
                        is.na(inscr_3) == TRUE &
                        is.na(inscr_4) == TRUE &
                        is.na(inscr_5) == TRUE &
                        is.na(inscr_7) == TRUE
                    )
                 )] = NA
table(poblacion$hijo)

detach(poblacion)

attach(poblacion)
poblacion4 = aggregate(
  x = list(jef, cony, hijo),
  by = list(folioviv, foliohog),
  FUN = sum,
  na.rm = FALSE
)
detach(poblacion)

names(poblacion4)[1:5] <-
  c("folioviv", "foliohog", "jef_1", "cony_1", "hijo_1")

poblacion = merge(poblacion,
                  poblacion4,
                  by.x = c("folioviv", "foliohog"),
                  all.x = TRUE)

rm(poblacion4)
attach(poblacion)

#Acceso directo a los servicios de salud de la jefatura del hogar
poblacion$jef_sa = poblacion$jef_1
table(poblacion$jef_sa)

#Acceso directo a los servicios de salud de conyuge de la jefatura del hogar
poblacion$cony_sa = 0
poblacion$cony_sa[(cony_1 >= 1 & is.na(cony_1) == FALSE)] = 1
table(poblacion$cony_sa)

#Acceso directo a los servicios de salud de hijos(as) de la jefatura del hogar
poblacion$hijo_sa = 0
poblacion$hijo_sa[(hijo_1 >= 1 & is.na(hijo_1) == FALSE)] = 1
table(poblacion$hijo_sa)

#Otros n?cleos familiares: se identifica a la poblaci?n con acceso a los servicios
#de salud mediante otros n?cleos familiares a trav?s de la afiliaci?n
#o inscripci?n a servicios de salud por alg?n familiar dentro o
#fuera del hogar, muerte del asegurado o por contrataci?n propia
poblacion$s_salud = NA
poblacion$s_salud[atemed == 1 &
                    (inst_1 == 1 |
                       inst_2 == 2 |
                       inst_3 == 3 |
                       inst_4 == 4) &
                    (inscr_3 == 3 |
                       inscr_4 == 4 | inscr_6 == 6 | inscr_7 == 7)] = 1
poblacion$s_salud[is.na(segpop) == FALSE &
                    is.na(atemed) == FALSE &
                    is.na(poblacion$s_salud) == TRUE] = 0
table(poblacion$s_salud)

detach(poblacion)


attach(poblacion)
#Indicador de carencia por acceso a los servicios de salud
#Se considera en situaci?n de carencia por acceso a servicios de salud
#a la poblaci?n que:
#1. No se encuentra afiliada o inscrita al Seguro Popular o alguna
#instituci?n que proporcione servicios m?dicos, ya sea por prestaci?n laboral,
#contrataci?n voluntaria o afiliaci?n de un familiar por parentesco directo

poblacion$ic_asalud = 1

#Acceso directo
poblacion$ic_asalud[sa_dir == 1] = 0

#Parentesco directo: jefatura
poblacion$ic_asalud[par == 1 & cony_sa == 1] = 0
poblacion$ic_asalud[par == 1 & pea == 0 & hijo_sa == 1] = 0

#Parentesco directo: c?nyuge
poblacion$ic_asalud[par == 2 & jef_sa == 1] = 0
poblacion$ic_asalud[par == 2 & pea == 0 & hijo_sa == 1] = 0

#Parentesco directo: descendientes
poblacion$ic_asalud[par == 3 & edad < 16 & jef_sa == 1] = 0
poblacion$ic_asalud[par == 3 & edad < 16 & cony_sa == 1] = 0
poblacion$ic_asalud[par == 3 &
                      (edad >= 16 &
                         edad <= 25) & inas_esc == 0 & jef_sa == 1] = 0
poblacion$ic_asalud[par == 3 &
                      (edad >= 16 &
                         edad <= 25) & inas_esc == 0 & cony_sa == 1] = 0

#Parentesco directo: ascendientes
poblacion$ic_asalud[par == 4 & pea == 0 & jef_sa == 1] = 0
poblacion$ic_asalud[par == 5 & pea == 0 & cony_sa == 1] = 0

#Otros n?cleos familiares
poblacion$ic_asalud[s_salud == 1] = 0

#Acceso reportado
poblacion$ic_asalud[segpop == 1 |
                      (
                        segpop == 2 &
                          atemed == 1 &
                          (
                            inst_1 == "1" |
                              inst_2 == "2" |
                              inst_3 == "3" |
                              inst_4 == "4" |
                              inst_5 == "5" | inst_6 == "6"
                          )
                      ) | segvol_2 == "2"] = 0


##Poblaci?n con al menos alguna discapacidad, sea f?sica o mental

poblacion$discap = 0
poblacion$discap[(disc1 >= 1 & disc1 <= 7)] = 1
poblacion$discap[(disc2 >= 2 & disc2 <= 7)] = 1
poblacion$discap[(disc3 >= 3 & disc3 <= 7)] = 1
poblacion$discap[(disc4 >= 4 & disc4 <= 7)] = 1
poblacion$discap[(disc5 >= 5 & disc5 <= 7)] = 1
poblacion$discap[(disc6 >= 6 & disc6 <= 7)] = 1
poblacion$discap[(disc7 == 7)] = 1
poblacion$discap[(disc1 == 8 |
                    disc1 == "&" | is.na(disc1) == TRUE)] = 0


detach(poblacion)

table(poblacion$ic_asalud)
table(poblacion$discap)

poblacion = poblacion[, c("folioviv", "foliohog", "numren", "sexo", "discap", "ic_asalud")]
poblacion <-
  orderBy(~ +folioviv + foliohog + numren, data = poblacion)
write.csv(poblacion, 'Bases/ic_asalud16.csv', row.names = FALSE)
write.dbf(poblacion, 'Bases/ic_asalud16.dbf')

##Parte III Indicadores de Privaci?n social:
##INDICADOR DE CARENCIA POR ACCESO A LA SEGURIDAD SOCIAL ----

rm(list = ls())
trabajos <- read.dbf("Bases de datos/trabajos.dbf", as.is = TRUE)

##Tipo de trabajador: identifica la poblaci?n subordinada e independiente
attach(trabajos)

#Subordinados
trabajos$tipo_trab = NA
trabajos$tipo_trab[subor == 1] = 1

#Independientes que reciben un pago
trabajos$tipo_trab[subor == 2 & indep == 1 & tiene_suel == 1] = 2
trabajos$tipo_trab[subor == 2 & indep == 2 & pago == 1] = 2

#Independientes que no reciben un pago
trabajos$tipo_trab[subor == 2 & indep == 1 & tiene_suel == 2] = 3
trabajos$tipo_trab[subor == 2 &
                     indep == 2 & (pago == 2 | pago == 3)] =
  3


#Prestaciones laborales: incapacidad en caso de enfermedad o maternidad con goce de sueldo y SAR o Afore
trabajos$inclab = NA
trabajos$inclab[is.na(pres_7) == TRUE] = 0
trabajos$inclab[pres_7 == "07"] = 1

trabajos$aforlab = NA
trabajos$aforlab[is.na(pres_14) == TRUE] = 0
trabajos$aforlab[pres_14 == "14"] = 1

#Ocupaci?n principal o secundaria
trabajos$ocupa = NA
trabajos$ocupa[id_trabajo == 1] = 1
trabajos$ocupa[id_trabajo == 2] = 0

#Distinci?n de prestaciones en trabajo principal y secundario
trabajos = trabajos[, c(
  "folioviv",
  "foliohog",
  "numren",
  "id_trabajo",
  "tipo_trab",
  "inclab",
  "aforlab",
  "ocupa"
)]
trabajos2 = reshape(
  trabajos,
  idvar = c("folioviv", "foliohog", "numren"),
  timevar = "id_trabajo",
  direction = "wide"
)
detach(trabajos)

attach(trabajos2)
trabajos2$ocupa2 = NA
trabajos2$ocupa2[is.na(ocupa.2) == TRUE] = 0
trabajos2$ocupa2[ocupa.2 == 0] = 1
detach(trabajos2)

names(trabajos2)[4:10] <-
  c("tipo_trab1",
    "inclab1",
    "aforlab1",
    "ocupa1",
    "tipo_trab2",
    "inclab2",
    "aforlab2")
trabajos2 <-
  trabajos2[, -(5:6)] #Se eliminan las variales: "inclab1", "aforlab1"

#Identificaci?n de la poblaci?n trabajadora (toda la que reporta al menos un empleo en la base de trabajos)
trabajos2$trab = 1
table(trabajos2$trab)

trabajos2 = trabajos2[, c(
  "folioviv",
  "foliohog",
  "numren",
  "tipo_trab1",
  "ocupa1",
  "tipo_trab2",
  "inclab2",
  "aforlab2",
  "ocupa2",
  "trab"
)]
trabajos2 <-
  orderBy(~ +folioviv + foliohog + numren, data = trabajos2)
write.csv(trabajos2, 'Bases/prestaciones16.csv', row.names = FALSE)
write.dbf(trabajos2, 'Bases/prestaciones16.dbf')



#Ingresos por jubilaciones o pensiones
rm(list = ls())
ingresos <- read.dbf("Bases de datos/ingresos.dbf", as.is = TRUE)
names(ingresos) =  tolower(names(ingresos))
attach(ingresos)
index = (clave == "P032" |
           clave == "P033" | clave == "P044" | clave == "P045")
index = (index == TRUE)
ingresos = ingresos[index,]
detach(ingresos)

attach(ingresos)
ingresos$ing_pens[(clave == "P032" | clave == "P033")] = rowMeans(
  data.frame(ing_1[(clave == "P032" |
                                                                      clave == "P033")], ing_2[(clave == "P032" |
                                                                                                  clave == "P033")], ing_3[(clave == "P032" |
                                                                                                                              clave == "P033")], ing_4[(clave == "P032" |
                                                                                                                                                          clave == "P033")], ing_5[(clave == "P032" |
                                                                                                                                                                                      clave == "P033")], ing_6[(clave == "P032" |
                                                                                                                                                                                                                  clave == "P033")]),
                                                  na.rm = TRUE)
ingresos$ing_pam[(clave == "P044" |
                    clave == "P045")] = rowMeans(data.frame(ing_1[(clave == "P044" |
                                                                     clave == "P045")], ing_2[(clave == "P044" |
                                                                                                 clave == "P045")], ing_3[(clave == "P044" |
                                                                                                                             clave == "P045")], ing_4[(clave == "P044" |
                                                                                                                                                         clave == "P045")], ing_5[(clave == "P044" |
                                                                                                                                                                                     clave == "P045")], ing_6[(clave == "P044" |
                                                                                                                                                                                                                 clave == "P045")]),
                                                 na.rm = TRUE)
ingresos$ing_pens[is.na(ingresos$ing_pens) == TRUE] = 0
ingresos$ing_pam[is.na(ingresos$ing_pam) == TRUE] = 0
detach(ingresos)

ingresos = ingresos[, c("folioviv", "foliohog", "numren", "ing_pens", "ing_pam")]

attach(ingresos)
ingresos2 = aggregate(
  x = list(ing_pens, ing_pam),
  by = list(folioviv, foliohog, numren),
  FUN = sum,
  na.rm = FALSE
)
detach(ingresos)

names(ingresos2)[1:5] <-
  c("folioviv", "foliohog", "numren", "ing_pens", "ing_pam")

ingresos2 <-
  orderBy(~ +folioviv + foliohog + numren, data = ingresos2)
write.csv(ingresos2, 'Bases/pensiones16.csv', row.names = FALSE)
write.dbf(ingresos2, 'Bases/pensiones16.dbf')

#Construcci?n del indicador

rm(list = ls())

poblacion1 <- read.dbf("Bases de datos/poblacion.dbf", as.is = TRUE)
names(poblacion1) =  tolower(names(poblacion1))
#Poblaci?n objeto: no se incluye a hu?spedes ni trabajadores dom?sticos

poblacion2 <- as.numeric(poblacion1$parentesco)
index = ((poblacion2 >= 400 &
            poblacion2 < 500) |
           (poblacion2 >= 700 & poblacion2 < 800))
index = (index == FALSE)
poblacion = poblacion1[index,]

poblacion <-
  orderBy(~ +folioviv + foliohog + numren, data = poblacion)
prestaciones = read.dbf('Bases/prestaciones16.dbf', as.is = TRUE)
poblacion = merge(
  poblacion,
  prestaciones,
  by.x = c("folioviv", "foliohog", "numren"),
  all.x = TRUE
)
rm(prestaciones)
poblacion <-
  orderBy(~ +folioviv + foliohog + numren, data = poblacion)
pensiones = read.dbf('Bases/pensiones16.dbf', as.is = TRUE)
poblacion = merge(
  poblacion,
  pensiones,
  by.x = c("folioviv", "foliohog", "numren"),
  all.x = TRUE
)
rm(pensiones)

#PEA (personas de 16 a?os o m?s)
attach(poblacion)
poblacion$pea = NA
poblacion$pea[(trab == 1 & (edad >= 16 & is.na(edad) == FALSE))] = 1
poblacion$pea[((act_pnea1 == "1" |
                  act_pnea2 == "1") &
                 (edad >= 16 & is.na(edad) == FALSE))] = 2
poblacion$pea[(edad >= 16 &
                 is.na(edad) == FALSE) &
                (act_pnea1 == "2" |
                   act_pnea1 == "3" |
                   act_pnea1 == "4" |
                   act_pnea1 == "5" | act_pnea1 == "6") &
                is.na(poblacion$pea) == TRUE] = 0
poblacion$pea[(edad >= 16 &
                 is.na(edad) == FALSE) &
                (act_pnea2 == "2" |
                   act_pnea2 == "3" |
                   act_pnea2 == "4" |
                   act_pnea2 == "5" | act_pnea2 == "6") &
                is.na(poblacion$pea) == TRUE] = 0

table(poblacion$pea)
detach(poblacion)


#Tipo de trabajo

#Ocupaci?n principal
attach(poblacion)
poblacion$tipo_trab1[pea == 1 &
                       is.na(tipo_trab1) == FALSE &
                       is.na(pea) == FALSE] = poblacion$tipo_trab1[pea == 1 &
                                                                     is.na(tipo_trab1) == FALSE &
                                                                     is.na(pea) == FALSE]
poblacion$tipo_trab1[(pea == 0 | pea == 2)] = NA
poblacion$tipo_trab1[is.na(pea) == TRUE] = NA


#Ocupaci?n secundaria
poblacion$tipo_trab2[pea == 1 &
                       is.na(tipo_trab2) == FALSE &
                       is.na(pea) == FALSE] = poblacion$tipo_trab2[pea == 1 &
                                                                     is.na(tipo_trab2) == FALSE &
                                                                     is.na(pea) == FALSE]
poblacion$tipo_trab2[(pea == 0 | pea == 2)] = NA
poblacion$tipo_trab2[is.na(pea) == TRUE] = NA

#Jubilados o pensionados
poblacion$jub = NA
poblacion$jub[trabajo_mp == 2 &
                (poblacion$act_pnea1 == "2" |
                   poblacion$act_pnea2 == "2")] = 1
poblacion$jub[ing_pens > 0 &  is.na(ing_pens) == FALSE] = 1
poblacion$jub[inscr_2 == 2] = 1
poblacion$jub[is.na(poblacion$jub) == TRUE] = 0


#Prestaciones b?sicas

#Prestaciones laborales (Servicios m?dicos)

#Ocupaci?n principal
poblacion$smlab1 = NA
poblacion$smlab1[ocupa1 == 1 &
                   atemed == 1 &
                   (inst_1 == 1 | inst_2 == 2 |
                      inst_3 == 3 |
                      inst_4 == 4) & (inscr_1 == 1)] = 1
poblacion$smlab1[ocupa1 == 1 & is.na(poblacion$smlab1) == TRUE] = 0



#Ocupaci?n secundaria
poblacion$smlab2 = NA
poblacion$smlab2[ocupa2 == 1 &
                   atemed == 1 &
                   (inst_1 == 1 | inst_2 == 2 |
                      inst_3 == 3 |
                      inst_4 == 4) & (inscr_1 == 1)] = 1
poblacion$smlab2[ocupa2 == 1 & is.na(poblacion$smlab2) == TRUE] = 0


#Contrataci?n voluntaria: servicios m?dicos y SAR o Afore

#Servicios m?dicos
poblacion$smcv = NA
poblacion$smcv[atemed == 1 &
                 (inst_1 == 1 |
                    inst_2 == 2 |
                    inst_3 == 3 | inst_4 == 4) &
                 (inscr_6 == 6) & (edad >= 12 & edad <= 97)] = 1
poblacion$smcv[(edad >= 12 &
                  edad <= 97) & is.na(poblacion$smcv) == TRUE] = 0



#SAR o Afore
poblacion$aforecv = NA
poblacion$aforecv[segvol_1 == 1 & (edad >= 12 &
                                     is.na(edad) == FALSE)] = 1
poblacion$aforecv[is.na(segvol_1) == TRUE &
                    (edad >= 12 &
                       is.na(edad) == FALSE) &
                    is.na(poblacion$aforecv) == TRUE] = 0
detach(poblacion)

names(poblacion)[181:182] <- c("aforlab1", "inclab1")


#Acceso directo a la seguridad social

#Ocupaci?n principal
attach(poblacion)
poblacion$ss_dir = NA
poblacion$ss_dir[tipo_trab1 == 1 &
                   (smlab1 == 1 & inclab1 == 1 & aforlab1 == 1)] = 1
poblacion$ss_dir[tipo_trab1 == 2 &
                   ((smlab1 == 1 |
                       smcv == 1) & (aforlab1 == 1 | aforecv == 1))] = 1
poblacion$ss_dir[tipo_trab1 == 3 &
                   ((smlab1 == 1 | smcv == 1) & aforecv == 1)] = 1

#Ocupaci?n secundaria
poblacion$ss_dir[tipo_trab2 == 1 &
                   (smlab2 == 1 & inclab2 == 1 & aforlab2 == 1)] = 1
poblacion$ss_dir[tipo_trab2 == 2 &
                   ((smlab2 == 1 |
                       smcv == 1) & (aforlab2 == 1 | aforecv == 1))] = 1
poblacion$ss_dir[tipo_trab2 == 3 &
                   ((smlab2 == 1 | smcv == 1) & aforecv == 1)] = 1

#Jubilados y pensionados
poblacion$ss_dir[jub == 1] = 1
poblacion$ss_dir[is.na(poblacion$ss_dir) == TRUE] = 0


#N?cleos familiares
parent <- as.numeric(poblacion$parentesco)
poblacion$par = 6
poblacion$par[(parent >= 100 & parent < 200)] = 1
poblacion$par[(parent >= 200 & parent < 300)] = 2
poblacion$par[(parent >= 300 & parent < 400)] = 3
poblacion$par[parent == 601] = 4
poblacion$par[parent == 615] = 5
table(poblacion$par)


#Asimismo, se utilizar? la informaci?n relativa a la asistencia a la escuela
poblacion$inas_esc = NA
poblacion$inas_esc[asis_esc == 1] = 0
poblacion$inas_esc[asis_esc == 2] = 1
detach(poblacion)

#En primer lugar se identifican los principales parentescos respecto
#a la jefatura del hogar;

attach(poblacion)

poblacion$jef = 0
poblacion$jef[par == 1 & ss_dir == 1] = 1
poblacion$jef[par == 1 &
                ss_dir == 1 &
                (((inst_2 == "2" |
                     inst_3 == "3") &
                    inscr_6 == "6") &
                   (
                     is.na(inst_1) == TRUE &
                       is.na(inst_4) == TRUE &
                       is.na(inst_6) == TRUE
                   ) &
                   (
                     is.na(inscr_1) == TRUE &
                       is.na(inscr_2) == TRUE &
                       is.na(inscr_3) == TRUE &
                       is.na(inscr_4) == TRUE &
                       is.na(inscr_5) == TRUE &
                       is.na(inscr_7) == TRUE
                   )
                )] = NA
table(poblacion$jef)

poblacion$cony = 0
poblacion$cony[par == 2 & ss_dir == 1] = 1
poblacion$cony[par == 2 &
                 ss_dir == 1 &
                 (((inst_2 == "2" |
                      inst_3 == "3") &
                     inscr_6 == "6") &
                    (
                      is.na(inst_1) == TRUE &
                        is.na(inst_4) == TRUE &
                        is.na(inst_6) == TRUE
                    ) &
                    (
                      is.na(inscr_1) == TRUE &
                        is.na(inscr_2) == TRUE &
                        is.na(inscr_3) == TRUE &
                        is.na(inscr_4) == TRUE &
                        is.na(inscr_5) == TRUE &
                        is.na(inscr_7) == TRUE
                    )
                 )] = NA
table(poblacion$cony)

poblacion$hijo = 0
poblacion$hijo[par == 3 & ss_dir == 1 & jub == 0] = 1
poblacion$hijo[par == 3 &
                 ss_dir == 1 &
                 jub == 1 & (edad > 25 & is.na(edad) == FALSE)] = 1
poblacion$hijo[par == 3 &
                 ss_dir == 1 &
                 (((inst_2 == "2" |
                      inst_3 == "3") &
                     inscr_6 == "6") &
                    (
                      is.na(inst_1) == TRUE &
                        is.na(inst_4) == TRUE &
                        is.na(inst_6) == TRUE
                    ) &
                    (
                      is.na(inscr_1) == TRUE &
                        is.na(inscr_2) == TRUE &
                        is.na(inscr_3) == TRUE &
                        is.na(inscr_4) == TRUE &
                        is.na(inscr_5) == TRUE &
                        is.na(inscr_7) == TRUE
                    )
                 )] = NA
table(poblacion$hijo)

detach(poblacion)

attach(poblacion)
poblacion4 = aggregate(
  x = list(jef, cony, hijo),
  by = list(folioviv, foliohog),
  FUN = sum,
  na.rm = FALSE
)
detach(poblacion)
names(poblacion4)[1:5] <-
  c("folioviv", "foliohog", "jef_1", "cony_1", "hijo_1")

poblacion = merge(poblacion,
                  poblacion4,
                  by.x = c("folioviv", "foliohog"),
                  all.x = TRUE)

rm(poblacion4)
attach(poblacion)

#Acceso directo a la seguridad social de la jefatura del hogar
poblacion$jef_ss = poblacion$jef_1
table(poblacion$jef_ss)

#Acceso directo a la seguridad social de conyuge de la jefatura del hogar
poblacion$cony_ss = 0
poblacion$cony_ss[(cony_1 >= 1 & is.na(cony_1) == FALSE)] = 1
table(poblacion$cony_ss)

#Acceso directo a la seguridad social de hijos(as) de la jefatura del hogar
poblacion$hijo_ss = 0
poblacion$hijo_ss[(hijo_1 >= 1 & is.na(hijo_1) == FALSE)] = 1
table(poblacion$hijo_ss)

#Otros n?cleos familiares: se identifica a la poblaci?n con acceso a la seguridad
#social mediante otros n?cleos familiares a trav?s de la afiliaci?n
#o inscripci?n a servicios de salud por alg?n familiar dentro o
#fuera del hogar, muerte del asegurado o por contrataci?n propia
poblacion$s_salud = NA
poblacion$s_salud[atemed == 1 &
                    (inst_1 == 1 |
                       inst_2 == 2 |
                       inst_3 == 3 |
                       inst_4 == 4) &
                    (inscr_3 == 3 |
                       inscr_4 == 4 | inscr_6 == 6 | inscr_7 == 7)] = 1
poblacion$s_salud[is.na(segpop) == FALSE &
                    is.na(atemed) == FALSE &
                    is.na(poblacion$s_salud) == TRUE] = 0
table(poblacion$s_salud)

#Programas sociales de pensiones para adultos mayores
poblacion$pam = NA
poblacion$pam[(edad >= 65 & is.na(edad) == FALSE) & ing_pam > 0] = 1
poblacion$pam[(edad >= 65 &
                 is.na(edad) == FALSE) &
                is.na(poblacion$pam) == TRUE] = 0
table(poblacion$pam)

detach(poblacion)

#Indicador de carencia por acceso a la seguridad social
#No se considera en situaci?n de carencia por acceso a la seguridad social
#a la poblaci?n que:
#1. Disponga de acceso directo a la seguridad social,
#2. Cuente con parentesco directo con alguna persona dentro del hogar
#que tenga acceso directo,
#3. Reciba servicios m?dicos por parte de alg?n familiar dentro o
#fuera del hogar, por muerte del asegurado o por contrataci?n propia, o,
#4. Reciba ingresos por parte de un programa de adultos mayores.

#Se considera en situaci?n de carencia por acceso a la seguridad social
#aquella poblaci?n:

#1. En cualquier otra situaci?n.

attach(poblacion)

#Indicador de carencia por acceso a la seguridad social
poblacion$ic_segsoc = NA

#Acceso directo
poblacion$ic_segsoc[ss_dir == 1] = 0

#Parentesco directo: jefatura
poblacion$ic_segsoc[par == 1 & cony_ss == 1] = 0
poblacion$ic_segsoc[par == 1 & pea == 0 & hijo_ss == 1] = 0

#Parentesco directo: c?nyuge
poblacion$ic_segsoc[par == 2 & jef_ss == 1] = 0
poblacion$ic_segsoc[par == 2 & pea == 0 & hijo_ss == 1] = 0

#Parentesco directo: descendientes
poblacion$ic_segsoc[par == 3 & edad < 16 & jef_ss == 1] = 0
poblacion$ic_segsoc[par == 3 & edad < 16 & cony_ss == 1] = 0
poblacion$ic_segsoc[par == 3 &
                      (edad >= 16 &
                         edad <= 25) & inas_esc == 0 & jef_ss == 1] = 0
poblacion$ic_segsoc[par == 3 &
                      (edad >= 16 &
                         edad <= 25) & inas_esc == 0 & cony_ss == 1] = 0

#Parentesco directo: ascendientes
poblacion$ic_segsoc[par == 4 & pea == 0 & jef_ss == 1] = 0
poblacion$ic_segsoc[par == 5 & pea == 0 & cony_ss == 1] = 0

#Otros n?cleos familiares
poblacion$ic_segsoc[s_salud == 1] = 0

#Programa de adultos mayores
poblacion$ic_segsoc[pam == 1] = 0

poblacion$ic_segsoc[is.na(poblacion$ic_segsoc) == TRUE] = 1

table(poblacion$pea)
table(poblacion$jub)
table(poblacion$ss_dir)
table(poblacion$jef_ss)
table(poblacion$cony_ss)
table(poblacion$hijo_ss)
table(poblacion$s_salud)
table(poblacion$pam)
table(poblacion$ic_segsoc)

detach(poblacion)


poblacion = poblacion[, c(
  "folioviv",
  "foliohog",
  "numren",
  "pea",
  "jub",
  "ss_dir",
  "par",
  "jef_ss",
  "cony_ss",
  "hijo_ss",
  "s_salud",
  "pam",
  "ic_segsoc"
)]
poblacion <-
  orderBy(~ +folioviv + foliohog + numren, data = poblacion)
write.csv(poblacion, 'Bases/ic_segsoc16.csv', row.names = FALSE)
write.dbf(poblacion, 'Bases/ic_segsoc16.dbf')

#Parte IV Indicadores de Privaci?n social:
#INDICADOR DE CARENCIA POR CALIDAD Y ESPACIOS EN LA VIVIENDA ----

#Material de construcci?n de la vivienda
rm(list = ls())

hogares <- read.dbf('Bases de datos/hogares.dbf', as.is = TRUE)

hogares <- orderBy(~ +folioviv, data = hogares)

vivienda <- read.dbf('Bases de datos/viviendas.dbf', as.is = TRUE)

hogares2 = merge(hogares,
                 vivienda,
                 by = c("folioviv"),
                 all.x = TRUE)
hogares2 <- orderBy(~ +folioviv, data = hogares2)

attach(hogares2)
hogares2$mat_pisos = recode(mat_pisos, "-1=NA")
hogares2$mat_pisos <- ifelse(mat_pisos == "&", NA, mat_pisos)
detach(hogares2)

attach(hogares2)
hogares2$mat_pisos <- as.numeric(mat_pisos)
hogares2$mat_techos <- as.numeric(mat_techos)
hogares2$mat_muros <- as.numeric(mat_pared)
hogares2$tot_resid <- as.numeric(tot_resid)
hogares2$num_cuarto <- as.numeric(num_cuarto)
detach(hogares2)

#?ndice de hacinamiento
hogares2$cv_hac = hogares2$tot_resid / hogares2$num_cuarto

#detach(hogares2)
attach(hogares2)

#Indicador de carencia del material de piso de la vivienda
hogares2$icv_pisos = NA
hogares2$icv_pisos[mat_pisos == 1] = 1
hogares2$icv_pisos[mat_pisos > 1 & is.na(mat_pisos) == FALSE] = 0
table(hogares2$icv_pisos)

#Indicador de carencia por material de techos de la vivienda
hogares2$icv_techos = NA
hogares2$icv_techos[mat_techos <= 2] = 1
hogares2$icv_techos[mat_techos > 2 & is.na(mat_techos) == FALSE] = 0
table(hogares2$icv_techos)

#Indicador de carencia del material de muros de la vivienda
hogares2$icv_muros = NA
hogares2$icv_muros[mat_muros <= 5] = 1
hogares2$icv_muros[mat_muros > 5 & is.na(mat_muros) == FALSE] = 0
table(hogares2$icv_muros)

#Indicador de carencia por ?ndice de hacinamiento de la vivienda
hogares2$icv_hac = NA
hogares2$icv_hac[cv_hac > 2.5 & is.na(cv_hac) == FALSE] = 1
hogares2$icv_hac[cv_hac <= 2.5] = 0
table(hogares2$icv_hac)
detach(hogares2)


#Indicador de carencia por calidad y espacios de la vivienda;

#Se considera en situaci?n de carencia por calidad y espacios
#de la vivienda a la poblaci?n que:

#1. Presente carencia en cualquiera de los subindicadores de esta dimensi?n

#No se considera en situaci?n de carencia por rezago calidad y espacios
#de la vivienda a la poblaci?n que:

#1. Habite en una vivienda sin carencia en todos los subindicadores
#de esta dimensi?n

attach(hogares2)

hogares2$ic_cv[icv_pisos == 1 |
                 icv_techos == 1 |
                 icv_muros == 1 | icv_hac == 1] = 1
hogares2$ic_cv[icv_pisos == 0 &
                 icv_techos == 0 &
                 icv_muros == 0 & icv_hac == 0] = 0
hogares2$ic_cv[is.na(icv_pisos) == TRUE |
                 is.na(icv_muros) == TRUE |
                 is.na(icv_muros) == TRUE |
                 is.na(icv_hac) == TRUE] = NA

detach(hogares2)

table(hogares2$icv_pisos)
table(hogares2$icv_techos)
table(hogares2$icv_muros)
table(hogares2$icv_hac)
table(hogares2$ic_cv)

hogares2 = hogares2[, c(
  "folioviv",
  "foliohog",
  "tot_resid",
  "num_cuarto",
  "icv_pisos",
  "icv_techos",
  "icv_muros",
  "icv_hac",
  "ic_cv"
)]
hogares2 <- orderBy(~ +folioviv + foliohog, data = hogares2)
write.csv(hogares2, 'Bases/ic_cev16.csv', row.names = FALSE)
write.dbf(hogares2, 'Bases/ic_cev16.dbf')


#Parte V Indicadores de Privaci?n Social:
#INDICADOR DE CARENCIA POR ACCESO A LOS SERVICIOS B?SICOS EN LA VIVIENDA ----

rm(list = ls())


hogares <- read.dbf("Bases de datos/viviendas.dbf", as.is = TRUE)

attach(hogares)
hogares$aguav <- as.numeric(disp_agua)
hogares$drenajev <- as.numeric(drenaje)
hogares$elecv <- as.numeric(disp_elect)
hogares$combusv <- as.numeric(combustibl)
detach(hogares)

#Disponibilidad de agua
attach(hogares)
hogares$aguav[hogares$aguav == 0] = NA
hogares$sb_agua = NA
hogares$sb_agua[aguav == 7] = 1
hogares$sb_agua[aguav == 6] = 2
hogares$sb_agua[aguav == 5] = 3
hogares$sb_agua[aguav == 4] = 4
hogares$sb_agua[aguav == 3] = 5
hogares$sb_agua[aguav == 2] = 6
hogares$sb_agua[aguav == 1] = 7
table(hogares$sb_agua)

#Drenaje
hogares$drenajev[hogares$drenajev == 0] = NA
hogares$sb_dren = NA
hogares$sb_dren[drenajev == 5] = 1
hogares$sb_dren[drenajev == 4] = 2
hogares$sb_dren[drenajev == 3] = 3
hogares$sb_dren[drenajev == 2] = 4
hogares$sb_dren[drenajev == 1] = 5
table(hogares$sb_dren)

#Electricidad
hogares$elecv[hogares$elecv == 0] = NA
hogares$sb_luz = NA
hogares$sb_luz[elecv == 5] = 1
hogares$sb_luz[elecv == 3 | elecv == 4] = 2
hogares$sb_luz[elecv == 2] = 3
hogares$sb_luz[elecv == 1] = 4
table(hogares$sb_luz)

detach(hogares)

#Indicador de carencia de servicio de agua

attach(hogares)
hogares$isb_agua[sb_agua <= 5] = 1
hogares$isb_agua[sb_agua > 5 & is.na(sb_agua) == FALSE] = 0
hogares$isb_agua[sb_agua == 4 & ubica_geo == "200580016"] = 0
table(hogares$isb_agua)

#Indicador de carencia de servicio de drenaje
hogares$isb_dren[sb_dren <= 3] = 1
hogares$isb_dren[sb_dren > 3 & is.na(sb_dren) == FALSE] = 0
table(hogares$isb_dren)

#Indicador de carencia de servicios de electricidad
hogares$isb_luz[sb_luz == 1] = 1
hogares$isb_luz[sb_luz > 1 & is.na(sb_luz) == FALSE] = 0
table(hogares$isb_luz)

#Indicador combus
hogares$isb_combus = NA
hogares$isb_combus[(combusv == 1 |
                      combusv == 2) &
                     estufa_chi == "2" & is.na(combusv) == FALSE] = 1
hogares$isb_combus[(combusv == 1 |
                      combusv == 2) &
                     estufa_chi == "1" & is.na(combusv) == FALSE] = 0
hogares$isb_combus[combusv >= 3 &
                     combusv <= 6 & is.na(combusv) == FALSE] = 0
table(hogares$isb_combus)
detach(hogares)
#Indicador de carencia por acceso a los servicios b?sicos en la vivienda;
#Se considera en situaci?n de carencia por servicios b?sicos en la vivienda
#a la poblaci?n que:

#1. Presente carencia en cualquiera de los subindicadores de esta dimensi?n

#No se considera en situaci?n de carencia por  servicios b?sicos en la vivienda
#a la poblaci?n que:

#1. Habite en una vivienda sin carencia en todos los subindicadores
#de esta dimensi?n

attach(hogares)
hogares$ic_sbv = NA
hogares$ic_sbv[(isb_agua == 1 |
                  isb_dren == 1 |
                  isb_luz == 1 | isb_combus == 1)] = 1
hogares$ic_sbv[isb_agua == 0 &
                 isb_dren == 0 & isb_luz == 0 & isb_combus == 0] = 0
hogares$ic_sbv[is.na(isb_agua) == TRUE |
                 is.na(isb_dren) == TRUE |
                 is.na(isb_luz) == TRUE |
                 is.na(isb_combus) == TRUE] = NA

table(hogares$isb_agua)
table(hogares$isb_dren)
table(hogares$isb_luz)
table(hogares$isb_combus)
table(hogares$ic_sbv)
detach(hogares)

attach(hogares)
hogares = hogares[, c("folioviv",
                      "isb_agua",
                      "isb_dren",
                      "isb_luz",
                      "isb_combus",
                      "ic_sbv")]
hogares <- orderBy(~ +folioviv, data = hogares)
write.csv(hogares, 'Bases/ic_sbv16.csv', row.names = FALSE)
write.dbf(hogares, 'Bases/ic_sbv16.dbf')
detach(hogares)

#Parte VI Indicadores de Privaci?n Social:
#INDICADOR DE CARENCIA POR ACCESO A LA ALIMENTACION ----

rm(list = ls())

#Poblaci?n objeto: no se incluye a hu?spedes ni trabajadores dom?sticos
poblacion1 <- read.dbf("Bases de datos/poblacion.dbf", as.is = TRUE)
names(poblacion1) =  tolower(names(poblacion1))
poblacion2 <- as.numeric(poblacion1$parentesco)
index = ((poblacion2 >= 400 &
            poblacion2 < 500) |
           (poblacion2 >= 700 & poblacion2 < 800))
index = (index == FALSE)
poblacion = poblacion1[index,]



#Indicador de hogares con menores de 18 a?os
attach(poblacion)
poblacion$men = NA
poblacion$men[edad >= 0 & edad <= 17] = 1
poblacion$men[edad > 17] = 0
detach(poblacion)

attach(poblacion)
poblacion2 = aggregate(
  x = list(men),
  by = list(folioviv, foliohog),
  FUN = sum,
  na.rm = TRUE
)
detach(poblacion)
names(poblacion2)[1:3] <- c("folioviv", "foliohog",  "men")

attach(poblacion2)
poblacion2$id_men = NA
poblacion2$id_men[men >= 1 & is.na(men) == FALSE] = 1
poblacion2$id_men[men == 0] = 0
detach(poblacion2)

attach(poblacion2)
poblacion2 = poblacion2[, c("folioviv", "foliohog", "id_men")]
poblacion2 <- orderBy(~ +folioviv + foliohog, data = poblacion2)
write.dbf(poblacion2, 'Bases/menores16.dbf')
detach(poblacion2)

hogares <- read.dbf("Bases de datos/hogares.dbf", as.is = TRUE)

#SEIS PREGUNTAS PARA HOGARES SIN POBLACI?N MENOR A 18 A?OS
#Alg?n adulto tuvo una alimentaci?n basada en muy poca variedad de acc_alimentos
attach(hogares)
hogares$ia_1ad = NA
hogares$ia_1ad[acc_alim4 == 1] = 1
hogares$ia_1ad[acc_alim4 == 2] = 0
hogares$ia_1ad[is.na(acc_alim4) == TRUE] = 0

#Alg?n adulto dej? de desayunar, comer o cenar
hogares$ia_2ad = NA
hogares$ia_2ad[acc_alim5 == 1] = 1
hogares$ia_2ad[acc_alim5 == 2] = 0
hogares$ia_2ad[is.na(acc_alim5) == TRUE] = 0

#Alg?n adulto comi? menos de lo que deb?a comer
hogares$ia_3ad = NA
hogares$ia_3ad[acc_alim6 == 1] = 1
hogares$ia_3ad[acc_alim6 == 2] = 0
hogares$ia_3ad[is.na(acc_alim6) == TRUE] = 0

#El hogar se qued? sin comida
hogares$ia_4ad = NA
hogares$ia_4ad[acc_alim2 == 1] = 1
hogares$ia_4ad[acc_alim2 == 2] = 0
hogares$ia_4ad[is.na(acc_alim2) == TRUE] = 0

#Alg?n adulto sinti? hambre pero no comi?
hogares$ia_5ad = NA
hogares$ia_5ad[acc_alim7 == 1] = 1
hogares$ia_5ad[acc_alim7 == 2] = 0
hogares$ia_5ad[is.na(acc_alim7) == TRUE] = 0

#Alg?n adulto solo comi? una vez al d?a o dej? de comer todo un d?a
hogares$ia_6ad = NA
hogares$ia_6ad[acc_alim8 == 1] = 1
hogares$ia_6ad[acc_alim8 == 2] = 0
hogares$ia_6ad[is.na(acc_alim8) == TRUE] = 0

#SEIS PREGUNTAS PARA HOGARES CON POBLACI?N MENOR A 18 A?OS
#Alguien de 0 a 17 a?os tuvo una alimentaci?n basada en muy poca variedad de alimentos
hogares$ia_7men = NA
hogares$ia_7men[acc_alim11 == 1] = 1
hogares$ia_7men[acc_alim11 == 2] = 0
hogares$ia_7men[is.na(acc_alim11) == TRUE] = 0

#Alguien de 0 a 17 a?os comi? menos de lo que deb?a
hogares$ia_8men = NA
hogares$ia_8men[acc_alim12 == 1] = 1
hogares$ia_8men[acc_alim12 == 2] = 0
hogares$ia_8men[is.na(acc_alim12) == TRUE] = 0

#Se tuvo que disminuir la cantidad servida en las comidas a alguien de 0 a 17 a?os
hogares$ia_9men = NA
hogares$ia_9men[acc_alim13 == 1] = 1
hogares$ia_9men[acc_alim13 == 2] = 0
hogares$ia_9men[is.na(acc_alim13) == TRUE] = 0

#Alguien de 0 a 17 a?os sinti? hambre pero no comi?
hogares$ia_10men = NA
hogares$ia_10men[acc_alim14 == 1] = 1
hogares$ia_10men[acc_alim14 == 2] = 0
hogares$ia_10men[is.na(acc_alim14) == TRUE] = 0

#Alguien de 0 a 17 a?os se acost? con hambre
hogares$ia_11men = NA
hogares$ia_11men[acc_alim15 == 1] = 1
hogares$ia_11men[acc_alim15 == 2] = 0
hogares$ia_11men[is.na(acc_alim15) == TRUE] = 0

#Alguien de 0 a 17 a?os comi? una vez al d?a o dej? de comer todo un d?a
hogares$ia_12men = NA
hogares$ia_12men[acc_alim16 == 1] = 1
hogares$ia_12men[acc_alim16 == 2] = 0
hogares$ia_12men[is.na(acc_alim16) == TRUE] = 0
detach(hogares)


#Construcci?n de la escala de inseguridad acc_alimentaria
hogares <- orderBy(~ +folioviv + foliohog, data = hogares)
hogares = merge(hogares,
                poblacion2,
                by = c("folioviv", "foliohog"),
                all.x = TRUE)

attach(hogares)
#Escala para hogares sin menores de 18 a?os
hogares$tot_iaad = NA
hogares$tot_iaad[id_men == 0] = ia_1ad[id_men == 0] + ia_2ad[id_men == 0] + ia_3ad[id_men ==
                                                                                     0] + ia_4ad[id_men == 0] + ia_5ad[id_men == 0] + ia_6ad[id_men == 0]
table(hogares$tot_iaad)

#Escala para hogares con menores de 18 a?os
hogares$tot_iamen = NA
hogares$tot_iamen[id_men == 1] = ia_1ad[id_men == 1] + ia_2ad[id_men == 1] + ia_3ad[id_men ==
                                                                                      1] + ia_4ad[id_men == 1] + ia_5ad[id_men == 1] + ia_6ad[id_men == 1] + ia_7men[id_men ==
                                                                                                                                                                       1] + ia_8men[id_men == 1] + ia_9men[id_men == 1] + ia_10men[id_men == 1] + ia_11men[id_men ==
                                                                                                                                                                                                                                                             1] + ia_12men[id_men == 1]
table(hogares$tot_iamen)
detach(hogares)

####GRADO DE INSEGURIDAD ALIMENTARIA####

#Este subindicador genera cuatro clasificaciones de la siguiente forma:
# 0 Sin inseguridad alimentaria
# 1 Inseguridad alimentaria leve
# 2 Inseguridad alimentaria moderada
# 3 Inseguridad alimentaria severa

attach(hogares)
hogares$ins_ali = NA
hogares$ins_ali[tot_iaad == 0 | tot_iamen == 0] = 0
hogares$ins_ali[(tot_iaad == 1 |
                   tot_iaad == 2) |
                  (tot_iamen == 1 | tot_iamen == 2 | tot_iamen == 3)] =
  1
hogares$ins_ali[(tot_iaad == 3 |
                   tot_iaad == 4) |
                  (tot_iamen == 4 |
                     tot_iamen == 5 | tot_iamen == 6 | tot_iamen == 7)] =
  2
hogares$ins_ali[(tot_iaad == 5 |
                   tot_iaad == 6) |
                  (tot_iamen >= 8 & is.na(tot_iamen) == FALSE)] = 3
table(hogares$ins_ali)
detach(hogares)

#Indicador de carencia por acceso a la alimentaci?n;
#Se considera en situaci?n de carencia por acceso a la alimentaci?n
#a la poblaci?n en hogares que:

#1. Presenten inseguridad alimentaria moderada o severa.

#No se considera en situaci?n de carencia por acceso a la alimentaci?n
#a la poblaci?n que:

#1. No presente inseguridad alimentaria o presente un grado de inseguridad
#alimentaria leve.

attach(hogares)
hogares$ic_ali = 0
hogares$ic_ali[ins_ali == 2 | ins_ali == 3] = 1

table(hogares$ins_ali)
table(hogares$ic_ali)

detach(hogares)

attach(hogares)
hogares = hogares[, c(
  "folioviv",
  "foliohog",
  "id_men" ,
  "ia_1ad" ,
  "ia_2ad" ,
  "ia_3ad" ,
  "ia_4ad" ,
  "ia_5ad",
  "ia_6ad",
  "ia_7men" ,
  "ia_8men" ,
  "ia_9men",
  "ia_10men" ,
  "ia_11men",
  "ia_12men",
  "tot_iaad",
  "tot_iamen",
  "ins_ali",
  "ic_ali"
)]
hogares <- orderBy(~ +folioviv + foliohog, data = hogares)
write.csv(hogares, 'Bases/ic_ali16.csv', row.names = FALSE)
write.dbf(hogares, 'Bases/ic_ali16.dbf')
detach(hogares)


#Parte VII Bienestar (ingresos) ----

#Para la construcci?n del ingreso corriente del hogar es necesario utilizar
#informaci?n sobre la condici?n de ocupaci?n y los ingresos de los individuos.
#Se utiliza la informaci?n contenida en la base "trabajos" para
#identificar a la poblaci?n ocupada que declara tener como prestaci?n laboral aguinaldo,
#ya sea por su trabajo principal o secundario, a fin de incorporar los ingresos por este
#concepto en la medici?n

#Creaci?n del ingreso monetario deflactado a pesos de agosto del 2016.

#Ingresos

rm(list = ls())
trabajos <- read.dbf("Bases de datos/trabajos.dbf", as.is = TRUE)

trabajos = trabajos[, c("folioviv", "foliohog", "numren", "id_trabajo", "pres_8")]

trabajos2 = reshape(
  trabajos,
  idvar = c("folioviv", "foliohog", "numren"),
  timevar = "id_trabajo",
  direction = "wide"
)

names(trabajos2)[4:5] <- c("pres_81", "pres_82")

#Poblaci?n con al menos un empleo
trabajos2$trab = 1

attach(trabajos2)
#Aguinaldo trabajo principal
trabajos2$aguinaldo1 = 0
trabajos2$aguinaldo1[pres_81 == "08"] = 1


#Aguinaldo trabajo secundario
trabajos2$aguinaldo2 = 0
trabajos2$aguinaldo2[pres_82 == "08"] = 1
detach(trabajos2)


attach(trabajos2)
aguinaldo = trabajos2[, c("folioviv",
                          "foliohog",
                          "numren" ,
                          "aguinaldo1" ,
                          "aguinaldo2" ,
                          "trab")]
aguinaldo <-
  orderBy(~ +folioviv + foliohog + numren, data = aguinaldo)
write.dbf(aguinaldo, 'Bases/aguinaldo.dbf')
write.csv(aguinaldo, 'Bases/aguinaldo.csv', row.names = FALSE)
detach(trabajos2)

ingresos <- read.dbf("Bases de datos/ingresos.dbf", as.is = TRUE)
names(ingresos) =  tolower(names(ingresos))
ingresos <-
  orderBy(~ +folioviv + foliohog + numren, data = ingresos)


ingresos3 = merge(
  ingresos,
  aguinaldo,
  by.x = c("folioviv", "foliohog", "numren"),
  all = TRUE
)

index1 = (ingresos3$clave == "P009" &
            ingresos3$aguinaldo1 != 1 &
            is.na(ingresos3$clave) == FALSE)
index2 = (ingresos3$clave == "P016" &
            ingresos3$aguinaldo2 != 1 &
            is.na(ingresos3$clave) == FALSE)
index = (index1 == FALSE & index2 == FALSE)
ingresos3 = ingresos3[index,]

rm(ingresos)

#Una vez realizado lo anterior, se procede a deflactar el ingreso recibido
#por los hogares a precios de agosto de 2016. Para ello se utilizan las
#variables meses, las cuales toman los valores 2 a 10 e indican el mes en
#que se recibi? el ingreso respectivo

ingresos = ingresos3

#Definici?n de los deflactores 2016


dic15	=	0.9915096155
ene16	=	0.9952905552
feb16	=	0.9996486737
mar16	=	1.0011208981
abr16	=	0.9979505968
may16	=	0.9935004643
jun16	=	0.9945962676
jul16	=	0.9971893899
ago16	=	1.0000000000
sep16	=	1.0061063849
oct16	=	1.0122127699
nov16	=	1.0201259756
dic16	=	1.0248270555


ingresos$ing_6 = ingresos$ing_6
ingresos$ing_5 = ingresos$ing_5
ingresos$ing_4 = ingresos$ing_4
ingresos$ing_3 = ingresos$ing_3
ingresos$ing_2 = ingresos$ing_2
ingresos$ing_1 = ingresos$ing_1

mes_6 <- as.numeric(ingresos$mes_6)
mes_5 <- as.numeric(ingresos$mes_5)
mes_4 <- as.numeric(ingresos$mes_4)
mes_3 <- as.numeric(ingresos$mes_3)
mes_2 <- as.numeric(ingresos$mes_2)
mes_1 <- as.numeric(ingresos$mes_1)

ingresos$ing_6[(is.na(mes_6 == 2) != TRUE &
                  mes_6 == 2 &
                  is.na(ingresos$ing_6) != TRUE)] = ingresos$ing_6[(is.na(mes_6 == 2) != TRUE &
                                                                      mes_6 == 2 &
                                                                      is.na(ingresos$ing_6) != TRUE)] / feb16
ingresos$ing_6[(is.na(mes_6 == 3) != TRUE &
                  mes_6 == 3 &
                  is.na(ingresos$ing_6) != TRUE)] = ingresos$ing_6[(is.na(mes_6 == 3) != TRUE &
                                                                      mes_6 == 3 &
                                                                      is.na(ingresos$ing_6) != TRUE)] / mar16
ingresos$ing_6[(is.na(mes_6 == 4) != TRUE &
                  mes_6 == 4 &
                  is.na(ingresos$ing_6) != TRUE)] = ingresos$ing_6[(is.na(mes_6 == 4) != TRUE &
                                                                      mes_6 == 4 &
                                                                      is.na(ingresos$ing_6) != TRUE)] / abr16
ingresos$ing_6[(is.na(mes_6 == 5) != TRUE &
                  mes_6 == 5 &
                  is.na(ingresos$ing_6) != TRUE)] = ingresos$ing_6[(is.na(mes_6 == 5) != TRUE &
                                                                      mes_6 == 5 &
                                                                      is.na(ingresos$ing_6) != TRUE)] / may16
ingresos$ing_6[(is.na(mes_6 == 6) != TRUE &
                  mes_6 == 6 &
                  is.na(ingresos$ing_6) != TRUE)] = ingresos$ing_6[(is.na(mes_6 == 6) != TRUE &
                                                                      mes_6 == 6 &
                                                                      is.na(ingresos$ing_6) != TRUE)] / jun16

ingresos$ing_5[(is.na(mes_5 == 3) != TRUE &
                  mes_5 == 3 &
                  is.na(ingresos$ing_5) != TRUE)] = ingresos$ing_5[(is.na(mes_5 == 3) != TRUE &
                                                                      mes_5 == 3 &
                                                                      is.na(ingresos$ing_5) != TRUE)] / mar16
ingresos$ing_5[(is.na(mes_5 == 4) != TRUE &
                  mes_5 == 4 &
                  is.na(ingresos$ing_5) != TRUE)] = ingresos$ing_5[(is.na(mes_5 == 4) != TRUE &
                                                                      mes_5 == 4 &
                                                                      is.na(ingresos$ing_5) != TRUE)] / abr16
ingresos$ing_5[(is.na(mes_5 == 5) != TRUE &
                  mes_5 == 5 &
                  is.na(ingresos$ing_5) != TRUE)] = ingresos$ing_5[(is.na(mes_5 == 5) != TRUE &
                                                                      mes_5 == 5 &
                                                                      is.na(ingresos$ing_5) != TRUE)] / may16
ingresos$ing_5[(is.na(mes_5 == 6) != TRUE &
                  mes_5 == 6 &
                  is.na(ingresos$ing_5) != TRUE)] = ingresos$ing_5[(is.na(mes_5 == 6) != TRUE &
                                                                      mes_5 == 6 &
                                                                      is.na(ingresos$ing_5) != TRUE)] / jun16
ingresos$ing_5[(is.na(mes_5 == 7) != TRUE &
                  mes_5 == 7 &
                  is.na(ingresos$ing_5) != TRUE)] = ingresos$ing_5[(is.na(mes_5 == 7) != TRUE &
                                                                      mes_5 == 7 &
                                                                      is.na(ingresos$ing_5) != TRUE)] / jul16

ingresos$ing_4[(is.na(mes_4 == 4) != TRUE &
                  mes_4 == 4 &
                  is.na(ingresos$ing_4) != TRUE)] = ingresos$ing_4[(is.na(mes_4 == 4) != TRUE &
                                                                      mes_4 == 4 &
                                                                      is.na(ingresos$ing_4) != TRUE)] / abr16
ingresos$ing_4[(is.na(mes_4 == 5) != TRUE &
                  mes_4 == 5 &
                  is.na(ingresos$ing_4) != TRUE)] = ingresos$ing_4[(is.na(mes_4 == 5) != TRUE &
                                                                      mes_4 == 5 &
                                                                      is.na(ingresos$ing_4) != TRUE)] / may16
ingresos$ing_4[(is.na(mes_4 == 6) != TRUE &
                  mes_4 == 6 &
                  is.na(ingresos$ing_4) != TRUE)] = ingresos$ing_4[(is.na(mes_4 == 6) != TRUE &
                                                                      mes_4 == 6 &
                                                                      is.na(ingresos$ing_4) != TRUE)] / jun16
ingresos$ing_4[(is.na(mes_4 == 7) != TRUE &
                  mes_4 == 7 &
                  is.na(ingresos$ing_4) != TRUE)] = ingresos$ing_4[(is.na(mes_4 == 7) != TRUE &
                                                                      mes_4 == 7 &
                                                                      is.na(ingresos$ing_4) != TRUE)] / jul16
ingresos$ing_4[(is.na(mes_4 == 8) != TRUE &
                  mes_4 == 8 &
                  is.na(ingresos$ing_4) != TRUE)] = ingresos$ing_4[(is.na(mes_4 == 8) != TRUE &
                                                                      mes_4 == 8 &
                                                                      is.na(ingresos$ing_4) != TRUE)] / ago16

ingresos$ing_3[(is.na(mes_3 == 5) != TRUE &
                  mes_3 == 5 &
                  is.na(ingresos$ing_3) != TRUE)] = ingresos$ing_3[(is.na(mes_3 == 5) != TRUE &
                                                                      mes_3 == 5 &
                                                                      is.na(ingresos$ing_3) != TRUE)] / may16
ingresos$ing_3[(is.na(mes_3 == 6) != TRUE &
                  mes_3 == 6 &
                  is.na(ingresos$ing_3) != TRUE)] = ingresos$ing_3[(is.na(mes_3 == 6) != TRUE &
                                                                      mes_3 == 6 &
                                                                      is.na(ingresos$ing_3) != TRUE)] / jun16
ingresos$ing_3[(is.na(mes_3 == 7) != TRUE &
                  mes_3 == 7 &
                  is.na(ingresos$ing_3) != TRUE)] = ingresos$ing_3[(is.na(mes_3 == 7) != TRUE &
                                                                      mes_3 == 7 &
                                                                      is.na(ingresos$ing_3) != TRUE)] / jul16
ingresos$ing_3[(is.na(mes_3 == 8) != TRUE &
                  mes_3 == 8 &
                  is.na(ingresos$ing_3) != TRUE)] = ingresos$ing_3[(is.na(mes_3 == 8) != TRUE &
                                                                      mes_3 == 8 &
                                                                      is.na(ingresos$ing_3) != TRUE)] / ago16
ingresos$ing_3[(is.na(mes_3 == 9) != TRUE &
                  mes_3 == 9 &
                  is.na(ingresos$ing_3) != TRUE)] = ingresos$ing_3[(is.na(mes_3 == 9) != TRUE &
                                                                      mes_3 == 9 &
                                                                      is.na(ingresos$ing_3) != TRUE)] / sep16

ingresos$ing_2[(is.na(mes_2 == 6) != TRUE &
                  mes_2 == 6 &
                  is.na(ingresos$ing_2) != TRUE)] = ingresos$ing_2[(is.na(mes_2 == 6) != TRUE &
                                                                      mes_2 == 6 &
                                                                      is.na(ingresos$ing_2) != TRUE)] / jun16
ingresos$ing_2[(is.na(mes_2 == 7) != TRUE &
                  mes_2 == 7 &
                  is.na(ingresos$ing_2) != TRUE)] = ingresos$ing_2[(is.na(mes_2 == 7) != TRUE &
                                                                      mes_2 == 7 &
                                                                      is.na(ingresos$ing_2) != TRUE)] / jul16
ingresos$ing_2[(is.na(mes_2 == 8) != TRUE &
                  mes_2 == 8 &
                  is.na(ingresos$ing_2) != TRUE)] = ingresos$ing_2[(is.na(mes_2 == 8) != TRUE &
                                                                      mes_2 == 8 &
                                                                      is.na(ingresos$ing_2) != TRUE)] / ago16
ingresos$ing_2[(is.na(mes_2 == 9) != TRUE &
                  mes_2 == 9 &
                  is.na(ingresos$ing_2) != TRUE)] = ingresos$ing_2[(is.na(mes_2 == 9) != TRUE &
                                                                      mes_2 == 9 &
                                                                      is.na(ingresos$ing_2) != TRUE)] / sep16
ingresos$ing_2[(is.na(mes_2 == 10) != TRUE &
                  mes_2 == 10 &
                  is.na(ingresos$ing_2) != TRUE)] = ingresos$ing_2[(is.na(mes_2 == 10) !=
                                                                      TRUE &
                                                                      mes_2 == 10 & is.na(ingresos$ing_2) != TRUE)] / oct16

ingresos$ing_1[(is.na(mes_1 == 7) != TRUE &
                  mes_1 == 7 &
                  is.na(ingresos$ing_1) != TRUE)] = ingresos$ing_1[(is.na(mes_1 == 7) != TRUE &
                                                                      mes_1 == 7 &
                                                                      is.na(ingresos$ing_1) != TRUE)] / jul16
ingresos$ing_1[(is.na(mes_1 == 8) != TRUE &
                  mes_1 == 8 &
                  is.na(ingresos$ing_1) != TRUE)] = ingresos$ing_1[(is.na(mes_1 == 8) != TRUE &
                                                                      mes_1 == 8 &
                                                                      is.na(ingresos$ing_1) != TRUE)] / ago16
ingresos$ing_1[(is.na(mes_1 == 9) != TRUE &
                  mes_1 == 9 &
                  is.na(ingresos$ing_1) != TRUE)] = ingresos$ing_1[(is.na(mes_1 == 9) != TRUE &
                                                                      mes_1 == 9 &
                                                                      is.na(ingresos$ing_1) != TRUE)] / sep16
ingresos$ing_1[(is.na(mes_1 == 10) != TRUE &
                  mes_1 == 10 &
                  is.na(ingresos$ing_1) != TRUE)] = ingresos$ing_1[(is.na(mes_1 == 10) !=
                                                                      TRUE &
                                                                      mes_1 == 10 & is.na(ingresos$ing_1) != TRUE)] / oct16
ingresos$ing_1[(is.na(mes_1 == 11) != TRUE &
                  mes_1 == 11 &
                  is.na(ingresos$ing_1) != TRUE)] = ingresos$ing_1[(is.na(mes_1 == 11) !=
                                                                      TRUE &
                                                                      mes_1 == 11 & is.na(ingresos$ing_1) != TRUE)] / nov16

#Se deflactan las claves P008 y P015 (Reparto de utilidades) y P009 y P016 (aguinaldo)
#con los deflactores de mayo a agosto 2016 y de dicembre de 2015 a agosto 2016,
#respectivamente, y se obtiene el promedio mensual.

ingresos$ing_1[is.na(ingresos$ing_1) != TRUE &
                 ingresos$clave == "P008"] = ingresos$ing_1[is.na(ingresos$ing_1) != TRUE &
                                                              ingresos$clave == "P008"] / may16 / 12
ingresos$ing_1[is.na(ingresos$ing_1) != TRUE &
                 ingresos$clave == "P015"] = ingresos$ing_1[is.na(ingresos$ing_1) != TRUE &
                                                              ingresos$clave == "P015"] / may16 / 12

ingresos$ing_1[is.na(ingresos$ing_1) != TRUE &
                 ingresos$clave == "P009"] = ingresos$ing_1[is.na(ingresos$ing_1) != TRUE &
                                                              ingresos$clave == "P009"] / dic15 / 12
ingresos$ing_1[is.na(ingresos$ing_1) != TRUE &
                 ingresos$clave == "P016"] = ingresos$ing_1[is.na(ingresos$ing_1) != TRUE &
                                                              ingresos$clave == "P016"] / dic15 / 12


#Una vez realizada la deflactaci?n, se procede a obtener el ingreso mensual promedio en los ?ltimos seis meses,
#para cada persona y clave de ingreso
ingresos$ing_mens = rowMeans(
  data.frame(
    ingresos$ing_1,
    ingresos$ing_2,
    ingresos$ing_3,
    ingresos$ing_4,
    ingresos$ing_5,
    ingresos$ing_6
  ),
  na.rm = TRUE
)
#sum(ingresos$ing_mens, na.rm=TRUE)

#Para obtener el ingreso corriente monetario, se seleccionan las claves de ingreso correspondientes
attach(ingresos)

for (i in 1:9) {
  ing_mon = 1000 + i
  string = paste("P", substr(as.character(ing_mon), 2, 4),  sep = "")
  ingresos$ing_mon[(clave == string &
                      is.na(clave) != TRUE)] = ingresos$ing_mens[(clave == string &
                                                                    is.na(clave) != TRUE)]
}

for (i in 11:16) {
  ing_mon = 1000 + i
  string = paste("P", substr(as.character(ing_mon), 2, 4),  sep = "")
  ingresos$ing_mon[(clave == string &
                      is.na(clave) != TRUE)] = ingresos$ing_mens[(clave == string &
                                                                    is.na(clave) != TRUE)]
}

for (i in 18:48) {
  ing_mon = 1000 + i
  string = paste("P", substr(as.character(ing_mon), 2, 4),  sep = "")
  ingresos$ing_mon[(clave == string &
                      is.na(clave) != TRUE)] = ingresos$ing_mens[(clave == string &
                                                                    is.na(clave) != TRUE)]
}

for (i in 67:81) {
  ing_mon = 1000 + i
  string = paste("P", substr(as.character(ing_mon), 2, 4),  sep = "")
  ingresos$ing_mon[(clave == string &
                      is.na(clave) != TRUE)] = ingresos$ing_mens[(clave == string &
                                                                    is.na(clave) != TRUE)]
}

#ingreso corriente monetario del hogar
ingresos$ing_mon1 = as.numeric(ingresos$ing_mon)

detach(ingresos)

#sum(ingresos$ing_mon, na.rm=TRUE)

ingresos = data.frame(ingresos)
ingresos = ingresos[, c("folioviv", "foliohog", "ing_mon1")]


attach(ingresos)
ingresos2 = aggregate(
  x = list(ing_mon1),
  by = list(folioviv, foliohog),
  FUN = sum,
  na.rm = TRUE
)
detach(ingresos)

names(ingresos2)[1:3] <- c("folioviv", "foliohog", "ing_mon")
mean(ingresos2$ing_mon)

ingresos2 <- orderBy(~ +folioviv + foliohog, data = ingresos2)
write.csv(ingresos2, 'Bases/ingreso_deflactado16.csv', row.names = FALSE)
write.dbf(ingresos2, 'Bases/ingreso_deflactado16.dbf')

#Creaci?n del ingreso no monetario deflactado a pesos de agosto del 2016.

#No Monetario

nomonetariopersona <-
  read.dbf("Bases de datos/gastospersona.dbf", as.is = TRUE)
names(nomonetariopersona) =  tolower(names(nomonetariopersona))
names(nomonetariopersona)[7] <- "frecuenciap"

nomonetarioper <-
  orderBy(~ +folioviv + foliohog, data = nomonetariopersona)


nomonetariohogar <-
  read.dbf("Bases de datos/gastoshogar.dbf", as.is = TRUE)
names(nomonetariohogar) =  tolower(names(nomonetariohogar))
nomonetariohog   <-
  orderBy(~ +folioviv + foliohog, data = nomonetariohogar)
nomonetariohog   <-
  data.frame(
    nomonetariohog,
    numren = NA,
    origen_rem = NA,
    inst = NA
  )



#En el caso de la informaci?n de gasto no monetario, para
#deflactar se utiliza la decena de levantamiento de la
#encuesta, la cual se encuentra en la octava posici?n del
#folio de la vivienda. En primer lugar se obtiene una variable que
#identifique la decena de levantamiento

nomonetario <- merge(nomonetariohog, nomonetarioper, all = TRUE)
nomonetario$decena = substr(nomonetario$folioviv, 8, 8)
nomonetario$decena = as.numeric(nomonetario$decena)

#Definici?n de los deflactores

#Rubro 1.1 semanal, Alimentos
d11w07 =	0.9985457696
d11w08 =	1.0000000000
d11w09 =	1.0167932672
d11w10 =	1.0199415214
d11w11 =	1.0251086805

#Rubro 1.2 semanal, Bebidas alcoh?licas y tabaco
d12w07 =	0.9959845820
d12w08 =	1.0000000000
d12w09 =	1.0066744829
d12w10 =	1.0087894741
d12w11 =	1.0100998490

#Rubro 2 trimestral, Vestido, calzado y accesorios
d2t05 =	0.9920067602
d2t06 =	0.9948005139
d2t07 =	0.9986462366
d2t08 =	1.0053546946

#Rubro 3 mensual, Vivienda
d3m07 =	1.0017314941
d3m08 =	1.0000000000
d3m09 =	0.9978188915
d3m10 =	1.0133832055
d3m11 =	1.0358543632

#Rubro 4.2 mensual, Accesorios y art?culos de limpieza para el hogar
d42m07 =	0.9936894797
d42m08 =	1.0000000000
d42m09 =	1.0041605121
d42m10 =	1.0056376169
d42m11 =	1.0087477433

#Rubro 4.2 trimestral, Accesorios y art?culos de limpieza para el hogar
d42t05 =	0.9932545544
d42t06 =	0.9960501122
d42t07 =	0.9992833306
d42t08 =	1.0032660430

#Rubro 4.1 semestral, Muebles y aparatos d?mesticos
d41s02 =	1.0081456317
d41s03 =	1.0057381027
d41s04 =	1.0038444337
d41s05 =	1.0025359940

#Rubro 5.1 trimestral, Salud
d51t05 =	0.9948500567
d51t06 =	0.9974422922
d51t07 =	1.0000318717
d51t08 =	1.0028179937

#Rubro 6.1.1 semanal, Transporte p?blico urbano
d611w07 =	0.9998162514
d611w08 =	1.0000000000
d611w09 =	1.0010465683
d611w10 =	1.0030038907
d611w11 =	1.0040584480

#Rubro 6 mensual, Transporte
d6m07 =	0.9907765708
d6m08 =	1.0000000000
d6m09 =	1.0049108739
d6m10 =	1.0097440440
d6m11 =	1.0137147031

#Rubro 6 semestral, Transporte
d6s02 =	0.9749314912
d6s03 =	0.9796636466
d6s04 =	0.9851637735
d6s05 =	0.9917996695

#Rubro 7 mensual, Educaci?n y esparcimiento
d7m07 =	0.9997765641
d7m08 =	1.0000000000
d7m09 =	1.0128930818
d7m10 =	1.0131744455
d7m11 =	1.0158805031

#Rubro 2.3 mensual, Accesorios y cuidados del vestido
d23m07 =	0.9923456541
d23m08 =	1.0000000000
d23m09 =	1.0029207372
d23m10 =	1.0029710948
d23m11 =	1.0057155806

#Rubro 2.3 trimestral,  Accesorios y cuidados del vestido
d23t05 =	0.9913748727
d23t06 =	0.9950229966
d23t07 =	0.9984221305
d23t08 =	1.0019639440

#INPC semestral
dINPCs02 = 0.9973343817
dINPCs03 = 0.9973929361
dINPCs04 = 0.9982238506
dINPCs05 = 1.0006008794

#Una vez definidos los deflactores, se seleccionan los rubros
nomonetario$gasnomon = nomonetario$gas_nm_tri / 3
nomonetario$gasnomon[is.na(nomonetario$gasnomon) == TRUE] = 0


nomonetario$esp[nomonetario$tipo_gasto == "G4"] = 1
nomonetario$reg[nomonetario$tipo_gasto == "G5"] = 1
nomonetario$reg[nomonetario$tipo_gasto == "G6"] = 1

index1 = nomonetario$tipo_gasto == "G2"
index2 = nomonetario$tipo_gasto == "G3"
index3 = nomonetario$tipo_gasto == "G7"
indext = (index1 == FALSE & index2 == FALSE & index3 == FALSE)
nomonetario = nomonetario[indext,]


#Control para la frecuencia de los regalos recibidos, para no monetario ENIGH

index1 = ((
  nomonetario$frecuenciap == "9" |
    nomonetario$frecuenciap == "NA" |
    nomonetario$frecuenciap == "0"
) &  nomonetario$tipo_gasto == "G5"
)
index1[is.na(index1) == TRUE] = FALSE
index2 = ((
  nomonetario$frecuencia == "5" |
    nomonetario$frecuencia == "6" |
    nomonetario$frecuencia == "0" |
    nomonetario$frecuencia == "NA"
) & nomonetario$tipo_gasto == "G5"
)
index2[is.na(index2) == TRUE] = FALSE
indext = (index1 == FALSE & index2 == FALSE)
nomonetario = nomonetario[indext,]


attach(nomonetario)

#Gasto en acc_alimentos deflactado (semanal)

for (i in 1:222) {
  gasto_ali = 1000 + i
  string = paste("A", substr(as.character(gasto_ali), 2, 4),  sep = "")
  nomonetario$ali_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}

for (i in 242:247) {
  gas_ali = 1000 + i
  string = paste("A", substr(as.character(gas_ali), 2, 4),  sep = "")
  nomonetario$ali_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
nomonetario$ali_nm[(decena == 1 &
                      is.na(decena) != TRUE)] = nomonetario$ali_nm[(decena == 1 &
                                                                      is.na(decena) != TRUE)] / d11w08
nomonetario$ali_nm[(decena == 2 &
                      is.na(decena) != TRUE)] = nomonetario$ali_nm[(decena == 2 &
                                                                      is.na(decena) != TRUE)] / d11w08
nomonetario$ali_nm[(decena == 3 &
                      is.na(decena) != TRUE)] = nomonetario$ali_nm[(decena == 3 &
                                                                      is.na(decena) != TRUE)] / d11w08
nomonetario$ali_nm[(decena == 4 &
                      is.na(decena) != TRUE)] = nomonetario$ali_nm[(decena == 4 &
                                                                      is.na(decena) != TRUE)] / d11w09
nomonetario$ali_nm[(decena == 5 &
                      is.na(decena) != TRUE)] = nomonetario$ali_nm[(decena == 5 &
                                                                      is.na(decena) != TRUE)] / d11w09
nomonetario$ali_nm[(decena == 6 &
                      is.na(decena) != TRUE)] = nomonetario$ali_nm[(decena == 6 &
                                                                      is.na(decena) != TRUE)] / d11w09
nomonetario$ali_nm[(decena == 7 &
                      is.na(decena) != TRUE)] = nomonetario$ali_nm[(decena == 7 &
                                                                      is.na(decena) != TRUE)] / d11w10
nomonetario$ali_nm[(decena == 8 &
                      is.na(decena) != TRUE)] = nomonetario$ali_nm[(decena == 8 &
                                                                      is.na(decena) != TRUE)] / d11w10
nomonetario$ali_nm[(decena == 9 &
                      is.na(decena) != TRUE)] = nomonetario$ali_nm[(decena == 9 &
                                                                      is.na(decena) != TRUE)] / d11w10
nomonetario$ali_nm[(decena == 0 &
                      is.na(decena) != TRUE)] = nomonetario$ali_nm[(decena == 0 &
                                                                      is.na(decena) != TRUE)] / d11w11

#Gasto en Alcohol y tabaco deflactado (semanal)

for (i in 223:241) {
  gas_alctab = 1000 + i
  string = paste("A", substr(as.character(gas_alctab), 2, 4),  sep = "")
  nomonetario$alta_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
nomonetario$alta_nm[(decena == 1 &
                       is.na(decena) != TRUE)] = nomonetario$alta_nm[(decena == 1 &
                                                                        is.na(decena) != TRUE)] / d12w08
nomonetario$alta_nm[(decena == 2 &
                       is.na(decena) != TRUE)] = nomonetario$alta_nm[(decena == 2 &
                                                                        is.na(decena) != TRUE)] / d12w08
nomonetario$alta_nm[(decena == 3 &
                       is.na(decena) != TRUE)] = nomonetario$alta_nm[(decena == 3 &
                                                                        is.na(decena) != TRUE)] / d12w08
nomonetario$alta_nm[(decena == 4 &
                       is.na(decena) != TRUE)] = nomonetario$alta_nm[(decena == 4 &
                                                                        is.na(decena) != TRUE)] / d12w09
nomonetario$alta_nm[(decena == 5 &
                       is.na(decena) != TRUE)] = nomonetario$alta_nm[(decena == 5 &
                                                                        is.na(decena) != TRUE)] / d12w09
nomonetario$alta_nm[(decena == 6 &
                       is.na(decena) != TRUE)] = nomonetario$alta_nm[(decena == 6 &
                                                                        is.na(decena) != TRUE)] / d12w09
nomonetario$alta_nm[(decena == 7 &
                       is.na(decena) != TRUE)] = nomonetario$alta_nm[(decena == 7 &
                                                                        is.na(decena) != TRUE)] / d12w10
nomonetario$alta_nm[(decena == 8 &
                       is.na(decena) != TRUE)] = nomonetario$alta_nm[(decena == 8 &
                                                                        is.na(decena) != TRUE)] / d12w10
nomonetario$alta_nm[(decena == 9 &
                       is.na(decena) != TRUE)] = nomonetario$alta_nm[(decena == 9 &
                                                                        is.na(decena) != TRUE)] / d12w10
nomonetario$alta_nm[(decena == 0 &
                       is.na(decena) != TRUE)] = nomonetario$alta_nm[(decena == 0 &
                                                                        is.na(decena) != TRUE)] / d12w11

#Gasto en Vestido y calzado deflactado (trimestral)

for (i in 1:122) {
  gas_vescal = 1000 + i
  string = paste("H", substr(as.character(gas_vescal), 2, 4),  sep = "")
  nomonetario$veca_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
nomonetario$veca_nm[(clave == "H136" &
                       is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == "H136" &
                                                                        is.na(clave) != TRUE)]

nomonetario$veca_nm[(decena == 1 &
                       is.na(decena) != TRUE)] = nomonetario$veca_nm[(decena == 1 &
                                                                        is.na(decena) != TRUE)] / d2t05
nomonetario$veca_nm[(decena == 2 &
                       is.na(decena) != TRUE)] = nomonetario$veca_nm[(decena == 2 &
                                                                        is.na(decena) != TRUE)] / d2t05
nomonetario$veca_nm[(decena == 3 &
                       is.na(decena) != TRUE)] = nomonetario$veca_nm[(decena == 3 &
                                                                        is.na(decena) != TRUE)] / d2t06
nomonetario$veca_nm[(decena == 4 &
                       is.na(decena) != TRUE)] = nomonetario$veca_nm[(decena == 4 &
                                                                        is.na(decena) != TRUE)] / d2t06
nomonetario$veca_nm[(decena == 5 &
                       is.na(decena) != TRUE)] = nomonetario$veca_nm[(decena == 5 &
                                                                        is.na(decena) != TRUE)] / d2t06
nomonetario$veca_nm[(decena == 6 &
                       is.na(decena) != TRUE)] = nomonetario$veca_nm[(decena == 6 &
                                                                        is.na(decena) != TRUE)] / d2t07
nomonetario$veca_nm[(decena == 7 &
                       is.na(decena) != TRUE)] = nomonetario$veca_nm[(decena == 7 &
                                                                        is.na(decena) != TRUE)] / d2t07
nomonetario$veca_nm[(decena == 8 &
                       is.na(decena) != TRUE)] = nomonetario$veca_nm[(decena == 8 &
                                                                        is.na(decena) != TRUE)] / d2t07
nomonetario$veca_nm[(decena == 9 &
                       is.na(decena) != TRUE)] = nomonetario$veca_nm[(decena == 9 &
                                                                        is.na(decena) != TRUE)] / d2t08
nomonetario$veca_nm[(decena == 0 &
                       is.na(decena) != TRUE)] = nomonetario$veca_nm[(decena == 0 &
                                                                        is.na(decena) != TRUE)] / d2t08

#Gasto en Vivienda y servicios de conservaci?n deflactado (mensual)

for (i in 1:16) {
  gas_viv = 1000 + i
  string = paste("G", substr(as.character(gas_viv), 2, 4),  sep = "")
  nomonetario$viv_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
for (i in 1:4) {
  gas_viv = 1000 + i
  string = paste("R", substr(as.character(gas_viv), 2, 4),  sep = "")
  nomonetario$viv_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
nomonetario$viv_nm[(clave == "R013" &
                      is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == "R013" &
                                                                       is.na(clave) != TRUE)]

nomonetario$viv_nm[(decena == 1 &
                      is.na(decena) != TRUE)] = nomonetario$viv_nm[(decena == 1 &
                                                                      is.na(decena) != TRUE)] / d3m07
nomonetario$viv_nm[(decena == 2 &
                      is.na(decena) != TRUE)] = nomonetario$viv_nm[(decena == 2 &
                                                                      is.na(decena) != TRUE)] / d3m07
nomonetario$viv_nm[(decena == 3 &
                      is.na(decena) != TRUE)] = nomonetario$viv_nm[(decena == 3 &
                                                                      is.na(decena) != TRUE)] / d3m08
nomonetario$viv_nm[(decena == 4 &
                      is.na(decena) != TRUE)] = nomonetario$viv_nm[(decena == 4 &
                                                                      is.na(decena) != TRUE)] / d3m08
nomonetario$viv_nm[(decena == 5 &
                      is.na(decena) != TRUE)] = nomonetario$viv_nm[(decena == 5 &
                                                                      is.na(decena) != TRUE)] / d3m08
nomonetario$viv_nm[(decena == 6 &
                      is.na(decena) != TRUE)] = nomonetario$viv_nm[(decena == 6 &
                                                                      is.na(decena) != TRUE)] / d3m09
nomonetario$viv_nm[(decena == 7 &
                      is.na(decena) != TRUE)] = nomonetario$viv_nm[(decena == 7 &
                                                                      is.na(decena) != TRUE)] / d3m09
nomonetario$viv_nm[(decena == 8 &
                      is.na(decena) != TRUE)] = nomonetario$viv_nm[(decena == 8 &
                                                                      is.na(decena) != TRUE)] / d3m09
nomonetario$viv_nm[(decena == 9 &
                      is.na(decena) != TRUE)] = nomonetario$viv_nm[(decena == 9 &
                                                                      is.na(decena) != TRUE)] / d3m10
nomonetario$viv_nm[(decena == 0 &
                      is.na(decena) != TRUE)] = nomonetario$viv_nm[(decena == 0 &
                                                                      is.na(decena) != TRUE)] / d3m10

#Gasto en Art?culos de limpieza deflactado (mensual)

for (i in 1:24) {
  gas_limp = 1000 + i
  string = paste("C", substr(as.character(gas_limp), 2, 4),  sep = "")
  nomonetario$lim_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
nomonetario$lim_nm[(decena == 1 &
                      is.na(decena) != TRUE)] = nomonetario$lim_nm[(decena == 1 &
                                                                      is.na(decena) != TRUE)] / d42m07
nomonetario$lim_nm[(decena == 2 &
                      is.na(decena) != TRUE)] = nomonetario$lim_nm[(decena == 2 &
                                                                      is.na(decena) != TRUE)] / d42m07
nomonetario$lim_nm[(decena == 3 &
                      is.na(decena) != TRUE)] = nomonetario$lim_nm[(decena == 3 &
                                                                      is.na(decena) != TRUE)] / d42m08
nomonetario$lim_nm[(decena == 4 &
                      is.na(decena) != TRUE)] = nomonetario$lim_nm[(decena == 4 &
                                                                      is.na(decena) != TRUE)] / d42m08
nomonetario$lim_nm[(decena == 5 &
                      is.na(decena) != TRUE)] = nomonetario$lim_nm[(decena == 5 &
                                                                      is.na(decena) != TRUE)] / d42m08
nomonetario$lim_nm[(decena == 6 &
                      is.na(decena) != TRUE)] = nomonetario$lim_nm[(decena == 6 &
                                                                      is.na(decena) != TRUE)] / d42m09
nomonetario$lim_nm[(decena == 7 &
                      is.na(decena) != TRUE)] = nomonetario$lim_nm[(decena == 7 &
                                                                      is.na(decena) != TRUE)] / d42m09
nomonetario$lim_nm[(decena == 8 &
                      is.na(decena) != TRUE)] = nomonetario$lim_nm[(decena == 8 &
                                                                      is.na(decena) != TRUE)] / d42m09
nomonetario$lim_nm[(decena == 9 &
                      is.na(decena) != TRUE)] = nomonetario$lim_nm[(decena == 9 &
                                                                      is.na(decena) != TRUE)] / d42m10
nomonetario$lim_nm[(decena == 0 &
                      is.na(decena) != TRUE)] = nomonetario$lim_nm[(decena == 0 &
                                                                      is.na(decena) != TRUE)] / d42m10

#Gasto en Cristaler?a y blancos deflactado (trimestral)

for (i in 1:26) {
  gas_cris = 1000 + i
  string = paste("I", substr(as.character(gas_cris), 2, 4),  sep = "")
  nomonetario$cris_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
nomonetario$cris_nm[(decena == 1 &
                       is.na(decena) != TRUE)] = nomonetario$cris_nm[(decena == 1 &
                                                                        is.na(decena) != TRUE)] / d42t05
nomonetario$cris_nm[(decena == 2 &
                       is.na(decena) != TRUE)] = nomonetario$cris_nm[(decena == 2 &
                                                                        is.na(decena) != TRUE)] / d42t05
nomonetario$cris_nm[(decena == 3 &
                       is.na(decena) != TRUE)] = nomonetario$cris_nm[(decena == 3 &
                                                                        is.na(decena) != TRUE)] / d42t06
nomonetario$cris_nm[(decena == 4 &
                       is.na(decena) != TRUE)] = nomonetario$cris_nm[(decena == 4 &
                                                                        is.na(decena) != TRUE)] / d42t06
nomonetario$cris_nm[(decena == 5 &
                       is.na(decena) != TRUE)] = nomonetario$cris_nm[(decena == 5 &
                                                                        is.na(decena) != TRUE)] / d42t06
nomonetario$cris_nm[(decena == 6 &
                       is.na(decena) != TRUE)] = nomonetario$cris_nm[(decena == 6 &
                                                                        is.na(decena) != TRUE)] / d42t07
nomonetario$cris_nm[(decena == 7 &
                       is.na(decena) != TRUE)] = nomonetario$cris_nm[(decena == 7 &
                                                                        is.na(decena) != TRUE)] / d42t07
nomonetario$cris_nm[(decena == 8 &
                       is.na(decena) != TRUE)] = nomonetario$cris_nm[(decena == 8 &
                                                                        is.na(decena) != TRUE)] / d42t07
nomonetario$cris_nm[(decena == 9 &
                       is.na(decena) != TRUE)] = nomonetario$cris_nm[(decena == 9 &
                                                                        is.na(decena) != TRUE)] / d42t08
nomonetario$cris_nm[(decena == 0 &
                       is.na(decena) != TRUE)] = nomonetario$cris_nm[(decena == 0 &
                                                                        is.na(decena) != TRUE)] / d42t08

#Gasto en Enseres dom?sticos y muebles deflactado (semestral)

for (i in 1:37) {
  gas_endom = 1000 + i
  string = paste("K", substr(as.character(gas_endom), 2, 4),  sep = "")
  nomonetario$ens_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
nomonetario$ens_nm[(decena == 1 &
                      is.na(decena) != TRUE)] = nomonetario$ens_nm[(decena == 1 &
                                                                      is.na(decena) != TRUE)] / d41s02
nomonetario$ens_nm[(decena == 2 &
                      is.na(decena) != TRUE)] = nomonetario$ens_nm[(decena == 2 &
                                                                      is.na(decena) != TRUE)] / d41s02
nomonetario$ens_nm[(decena == 3 &
                      is.na(decena) != TRUE)] = nomonetario$ens_nm[(decena == 3 &
                                                                      is.na(decena) != TRUE)] / d41s03
nomonetario$ens_nm[(decena == 4 &
                      is.na(decena) != TRUE)] = nomonetario$ens_nm[(decena == 4 &
                                                                      is.na(decena) != TRUE)] / d41s03
nomonetario$ens_nm[(decena == 5 &
                      is.na(decena) != TRUE)] = nomonetario$ens_nm[(decena == 5 &
                                                                      is.na(decena) != TRUE)] / d41s03
nomonetario$ens_nm[(decena == 6 &
                      is.na(decena) != TRUE)] = nomonetario$ens_nm[(decena == 6 &
                                                                      is.na(decena) != TRUE)] / d41s04
nomonetario$ens_nm[(decena == 7 &
                      is.na(decena) != TRUE)] = nomonetario$ens_nm[(decena == 7 &
                                                                      is.na(decena) != TRUE)] / d41s04
nomonetario$ens_nm[(decena == 8 &
                      is.na(decena) != TRUE)] = nomonetario$ens_nm[(decena == 8 &
                                                                      is.na(decena) != TRUE)] / d41s04
nomonetario$ens_nm[(decena == 9 &
                      is.na(decena) != TRUE)] = nomonetario$ens_nm[(decena == 9 &
                                                                      is.na(decena) != TRUE)] / d41s05
nomonetario$ens_nm[(decena == 0 &
                      is.na(decena) != TRUE)] = nomonetario$ens_nm[(decena == 0 &
                                                                      is.na(decena) != TRUE)] / d41s05

#Gasto en Salud deflactado (trimestral)

for (i in 1:72) {
  gas_salud = 1000 + i
  string = paste("J", substr(as.character(gas_salud), 2, 4),  sep = "")
  nomonetario$sal_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
nomonetario$sal_nm[(decena == 1 &
                      is.na(decena) != TRUE)] = nomonetario$sal_nm[(decena == 1 &
                                                                      is.na(decena) != TRUE)] / d51t05
nomonetario$sal_nm[(decena == 2 &
                      is.na(decena) != TRUE)] = nomonetario$sal_nm[(decena == 2 &
                                                                      is.na(decena) != TRUE)] / d51t05
nomonetario$sal_nm[(decena == 3 &
                      is.na(decena) != TRUE)] = nomonetario$sal_nm[(decena == 3 &
                                                                      is.na(decena) != TRUE)] / d51t06
nomonetario$sal_nm[(decena == 4 &
                      is.na(decena) != TRUE)] = nomonetario$sal_nm[(decena == 4 &
                                                                      is.na(decena) != TRUE)] / d51t06
nomonetario$sal_nm[(decena == 5 &
                      is.na(decena) != TRUE)] = nomonetario$sal_nm[(decena == 5 &
                                                                      is.na(decena) != TRUE)] / d51t06
nomonetario$sal_nm[(decena == 6 &
                      is.na(decena) != TRUE)] = nomonetario$sal_nm[(decena == 6 &
                                                                      is.na(decena) != TRUE)] / d51t07
nomonetario$sal_nm[(decena == 7 &
                      is.na(decena) != TRUE)] = nomonetario$sal_nm[(decena == 7 &
                                                                      is.na(decena) != TRUE)] / d51t07
nomonetario$sal_nm[(decena == 8 &
                      is.na(decena) != TRUE)] = nomonetario$sal_nm[(decena == 8 &
                                                                      is.na(decena) != TRUE)] / d51t07
nomonetario$sal_nm[(decena == 9 &
                      is.na(decena) != TRUE)] = nomonetario$sal_nm[(decena == 9 &
                                                                      is.na(decena) != TRUE)] / d51t08
nomonetario$sal_nm[(decena == 0 &
                      is.na(decena) != TRUE)] = nomonetario$sal_nm[(decena == 0 &
                                                                      is.na(decena) != TRUE)] / d51t08

#Gasto en Transporte p?blico deflactado (semanal)

for (i in 1:7) {
  gas_transpub = 1000 + i
  string = paste("B", substr(as.character(gas_transpub), 2, 4),  sep = "")
  nomonetario$tpub_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
nomonetario$tpub_nm[(decena == 1 &
                       is.na(decena) != TRUE)] = nomonetario$tpub_nm[(decena == 1 &
                                                                        is.na(decena) != TRUE)] / d611w08
nomonetario$tpub_nm[(decena == 2 &
                       is.na(decena) != TRUE)] = nomonetario$tpub_nm[(decena == 2 &
                                                                        is.na(decena) != TRUE)] / d611w08
nomonetario$tpub_nm[(decena == 3 &
                       is.na(decena) != TRUE)] = nomonetario$tpub_nm[(decena == 3 &
                                                                        is.na(decena) != TRUE)] / d611w08
nomonetario$tpub_nm[(decena == 4 &
                       is.na(decena) != TRUE)] = nomonetario$tpub_nm[(decena == 4 &
                                                                        is.na(decena) != TRUE)] / d611w09
nomonetario$tpub_nm[(decena == 5 &
                       is.na(decena) != TRUE)] = nomonetario$tpub_nm[(decena == 5 &
                                                                        is.na(decena) != TRUE)] / d611w09
nomonetario$tpub_nm[(decena == 6 &
                       is.na(decena) != TRUE)] = nomonetario$tpub_nm[(decena == 6 &
                                                                        is.na(decena) != TRUE)] / d611w09
nomonetario$tpub_nm[(decena == 7 &
                       is.na(decena) != TRUE)] = nomonetario$tpub_nm[(decena == 7 &
                                                                        is.na(decena) != TRUE)] / d611w10
nomonetario$tpub_nm[(decena == 8 &
                       is.na(decena) != TRUE)] = nomonetario$tpub_nm[(decena == 8 &
                                                                        is.na(decena) != TRUE)] / d611w10
nomonetario$tpub_nm[(decena == 9 &
                       is.na(decena) != TRUE)] = nomonetario$tpub_nm[(decena == 9 &
                                                                        is.na(decena) != TRUE)] / d611w10
nomonetario$tpub_nm[(decena == 0 &
                       is.na(decena) != TRUE)] = nomonetario$tpub_nm[(decena == 0 &
                                                                        is.na(decena) != TRUE)] / d611w11

#Gasto en Transporte for?neo deflactado (semestral)

for (i in 1:18) {
  gas_transfor = 1000 + i
  string = paste("M", substr(as.character(gas_transfor), 2, 4),  sep = "")
  nomonetario$tfor_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
for (i in 7:14) {
  gas_transfor = 1000 + i
  string = paste("F", substr(as.character(gas_transfor), 2, 4),  sep = "")
  nomonetario$tfor_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
nomonetario$tfor_nm[(decena == 1 &
                       is.na(decena) != TRUE)] = nomonetario$tfor_nm[(decena == 1 &
                                                                        is.na(decena) != TRUE)] / d6s02
nomonetario$tfor_nm[(decena == 2 &
                       is.na(decena) != TRUE)] = nomonetario$tfor_nm[(decena == 2 &
                                                                        is.na(decena) != TRUE)] / d6s02
nomonetario$tfor_nm[(decena == 3 &
                       is.na(decena) != TRUE)] = nomonetario$tfor_nm[(decena == 3 &
                                                                        is.na(decena) != TRUE)] / d6s03
nomonetario$tfor_nm[(decena == 4 &
                       is.na(decena) != TRUE)] = nomonetario$tfor_nm[(decena == 4 &
                                                                        is.na(decena) != TRUE)] / d6s03
nomonetario$tfor_nm[(decena == 5 &
                       is.na(decena) != TRUE)] = nomonetario$tfor_nm[(decena == 5 &
                                                                        is.na(decena) != TRUE)] / d6s03
nomonetario$tfor_nm[(decena == 6 &
                       is.na(decena) != TRUE)] = nomonetario$tfor_nm[(decena == 6 &
                                                                        is.na(decena) != TRUE)] / d6s04
nomonetario$tfor_nm[(decena == 7 &
                       is.na(decena) != TRUE)] = nomonetario$tfor_nm[(decena == 7 &
                                                                        is.na(decena) != TRUE)] / d6s04
nomonetario$tfor_nm[(decena == 8 &
                       is.na(decena) != TRUE)] = nomonetario$tfor_nm[(decena == 8 &
                                                                        is.na(decena) != TRUE)] / d6s04
nomonetario$tfor_nm[(decena == 9 &
                       is.na(decena) != TRUE)] = nomonetario$tfor_nm[(decena == 9 &
                                                                        is.na(decena) != TRUE)] / d6s05
nomonetario$tfor_nm[(decena == 0 &
                       is.na(decena) != TRUE)] = nomonetario$tfor_nm[(decena == 0 &
                                                                        is.na(decena) != TRUE)] / d6s05

#Gasto en Comunicaciones deflactado (mensual)

for (i in 1:6) {
  gas_com = 1000 + i
  string = paste("F", substr(as.character(gas_com), 2, 4),  sep = "")
  nomonetario$com_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
for (i in 5:8) {
  gas_com = 1000 + i
  string = paste("R", substr(as.character(gas_com), 2, 4),  sep = "")
  nomonetario$com_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
for (i in 10:11) {
  gas_com = 1000 + i
  string = paste("R", substr(as.character(gas_com), 2, 4),  sep = "")
  nomonetario$com_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
nomonetario$com_nm[(decena == 1 &
                      is.na(decena) != TRUE)] = nomonetario$com_nm[(decena == 1 &
                                                                      is.na(decena) != TRUE)] / d6m07
nomonetario$com_nm[(decena == 2 &
                      is.na(decena) != TRUE)] = nomonetario$com_nm[(decena == 2 &
                                                                      is.na(decena) != TRUE)] / d6m07
nomonetario$com_nm[(decena == 3 &
                      is.na(decena) != TRUE)] = nomonetario$com_nm[(decena == 3 &
                                                                      is.na(decena) != TRUE)] / d6m08
nomonetario$com_nm[(decena == 4 &
                      is.na(decena) != TRUE)] = nomonetario$com_nm[(decena == 4 &
                                                                      is.na(decena) != TRUE)] / d6m08
nomonetario$com_nm[(decena == 5 &
                      is.na(decena) != TRUE)] = nomonetario$com_nm[(decena == 5 &
                                                                      is.na(decena) != TRUE)] / d6m08
nomonetario$com_nm[(decena == 6 &
                      is.na(decena) != TRUE)] = nomonetario$com_nm[(decena == 6 &
                                                                      is.na(decena) != TRUE)] / d6m09
nomonetario$com_nm[(decena == 7 &
                      is.na(decena) != TRUE)] = nomonetario$com_nm[(decena == 7 &
                                                                      is.na(decena) != TRUE)] / d6m09
nomonetario$com_nm[(decena == 8 &
                      is.na(decena) != TRUE)] = nomonetario$com_nm[(decena == 8 &
                                                                      is.na(decena) != TRUE)] / d6m09
nomonetario$com_nm[(decena == 9 &
                      is.na(decena) != TRUE)] = nomonetario$com_nm[(decena == 9 &
                                                                      is.na(decena) != TRUE)] / d6m10
nomonetario$com_nm[(decena == 0 &
                      is.na(decena) != TRUE)] = nomonetario$com_nm[(decena == 0 &
                                                                      is.na(decena) != TRUE)] / d6m10

#Gasto en Educaci?n y recreaci?n deflactado (mensual)

for (i in 1:34) {
  gas_edurec = 1000 + i
  string = paste("E", substr(as.character(gas_edurec), 2, 4),  sep = "")
  nomonetario$edre_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
for (i in 134:135) {
  gas_edurec = 1000 + i
  string = paste("H", substr(as.character(gas_edurec), 2, 4),  sep = "")
  nomonetario$edre_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
for (i in 1:29) {
  gas_edurec = 1000 + i
  string = paste("L", substr(as.character(gas_edurec), 2, 4),  sep = "")
  nomonetario$edre_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
for (i in 3:5) {
  gas_edurec = 1000 + i
  string = paste("N", substr(as.character(gas_edurec), 2, 4),  sep = "")
  nomonetario$edre_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
nomonetario$edre_nm[(clave == "R009" &
                       is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == "R009" &
                                                                        is.na(clave) != TRUE)]

nomonetario$edre_nm[(decena == 1 &
                       is.na(decena) != TRUE)] = nomonetario$edre_nm[(decena == 1 &
                                                                        is.na(decena) != TRUE)] / d7m07
nomonetario$edre_nm[(decena == 2 &
                       is.na(decena) != TRUE)] = nomonetario$edre_nm[(decena == 2 &
                                                                        is.na(decena) != TRUE)] / d7m07
nomonetario$edre_nm[(decena == 3 &
                       is.na(decena) != TRUE)] = nomonetario$edre_nm[(decena == 3 &
                                                                        is.na(decena) != TRUE)] / d7m08
nomonetario$edre_nm[(decena == 4 &
                       is.na(decena) != TRUE)] = nomonetario$edre_nm[(decena == 4 &
                                                                        is.na(decena) != TRUE)] / d7m08
nomonetario$edre_nm[(decena == 5 &
                       is.na(decena) != TRUE)] = nomonetario$edre_nm[(decena == 5 &
                                                                        is.na(decena) != TRUE)] / d7m08
nomonetario$edre_nm[(decena == 6 &
                       is.na(decena) != TRUE)] = nomonetario$edre_nm[(decena == 6 &
                                                                        is.na(decena) != TRUE)] / d7m09
nomonetario$edre_nm[(decena == 7 &
                       is.na(decena) != TRUE)] = nomonetario$edre_nm[(decena == 7 &
                                                                        is.na(decena) != TRUE)] / d7m09
nomonetario$edre_nm[(decena == 8 &
                       is.na(decena) != TRUE)] = nomonetario$edre_nm[(decena == 8 &
                                                                        is.na(decena) != TRUE)] / d7m09
nomonetario$edre_nm[(decena == 9 &
                       is.na(decena) != TRUE)] = nomonetario$edre_nm[(decena == 9 &
                                                                        is.na(decena) != TRUE)] / d7m10
nomonetario$edre_nm[(decena == 0 &
                       is.na(decena) != TRUE)] = nomonetario$edre_nm[(decena == 0 &
                                                                        is.na(decena) != TRUE)] / d7m10

#Gasto en Educaci?n b?sica deflactado (mensual)

for (i in 2:3) {
  gas_edubas = 1000 + i
  string = paste("E", substr(as.character(gas_edubas), 2, 4),  sep = "")
  nomonetario$edba_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
for (i in 134:135) {
  gas_edubas = 1000 + i
  string = paste("H", substr(as.character(gas_edubas), 2, 4),  sep = "")
  nomonetario$edba_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
nomonetario$edba_nm[(decena == 1 &
                       is.na(decena) != TRUE)] = nomonetario$edba_nm[(decena == 1 &
                                                                        is.na(decena) != TRUE)] / d7m07
nomonetario$edba_nm[(decena == 2 &
                       is.na(decena) != TRUE)] = nomonetario$edba_nm[(decena == 2 &
                                                                        is.na(decena) != TRUE)] / d7m07
nomonetario$edba_nm[(decena == 3 &
                       is.na(decena) != TRUE)] = nomonetario$edba_nm[(decena == 3 &
                                                                        is.na(decena) != TRUE)] / d7m08
nomonetario$edba_nm[(decena == 4 &
                       is.na(decena) != TRUE)] = nomonetario$edba_nm[(decena == 4 &
                                                                        is.na(decena) != TRUE)] / d7m08
nomonetario$edba_nm[(decena == 5 &
                       is.na(decena) != TRUE)] = nomonetario$edba_nm[(decena == 5 &
                                                                        is.na(decena) != TRUE)] / d7m08
nomonetario$edba_nm[(decena == 6 &
                       is.na(decena) != TRUE)] = nomonetario$edba_nm[(decena == 6 &
                                                                        is.na(decena) != TRUE)] / d7m09
nomonetario$edba_nm[(decena == 7 &
                       is.na(decena) != TRUE)] = nomonetario$edba_nm[(decena == 7 &
                                                                        is.na(decena) != TRUE)] / d7m09
nomonetario$edba_nm[(decena == 8 &
                       is.na(decena) != TRUE)] = nomonetario$edba_nm[(decena == 8 &
                                                                        is.na(decena) != TRUE)] / d7m09
nomonetario$edba_nm[(decena == 9 &
                       is.na(decena) != TRUE)] = nomonetario$edba_nm[(decena == 9 &
                                                                        is.na(decena) != TRUE)] / d7m10
nomonetario$edba_nm[(decena == 0 &
                       is.na(decena) != TRUE)] = nomonetario$edba_nm[(decena == 0 &
                                                                        is.na(decena) != TRUE)] / d7m10

#Gasto en Cuidado personal deflactado (mensual)

for (i in 1:26) {
  gas_cuiper = 1000 + i
  string = paste("D", substr(as.character(gas_cuiper), 2, 4),  sep = "")
  nomonetario$cuip_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
nomonetario$cuip_nm[(clave == "H132" &
                       is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == "H132" &
                                                                        is.na(clave) != TRUE)]

nomonetario$cuip_nm[(decena == 1 &
                       is.na(decena) != TRUE)] = nomonetario$cuip_nm[(decena == 1 &
                                                                        is.na(decena) != TRUE)] / d23m07
nomonetario$cuip_nm[(decena == 2 &
                       is.na(decena) != TRUE)] = nomonetario$cuip_nm[(decena == 2 &
                                                                        is.na(decena) != TRUE)] / d23m07
nomonetario$cuip_nm[(decena == 3 &
                       is.na(decena) != TRUE)] = nomonetario$cuip_nm[(decena == 3 &
                                                                        is.na(decena) != TRUE)] / d23m08
nomonetario$cuip_nm[(decena == 4 &
                       is.na(decena) != TRUE)] = nomonetario$cuip_nm[(decena == 4 &
                                                                        is.na(decena) != TRUE)] / d23m08
nomonetario$cuip_nm[(decena == 5 &
                       is.na(decena) != TRUE)] = nomonetario$cuip_nm[(decena == 5 &
                                                                        is.na(decena) != TRUE)] / d23m08
nomonetario$cuip_nm[(decena == 6 &
                       is.na(decena) != TRUE)] = nomonetario$cuip_nm[(decena == 6 &
                                                                        is.na(decena) != TRUE)] / d23m09
nomonetario$cuip_nm[(decena == 7 &
                       is.na(decena) != TRUE)] = nomonetario$cuip_nm[(decena == 7 &
                                                                        is.na(decena) != TRUE)] / d23m09
nomonetario$cuip_nm[(decena == 8 &
                       is.na(decena) != TRUE)] = nomonetario$cuip_nm[(decena == 8 &
                                                                        is.na(decena) != TRUE)] / d23m09
nomonetario$cuip_nm[(decena == 9 &
                       is.na(decena) != TRUE)] = nomonetario$cuip_nm[(decena == 9 &
                                                                        is.na(decena) != TRUE)] / d23m10
nomonetario$cuip_nm[(decena == 0 &
                       is.na(decena) != TRUE)] = nomonetario$cuip_nm[(decena == 0 &
                                                                        is.na(decena) != TRUE)] / d23m10

#Gasto en Accesorios personales deflactado (trimestral)

for (i in 123:131) {
  gas_accper = 1000 + i
  string = paste("H", substr(as.character(gas_accper), 2, 4),  sep = "")
  nomonetario$accp_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
nomonetario$accp_nm[(clave == "H133" &
                       is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == "H133" &
                                                                        is.na(clave) != TRUE)]

nomonetario$accp_nm[(decena == 1 &
                       is.na(decena) != TRUE)] = nomonetario$accp_nm[(decena == 1 &
                                                                        is.na(decena) != TRUE)] / d23t05
nomonetario$accp_nm[(decena == 2 &
                       is.na(decena) != TRUE)] = nomonetario$accp_nm[(decena == 2 &
                                                                        is.na(decena) != TRUE)] / d23t05
nomonetario$accp_nm[(decena == 3 &
                       is.na(decena) != TRUE)] = nomonetario$accp_nm[(decena == 3 &
                                                                        is.na(decena) != TRUE)] / d23t06
nomonetario$accp_nm[(decena == 4 &
                       is.na(decena) != TRUE)] = nomonetario$accp_nm[(decena == 4 &
                                                                        is.na(decena) != TRUE)] / d23t06
nomonetario$accp_nm[(decena == 5 &
                       is.na(decena) != TRUE)] = nomonetario$accp_nm[(decena == 5 &
                                                                        is.na(decena) != TRUE)] / d23t06
nomonetario$accp_nm[(decena == 6 &
                       is.na(decena) != TRUE)] = nomonetario$accp_nm[(decena == 6 &
                                                                        is.na(decena) != TRUE)] / d23t07
nomonetario$accp_nm[(decena == 7 &
                       is.na(decena) != TRUE)] = nomonetario$accp_nm[(decena == 7 &
                                                                        is.na(decena) != TRUE)] / d23t07
nomonetario$accp_nm[(decena == 8 &
                       is.na(decena) != TRUE)] = nomonetario$accp_nm[(decena == 8 &
                                                                        is.na(decena) != TRUE)] / d23t07
nomonetario$accp_nm[(decena == 9 &
                       is.na(decena) != TRUE)] = nomonetario$accp_nm[(decena == 9 &
                                                                        is.na(decena) != TRUE)] / d23t08
nomonetario$accp_nm[(decena == 0 &
                       is.na(decena) != TRUE)] = nomonetario$accp_nm[(decena == 0 &
                                                                        is.na(decena) != TRUE)] / d23t08

#Gasto en Otros gastos y transferencias deflactado (semestral)

for (i in 1:2) {
  gas_otro = 1000 + i
  string = paste("N", substr(as.character(gas_otro), 2, 4),  sep = "")
  nomonetario$otr_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
for (i in 6:16) {
  gas_otro = 1000 + i
  string = paste("N", substr(as.character(gas_otro), 2, 4),  sep = "")
  nomonetario$otr_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
for (i in 901:915) {
  gas_otro = 1000 + i
  string = paste("T", substr(as.character(gas_otro), 2, 4),  sep = "")
  nomonetario$otr_nm[(clave == string &
                        is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                         is.na(clave) != TRUE)]
}
nomonetario$otr_nm[(clave == "R012" &
                      is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == "R012" &
                                                                       is.na(clave) != TRUE)]

nomonetario$otr_nm[(decena == 1 &
                      is.na(decena) != TRUE)] = nomonetario$otr_nm[(decena == 1 &
                                                                      is.na(decena) != TRUE)] / dINPCs02
nomonetario$otr_nm[(decena == 2 &
                      is.na(decena) != TRUE)] = nomonetario$otr_nm[(decena == 2 &
                                                                      is.na(decena) != TRUE)] / dINPCs02
nomonetario$otr_nm[(decena == 3 &
                      is.na(decena) != TRUE)] = nomonetario$otr_nm[(decena == 3 &
                                                                      is.na(decena) != TRUE)] / dINPCs03
nomonetario$otr_nm[(decena == 4 &
                      is.na(decena) != TRUE)] = nomonetario$otr_nm[(decena == 4 &
                                                                      is.na(decena) != TRUE)] / dINPCs03
nomonetario$otr_nm[(decena == 5 &
                      is.na(decena) != TRUE)] = nomonetario$otr_nm[(decena == 5 &
                                                                      is.na(decena) != TRUE)] / dINPCs03
nomonetario$otr_nm[(decena == 6 &
                      is.na(decena) != TRUE)] = nomonetario$otr_nm[(decena == 6 &
                                                                      is.na(decena) != TRUE)] / dINPCs04
nomonetario$otr_nm[(decena == 7 &
                      is.na(decena) != TRUE)] = nomonetario$otr_nm[(decena == 7 &
                                                                      is.na(decena) != TRUE)] / dINPCs04
nomonetario$otr_nm[(decena == 8 &
                      is.na(decena) != TRUE)] = nomonetario$otr_nm[(decena == 8 &
                                                                      is.na(decena) != TRUE)] / dINPCs04
nomonetario$otr_nm[(decena == 9 &
                      is.na(decena) != TRUE)] = nomonetario$otr_nm[(decena == 9 &
                                                                      is.na(decena) != TRUE)] / dINPCs05
nomonetario$otr_nm[(decena == 0 &
                      is.na(decena) != TRUE)] = nomonetario$otr_nm[(decena == 0 &
                                                                      is.na(decena) != TRUE)] / dINPCs05

#Gasto en Regalos Otorgados deflactado

for (i in 901:915) {
  gas_reg = 1000 + i
  string = paste("T", substr(as.character(gas_reg), 2, 4),  sep = "")
  nomonetario$reda_nm[(clave == string &
                         is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == string &
                                                                          is.na(clave) != TRUE)]
}
nomonetario$reda_nm[(clave == "N013" &
                       is.na(clave) != TRUE)] = nomonetario$gasnomon[(clave == "N013" &
                                                                        is.na(clave) != TRUE)]

nomonetario$reda_nm[(decena == 1 &
                       is.na(decena) != TRUE)] = nomonetario$reda_nm[(decena == 1 &
                                                                        is.na(decena) != TRUE)] / dINPCs02
nomonetario$reda_nm[(decena == 2 &
                       is.na(decena) != TRUE)] = nomonetario$reda_nm[(decena == 2 &
                                                                        is.na(decena) != TRUE)] / dINPCs02
nomonetario$reda_nm[(decena == 3 &
                       is.na(decena) != TRUE)] = nomonetario$reda_nm[(decena == 3 &
                                                                        is.na(decena) != TRUE)] / dINPCs03
nomonetario$reda_nm[(decena == 4 &
                       is.na(decena) != TRUE)] = nomonetario$reda_nm[(decena == 4 &
                                                                        is.na(decena) != TRUE)] / dINPCs03
nomonetario$reda_nm[(decena == 5 &
                       is.na(decena) != TRUE)] = nomonetario$reda_nm[(decena == 5 &
                                                                        is.na(decena) != TRUE)] / dINPCs03
nomonetario$reda_nm[(decena == 6 &
                       is.na(decena) != TRUE)] = nomonetario$reda_nm[(decena == 6 &
                                                                        is.na(decena) != TRUE)] / dINPCs04
nomonetario$reda_nm[(decena == 7 &
                       is.na(decena) != TRUE)] = nomonetario$reda_nm[(decena == 7 &
                                                                        is.na(decena) != TRUE)] / dINPCs04
nomonetario$reda_nm[(decena == 8 &
                       is.na(decena) != TRUE)] = nomonetario$reda_nm[(decena == 8 &
                                                                        is.na(decena) != TRUE)] / dINPCs04
nomonetario$reda_nm[(decena == 9 &
                       is.na(decena) != TRUE)] = nomonetario$reda_nm[(decena == 9 &
                                                                        is.na(decena) != TRUE)] / dINPCs05
nomonetario$reda_nm[(decena == 0 &
                       is.na(decena) != TRUE)] = nomonetario$reda_nm[(decena == 0 &
                                                                        is.na(decena) != TRUE)] / dINPCs05
detach(nomonetario)

mean(nomonetario$ali_nm, na.rm = TRUE)
mean(nomonetario$alta_nm, na.rm = TRUE)
mean(nomonetario$veca_nm, na.rm = TRUE)
mean(nomonetario$viv_nm, na.rm = TRUE)
mean(nomonetario$lim_nm, na.rm = TRUE)
mean(nomonetario$cris_nm, na.rm = TRUE)
mean(nomonetario$ens_nm, na.rm = TRUE)
mean(nomonetario$sal_nm, na.rm = TRUE)
mean(nomonetario$tpub_nm, na.rm = TRUE)
mean(nomonetario$tfor_nm, na.rm = TRUE)
mean(nomonetario$com_nm, na.rm = TRUE)
mean(nomonetario$edre_nm, na.rm = TRUE)
mean(nomonetario$edba_nm, na.rm = TRUE)
mean(nomonetario$cuip_nm, na.rm = TRUE)
mean(nomonetario$accp_nm, na.rm = TRUE)
mean(nomonetario$otr_nm, na.rm = TRUE)
mean(nomonetario$reda_nm, na.rm = TRUE)

indexesp = (nomonetario$esp == 1 & is.na(nomonetario$esp) != TRUE)
nomonetarioesp = nomonetario[indexesp,]


#Construcci?n de la base de pagos en especie a partir de la base de gasto no monetario
attach(nomonetarioesp)
nomonetarioesp2 = aggregate(
  x = list(
    ali_nm,
    alta_nm,
    veca_nm,
    viv_nm,
    lim_nm,
    cris_nm,
    ens_nm,
    sal_nm,
    tpub_nm,
    tfor_nm,
    com_nm,
    edre_nm,
    edba_nm,
    cuip_nm,
    accp_nm,
    otr_nm,
    reda_nm
  ),
  by = list(folioviv, foliohog),
  FUN = sum,
  na.rm = TRUE
)
detach(nomonetarioesp)

names(nomonetarioesp2)[1:19] <-
  c(
    "folioviv",
    "foliohog",
    "ali_nme",
    "alta_nme",
    "veca_nme",
    "viv_nme",
    "lim_nme",
    "cris_nme",
    "ens_nme",
    "sal_nme",
    "tpub_nme",
    "tfor_nme",
    "com_nme",
    "edre_nme",
    "edba_nme",
    "cuip_nme",
    "accp_nme",
    "otr_nme",
    "reda_nme"
  )
nomonetarioesp2 <-
  orderBy(~ +folioviv + foliohog, data = nomonetarioesp2)


indexreg = (nomonetario$reg == 1 & is.na(nomonetario$reg) != TRUE)
nomonetarioreg = nomonetario[indexreg,]

#Construcci?n de base de regalos a partir de la base no monetaria
attach(nomonetarioreg)
nomonetarioreg2 = aggregate(
  x = list(
    ali_nm,
    alta_nm,
    veca_nm,
    viv_nm,
    lim_nm,
    cris_nm,
    ens_nm,
    sal_nm,
    tpub_nm,
    tfor_nm,
    com_nm,
    edre_nm,
    edba_nm,
    cuip_nm,
    accp_nm,
    otr_nm,
    reda_nm
  ),
  by = list(folioviv, foliohog),
  FUN = sum,
  na.rm = TRUE
)
detach(nomonetarioreg)

names(nomonetarioreg2)[1:19] <-
  c(
    "folioviv",
    "foliohog",
    "ali_nmr",
    "alta_nmr",
    "veca_nmr",
    "viv_nmr",
    "lim_nmr",
    "cris_nmr",
    "ens_nmr",
    "sal_nmr",
    "tpub_nmr",
    "tfor_nmr",
    "com_nmr",
    "edre_nmr",
    "edba_nmr",
    "cuip_nmr",
    "accp_nmr",
    "otr_nmr",
    "reda_nmr"
  )
nomonetarioreg2 <-
  orderBy(~ +folioviv + foliohog, data = nomonetarioreg2)

#Construcci?n del ingreso corriente total
concentrado <-
  read.dbf("Bases de datos/concentradohogar.dbf", as.is = TRUE)
names(concentrado) =  tolower(names(concentrado))
concentrado = concentrado[c("folioviv" ,  "foliohog"  , "factor" ,  "tam_loc" , "tot_integ")]
concentrado <- orderBy(~ +folioviv + foliohog, data = concentrado)
concentrado1 <-
  merge(concentrado,
        ingresos2,
        by.x = c("folioviv", "foliohog"),
        all = TRUE)
concentrado1 <-
  orderBy(~ +folioviv + foliohog, data = concentrado1)
concentrado2 <-
  merge(
    concentrado1,
    nomonetarioesp2,
    by.x = c("folioviv", "foliohog"),
    all = TRUE
  )
concentrado2 <-
  orderBy(~ +folioviv + foliohog, data = concentrado2)
concentrado3 <-
  merge(
    concentrado2,
    nomonetarioreg2,
    by.x = c("folioviv", "foliohog"),
    all = TRUE
  )
concentrado3 <-
  orderBy(~ +folioviv + foliohog, data = concentrado3)

concentrado3$rururb = 0
concentrado3$rururb[concentrado3$tam_loc == "4"] = 1

concentrado3$pago_esp = rowSums(
  data.frame(
    concentrado3$ali_nme,
    concentrado3$alta_nme,
    concentrado3$veca_nme,
    concentrado3$viv_nme,
    concentrado3$lim_nme,
    concentrado3$cris_nme,
    concentrado3$ens_nme,
    concentrado3$sal_nme,
    concentrado3$tpub_nme,
    concentrado3$tfor_nme,
    concentrado3$com_nme,
    concentrado3$edre_nme,
    concentrado3$cuip_nme,
    concentrado3$accp_nme,
    concentrado3$otr_nme
  ),
  na.rm = TRUE
)
concentrado3$reg_esp = rowSums(
  data.frame(
    concentrado3$ali_nmr,
    concentrado3$alta_nmr,
    concentrado3$veca_nmr,
    concentrado3$viv_nmr,
    concentrado3$lim_nmr,
    concentrado3$cris_nmr,
    concentrado3$ens_nmr,
    concentrado3$sal_nmr,
    concentrado3$tpub_nmr,
    concentrado3$tfor_nmr,
    concentrado3$com_nmr,
    concentrado3$edre_nmr,
    concentrado3$cuip_nmr,
    concentrado3$accp_nmr,
    concentrado3$otr_nmr
  ),
  na.rm = TRUE
)
concentrado3$nomon = rowSums(data.frame(concentrado3$pago_esp, concentrado3$reg_esp),
                             na.rm = TRUE)
concentrado3$ict = rowSums(data.frame(concentrado3$ing_mon, concentrado3$nomon), na.rm = TRUE)
concentrado3 <-
  orderBy(~ +folioviv + foliohog, data = concentrado3)

mean(concentrado3$pago_esp)
mean(concentrado3$reg_esp)
mean(concentrado3$nomon)
mean(concentrado3$ict)

write.csv(concentrado3, 'Bases/ingresotot16.csv', row.names = FALSE)
write.dbf(concentrado3, 'Bases/ingresotot16.dbf')


rm(list = ls())

#Construcci?n del tama?o de hogar con econom?as de escala
#y escalas de equivalencia
poblacion1 <- read.dbf("Bases de datos/poblacion.dbf", as.is = TRUE)
names(poblacion1) =  tolower(names(poblacion1))
#Poblaci?n objeto: no se incluye a hu?spedes ni trabajadores dom?sticos
poblacion2 <- as.numeric(poblacion1$parentesco)
index = ((poblacion2 >= 400 &
            poblacion2 < 500) |
           (poblacion2 >= 700 & poblacion2 < 800))
index = (index == FALSE)
poblacion = poblacion1[index,]

poblacion$ind = 1

attach(poblacion)
poblacion2 = aggregate(
  x = list(ind),
  by = list(folioviv, foliohog),
  FUN = sum,
  na.rm = TRUE
)
detach(poblacion)

names(poblacion2)[1:3] <- c("folioviv",  "foliohog", "tot_ind")

poblacion = merge(poblacion,
                  poblacion2,
                  by = c("folioviv",  "foliohog"),
                  all.x = TRUE)

#Escalas de equivalencia
attach(poblacion)
poblacion$n_05 = NA
poblacion$n_05[edad >= 0 & edad <= 5] = 1
poblacion$n_05[(edad > 5 & is.na(edad) == FALSE)] = 0
table(poblacion$n_05)

poblacion$n_6_12 = NA
poblacion$n_6_12[edad >= 6 & edad <= 12] = 1
poblacion$n_6_12[((edad > 12 |
                     edad < 6) & is.na(edad) == FALSE)] = 0
table(poblacion$n_6_12)

poblacion$n_13_18 = NA
poblacion$n_13_18[edad >= 13 & edad <= 18] = 1
poblacion$n_13_18[((edad > 18 |
                      edad < 13) & is.na(edad) == FALSE)] = 0
table(poblacion$n_13_18)


poblacion$n_19 = NA
poblacion$n_19[edad >= 19] = 1
poblacion$n_19[((edad < 19) & is.na(edad) == FALSE)] = 0
table(poblacion$n_19)
detach(poblacion)

attach(poblacion)
poblacion$tamhogesc = n_05 * .7031
poblacion$tamhogesc[n_6_12 == 1 &
                      is.na(n_6_12) == FALSE] = n_6_12[n_6_12 == 1 &
                                                         is.na(n_6_12) == FALSE] * .7382
poblacion$tamhogesc[n_13_18 == 1 &
                      is.na(n_13_18) == FALSE] = n_13_18[n_13_18 == 1 &
                                                           is.na(n_13_18) == FALSE] * .7057
poblacion$tamhogesc[n_19 == 1 &
                      is.na(n_19) == FALSE] = n_19[n_19 == 1 &
                                                     is.na(n_19) == FALSE] * .9945
poblacion$tamhogesc[tot_ind == 1 & is.na(tot_ind) == FALSE] = 1

detach(poblacion)

attach(poblacion)
poblacion2 = aggregate(
  x = list(tamhogesc),
  by = list(folioviv, foliohog),
  FUN = sum,
  na.rm = TRUE
)
detach(poblacion)

names(poblacion2)[1:3] <- c("folioviv", "foliohog", "tamhogesc")
poblacion2 <- orderBy(~ +folioviv + foliohog, data = poblacion2)
write.csv(poblacion2, 'Bases/tamhogesc16.csv', row.names = FALSE)
write.dbf(poblacion2, 'Bases/tamhogesc16.dbf')


#Bienestar por ingresos

ingresos = read.dbf('Bases/ingresotot16.dbf', as.is = TRUE)
names(ingresos) =  tolower(names(ingresos))
ingresos2 = merge(ingresos,
                  poblacion2,
                  by = c("folioviv",  "foliohog"),
                  all = TRUE)
ingresos2 <- orderBy(~ +folioviv + foliohog, data = ingresos2)


ingresos2$ictpc = as.numeric(ingresos2$ict) / ingresos2$tamhogesc
ingresos2$factorp = ingresos2$factor * ingresos2$tot_integ

#Indicador de Bienestar por ingresos
#LP I: Valor de la Canasta para bienestar m?nimo

#LP II: Valor de la Canasta acc_alimentaria m?s el valor de la canasta
#no acc_alimentaria (ver Anexo A del documento metodol?gico).

#En este programa se construyen los indicadores de bienestar por ingresos
#mediante las 2 l?neas definidas por CONEVAL , denomin?ndoles:

#lp1 : L?nea de Bienestar M?nimo
#lp2 : L?nea de Bienestar

#Bienestar m?nimo

#Valor de la canasta b?sica (ver Nota T?cnica)

lp1_urb = 1310.94
lp1_rur = 933.20


#Se identifica a los hogares bajo LP1
attach(ingresos2)
ingresos2$plb_m = NA
ingresos2$plb_m[ictpc < lp1_urb & rururb == 0] = 1
ingresos2$plb_m[ictpc >= lp1_urb & rururb == 0] = 0
ingresos2$plb_m[ictpc < lp1_rur & rururb == 1] = 1
ingresos2$plb_m[ictpc >= lp1_rur & rururb == 1] = 0
table(ingresos2$plb_m)
detach(ingresos2)

#Bienestar
lp2_urb = 2660.40
lp2_rur = 1715.57


attach(ingresos2)
ingresos2$plb = NA
ingresos2$plb[(ictpc < lp2_urb & rururb == 0)] = 1
ingresos2$plb[(ictpc >= lp2_urb & rururb == 0)] = 0
ingresos2$plb[(ictpc < lp2_rur & rururb == 1)] = 1
ingresos2$plb[(ictpc >= lp2_rur & rururb == 1)] = 0
table(ingresos2$plb)
detach(ingresos2)



attach(ingresos2)
ingresos2 = ingresos2[, c(
  "folioviv",
  "foliohog",
  "factor",
  "tam_loc",
  "rururb",
  "tamhogesc",
  "ict",
  "ictpc",
  "plb_m",
  "plb"
)]
ingresos2 <- orderBy(~ +folioviv + foliohog, data = ingresos2)
write.csv(ingresos2, 'Bases/p_ingresos16.csv', row.names = FALSE)
write.dbf(ingresos2, 'Bases/p_ingresos16.dbf')
detach(ingresos2)

##Parte VIII Pobreza ----

#

rm(list = ls())
rezedu = read.csv(
  'Bases/ic_rezedu16.csv',
  header = TRUE,
  sep = ",",
  na.strings = "NA",
  dec = "."
)
salud = read.csv(
  'Bases/ic_asalud16.csv',
  header = TRUE,
  sep = ",",
  na.strings = "NA",
  dec = "."
)
base_pobreza = merge(rezedu,
                     salud,
                     by.x = c("folioviv", "foliohog", "numren"),
                     all.x = TRUE)
segsoc = read.csv(
  'Bases/ic_segsoc16.csv',
  header = TRUE,
  sep = ",",
  na.strings = "NA",
  dec = "."
)
base_pobreza = merge(
  base_pobreza,
  segsoc,
  by.x = c("folioviv", "foliohog", "numren"),
  all.x = TRUE
)
ingresos = read.csv(
  'Bases/p_ingresos16.csv',
  header = TRUE,
  sep = ",",
  na.strings = "NA",
  dec = "."
)
base_pobreza = merge(base_pobreza,
                     ingresos,
                     by.x = c("folioviv", "foliohog"),
                     all.x = TRUE)
cev = read.csv(
  'Bases/ic_cev16.csv',
  header = TRUE,
  sep = ",",
  na.strings = "NA",
  dec = "."
)
base_pobreza = merge(base_pobreza,
                     cev,
                     by.x = c("folioviv", "foliohog"),
                     all.x = TRUE)
sbv = read.csv(
  'Bases/ic_sbv16.csv',
  header = TRUE,
  sep = ",",
  na.strings = "NA",
  dec = "."
)
base_pobreza = merge(base_pobreza,
                     sbv,
                     by.x = c("folioviv"),
                     all.x = TRUE)
ali = read.csv(
  'Bases/ic_ali16.csv',
  header = TRUE,
  sep = ",",
  na.strings = "NA",
  dec = "."
)
base_pobreza = merge(base_pobreza,
                     ali,
                     by.x = c("folioviv", "foliohog"),
                     all.x = TRUE)

#######################################################

#Identificador de la entidad federativa
base_pobreza$ent = substr(10000000000 + base_pobreza$folioviv, 2, 3)

#?ndice de Privaci?n Social
base_pobreza$i_privacion = rowSums(
  data.frame(
    base_pobreza$ic_rezedu,
    base_pobreza$ic_asalud,
    base_pobreza$ic_segsoc,
    base_pobreza$ic_cv,
    base_pobreza$ic_sbv,
    base_pobreza$ic_ali
  ),
  na.rm = TRUE
)
base_pobreza$i_privacion[(
  is.na(base_pobreza$ic_rezedu) == TRUE |
    is.na(base_pobreza$ic_asalud) == TRUE |
    is.na(base_pobreza$ic_segsoc) == TRUE |
    is.na(base_pobreza$ic_cv) == TRUE |
    is.na(base_pobreza$ic_sbv) == TRUE |
    is.na(base_pobreza$ic_ali) == TRUE
)] = NA
table(base_pobreza$i_privacion)
#Pobreza
base_pobreza$pobreza[((
  base_pobreza$i_privacion >= 1 &
    is.na(base_pobreza$i_privacion) != TRUE
) & base_pobreza$plb == 1)] = 1
base_pobreza$pobreza[(base_pobreza$plb == 0 |
                        base_pobreza$i_privacion == 0) &
                       (is.na(base_pobreza$plb) != TRUE &
                          is.na(base_pobreza$i_privacion) != TRUE)] = 0
table(base_pobreza$pobreza)

#Pobreza extrema
base_pobreza$pobreza_e[(base_pobreza$i_privacion >= 3 &
                          is.na(base_pobreza$i_privacion) != TRUE) &
                         base_pobreza$plb_m == 1] = 1
base_pobreza$pobreza_e[(base_pobreza$plb_m == 0 |
                          base_pobreza$i_privacion < 3) &
                         (is.na(base_pobreza$plb_m) != TRUE &
                            is.na(base_pobreza$i_privacion) != TRUE)] = 0
table(base_pobreza$pobreza_e)

#Pobreza moderada
base_pobreza$pobreza_m[(base_pobreza$pobreza == 1 &
                          base_pobreza$pobreza_e == 0)] = 1
base_pobreza$pobreza_m[base_pobreza$pobreza == 0 |
                         (base_pobreza$pobreza == 1 &
                            base_pobreza$pobreza_e == 1)] = 0
table(base_pobreza$pobreza_m)

#Poblaci?n vulnerable

#Vulnerables por carencias
base_pobreza$vul_car[((
  base_pobreza$i_privacion >= 1 &
    is.na(base_pobreza$i_privacion) != TRUE
) & base_pobreza$plb == 0)] = 1
base_pobreza$vul_car[((base_pobreza$plb == 0 &
                         base_pobreza$i_privacion == 0) |
                        base_pobreza$plb == 1)] = 0
base_pobreza$vul_car[is.na(base_pobreza$pobreza) == TRUE] = NA
table(base_pobreza$vul_car)

#Vulnerables por ingresos
base_pobreza$vul_ing[(base_pobreza$i_privacion == 0 &
                        base_pobreza$plb == 1)] = 1
base_pobreza$vul_ing[base_pobreza$plb == 0 |
                       (base_pobreza$plb == 1 &
                          base_pobreza$i_privacion >= 1)] = 0
base_pobreza$vul_ing[is.na(base_pobreza$pobreza) == TRUE] = NA
table(base_pobreza$vul_ing)

#Poblaci?n no pobre y no vulnerable
base_pobreza$no_pobv[(base_pobreza$i_privacion == 0 &
                        base_pobreza$plb == 0)] = 1
base_pobreza$no_pobv[base_pobreza$plb == 1 |
                       (base_pobreza$plb == 0 &
                          base_pobreza$i_privacion >= 1)] = 0
base_pobreza$no_pobv[is.na(base_pobreza$pobreza) == TRUE] = NA
table(base_pobreza$no_pobv)

#Poblaci?n con carencias sociales#
base_pobreza$carencias[base_pobreza$i_privacion >= 1] = 1
base_pobreza$carencias[base_pobreza$i_privacion == 0] = 0
base_pobreza$carencias[is.na(base_pobreza$pobreza) == TRUE] = NA
table(base_pobreza$carencias)

base_pobreza$carencias3[base_pobreza$i_privacion >= 3] = 1
base_pobreza$carencias3[base_pobreza$i_privacion <= 2] = 0
base_pobreza$carencias3[is.na(base_pobreza$pobreza) == TRUE] = NA
table(base_pobreza$carencias3)

#Cuadrantes
base_pobreza$cuadrantes = NA
base_pobreza$cuadrantes[(base_pobreza$i_privacion >= 1 &
                           is.na(base_pobreza$i_privacion) != TRUE) &
                          base_pobreza$plb == 1] = 1
base_pobreza$cuadrantes[((
  base_pobreza$i_privacion >= 1 &
    is.na(base_pobreza$i_privacion) != TRUE
) & base_pobreza$plb == 0)] = 2
base_pobreza$cuadrantes[base_pobreza$i_privacion == 0 &
                          base_pobreza$plb == 1] = 3
base_pobreza$cuadrantes[base_pobreza$i_privacion == 0 &
                          base_pobreza$plb == 0] = 4
table(base_pobreza$cuadrantes)

#Profundidad en el espacio del bienestar

#FGT (a=1)

lp1_urb = 1310.94
lp1_rur = 933.20

lp2_urb = 2660.40
lp2_rur = 1715.57

#Distancia normalizada del ingreso respecto a la l?nea de bienestar
base_pobreza$prof_b1[base_pobreza$rururb == 1 &
                       base_pobreza$plb == 1 &
                       is.na(base_pobreza$rururb) != TRUE &
                       is.na(base_pobreza$plb) != TRUE] = (lp2_rur - base_pobreza$ictpc[base_pobreza$rururb ==
                                                                                          1 &
                                                                                          base_pobreza$plb == 1]) / lp2_rur
base_pobreza$prof_b1[base_pobreza$rururb == 0 &
                       base_pobreza$plb == 1 &
                       is.na(base_pobreza$rururb) != TRUE &
                       is.na(base_pobreza$plb) != TRUE] = (lp2_urb - base_pobreza$ictpc[base_pobreza$rururb ==
                                                                                          0 &
                                                                                          base_pobreza$plb == 1]) / lp2_urb
base_pobreza$prof_b1[is.na(base_pobreza$ictpc) != TRUE &
                       is.na(base_pobreza$prof_b1) == TRUE] = 0
mean(base_pobreza$prof_b1[is.na(base_pobreza$prof_b1) != TRUE])

#Distancia normalizada del ingreso respecto a la l?nea de bienestar m?nimo
base_pobreza$prof_bm1[base_pobreza$rururb == 1 &
                        base_pobreza$plb_m == 1 &
                        is.na(base_pobreza$rururb) != TRUE &
                        is.na(base_pobreza$plb_m) != TRUE] = (lp1_rur - base_pobreza$ictpc[base_pobreza$rururb ==
                                                                                             1 &
                                                                                             base_pobreza$plb_m == 1]) / lp1_rur
base_pobreza$prof_bm1[base_pobreza$rururb == 0 &
                        base_pobreza$plb_m == 1 &
                        is.na(base_pobreza$rururb) != TRUE &
                        is.na(base_pobreza$plb_m) != TRUE] = (lp1_urb - base_pobreza$ictpc[base_pobreza$rururb ==
                                                                                             0 &
                                                                                             base_pobreza$plb_m == 1]) / lp1_urb
base_pobreza$prof_bm1[is.na(base_pobreza$ictpc) != TRUE &
                        is.na(base_pobreza$prof_bm1) == TRUE] = 0
mean(base_pobreza$prof_bm1[is.na(base_pobreza$prof_bm1) != TRUE])

#Profundidad de la privaci?n social
base_pobreza$profun = base_pobreza$i_privacion / 6
mean(base_pobreza$profun[is.na(base_pobreza$profun) != TRUE])

#Intensidad de la privaci?n social

#Poblaci?n pobre
#Intensidad de la privaci?n social: pobres
base_pobreza$int_pob = base_pobreza$profun * base_pobreza$pobreza
mean(base_pobreza$int_pob[is.na(base_pobreza$int_pob) != TRUE])

#Poblaci?n pobre extrema
#Intensidad de la privaci?n social: pobres extremos
base_pobreza$int_pobe = base_pobreza$profun * base_pobreza$pobreza_e
mean(base_pobreza$int_pobe[is.na(base_pobreza$int_pobe) != TRUE])

#Poblaci?n vulnerable por carencias
#Intensidad de la privaci?n social: poblaci?n vulnerable por carencias
base_pobreza$int_vulcar = base_pobreza$profun * base_pobreza$vul_car
mean(base_pobreza$int_vulcar[is.na(base_pobreza$int_vulcar) != TRUE])

#Poblaci?n carenciada
#Intensidad de la privaci?n social: poblaci?n carenciada
base_pobreza$int_caren = base_pobreza$profun * base_pobreza$carencias
mean(base_pobreza$int_caren[is.na(base_pobreza$int_caren) != TRUE])

base_pobreza <-
  orderBy(~ +folioviv + foliohog, data = base_pobreza)

base_pobreza = base_pobreza[, c(
  "folioviv" ,
  "foliohog" ,
  "numren" ,
  "edad" ,
  "anac_e" ,
  "inas_esc" ,
  "antec_esc" ,
  "niv_ed" ,
  "ic_rezedu" ,
  "hli" ,
  "sexo" ,
  "discap" ,
  "ic_asalud" ,
  "pea" ,
  "jub" ,
  "ss_dir" ,
  "par" ,
  "jef_ss" ,
  "cony_ss" ,
  "hijo_ss" ,
  "s_salud" ,
  "pam" ,
  "ic_segsoc" ,
  "factor" ,
  "tam_loc" ,
  "rururb" ,
  "tamhogesc" ,
  "ict" ,
  "ictpc" ,
  "plb_m" ,
  "plb" ,
  "tot_resid" ,
  "num_cuarto" ,
  "icv_pisos" ,
  "icv_techos" ,
  "icv_muros" ,
  "icv_hac" ,
  "ic_cv" ,
  "isb_agua" ,
  "isb_dren" ,
  "isb_luz" ,
  "isb_combus" ,
  "ic_sbv" ,
  "id_men" ,
  "ia_1ad" ,
  "ia_2ad" ,
  "ia_3ad" ,
  "ia_4ad" ,
  "ia_5ad" ,
  "ia_6ad" ,
  "ia_7men" ,
  "ia_8men" ,
  "ia_9men" ,
  "ia_10men" ,
  "ia_11men" ,
  "ia_12men" ,
  "tot_iaad" ,
  "tot_iamen" ,
  "ins_ali" ,
  "ic_ali" ,
  "ent" ,
  "i_privacion" ,
  "pobreza" ,
  "pobreza_e" ,
  "pobreza_m" ,
  "vul_car" ,
  "vul_ing" ,
  "no_pobv" ,
  "carencias" ,
  "carencias3" ,
  "cuadrantes" ,
  "prof_b1" ,
  "prof_bm1" ,
  "profun" ,
  "int_pob" ,
  "int_pobe" ,
  "int_vulcar" ,
  "int_caren"
)]



write.csv(base_pobreza, 'Bases/pobreza_16.csv', row.names = FALSE)
write.dbf(base_pobreza, 'Bases/pobreza_16.dbf')

#Indicadores de la medici?n de pobreza, Estados Unidos Mexicanos


#Pobreza
pobreza = by(base_pobreza, base_pobreza[, "ent"],
             function(base_pobreza)
               sum(base_pobreza$factor[base_pobreza$pobreza == 1 &
                                         is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Pobreza moderada
pobreza_m = by(base_pobreza, base_pobreza[, "ent"],
               function(base_pobreza)
                 sum(base_pobreza$factor[base_pobreza$pobreza_m == 1 &
                                           is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Pobreza extrema
pobreza_e = by(base_pobreza, base_pobreza[, "ent"],
               function(base_pobreza)
                 sum(base_pobreza$factor[base_pobreza$pobreza_e == 1 &
                                           is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Vulnerables por carencias sociales
vul_car = by(base_pobreza, base_pobreza[, "ent"],
             function(base_pobreza)
               sum(base_pobreza$factor[base_pobreza$vul_car == 1 &
                                         is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Vulnerables por ingresos
vul_ing = by(base_pobreza, base_pobreza[, "ent"],
             function(base_pobreza)
               sum(base_pobreza$factor[base_pobreza$vul_ing == 1 &
                                         is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#No pobre multidimensional no vulnerable
no_pobv = by(base_pobreza, base_pobreza[, "ent"],
             function(base_pobreza)
               sum(base_pobreza$factor[base_pobreza$no_pobv == 1 &
                                         is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Poblacii?n con al menos una carencia social
carencias = by(base_pobreza, base_pobreza[, "ent"],
               function(base_pobreza)
                 sum(base_pobreza$factor[base_pobreza$carencias == 1 &
                                           is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#poblaci?n con al menos tres carencias sociales
carencias3 = by(base_pobreza, base_pobreza[, "ent"],
                function(base_pobreza)
                  sum(base_pobreza$factor[base_pobreza$carencias3 == 1 &
                                            is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Rezago educativo
ic_rezedu = by(base_pobreza, base_pobreza[, "ent"],
               function(base_pobreza)
                 sum(base_pobreza$factor[base_pobreza$ic_rezedu == 1 &
                                           is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Carencia por acceso a servicios de salud
ic_asalud = by(base_pobreza, base_pobreza[, "ent"],
               function(base_pobreza)
                 sum(base_pobreza$factor[base_pobreza$ic_asalud == 1 &
                                           is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Carencias por acceso a seguridad social
ic_segsoc = by(base_pobreza, base_pobreza[, "ent"],
               function(base_pobreza)
                 sum(base_pobreza$factor[base_pobreza$ic_segsoc == 1 &
                                           is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Carencias por calidad y espacios de la vivienda
ic_cev = by(base_pobreza, base_pobreza[, "ent"],
            function(base_pobreza)
              sum(base_pobreza$factor[base_pobreza$ic_cv == 1 &
                                        is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Carencias por acceso a los servicios b?sicos de la vivienda
ic_sbv = by(base_pobreza, base_pobreza[, "ent"],
            function(base_pobreza)
              sum(base_pobreza$factor[base_pobreza$ic_sbv == 1 &
                                        is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Carencias por acceso a la alimentaci?n
ic_ali = by(base_pobreza, base_pobreza[, "ent"],
            function(base_pobreza)
              sum(base_pobreza$factor[base_pobreza$ic_ali == 1 &
                                        is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Poblacion con un ingreso inferior a la l?nea de bienestar m?nimo
plb_m = by(base_pobreza, base_pobreza[, "ent"],
           function(base_pobreza)
             sum(base_pobreza$factor[base_pobreza$plb_m == 1 &
                                       is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))
#Poblacion con un ingreso inferior a la l?nea de bienestar
plb = by(base_pobreza, base_pobreza[, "ent"],
         function(base_pobreza)
           sum(base_pobreza$factor[base_pobreza$plb == 1 &
                                     is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE))


tabstat_sum = as.data.frame(matrix(0, nr = 33, nc = 16))


names(tabstat_sum)[1:16] = cbind(
  "pobreza",
  "pobreza_m",
  "pobreza_e",
  "vul_car",
  "vul_ing",
  "no_pobv",
  "carencias",
  "carencias3",
  "ic_rezedu",
  "ic_asalud",
  "ic_segsoc",
  "ic_cev",
  "ic_sbv",
  "ic_ali",
  "plbm",
  "plb"
)


row.names(tabstat_sum)[1:33] = cbind(
  "Aguascalientes",
  "Baja California",
  "Baja California Sur",
  "Campeche",
  "Coahuila",
  "Colima"
  ,
  "Chiapas",
  "Chihuahua",
  "Ciudad de M?xico",
  "Durango",
  "Guanajuato",
  "Guerrero",
  "Hidalgo",
  "Jalisco",
  "M?xico",
  "Michoac?n",
  "Morelos",
  "Nayarit",
  "Nuevo Le?n",
  "Oaxaca",
  "Puebla",
  "Quer?taro",
  "Quintana Roo",
  "San Luis Potos?",
  "Sinaloa",
  "Sonora",
  "Tabasco",
  "Tamaulipas",
  "Tlaxcala",
  "Veracruz",
  "Yucat?n",
  "Zacatecas",
  "NACIONAL"
)
#NACIONAL

#Pobreza
tabstat_sum[33, 1] = sum(base_pobreza$factor[base_pobreza$pobreza == 1 &
                                               is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Pobreza moderada
tabstat_sum[33, 2] = sum(base_pobreza$factor[base_pobreza$pobreza_m == 1 &
                                               is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Pobreza extrema
tabstat_sum[33, 3] = sum(base_pobreza$factor[base_pobreza$pobreza_e == 1 &
                                               is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Vulnerables por carencias sociales
tabstat_sum[33, 4] = sum(base_pobreza$factor[base_pobreza$vul_car == 1 &
                                               is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Vulnerables por ingresos
tabstat_sum[33, 5] = sum(base_pobreza$factor[base_pobreza$vul_ing == 1 &
                                               is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#No pobre multidimensional no vulnerable
tabstat_sum[33, 6] = sum(base_pobreza$factor[base_pobreza$no_pobv == 1 &
                                               is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Poblacii?n con al menos una carencia social
tabstat_sum[33, 7] = sum(base_pobreza$factor[base_pobreza$carencias == 1 &
                                               is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#poblaci?n con al menos tres carencias sociales
tabstat_sum[33, 8] = sum(base_pobreza$factor[base_pobreza$carencias3 == 1 &
                                               is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Rezago educativo
tabstat_sum[33, 9] = sum(base_pobreza$factor[base_pobreza$ic_rezedu == 1 &
                                               is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Carencia por acceso a servicios de salud
tabstat_sum[33, 10] = sum(base_pobreza$factor[base_pobreza$ic_asalud == 1 &
                                                is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Carencias por acceso a seguridad social
tabstat_sum[33, 11] = sum(base_pobreza$factor[base_pobreza$ic_segsoc == 1 &
                                                is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Carencias por calidad y espacios de la vivienda
tabstat_sum[33, 12] = sum(base_pobreza$factor[base_pobreza$ic_cv == 1 &
                                                is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Carencias por acceso a los servicios b?sicos de la vivienda
tabstat_sum[33, 13] = sum(base_pobreza$factor[base_pobreza$ic_sbv == 1 &
                                                is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Carencias por acceso a la alimentaci?n
tabstat_sum[33, 14] = sum(base_pobreza$factor[base_pobreza$ic_ali == 1 &
                                                is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Poblacion con un ingreso inferior a la l?nea de bienestar m?nimo
tabstat_sum[33, 15] = sum(base_pobreza$factor[base_pobreza$plb_m == 1 &
                                                is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)
#Poblacion con un ingreso inferior a la l?nea de bienestar
tabstat_sum[33, 16] = sum(base_pobreza$factor[base_pobreza$plb == 1 &
                                                is.na(base_pobreza$pobreza) == FALSE], na.rm = TRUE)

#ESTATAL

for (i in 1:32) {
  tabstat_sum[i, 1] = pobreza[[i]][1]
  tabstat_sum[i, 2] = pobreza_m[[i]][1]
  tabstat_sum[i, 3] = pobreza_e[[i]][1]
  tabstat_sum[i, 4] = vul_car[[i]][1]
  tabstat_sum[i, 5] = vul_ing[[i]][1]
  tabstat_sum[i, 6] = no_pobv[[i]][1]
  tabstat_sum[i, 7] = carencias[[i]][1]
  tabstat_sum[i, 8] = carencias3[[i]][1]
  tabstat_sum[i, 9] = ic_rezedu[[i]][1]
  tabstat_sum[i, 10] = ic_asalud[[i]][1]
  tabstat_sum[i, 11] = ic_segsoc[[i]][1]
  tabstat_sum[i, 12] = ic_cev[[i]][1]
  tabstat_sum[i, 13] = ic_sbv[[i]][1]
  tabstat_sum[i, 14] = ic_ali[[i]][1]
  tabstat_sum[i, 15] = plb_m[[i]][1]
  tabstat_sum[i, 16] = plb[[i]][1]
}




#Porcentaje de la poblaci?n total en pobreza
p.pobreza <-
  by(base_pobreza, base_pobreza[, "ent"], function(base_pobreza)
    weighted.mean(base_pobreza$pobreza[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                    FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total en pobreza moderada
p.pobreza_m <-
  by(base_pobreza, base_pobreza[, "ent"],    function(base_pobreza)
    weighted.mean(base_pobreza$pobreza_m[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                      FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total en pobreza extrema
p.pobreza_e <-
  by(base_pobreza, base_pobreza[, "ent"], function(base_pobreza)
    weighted.mean(base_pobreza$pobreza_e[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                      FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total vulnerable por carencias sociales
p.vul_car <-
  by(base_pobreza, base_pobreza[, "ent"], function(base_pobreza)
    weighted.mean(base_pobreza$vul_car[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                    FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total vulnerable por ingresos
p.vul_ing <-
  by(base_pobreza, base_pobreza[, "ent"], function(base_pobreza)
    weighted.mean(base_pobreza$vul_ing[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                    FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total no pobre y no vulnerable
p.no_pobv <-
  by(base_pobreza, base_pobreza[, "ent"], function(base_pobreza)
    weighted.mean(base_pobreza$no_pobv[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                    FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total con rezago educativo
p.carencias <-
  by(base_pobreza, base_pobreza[, "ent"], function(base_pobreza)
    weighted.mean(base_pobreza$carencias[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                      FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total con carencia por acceso a la salud
p.carencias3 <-
  by(base_pobreza, base_pobreza[, "ent"],    function(base_pobreza)
    weighted.mean(base_pobreza$carencias3[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                       FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total con rezago educativo
p.ic_rezedu <-
  by(base_pobreza, base_pobreza[, "ent"], function(base_pobreza)
    weighted.mean(base_pobreza$ic_rezedu[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                      FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total con carencia por acceso a la salud
p.ic_asalud <-
  by(base_pobreza, base_pobreza[, "ent"],    function(base_pobreza)
    weighted.mean(base_pobreza$ic_asalud[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                      FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total con carencia por acceso a seguridad social
p.ic_segsoc <-
  by(base_pobreza, base_pobreza[, "ent"],    function(base_pobreza)
    weighted.mean(base_pobreza$ic_segsoc[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                      FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total con carencia por calidad y espacios de la vivienda
p.ic_cev <-
  by(base_pobreza, base_pobreza[, "ent"],    function(base_pobreza)
    weighted.mean(base_pobreza$ic_cv[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                  FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total con carencia por servicios b?sicos de la vivienda
p.ic_sbv <-
  by(base_pobreza, base_pobreza[, "ent"],    function(base_pobreza)
    weighted.mean(base_pobreza$ic_sbv[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                   FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total con carencia por acceso a la acc_alimentaci?n
p.ic_ali <-
  by(base_pobreza, base_pobreza[, "ent"],    function(base_pobreza)
    weighted.mean(base_pobreza$ic_ali[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                   FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total con carencia por servicios b?sicos de la vivienda
p.plb_m <-
  by(base_pobreza, base_pobreza[, "ent"],    function(base_pobreza)
    weighted.mean(base_pobreza$plb_m[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                  FALSE], na.rm = TRUE) * 100)
#Porcentaje de la poblaci?n total con carencia por acceso a la acc_alimentaci?n
p.plb <-
  by(base_pobreza, base_pobreza[, "ent"],    function(base_pobreza)
    weighted.mean(base_pobreza$plb[is.na(base_pobreza$pobreza) == FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) ==
                                                                                                FALSE], na.rm = TRUE) * 100)



tabstat_mean = as.data.frame(matrix(0, nr = 33, nc = 16))


names(tabstat_mean)[1:16] = cbind(
  "pobreza",
  "pobreza_m",
  "pobreza_e",
  "vul_car",
  "vul_ing",
  "no_pobv",
  "carencias",
  "carencias3",
  "ic_rezedu",
  "ic_asalud",
  "ic_segsoc",
  "ic_cev",
  "ic_sbv",
  "ic_ali",
  "plbm",
  "plb"
)


row.names(tabstat_mean)[1:33] = cbind(
  "Aguascalientes",
  "Baja California",
  "Baja California Sur",
  "Campeche",
  "Coahuila",
  "Colima"
  ,
  "Chiapas",
  "Chihuahua",
  "Ciudad de M?xico",
  "Durango",
  "Guanajuato",
  "Guerrero",
  "Hidalgo",
  "Jalisco",
  "M?xico",
  "Michoac?n",
  "Morelos",
  "Nayarit",
  "Nuevo Le?n",
  "Oaxaca",
  "Puebla",
  "Quer?taro",
  "Quintana Roo",
  "San Luis Potos?",
  "Sinaloa",
  "Sonora",
  "Tabasco",
  "Tamaulipas",
  "Tlaxcala",
  "Veracruz",
  "Yucat?n",
  "Zacatecas",
  "NACIONAL"
)

#NACIONAL
#Porcentaje de la poblaci?n total en pobreza
tabstat_mean[33, 1] = weighted.mean(base_pobreza$pobreza[is.na(base_pobreza$pobreza) ==
                                                           FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                      TRUE) * 100
#Porcentaje de la poblaci?n total en pobreza moderada
tabstat_mean[33, 2] = weighted.mean(base_pobreza$pobreza_m[is.na(base_pobreza$pobreza) ==
                                                             FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                      TRUE) * 100
#Porcentaje de la poblaci?n total en pobreza extrema
tabstat_mean[33, 3] = weighted.mean(base_pobreza$pobreza_e[is.na(base_pobreza$pobreza) ==
                                                             FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                      TRUE) * 100
#Porcentaje de la poblaci?n total vulnerable por carencias sociales
tabstat_mean[33, 4] = weighted.mean(base_pobreza$vul_car[is.na(base_pobreza$pobreza) ==
                                                           FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                      TRUE) * 100
#Porcentaje de la poblaci?n total vulnerable por ingresos
tabstat_mean[33, 5] = weighted.mean(base_pobreza$vul_ing[is.na(base_pobreza$pobreza) ==
                                                           FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                      TRUE) * 100
#Porcentaje de la poblaci?n total no pobre y no vulnerable
tabstat_mean[33, 6] = weighted.mean(base_pobreza$no_pobv[is.na(base_pobreza$pobreza) ==
                                                           FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                      TRUE) * 100
#Porcentaje de la poblaci?n total con rezago educativo
tabstat_mean[33, 7] = weighted.mean(base_pobreza$carencias[is.na(base_pobreza$pobreza) ==
                                                             FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                      TRUE) * 100
#Porcentaje de la poblaci?n total con carencia por acceso a la salud

tabstat_mean[33, 8] = weighted.mean(base_pobreza$carencias3[is.na(base_pobreza$pobreza) ==
                                                              FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                      TRUE) * 100
#Porcentaje de la poblaci?n total con rezago educativo
tabstat_mean[33, 9] = weighted.mean(base_pobreza$ic_rezedu[is.na(base_pobreza$pobreza) ==
                                                             FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                      TRUE) * 100
#Porcentaje de la poblaci?n total con carencia por acceso a la salud
tabstat_mean[33, 10] = weighted.mean(base_pobreza$ic_asalud[is.na(base_pobreza$pobreza) ==
                                                              FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                       TRUE) * 100
#Porcentaje de la poblaci?n total con carencia por acceso a seguridad social
tabstat_mean[33, 11] = weighted.mean(base_pobreza$ic_segsoc[is.na(base_pobreza$pobreza) ==
                                                              FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                       TRUE) * 100
#Porcentaje de la poblaci?n total con carencia por calidad y espacios de la vivienda
tabstat_mean[33, 12] = weighted.mean(base_pobreza$ic_cv[is.na(base_pobreza$pobreza) ==
                                                          FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                       TRUE) * 100
#Porcentaje de la poblaci?n total con carencia por servicios b?sicos de la vivienda
tabstat_mean[33, 13] = weighted.mean(base_pobreza$ic_sbv[is.na(base_pobreza$pobreza) ==
                                                           FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                       TRUE) * 100
#Porcentaje de la poblaci?n total con carencia por acceso a la acc_alimentaci?n
tabstat_mean[33, 14] = weighted.mean(base_pobreza$ic_ali[is.na(base_pobreza$pobreza) ==
                                                           FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                       TRUE) * 100
#Porcentaje de la poblaci?n total con carencia por servicios b?sicos de la vivienda
tabstat_mean[33, 15] = weighted.mean(base_pobreza$plb_m[is.na(base_pobreza$pobreza) ==
                                                          FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                       TRUE) * 100
#Porcentaje de la poblaci?n total con carencia por acceso a la acc_alimentaci?n
tabstat_mean[33, 16] = weighted.mean(base_pobreza$plb[is.na(base_pobreza$pobreza) ==
                                                        FALSE], base_pobreza$factor[is.na(base_pobreza$pobreza) == FALSE], na.rm =
                                       TRUE) * 100

#ESTATAL

for (i in 1:32) {
  tabstat_mean[i, 1] = p.pobreza[[i]][1]
  tabstat_mean[i, 2] = p.pobreza_m[[i]][1]
  tabstat_mean[i, 3] = p.pobreza_e[[i]][1]
  tabstat_mean[i, 4] = p.vul_car[[i]][1]
  tabstat_mean[i, 5] = p.vul_ing[[i]][1]
  tabstat_mean[i, 6] = p.no_pobv[[i]][1]
  tabstat_mean[i, 7] = p.carencias[[i]][1]
  tabstat_mean[i, 8] = p.carencias3[[i]][1]
  tabstat_mean[i, 9] = p.ic_rezedu[[i]][1]
  tabstat_mean[i, 10] = p.ic_asalud[[i]][1]
  tabstat_mean[i, 11] = p.ic_segsoc[[i]][1]
  tabstat_mean[i, 12] = p.ic_cev[[i]][1]
  tabstat_mean[i, 13] = p.ic_sbv[[i]][1]
  tabstat_mean[i, 14] = p.ic_ali[[i]][1]
  tabstat_mean[i, 15] = p.plb_m[[i]][1]
  tabstat_mean[i, 16] = p.plb[[i]][1]
}

tabstat_sum
tabstat_mean

write.csv(tabstat_sum, 'Bases/Indicadores_total_16.csv', row.names = TRUE)
write.dbf(tabstat_sum, 'Bases/Indicadores_total_16.dbf')

write.csv(tabstat_mean, 'Bases/Indicadores_mean_16.csv', row.names = TRUE)
write.dbf(tabstat_mean, 'Bases/Indicadores_mean_16.dbf')

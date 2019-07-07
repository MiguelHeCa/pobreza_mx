
# Ingreso no monetario ----------------------------------------------------

columnas <- colnames(nomonetario)

df_o <- nomonetario %>% arrange(folioviv, foliohog, clave, decena, gasnomon)
df_n <- no_monetario %>% select(columnas) %>% arrange(folioviv, foliohog, clave, decena, gasnomon)

compare(df_o, df_n)

df_o_ <- df_o %>% select(folioviv, foliohog, clave, decena, gasnomon)
df_n_ <- df_n %>% select(folioviv, foliohog, clave, decena, gasnomon)
compare(df_n_, df_o_)

# Código refactorizado ----------------------------------------------------

df_n_ <- df_n_ %>% 
  # Gastos deflactados en:
  mutate(
    # Alimentos (semanal)
    ali_nm = if_else(
      clave %in% paste0("A", sprintf("%03d", c(1:222, 242:247))),
      true = case_when(
        decena %in% c(1:3) ~ gasnomon / deflactores_$R1.1$w08,
        decena %in% c(4:6) ~ gasnomon / deflactores_$R1.1$w09,
        decena %in% c(7:9) ~ gasnomon / deflactores_$R1.1$w10,
        decena == 0        ~ gasnomon / deflactores_$R1.1$w11
      ),
      false = NA_real_
    ),
    # Alcohol y tabaco (semanal)
    alta_nm = if_else(
      clave %in% paste0("A", sprintf("%03d", c(223:241))),
      true = case_when(
        decena %in% c(1:3) ~ gasnomon / deflactores_$R1.2$w08,
        decena %in% c(4:6) ~ gasnomon / deflactores_$R1.2$w09,
        decena %in% c(7:9) ~ gasnomon / deflactores_$R1.2$w10,
        decena == 0        ~ gasnomon / deflactores_$R1.2$w11
      ),
      false = NA_real_
    ),
    # Vestido y calzado (trimestral)
    veca_nm = if_else(
      clave %in% paste0("H", sprintf("%03d", c(1:122, 136))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R2$t05,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R2$t06,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R2$t07,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R2$t08
      ),
      false = NA_real_
    ),
    # Vivienda y servicios de conservación (mensual)
    viv_nm = if_else(
      clave %in% paste0("G", sprintf("%03d", c(1:16))) |
        clave %in% paste0("R", sprintf("%03d", c(1:4, 13))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R3$m07,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R3$m08,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R3$m09,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R3$m10
      ),
      false = NA_real_
    ),
    # Artículos de limpieza (mensual)
    lim_nm = if_else(
      clave %in% paste0("C", sprintf("%03d", c(1:24))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R4.2a$m07,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R4.2a$m08,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R4.2a$m09,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R4.2a$m10
      ),
      false = NA_real_
    ),
    # Cristalería y blancos (trimestral)
    cris_nm = if_else(
      clave %in% paste0("I", sprintf("%03d", c(1:26))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R4.2b$t05,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R4.2b$t06,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R4.2b$t07,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R4.2b$t08
      ),
      false = NA_real_
    ),
    # Enseres domésticos y muebles (semestral)
    ens_nm = if_else(
      clave %in% paste0("K", sprintf("%03d", c(1:37))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R4.1$s02,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R4.1$s03,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R4.1$s04,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R4.1$s05
      ),
      false = NA_real_
    ),
    # Salud (trimestral)
    sal_nm = if_else(
      clave %in% paste0("J", sprintf("%03d", c(1:72))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R5.1$t05,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R5.1$t06,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R5.1$t07,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R5.1$t08
      ),
      false = NA_real_
    ),
    # Transporte público (semanal)
    tpub_nm = if_else(
      clave %in% paste0("B", sprintf("%03d", c(1:7))),
      true = case_when(
        decena %in% c(1:3) ~ gasnomon / deflactores_$R6.1.1$w08,
        decena %in% c(4:6) ~ gasnomon / deflactores_$R6.1.1$w09,
        decena %in% c(7:9) ~ gasnomon / deflactores_$R6.1.1$w10,
        decena == 0        ~ gasnomon / deflactores_$R6.1.1$w11
      ),
      false = NA_real_
    ),
    # Transporte foráneo (semestral)
    tfor_nm = if_else(
      clave %in% paste0("M", sprintf("%03d", c(1:18))) |
        clave %in% paste0("F", sprintf("%03d", c(7:14))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R6b$s02,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R6b$s03,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R6b$s04,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R6b$s05
      ),
      false = NA_real_
    ),
    # Comunicaciones (mensual)
    com_nm = if_else(
      clave %in% paste0("F", sprintf("%03d", c(1:6))) |
        clave %in% paste0("R", sprintf("%03d", c(5:8, 10:11))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R6a$m07,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R6a$m08,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R6a$m09,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R6a$m10
      ),
      false = NA_real_
    ),
    # Educación y recreación (mensual)
    edre_nm = if_else(
      clave %in% paste0("E", sprintf("%03d", c(1:34))) |
        clave %in% paste0("H", sprintf("%03d", c(134:135))) |
        clave %in% paste0("L", sprintf("%03d", c(1:29))) |
        clave %in% paste0("N", sprintf("%03d", c(3:5))) |
        clave == "R009",
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R7$m07,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R7$m08,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R7$m09,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R7$m10
      ),
      false = NA_real_
    ),
    # Educación básica (mensual)
    edba_nm = if_else(
      clave %in% paste0("E", sprintf("%03d", c(2:3))) |
        clave %in% paste0("H", sprintf("%03d", c(134:135))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R7$m07,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R7$m08,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R7$m09,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R7$m10
      ),
      false = NA_real_
    ),
    # Cuidado personal (mensual)
    cuip_nm = if_else(
      clave %in% paste0("D", sprintf("%03d", c(1:26))) |
        clave == "H132",
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R2.3a$m07,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R2.3a$m08,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R2.3a$m09,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R2.3a$m10
      ),
      false = NA_real_
    ),
    # Accesorios personales (trimestral)
    accp_nm = if_else(
      clave %in% paste0("H", sprintf("%03d", c(123:131, 133))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$R2.3b$t05,
        decena %in% c(3:5) ~ gasnomon / deflactores_$R2.3b$t06,
        decena %in% c(6:8) ~ gasnomon / deflactores_$R2.3b$t07,
        decena %in% c(9,0) ~ gasnomon / deflactores_$R2.3b$t08
      ),
      false = NA_real_
    ),
    # Otros gastos y transferencias (semestral)
    otr_nm = if_else(
      clave %in% paste0("N", sprintf("%03d", c(1:2, 6:16))) |
        clave %in% paste0("T", sprintf("%03d", c(901:915))) |
        clave == "R012",
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$INPC$s02,
        decena %in% c(3:5) ~ gasnomon / deflactores_$INPC$s03,
        decena %in% c(6:8) ~ gasnomon / deflactores_$INPC$s04,
        decena %in% c(9,0) ~ gasnomon / deflactores_$INPC$s05
      ),
      false = NA_real_
    ),
    # Regalos otorgados (semestral)
    reda_nm = if_else(
      clave == "N013" |
        clave %in% paste0("T", sprintf("%03d", c(901:915))),
      true = case_when(
        decena %in% c(1:2) ~ gasnomon / deflactores_$INPC$s02,
        decena %in% c(3:5) ~ gasnomon / deflactores_$INPC$s03,
        decena %in% c(6:8) ~ gasnomon / deflactores_$INPC$s04,
        decena %in% c(9,0) ~ gasnomon / deflactores_$INPC$s05
      ),
      false = NA_real_
    )
  )



# Código original ---------------------------------------------------------

attach(df_o_)
#Gasto en acc_alimentos deflactado (semanal)
for (i in 1:222) {
  gasto_ali = 1000 + i
  string = paste("A", substr(as.character(gasto_ali), 2, 4),  sep = "")
  df_o_$ali_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}

for (i in 242:247) {
  gas_ali = 1000 + i
  string = paste("A", substr(as.character(gas_ali), 2, 4),  sep = "")
  df_o_$ali_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
#detach(df_o_)

df_o_$ali_nm[(decena == 1 &
                is.na(decena) != TRUE)] = df_o_$ali_nm[(decena == 1 &
                                                          is.na(decena) != TRUE)] / d11w08
df_o_$ali_nm[(decena == 2 &
                is.na(decena) != TRUE)] = df_o_$ali_nm[(decena == 2 &
                                                          is.na(decena) != TRUE)] / d11w08
df_o_$ali_nm[(decena == 3 &
                is.na(decena) != TRUE)] = df_o_$ali_nm[(decena == 3 &
                                                          is.na(decena) != TRUE)] / d11w08
df_o_$ali_nm[(decena == 4 &
                is.na(decena) != TRUE)] = df_o_$ali_nm[(decena == 4 &
                                                          is.na(decena) != TRUE)] / d11w09
df_o_$ali_nm[(decena == 5 &
                is.na(decena) != TRUE)] = df_o_$ali_nm[(decena == 5 &
                                                          is.na(decena) != TRUE)] / d11w09
df_o_$ali_nm[(decena == 6 &
                is.na(decena) != TRUE)] = df_o_$ali_nm[(decena == 6 &
                                                          is.na(decena) != TRUE)] / d11w09
df_o_$ali_nm[(decena == 7 &
                is.na(decena) != TRUE)] = df_o_$ali_nm[(decena == 7 &
                                                          is.na(decena) != TRUE)] / d11w10
df_o_$ali_nm[(decena == 8 &
                is.na(decena) != TRUE)] = df_o_$ali_nm[(decena == 8 &
                                                          is.na(decena) != TRUE)] / d11w10
df_o_$ali_nm[(decena == 9 &
                is.na(decena) != TRUE)] = df_o_$ali_nm[(decena == 9 &
                                                          is.na(decena) != TRUE)] / d11w10
df_o_$ali_nm[(decena == 0 &
                is.na(decena) != TRUE)] = df_o_$ali_nm[(decena == 0 &
                                                          is.na(decena) != TRUE)] / d11w11



#Gasto en Alcohol y tabaco deflactado (semanal)

for (i in 223:241) {
  gas_alctab = 1000 + i
  string = paste("A", substr(as.character(gas_alctab), 2, 4),  sep = "")
  df_o_$alta_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
df_o_$alta_nm[(decena == 1 &
                 is.na(decena) != TRUE)] = df_o_$alta_nm[(decena == 1 &
                                                            is.na(decena) != TRUE)] / d12w08
df_o_$alta_nm[(decena == 2 &
                 is.na(decena) != TRUE)] = df_o_$alta_nm[(decena == 2 &
                                                            is.na(decena) != TRUE)] / d12w08
df_o_$alta_nm[(decena == 3 &
                 is.na(decena) != TRUE)] = df_o_$alta_nm[(decena == 3 &
                                                            is.na(decena) != TRUE)] / d12w08
df_o_$alta_nm[(decena == 4 &
                 is.na(decena) != TRUE)] = df_o_$alta_nm[(decena == 4 &
                                                            is.na(decena) != TRUE)] / d12w09
df_o_$alta_nm[(decena == 5 &
                 is.na(decena) != TRUE)] = df_o_$alta_nm[(decena == 5 &
                                                            is.na(decena) != TRUE)] / d12w09
df_o_$alta_nm[(decena == 6 &
                 is.na(decena) != TRUE)] = df_o_$alta_nm[(decena == 6 &
                                                            is.na(decena) != TRUE)] / d12w09
df_o_$alta_nm[(decena == 7 &
                 is.na(decena) != TRUE)] = df_o_$alta_nm[(decena == 7 &
                                                            is.na(decena) != TRUE)] / d12w10
df_o_$alta_nm[(decena == 8 &
                 is.na(decena) != TRUE)] = df_o_$alta_nm[(decena == 8 &
                                                            is.na(decena) != TRUE)] / d12w10
df_o_$alta_nm[(decena == 9 &
                 is.na(decena) != TRUE)] = df_o_$alta_nm[(decena == 9 &
                                                            is.na(decena) != TRUE)] / d12w10
df_o_$alta_nm[(decena == 0 &
                 is.na(decena) != TRUE)] = df_o_$alta_nm[(decena == 0 &
                                                            is.na(decena) != TRUE)] / d12w11

#Gasto en Vestido y calzado deflactado (trimestral)

for (i in 1:122) {
  gas_vescal = 1000 + i
  string = paste("H", substr(as.character(gas_vescal), 2, 4),  sep = "")
  df_o_$veca_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
df_o_$veca_nm[(clave == "H136" &
                 is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == "H136" &
                                                            is.na(clave) != TRUE)]

df_o_$veca_nm[(decena == 1 &
                 is.na(decena) != TRUE)] = df_o_$veca_nm[(decena == 1 &
                                                            is.na(decena) != TRUE)] / d2t05
df_o_$veca_nm[(decena == 2 &
                 is.na(decena) != TRUE)] = df_o_$veca_nm[(decena == 2 &
                                                            is.na(decena) != TRUE)] / d2t05
df_o_$veca_nm[(decena == 3 &
                 is.na(decena) != TRUE)] = df_o_$veca_nm[(decena == 3 &
                                                            is.na(decena) != TRUE)] / d2t06
df_o_$veca_nm[(decena == 4 &
                 is.na(decena) != TRUE)] = df_o_$veca_nm[(decena == 4 &
                                                            is.na(decena) != TRUE)] / d2t06
df_o_$veca_nm[(decena == 5 &
                 is.na(decena) != TRUE)] = df_o_$veca_nm[(decena == 5 &
                                                            is.na(decena) != TRUE)] / d2t06
df_o_$veca_nm[(decena == 6 &
                 is.na(decena) != TRUE)] = df_o_$veca_nm[(decena == 6 &
                                                            is.na(decena) != TRUE)] / d2t07
df_o_$veca_nm[(decena == 7 &
                 is.na(decena) != TRUE)] = df_o_$veca_nm[(decena == 7 &
                                                            is.na(decena) != TRUE)] / d2t07
df_o_$veca_nm[(decena == 8 &
                 is.na(decena) != TRUE)] = df_o_$veca_nm[(decena == 8 &
                                                            is.na(decena) != TRUE)] / d2t07
df_o_$veca_nm[(decena == 9 &
                 is.na(decena) != TRUE)] = df_o_$veca_nm[(decena == 9 &
                                                            is.na(decena) != TRUE)] / d2t08
df_o_$veca_nm[(decena == 0 &
                 is.na(decena) != TRUE)] = df_o_$veca_nm[(decena == 0 &
                                                            is.na(decena) != TRUE)] / d2t08

#Gasto en Vivienda y servicios de conservaci?n deflactado (mensual)

for (i in 1:16) {
  gas_viv = 1000 + i
  string = paste("G", substr(as.character(gas_viv), 2, 4),  sep = "")
  df_o_$viv_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
for (i in 1:4) {
  gas_viv = 1000 + i
  string = paste("R", substr(as.character(gas_viv), 2, 4),  sep = "")
  df_o_$viv_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
df_o_$viv_nm[(clave == "R013" &
                is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == "R013" &
                                                           is.na(clave) != TRUE)]

df_o_$viv_nm[(decena == 1 &
                is.na(decena) != TRUE)] = df_o_$viv_nm[(decena == 1 &
                                                          is.na(decena) != TRUE)] / d3m07
df_o_$viv_nm[(decena == 2 &
                is.na(decena) != TRUE)] = df_o_$viv_nm[(decena == 2 &
                                                          is.na(decena) != TRUE)] / d3m07
df_o_$viv_nm[(decena == 3 &
                is.na(decena) != TRUE)] = df_o_$viv_nm[(decena == 3 &
                                                          is.na(decena) != TRUE)] / d3m08
df_o_$viv_nm[(decena == 4 &
                is.na(decena) != TRUE)] = df_o_$viv_nm[(decena == 4 &
                                                          is.na(decena) != TRUE)] / d3m08
df_o_$viv_nm[(decena == 5 &
                is.na(decena) != TRUE)] = df_o_$viv_nm[(decena == 5 &
                                                          is.na(decena) != TRUE)] / d3m08
df_o_$viv_nm[(decena == 6 &
                is.na(decena) != TRUE)] = df_o_$viv_nm[(decena == 6 &
                                                          is.na(decena) != TRUE)] / d3m09
df_o_$viv_nm[(decena == 7 &
                is.na(decena) != TRUE)] = df_o_$viv_nm[(decena == 7 &
                                                          is.na(decena) != TRUE)] / d3m09
df_o_$viv_nm[(decena == 8 &
                is.na(decena) != TRUE)] = df_o_$viv_nm[(decena == 8 &
                                                          is.na(decena) != TRUE)] / d3m09
df_o_$viv_nm[(decena == 9 &
                is.na(decena) != TRUE)] = df_o_$viv_nm[(decena == 9 &
                                                          is.na(decena) != TRUE)] / d3m10
df_o_$viv_nm[(decena == 0 &
                is.na(decena) != TRUE)] = df_o_$viv_nm[(decena == 0 &
                                                          is.na(decena) != TRUE)] / d3m10

#Gasto en Art?culos de limpieza deflactado (mensual)

for (i in 1:24) {
  gas_limp = 1000 + i
  string = paste("C", substr(as.character(gas_limp), 2, 4),  sep = "")
  df_o_$lim_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
df_o_$lim_nm[(decena == 1 &
                is.na(decena) != TRUE)] = df_o_$lim_nm[(decena == 1 &
                                                          is.na(decena) != TRUE)] / d42m07
df_o_$lim_nm[(decena == 2 &
                is.na(decena) != TRUE)] = df_o_$lim_nm[(decena == 2 &
                                                          is.na(decena) != TRUE)] / d42m07
df_o_$lim_nm[(decena == 3 &
                is.na(decena) != TRUE)] = df_o_$lim_nm[(decena == 3 &
                                                          is.na(decena) != TRUE)] / d42m08
df_o_$lim_nm[(decena == 4 &
                is.na(decena) != TRUE)] = df_o_$lim_nm[(decena == 4 &
                                                          is.na(decena) != TRUE)] / d42m08
df_o_$lim_nm[(decena == 5 &
                is.na(decena) != TRUE)] = df_o_$lim_nm[(decena == 5 &
                                                          is.na(decena) != TRUE)] / d42m08
df_o_$lim_nm[(decena == 6 &
                is.na(decena) != TRUE)] = df_o_$lim_nm[(decena == 6 &
                                                          is.na(decena) != TRUE)] / d42m09
df_o_$lim_nm[(decena == 7 &
                is.na(decena) != TRUE)] = df_o_$lim_nm[(decena == 7 &
                                                          is.na(decena) != TRUE)] / d42m09
df_o_$lim_nm[(decena == 8 &
                is.na(decena) != TRUE)] = df_o_$lim_nm[(decena == 8 &
                                                          is.na(decena) != TRUE)] / d42m09
df_o_$lim_nm[(decena == 9 &
                is.na(decena) != TRUE)] = df_o_$lim_nm[(decena == 9 &
                                                          is.na(decena) != TRUE)] / d42m10
df_o_$lim_nm[(decena == 0 &
                is.na(decena) != TRUE)] = df_o_$lim_nm[(decena == 0 &
                                                          is.na(decena) != TRUE)] / d42m10

#Gasto en Cristaler?a y blancos deflactado (trimestral)

for (i in 1:26) {
  gas_cris = 1000 + i
  string = paste("I", substr(as.character(gas_cris), 2, 4),  sep = "")
  df_o_$cris_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
df_o_$cris_nm[(decena == 1 &
                 is.na(decena) != TRUE)] = df_o_$cris_nm[(decena == 1 &
                                                            is.na(decena) != TRUE)] / d42t05
df_o_$cris_nm[(decena == 2 &
                 is.na(decena) != TRUE)] = df_o_$cris_nm[(decena == 2 &
                                                            is.na(decena) != TRUE)] / d42t05
df_o_$cris_nm[(decena == 3 &
                 is.na(decena) != TRUE)] = df_o_$cris_nm[(decena == 3 &
                                                            is.na(decena) != TRUE)] / d42t06
df_o_$cris_nm[(decena == 4 &
                 is.na(decena) != TRUE)] = df_o_$cris_nm[(decena == 4 &
                                                            is.na(decena) != TRUE)] / d42t06
df_o_$cris_nm[(decena == 5 &
                 is.na(decena) != TRUE)] = df_o_$cris_nm[(decena == 5 &
                                                            is.na(decena) != TRUE)] / d42t06
df_o_$cris_nm[(decena == 6 &
                 is.na(decena) != TRUE)] = df_o_$cris_nm[(decena == 6 &
                                                            is.na(decena) != TRUE)] / d42t07
df_o_$cris_nm[(decena == 7 &
                 is.na(decena) != TRUE)] = df_o_$cris_nm[(decena == 7 &
                                                            is.na(decena) != TRUE)] / d42t07
df_o_$cris_nm[(decena == 8 &
                 is.na(decena) != TRUE)] = df_o_$cris_nm[(decena == 8 &
                                                            is.na(decena) != TRUE)] / d42t07
df_o_$cris_nm[(decena == 9 &
                 is.na(decena) != TRUE)] = df_o_$cris_nm[(decena == 9 &
                                                            is.na(decena) != TRUE)] / d42t08
df_o_$cris_nm[(decena == 0 &
                 is.na(decena) != TRUE)] = df_o_$cris_nm[(decena == 0 &
                                                            is.na(decena) != TRUE)] / d42t08

#Gasto en Enseres dom?sticos y muebles deflactado (semestral)

for (i in 1:37) {
  gas_endom = 1000 + i
  string = paste("K", substr(as.character(gas_endom), 2, 4),  sep = "")
  df_o_$ens_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
df_o_$ens_nm[(decena == 1 &
                is.na(decena) != TRUE)] = df_o_$ens_nm[(decena == 1 &
                                                          is.na(decena) != TRUE)] / d41s02
df_o_$ens_nm[(decena == 2 &
                is.na(decena) != TRUE)] = df_o_$ens_nm[(decena == 2 &
                                                          is.na(decena) != TRUE)] / d41s02
df_o_$ens_nm[(decena == 3 &
                is.na(decena) != TRUE)] = df_o_$ens_nm[(decena == 3 &
                                                          is.na(decena) != TRUE)] / d41s03
df_o_$ens_nm[(decena == 4 &
                is.na(decena) != TRUE)] = df_o_$ens_nm[(decena == 4 &
                                                          is.na(decena) != TRUE)] / d41s03
df_o_$ens_nm[(decena == 5 &
                is.na(decena) != TRUE)] = df_o_$ens_nm[(decena == 5 &
                                                          is.na(decena) != TRUE)] / d41s03
df_o_$ens_nm[(decena == 6 &
                is.na(decena) != TRUE)] = df_o_$ens_nm[(decena == 6 &
                                                          is.na(decena) != TRUE)] / d41s04
df_o_$ens_nm[(decena == 7 &
                is.na(decena) != TRUE)] = df_o_$ens_nm[(decena == 7 &
                                                          is.na(decena) != TRUE)] / d41s04
df_o_$ens_nm[(decena == 8 &
                is.na(decena) != TRUE)] = df_o_$ens_nm[(decena == 8 &
                                                          is.na(decena) != TRUE)] / d41s04
df_o_$ens_nm[(decena == 9 &
                is.na(decena) != TRUE)] = df_o_$ens_nm[(decena == 9 &
                                                          is.na(decena) != TRUE)] / d41s05
df_o_$ens_nm[(decena == 0 &
                is.na(decena) != TRUE)] = df_o_$ens_nm[(decena == 0 &
                                                          is.na(decena) != TRUE)] / d41s05

#Gasto en Salud deflactado (trimestral)

for (i in 1:72) {
  gas_salud = 1000 + i
  string = paste("J", substr(as.character(gas_salud), 2, 4),  sep = "")
  df_o_$sal_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
df_o_$sal_nm[(decena == 1 &
                is.na(decena) != TRUE)] = df_o_$sal_nm[(decena == 1 &
                                                          is.na(decena) != TRUE)] / d51t05
df_o_$sal_nm[(decena == 2 &
                is.na(decena) != TRUE)] = df_o_$sal_nm[(decena == 2 &
                                                          is.na(decena) != TRUE)] / d51t05
df_o_$sal_nm[(decena == 3 &
                is.na(decena) != TRUE)] = df_o_$sal_nm[(decena == 3 &
                                                          is.na(decena) != TRUE)] / d51t06
df_o_$sal_nm[(decena == 4 &
                is.na(decena) != TRUE)] = df_o_$sal_nm[(decena == 4 &
                                                          is.na(decena) != TRUE)] / d51t06
df_o_$sal_nm[(decena == 5 &
                is.na(decena) != TRUE)] = df_o_$sal_nm[(decena == 5 &
                                                          is.na(decena) != TRUE)] / d51t06
df_o_$sal_nm[(decena == 6 &
                is.na(decena) != TRUE)] = df_o_$sal_nm[(decena == 6 &
                                                          is.na(decena) != TRUE)] / d51t07
df_o_$sal_nm[(decena == 7 &
                is.na(decena) != TRUE)] = df_o_$sal_nm[(decena == 7 &
                                                          is.na(decena) != TRUE)] / d51t07
df_o_$sal_nm[(decena == 8 &
                is.na(decena) != TRUE)] = df_o_$sal_nm[(decena == 8 &
                                                          is.na(decena) != TRUE)] / d51t07
df_o_$sal_nm[(decena == 9 &
                is.na(decena) != TRUE)] = df_o_$sal_nm[(decena == 9 &
                                                          is.na(decena) != TRUE)] / d51t08
df_o_$sal_nm[(decena == 0 &
                is.na(decena) != TRUE)] = df_o_$sal_nm[(decena == 0 &
                                                          is.na(decena) != TRUE)] / d51t08

#Gasto en Transporte p?blico deflactado (semanal)

for (i in 1:7) {
  gas_transpub = 1000 + i
  string = paste("B", substr(as.character(gas_transpub), 2, 4),  sep = "")
  df_o_$tpub_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
df_o_$tpub_nm[(decena == 1 &
                 is.na(decena) != TRUE)] = df_o_$tpub_nm[(decena == 1 &
                                                            is.na(decena) != TRUE)] / d611w08
df_o_$tpub_nm[(decena == 2 &
                 is.na(decena) != TRUE)] = df_o_$tpub_nm[(decena == 2 &
                                                            is.na(decena) != TRUE)] / d611w08
df_o_$tpub_nm[(decena == 3 &
                 is.na(decena) != TRUE)] = df_o_$tpub_nm[(decena == 3 &
                                                            is.na(decena) != TRUE)] / d611w08
df_o_$tpub_nm[(decena == 4 &
                 is.na(decena) != TRUE)] = df_o_$tpub_nm[(decena == 4 &
                                                            is.na(decena) != TRUE)] / d611w09
df_o_$tpub_nm[(decena == 5 &
                 is.na(decena) != TRUE)] = df_o_$tpub_nm[(decena == 5 &
                                                            is.na(decena) != TRUE)] / d611w09
df_o_$tpub_nm[(decena == 6 &
                 is.na(decena) != TRUE)] = df_o_$tpub_nm[(decena == 6 &
                                                            is.na(decena) != TRUE)] / d611w09
df_o_$tpub_nm[(decena == 7 &
                 is.na(decena) != TRUE)] = df_o_$tpub_nm[(decena == 7 &
                                                            is.na(decena) != TRUE)] / d611w10
df_o_$tpub_nm[(decena == 8 &
                 is.na(decena) != TRUE)] = df_o_$tpub_nm[(decena == 8 &
                                                            is.na(decena) != TRUE)] / d611w10
df_o_$tpub_nm[(decena == 9 &
                 is.na(decena) != TRUE)] = df_o_$tpub_nm[(decena == 9 &
                                                            is.na(decena) != TRUE)] / d611w10
df_o_$tpub_nm[(decena == 0 &
                 is.na(decena) != TRUE)] = df_o_$tpub_nm[(decena == 0 &
                                                            is.na(decena) != TRUE)] / d611w11

#Gasto en Transporte for?neo deflactado (semestral)

for (i in 1:18) {
  gas_transfor = 1000 + i
  string = paste("M", substr(as.character(gas_transfor), 2, 4),  sep = "")
  df_o_$tfor_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
for (i in 7:14) {
  gas_transfor = 1000 + i
  string = paste("F", substr(as.character(gas_transfor), 2, 4),  sep = "")
  df_o_$tfor_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
df_o_$tfor_nm[(decena == 1 &
                 is.na(decena) != TRUE)] = df_o_$tfor_nm[(decena == 1 &
                                                            is.na(decena) != TRUE)] / d6s02
df_o_$tfor_nm[(decena == 2 &
                 is.na(decena) != TRUE)] = df_o_$tfor_nm[(decena == 2 &
                                                            is.na(decena) != TRUE)] / d6s02
df_o_$tfor_nm[(decena == 3 &
                 is.na(decena) != TRUE)] = df_o_$tfor_nm[(decena == 3 &
                                                            is.na(decena) != TRUE)] / d6s03
df_o_$tfor_nm[(decena == 4 &
                 is.na(decena) != TRUE)] = df_o_$tfor_nm[(decena == 4 &
                                                            is.na(decena) != TRUE)] / d6s03
df_o_$tfor_nm[(decena == 5 &
                 is.na(decena) != TRUE)] = df_o_$tfor_nm[(decena == 5 &
                                                            is.na(decena) != TRUE)] / d6s03
df_o_$tfor_nm[(decena == 6 &
                 is.na(decena) != TRUE)] = df_o_$tfor_nm[(decena == 6 &
                                                            is.na(decena) != TRUE)] / d6s04
df_o_$tfor_nm[(decena == 7 &
                 is.na(decena) != TRUE)] = df_o_$tfor_nm[(decena == 7 &
                                                            is.na(decena) != TRUE)] / d6s04
df_o_$tfor_nm[(decena == 8 &
                 is.na(decena) != TRUE)] = df_o_$tfor_nm[(decena == 8 &
                                                            is.na(decena) != TRUE)] / d6s04
df_o_$tfor_nm[(decena == 9 &
                 is.na(decena) != TRUE)] = df_o_$tfor_nm[(decena == 9 &
                                                            is.na(decena) != TRUE)] / d6s05
df_o_$tfor_nm[(decena == 0 &
                 is.na(decena) != TRUE)] = df_o_$tfor_nm[(decena == 0 &
                                                            is.na(decena) != TRUE)] / d6s05

#Gasto en Comunicaciones deflactado (mensual)

for (i in 1:6) {
  gas_com = 1000 + i
  string = paste("F", substr(as.character(gas_com), 2, 4),  sep = "")
  df_o_$com_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
for (i in 5:8) {
  gas_com = 1000 + i
  string = paste("R", substr(as.character(gas_com), 2, 4),  sep = "")
  df_o_$com_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
for (i in 10:11) {
  gas_com = 1000 + i
  string = paste("R", substr(as.character(gas_com), 2, 4),  sep = "")
  df_o_$com_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
df_o_$com_nm[(decena == 1 &
                is.na(decena) != TRUE)] = df_o_$com_nm[(decena == 1 &
                                                          is.na(decena) != TRUE)] / d6m07
df_o_$com_nm[(decena == 2 &
                is.na(decena) != TRUE)] = df_o_$com_nm[(decena == 2 &
                                                          is.na(decena) != TRUE)] / d6m07
df_o_$com_nm[(decena == 3 &
                is.na(decena) != TRUE)] = df_o_$com_nm[(decena == 3 &
                                                          is.na(decena) != TRUE)] / d6m08
df_o_$com_nm[(decena == 4 &
                is.na(decena) != TRUE)] = df_o_$com_nm[(decena == 4 &
                                                          is.na(decena) != TRUE)] / d6m08
df_o_$com_nm[(decena == 5 &
                is.na(decena) != TRUE)] = df_o_$com_nm[(decena == 5 &
                                                          is.na(decena) != TRUE)] / d6m08
df_o_$com_nm[(decena == 6 &
                is.na(decena) != TRUE)] = df_o_$com_nm[(decena == 6 &
                                                          is.na(decena) != TRUE)] / d6m09
df_o_$com_nm[(decena == 7 &
                is.na(decena) != TRUE)] = df_o_$com_nm[(decena == 7 &
                                                          is.na(decena) != TRUE)] / d6m09
df_o_$com_nm[(decena == 8 &
                is.na(decena) != TRUE)] = df_o_$com_nm[(decena == 8 &
                                                          is.na(decena) != TRUE)] / d6m09
df_o_$com_nm[(decena == 9 &
                is.na(decena) != TRUE)] = df_o_$com_nm[(decena == 9 &
                                                          is.na(decena) != TRUE)] / d6m10
df_o_$com_nm[(decena == 0 &
                is.na(decena) != TRUE)] = df_o_$com_nm[(decena == 0 &
                                                          is.na(decena) != TRUE)] / d6m10

#Gasto en Educaci?n y recreaci?n deflactado (mensual)

for (i in 1:34) {
  gas_edurec = 1000 + i
  string = paste("E", substr(as.character(gas_edurec), 2, 4),  sep = "")
  df_o_$edre_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
for (i in 134:135) {
  gas_edurec = 1000 + i
  string = paste("H", substr(as.character(gas_edurec), 2, 4),  sep = "")
  df_o_$edre_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
for (i in 1:29) {
  gas_edurec = 1000 + i
  string = paste("L", substr(as.character(gas_edurec), 2, 4),  sep = "")
  df_o_$edre_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
for (i in 3:5) {
  gas_edurec = 1000 + i
  string = paste("N", substr(as.character(gas_edurec), 2, 4),  sep = "")
  df_o_$edre_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
df_o_$edre_nm[(clave == "R009" &
                 is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == "R009" &
                                                            is.na(clave) != TRUE)]

df_o_$edre_nm[(decena == 1 &
                 is.na(decena) != TRUE)] = df_o_$edre_nm[(decena == 1 &
                                                            is.na(decena) != TRUE)] / d7m07
df_o_$edre_nm[(decena == 2 &
                 is.na(decena) != TRUE)] = df_o_$edre_nm[(decena == 2 &
                                                            is.na(decena) != TRUE)] / d7m07
df_o_$edre_nm[(decena == 3 &
                 is.na(decena) != TRUE)] = df_o_$edre_nm[(decena == 3 &
                                                            is.na(decena) != TRUE)] / d7m08
df_o_$edre_nm[(decena == 4 &
                 is.na(decena) != TRUE)] = df_o_$edre_nm[(decena == 4 &
                                                            is.na(decena) != TRUE)] / d7m08
df_o_$edre_nm[(decena == 5 &
                 is.na(decena) != TRUE)] = df_o_$edre_nm[(decena == 5 &
                                                            is.na(decena) != TRUE)] / d7m08
df_o_$edre_nm[(decena == 6 &
                 is.na(decena) != TRUE)] = df_o_$edre_nm[(decena == 6 &
                                                            is.na(decena) != TRUE)] / d7m09
df_o_$edre_nm[(decena == 7 &
                 is.na(decena) != TRUE)] = df_o_$edre_nm[(decena == 7 &
                                                            is.na(decena) != TRUE)] / d7m09
df_o_$edre_nm[(decena == 8 &
                 is.na(decena) != TRUE)] = df_o_$edre_nm[(decena == 8 &
                                                            is.na(decena) != TRUE)] / d7m09
df_o_$edre_nm[(decena == 9 &
                 is.na(decena) != TRUE)] = df_o_$edre_nm[(decena == 9 &
                                                            is.na(decena) != TRUE)] / d7m10
df_o_$edre_nm[(decena == 0 &
                 is.na(decena) != TRUE)] = df_o_$edre_nm[(decena == 0 &
                                                            is.na(decena) != TRUE)] / d7m10

#Gasto en Educaci?n b?sica deflactado (mensual)

for (i in 2:3) {
  gas_edubas = 1000 + i
  string = paste("E", substr(as.character(gas_edubas), 2, 4),  sep = "")
  df_o_$edba_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
for (i in 134:135) {
  gas_edubas = 1000 + i
  string = paste("H", substr(as.character(gas_edubas), 2, 4),  sep = "")
  df_o_$edba_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
df_o_$edba_nm[(decena == 1 &
                 is.na(decena) != TRUE)] = df_o_$edba_nm[(decena == 1 &
                                                            is.na(decena) != TRUE)] / d7m07
df_o_$edba_nm[(decena == 2 &
                 is.na(decena) != TRUE)] = df_o_$edba_nm[(decena == 2 &
                                                            is.na(decena) != TRUE)] / d7m07
df_o_$edba_nm[(decena == 3 &
                 is.na(decena) != TRUE)] = df_o_$edba_nm[(decena == 3 &
                                                            is.na(decena) != TRUE)] / d7m08
df_o_$edba_nm[(decena == 4 &
                 is.na(decena) != TRUE)] = df_o_$edba_nm[(decena == 4 &
                                                            is.na(decena) != TRUE)] / d7m08
df_o_$edba_nm[(decena == 5 &
                 is.na(decena) != TRUE)] = df_o_$edba_nm[(decena == 5 &
                                                            is.na(decena) != TRUE)] / d7m08
df_o_$edba_nm[(decena == 6 &
                 is.na(decena) != TRUE)] = df_o_$edba_nm[(decena == 6 &
                                                            is.na(decena) != TRUE)] / d7m09
df_o_$edba_nm[(decena == 7 &
                 is.na(decena) != TRUE)] = df_o_$edba_nm[(decena == 7 &
                                                            is.na(decena) != TRUE)] / d7m09
df_o_$edba_nm[(decena == 8 &
                 is.na(decena) != TRUE)] = df_o_$edba_nm[(decena == 8 &
                                                            is.na(decena) != TRUE)] / d7m09
df_o_$edba_nm[(decena == 9 &
                 is.na(decena) != TRUE)] = df_o_$edba_nm[(decena == 9 &
                                                            is.na(decena) != TRUE)] / d7m10
df_o_$edba_nm[(decena == 0 &
                 is.na(decena) != TRUE)] = df_o_$edba_nm[(decena == 0 &
                                                            is.na(decena) != TRUE)] / d7m10

#Gasto en Cuidado personal deflactado (mensual)

for (i in 1:26) {
  gas_cuiper = 1000 + i
  string = paste("D", substr(as.character(gas_cuiper), 2, 4),  sep = "")
  df_o_$cuip_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
df_o_$cuip_nm[(clave == "H132" &
                 is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == "H132" &
                                                            is.na(clave) != TRUE)]

df_o_$cuip_nm[(decena == 1 &
                 is.na(decena) != TRUE)] = df_o_$cuip_nm[(decena == 1 &
                                                            is.na(decena) != TRUE)] / d23m07
df_o_$cuip_nm[(decena == 2 &
                 is.na(decena) != TRUE)] = df_o_$cuip_nm[(decena == 2 &
                                                            is.na(decena) != TRUE)] / d23m07
df_o_$cuip_nm[(decena == 3 &
                 is.na(decena) != TRUE)] = df_o_$cuip_nm[(decena == 3 &
                                                            is.na(decena) != TRUE)] / d23m08
df_o_$cuip_nm[(decena == 4 &
                 is.na(decena) != TRUE)] = df_o_$cuip_nm[(decena == 4 &
                                                            is.na(decena) != TRUE)] / d23m08
df_o_$cuip_nm[(decena == 5 &
                 is.na(decena) != TRUE)] = df_o_$cuip_nm[(decena == 5 &
                                                            is.na(decena) != TRUE)] / d23m08
df_o_$cuip_nm[(decena == 6 &
                 is.na(decena) != TRUE)] = df_o_$cuip_nm[(decena == 6 &
                                                            is.na(decena) != TRUE)] / d23m09
df_o_$cuip_nm[(decena == 7 &
                 is.na(decena) != TRUE)] = df_o_$cuip_nm[(decena == 7 &
                                                            is.na(decena) != TRUE)] / d23m09
df_o_$cuip_nm[(decena == 8 &
                 is.na(decena) != TRUE)] = df_o_$cuip_nm[(decena == 8 &
                                                            is.na(decena) != TRUE)] / d23m09
df_o_$cuip_nm[(decena == 9 &
                 is.na(decena) != TRUE)] = df_o_$cuip_nm[(decena == 9 &
                                                            is.na(decena) != TRUE)] / d23m10
df_o_$cuip_nm[(decena == 0 &
                 is.na(decena) != TRUE)] = df_o_$cuip_nm[(decena == 0 &
                                                            is.na(decena) != TRUE)] / d23m10

#Gasto en Accesorios personales deflactado (trimestral)

for (i in 123:131) {
  gas_accper = 1000 + i
  string = paste("H", substr(as.character(gas_accper), 2, 4),  sep = "")
  df_o_$accp_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
df_o_$accp_nm[(clave == "H133" &
                 is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == "H133" &
                                                            is.na(clave) != TRUE)]

df_o_$accp_nm[(decena == 1 &
                 is.na(decena) != TRUE)] = df_o_$accp_nm[(decena == 1 &
                                                            is.na(decena) != TRUE)] / d23t05
df_o_$accp_nm[(decena == 2 &
                 is.na(decena) != TRUE)] = df_o_$accp_nm[(decena == 2 &
                                                            is.na(decena) != TRUE)] / d23t05
df_o_$accp_nm[(decena == 3 &
                 is.na(decena) != TRUE)] = df_o_$accp_nm[(decena == 3 &
                                                            is.na(decena) != TRUE)] / d23t06
df_o_$accp_nm[(decena == 4 &
                 is.na(decena) != TRUE)] = df_o_$accp_nm[(decena == 4 &
                                                            is.na(decena) != TRUE)] / d23t06
df_o_$accp_nm[(decena == 5 &
                 is.na(decena) != TRUE)] = df_o_$accp_nm[(decena == 5 &
                                                            is.na(decena) != TRUE)] / d23t06
df_o_$accp_nm[(decena == 6 &
                 is.na(decena) != TRUE)] = df_o_$accp_nm[(decena == 6 &
                                                            is.na(decena) != TRUE)] / d23t07
df_o_$accp_nm[(decena == 7 &
                 is.na(decena) != TRUE)] = df_o_$accp_nm[(decena == 7 &
                                                            is.na(decena) != TRUE)] / d23t07
df_o_$accp_nm[(decena == 8 &
                 is.na(decena) != TRUE)] = df_o_$accp_nm[(decena == 8 &
                                                            is.na(decena) != TRUE)] / d23t07
df_o_$accp_nm[(decena == 9 &
                 is.na(decena) != TRUE)] = df_o_$accp_nm[(decena == 9 &
                                                            is.na(decena) != TRUE)] / d23t08
df_o_$accp_nm[(decena == 0 &
                 is.na(decena) != TRUE)] = df_o_$accp_nm[(decena == 0 &
                                                            is.na(decena) != TRUE)] / d23t08

#Gasto en Otros gastos y transferencias deflactado (semestral)

for (i in 1:2) {
  gas_otro = 1000 + i
  string = paste("N", substr(as.character(gas_otro), 2, 4),  sep = "")
  df_o_$otr_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
for (i in 6:16) {
  gas_otro = 1000 + i
  string = paste("N", substr(as.character(gas_otro), 2, 4),  sep = "")
  df_o_$otr_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
for (i in 901:915) {
  gas_otro = 1000 + i
  string = paste("T", substr(as.character(gas_otro), 2, 4),  sep = "")
  df_o_$otr_nm[(clave == string &
                  is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                             is.na(clave) != TRUE)]
}
df_o_$otr_nm[(clave == "R012" &
                is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == "R012" &
                                                           is.na(clave) != TRUE)]

df_o_$otr_nm[(decena == 1 &
                is.na(decena) != TRUE)] = df_o_$otr_nm[(decena == 1 &
                                                          is.na(decena) != TRUE)] / dINPCs02
df_o_$otr_nm[(decena == 2 &
                is.na(decena) != TRUE)] = df_o_$otr_nm[(decena == 2 &
                                                          is.na(decena) != TRUE)] / dINPCs02
df_o_$otr_nm[(decena == 3 &
                is.na(decena) != TRUE)] = df_o_$otr_nm[(decena == 3 &
                                                          is.na(decena) != TRUE)] / dINPCs03
df_o_$otr_nm[(decena == 4 &
                is.na(decena) != TRUE)] = df_o_$otr_nm[(decena == 4 &
                                                          is.na(decena) != TRUE)] / dINPCs03
df_o_$otr_nm[(decena == 5 &
                is.na(decena) != TRUE)] = df_o_$otr_nm[(decena == 5 &
                                                          is.na(decena) != TRUE)] / dINPCs03
df_o_$otr_nm[(decena == 6 &
                is.na(decena) != TRUE)] = df_o_$otr_nm[(decena == 6 &
                                                          is.na(decena) != TRUE)] / dINPCs04
df_o_$otr_nm[(decena == 7 &
                is.na(decena) != TRUE)] = df_o_$otr_nm[(decena == 7 &
                                                          is.na(decena) != TRUE)] / dINPCs04
df_o_$otr_nm[(decena == 8 &
                is.na(decena) != TRUE)] = df_o_$otr_nm[(decena == 8 &
                                                          is.na(decena) != TRUE)] / dINPCs04
df_o_$otr_nm[(decena == 9 &
                is.na(decena) != TRUE)] = df_o_$otr_nm[(decena == 9 &
                                                          is.na(decena) != TRUE)] / dINPCs05
df_o_$otr_nm[(decena == 0 &
                is.na(decena) != TRUE)] = df_o_$otr_nm[(decena == 0 &
                                                          is.na(decena) != TRUE)] / dINPCs05

#Gasto en Regalos Otorgados deflactado

for (i in 901:915) {
  gas_reg = 1000 + i
  string = paste("T", substr(as.character(gas_reg), 2, 4),  sep = "")
  df_o_$reda_nm[(clave == string &
                   is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == string &
                                                              is.na(clave) != TRUE)]
}
df_o_$reda_nm[(clave == "N013" &
                 is.na(clave) != TRUE)] = df_o_$gasnomon[(clave == "N013" &
                                                            is.na(clave) != TRUE)]

df_o_$reda_nm[(decena == 1 &
                 is.na(decena) != TRUE)] = df_o_$reda_nm[(decena == 1 &
                                                            is.na(decena) != TRUE)] / dINPCs02
df_o_$reda_nm[(decena == 2 &
                 is.na(decena) != TRUE)] = df_o_$reda_nm[(decena == 2 &
                                                            is.na(decena) != TRUE)] / dINPCs02
df_o_$reda_nm[(decena == 3 &
                 is.na(decena) != TRUE)] = df_o_$reda_nm[(decena == 3 &
                                                            is.na(decena) != TRUE)] / dINPCs03
df_o_$reda_nm[(decena == 4 &
                 is.na(decena) != TRUE)] = df_o_$reda_nm[(decena == 4 &
                                                            is.na(decena) != TRUE)] / dINPCs03
df_o_$reda_nm[(decena == 5 &
                 is.na(decena) != TRUE)] = df_o_$reda_nm[(decena == 5 &
                                                            is.na(decena) != TRUE)] / dINPCs03
df_o_$reda_nm[(decena == 6 &
                 is.na(decena) != TRUE)] = df_o_$reda_nm[(decena == 6 &
                                                            is.na(decena) != TRUE)] / dINPCs04
df_o_$reda_nm[(decena == 7 &
                 is.na(decena) != TRUE)] = df_o_$reda_nm[(decena == 7 &
                                                            is.na(decena) != TRUE)] / dINPCs04
df_o_$reda_nm[(decena == 8 &
                 is.na(decena) != TRUE)] = df_o_$reda_nm[(decena == 8 &
                                                            is.na(decena) != TRUE)] / dINPCs04
df_o_$reda_nm[(decena == 9 &
                 is.na(decena) != TRUE)] = df_o_$reda_nm[(decena == 9 &
                                                            is.na(decena) != TRUE)] / dINPCs05
df_o_$reda_nm[(decena == 0 &
                 is.na(decena) != TRUE)] = df_o_$reda_nm[(decena == 0 &
                                                            is.na(decena) != TRUE)] / dINPCs05
detach(df_o_)


# pruebas finales ---------------------------------------------------------
compare(df_n_, df_o_)

nomonetario_ <- nomonetario %>%
  select(folioviv, foliohog, clave, decena, gasnomon, ali_nm:reda_nm) %>% 
  arrange(folioviv, foliohog, clave, decena, gasnomon)
no_monetario__ <- no_monetario_ %>% 
  select(folioviv, foliohog, clave, decena, gasnomon, ali_nm:reda_nm)

compare(nomonetario_, no_monetario__)


mean(nomonetario$ali_nm, na.rm = T) == mean(no_monetario_$ali_nm, na.rm = T) &
  sum(is.na(nomonetario$ali_nm)) == sum(is.na(no_monetario_$ali_nm))
mean(nomonetario$alta_nm, na.rm = T) == mean(no_monetario_$alta_nm, na.rm = T) &
  sum(is.na(nomonetario$alta_nm)) == sum(is.na(no_monetario_$alta_nm))
mean(nomonetario$veca_nm, na.rm = T) == mean(no_monetario_$veca_nm, na.rm = T) &
  sum(is.na(nomonetario$veca_nm)) == sum(is.na(no_monetario_$veca_nm))
mean(nomonetario$viv_nm, na.rm = T) == mean(no_monetario_$viv_nm, na.rm = T) &
  sum(is.na(nomonetario$viv_nm)) == sum(is.na(no_monetario_$viv_nm))
mean(nomonetario$lim_nm, na.rm = T) == mean(no_monetario_$lim_nm, na.rm = T) &
  sum(is.na(nomonetario$lim_nm)) == sum(is.na(no_monetario_$lim_nm))
mean(nomonetario$cris_nm, na.rm = T) == mean(no_monetario_$cris_nm, na.rm = T) &
  sum(is.na(nomonetario$cris_nm)) == sum(is.na(no_monetario_$cris_nm))
mean(nomonetario$ens_nm, na.rm = T) == mean(no_monetario_$ens_nm, na.rm = T) &
  sum(is.na(nomonetario$ens_nm)) == sum(is.na(no_monetario_$ens_nm))
mean(nomonetario$sal_nm, na.rm = T) == mean(no_monetario_$sal_nm, na.rm = T) &
  sum(is.na(nomonetario$sal_nm)) == sum(is.na(no_monetario_$sal_nm))
mean(nomonetario$tpub_nm, na.rm = T) == mean(no_monetario_$tpub_nm, na.rm = T) &
  sum(is.na(nomonetario$tpub_nm)) == sum(is.na(no_monetario_$tpub_nm))
mean(nomonetario$tfor_nm, na.rm = T) == mean(no_monetario_$tfor_nm, na.rm = T) &
  sum(is.na(nomonetario$tfor_nm)) == sum(is.na(no_monetario_$tfor_nm))
mean(nomonetario$com_nm, na.rm = T) == mean(no_monetario_$com_nm, na.rm = T) &
  sum(is.na(nomonetario$com_nm)) == sum(is.na(no_monetario_$com_nm))
mean(nomonetario$edre_nm, na.rm = T) == mean(no_monetario_$edre_nm, na.rm = T) &
  sum(is.na(nomonetario$edre_nm)) == sum(is.na(no_monetario_$edre_nm))
mean(nomonetario$edba_nm, na.rm = T) == mean(no_monetario_$edba_nm, na.rm = T) &
  sum(is.na(nomonetario$edba_nm)) == sum(is.na(no_monetario_$edba_nm))
mean(nomonetario$cuip_nm, na.rm = T) == mean(no_monetario_$cuip_nm, na.rm = T) &
  sum(is.na(nomonetario$cuip_nm)) == sum(is.na(no_monetario_$cuip_nm))
mean(nomonetario$accp_nm, na.rm = T) == mean(no_monetario_$accp_nm, na.rm = T) &
  sum(is.na(nomonetario$accp_nm)) == sum(is.na(no_monetario_$accp_nm))
mean(nomonetario$otr_nm, na.rm = T) == mean(no_monetario_$otr_nm, na.rm = T) &
  sum(is.na(nomonetario$otr_nm)) == sum(is.na(no_monetario_$otr_nm))
mean(nomonetario$reda_nm, na.rm = T) == mean(no_monetario_$reda_nm, na.rm = T) &
  sum(is.na(nomonetario$reda_nm)) == sum(is.na(no_monetario_$reda_nm))


# Pagos en especie --------------------------------------------------------

setequal(nomonetarioesp2, no_monetario_esp)

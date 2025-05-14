#CREACION Y CALCULO DE LAS VARIABLES FALTANTES

DATA_Manipulada<- DATA_Manipulada %>%
  mutate(Debt = Total_Liabilities / Total_Assets,
         OpInc= log(Operating_Revenue),
         AssetT = Operating_Revenue / Total_Assets ,
         StockT = Operating_Revenue / Inventory,
         Age = Seniority / 365 )%>% view()


#selección de variables(existentes en la DATA) a utilizar para la Tabla 1
DATAM_SELECT <- DATA_Manipulada %>%
  select(ROE, ROA, Ln_Total_Assets,Debt, Growth, GDP_Var, Inflation, Gender, OpInc,
         StockT, AssetT, CollectionPeriod, 
         Payment_Period, Age, Legal_Form, Country) %>%
  rename(
    Size = Ln_Total_Assets,
    VarGDP = GDP_Var,
    Inflat = Inflation,
    ARP = CollectionPeriod,
    APP = Payment_Period,
    LForm = Legal_Form
  )  %>%   view()


#aplica a todas las columnas numéricas, redondeando los valores a dos decimales.
DATA_SELECT <- DATAM_SELECT %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  view()

names(DATA_SELECT)

#FILTAR OBSERVACION SEGUN EL PAIS
OB_ES<-DATA_SELECT %>% filter(Country=="1") #ESPAÑA
OB_IT<-DATA_SELECT %>% filter(Country=="0") #ITALIA

GENERAL <- DATA_SELECT %>% select (-c (Country, LForm))%>% view() 
ESPAÑA <-OB_ES %>% select (-c (Country, LForm))%>% view() 
ITALIA<- OB_IT %>% select (-c (Country, LForm))%>% view() 


#RESUMENES ESTADISTICO POR CRITERIOS (GENERAL, ESPAÑA, ITALIA)
GENERAL %>% get_summary_stats() %>% select(variable, mean, sd, min, max)%>% view("RE G")
ESPAÑA %>% get_summary_stats() %>% select(variable, mean, sd, min, max)%>% view("RE ES")
ITALIA %>% get_summary_stats() %>% select(variable, mean, sd, min, max)%>% view("RE IT")

#TABLA 2 (FORMA 2)  ----

# Filtrar observaciones y obtener el resumen estadístico (EXCLUYENFO VALORES NO DESADOS INF)
G <- GENERAL %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .)))
RE_G<- G %>% get_summary_stats() %>% select(variable, mean, sd, min, max)%>% view("RG")


E <- ESPAÑA %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .)))
RE_E <- E %>% get_summary_stats() %>% select(variable, mean, sd, min, max)%>% view("RE")

I <- ITALIA %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .)))
RE_I<- I %>% get_summary_stats() %>% select(variable, mean, sd, min, max)%>% view("RI")

RE_UNIDO_E_I <- full_join(RE_E, RE_I, by = "variable") %>% view()

RE_UNIDO <- RE_G %>%
  full_join(RE_E, by = "variable") %>%
  full_join(RE_I, by = "variable")  %>% view()

RE_UNIDO <- RE_G %>%
  left_join(RE_E, by = "variable") %>%
  left_join(RE_I, by = "variable")  %>% view()

# Calcular p-value para cada variable
p_values <- map_dfr(RE_E$variable, ~ {
  var <- .x
  t_test <- t.test(E[[var]], I[[var]], var.equal = TRUE)
  tibble(variable = var, p_value = round(t_test$p.value, 4))  # Redondear a 4 decimales
})
# Agregar una columna de significancia con asteriscos
p_values <- map_dfr(RE_E$variable, ~ {
  var <- .x
  t_test <- t.test(E[[var]], I[[var]], var.equal = TRUE)
  p_value <- round(t_test$p.value, 4)
  
  tibble(
    variable = var,
    p_value = paste0(
      format(p_value, nsmall = 4, scientific = FALSE),  # Forzar formato decimal
      case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE ~ ""
      )
    )
  )
})


# Combinar las estadísticas descriptivas de España e Italia
spain_italy_table <- RE_E %>%
  left_join(RE_I, by = "variable", suffix = c("_spain", "_italy"))

# Agregar los p-values a la tabla combinada de España e Italia
spain_italy_table <- spain_italy_table %>%
  left_join(p_values, by = "variable")


#TABLA 3 ---- 
LF_GENERAL <- DATA_SELECT %>% count(LForm) %>% rename("Total sample" = n)%>% view("LF_GENERAL") 
LF_ESPAÑA<- OB_ES %>% count(LForm) %>% rename(Spain = n) %>% view("LF_ESPAÑA")
LF_ITALIA<- OB_IT %>% count(LForm) %>% rename(Italy = n)%>% view("LF_ITALIA")

# Unir las tablas por 'LForm' (el tipo de empresa)
T_LEGAL_FORM <- LF_GENERAL %>%
  full_join(LF_ESPAÑA, by = "LForm") %>%
  full_join(LF_ITALIA, by = "LForm")  %>% view()

T_LEGAL_FORM <- LF_GENERAL %>%
  left_join(LF_ESPAÑA, by = "LForm") %>%
  left_join(LF_ITALIA, by = "LForm")  %>% view()


# Rellenar con ceros en caso de valores faltantes
T_LEGAL_FORM[is.na(T_LEGAL_FORM)] <- 0

DATA_Manipulada %>% distinct(Standard_Legal_Form)
DATA_Manipulada %>% distinct(Legal_Form)
tipo_empresa <- c("0" = "Public limited company", 
                  "1" = "Private limited company", 
                  "3" = "Cooperative", 
                  "4" = "Other legal forms")
# Reemplazar los códigos numéricos con los nombres de tipos de empresas
T_LEGAL_FORM <-T_LEGAL_FORM %>%
  mutate(LForm = tipo_empresa[as.character(LForm)])%>% 
  rename("Legal form" = LForm)

View(T_LEGAL_FORM)

#Calculo de Chi-squared test
observed <- T_LEGAL_FORM %>%
  select(Spain, Italy) %>%
  as.matrix()

chisq_test <- chisq.test(observed)

# Extraer los valores del test y formatearlos
chi_squared_value <- round(chisq_test$statistic, 4)

# Formatear p-value manualmente (sin "<")
p_value <- ifelse(chisq_test$p.value < 0.0000, "0.0000", sprintf("%.4f", chisq_test$p.value)) 

# Agregar el resultado del Chi-cuadrado a la tabla
LEGAL_FORM_chi <- T_LEGAL_FORM %>%
  mutate(`Chi-squared test` = ifelse(row_number() == 1, 
                                     paste0(chi_squared_value, " (", p_value, ")"), 
                                     ""))

LEGAL_FORM_chi %>%
  gt() %>%
  tab_header(
    title = "Table 3. Legal form by country.",
    subtitle = ""
  ) %>%
  cols_label(
    `Legal form` = "Legal form",
    `Total sample` = "Total sample",
    Spain = "Spain",
    Italy = "Italy",
    `Chi-squared test` = html("Chi-squared test")
  ) %>%
  cols_align(
    align = "left",
    columns = `Legal form`
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Total sample`, Spain, Italy, `Chi-squared test`)
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_body(columns = everything(), rows = everything))

#TABLA 4 ----
# Generar la matriz de correlación
correlation_matrix <- G %>%
  select(ROE, ROA, Size, Debt, Growth, VarGDP, Inflat, Gender, OpInc, StockT, AssetT, ARP, APP, Age) %>%
  cor_mat(method = "pearson") %>%
  cor_mark_significant() # Añade los niveles de significancia

# Mostrar la matriz de correlación en una vista interactiva
View(correlation_matrix)
correlation_matrix  %>% gt()

#tabla 5 ----

# Calcular promedios de ROE y ROA por tipo de empresa (Legal Form) GENRAL
legal_form_means_GENERAL <- DATA_SELECT %>% select(LForm , Country, ROA, ROE) %>% 
  group_by(LForm) %>%
  summarise(
    Mean_ROE = mean(ROE, na.rm = TRUE),
    Mean_ROA = mean(ROA, na.rm = TRUE)
  )
# Calcular promedios de ROE y ROA por tipo de empresa (Legal Form) ESPAÑA
legal_form_means_ESPAÑA <- OB_ES %>% select(LForm , Country, ROA, ROE) %>% 
  group_by(LForm) %>%
  summarise(
    Mean_ROE = mean(ROE, na.rm = TRUE),
    Mean_ROA = mean(ROA, na.rm = TRUE)
  )
# Calcular promedios de ROE y ROA por tipo de empresa (Legal Form) ITALIA
legal_form_means_ITALIA <- OB_IT %>% select(LForm , Country, ROA, ROE) %>% 
  group_by(LForm) %>%
  summarise(
    Mean_ROE = mean(ROE, na.rm = TRUE),
    Mean_ROA = mean(ROA, na.rm = TRUE)
  )

# ANOVA por forma legal para ROA y ROE (Muestra total)
anova_roa_total <- aov(ROA ~ LForm, data = DATA_SELECT)
anova_roe_total <- aov(ROE ~ LForm, data = DATA_SELECT)
# ANOVA por forma legal para España
anova_roa_es <- aov(ROA ~ LForm, data = OB_ES)
anova_roe_es <- aov(ROE ~ LForm, data = OB_ES)
# ANOVA por forma legal para Italia
anova_roa_it <- aov(ROA ~ LForm, data = OB_IT)
anova_roe_it <- aov(ROE ~ LForm, data = OB_IT)


# Combinar los datos de means 
tabla_means <- legal_form_means_GENERAL %>%
  rename("ROE_Total_sample" = Mean_ROE, "ROA_Total_sample" = Mean_ROA) %>%
  left_join(
    legal_form_means_ESPAÑA %>%
      rename("ROE_Spain" = Mean_ROE, "ROA_Spain" = Mean_ROA),
    by = "LForm"
  ) %>%
  left_join(
    legal_form_means_ITALIA %>%
      rename("ROE_Italy" = Mean_ROE, "ROA_Italy" = Mean_ROA),
    by = "LForm"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))  # Redondear a 2 decimales


# Crear un dataframe separado para los valores ANOVA
anova_results <- data.frame(
  `Legal Form` = c("Total sample", "Spain", "Italy"),
  
  # F-value con significancia para ROE
  `F-value (ROE)` = paste0(
    round(c(
      summary(anova_roe_total)[[1]][["F value"]][1],
      summary(anova_roe_es)[[1]][["F value"]][1],
      summary(anova_roe_it)[[1]][["F value"]][1]
    ), 2),
    c(
      ifelse(summary(anova_roe_total)[[1]][["Pr(>F)"]][1] < 0.01, "***",
             ifelse(summary(anova_roe_total)[[1]][["Pr(>F)"]][1] < 0.05, "**",
                    ifelse(summary(anova_roe_total)[[1]][["Pr(>F)"]][1] < 0.10, "*", "")
             )
      ),
      ifelse(summary(anova_roe_es)[[1]][["Pr(>F)"]][1] < 0.01, "***",
             ifelse(summary(anova_roe_es)[[1]][["Pr(>F)"]][1] < 0.05, "**",
                    ifelse(summary(anova_roe_es)[[1]][["Pr(>F)"]][1] < 0.10, "*", "")
             )
      ),
      ifelse(summary(anova_roe_it)[[1]][["Pr(>F)"]][1] < 0.01, "***",
             ifelse(summary(anova_roe_it)[[1]][["Pr(>F)"]][1] < 0.05, "**",
                    ifelse(summary(anova_roe_it)[[1]][["Pr(>F)"]][1] < 0.10, "*", "")
             )
      )
    )
  ),
  
  # p-value para ROE
  `p-value (ROE)` = c(
    summary(anova_roe_total)[[1]][["Pr(>F)"]][1],
    summary(anova_roe_es)[[1]][["Pr(>F)"]][1],
    summary(anova_roe_it)[[1]][["Pr(>F)"]][1]
  ),
  
  # F-value con significancia para ROA
  `F-value (ROA)` = paste0(
    round(c(
      summary(anova_roa_total)[[1]][["F value"]][1],
      summary(anova_roa_es)[[1]][["F value"]][1],
      summary(anova_roa_it)[[1]][["F value"]][1]
    ), 2),
    c(
      ifelse(summary(anova_roa_total)[[1]][["Pr(>F)"]][1] < 0.01, "***",
             ifelse(summary(anova_roa_total)[[1]][["Pr(>F)"]][1] < 0.05, "**",
                    ifelse(summary(anova_roa_total)[[1]][["Pr(>F)"]][1] < 0.10, "*", "")
             )
      ),
      ifelse(summary(anova_roa_es)[[1]][["Pr(>F)"]][1] < 0.01, "***",
             ifelse(summary(anova_roa_es)[[1]][["Pr(>F)"]][1] < 0.05, "**",
                    ifelse(summary(anova_roa_es)[[1]][["Pr(>F)"]][1] < 0.10, "*", "")
             )
      ),
      ifelse(summary(anova_roa_it)[[1]][["Pr(>F)"]][1] < 0.01, "***",
             ifelse(summary(anova_roa_it)[[1]][["Pr(>F)"]][1] < 0.05, "**",
                    ifelse(summary(anova_roa_it)[[1]][["Pr(>F)"]][1] < 0.10, "*", "")
             )
      )
    )
  ),
  
  # p-value para ROA
  `p-value (ROA)` = c(
    summary(anova_roa_total)[[1]][["Pr(>F)"]][1],
    summary(anova_roa_es)[[1]][["Pr(>F)"]][1],
    summary(anova_roa_it)[[1]][["Pr(>F)"]][1]
  )
)



# Crear una fila separada para los valores F y p-value con significancia
anova_row <- data.frame(
  LForm = "F", 
  `ROE_Total_sample` = paste0(
    anova_results$F.value..ROE.[1],
    " (", formatC(anova_results$p.value..ROE.[1], format = "f", digits = 4), ")"
  ),
  `ROA_Total_sample` = paste0(
    anova_results$F.value..ROA.[1],
    " (", formatC(anova_results$p.value..ROA.[1], format = "f", digits = 4), ")"
  ),
  `ROE_Spain` = paste0(
    anova_results$F.value..ROE.[2],
    " (", formatC(anova_results$p.value..ROE.[2], format = "f", digits = 4), ")"
  ),
  `ROA_Spain` = paste0(
    anova_results$F.value..ROA.[2],
    " (", formatC(anova_results$p.value..ROA.[2], format = "f", digits = 4), ")"
  ),
  `ROE_Italy` = paste0(
    anova_results$F.value..ROE.[3],
    " (", formatC(anova_results$p.value..ROE.[3], format = "f", digits = 4), ")"
  ),
  `ROA_Italy` = paste0(
    anova_results$F.value..ROA.[3],
    " (", formatC(anova_results$p.value..ROA.[3], format = "f", digits = 4), ")"
  )
)

# Verificar la fila con los resultados
print(anova_row)

# Convertir columnas en tabla_means a character y reemplazar NA por "0"
tabla_means <- tabla_means %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~ replace_na(.x, "0")))

# Convertir columnas en anova_row a character y reemplazar NA por "0"
anova_row <- anova_row %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~ replace_na(.x, "0")))

# Combinar ambas tablas
tabla_combinada <- bind_rows(tabla_means, anova_row)


tabla5_empresa <- c("0" = "Public limited company", 
                    "1" = "Private limited company", 
                    "3" = "Cooperative", 
                    "4" = "Other legal forms",
                    "F" = "F")
# Reemplazar los códigos numéricos con los nombres de tipos de empresas
tabla_combinada <-tabla_combinada %>%
  mutate(LForm = tabla5_empresa[as.character(LForm)])%>% 
  rename("Legal_Form" = LForm)

# FILTRO DE VARIOS #####
DATA_SELECT %>%
  summarize(
    min_ROE = min(ROE, na.rm = TRUE),
    max_ROE = max(ROE, na.rm = TRUE),
    min_ROA = min(ROA, na.rm = TRUE),
    max_ROA = max(ROA, na.rm = TRUE)) %>% view()

G %>%group_by(ROE)%>% summarize(
  min_ROE = min(ROE, na.rm = TRUE),
  max_ROE = max(ROE, na.rm = TRUE)) %>% view()

G %>%   top_10_min = arrange(G, ROE),
top_10_max = arrange(G, desc(ROE))%>% view()
#FILTROS DE VACIOS
faltantes <- GENERAL %>% filter(rowSums(is.na(.)) > 0)
# Filas con NA
na_rows <- GENERAL%>% filter_all(any_vars(is.na(.)))

# Filas con #NUM!
num_error_rows <- GENERAL[apply(GENERAL, 1, function(x) any(x == "#NUM!")), ]

# Filas vacías
vacias_rows <- GENERAL[apply(GENERAL, 1, function(x) any(x == "")), ]

G %>%
  get_summary_stats() %>% select(variable, mean, sd, min, max)%>%
  view()

# Filtrar para ROE (2,109)!!
GENERAL %>% #no se encuentra el maximo
  select(ROE) %>%
  filter(ROE >= -274.84 & ROE <= 270.22) %>%
  arrange(ROE) %>%
  view()
GENERAL %>% #no se encuentra el maximo
  select(ROE)%>%
  arrange(desc(ROE)) %>%
  view()
# Filtrar para ROA (2,153)
GENERAL %>% #el maximo varia en un decimal 41.87 (.86)
  select(ROA) %>%
  filter(ROA >= -44.59 & ROA <= 42) %>%
  arrange(ROA) %>%
  view()

# Filtrar para Size (2,161)
GENERAL %>%
  select(Size) %>%
  filter(Size >= 2.75 & Size <= 12.63) %>%
  arrange(Size) %>%
  view()

# Filtrar para Debt( 2,162)
GENERAL %>%
  select(Debt) %>%
  filter(Debt >= 0.00 & Debt <= 2.59) %>% get_summary_stats() %>% select(variable, mean, sd, min, max)%>%
  view()%>%
  arrange(Debt) %>%
  view()

# Filtrar para Growth (1,763)!!
GENERAL %>%
  select(Growth) %>%
  filter(Growth >= -0.54 & Growth <= 454.58) %>%
  arrange(Growth) %>%
  view()
GENERAL %>%
  filter((Growth >= -0.54 & Growth <= 454.58) | is.na(Growth)) %>%
  arrange(Growth) %>%
  view()

PRUEBA <- GENERAL %>%  drop_na (Growth)
# Filtrar para VarGDP (2,269)
GENERAL %>%
  select(VarGDP) %>%
  filter(VarGDP >= 0.29 & VarGDP <= 3.84) %>%
  arrange(VarGDP) %>%
  view()


# Filtrar para Inflat´(2,270 )
GENERAL %>%
  select(Inflat) %>%
  filter(Inflat >= -0.50 & Inflat <= 1.96) %>%
  arrange(Inflat) %>%
  view()

# Filtrar para Gender(2,260 )
GENERAL %>%
  select(Gender) %>%
  filter(Gender >= 0.00 & Gender <= 100.00) %>%
  arrange(Gender) %>%
  view()

# Filtrar para OpInc (2,144 )
GENERAL %>%
  select(OpInc) %>%
  filter(OpInc >= 0.00 & OpInc <= 12.43) %>%
  arrange(OpInc) %>%
  view()


GENERAL %>%
  filter(OpInc >= 0.00 & OpInc <= 12.43) %>%
  view()
# Filtrar para StockT (1,837 )!!
GENERAL %>%
  select(StockT) %>%
  filter(StockT >= 0.00 & StockT <= 9680.32) %>%
  arrange(StockT) %>%
  view()

# Filtrar para AssetT (2,153 )
GENERAL %>%
  select(AssetT) %>%
  filter(AssetT >= 0.00 & AssetT <= 15.20) %>%
  arrange(AssetT) %>%
  view()

# Filtrar para ARP (2,226)
GENERAL %>%
  select(ARP) %>%
  filter(ARP >= 0.00 & ARP <= 981.75) %>%
  arrange(ARP) %>%
  view()

# Filtrar para APP (2,243 )
GENERAL %>%
  select(APP) %>%
  filter(APP >= 0.00 & APP <= 993.43) %>%
  arrange(APP) %>%
  view()

# Filtrar para Age (2,238 )
GENERAL %>%
  select(Age) %>%
  filter(Age >= 0.14 & Age <= 103.41) %>%
  arrange(Age) %>%
  view()

OB_NA <- drop_na(GENERAL)


# PRUEBAS #####
GENERAL %>% summarize(min(ROE), max(ROE))

# Filtrar para Age 
GENERAL %>%
  select(Age) %>%
  filter(Age >= 0.14 & Age <= 103.41) %>%
  arrange(Age) %>%
  view()
GENERAL %>%
  filter(OpInc >= 0.00 & OpInc <= 12.43) %>%
  view()

PRUEBA<- GENERAL %>%
  filter (!OpInc <= 0.00) %>%
  view("P1")
PRUEBA <- PRUEBA %>%
  filter(!is.na(Debt)) %>%
  view("P2")

PRUEBA2 <- PRUEBA2 %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
  view()
PRUEBA<- PRUEBA %>%
  filter (!Age <= 0.00) %>%
  view("P3")
PRUEBA <- PRUEBA %>%
  filter(rowSums(is.na(.)) <= 6) %>%
  view("P55")
PRUEBA2<- PRUEBA2 %>%
  filter(!is.nan(OpInc)) %>%
  view()
PRUEBA2<- PRUEBA2 %>%
  filter(!is.na(StockT)) %>%
  view()
PRUEBA<-PRUEBA2 %>%
  filter(rowSums(is.na(.)) <= 6) %>%
  view("P55")
PRUEBA2<- PRUEBA %>%
  filter(!is.nan(OpInc)) %>%
  view()
PRUEBA2<- GENERAL %>%
  filter (!Age <= 0.00) %>%
  view("P3")

PRUEBA3 <- PRUEBA3 %>%
  filter(OpInc != -Inf) %>%
  view()

PRUEBA3<- GENERAL %>%
  filter (!Age <= 0.13) %>%
  view("P3")
PRUEBA3<- PRUEBA3 %>%
  filter (!Debt <= -0.01) %>%
  view("P3")
PRUEBA3<- PRUEBA3 %>%
  filter (!OpInc <= -0.01) %>%
  view("P3")
PRUEBA3<- PRUEBA3 %>%
  filter(!is.nan(OpInc)) %>%
  view()
PRUEBA3<- PRUEBA3 %>%
  filter(!is.nan(StockT)) %>%
  view()
PRUEBA3<- GENERAL %>%
  filter (!OpInc <= 0.00) %>%
  view("P3")
PRUEBA2<- PRUEBA3 %>%
  filter(rowSums(is.na(.)) <= 4) %>%
  view("P55")
PRUEBA %>% get_summary_stats() %>% select(variable, mean, sd, min, max)%>% view("RP")

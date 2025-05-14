# instalar paquetes ####
install.packages("tidyverse")
install.packages("openxlsx")
install.packages("readr")
install.packages("janitor")
install.packages("rstatix")
install.packages("gt")


# cargar paquetes------
library(tidyverse)
library(dplyr)
stats::filter 
library(openxlsx)
library(readr)
library(janitor)
stats::chisq.test
stats::fisher.test
library(rstatix)
library(gt)

# importar data--------
datos<-read.xlsx("C:/Users/Usuario/Desktop/DALLY/SEMILLERO/DATA_STATA/DATA/DatosStata.xlsx")

# explorando el objeto datos------
str(datos)

datos_ex <-datos %>% select (- c(Número.de.accionistas,DM.Edad,ADV.Edad)) #eliminar duplicados

# Diccionario de traducción
DATA <- datos_ex %>%
  rename(
    "Year" = "Año",
    "ID" = "Id.",
    "Identity" = "Ident",
    "Gender" = "Género",
    "Company_Name" = "Nombre.empresa",
    "City" = "Ciudad",
    "Country_ISO_Code" = "Código.ISO.del.país",
    "Total_Assets" = "Activos.totales",
    "Growth" = "Grow",
    "GDP" = "PIB",
    "GDP_Var" = "VarPIB",
    "Inflation" = "Inflacion",
    "Ln_GDP" = "LnPIB",
    "Ln_Inflation" = "LnInflacion",
    "Country" = "Pais",
    "NACE_Code" = "NACE.code",
    "Employees_Last_Year" = "Número.empleados.Últ..año.disp.",
    "Standard_Legal_Form" = "Forma.jurídica.estándar",
    "Legal_Form_Tabul" = "FJurídicaTabul",
    "Legal_Form" = "FormaJurídica",
    "Incorporation_Date" = "Fecha.de.constitución",
    "End_Date" = "Fecha.final",
    "Seniority" = "Antigüedad",
    "Ln_Seniority" = "LnAntigüedad",
    "Cash_Flows" = "Flujos.de.Caja",
    "Fixed_Assets" = "Activos.Fijos",
    "Current_Assets" = "Activos.Corrientes",
    "Inventory" = "Stock",
    "Receivables" = "Deudores",
    "Other_Current_Assets" = "Otros.activos.corrientes",
    "Cash_and_Equivalents" = "Efectivo.y.equivalentes",
    "Ln_Fixed_Assets" = "LnActFijo",
    "Ln_Current_Assets" = "LnActCorr",
    "Ln_Inventory" = "LnStock",
    "Ln_Receivables" = "LnDeudores",
    "Ln_Other_Assets" = "LnOtrosactiv",
    "Ln_Cash" = "LnEfectivo",
    "Ln_Total_Assets" = "LnActTotal",
    "Non_Current_Liabilities" = "Pasivos.no.corrientes",
    "Current_Liabilities" = "Pasivos.Corrientes",
    "Liquidity1" = "Liquidez1",
    "Liquidity1_Dummy" = "Liquidez1Dummy",
    "Total_Liabilities" = "Pasivo.Total",
    "Equity" = "Fondos.Propios",
    "Ln_Non_Current_Liabilities" = "LnPasivoNoCorr",
    "Ln_Current_Liabilities" = "LnPasivoCorr",
    "Ln_Total_Liabilities" = "LnPasivoTotal",
    "Ln_Equity" = "LnFondosPropios",
    "Operating_Revenue" = "Ingresos.Explotación",
    "Operating_Profit" = "Resultado.Explotación",
    "Financial_Expenses" = "Gastos.Financieros",
    "Ordinary_Profit_Before_Tax" = "Rdo..Ordinario.antes.Impuestos",
    "Taxes" = "Impuestos",
    "Ordinary_Activities_Profit" = "Rdo..Actividades.Odinarias",
    "Extraordinary_and_Other_Profit" = "Rdo..Extr..y.Otros",
    "Net_Profit" = "Rdo.Ejercicio",
    "ROE" = "ROE",
    "ROA" = "ROA",
    "Collection_Period" = "Período.de.Cobro",
    "Credit_Period" = "Período.de.Credito",
    "ROEE" = "ROEE",
    "ROAA" = "ROAA",
    "CollectionPeriod" = "PeríodoCobro",
    "Payment_Period" = "PeríodoPago",
    "PMC_PMP" = "PMC-PMP",
    "Net_Asset_Turnover" = "Rotación.de.activos.netos",
    "Inventory_Turnover" = "Rotación.de.las.existencias",
    "Solvency_Turnover" = "Rotacion.de.Solvencia",
    "Asset_Turnover" = "RotacActivos",
    "InventoryTurnover" = "RotacExistenc",
    "SolvencyTurnover" = "RotacSolvencia",
    "Liquidity_Ratio" = "Ratio.de.Liquidez",
    "Leverage" = "Apalancamiento",
    "Profit_per_Employee" = "Beneficio.por.empleado",
    "Operating_Revenue_per_Employee" = "Ingresos.Explotación.por.empleado",
    "Average_Employee_Cost" = "Coste.medio.Empleados",
    "Total_Assets_per_Employee" = "Total.acivos.por.empleado",
    "Levera" = "Apalancam",
    "Profit_Employee" = "Benefic/empleado",
    "OperatingRevenue_Employee" = "IngrExpl/empleado",
    "Cost_Employee" = "Coste/empleado",
    "Assets_Employee" = "Activos/empleado",
    "Number_of_Board_and_Management_Members" = "Nümero.de.miembros.de.las.juntas.&.gestión",
    "Board_Members" = "MiembrosJuntas",
    "DM_Full_Name" = "DM.Nombre.completo",
    "DM_Job_Title" = "DM.Título.trabajo",
    "Shareholder_Direct_Percentage" = "Accionista.-.%.directo",
    "Shareholder_Total_Percentage" = "Accionista.-.%.total",
    "CSH_Direct_Percentage" = "CSH.-.%.directo",
    "DM_Original_Job_Title" = "DM.Título.original.trabajo",
    "DM_Board_Committee_or_Executive_Department" = "DM.Junta,.comité.or.departamento.ejecutivo",
    "DM_Level_of_Responsibility" = "DM.Nivel.de.responsabilidad",
    "DM_First_Name" = "DM.Nombre",
    "DM_Last_Name" = "DM.Apellido",
    "DM_Gender" = "DM.Género",
    "DM_Nationality_Country" = "DM.País.de.nacionalidad",
    "DM_Also_a_Shareholder" = "DM.También.un.accionista",
    "DM_Position_Type" = "DM.Tipo.de.posición",
    "Number_of_Advisors" = "Número.de.asesores",
    "ADV_First_Name" = "ADV.Nombre",
    "ADV_Last_Name" = "ADV.Apellido",
    "ADV_Gender" = "ADV.Género",
    "ADV_Nationality_Country" = "ADV.País.de.nacionalidad",
    "Nationality_Country" = "País.de.nacionalidad",
    "Number_of_Employees" = "Número.empleados",
    "BvD_Independence_Indicator" = "Indicador.independencia.BvD"
  )

#coercion de datos
colnames(DATA)
str(DATA)
DATA_Manipulada <- DATA %>%
  mutate(
    # Conversión de columnas a numérico
    Growth = parse_number(Growth, locale = locale(decimal_mark = ".")),
    Ln_Inflation = parse_number(Ln_Inflation, locale = locale(decimal_mark = ".")),
    Ln_Seniority = parse_number(Ln_Seniority, locale = locale(decimal_mark = ".")),
    Ln_Fixed_Assets = parse_number(Ln_Fixed_Assets, locale = locale(decimal_mark = ".")),
    Ln_Current_Assets = parse_number(Ln_Current_Assets, locale = locale(decimal_mark = ".")),
    Ln_Inventory = parse_number(Ln_Inventory, locale = locale(decimal_mark = ".")),
    Ln_Receivables = parse_number(Ln_Receivables, locale = locale(decimal_mark = ".")),
    Ln_Other_Assets = parse_number(Ln_Other_Assets, locale = locale(decimal_mark = ".")),
    Ln_Cash = parse_number(Ln_Cash, locale = locale(decimal_mark = ".")),
    Ln_Total_Assets = parse_number(Ln_Total_Assets, locale = locale(decimal_mark = ".")),
    Liquidity1 = parse_number(Liquidity1, locale = locale(decimal_mark = ".")),
    Liquidity1_Dummy = parse_number(Liquidity1_Dummy, locale = locale(decimal_mark = ".")),
    Ln_Non_Current_Liabilities = parse_number(Ln_Non_Current_Liabilities, locale = locale(decimal_mark = ".")),
    Ln_Current_Liabilities = parse_number(Ln_Current_Liabilities, locale = locale(decimal_mark = ".")),
    Ln_Total_Liabilities = parse_number(Ln_Total_Liabilities, locale = locale(decimal_mark = ".")),
    Ln_Equity = parse_number(Ln_Equity, locale = locale(decimal_mark = ".")),
    ROE = parse_number(ROE, locale = locale(decimal_mark = ".")),
    ROA = parse_number(ROA, locale = locale(decimal_mark = ".")),
    Collection_Period = parse_number(Collection_Period, locale = locale(decimal_mark = ".")),
    Credit_Period = parse_number(Credit_Period, locale = locale(decimal_mark = ".")),
    ROEE = parse_number(ROEE, locale = locale(decimal_mark = ".")),
    ROAA = parse_number(ROAA, locale = locale(decimal_mark = ".")),
    CollectionPeriod = parse_number(CollectionPeriod, locale = locale(decimal_mark = ".")),
    Payment_Period = parse_number(Payment_Period, locale = locale(decimal_mark = ".")),
    PMC_PMP = parse_number(PMC_PMP, locale = locale(decimal_mark = ".")),
    Net_Asset_Turnover = parse_number(Net_Asset_Turnover, locale = locale(decimal_mark = ".")),
    Inventory_Turnover = parse_number(Inventory_Turnover, locale = locale(decimal_mark = ".")),
    Solvency_Turnover = parse_number(Solvency_Turnover, locale = locale(decimal_mark = ".")),
    Asset_Turnover = parse_number(Asset_Turnover, locale = locale(decimal_mark = ".")),
    InventoryTurnover = parse_number(InventoryTurnover, locale = locale(decimal_mark = ".")),
    SolvencyTurnover = parse_number(SolvencyTurnover, locale = locale(decimal_mark = ".")),
    Liquidity_Ratio = parse_number(Liquidity_Ratio, locale = locale(decimal_mark = ".")),
    Leverage = parse_number(Leverage, locale = locale(decimal_mark = ".")),
    Profit_per_Employee = parse_number(Profit_per_Employee, locale = locale(decimal_mark = ".")),
    Operating_Revenue_per_Employee = parse_number(Operating_Revenue_per_Employee, locale = locale(decimal_mark = ".")),
    Levera = parse_number(Levera, locale = locale(decimal_mark = ".")),
    Profit_Employee = parse_number(Profit_Employee, locale = locale(decimal_mark = ".")),
    OperatingRevenue_Employee = parse_number(OperatingRevenue_Employee, locale = locale(decimal_mark = ".")),
    Shareholder_Direct_Percentage = parse_number(Shareholder_Direct_Percentage, locale = locale(decimal_mark = ".")),
    Shareholder_Total_Percentage = parse_number(Shareholder_Total_Percentage, locale = locale(decimal_mark = ".")),
    CSH_Direct_Percentage = parse_number(CSH_Direct_Percentage, locale = locale(decimal_mark = ".")),
    
    # Conversión de columnas a Date 
    Incorporation_Date = as.Date(Incorporation_Date, origin = "1899-12-30"),
    End_Date = as.Date(End_Date, origin = "1899-12-30"),
    
    # Conversión de columnas a character
    Country = as.character(Country),
    Identity = as.character(Identity),
    Legal_Form = as.character(Legal_Form),
    Legal_Form_Tabul = as.character(Legal_Form_Tabul),
    
    #Conversion de columnas a factor
    ADV_Gender= parse_factor(ADV_Gender,
                             levels = c("M","F","M\nM","M\nM\nM","M\nF"),
                             ordered = TRUE),
    BvD_Independence_Indicator= parse_factor(BvD_Independence_Indicator,
                                             levels = c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "U"),
                                             ordered = TRUE),
    Standard_Legal_Form = parse_factor(Standard_Legal_Form,
                                       levels = c("Public limited companies", "Private limited companies", "Partnerships", "Other legal forms"),
                                       ordered = FALSE))



# TABLA 1 ----
# Crear la tabla utilizando tibble
tabla <- tibble::tibble(
  Abreviatura = c("ROE", "ROA", "Tamaño", "Deuda", "Crecimiento", "VarPIB", "Inflación", 
                  "Género", "IngresoOp", "RotaciónStock", "RotaciónActivos", "PPC", "PPP", 
                  "Edad", "FormaJurid", "País"),
  Variable = c("Retorno sobre el patrimonio", "Retorno sobre los activos", "Tamaño de la empresa",
               "Endeudamiento", "Crecimiento de la empresa", "Cambio en el PIB", 
               "Inflación del país", "Diversidad de género", "Ingreso operativo", 
               "Rotación del inventario", "Rotación de activos", 
               "Período promedio de cobro", "Período promedio de pago", "Edad de la empresa",
               "Forma jurídica", "País de residencia de la empresa"),
  Definición = c("Beneficio neto dividido por el patrimonio", 
                 "Beneficio neto dividido por los activos totales", 
                 "Logaritmo natural del total de activos de la empresa", 
                 "Pasivos totales divididos por los activos totales", 
                 "Porcentaje de cambio en los activos totales", 
                 "Porcentaje de cambio en el producto interno bruto", 
                 "Inflación del país en el año", 
                 "Porcentaje de mujeres en el consejo de administración", 
                 "Logaritmo natural del ingreso operativo", 
                 "Costo de ventas dividido por inventario", 
                 "Ingreso operativo dividido por activos totales", 
                 "Promedio de días que la empresa tarda en recibir pagos de clientes", 
                 "Promedio de días que la empresa tarda en pagar a los proveedores", 
                 "Edad de la empresa en años", 
                 "La forma jurídica de la empresa: • Sociedad anónima • Sociedad limitada • Cooperativa • Otras formas legales",
                 "Variable ficticia, igual a 1 para España y 0 para Italia")
)
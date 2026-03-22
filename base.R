# Tabla QPC ##################################################################################
# Crear key larga (pais_anio inicio_anio fin_programa_review_QPC)
# Crear key corta (pais_anio_programa_review)
# Eliminar todas las filas que no digan descripcion en la columna Sub
# Eliminar todas las filas que no tengan en la col Status M, NM, W, WA
# Crear columna R con el valor del review
# Para igual valor en todas las restantes columnas, quedarse solo con las filas que Tengan el menor valor en la columna R

#library(readxl)
#QPC_11_ <- read_excel("C:/Users/HP/Desktop/Tesis UDESA/QPC (11).xlsx") #Tabla original de imf.org/mona

# install.packages("openxlsx", type = "binary")
library(openxlsx)
url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/QPC.xlsx"
QPC_11 <- read.xlsx(url)

QPC_11_ <- QPC_11

str(QPC_11_)
colnames(QPC_11_)


library(dplyr)

QPC_11_ <- QPC_11_ %>%
  mutate(Key_corta = paste(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Review.Type, sep = "_"))

QPC_11_ <- QPC_11_ %>%
  mutate(Key_larga = paste(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Review.Type, Criteria.Order, sep = "_"))

QPC_11_  <- QPC_11_  %>%
  mutate(Key_min = paste(Country.Code , Arrangement.Number, sep = "_"))

QPC_11_  <- QPC_11_  %>%
  relocate(Key_min, .before = 3)


#########################

library(dplyr)
library(stringr)

QPC_11_ <- QPC_11_ %>%
  mutate(Sub.Option = ifelse(
    Criteria.Order == 135 & str_detect(Sub, regex("Description", ignore_case = TRUE)),
    case_when(
      str_detect(Sub.Option, regex("tax|revenue", ignore_case = TRUE)) ~ "Revenue",
      str_detect(Sub.Option, regex("primary", ignore_case = TRUE)) ~ "Primary_deficit",
      str_detect(Sub.Option, regex("overall|deficit|borrowing|surplus|savings|lending|balance|financing", ignore_case = TRUE)) ~ "Overall_deficit",
      str_detect(Sub.Option, regex("spending|wages|social|contracts|investment", ignore_case = TRUE)) ~ "Spending",
      TRUE ~ "Other"
    ),
    Sub.Option
  ))

resultado <- QPC_11_ %>%
  filter(Criteria.Order == 135,
         Sub == "Description") %>%
  count(Sub.Option, name = "Count", sort = TRUE)
print(resultado) # primary vs others




QPC_11_ <- QPC_11_ %>%
  mutate(Sub.Option = ifelse(
    Criteria.Order == 32 & str_detect(Sub, regex("Description", ignore_case = TRUE)),
    case_when(
      str_detect(Sub.Option, regex("reserves|reserve|NIR|assets|exchange|FX|currency", ignore_case = TRUE)) &
        str_detect(Sub.Option, regex("floor|net|gross", ignore_case = TRUE)) ~ "NIR_floor",
      str_detect(Sub.Option, regex("reserves|NIR|assets|FX", ignore_case = TRUE)) &
        str_detect(Sub.Option, regex("change|increase|purchases|purchases/sales", ignore_case = TRUE)) ~ "NIR_change",
      str_detect(Sub.Option, regex("restrictions|multiple|Article VIII", ignore_case = TRUE))~ "FX_ctrol",
      TRUE ~ "Other"
    ),
    Sub.Option
  ))

resultado <- QPC_11_ %>%
  filter(Criteria.Order == 32,
         Sub == "Description") %>%
  count(Sub.Option, name = "Count", sort = TRUE)
print(resultado)



QPC_11_ <- QPC_11_ %>%
  mutate(Sub.Option = ifelse(
    Criteria.Order == 47 & str_detect(Sub, regex("Description", ignore_case = TRUE)),
    case_when(
      str_detect(Sub.Option, regex("contracted or guaranteed|contracting or guaranteeing|Contracting and guaranteeing", ignore_case = TRUE)) ~ "External_debt_Contracted_Guaranteed",
      str_detect(Sub.Option, regex("contracted|public sector|central|government|general", ignore_case = TRUE)) ~ "External_debt_long_Contracted",
      str_detect(Sub.Option, regex("guaranteed", ignore_case = TRUE)) ~ "External_debt_long_Guaranteed",
      TRUE ~ "External_debt_long_Contracted"
    ),
    Sub.Option
  ))


resultado <- QPC_11_ %>%
  filter(Criteria.Order == 47,
         Sub == "Description") %>%
  count(Sub.Option, name = "Count", sort = TRUE)
print(resultado) #




QPC_11_ <- QPC_11_ %>%
  mutate(Sub.Option = ifelse(
    Criteria.Order == 114 & str_detect(Sub, regex("Description", ignore_case = TRUE)),
    case_when(
      str_detect(Sub.Option, regex("stock|outstanding", ignore_case = TRUE)) ~ "External_Arrears_stock",
      str_detect(Sub.Option, regex("accumulation|reduction|change|changes|Accumulaion|new|increase", ignore_case = TRUE)) ~ "External_Arrears_accum",
      TRUE ~ "External_Arrears_stock"
    ),
    Sub.Option
  ))

resultado <- QPC_11_ %>%
  filter(Criteria.Order == 114,
         Sub == "Description") %>%
  count(Sub.Option, name = "Count", sort = TRUE)
print(resultado)



QPC_11_ <- QPC_11_ %>%
  mutate(Sub.Option = ifelse(
    Criteria.Order == 16 & str_detect(Sub, regex("Description", ignore_case = TRUE)),
    case_when(
      str_detect(Sub.Option, regex("central bank|Monetary", ignore_case = TRUE)) ~ "Credit_Central_bank",
      str_detect(Sub.Option, regex("banking|bank", ignore_case = TRUE)) ~ "Credit_Banks",
      str_detect(Sub.Option, regex("domestic", ignore_case = TRUE)) ~ "Credit_Domestic",
      TRUE ~ "Credit_Other"
    ),
    Sub.Option
  ))

resultado <- QPC_11_ %>%
  filter(Criteria.Order == 16,
         Sub == "Description") %>%
  count(Sub.Option, name = "Count", sort = TRUE)
print(resultado) #





QPC_11_ <- QPC_11_ %>%
  mutate(Sub.Option = ifelse(
    Criteria.Order == 84 & str_detect(Sub, regex("Description", ignore_case = TRUE)),
    case_when(
      str_detect(Sub.Option, regex("stock|outstanding", ignore_case = TRUE)) ~ "Debt_1yr_stock",
      str_detect(Sub.Option, regex("accumulation|reduction|change|changes|Accumulaion|new|increase", ignore_case = TRUE)) ~ "Debt_1yr_accum",
      TRUE ~ "Debt_1yr_stock"
    ),
    Sub.Option
  ))

resultado <- QPC_11_ %>%
  filter(Criteria.Order == 84,
         Sub == "Description") %>%
  count(Sub.Option, name = "Count", sort = TRUE)
print(resultado) #



QPC_11_ <- QPC_11_ %>%
  mutate(Sub.Option = ifelse(
    Criteria.Order == 1 & str_detect(Sub, regex("Description", ignore_case = TRUE)),
    case_when(
      str_detect(Sub.Option, regex("assets", ignore_case = TRUE)) ~ "CentralBank_Assets",
      str_detect(Sub.Option, regex("money", ignore_case = TRUE)) ~ "CentralBank_Money",
      str_detect(Sub.Option, regex("government", ignore_case = TRUE)) ~ "CentralBank_Gov",
      TRUE ~ "Other"
    ),
    Sub.Option
  ))

resultado <- QPC_11_ %>%
  filter(Criteria.Order == 1,
         Sub == "Description") %>%
  count(Sub.Option, name = "Count", sort = TRUE)
print(resultado)




QPC_11_ <- QPC_11_ %>%
  mutate(Sub.Option = ifelse(
    Criteria.Order == 149 & str_detect(Sub, regex("Description", ignore_case = TRUE)),
    case_when(
      str_detect(Sub.Option, regex("stock|outstanding", ignore_case = TRUE)) ~ "Domestic_arrears_stock",
      str_detect(Sub.Option, regex("accumulation|reduction|change|changes|Accumulaion|new|increase", ignore_case = TRUE)) ~ "Domestic_arrears_accum",
      TRUE ~ "Domestic_arrears_stock"
    ),
    Sub.Option
  ))

resultado <- QPC_11_ %>%
  filter(Criteria.Order == 149,
         Sub == "Description") %>%
  count(Sub.Option, name = "Count", sort = TRUE)
print(resultado)



QPC_11_ <- QPC_11_ %>%
  mutate(Sub.Option = ifelse(
    Criteria.Order == 107 & str_detect(Sub, regex("Description", ignore_case = TRUE)),
    case_when(
      str_detect(Sub.Option, regex("stock|outstanding", ignore_case = TRUE)) ~ "External_arrears_stock",
      str_detect(Sub.Option, regex("accumulation|reduction|change|changes|Accumulaion|new|increase", ignore_case = TRUE)) ~ "External_arrears_accum",
      TRUE ~ "External_arrears_stock"
    ),
    Sub.Option
  ))


resultado <- QPC_11_ %>%
  filter(Criteria.Order == 107,
         Sub == "Description") %>%
  count(Sub.Option, name = "Count", sort = TRUE)
print(resultado) #



QPC_11_ <- QPC_11_ %>%
  mutate(Sub.Option = ifelse(
    Criteria.Order == 74 & str_detect(Sub, regex("Description", ignore_case = TRUE)),
    case_when(
      str_detect(Sub.Option, regex("contracted or guaranteed|contracting or guaranteeing|Contracting and guaranteeing", ignore_case = TRUE)) ~ "External_debt_long_Contracted_Guaranteed",
      str_detect(Sub.Option, regex("contracted|public sector|central|government|general", ignore_case = TRUE)) ~ "External_debt_long_Contracted",
      TRUE ~ "Other"
    ),
    Sub.Option
  ))

resultado <- QPC_11_ %>%
  filter(Criteria.Order == 74,
         Sub == "Description") %>%
  count(Sub.Option, name = "Count", sort = TRUE)
print(resultado)

unique(QPC_11_$Status)

######################

df_filtrado <- QPC_11_ %>%
  filter(
    grepl("Description", Sub, ignore.case = TRUE),
    grepl("NM|M|W|WA|WM|Mod", Status, ignore.case = TRUE)  # "|" means OR in regex
  )

df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Sub")]



library(fastDummies)

df_filtrado <- df_filtrado  %>%
  arrange(Sub.Option) %>%
  dummy_cols(select_columns = "Sub.Option")
df_filtrado  <- df_filtrado  %>% select(-Sub.Option)



library(stringr)

QPC <- df_filtrado %>%
  mutate(R = as.numeric(str_extract(Review.Type, "\\d+"))) %>%
  relocate(R, .after = 11)

str(QPC)

QPC_final <- QPC %>%
  group_by(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Criteria.Order, Status, Test.Date, Program.Amount) %>%
  filter(R == min(R)) %>%
  ungroup()

QPC_final <- QPC_final[, -which(names(QPC_final) == "is.IT.Test.Date?")]
QPC_final <- QPC_final[, -which(names(QPC_final) == "Grouping")]
QPC_final <- QPC_final[, -which(names(QPC_final) == "Comments")]
QPC_final <- QPC_final[, -which(names(QPC_final) == "Program.Type")]
QPC_final <- QPC_final[, -which(names(QPC_final) == "Board.Document.Number")]
QPC_final <- QPC_final[, -which(names(QPC_final) == "Dqpc.Code")]

QPC_final <- QPC_final %>%
  relocate(Key_corta, .before = 1)

QPC_final <- QPC_final %>%
  relocate(Key_larga, .before = 1)

unique(QPC_final$Review.Sequence)

QPC_final <- QPC_final %>%
  mutate(Review.Sequence = trimws(Review.Sequence))

QPC_final <- QPC_final %>%
  mutate(Review.Sequence = ifelse(Review.Sequence == "L", 1, 0))

QPC_final <- QPC_final %>%
  mutate(Revised.Amount = ifelse(is.na(Revised.Amount),Program.Amount, Revised.Amount))

QPC_final<- QPC_final %>%
  mutate(Adjusted.Amount = ifelse(is.na(Adjusted.Amount), Revised.Amount, Adjusted.Amount))

QPC_final <- QPC_final %>%
  mutate(Actual.Amount = ifelse(is.na(Actual.Amount), Adjusted.Amount, Actual.Amount))

QPC_final$Test.Date <- format(as.Date(QPC_final$Test.Date, format = "%m/%d/%Y"), "%d/%m/%Y")

colnames(QPC_final)


# Eliminar errores de columna Units

QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("SDR (millions)",
                   "SDR millions",
                   "SDR (in million)",
                   "SDR (in millions)",
                   "SRD (in million)",
                   "SRD (in millions)") ~ "SDR_millions",
      TRUE ~ Units
    )
  )


QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("US$(billions)",
                   "USD(billions)",
                   "US$ (billions)",
                   "US$(bmllions)",
                   "US$") ~ "USD_billions",
      TRUE ~ Units
    )
  )

QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("US$(millions)",
                   "US$(mllions)",
                   "USD(millions)",
                   "US$(Millions)",
                   "US$ (millions)",
                   "US$(million)",
                   "None",
                   " US$(millions)",
                   " US$(millions) ",
                   "US$(millions) ") ~ "USD_millions",
      TRUE ~ Units
    )
  )

QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("NC (millions)",
                   "NCU(millions)",
                   "NCU (millions)",
                   "NCU (millions)",
                   "Euros(millions)",
                   "Euro(millions)",
                   "KM(millions)",
                   "KM(millions)",
                   "Millions of national currency",
                   "Rufiyaa (millions)",
                   "CGF (millions)",
                   "LCU(millions)",
                   "Euros (millions)",
                   "KM (millions)",
                   "NC(millions)",
                   "Rufiyaa(millions)",
                   "Euors(millions)",
                   "NUC(millions)",
                   "LCU(Millions)",
                   "NC$(millions)",
                   "rufiyaa(millions)",
                   "LCU(million)",
                   "million rupees",
                   "euros(millions)") ~ "NC_millions",
      TRUE ~ Units
    )
  )


QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("NC (billions)",
                   "NC(billions)",
                   "NC$ (billions)",
                   "NCU(billions)",
                   "Euro (billions)",
                   "CFAF (billions)",
                   "rufiyaa(millions)",
                   "Rwandan franc(billions)",
                   "Tsh billions",
                   "NCU (billions)",
                   "NCU$ (billions)",
                   "Euros (billions)",
                   "Euro(billions)",
                   "CFAF(billions)",
                   "Euros(billions)",
                   "AMD(billions)",
                   "LCU(billions)",
                   "Dinars (billions)",
                   "NC (billions)",
                   " NCU (billions) ",
                   " NCU(billions)  ",
                   " NC (billions) ",
                   "NCU (billions) ",
                   "NCU(billions) ",
                   " NC(billions) ",
                   " NC (billions)",
                   " NCU(billions) ",
                   " Euros(billions) ") ~ "NC_billions",
      TRUE ~ Units
    )
  )


QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("percent", "Percent") ~ "Percent",
      TRUE ~ Units
    )
  )

unique(QPC_final$Units)


QPC_final <- QPC_final %>%
  mutate(Units_USD_bill = if_else(grepl("USD_billions", Units, ignore.case = TRUE), 1, 0))

QPC_final <- QPC_final %>%
  mutate(Units_USD_mill = if_else(grepl("USD_millions", Units, ignore.case = TRUE), 1, 0))

QPC_final <- QPC_final %>%
  mutate(Units_NC_bill = if_else(grepl("NC_billions", Units, ignore.case = TRUE), 1, 0))

QPC_final <- QPC_final %>%
  mutate(Units_NC_mill = if_else(grepl("NC_millions", Units, ignore.case = TRUE), 1, 0))

QPC_final <- QPC_final %>%
  mutate(Units_SDR_mill = if_else(grepl("SDR_millions", Units, ignore.case = TRUE), 1, 0))

colnames(QPC_final)

QPC_final$Program.Amount <- ifelse(QPC_final$Units == "USD_millions", as.numeric(QPC_final$Program.Amount) / 1000, QPC_final$Program.Amount)
QPC_final$Revised.Amount <- ifelse(QPC_final$Units == "USD_millions", as.numeric(QPC_final$Revised.Amount) / 1000, QPC_final$Revised.Amount)
QPC_final$Adjusted.Amount <- ifelse(QPC_final$Units == "USD_millions", as.numeric(QPC_final$Adjusted.Amount) / 1000, QPC_final$Adjusted.Amount)
QPC_final$Actual.Amount <- ifelse(QPC_final$Units == "USD_millions", as.numeric(QPC_final$Actual.Amount) / 1000, QPC_final$Actual.Amount)

QPC_final$Program.Amount <- ifelse(QPC_final$Units == "NC_millions", as.numeric(QPC_final$Program.Amount) / 1000, QPC_final$Program.Amount)
QPC_final$Revised.Amount <- ifelse(QPC_final$Units == "NC_millions", as.numeric(QPC_final$Revised.Amount) / 1000, QPC_final$Revised.Amount)
QPC_final$Adjusted.Amount <- ifelse(QPC_final$Units == "NC_millions", as.numeric(QPC_final$Adjusted.Amount) / 1000, QPC_final$Adjusted.Amount)
QPC_final$Actual.Amount <- ifelse(QPC_final$Units == "NC_millions", as.numeric(QPC_final$Actual.Amount) / 1000, QPC_final$Actual.Amount)

QPC_final$Program.Amount <- ifelse(QPC_final$Units == "SDR_millions", as.numeric(QPC_final$Program.Amount) / 1000, QPC_final$Program.Amount)
QPC_final$Revised.Amount <- ifelse(QPC_final$Units == "SDR_millions", as.numeric(QPC_final$Revised.Amount) / 1000, QPC_final$Revised.Amount)
QPC_final$Adjusted.Amount <- ifelse(QPC_final$Units == "SDR_millions", as.numeric(QPC_final$Adjusted.Amount) / 1000, QPC_final$Adjusted.Amount)
QPC_final$Actual.Amount <- ifelse(QPC_final$Units == "SDR_millions", as.numeric(QPC_final$Actual.Amount) / 1000, QPC_final$Actual.Amount)


QPC_final <- QPC_final %>%
  filter(!is.na(Program.Amount) & Program.Amount != "")  

#QPC_final <- QPC_final[, -which(names(QPC_final) == "Revised.Amount")]
#QPC_final <- QPC_final[, -which(names(QPC_final) == "Adjusted.Amount")]
#QPC_final <- QPC_final[, -which(names(QPC_final) == "Actual.Amount")]


QPC_final <- QPC_final %>%
  mutate(Country_1 = if_else(grepl("668|744|624|524|439|433|926|911|676|656|648|923|754|738|716|714|688|652|614|564|278|243|968|965|960|921|915|913|746|724|666|664|558|513|512|268|258|218|213|176", Country.Code), 1, 0))


QPC_final <- QPC_final %>%
  mutate(Country_2 = if_else(grepl("668|744|624|524|439|433|926|911|676|656", Country.Code), 1, 0))

colnames(QPC_final)


library(fastDummies)

QPC_final <- QPC_final %>%
  arrange(Arrangement.Type) %>%
  dummy_cols(select_columns = "Arrangement.Type")
#QPC_final <- QPC_final %>% select(-Arrangement.Type)


QPC_final <- QPC_final %>%
  arrange(Country.Code) %>%
  dummy_cols(select_columns = "Country.Code")
#QPC_final <- QPC_final %>% select(-Country.Code)



########################

QPC_final  <- QPC_final %>% distinct()

# Pasar a excel

library(writexl)
write_xlsx(QPC_final, "C:/Users/HP/Desktop/Tesis UDESA/QPC_final.xlsx")


# Tabla Purchases ##################################################################################
# Crear key (corta)
# Columna Review Sequence quedarme solo con los L
# Columna Original Basis sacar el numero que es la revision R del purchase
# Quedarme con el monto (original y actual)  del purchase para cada revision (ACTUAL BASIS)  y su fecha (original y actual).


# Levanto tabla purchases
#Purchases_8 <- read_excel("C:/Users/HP/Desktop/Tesis UDESA/Purchases.xlsx")


library(openxlsx)
url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/Purchases.xlsx"
Purchases_ <- read.xlsx(url)

Purchases <-  Purchases_

# Creo key

colnames(Purchases)

Purchases <- Purchases %>%
  mutate(Key_corta = paste(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Review.Type, sep = "_"))

Purchases <- Purchases %>%
  relocate(Key_corta, .before = 1)

Purch <- Purchases %>%
  mutate(R = as.numeric(str_extract(Review.Type, "\\d+"))) %>%
  relocate(R, .after = 15)

Purch <- Purch %>%
  mutate(Rbis = as.numeric(str_extract(Original.Basis, "\\d+"))) %>%
  relocate(Rbis, .after = 16)

Purch <- Purch %>%
  mutate(Rbis = replace(Rbis, is.na(Rbis), 0))

df_filtrado <- Purch %>%
  filter(R == Rbis)

colnames(df_filtrado)

df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Country.Name")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Country.Code")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Approval.Year")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Approval.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Initial.End.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Initial.End.Year")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Duration.Of.Annual.Arrangement.From")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Duration.Of.Annual.Arrangement.To")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Actual.Set.Aside")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Actual.Augmentation")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Comments")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Sort")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Arrangement.Number")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Arrangement.Type")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Board.Action.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Original.Scheduled.PC.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Revised.Scheduled.PC.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Revised.End.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Program.Type")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Revised.Scheduled.Amount")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Revised.Scheduled.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Revised.Basis")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Rbis")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Review.Type")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Review.Sequence")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Actual.Amount")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Actual.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Actual.Basis")]
colnames(df_filtrado)
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Original.Basis")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "R")]

df_filtrado  <- df_filtrado %>% distinct()

# Pasar a excel
library(writexl)
write_xlsx(df_filtrado, "C:/Users/HP/Desktop/Tesis UDESA/Purch_final.xlsx")

##################################################################################

# Left Join
anti_join(QPC_final, df_filtrado, by = "Key_corta") %>% count()
glimpse(QPC_final$Key_corta)
glimpse(df_filtrado$Key_corta)

sample(unique(QPC_final$Key_corta), 10)
sample(unique(df_filtrado$Key_corta), 10)

library(stringr)

tabla_combined <- QPC_final %>%
  mutate(Key_corta = str_trim(Key_corta)) %>%
  left_join(
    df_filtrado %>% mutate(Key_corta = str_trim(Key_corta)),
    by = "Key_corta"
  )

tabla_combined  <- tabla_combined %>% distinct()

colnames(tabla_combined)

mean(is.na(tabla_combined$Original.Scheduled.Amount)) * 100
mean(is.na(tabla_combined$Original.Scheduled.Date)) * 100

tabla_combined <- tabla_combined %>%
  rename(
    #Arrangement_type = Arrangement.Type,
    QPC_Original_Amount = Program.Amount,
    QPC_Test_Date = Test.Date,
    QPC_Units = Units,
    #QPC_Code = Dqpc.Code,
    QPC_Criteria_Order = Criteria.Order,
    Purchase_Original_Amount = Original.Scheduled.Amount,
    Purchase_Original_Date = Original.Scheduled.Date)

colnames(tabla_combined)

# Creo columna unidades Purchase y elimino columnas redundantes

#tabla_combined$Purchase_Units <- "SDR_millions"

# Crear label

tabla_combined <- tabla_combined %>%
  mutate(
    Status = ifelse(Status == "M", 0, 1)
  )

tabla_combined <- tabla_combined %>%
  relocate(Status, .before = 1)

tabla_combined  <- tabla_combined %>% distinct()

# Pasar a excel
library(writexl)
write_xlsx(tabla_combined, "C:/Users/HP/Desktop/Tesis UDESA/QPC_Purch_final.xlsx")


# Tabla description ##################################################################################
# Crear la key (corta)
# Quedarse con columnas Total Access y Precautionary

library(openxlsx)
url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/Description.xlsx"
Description_13_ <- read.xlsx(url)

Description_13 <- Description_13_

colnames(Description_13)

Description_13 <- Description_13 %>%
  mutate(Key_corta = paste(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Review.Type, sep = "_"))

Desc <- Description_13 %>%
  select(Key_corta, Totalaccess, Precautionary)

Desc <- Desc %>%
  mutate(`Precautionary` = ifelse(is.na(`Precautionary`) | trimws(`Precautionary`) == "", "N", `Precautionary`))

unique(Desc$Precautionary)

Desc <- Desc %>%
  mutate(`Precautionary` = ifelse(is.na(`Precautionary`) | trimws(`Precautionary`) == "Y", 1, 0))

# Joint

tabla <- tabla_combined %>%
  left_join(
    Desc %>%
      distinct(Key_corta, .keep_all = TRUE),# Elimina duplicados
    by = "Key_corta"
  )


tabla_comb <- tabla %>% distinct()

mean(is.na(tabla_comb$Precautionary)) * 100
mean(is.na(tabla_comb$Totalaccess)) * 100

colnames(tabla_comb)

# Pasar a excel
library(writexl)
write_xlsx(tabla_comb, "C:/Users/HP/Desktop/Tesis UDESA/QPC_Purch_Desc_final.xlsx")

# Tabla Mecon ##################################################################################
# Crear key (corta)
# Quedarse solo con las filas R0 (que muestra las variables macro de los tres anios previous al programa, y muestra los valores esperados de esas variables para los proximos 5 anios T a T=4)
# Sacar las tasas de cambio de cada variable macro de T respecto de T-1, de T=1 respecto de T, etc.
# Eliminar todas las columnas que no son la key, ni el Mneumonic, ni las variables macro
# Eliminar las filas que tienen porcentaje y blanks en la columna Indicator Currency
# Pivoteas. Filas ahora son las keys y columnas son cada una de las variables macro para cada momento del tiempo
# Eliminar variables que Tengan mas de 5% de missing values por columna. Asignar la mediana a esos missing values.
# Reexpresar variables macro para evitar nominalidad. Las variables en USD divider por BXG (exportaciones de Bienes) y multiplicar por 100. Las variables en moneda local, dividir por NGDP y multiplicar por 100. Los indices de precios dividir por ENDA (tipo de cambio). Los tipos de cambio dividir por indices de precios (PCPI).

library(openxlsx)
url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/Mecon.xlsx"
Mecon_10_ <- read.xlsx(url)

Mecon_10 <- Mecon_10_

colnames(Mecon_10)

#######

Mecon_10 <- Mecon_10 %>%
  mutate(var_t4_t3 = ((`T+4` - `T+3`) / `T+3`) * 100)

Mecon_10 <- Mecon_10 %>%
  mutate(var_t3_t2 = ((`T+3` - `T+2`) / `T+2`) * 100)

Mecon_10 <- Mecon_10 %>%
  mutate(var_t2_t1 = ((`T+2` - `T+1`) / `T+1`) * 100)

Mecon_10 <- Mecon_10 %>%
  mutate(var_t1_t = ((`T+1` - `T`) / `T`) * 100)

Mecon_10 <- Mecon_10 %>%
  mutate(var_t_t_1 = ((`T` - `T-1`) / `T-1`) * 100)

Mecon_10 <- Mecon_10 %>%
  mutate(var_t_1_t_2 = ((`T-1` - `T-2`) / `T-2`) * 100)

Mecon_10 <- Mecon_10 %>%
  mutate(var_t_2_t_3 = ((`T-2` - `T-3`) / `T-3`) * 100)

Mecon_10 <- Mecon_10 %>%
  mutate(var_t4_t_3 = ((`T+4` - `T-3`) / `T-3`) * 100)

Mecon_10 <- Mecon_10 %>%
  mutate(var_t4_t = ((`T+4` - `T`) / `T`) * 100)

#######

Mecon_10 <- Mecon_10 %>%
  group_by(Country.Code, Arrangement.Number, Mneumonic) %>%
  mutate(
    valor_base = first(`T-3`[Sort == 0]),  # Obtener el valor base cuando Sort == 0
    correccion_T_3 = ifelse(!is.na(valor_base),
                            (`T-3` - valor_base) / valor_base * 100,
                            0)
  ) %>%
  select(-valor_base) %>%  # Eliminar columna temporal
  arrange(Sort) %>%
  ungroup()


Mecon_10 <- Mecon_10 %>%
  group_by(Country.Code, Arrangement.Number, Mneumonic) %>%
  mutate(
    valor_base = first(`T-2`[Sort == 0]),  # Obtener el valor base cuando Sort == 0
    correccion_T_2 = ifelse(!is.na(valor_base),
                            (`T-2` - valor_base) / valor_base * 100,
                            0)
  ) %>%
  select(-valor_base) %>%  # Eliminar columna temporal
  arrange(Sort) %>%
  ungroup()


Mecon_10 <- Mecon_10 %>%
  group_by(Country.Code, Arrangement.Number, Mneumonic) %>%
  mutate(
    valor_base = first(`T-1`[Sort == 0]),  # Obtener el valor base cuando Sort == 0
    correccion_T_1 = ifelse(!is.na(valor_base),
                            (`T-1` - valor_base) / valor_base * 100,
                            0)
  ) %>%
  select(-valor_base) %>%  # Eliminar columna temporal
  arrange(Sort) %>%
  ungroup()


Mecon_10 <- Mecon_10 %>%
  group_by(Country.Code, Arrangement.Number, Mneumonic) %>%
  mutate(
    valor_base = first(`T`[Sort == 0]),  # Obtener el valor base cuando Sort == 0
    correccion_T = ifelse(!is.na(valor_base),
                          (`T` - valor_base) / valor_base * 100,
                          0)
  ) %>%
  select(-valor_base) %>%  # Eliminar columna temporal
  arrange(Sort) %>%
  ungroup()



Mecon_10 <- Mecon_10 %>%
  group_by(Country.Code, Arrangement.Number, Mneumonic) %>%
  mutate(
    valor_base = first(`T+1`[Sort == 0]),  # Obtener el valor base cuando Sort == 0
    correccion_T1 = ifelse(!is.na(valor_base),
                           (`T+1` - valor_base) / valor_base * 100,
                           0)
  ) %>%
  select(-valor_base) %>%  # Eliminar columna temporal
  arrange(Sort) %>%
  ungroup()


Mecon_10 <- Mecon_10 %>%
  group_by(Country.Code, Arrangement.Number, Mneumonic) %>%
  mutate(
    valor_base = first(`T+2`[Sort == 0]),  # Obtener el valor base cuando Sort == 0
    correccion_T2 = ifelse(!is.na(valor_base),
                           (`T+2` - valor_base) / valor_base * 100,
                           0)
  ) %>%
  select(-valor_base) %>%  # Eliminar columna temporal
  arrange(Sort) %>%
  ungroup()


Mecon_10 <- Mecon_10 %>%
  group_by(Country.Code, Arrangement.Number, Mneumonic) %>%
  mutate(
    valor_base = first(`T+3`[Sort == 0]),  # Obtener el valor base cuando Sort == 0
    correccion_T3 = ifelse(!is.na(valor_base),
                           (`T+3` - valor_base) / valor_base * 100,
                           0)
  ) %>%
  select(-valor_base) %>%  # Eliminar columna temporal
  arrange(Sort) %>%
  ungroup()


Mecon_10 <- Mecon_10 %>%
  group_by(Country.Code, Arrangement.Number, Mneumonic) %>%
  mutate(
    valor_base = first(`T+4`[Sort == 0]),  # Obtener el valor base cuando Sort == 0
    correccion_T4 = ifelse(!is.na(valor_base),
                           (`T+4` - valor_base) / valor_base * 100,
                           0)
  ) %>%
  select(-valor_base) %>%  # Eliminar columna temporal
  arrange(Sort) %>%
  ungroup()



#######

df_filt <- Mecon_10

#df_filt <- Mecon_10 %>%
#  filter(
#    grepl("R0", Review.Type, ignore.case = TRUE),
#  )

df_filt  <- df_filt  %>%
  mutate(Key_corta = paste(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Review.Type, sep = "_"))

df_filt  <- df_filt  %>%
  mutate(Key_min = paste(Country.Code , Arrangement.Number, sep = "_"))

df_filt <- df_filt %>%
  relocate(Key_min, .before = 1)

df_filt  <- df_filt %>% distinct()

colnames(df_filt)

macro_df <- df_filt %>%
  select(Key_corta, Mneumonic, "T-3", "T-2", "T-1", "T", "T+1","T+2","T+3","T+4",
         "var_t4_t3",                          
         "var_t3_t2",                          
         "var_t2_t1",                          
         "var_t1_t",                          
         "var_t_t_1",                          
         "var_t_1_t_2",                        
         "var_t_2_t_3" ,                      
         "var_t4_t_3",                        
         "var_t4_t"  ,                        
         "correccion_T_3",                    
         "correccion_T_2",                    
         "correccion_T_1" ,                    
         "correccion_T" ,                      
         "correccion_T1",                      
         "correccion_T2",                      
         "correccion_T3" ,                    
         "correccion_T4")

sapply(macro_df, class)



##########

# pivoteamos

library(tidyr)

wide_data <- macro_df %>%
  pivot_wider(names_from = 'Mneumonic', values_from = c("T-3", "T-2", "T-1", "T", "T+1","T+2","T+3","T+4",
                                                        "var_t4_t3",                          
                                                        "var_t3_t2",                          
                                                        "var_t2_t1",                          
                                                        "var_t1_t",                          
                                                        "var_t_t_1",                          
                                                        "var_t_1_t_2",                        
                                                        "var_t_2_t_3" ,                      
                                                        "var_t4_t_3",                        
                                                        "var_t4_t"  ,                        
                                                        "correccion_T_3",                    
                                                        "correccion_T_2",                    
                                                        "correccion_T_1" ,                    
                                                        "correccion_T" ,                      
                                                        "correccion_T1",                      
                                                        "correccion_T2",                      
                                                        "correccion_T3" ,                    
                                                        "correccion_T4"), values_fn = mean )

sapply(wide_data, class)

# si mas del 80 porciento de una columna son NULL, entonces eliminar la columna

df_clean <- wide_data %>%
  select(where(~mean(is.na(.x) | sapply(.x, is.null)) <= 0.05))

sapply(df_clean, class)


# Reemplazar NA por la media

df_clean <- df_clean %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

df_clean <- df_clean %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.) | . == "", median(., na.rm = TRUE), .)))

any(is.na(df_clean))


##################################################################################

# Join

test_join <- head(tabla_comb) %>%
  left_join(head(df_clean), by = "Key_corta")

# Obtener las keys únicas de ambas tablas
keys_tabla_comb <- unique(tabla_comb$Key_corta)
keys_df_clean <- unique(df_clean$Key_corta)

# Encontrar intersección (keys comunes)
keys_comunes <- intersect(keys_tabla_comb, keys_df_clean)


tabla <- tabla_comb %>%
  mutate(Key_corta = str_trim(Key_corta) %>% toupper()) %>%
  left_join(
    df_clean %>%
      mutate(Key_corta = str_trim(Key_corta) %>% toupper()) %>%
      distinct(Key_corta, .keep_all = TRUE),
    by = "Key_corta"
  )

colnames(tabla)

#tabla <- tabla[, -which(names(tabla) == "Key_corta.x")]
#tabla <- tabla[, -which(names(tabla) == "Key_corta.y")]
#tabla <- tabla[, -which(names(tabla) == "Key_min")]

tabla <- tabla %>% distinct()

colnames(tabla)

# Pasar a excel

library(writexl)
write_xlsx(tabla , "C:/Users/HP/Desktop/Tesis UDESA/QPC_Purch_Desc_Macro.xlsx")

##################################################################################
# Eliminar filas si variables macro son blanck

colnames(tabla)

tabla1 <- tabla %>%
  filter(!is.na(`T-3_BXG`), `T-3_BXG` != "")

tabla2 <- tabla1 %>%
  filter(!is.na(`T-3_NGDP`), `T-3_NGDP` != "")

tabla3 <- tabla2 %>%
  filter(!is.na(`QPC_Original_Amount`), `QPC_Original_Amount` != "")

colnames(tabla3)

tabla5 <- tabla3 %>%
  select(where(~mean(is.na(.x) | sapply(.x, is.null)) <= 0.1))

tabla6 <- tabla5 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.) | . == "", median(., na.rm = TRUE), .)))

colnames(tabla6)


##################################################################################
library(lubridate)

#tabla6$Purchase_Original_Date <- dmy(tabla6$Purchase_Original_Date)
#tabla6$Approval.Date_  <- as.Date(tabla6$Approval.Date, origin = "1899-12-30")

tabla6$Purchase_date_span <- tabla6$Purchase_Original_Date - tabla6$Approval.Date
tabla6$Purchase_date_span <- as.numeric(tabla6$Purchase_date_span)

tabla6 <- tabla6 %>% select(-Purchase_Original_Date)


##################################################################################
# Eliminar nominalidad

tabla6$`T-3_BCA_bxg` <- tabla6$`T-3_BCA` / tabla6$`T-3_BXG` *100
tabla6$`T-3_BXS_bxg` <- tabla6$`T-3_BXS` / tabla6$`T-3_BXG` *100
tabla6$`T-3_BK_bxg` <- tabla6$`T-3_BK` / tabla6$`T-3_BXG` *100
#tabla6$`T-3_BFP_bxg` <- tabla6$`T-3_BFP` / tabla6$`T-3_BXG` *100
tabla6$`T-3_BMG_bxg` <- tabla6$`T-3_BMG` / tabla6$`T-3_BXG` *100
tabla6$`T-3_BFD_bxg` <- tabla6$`T-3_BFD` / tabla6$`T-3_BXG` *100
tabla6$`T-3_BMS_bxg` <- tabla6$`T-3_BMS` / tabla6$`T-3_BXG` *100
tabla6$`T-3_D_bxg` <- tabla6$`T-3_D` / tabla6$`T-3_BXG` *100

tabla6$`T-3_NFI_gdp` <- tabla6$`T-3_NFI` / tabla6$`T-3_NGDP` *100
tabla6$`T-3_FMB_gdp` <- tabla6$`T-3_FMB` / tabla6$`T-3_NGDP` *100
tabla6$`T-3_NCG_gdp` <- tabla6$`T-3_NCG` / tabla6$`T-3_NGDP` *100
tabla6$`T-3_NCP_gdp` <- tabla6$`T-3_NCP` / tabla6$`T-3_NGDP` *100
tabla6$`T-3_NM_gdp` <- tabla6$`T-3_NM` / tabla6$`T-3_NGDP` *100
tabla6$`T-3_NX_gdp` <- tabla6$`T-3_NX` / tabla6$`T-3_NGDP` *100
tabla6$`T-3_NFI_gdp` <- tabla6$`T-3_NFI` / tabla6$`T-3_NGDP` *100
tabla6$`T-3_NFI_gdp` <- tabla6$`T-3_NFI` / tabla6$`T-3_NGDP` *100

tabla6$`T-3_PCPI_real` <- tabla6$`T-3_PCPI` / tabla6$`T-3_ENDA`
tabla6$`T-3_NGDP_usd` <- tabla6$`T-3_NGDP` / tabla6$`T-3_ENDA`
tabla6$`T-3_ENDA_pcpi` <- tabla6$`T-3_ENDA` / tabla6$`T-3_PCPI`
tabla6$`T-3_BXG_gdp_usd` <- tabla6$`T-3_BXG` / tabla6$`T-3_NGDP_usd`
tabla6$`T-3_PCPIE_real` <- tabla6$`T-3_PCPIE` / tabla6$`T-3_ENDA`


tabla6$`T-2_BCA_bxg` <- tabla6$`T-2_BCA` / tabla6$`T-2_BXG` *100
tabla6$`T-2_BXS_bxg` <- tabla6$`T-2_BXS` / tabla6$`T-2_BXG` *100
tabla6$`T-2_BK_bxg` <- tabla6$`T-2_BK` / tabla6$`T-2_BXG` *100
tabla6$`T-2_BFP_bxg` <- tabla6$`T-2_BFP` / tabla6$`T-2_BXG` *100
tabla6$`T-2_BMG_bxg` <- tabla6$`T-2_BMG` / tabla6$`T-2_BXG` *100
tabla6$`T-2_BFD_bxg` <- tabla6$`T-2_BFD` / tabla6$`T-2_BXG` *100
tabla6$`T-2_BMS_bxg` <- tabla6$`T-2_BMS` / tabla6$`T-2_BXG` *100
tabla6$`T-2_D_bxg` <- tabla6$`T-2_D` / tabla6$`T-2_BXG` *100

tabla6$`T-2_NFI_gdp` <- tabla6$`T-2_NFI` / tabla6$`T-2_NGDP` *100
tabla6$`T-2_FMB_gdp` <- tabla6$`T-2_FMB` / tabla6$`T-2_NGDP` *100
tabla6$`T-2_NCG_gdp` <- tabla6$`T-2_NCG` / tabla6$`T-2_NGDP` *100
tabla6$`T-2_NCP_gdp` <- tabla6$`T-2_NCP` / tabla6$`T-2_NGDP` *100
tabla6$`T-2_NM_gdp` <- tabla6$`T-2_NM` / tabla6$`T-2_NGDP` *100
tabla6$`T-2_NX_gdp` <- tabla6$`T-2_NX` / tabla6$`T-2_NGDP` *100
tabla6$`T-2_NFI_gdp` <- tabla6$`T-2_NFI` / tabla6$`T-2_NGDP` *100
tabla6$`T-2_NFI_gdp` <- tabla6$`T-2_NFI` / tabla6$`T-2_NGDP` *100

tabla6$`T-2_PCPI_real` <- tabla6$`T-2_PCPI` / tabla6$`T-2_ENDA`
tabla6$`T-2_NGDP_usd` <- tabla6$`T-2_NGDP` / tabla6$`T-2_ENDA`
tabla6$`T-2_ENDA_pcpi` <- tabla6$`T-2_ENDA` / tabla6$`T-2_PCPI`
tabla6$`T-2_BXG_gdp_usd` <- tabla6$`T-2_BXG` / tabla6$`T-2_NGDP_usd`
tabla6$`T-2_PCPIE_real` <- tabla6$`T-2_PCPIE` / tabla6$`T-2_ENDA`

tabla6$`T-1_BCA_bxg` <- tabla6$`T-1_BCA` / tabla6$`T-1_BXG` *100
tabla6$`T-1_BXS_bxg` <- tabla6$`T-1_BXS` / tabla6$`T-1_BXG` *100
tabla6$`T-1_BK_bxg` <- tabla6$`T-1_BK` / tabla6$`T-1_BXG` *100
tabla6$`T-1_BFP_bxg` <- tabla6$`T-1_BFP` / tabla6$`T-1_BXG` *100
tabla6$`T-1_BMG_bxg` <- tabla6$`T-1_BMG` / tabla6$`T-1_BXG` *100
tabla6$`T-1_BFD_bxg` <- tabla6$`T-1_BFD` / tabla6$`T-1_BXG` *100
tabla6$`T-1_BMS_bxg` <- tabla6$`T-1_BMS` / tabla6$`T-1_BXG` *100
tabla6$`T-1_D_bxg` <- tabla6$`T-1_D` / tabla6$`T-1_BXG` *100

tabla6$`T-1_NFI_gdp` <- tabla6$`T-1_NFI` / tabla6$`T-1_NGDP` *100
tabla6$`T-1_FMB_gdp` <- tabla6$`T-1_FMB` / tabla6$`T-1_NGDP` *100
tabla6$`T-1_NCG_gdp` <- tabla6$`T-1_NCG` / tabla6$`T-1_NGDP` *100
tabla6$`T-1_NCP_gdp` <- tabla6$`T-1_NCP` / tabla6$`T-1_NGDP` *100
tabla6$`T-1_NM_gdp` <- tabla6$`T-1_NM` / tabla6$`T-1_NGDP` *100
tabla6$`T-1_NX_gdp` <- tabla6$`T-1_NX` / tabla6$`T-1_NGDP` *100
tabla6$`T-1_NFI_gdp` <- tabla6$`T-1_NFI` / tabla6$`T-1_NGDP` *100
tabla6$`T-1_NFI_gdp` <- tabla6$`T-1_NFI` / tabla6$`T-1_NGDP` *100

tabla6$`T-1_PCPI_real` <- tabla6$`T-1_PCPI` / tabla6$`T-1_ENDA`
tabla6$`T-1_NGDP_usd` <- tabla6$`T-1_NGDP` / tabla6$`T-1_ENDA`
tabla6$`T-1_ENDA_pcpi` <- tabla6$`T-1_ENDA` / tabla6$`T-1_PCPI`
tabla6$`T-1_BXG_gdp_usd` <- tabla6$`T-1_BXG` / tabla6$`T-1_NGDP_usd`
tabla6$`T-1_PCPIE_real` <- tabla6$`T-1_PCPIE` / tabla6$`T-1_ENDA`


tabla6$`T_BCA_bxg` <- tabla6$`T_BCA` / tabla6$`T_BXG` *100
tabla6$`T_BXS_bxg` <- tabla6$`T_BXS` / tabla6$`T_BXG` *100
tabla6$`T_BK_bxg` <- tabla6$`T_BK` / tabla6$`T_BXG` *100
tabla6$`T_BFP_bxg` <- tabla6$`T_BFP` / tabla6$`T_BXG` *100
tabla6$`T_BMG_bxg` <- tabla6$`T_BMG` / tabla6$`T_BXG` *100
tabla6$`T_BFD_bxg` <- tabla6$`T_BFD` / tabla6$`T_BXG` *100
tabla6$`T_BMS_bxg` <- tabla6$`T_BMS` / tabla6$`T_BXG` *100
tabla6$`T_D_bxg` <- tabla6$`T_D` / tabla6$`T_BXG` *100

tabla6$`T_NFI_gdp` <- tabla6$`T_NFI` / tabla6$`T_NGDP` *100
tabla6$`T_FMB_gdp` <- tabla6$`T_FMB` / tabla6$`T_NGDP` *100
tabla6$`T_NCG_gdp` <- tabla6$`T_NCG` / tabla6$`T_NGDP` *100
tabla6$`T_NCP_gdp` <- tabla6$`T_NCP` / tabla6$`T_NGDP` *100
tabla6$`T_NM_gdp` <- tabla6$`T_NM` / tabla6$`T_NGDP` *100
tabla6$`T_NX_gdp` <- tabla6$`T_NX` / tabla6$`T_NGDP` *100
tabla6$`T_NFI_gdp` <- tabla6$`T_NFI` / tabla6$`T_NGDP` *100
tabla6$`T_NFI_gdp` <- tabla6$`T_NFI` / tabla6$`T_NGDP` *100

tabla6$`T_PCPI_real` <- tabla6$`T_PCPI` / tabla6$`T_ENDA`
tabla6$`T_NGDP_usd` <- tabla6$`T_NGDP` / tabla6$`T_ENDA`
tabla6$`T_ENDA_pcpi` <- tabla6$`T_ENDA` / tabla6$`T_PCPI`
tabla6$`T_BXG_gdp_usd` <- tabla6$`T_BXG` / tabla6$`T_NGDP_usd`
tabla6$`T_PCPIE_real` <- tabla6$`T_PCPIE` / tabla6$`T_ENDA`


tabla6$`T+1_BCA_bxg` <- tabla6$`T+1_BCA` / tabla6$`T+1_BXG` *100
tabla6$`T+1_BXS_bxg` <- tabla6$`T+1_BXS` / tabla6$`T+1_BXG` *100
tabla6$`T+1_BK_bxg` <- tabla6$`T+1_BK` / tabla6$`T+1_BXG` *100
tabla6$`T+1_BFP_bxg` <- tabla6$`T+1_BFP` / tabla6$`T+1_BXG` *100
tabla6$`T+1_BMG_bxg` <- tabla6$`T+1_BMG` / tabla6$`T+1_BXG` *100
tabla6$`T+1_BFD_bxg` <- tabla6$`T+1_BFD` / tabla6$`T+1_BXG` *100
tabla6$`T+1_BMS_bxg` <- tabla6$`T+1_BMS` / tabla6$`T+1_BXG` *100
tabla6$`T+1_D_bxg` <- tabla6$`T+1_D` / tabla6$`T+1_BXG` *100

tabla6$`T+1_NFI_gdp` <- tabla6$`T+1_NFI` / tabla6$`T+1_NGDP` *100
#tabla6$`T+1_FMB_gdp` <- tabla6$`T+1_FMB` / tabla6$`T+1_NGDP` *100
tabla6$`T+1_NCG_gdp` <- tabla6$`T+1_NCG` / tabla6$`T+1_NGDP` *100
tabla6$`T+1_NCP_gdp` <- tabla6$`T+1_NCP` / tabla6$`T+1_NGDP` *100
tabla6$`T+1_NM_gdp` <- tabla6$`T+1_NM` / tabla6$`T+1_NGDP` *100
tabla6$`T+1_NX_gdp` <- tabla6$`T+1_NX` / tabla6$`T+1_NGDP` *100
tabla6$`T+1_NFI_gdp` <- tabla6$`T+1_NFI` / tabla6$`T+1_NGDP` *100
tabla6$`T+1_NFI_gdp` <- tabla6$`T+1_NFI` / tabla6$`T+1_NGDP` *100

tabla6$`T+1_PCPI_real` <- tabla6$`T+1_PCPI` / tabla6$`T+1_ENDA`
tabla6$`T+1_NGDP_usd` <- tabla6$`T+1_NGDP` / tabla6$`T+1_ENDA`
tabla6$`T+1_ENDA_pcpi` <- tabla6$`T+1_ENDA` / tabla6$`T+1_PCPI`
tabla6$`T+1_BXG_gdp_usd` <- tabla6$`T+1_BXG` / tabla6$`T+1_NGDP_usd`
tabla6$`T+1_PCPIE_real` <- tabla6$`T+1_PCPIE` / tabla6$`T+1_ENDA`


tabla6$`T+2_BCA_bxg` <- tabla6$`T+2_BCA` / tabla6$`T+2_BXG` *100
tabla6$`T+2_BXS_bxg` <- tabla6$`T+2_BXS` / tabla6$`T+2_BXG` *100
tabla6$`T+2_BK_bxg` <- tabla6$`T+2_BK` / tabla6$`T+2_BXG` *100
tabla6$`T+2_BFP_bxg` <- tabla6$`T+2_BFP` / tabla6$`T+2_BXG` *100
tabla6$`T+2_BMG_bxg` <- tabla6$`T+2_BMG` / tabla6$`T+2_BXG` *100
tabla6$`T+2_BFD_bxg` <- tabla6$`T+2_BFD` / tabla6$`T+2_BXG` *100
tabla6$`T+2_BMS_bxg` <- tabla6$`T+2_BMS` / tabla6$`T+2_BXG` *100
tabla6$`T+2_D_bxg` <- tabla6$`T+2_D` / tabla6$`T+2_BXG` *100

tabla6$`T+2_NFI_gdp` <- tabla6$`T+2_NFI` / tabla6$`T+2_NGDP` *100
#tabla6$`T+2_FMB_gdp` <- tabla6$`T+2_FMB` / tabla6$`T+2_NGDP` *100
tabla6$`T+2_NCG_gdp` <- tabla6$`T+2_NCG` / tabla6$`T+2_NGDP` *100
tabla6$`T+2_NCP_gdp` <- tabla6$`T+2_NCP` / tabla6$`T+2_NGDP` *100
tabla6$`T+2_NM_gdp` <- tabla6$`T+2_NM` / tabla6$`T+2_NGDP` *100
tabla6$`T+2_NX_gdp` <- tabla6$`T+2_NX` / tabla6$`T+2_NGDP` *100
tabla6$`T+2_NFI_gdp` <- tabla6$`T+2_NFI` / tabla6$`T+2_NGDP` *100
tabla6$`T+2_NFI_gdp` <- tabla6$`T+2_NFI` / tabla6$`T+2_NGDP` *100

tabla6$`T+2_PCPI_real` <- tabla6$`T+2_PCPI` / tabla6$`T+2_ENDA`
tabla6$`T+2_NGDP_usd` <- tabla6$`T+2_NGDP` / tabla6$`T+2_ENDA`
tabla6$`T+2_ENDA_pcpi` <- tabla6$`T+2_ENDA` / tabla6$`T+2_PCPI`
tabla6$`T+2_BXG_gdp_usd` <- tabla6$`T+2_BXG` / tabla6$`T+2_NGDP_usd`
tabla6$`T+2_PCPIE_real` <- tabla6$`T+2_PCPIE` / tabla6$`T+2_ENDA`


tabla6$`T+3_BCA_bxg` <- tabla6$`T+3_BCA` / tabla6$`T+3_BXG` *100
tabla6$`T+3_BXS_bxg` <- tabla6$`T+3_BXS` / tabla6$`T+3_BXG` *100
tabla6$`T+3_BK_bxg` <- tabla6$`T+3_BK` / tabla6$`T+3_BXG` *100
tabla6$`T+3_BFP_bxg` <- tabla6$`T+3_BFP` / tabla6$`T+3_BXG` *100
tabla6$`T+3_BMG_bxg` <- tabla6$`T+3_BMG` / tabla6$`T+3_BXG` *100
tabla6$`T+3_BFD_bxg` <- tabla6$`T+3_BFD` / tabla6$`T+3_BXG` *100
tabla6$`T+3_BMS_bxg` <- tabla6$`T+3_BMS` / tabla6$`T+3_BXG` *100
tabla6$`T+3_D_bxg` <- tabla6$`T+3_D` / tabla6$`T+3_BXG` *100

tabla6$`T+3_NFI_gdp` <- tabla6$`T+3_NFI` / tabla6$`T+3_NGDP` *100
#tabla6$`T+3_FMB_gdp` <- tabla6$`T+3_FMB` / tabla6$`T+3_NGDP` *100
tabla6$`T+3_NCG_gdp` <- tabla6$`T+3_NCG` / tabla6$`T+3_NGDP` *100
tabla6$`T+3_NCP_gdp` <- tabla6$`T+3_NCP` / tabla6$`T+3_NGDP` *100
tabla6$`T+3_NM_gdp` <- tabla6$`T+3_NM` / tabla6$`T+3_NGDP` *100
tabla6$`T+3_NX_gdp` <- tabla6$`T+3_NX` / tabla6$`T+3_NGDP` *100
tabla6$`T+3_NFI_gdp` <- tabla6$`T+3_NFI` / tabla6$`T+3_NGDP` *100
tabla6$`T+3_NFI_gdp` <- tabla6$`T+3_NFI` / tabla6$`T+3_NGDP` *100

tabla6$`T+3_PCPI_real` <- tabla6$`T+3_PCPI` / tabla6$`T+3_ENDA`
tabla6$`T+3_NGDP_usd` <- tabla6$`T+3_NGDP` / tabla6$`T+3_ENDA`
tabla6$`T+3_ENDA_pcpi` <- tabla6$`T+3_ENDA` / tabla6$`T+3_PCPI`
tabla6$`T+3_BXG_gdp_usd` <- tabla6$`T+3_BXG` / tabla6$`T+3_NGDP_usd`
tabla6$`T+3_PCPIE_real` <- tabla6$`T+3_PCPIE` / tabla6$`T+3_ENDA`


tabla6$`T+4_BCA_bxg` <- tabla6$`T+4_BCA` / tabla6$`T+4_BXG` *100
tabla6$`T+4_BXS_bxg` <- tabla6$`T+4_BXS` / tabla6$`T+4_BXG` *100
tabla6$`T+4_BK_bxg` <- tabla6$`T+4_BK` / tabla6$`T+4_BXG` *100
#tabla6$`T+4_BFP_bxg` <- tabla6$`T+4_BFP` / tabla6$`T+4_BXG` *100
tabla6$`T+4_BMG_bxg` <- tabla6$`T+4_BMG` / tabla6$`T+4_BXG` *100
tabla6$`T+4_BFD_bxg` <- tabla6$`T+4_BFD` / tabla6$`T+4_BXG` *100
tabla6$`T+4_BMS_bxg` <- tabla6$`T+4_BMS` / tabla6$`T+4_BXG` *100
tabla6$`T+4_D_bxg` <- tabla6$`T+4_D` / tabla6$`T+4_BXG` *100

tabla6$`T+4_NFI_gdp` <- tabla6$`T+4_NFI` / tabla6$`T+4_NGDP` *100
#tabla6$`T+4_FMB_gdp` <- tabla6$`T+4_FMB` / tabla6$`T+4_NGDP` *100
tabla6$`T+4_NCG_gdp` <- tabla6$`T+4_NCG` / tabla6$`T+4_NGDP` *100
tabla6$`T+4_NCP_gdp` <- tabla6$`T+4_NCP` / tabla6$`T+4_NGDP` *100
tabla6$`T+4_NM_gdp` <- tabla6$`T+4_NM` / tabla6$`T+4_NGDP` *100
tabla6$`T+4_NX_gdp` <- tabla6$`T+4_NX` / tabla6$`T+4_NGDP` *100
tabla6$`T+4_NFI_gdp` <- tabla6$`T+4_NFI` / tabla6$`T+4_NGDP` *100
tabla6$`T+4_NFI_gdp` <- tabla6$`T+4_NFI` / tabla6$`T+4_NGDP` *100

tabla6$`T+4_PCPI_real` <- tabla6$`T+4_PCPI` / tabla6$`T+4_ENDA`
tabla6$`T+4_NGDP_usd` <- tabla6$`T+4_NGDP` / tabla6$`T+4_ENDA`
tabla6$`T+4_ENDA_pcpi` <- tabla6$`T+4_ENDA` / tabla6$`T+4_PCPI`
tabla6$`T+4_BXG_gdp_usd` <- tabla6$`T+4_BXG` / tabla6$`T+4_NGDP_usd`
tabla6$`T+4_PCPIE_real` <- tabla6$`T+4_PCPIE` / tabla6$`T+4_ENDA`


# crear nuevas variables como ratios

tabla6$QPC_Units_ <- substr(tabla6$QPC_Units, 1, 3)
unique(tabla6$QPC_Units_)
colnames(tabla6)

tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "USD", as.numeric(tabla6$QPC_Original_Amount) / tabla6$`T-1_BXG`, tabla6$QPC_Original_Amount)
tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "NC_", as.numeric(tabla6$QPC_Original_Amount) / tabla6$`T-1_NGDP`, tabla6$QPC_Original_Amount)
tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "SDR", as.numeric(tabla6$QPC_Original_Amount) / tabla6$`T-1_BXG`, tabla6$QPC_Original_Amount)

glimpse(tabla6)
tabla6$QPC_Original_Amount_real <- as.numeric(tabla6$QPC_Original_Amount_real)
tabla6$QPC_Original_Amount <- as.numeric(tabla6$QPC_Original_Amount)

############################################################################

colnames(tabla6)

tabla6$Purchase_Original_Amount_real <- as.numeric(tabla6$Purchase_Original_Amount) / tabla6$`T-1_BXG`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_real`[is.finite(tabla6$`Purchase_Original_Amount_real`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_real`[is.na(tabla6$`Purchase_Original_Amount_real`)] <- mediana_valor
sum(is.na(tabla6$Purchase_Original_Amount_real))

##

tabla6$`Purchase_Original_Amount_bxg_1` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-1_BXG`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_bxg_1`[is.finite(tabla6$`Purchase_Original_Amount_bxg_1`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_bxg_1`[is.infinite(tabla6$`Purchase_Original_Amount_bxg_1`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_bxg_2` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-2_BXG`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_bxg_2`[is.finite(tabla6$`Purchase_Original_Amount_bxg_2`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_bxg_2`[is.infinite(tabla6$`Purchase_Original_Amount_bxg_2`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_bxg_3` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-3_BXG`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_bxg_3`[is.finite(tabla6$`Purchase_Original_Amount_bxg_3`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_bxg_3`[is.infinite(tabla6$`Purchase_Original_Amount_bxg_3`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_bxg_t` <- tabla6$`Purchase_Original_Amount` / tabla6$`T_BXG`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_bxg_t`[is.finite(tabla6$`Purchase_Original_Amount_bxg_t`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_bxg_t`[is.infinite(tabla6$`Purchase_Original_Amount_bxg_t`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_bxg+1` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+1_BXG`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_bxg+1`[is.finite(tabla6$`Purchase_Original_Amount_bxg+1`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_bxg+1`[is.infinite(tabla6$`Purchase_Original_Amount_bxg+1`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_bxg+2` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+2_BXG`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_bxg+2`[is.finite(tabla6$`Purchase_Original_Amount_bxg+2`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_bxg+2`[is.infinite(tabla6$`Purchase_Original_Amount_bxg+2`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_bxg+3` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+3_BXG`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_bxg+3`[is.finite(tabla6$`Purchase_Original_Amount_bxg+3`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_bxg+3`[is.infinite(tabla6$`Purchase_Original_Amount_bxg+3`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_bxg+4` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+4_BXG`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_bxg+4`[is.finite(tabla6$`Purchase_Original_Amount_bxg+4`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_bxg+4`[is.infinite(tabla6$`Purchase_Original_Amount_bxg+4`)] <- mediana_valor

##

tabla6$`Purchase_Original_Amount_ENDA_1` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-1_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA_1`[is.finite(tabla6$`Purchase_Original_Amount_ENDA_1`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA_1`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA_1`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA_2` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-2_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA_2`[is.finite(tabla6$`Purchase_Original_Amount_ENDA_2`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA_2`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA_2`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA_3` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-3_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA_3`[is.finite(tabla6$`Purchase_Original_Amount_ENDA_3`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA_3`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA_3`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA_t` <- tabla6$`Purchase_Original_Amount` / tabla6$`T_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA_t`[is.finite(tabla6$`Purchase_Original_Amount_ENDA_t`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA_t`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA_t`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA+1` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+1_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA+1`[is.finite(tabla6$`Purchase_Original_Amount_ENDA+1`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA+1`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA+1`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA+2` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+2_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA+2`[is.finite(tabla6$`Purchase_Original_Amount_ENDA+2`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA+2`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA+2`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA+3` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+3_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA+3`[is.finite(tabla6$`Purchase_Original_Amount_ENDA+3`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA+3`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA+3`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA+4` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+4_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA+4`[is.finite(tabla6$`Purchase_Original_Amount_ENDA+4`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA+4`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA+4`)] <- mediana_valor

##

tabla6$`Purchase_Original_Amount_BCA_1` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-1_BCA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_BCA_1`[is.finite(tabla6$`Purchase_Original_Amount_BCA_1`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_BCA_1`[is.infinite(tabla6$`Purchase_Original_Amount_BCA_1`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_BCA_2` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-2_BCA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_BCA_2`[is.finite(tabla6$`Purchase_Original_Amount_BCA_2`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_BCA_2`[is.infinite(tabla6$`Purchase_Original_Amount_BCA_2`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_BCA_3` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-3_BCA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_BCA_3`[is.finite(tabla6$`Purchase_Original_Amount_BCA_3`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_BCA_3`[is.infinite(tabla6$`Purchase_Original_Amount_BCA_3`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_BCA_t` <- tabla6$`Purchase_Original_Amount` / tabla6$`T_BCA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_BCA_t`[is.finite(tabla6$`Purchase_Original_Amount_BCA_t`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_BCA_t`[is.infinite(tabla6$`Purchase_Original_Amount_BCA_t`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_BCA+1` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+1_BCA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_BCA+1`[is.finite(tabla6$`Purchase_Original_Amount_BCA+1`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_BCA+1`[is.infinite(tabla6$`Purchase_Original_Amount_BCA+1`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_BCA+2` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+2_BCA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_BCA+2`[is.finite(tabla6$`Purchase_Original_Amount_BCA+2`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_BCA+2`[is.infinite(tabla6$`Purchase_Original_Amount_BCA+2`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_BCA+3` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+3_BCA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_BCA+3`[is.finite(tabla6$`Purchase_Original_Amount_BCA+3`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_BCA+3`[is.infinite(tabla6$`Purchase_Original_Amount_BCA+3`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_BCA+4` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+4_BCA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_BCA+4`[is.finite(tabla6$`Purchase_Original_Amount_BCA+4`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_BCA+4`[is.infinite(tabla6$`Purchase_Original_Amount_BCA+4`)] <- mediana_valor

##

tabla6$`Purchase_Original_Amount_D_1` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-1_D`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_D_1`[is.finite(tabla6$`Purchase_Original_Amount_D_1`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_D_1`[is.infinite(tabla6$`Purchase_Original_Amount_D_1`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_D_2` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-2_D`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_D_2`[is.finite(tabla6$`Purchase_Original_Amount_D_2`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_D_2`[is.infinite(tabla6$`Purchase_Original_Amount_D_2`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_D_3` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-3_D`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_D_3`[is.finite(tabla6$`Purchase_Original_Amount_D_3`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_D_3`[is.infinite(tabla6$`Purchase_Original_Amount_D_3`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_D_t` <- tabla6$`Purchase_Original_Amount` / tabla6$`T_D`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_D_t`[is.finite(tabla6$`Purchase_Original_Amount_D_t`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_D_t`[is.infinite(tabla6$`Purchase_Original_Amount_D_t`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_D+1` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+1_D`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_D+1`[is.finite(tabla6$`Purchase_Original_Amount_D+1`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_D+1`[is.infinite(tabla6$`Purchase_Original_Amount_D+1`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_D+2` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+2_D`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_D+2`[is.finite(tabla6$`Purchase_Original_Amount_D+2`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_D+2`[is.infinite(tabla6$`Purchase_Original_Amount_D+2`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_D+3` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+3_D`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_D+3`[is.finite(tabla6$`Purchase_Original_Amount_D+3`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_D+3`[is.infinite(tabla6$`Purchase_Original_Amount_D+3`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_D+4` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+4_D`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_D+4`[is.finite(tabla6$`Purchase_Original_Amount_D+4`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_D+4`[is.infinite(tabla6$`Purchase_Original_Amount_D+4`)] <- mediana_valor


##############################################################################

tabla6$`QPC_Original_Amount_bxg_1` <- tabla6$`QPC_Original_Amount` / tabla6$`T-1_BXG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_bxg_1`[is.finite(tabla6$`QPC_Original_Amount_bxg_1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_bxg_1`[is.infinite(tabla6$`QPC_Original_Amount_bxg_1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_bxg_4` <- tabla6$`QPC_Original_Amount` / tabla6$`T+4_BXG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_bxg_4`[is.finite(tabla6$`QPC_Original_Amount_bxg_4`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_bxg_4`[is.infinite(tabla6$`QPC_Original_Amount_bxg_4`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ngdp_1` <- tabla6$`QPC_Original_Amount` / tabla6$`T-1_NGDP`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ngdp_1`[is.finite(tabla6$`QPC_Original_Amount_ngdp_1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ngdp_1`[is.infinite(tabla6$`QPC_Original_Amount_ngdp_1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ngdp_2` <- tabla6$`QPC_Original_Amount` / tabla6$`T-2_NGDP`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ngdp_2`[is.finite(tabla6$`QPC_Original_Amount_ngdp_2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ngdp_2`[is.infinite(tabla6$`QPC_Original_Amount_ngdp_2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ngdp_3` <- tabla6$`QPC_Original_Amount` / tabla6$`T-3_NGDP`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ngdp_3`[is.finite(tabla6$`QPC_Original_Amount_ngdp_3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ngdp_3`[is.infinite(tabla6$`QPC_Original_Amount_ngdp_3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ngdp_t` <- tabla6$`QPC_Original_Amount` / tabla6$`T_NGDP`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ngdp_t`[is.finite(tabla6$`QPC_Original_Amount_ngdp_t`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ngdp_t`[is.infinite(tabla6$`QPC_Original_Amount_ngdp_t`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ngdp+1` <- tabla6$`QPC_Original_Amount` / tabla6$`T+1_NGDP`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ngdp+1`[is.finite(tabla6$`QPC_Original_Amount_ngdp+1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ngdp+1`[is.infinite(tabla6$`QPC_Original_Amount_ngdp+1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ngdp+2` <- tabla6$`QPC_Original_Amount` / tabla6$`T+2_NGDP`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ngdp+2`[is.finite(tabla6$`QPC_Original_Amount_ngdp+2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ngdp+2`[is.infinite(tabla6$`QPC_Original_Amount_ngdp+2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ngdp+3` <- tabla6$`QPC_Original_Amount` / tabla6$`T+3_NGDP`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ngdp+3`[is.finite(tabla6$`QPC_Original_Amount_ngdp+3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ngdp+3`[is.infinite(tabla6$`QPC_Original_Amount_ngdp+3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ngdp+4` <- tabla6$`QPC_Original_Amount` / tabla6$`T+4_NGDP`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ngdp+4`[is.finite(tabla6$`QPC_Original_Amount_ngdp+4`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ngdp+4`[is.infinite(tabla6$`QPC_Original_Amount_ngdp+4`)] <- mediana_valor

tabla6$`QPC_Original_Amount_bxg_1` <- tabla6$`QPC_Original_Amount` / tabla6$`T-1_BXG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_bxg_1`[is.finite(tabla6$`QPC_Original_Amount_bxg_1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_bxg_1`[is.infinite(tabla6$`QPC_Original_Amount_bxg_1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_bxg_2` <- tabla6$`QPC_Original_Amount` / tabla6$`T-2_BXG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_bxg_2`[is.finite(tabla6$`QPC_Original_Amount_bxg_2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_bxg_2`[is.infinite(tabla6$`QPC_Original_Amount_bxg_2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_bxg_3` <- tabla6$`QPC_Original_Amount` / tabla6$`T-3_BXG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_bxg_3`[is.finite(tabla6$`QPC_Original_Amount_bxg_3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_bxg_3`[is.infinite(tabla6$`QPC_Original_Amount_bxg_3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_bxg_t` <- tabla6$`QPC_Original_Amount` / tabla6$`T_BXG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_bxg_t`[is.finite(tabla6$`QPC_Original_Amount_bxg_t`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_bxg_t`[is.infinite(tabla6$`QPC_Original_Amount_bxg_t`)] <- mediana_valor

tabla6$`QPC_Original_Amount_bxg+1` <- tabla6$`QPC_Original_Amount` / tabla6$`T+1_BXG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_bxg+1`[is.finite(tabla6$`QPC_Original_Amount_bxg+1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_bxg+1`[is.infinite(tabla6$`QPC_Original_Amount_bxg+1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_bxg+2` <- tabla6$`QPC_Original_Amount` / tabla6$`T+2_BXG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_bxg+2`[is.finite(tabla6$`QPC_Original_Amount_bxg+2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_bxg+2`[is.infinite(tabla6$`QPC_Original_Amount_bxg+2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_bxg+3` <- tabla6$`QPC_Original_Amount` / tabla6$`T+3_BXG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_bxg+3`[is.finite(tabla6$`QPC_Original_Amount_bxg+3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_bxg+3`[is.infinite(tabla6$`QPC_Original_Amount_bxg+3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_bxg+4` <- tabla6$`QPC_Original_Amount` / tabla6$`T+4_BXG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_bxg+4`[is.finite(tabla6$`QPC_Original_Amount_bxg+4`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_bxg+4`[is.infinite(tabla6$`QPC_Original_Amount_bxg+4`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA_1` <- tabla6$`QPC_Original_Amount` / tabla6$`T-1_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA_1`[is.finite(tabla6$`QPC_Original_Amount_ENDA_1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA_1`[is.infinite(tabla6$`QPC_Original_Amount_ENDA_1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA_2` <- tabla6$`QPC_Original_Amount` / tabla6$`T-2_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA_2`[is.finite(tabla6$`QPC_Original_Amount_ENDA_2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA_2`[is.infinite(tabla6$`QPC_Original_Amount_ENDA_2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA_3` <- tabla6$`QPC_Original_Amount` / tabla6$`T-3_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA_3`[is.finite(tabla6$`QPC_Original_Amount_ENDA_3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA_3`[is.infinite(tabla6$`QPC_Original_Amount_ENDA_3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA_t` <- tabla6$`QPC_Original_Amount` / tabla6$`T_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA_t`[is.finite(tabla6$`QPC_Original_Amount_ENDA_t`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA_t`[is.infinite(tabla6$`QPC_Original_Amount_ENDA_t`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA+1` <- tabla6$`QPC_Original_Amount` / tabla6$`T+1_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA+1`[is.finite(tabla6$`QPC_Original_Amount_ENDA+1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA+1`[is.infinite(tabla6$`QPC_Original_Amount_ENDA+1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA+2` <- tabla6$`QPC_Original_Amount` / tabla6$`T+2_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA+2`[is.finite(tabla6$`QPC_Original_Amount_ENDA+2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA+2`[is.infinite(tabla6$`QPC_Original_Amount_ENDA+2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA+3` <- tabla6$`QPC_Original_Amount` / tabla6$`T+3_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA+3`[is.finite(tabla6$`QPC_Original_Amount_ENDA+3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA+3`[is.infinite(tabla6$`QPC_Original_Amount_ENDA+3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA+4` <- tabla6$`QPC_Original_Amount` / tabla6$`T+4_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA+4`[is.finite(tabla6$`QPC_Original_Amount_ENDA+4`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA+4`[is.infinite(tabla6$`QPC_Original_Amount_ENDA+4`)] <- mediana_valor

tabla6$`QPC_Original_Amount_PCPI_1` <- tabla6$`QPC_Original_Amount` / tabla6$`T-1_PCPI`
mediana_valor <- median(tabla6$`QPC_Original_Amount_PCPI_1`[is.finite(tabla6$`QPC_Original_Amount_PCPI_1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_PCPI_1`[is.infinite(tabla6$`QPC_Original_Amount_PCPI_1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_PCPI_2` <- tabla6$`QPC_Original_Amount` / tabla6$`T-2_PCPI`
mediana_valor <- median(tabla6$`QPC_Original_Amount_PCPI_2`[is.finite(tabla6$`QPC_Original_Amount_PCPI_2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_PCPI_2`[is.infinite(tabla6$`QPC_Original_Amount_PCPI_2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_PCPI_3` <- tabla6$`QPC_Original_Amount` / tabla6$`T-3_PCPI`
mediana_valor <- median(tabla6$`QPC_Original_Amount_PCPI_3`[is.finite(tabla6$`QPC_Original_Amount_PCPI_3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_PCPI_3`[is.infinite(tabla6$`QPC_Original_Amount_PCPI_3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_PCPI_t` <- tabla6$`QPC_Original_Amount` / tabla6$`T_PCPI`
mediana_valor <- median(tabla6$`QPC_Original_Amount_PCPI_t`[is.finite(tabla6$`QPC_Original_Amount_PCPI_t`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_PCPI_t`[is.infinite(tabla6$`QPC_Original_Amount_PCPI_t`)] <- mediana_valor

tabla6$`QPC_Original_Amount_PCPI+1` <- tabla6$`QPC_Original_Amount` / tabla6$`T+1_PCPI`
mediana_valor <- median(tabla6$`QPC_Original_Amount_PCPI+1`[is.finite(tabla6$`QPC_Original_Amount_PCPI+1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_PCPI+1`[is.infinite(tabla6$`QPC_Original_Amount_PCPI+1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_PCPI+2` <- tabla6$`QPC_Original_Amount` / tabla6$`T+2_PCPI`
mediana_valor <- median(tabla6$`QPC_Original_Amount_PCPI+2`[is.finite(tabla6$`QPC_Original_Amount_PCPI+2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_PCPI+2`[is.infinite(tabla6$`QPC_Original_Amount_PCPI+2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_PCPI+3` <- tabla6$`QPC_Original_Amount` / tabla6$`T+3_PCPI`
mediana_valor <- median(tabla6$`QPC_Original_Amount_PCPI+3`[is.finite(tabla6$`QPC_Original_Amount_PCPI+3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_PCPI+3`[is.infinite(tabla6$`QPC_Original_Amount_PCPI+3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_PCPI+4` <- tabla6$`QPC_Original_Amount` / tabla6$`T+4_PCPI`
mediana_valor <- median(tabla6$`QPC_Original_Amount_PCPI+4`[is.finite(tabla6$`QPC_Original_Amount_PCPI+4`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_PCPI+4`[is.infinite(tabla6$`QPC_Original_Amount_PCPI+4`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BK_1` <- tabla6$`QPC_Original_Amount` / tabla6$`T-1_BK`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BK_1`[is.finite(tabla6$`QPC_Original_Amount_BK_1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BK_1`[is.infinite(tabla6$`QPC_Original_Amount_BK_1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BK_2` <- tabla6$`QPC_Original_Amount` / tabla6$`T-2_BK`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BK_2`[is.finite(tabla6$`QPC_Original_Amount_BK_2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BK_2`[is.infinite(tabla6$`QPC_Original_Amount_BK_2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BK_3` <- tabla6$`QPC_Original_Amount` / tabla6$`T-3_BK`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BK_3`[is.finite(tabla6$`QPC_Original_Amount_BK_3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BK_3`[is.infinite(tabla6$`QPC_Original_Amount_BK_3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BK_t` <- tabla6$`QPC_Original_Amount` / tabla6$`T_BK`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BK_t`[is.finite(tabla6$`QPC_Original_Amount_BK_t`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BK_t`[is.infinite(tabla6$`QPC_Original_Amount_BK_t`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BK+1` <- tabla6$`QPC_Original_Amount` / tabla6$`T+1_BK`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BK+1`[is.finite(tabla6$`QPC_Original_Amount_BK+1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BK+1`[is.infinite(tabla6$`QPC_Original_Amount_BK+1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BK+2` <- tabla6$`QPC_Original_Amount` / tabla6$`T+2_BK`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BK+2`[is.finite(tabla6$`QPC_Original_Amount_BK+2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BK+2`[is.infinite(tabla6$`QPC_Original_Amount_BK+2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BK+3` <- tabla6$`QPC_Original_Amount` / tabla6$`T+3_BK`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BK+3`[is.finite(tabla6$`QPC_Original_Amount_BK+3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BK+3`[is.infinite(tabla6$`QPC_Original_Amount_BK+3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BK+4` <- tabla6$`QPC_Original_Amount` / tabla6$`T+4_BK`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BK+4`[is.finite(tabla6$`QPC_Original_Amount_BK+4`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BK+4`[is.infinite(tabla6$`QPC_Original_Amount_BK+4`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BCA_1` <- tabla6$`QPC_Original_Amount` / tabla6$`T-1_BCA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BCA_1`[is.finite(tabla6$`QPC_Original_Amount_BCA_1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BCA_1`[is.infinite(tabla6$`QPC_Original_Amount_BCA_1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BCA_2` <- tabla6$`QPC_Original_Amount` / tabla6$`T-2_BCA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BCA_2`[is.finite(tabla6$`QPC_Original_Amount_BCA_2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BCA_2`[is.infinite(tabla6$`QPC_Original_Amount_BCA_2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BCA_3` <- tabla6$`QPC_Original_Amount` / tabla6$`T-3_BCA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BCA_3`[is.finite(tabla6$`QPC_Original_Amount_BCA_3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BCA_3`[is.infinite(tabla6$`QPC_Original_Amount_BCA_3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BCA_t` <- tabla6$`QPC_Original_Amount` / tabla6$`T_BCA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BCA_t`[is.finite(tabla6$`QPC_Original_Amount_BCA_t`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BCA_t`[is.infinite(tabla6$`QPC_Original_Amount_BCA_t`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BCA+1` <- tabla6$`QPC_Original_Amount` / tabla6$`T+1_BCA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BCA+1`[is.finite(tabla6$`QPC_Original_Amount_BCA+1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BCA+1`[is.infinite(tabla6$`QPC_Original_Amount_BCA+1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BCA+2` <- tabla6$`QPC_Original_Amount` / tabla6$`T+2_BCA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BCA+2`[is.finite(tabla6$`QPC_Original_Amount_BCA+2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BCA+2`[is.infinite(tabla6$`QPC_Original_Amount_BCA+2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BCA+3` <- tabla6$`QPC_Original_Amount` / tabla6$`T+3_BCA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BCA+3`[is.finite(tabla6$`QPC_Original_Amount_BCA+3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BCA+3`[is.infinite(tabla6$`QPC_Original_Amount_BCA+3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_BCA+4` <- tabla6$`QPC_Original_Amount` / tabla6$`T+4_BCA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_BCA+4`[is.finite(tabla6$`QPC_Original_Amount_BCA+4`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_BCA+4`[is.infinite(tabla6$`QPC_Original_Amount_BCA+4`)] <- mediana_valor

tabla6$`QPC_Original_Amount_D_1` <- tabla6$`QPC_Original_Amount` / tabla6$`T-1_D`
mediana_valor <- median(tabla6$`QPC_Original_Amount_D_1`[is.finite(tabla6$`QPC_Original_Amount_D_1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_D_1`[is.infinite(tabla6$`QPC_Original_Amount_D_1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_D_2` <- tabla6$`QPC_Original_Amount` / tabla6$`T-2_D`
mediana_valor <- median(tabla6$`QPC_Original_Amount_D_2`[is.finite(tabla6$`QPC_Original_Amount_D_2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_D_2`[is.infinite(tabla6$`QPC_Original_Amount_D_2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_D_3` <- tabla6$`QPC_Original_Amount` / tabla6$`T-3_D`
mediana_valor <- median(tabla6$`QPC_Original_Amount_D_3`[is.finite(tabla6$`QPC_Original_Amount_D_3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_D_3`[is.infinite(tabla6$`QPC_Original_Amount_D_3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_D_t` <- tabla6$`QPC_Original_Amount` / tabla6$`T_D`
mediana_valor <- median(tabla6$`QPC_Original_Amount_D_t`[is.finite(tabla6$`QPC_Original_Amount_D_t`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_D_t`[is.infinite(tabla6$`QPC_Original_Amount_D_t`)] <- mediana_valor

tabla6$`QPC_Original_Amount_D+1` <- tabla6$`QPC_Original_Amount` / tabla6$`T+1_D`
mediana_valor <- median(tabla6$`QPC_Original_Amount_D+1`[is.finite(tabla6$`QPC_Original_Amount_D+1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_D+1`[is.infinite(tabla6$`QPC_Original_Amount_D+1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_D+2` <- tabla6$`QPC_Original_Amount` / tabla6$`T+2_D`
mediana_valor <- median(tabla6$`QPC_Original_Amount_D+2`[is.finite(tabla6$`QPC_Original_Amount_D+2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_D+2`[is.infinite(tabla6$`QPC_Original_Amount_D+2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_D+3` <- tabla6$`QPC_Original_Amount` / tabla6$`T+3_D`
mediana_valor <- median(tabla6$`QPC_Original_Amount_D+3`[is.finite(tabla6$`QPC_Original_Amount_D+3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_D+3`[is.infinite(tabla6$`QPC_Original_Amount_D+3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_D+4` <- tabla6$`QPC_Original_Amount` / tabla6$`T+4_D`
mediana_valor <- median(tabla6$`QPC_Original_Amount_D+4`[is.finite(tabla6$`QPC_Original_Amount_D+4`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_D+4`[is.infinite(tabla6$`QPC_Original_Amount_D+4`)] <- mediana_valor

tabla6$`QPC_Original_Amount_NCG_1` <- tabla6$`QPC_Original_Amount` / tabla6$`T-1_NCG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_NCG_1`[is.finite(tabla6$`QPC_Original_Amount_NCG_1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_NCG_1`[is.infinite(tabla6$`QPC_Original_Amount_NCG_1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_NCG_2` <- tabla6$`QPC_Original_Amount` / tabla6$`T-2_NCG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_NCG_2`[is.finite(tabla6$`QPC_Original_Amount_NCG_2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_NCG_2`[is.infinite(tabla6$`QPC_Original_Amount_NCG_2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_NCG_t` <- tabla6$`QPC_Original_Amount` / tabla6$`T_NCG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_NCG_t`[is.finite(tabla6$`QPC_Original_Amount_NCG_t`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_NCG_t`[is.infinite(tabla6$`QPC_Original_Amount_NCG_t`)] <- mediana_valor

tabla6$`QPC_Original_Amount_NCG+1` <- tabla6$`QPC_Original_Amount` / tabla6$`T+1_NCG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_NCG+1`[is.finite(tabla6$`QPC_Original_Amount_NCG+1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_NCG+1`[is.infinite(tabla6$`QPC_Original_Amount_NCG+1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_NCG+2` <- tabla6$`QPC_Original_Amount` / tabla6$`T+2_NCG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_NCG+2`[is.finite(tabla6$`QPC_Original_Amount_NCG+2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_NCG+2`[is.infinite(tabla6$`QPC_Original_Amount_NCG+2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_NCG+3` <- tabla6$`QPC_Original_Amount` / tabla6$`T+3_NCG`
mediana_valor <- median(tabla6$`QPC_Original_Amount_NCG+3`[is.finite(tabla6$`QPC_Original_Amount_NCG+3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_NCG+3`[is.infinite(tabla6$`QPC_Original_Amount_NCG+3`)] <- mediana_valor


##############################################################################

tabla6$`Totalaccess_bxg` <- tabla6$`Totalaccess` / tabla6$`T-1_BXG`
mediana_valor <- median(tabla6$`Totalaccess_bxg`[is.finite(tabla6$`Totalaccess_bxg`)], na.rm = TRUE)
tabla6$`Totalaccess_bxg`[is.infinite(tabla6$`Totalaccess_bxg`)] <- mediana_valor

tabla6$Program_span <- tabla6$Initial.End.Date - tabla6$Approval.Date

library(lubridate)

tabla6$QPC_Test_Date_ <- dmy(tabla6$QPC_Test_Date)
tabla6$Approval.Date_  <- as.Date(tabla6$Approval.Date, origin = "1899-12-30")

tabla6$QPC_span <- tabla6$QPC_Test_Date_ - tabla6$Approval.Date_
tabla6$QPC_date_span <- as.numeric(tabla6$QPC_span)

tabla6 <- tabla6[, -which(names(tabla6) == "QPC_Test_Date_")]
tabla6 <- tabla6[, -which(names(tabla6) == "Approval.Date_")]
tabla6 <- tabla6[, -which(names(tabla6) == "QPC_span")]



############################################################################################

#si review type len es mayor a 3 poner un 1 si no poner cero
tabla6$Joint_revision <- as.integer(nchar(tabla6$Review.Type) > 3)


############################################################################################

library(dplyr)
tabla6 <- tabla6 %>% distinct()

# Buscamos NAs

colSums(is.na(tabla6))

sapply(tabla6, function(x) sum(is.infinite(x)))

# Imputamos la mediana

library(dplyr)

tabla6 <- tabla6 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))


###

library(dplyr)
library(purrr)

# Función mejorada para reemplazar valores problemáticos
limpiar_columna <- function(x) {
  # Manejar columnas numéricas
  if(is.numeric(x)) {
    x[x == "" | is.infinite(x)] <- NA
    med <- median(x, na.rm = TRUE)
    x[is.na(x)] <- med
    return(x)
  }
  
  # Manejar columnas de caracteres/factores
  if(is.character(x) | is.factor(x)) {
    x[x == ""] <- NA
    moda <- names(which.max(table(x)))
    x[is.na(x)] <- moda
    return(x)
  }
  
  # Manejar columnas lógicas
  if(is.logical(x)) {
    x[is.na(x)] <- FALSE
    return(x)
  }
  
  return(x)
}

# Aplicar a todo el dataframe
tabla6 <- tabla6 %>%
  mutate(across(everything(), limpiar_columna))

# Verificación final
colSums(is.na(tabla6))

sapply(tabla6, function(x) sum(is.infinite(x)))

tabla6 <- tabla6 %>% distinct()

colnames(tabla6)

###########################################################################################
# Eliminar columnas redundantes

colnames(tabla6)

tabla6 <- tabla6 %>% select(-Country.Name)
#tabla6 <- tabla6 %>% select(-Arrangement_type)
tabla6 <- tabla6 %>% select(-Approval.Date)
tabla6 <- tabla6 %>% select(-Approval.Year)
tabla6 <- tabla6 %>% select(-Initial.End.Date)
tabla6 <- tabla6 %>% select(-Initial.End.Year)
tabla6 <- tabla6 %>% select(-Review.Type)
tabla6 <- tabla6 %>% select(-Review.Sequence)
tabla6 <- tabla6 %>% select(-Main)
tabla6 <- tabla6 %>% select(-QPC_Units)
#tabla6 <- tabla6 %>% select(-Purchase_Units)
tabla6 <- tabla6 %>% select(-Main.Criteria.Sequence)
#tabla6 <- tabla6 %>% select(-Sub.Option)
tabla6 <- tabla6 %>% select(-Program_span)

tabla6 <- tabla6 %>%
  relocate(Joint_revision, .before = 6)

tabla6 <- tabla6 %>%
  relocate(QPC_date_span, .before = 5)

tabla6 <- tabla6 %>%
  relocate(QPC_Test_Date, .before = 5)

tabla6 <- tabla6 %>%
  relocate(Totalaccess_bxg, .before = 11)

#tabla6 <- tabla6 %>%
# relocate(Totalaccess, .before = 11)

tabla6  <- tabla6 %>% distinct()

library(lubridate)
tabla6$QPC_Test_Date <- dmy(tabla6$QPC_Test_Date)

###########################################################################################

glimpse(tabla6)
colnames(tabla6)

tabla6$Adjusted.Amount <- as.numeric(tabla6$Adjusted.Amount)
tabla6$Actual.Amount <- as.numeric(tabla6$Actual.Amount)

tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "USD", tabla6$QPC_Original_Amount / tabla6$`T-1_BXG`, tabla6$QPC_Original_Amount)
tabla6$QPC_Adjusted_Amount_real <- ifelse(tabla6$QPC_Units_ == "USD", tabla6$Adjusted.Amount / tabla6$`T-1_BXG`, tabla6$Adjusted.Amount)
tabla6$QPC_Actual_Amount_real <- ifelse(tabla6$QPC_Units_ == "USD", tabla6$Actual.Amount / tabla6$`T-1_BXG`, tabla6$Actual.Amount)

tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "NC_", tabla6$QPC_Original_Amount / tabla6$`T-1_NGDP`, tabla6$QPC_Original_Amount)
tabla6$QPC_Adjusted_Amount_real <- ifelse(tabla6$QPC_Units_ == "NC_", tabla6$Adjusted.Amount / tabla6$`T-1_NGDP`, tabla6$Adjusted.Amount)
tabla6$QPC_Actual_Amount_real <- ifelse(tabla6$QPC_Units_ == "NC_", tabla6$Actual.Amount / tabla6$`T-1_NGDP`, tabla6$Actual.Amount)

tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "SDR", tabla6$QPC_Original_Amount / tabla6$`T-1_BXG`, tabla6$QPC_Original_Amount)
tabla6$QPC_Adjusted_Amount_real <- ifelse(tabla6$QPC_Units_ == "SDR", tabla6$Adjusted.Amount / tabla6$`T-1_BXG`, tabla6$Adjusted.Amount)
tabla6$QPC_Actual_Amount_real <- ifelse(tabla6$QPC_Units_ == "SDR", tabla6$Actual.Amount / tabla6$`T-1_BXG`, tabla6$Actual.Amount)

tabla6$QPC_Amount_span <- tabla6$QPC_Actual_Amount_real - tabla6$QPC_Original_Amount_real

#############################################

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_ENDA_1 = Sub.Option_NIR_floor * Purchase_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_ENDA_2 = Sub.Option_NIR_floor * Purchase_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_ENDA_3 = Sub.Option_NIR_floor * Purchase_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_ENDA_t = Sub.Option_NIR_floor * Purchase_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_ENDA1 = Sub.Option_NIR_floor * `Purchase_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_ENDA2 = Sub.Option_NIR_floor * `Purchase_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_ENDA3 = Sub.Option_NIR_floor * `Purchase_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_ENDA4 = Sub.Option_NIR_floor * `Purchase_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_bxg_1 = Sub.Option_NIR_floor * Purchase_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_bxg_2 = Sub.Option_NIR_floor * Purchase_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_bxg_3 = Sub.Option_NIR_floor * Purchase_Original_Amount_bxg_3)
#tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_bxg_4 = Sub.Option_NIR_floor * Purchase_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_bxg_t = Sub.Option_NIR_floor * Purchase_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_bxg1 = Sub.Option_NIR_floor * `Purchase_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_bxg2 = Sub.Option_NIR_floor * `Purchase_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_bxg4 = Sub.Option_NIR_floor * `Purchase_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_BCA_1 = Sub.Option_NIR_floor * Purchase_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_BCA_2 = Sub.Option_NIR_floor * Purchase_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_BCA_3 = Sub.Option_NIR_floor * Purchase_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_BCA_t = Sub.Option_NIR_floor * Purchase_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_BCA1 = Sub.Option_NIR_floor * `Purchase_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_BCA2 = Sub.Option_NIR_floor * `Purchase_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_BCA3 = Sub.Option_NIR_floor * `Purchase_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_BCA4 = Sub.Option_NIR_floor * `Purchase_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_D_1 = Sub.Option_NIR_floor * Purchase_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_D_2 = Sub.Option_NIR_floor * Purchase_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_D_3 = Sub.Option_NIR_floor * Purchase_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_D_t = Sub.Option_NIR_floor * Purchase_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_D1 = Sub.Option_NIR_floor * `Purchase_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_D2 = Sub.Option_NIR_floor * `Purchase_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_D3 = Sub.Option_NIR_floor * `Purchase_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_Purchase_Original_Amount_D4 = Sub.Option_NIR_floor * `Purchase_Original_Amount_D+4`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_ENDA_1 = Sub.Option_FX_ctrol * Purchase_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_ENDA_2 = Sub.Option_FX_ctrol * Purchase_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_ENDA_3 = Sub.Option_FX_ctrol * Purchase_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_ENDA_t = Sub.Option_FX_ctrol * Purchase_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_ENDA1 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_ENDA2 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_ENDA3 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_ENDA4 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_bxg_1 = Sub.Option_FX_ctrol * Purchase_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_bxg_2 = Sub.Option_FX_ctrol * Purchase_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_bxg_3 = Sub.Option_FX_ctrol * Purchase_Original_Amount_bxg_3)
#tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_bxg_4 = Sub.Option_FX_ctrol * Purchase_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_bxg_t = Sub.Option_FX_ctrol * Purchase_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_bxg1 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_bxg2 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_bxg4 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_BCA_1 = Sub.Option_FX_ctrol * Purchase_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_BCA_2 = Sub.Option_FX_ctrol * Purchase_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_BCA_3 = Sub.Option_FX_ctrol * Purchase_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_BCA_t = Sub.Option_FX_ctrol * Purchase_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_BCA1 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_BCA2 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_BCA3 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_BCA4 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_D_1 = Sub.Option_FX_ctrol * Purchase_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_D_2 = Sub.Option_FX_ctrol * Purchase_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_D_3 = Sub.Option_FX_ctrol * Purchase_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_D_t = Sub.Option_FX_ctrol * Purchase_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_D1 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_D2 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_D3 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_Purchase_Original_Amount_D4 = Sub.Option_FX_ctrol * `Purchase_Original_Amount_D+4`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_ENDA_1 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_ENDA_2 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_ENDA_3 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_ENDA_t = Sub.Option_External_arrears_accum * Purchase_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_ENDA1 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_ENDA2 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_ENDA3 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_ENDA4 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_bxg_1 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_bxg_2 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_bxg_3 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_bxg_3)
#tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_bxg_4 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_bxg_t = Sub.Option_External_arrears_accum * Purchase_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_bxg1 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_bxg2 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_bxg4 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_BCA_1 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_BCA_2 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_BCA_3 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_BCA_t = Sub.Option_External_arrears_accum * Purchase_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_BCA1 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_BCA2 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_BCA3 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_BCA4 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_D_1 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_D_2 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_D_3 = Sub.Option_External_arrears_accum * Purchase_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_D_t = Sub.Option_External_arrears_accum * Purchase_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_D1 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_D2 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_D3 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_Purchase_Original_Amount_D4 = Sub.Option_External_arrears_accum * `Purchase_Original_Amount_D+4`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_ENDA_1 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_ENDA_2 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_ENDA_3 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_ENDA_t = Sub.Option_External_arrears_stock * Purchase_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_ENDA1 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_ENDA2 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_ENDA3 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_ENDA4 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_bxg_1 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_bxg_2 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_bxg_3 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_bxg_3)
#tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_bxg_4 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_bxg_t = Sub.Option_External_arrears_stock * Purchase_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_bxg1 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_bxg2 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_bxg4 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_BCA_1 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_BCA_2 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_BCA_3 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_BCA_t = Sub.Option_External_arrears_stock * Purchase_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_BCA1 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_BCA2 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_BCA3 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_BCA4 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_D_1 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_D_2 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_D_3 = Sub.Option_External_arrears_stock * Purchase_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_D_t = Sub.Option_External_arrears_stock * Purchase_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_D1 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_D2 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_D3 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_Purchase_Original_Amount_D4 = Sub.Option_External_arrears_stock * `Purchase_Original_Amount_D+4`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_ENDA_1 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_ENDA_2 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_ENDA_3 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_ENDA_t = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_ENDA1 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_ENDA2 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_ENDA3 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_ENDA4 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_bxg_1 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_bxg_2 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_bxg_3 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_bxg_3)
#tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_bxg_4 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_bxg_t = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_bxg1 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_bxg2 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_bxg4 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_BCA_1 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_BCA_2 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_BCA_3 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_BCA_t = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_BCA1 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_BCA2 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_BCA3 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_BCA4 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_D_1 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_D_2 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_D_3 = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_D_t = Sub.Option_External_debt_Contracted_Guaranteed * Purchase_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_D1 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_D2 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_D3 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_Purchase_Original_Amount_D4 = Sub.Option_External_debt_Contracted_Guaranteed * `Purchase_Original_Amount_D+4`)

##

tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_ENDA_1 = Sub.Option_Primary_deficit * Purchase_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_ENDA_2 = Sub.Option_Primary_deficit * Purchase_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_ENDA_3 = Sub.Option_Primary_deficit * Purchase_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_ENDA_t = Sub.Option_Primary_deficit * Purchase_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_ENDA1 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_ENDA2 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_ENDA3 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_ENDA4 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_bxg_1 = Sub.Option_Primary_deficit * Purchase_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_bxg_2 = Sub.Option_Primary_deficit * Purchase_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_bxg_3 = Sub.Option_Primary_deficit * Purchase_Original_Amount_bxg_3)
#tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_bxg_4 = Sub.Option_Primary_deficit * Purchase_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_bxg_t = Sub.Option_Primary_deficit * Purchase_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_bxg1 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_bxg2 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_bxg4 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_BCA_1 = Sub.Option_Primary_deficit * Purchase_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_BCA_2 = Sub.Option_Primary_deficit * Purchase_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_BCA_3 = Sub.Option_Primary_deficit * Purchase_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_BCA_t = Sub.Option_Primary_deficit * Purchase_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_BCA1 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_BCA2 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_BCA3 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_BCA4 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_D_1 = Sub.Option_Primary_deficit * Purchase_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_D_2 = Sub.Option_Primary_deficit * Purchase_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_D_3 = Sub.Option_Primary_deficit * Purchase_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_D_t = Sub.Option_Primary_deficit * Purchase_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_D1 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_D2 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_D3 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_Purchase_Original_Amount_D4 = Sub.Option_Primary_deficit * `Purchase_Original_Amount_D+4`)

##


tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_ENDA_1 = Sub.Option_Overall_deficit * Purchase_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_ENDA_2 = Sub.Option_Overall_deficit * Purchase_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_ENDA_3 = Sub.Option_Overall_deficit * Purchase_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_ENDA_t = Sub.Option_Overall_deficit * Purchase_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_ENDA1 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_ENDA2 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_ENDA3 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_ENDA4 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_bxg_1 = Sub.Option_Overall_deficit * Purchase_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_bxg_2 = Sub.Option_Overall_deficit * Purchase_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_bxg_3 = Sub.Option_Overall_deficit * Purchase_Original_Amount_bxg_3)
#tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_bxg_4 = Sub.Option_Overall_deficit * Purchase_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_bxg_t = Sub.Option_Overall_deficit * Purchase_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_bxg1 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_bxg2 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_bxg4 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_BCA_1 = Sub.Option_Overall_deficit * Purchase_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_BCA_2 = Sub.Option_Overall_deficit * Purchase_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_BCA_3 = Sub.Option_Overall_deficit * Purchase_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_BCA_t = Sub.Option_Overall_deficit * Purchase_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_BCA1 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_BCA2 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_BCA3 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_BCA4 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_D_1 = Sub.Option_Overall_deficit * Purchase_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_D_2 = Sub.Option_Overall_deficit * Purchase_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_D_3 = Sub.Option_Overall_deficit * Purchase_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_D_t = Sub.Option_Overall_deficit * Purchase_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_D1 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_D2 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_D3 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_Purchase_Original_Amount_D4 = Sub.Option_Overall_deficit * `Purchase_Original_Amount_D+4`)


##


tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_ENDA_1 = Sub.Option_Revenue * Purchase_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_ENDA_2 = Sub.Option_Revenue * Purchase_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_ENDA_3 = Sub.Option_Revenue * Purchase_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_ENDA_t = Sub.Option_Revenue * Purchase_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_ENDA1 = Sub.Option_Revenue * `Purchase_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_ENDA2 = Sub.Option_Revenue * `Purchase_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_ENDA3 = Sub.Option_Revenue * `Purchase_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_ENDA4 = Sub.Option_Revenue * `Purchase_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_bxg_1 = Sub.Option_Revenue * Purchase_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_bxg_2 = Sub.Option_Revenue * Purchase_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_bxg_3 = Sub.Option_Revenue * Purchase_Original_Amount_bxg_3)
#tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_bxg_4 = Sub.Option_Revenue * Purchase_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_bxg_t = Sub.Option_Revenue * Purchase_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_bxg1 = Sub.Option_Revenue * `Purchase_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_bxg2 = Sub.Option_Revenue * `Purchase_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_bxg4 = Sub.Option_Revenue * `Purchase_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_BCA_1 = Sub.Option_Revenue * Purchase_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_BCA_2 = Sub.Option_Revenue * Purchase_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_BCA_3 = Sub.Option_Revenue * Purchase_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_BCA_t = Sub.Option_Revenue * Purchase_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_BCA1 = Sub.Option_Revenue * `Purchase_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_BCA2 = Sub.Option_Revenue * `Purchase_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_BCA3 = Sub.Option_Revenue * `Purchase_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_BCA4 = Sub.Option_Revenue * `Purchase_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_D_1 = Sub.Option_Revenue * Purchase_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_D_2 = Sub.Option_Revenue * Purchase_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_D_3 = Sub.Option_Revenue * Purchase_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_D_t = Sub.Option_Revenue * Purchase_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_D1 = Sub.Option_Revenue * `Purchase_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_D2 = Sub.Option_Revenue * `Purchase_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_D3 = Sub.Option_Revenue * `Purchase_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_Purchase_Original_Amount_D4 = Sub.Option_Revenue * `Purchase_Original_Amount_D+4`)


##


tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_ENDA_1 = Sub.Option_Spending * Purchase_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_ENDA_2 = Sub.Option_Spending * Purchase_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_ENDA_3 = Sub.Option_Spending * Purchase_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_ENDA_t = Sub.Option_Spending * Purchase_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_ENDA1 = Sub.Option_Spending * `Purchase_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_ENDA2 = Sub.Option_Spending * `Purchase_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_ENDA3 = Sub.Option_Spending * `Purchase_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_ENDA4 = Sub.Option_Spending * `Purchase_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_bxg_1 = Sub.Option_Spending * Purchase_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_bxg_2 = Sub.Option_Spending * Purchase_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_bxg_3 = Sub.Option_Spending * Purchase_Original_Amount_bxg_3)
#tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_bxg_4 = Sub.Option_Spending * Purchase_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_bxg_t = Sub.Option_Spending * Purchase_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_bxg1 = Sub.Option_Spending * `Purchase_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_bxg2 = Sub.Option_Spending * `Purchase_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_bxg4 = Sub.Option_Spending * `Purchase_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_BCA_1 = Sub.Option_Spending * Purchase_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_BCA_2 = Sub.Option_Spending * Purchase_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_BCA_3 = Sub.Option_Spending * Purchase_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_BCA_t = Sub.Option_Spending * Purchase_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_BCA1 = Sub.Option_Spending * `Purchase_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_BCA2 = Sub.Option_Spending * `Purchase_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_BCA3 = Sub.Option_Spending * `Purchase_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_BCA4 = Sub.Option_Spending * `Purchase_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_D_1 = Sub.Option_Spending * Purchase_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_D_2 = Sub.Option_Spending * Purchase_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_D_3 = Sub.Option_Spending * Purchase_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_D_t = Sub.Option_Spending * Purchase_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_D1 = Sub.Option_Spending * `Purchase_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_D2 = Sub.Option_Spending * `Purchase_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_D3 = Sub.Option_Spending * `Purchase_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_Purchase_Original_Amount_D4 = Sub.Option_Spending * `Purchase_Original_Amount_D+4`)



#############################################

colnames(tabla6)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_ENDA_1 = Sub.Option_NIR_floor * QPC_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_ENDA_2 = Sub.Option_NIR_floor * QPC_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_ENDA_3 = Sub.Option_NIR_floor * QPC_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_ENDA_t = Sub.Option_NIR_floor * QPC_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_ENDA1 = Sub.Option_NIR_floor * `QPC_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_ENDA2 = Sub.Option_NIR_floor * `QPC_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_ENDA3 = Sub.Option_NIR_floor * `QPC_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_ENDA4 = Sub.Option_NIR_floor * `QPC_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_bxg_1 = Sub.Option_NIR_floor * QPC_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_bxg_2 = Sub.Option_NIR_floor * QPC_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_bxg_3 = Sub.Option_NIR_floor * QPC_Original_Amount_bxg_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_bxg_4 = Sub.Option_NIR_floor * QPC_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_bxg_t = Sub.Option_NIR_floor * QPC_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_bxg1 = Sub.Option_NIR_floor * `QPC_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_bxg2 = Sub.Option_NIR_floor * `QPC_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_bxg4 = Sub.Option_NIR_floor * `QPC_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_BCA_1 = Sub.Option_NIR_floor * QPC_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_BCA_2 = Sub.Option_NIR_floor * QPC_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_BCA_3 = Sub.Option_NIR_floor * QPC_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_BCA_t = Sub.Option_NIR_floor * QPC_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_BCA1 = Sub.Option_NIR_floor * `QPC_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_BCA2 = Sub.Option_NIR_floor * `QPC_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_BCA3 = Sub.Option_NIR_floor * `QPC_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_BCA4 = Sub.Option_NIR_floor * `QPC_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_D_1 = Sub.Option_NIR_floor * QPC_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_D_2 = Sub.Option_NIR_floor * QPC_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_D_3 = Sub.Option_NIR_floor * QPC_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_D_t = Sub.Option_NIR_floor * QPC_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_D1 = Sub.Option_NIR_floor * `QPC_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_D2 = Sub.Option_NIR_floor * `QPC_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_D3 = Sub.Option_NIR_floor * `QPC_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_QPC_Original_Amount_D4 = Sub.Option_NIR_floor * `QPC_Original_Amount_D+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T_ENDA_pcpi = Sub.Option_NIR_floor * T_ENDA_pcpi)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T1_ENDA_pcpi = Sub.Option_NIR_floor * `T+1_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T2_ENDA_pcpi = Sub.Option_NIR_floor * `T+2_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T3_ENDA_pcpi = Sub.Option_NIR_floor * `T+3_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T4_ENDA_pcpi = Sub.Option_NIR_floor * `T+4_ENDA_pcpi`)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T_D_bxg = Sub.Option_NIR_floor * T_D_bxg)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T1_D_bxg = Sub.Option_NIR_floor * `T+1_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T2_D_bxg = Sub.Option_NIR_floor * `T+2_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T3_D_bxg = Sub.Option_NIR_floor * `T+3_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T4_D_bxg = Sub.Option_NIR_floor * `T+4_D_bxg`)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T_BXG_gdp_usd = Sub.Option_NIR_floor * T_BXG_gdp_usd)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T1_BXG_gdp_usd = Sub.Option_NIR_floor * `T+1_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T2_BXG_gdp_usd= Sub.Option_NIR_floor * `T+2_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T3_BXG_gdp_usd = Sub.Option_NIR_floor * `T+3_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T4_BXG_gdp_usd = Sub.Option_NIR_floor * `T+4_BXG_gdp_usd`)

tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T_3_BCA_bxg = Sub.Option_NIR_floor * `T-3_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T_2_BCA_bxg = Sub.Option_NIR_floor * `T-2_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T_1_BCA_bxg = Sub.Option_NIR_floor * `T-1_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T_BCA_bxg =  Sub.Option_NIR_floor * `T_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T1_BCA_bxg = Sub.Option_NIR_floor * `T+1_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T2_BCA_bxg = Sub.Option_NIR_floor * `T+2_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T3_BCA_bxg = Sub.Option_NIR_floor * `T+3_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_NIR_floor_T4_BCA_bxg = Sub.Option_NIR_floor * `T+4_BCA_bxg`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_ENDA_1 = Sub.Option_FX_ctrol * QPC_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_ENDA_2 = Sub.Option_FX_ctrol * QPC_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_ENDA_3 = Sub.Option_FX_ctrol * QPC_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_ENDA_t = Sub.Option_FX_ctrol * QPC_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_ENDA1 = Sub.Option_FX_ctrol * `QPC_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_ENDA2 = Sub.Option_FX_ctrol * `QPC_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_ENDA3 = Sub.Option_FX_ctrol * `QPC_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_ENDA4 = Sub.Option_FX_ctrol * `QPC_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_bxg_1 = Sub.Option_FX_ctrol * QPC_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_bxg_2 = Sub.Option_FX_ctrol * QPC_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_bxg_3 = Sub.Option_FX_ctrol * QPC_Original_Amount_bxg_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_bxg_4 = Sub.Option_FX_ctrol * QPC_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_bxg_t = Sub.Option_FX_ctrol * QPC_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_bxg1 = Sub.Option_FX_ctrol * `QPC_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_bxg2 = Sub.Option_FX_ctrol * `QPC_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_bxg4 = Sub.Option_FX_ctrol * `QPC_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_BCA_1 = Sub.Option_FX_ctrol * QPC_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_BCA_2 = Sub.Option_FX_ctrol * QPC_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_BCA_3 = Sub.Option_FX_ctrol * QPC_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_BCA_t = Sub.Option_FX_ctrol * QPC_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_BCA1 = Sub.Option_FX_ctrol * `QPC_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_BCA2 = Sub.Option_FX_ctrol * `QPC_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_BCA3 = Sub.Option_FX_ctrol * `QPC_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_BCA4 = Sub.Option_FX_ctrol * `QPC_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_D_1 = Sub.Option_FX_ctrol * QPC_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_D_2 = Sub.Option_FX_ctrol * QPC_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_D_3 = Sub.Option_FX_ctrol * QPC_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_D_t = Sub.Option_FX_ctrol * QPC_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_D1 = Sub.Option_FX_ctrol * `QPC_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_D2 = Sub.Option_FX_ctrol * `QPC_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_D3 = Sub.Option_FX_ctrol * `QPC_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_QPC_Original_Amount_D4 = Sub.Option_FX_ctrol * `QPC_Original_Amount_D+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T_ENDA_pcpi = Sub.Option_FX_ctrol * T_ENDA_pcpi)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T1_ENDA_pcpi = Sub.Option_FX_ctrol * `T+1_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T2_ENDA_pcpi = Sub.Option_FX_ctrol * `T+2_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T3_ENDA_pcpi = Sub.Option_FX_ctrol * `T+3_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T4_ENDA_pcpi = Sub.Option_FX_ctrol * `T+4_ENDA_pcpi`)

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T_D_bxg = Sub.Option_FX_ctrol * T_D_bxg)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T1_D_bxg = Sub.Option_FX_ctrol * `T+1_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T2_D_bxg = Sub.Option_FX_ctrol * `T+2_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T3_D_bxg = Sub.Option_FX_ctrol * `T+3_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T4_D_bxg = Sub.Option_FX_ctrol * `T+4_D_bxg`)

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T_BXG_gdp_usd = Sub.Option_FX_ctrol * T_BXG_gdp_usd)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T1_BXG_gdp_usd = Sub.Option_FX_ctrol * `T+1_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T2_BXG_gdp_usd= Sub.Option_FX_ctrol * `T+2_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T3_BXG_gdp_usd = Sub.Option_FX_ctrol * `T+3_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T4_BXG_gdp_usd = Sub.Option_FX_ctrol * `T+4_BXG_gdp_usd`)

tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T_3_BCA_bxg = Sub.Option_FX_ctrol * `T-3_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T_2_BCA_bxg = Sub.Option_FX_ctrol * `T-2_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T_1_BCA_bxg = Sub.Option_FX_ctrol * `T-1_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T_BCA_bxg =  Sub.Option_FX_ctrol * `T_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T1_BCA_bxg = Sub.Option_FX_ctrol * `T+1_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T2_BCA_bxg = Sub.Option_FX_ctrol * `T+2_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T3_BCA_bxg = Sub.Option_FX_ctrol * `T+3_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_FX_ctrol_T4_BCA_bxg = Sub.Option_FX_ctrol * `T+4_BCA_bxg`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_ENDA_1 = Sub.Option_External_arrears_accum * QPC_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_ENDA_2 = Sub.Option_External_arrears_accum * QPC_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_ENDA_3 = Sub.Option_External_arrears_accum * QPC_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_ENDA_t = Sub.Option_External_arrears_accum * QPC_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_ENDA1 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_ENDA2 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_ENDA3 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_ENDA4 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_bxg_1 = Sub.Option_External_arrears_accum * QPC_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_bxg_2 = Sub.Option_External_arrears_accum * QPC_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_bxg_3 = Sub.Option_External_arrears_accum * QPC_Original_Amount_bxg_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_bxg_4 = Sub.Option_External_arrears_accum * QPC_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_bxg_t = Sub.Option_External_arrears_accum * QPC_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_bxg1 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_bxg2 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_bxg4 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_BCA_1 = Sub.Option_External_arrears_accum * QPC_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_BCA_2 = Sub.Option_External_arrears_accum * QPC_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_BCA_3 = Sub.Option_External_arrears_accum * QPC_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_BCA_t = Sub.Option_External_arrears_accum * QPC_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_BCA1 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_BCA2 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_BCA3 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_BCA4 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_D_1 = Sub.Option_External_arrears_accum * QPC_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_D_2 = Sub.Option_External_arrears_accum * QPC_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_D_3 = Sub.Option_External_arrears_accum * QPC_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_D_t = Sub.Option_External_arrears_accum * QPC_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_D1 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_D2 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_D3 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_QPC_Original_Amount_D4 = Sub.Option_External_arrears_accum * `QPC_Original_Amount_D+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T_ENDA_pcpi = Sub.Option_External_arrears_accum * T_ENDA_pcpi)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T1_ENDA_pcpi = Sub.Option_External_arrears_accum * `T+1_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T2_ENDA_pcpi = Sub.Option_External_arrears_accum * `T+2_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T3_ENDA_pcpi = Sub.Option_External_arrears_accum * `T+3_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T4_ENDA_pcpi = Sub.Option_External_arrears_accum * `T+4_ENDA_pcpi`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T_D_bxg = Sub.Option_External_arrears_accum * T_D_bxg)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T1_D_bxg = Sub.Option_External_arrears_accum * `T+1_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T2_D_bxg = Sub.Option_External_arrears_accum * `T+2_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T3_D_bxg = Sub.Option_External_arrears_accum * `T+3_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T4_D_bxg = Sub.Option_External_arrears_accum * `T+4_D_bxg`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T_BXG_gdp_usd = Sub.Option_External_arrears_accum * T_BXG_gdp_usd)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T1_BXG_gdp_usd = Sub.Option_External_arrears_accum * `T+1_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T2_BXG_gdp_usd= Sub.Option_External_arrears_accum * `T+2_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T3_BXG_gdp_usd = Sub.Option_External_arrears_accum * `T+3_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T4_BXG_gdp_usd = Sub.Option_External_arrears_accum * `T+4_BXG_gdp_usd`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T_3_BCA_bxg = Sub.Option_External_arrears_accum * `T-3_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T_2_BCA_bxg = Sub.Option_External_arrears_accum * `T-2_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T_1_BCA_bxg = Sub.Option_External_arrears_accum * `T-1_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T_BCA_bxg =  Sub.Option_External_arrears_accum * `T_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T1_BCA_bxg = Sub.Option_External_arrears_accum * `T+1_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T2_BCA_bxg = Sub.Option_External_arrears_accum * `T+2_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T3_BCA_bxg = Sub.Option_External_arrears_accum * `T+3_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_accum_T4_BCA_bxg = Sub.Option_External_arrears_accum * `T+4_BCA_bxg`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_ENDA_1 = Sub.Option_External_arrears_stock * QPC_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_ENDA_2 = Sub.Option_External_arrears_stock * QPC_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_ENDA_3 = Sub.Option_External_arrears_stock * QPC_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_ENDA_t = Sub.Option_External_arrears_stock * QPC_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_ENDA1 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_ENDA2 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_ENDA3 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_ENDA4 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_bxg_1 = Sub.Option_External_arrears_stock * QPC_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_bxg_2 = Sub.Option_External_arrears_stock * QPC_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_bxg_3 = Sub.Option_External_arrears_stock * QPC_Original_Amount_bxg_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_bxg_4 = Sub.Option_External_arrears_stock * QPC_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_bxg_t = Sub.Option_External_arrears_stock * QPC_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_bxg1 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_bxg2 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_bxg4 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_BCA_1 = Sub.Option_External_arrears_stock * QPC_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_BCA_2 = Sub.Option_External_arrears_stock * QPC_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_BCA_3 = Sub.Option_External_arrears_stock * QPC_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_BCA_t = Sub.Option_External_arrears_stock * QPC_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_BCA1 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_BCA2 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_BCA3 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_BCA4 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_D_1 = Sub.Option_External_arrears_stock * QPC_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_D_2 = Sub.Option_External_arrears_stock * QPC_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_D_3 = Sub.Option_External_arrears_stock * QPC_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_D_t = Sub.Option_External_arrears_stock * QPC_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_D1 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_D2 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_D3 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_QPC_Original_Amount_D4 = Sub.Option_External_arrears_stock * `QPC_Original_Amount_D+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T_ENDA_pcpi = Sub.Option_External_arrears_stock * T_ENDA_pcpi)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T1_ENDA_pcpi = Sub.Option_External_arrears_stock * `T+1_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T2_ENDA_pcpi = Sub.Option_External_arrears_stock * `T+2_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T3_ENDA_pcpi = Sub.Option_External_arrears_stock * `T+3_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T4_ENDA_pcpi = Sub.Option_External_arrears_stock * `T+4_ENDA_pcpi`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T_D_bxg = Sub.Option_External_arrears_stock * T_D_bxg)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T1_D_bxg = Sub.Option_External_arrears_stock * `T+1_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T2_D_bxg = Sub.Option_External_arrears_stock * `T+2_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T3_D_bxg = Sub.Option_External_arrears_stock * `T+3_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T4_D_bxg = Sub.Option_External_arrears_stock * `T+4_D_bxg`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T_BXG_gdp_usd = Sub.Option_External_arrears_stock * T_BXG_gdp_usd)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T1_BXG_gdp_usd = Sub.Option_External_arrears_stock * `T+1_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T2_BXG_gdp_usd= Sub.Option_External_arrears_stock * `T+2_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T3_BXG_gdp_usd = Sub.Option_External_arrears_stock * `T+3_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T4_BXG_gdp_usd = Sub.Option_External_arrears_stock * `T+4_BXG_gdp_usd`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T_3_BCA_bxg = Sub.Option_External_arrears_stock * `T-3_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T_2_BCA_bxg = Sub.Option_External_arrears_stock * `T-2_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T_1_BCA_bxg = Sub.Option_External_arrears_stock * `T-1_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T_BCA_bxg =  Sub.Option_External_arrears_stock * `T_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T1_BCA_bxg = Sub.Option_External_arrears_stock * `T+1_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T2_BCA_bxg = Sub.Option_External_arrears_stock * `T+2_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T3_BCA_bxg = Sub.Option_External_arrears_stock * `T+3_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_arrears_stock_T4_BCA_bxg = Sub.Option_External_arrears_stock * `T+4_BCA_bxg`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_ENDA_1 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_ENDA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_ENDA_2 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_ENDA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_ENDA_3 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_ENDA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_ENDA_t = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_ENDA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_ENDA1 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_ENDA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_ENDA2 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_ENDA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_ENDA3 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_ENDA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_ENDA4 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_ENDA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_bxg_1 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_bxg_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_bxg_2 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_bxg_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_bxg_3 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_bxg_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_bxg_4 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_bxg_4)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_bxg_t = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_bxg_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_bxg1 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_bxg+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_bxg2 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_bxg+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_bxg4 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_bxg+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_BCA_1 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_BCA_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_BCA_2 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_BCA_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_BCA_3 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_BCA_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_BCA_t = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_BCA_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_BCA1 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_BCA+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_BCA2 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_BCA+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_BCA3 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_BCA+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_BCA4 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_BCA+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_D_1 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_D_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_D_2 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_D_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_D_3 = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_D_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_D_t = Sub.Option_External_debt_Contracted_Guaranteed * QPC_Original_Amount_D_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_D1 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_D+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_D2 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_D+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_D3 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_D+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_QPC_Original_Amount_D4 = Sub.Option_External_debt_Contracted_Guaranteed * `QPC_Original_Amount_D+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T_ENDA_pcpi = Sub.Option_External_debt_Contracted_Guaranteed * T_ENDA_pcpi)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T1_ENDA_pcpi = Sub.Option_External_debt_Contracted_Guaranteed * `T+1_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T2_ENDA_pcpi = Sub.Option_External_debt_Contracted_Guaranteed * `T+2_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T3_ENDA_pcpi = Sub.Option_External_debt_Contracted_Guaranteed * `T+3_ENDA_pcpi`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T4_ENDA_pcpi = Sub.Option_External_debt_Contracted_Guaranteed * `T+4_ENDA_pcpi`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T_D_bxg = Sub.Option_External_debt_Contracted_Guaranteed * T_D_bxg)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T1_D_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T+1_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T2_D_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T+2_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T3_D_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T+3_D_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T4_D_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T+4_D_bxg`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T_BXG_gdp_usd = Sub.Option_External_debt_Contracted_Guaranteed * T_BXG_gdp_usd)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T1_BXG_gdp_usd = Sub.Option_External_debt_Contracted_Guaranteed * `T+1_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T2_BXG_gdp_usd= Sub.Option_External_debt_Contracted_Guaranteed * `T+2_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T3_BXG_gdp_usd = Sub.Option_External_debt_Contracted_Guaranteed * `T+3_BXG_gdp_usd`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T4_BXG_gdp_usd = Sub.Option_External_debt_Contracted_Guaranteed * `T+4_BXG_gdp_usd`)

tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T_3_BCA_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T-3_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T_2_BCA_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T-2_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T_1_BCA_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T-1_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T_BCA_bxg =  Sub.Option_External_debt_Contracted_Guaranteed * `T_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T1_BCA_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T+1_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T2_BCA_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T+2_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T3_BCA_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T+3_BCA_bxg`)
tabla6 <- tabla6 %>% mutate(Sub.Option_External_debt_Contracted_Guaranteed_T4_BCA_bxg = Sub.Option_External_debt_Contracted_Guaranteed * `T+4_BCA_bxg`)


#

tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_ngdp_1 = Sub.Option_Primary_deficit * QPC_Original_Amount_ngdp_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_ngdp_2 = Sub.Option_Primary_deficit * QPC_Original_Amount_ngdp_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_ngdp_3 = Sub.Option_Primary_deficit * QPC_Original_Amount_ngdp_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_ngdp_t = Sub.Option_Primary_deficit * QPC_Original_Amount_ngdp_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_ngdp1 = Sub.Option_Primary_deficit * `QPC_Original_Amount_ngdp+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_ngdp2 = Sub.Option_Primary_deficit * `QPC_Original_Amount_ngdp+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_ngdp3 = Sub.Option_Primary_deficit * `QPC_Original_Amount_ngdp+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_ngdp4 = Sub.Option_Primary_deficit * `QPC_Original_Amount_ngdp+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_NCG_1 = Sub.Option_Primary_deficit * QPC_Original_Amount_NCG_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_NCG_2 = Sub.Option_Primary_deficit * QPC_Original_Amount_NCG_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_NCG_t = Sub.Option_Primary_deficit * QPC_Original_Amount_NCG_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_NCG1 = Sub.Option_Primary_deficit * `QPC_Original_Amount_NCG+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_NCG2 = Sub.Option_Primary_deficit * `QPC_Original_Amount_NCG+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_QPC_Original_Amount_NCG3 = Sub.Option_Primary_deficit * `QPC_Original_Amount_NCG+3`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_3_NFI_gdp = Sub.Option_Primary_deficit * `T-3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_2_NFI_gdp = Sub.Option_Primary_deficit * `T-2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_1_NFI_gdp = Sub.Option_Primary_deficit * `T-1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_NFI_gdp = Sub.Option_Primary_deficit * T_NFI_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T1_NFI_gdp = Sub.Option_Primary_deficit * `T+1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T2_NFI_gdp = Sub.Option_Primary_deficit * `T+2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T3_NFI_gdp = Sub.Option_Primary_deficit * `T+3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T4_NFI_gdp = Sub.Option_Primary_deficit * `T+4_NFI_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_3_NCG_gdp = Sub.Option_Primary_deficit * `T-3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_2_NCG_gdp = Sub.Option_Primary_deficit * `T-2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_1_NCG_gdp = Sub.Option_Primary_deficit * `T-1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_NCG_gdp =  Sub.Option_Primary_deficit * T_NCG_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T1_NCG_gdp = Sub.Option_Primary_deficit * `T+1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T2_NCG_gdp = Sub.Option_Primary_deficit * `T+2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T3_NCG_gdp = Sub.Option_Primary_deficit * `T+3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T4_NCG_gdp = Sub.Option_Primary_deficit * `T+4_NCG_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_3_NCP_gdp = Sub.Option_Primary_deficit * `T-3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_2_NCP_gdp = Sub.Option_Primary_deficit * `T-2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_1_NCP_gdp = Sub.Option_Primary_deficit * `T-1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T_NCP_gdp =  Sub.Option_Primary_deficit * T_NCP_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T1_NCP_gdp = Sub.Option_Primary_deficit * `T+1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T2_NCP_gdp = Sub.Option_Primary_deficit * `T+2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T3_NCP_gdp = Sub.Option_Primary_deficit * `T+3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Primary_deficit_T4_NCP_gdp = Sub.Option_Primary_deficit * `T+4_NCP_gdp`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_ngdp_1 = Sub.Option_Overall_deficit * QPC_Original_Amount_ngdp_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_ngdp_2 = Sub.Option_Overall_deficit * QPC_Original_Amount_ngdp_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_ngdp_3 = Sub.Option_Overall_deficit * QPC_Original_Amount_ngdp_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_ngdp_t = Sub.Option_Overall_deficit * QPC_Original_Amount_ngdp_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_ngdp1 = Sub.Option_Overall_deficit * `QPC_Original_Amount_ngdp+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_ngdp2 = Sub.Option_Overall_deficit * `QPC_Original_Amount_ngdp+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_ngdp3 = Sub.Option_Overall_deficit * `QPC_Original_Amount_ngdp+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_ngdp4 = Sub.Option_Overall_deficit * `QPC_Original_Amount_ngdp+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_NCG_1 = Sub.Option_Overall_deficit * QPC_Original_Amount_NCG_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_NCG_2 = Sub.Option_Overall_deficit * QPC_Original_Amount_NCG_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_NCG_t = Sub.Option_Overall_deficit * QPC_Original_Amount_NCG_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_NCG1 = Sub.Option_Overall_deficit * `QPC_Original_Amount_NCG+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_NCG2 = Sub.Option_Overall_deficit * `QPC_Original_Amount_NCG+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_QPC_Original_Amount_NCG3 = Sub.Option_Overall_deficit * `QPC_Original_Amount_NCG+3`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_3_NFI_gdp = Sub.Option_Overall_deficit * `T-3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_2_NFI_gdp = Sub.Option_Overall_deficit * `T-2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_1_NFI_gdp = Sub.Option_Overall_deficit * `T-1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_NFI_gdp = Sub.Option_Overall_deficit * T_NFI_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T1_NFI_gdp = Sub.Option_Overall_deficit * `T+1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T2_NFI_gdp = Sub.Option_Overall_deficit * `T+2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T3_NFI_gdp = Sub.Option_Overall_deficit * `T+3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T4_NFI_gdp = Sub.Option_Overall_deficit * `T+4_NFI_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_3_NCG_gdp = Sub.Option_Overall_deficit * `T-3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_2_NCG_gdp = Sub.Option_Overall_deficit * `T-2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_1_NCG_gdp = Sub.Option_Overall_deficit * `T-1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_NCG_gdp =  Sub.Option_Overall_deficit * T_NCG_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T1_NCG_gdp = Sub.Option_Overall_deficit * `T+1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T2_NCG_gdp = Sub.Option_Overall_deficit * `T+2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T3_NCG_gdp = Sub.Option_Overall_deficit * `T+3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T4_NCG_gdp = Sub.Option_Overall_deficit * `T+4_NCG_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_3_NCP_gdp = Sub.Option_Overall_deficit * `T-3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_2_NCP_gdp = Sub.Option_Overall_deficit * `T-2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_1_NCP_gdp = Sub.Option_Overall_deficit * `T-1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T_NCP_gdp =  Sub.Option_Overall_deficit * T_NCP_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T1_NCP_gdp = Sub.Option_Overall_deficit * `T+1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T2_NCP_gdp = Sub.Option_Overall_deficit * `T+2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T3_NCP_gdp = Sub.Option_Overall_deficit * `T+3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Overall_deficit_T4_NCP_gdp = Sub.Option_Overall_deficit * `T+4_NCP_gdp`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_ngdp_1 = Sub.Option_Revenue * QPC_Original_Amount_ngdp_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_ngdp_2 = Sub.Option_Revenue * QPC_Original_Amount_ngdp_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_ngdp_3 = Sub.Option_Revenue * QPC_Original_Amount_ngdp_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_ngdp_t = Sub.Option_Revenue * QPC_Original_Amount_ngdp_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_ngdp1 = Sub.Option_Revenue * `QPC_Original_Amount_ngdp+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_ngdp2 = Sub.Option_Revenue * `QPC_Original_Amount_ngdp+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_ngdp3 = Sub.Option_Revenue * `QPC_Original_Amount_ngdp+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_ngdp4 = Sub.Option_Revenue * `QPC_Original_Amount_ngdp+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_NCG_1 = Sub.Option_Revenue * QPC_Original_Amount_NCG_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_NCG_2 = Sub.Option_Revenue * QPC_Original_Amount_NCG_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_NCG_t = Sub.Option_Revenue * QPC_Original_Amount_NCG_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_NCG1 = Sub.Option_Revenue * `QPC_Original_Amount_NCG+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_NCG2 = Sub.Option_Revenue * `QPC_Original_Amount_NCG+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_QPC_Original_Amount_NCG3 = Sub.Option_Revenue * `QPC_Original_Amount_NCG+3`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_3_NFI_gdp = Sub.Option_Revenue * `T-3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_2_NFI_gdp = Sub.Option_Revenue * `T-2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_1_NFI_gdp = Sub.Option_Revenue * `T-1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_NFI_gdp = Sub.Option_Revenue * T_NFI_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T1_NFI_gdp = Sub.Option_Revenue * `T+1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T2_NFI_gdp = Sub.Option_Revenue * `T+2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T3_NFI_gdp = Sub.Option_Revenue * `T+3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T4_NFI_gdp = Sub.Option_Revenue * `T+4_NFI_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_3_NCG_gdp = Sub.Option_Revenue * `T-3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_2_NCG_gdp = Sub.Option_Revenue * `T-2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_1_NCG_gdp = Sub.Option_Revenue * `T-1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_NCG_gdp =  Sub.Option_Revenue * T_NCG_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T1_NCG_gdp = Sub.Option_Revenue * `T+1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T2_NCG_gdp = Sub.Option_Revenue * `T+2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T3_NCG_gdp = Sub.Option_Revenue * `T+3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T4_NCG_gdp = Sub.Option_Revenue * `T+4_NCG_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_3_NCP_gdp = Sub.Option_Revenue * `T-3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_2_NCP_gdp = Sub.Option_Revenue * `T-2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_1_NCP_gdp = Sub.Option_Revenue * `T-1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T_NCP_gdp =  Sub.Option_Revenue * T_NCP_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T1_NCP_gdp = Sub.Option_Revenue * `T+1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T2_NCP_gdp = Sub.Option_Revenue * `T+2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T3_NCP_gdp = Sub.Option_Revenue * `T+3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Revenue_T4_NCP_gdp = Sub.Option_Revenue * `T+4_NCP_gdp`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_ngdp_1 = Sub.Option_Spending * QPC_Original_Amount_ngdp_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_ngdp_2 = Sub.Option_Spending * QPC_Original_Amount_ngdp_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_ngdp_3 = Sub.Option_Spending * QPC_Original_Amount_ngdp_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_ngdp_t = Sub.Option_Spending * QPC_Original_Amount_ngdp_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_ngdp1 = Sub.Option_Spending * `QPC_Original_Amount_ngdp+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_ngdp2 = Sub.Option_Spending * `QPC_Original_Amount_ngdp+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_ngdp3 = Sub.Option_Spending * `QPC_Original_Amount_ngdp+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_ngdp4 = Sub.Option_Spending * `QPC_Original_Amount_ngdp+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_NCG_1 = Sub.Option_Spending * QPC_Original_Amount_NCG_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_NCG_2 = Sub.Option_Spending * QPC_Original_Amount_NCG_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_NCG_t = Sub.Option_Spending * QPC_Original_Amount_NCG_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_NCG1 = Sub.Option_Spending * `QPC_Original_Amount_NCG+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_NCG2 = Sub.Option_Spending * `QPC_Original_Amount_NCG+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_QPC_Original_Amount_NCG3 = Sub.Option_Spending * `QPC_Original_Amount_NCG+3`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_3_NFI_gdp = Sub.Option_Spending * `T-3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_2_NFI_gdp = Sub.Option_Spending * `T-2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_1_NFI_gdp = Sub.Option_Spending * `T-1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_NFI_gdp = Sub.Option_Spending * T_NFI_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T1_NFI_gdp = Sub.Option_Spending * `T+1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T2_NFI_gdp = Sub.Option_Spending * `T+2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T3_NFI_gdp = Sub.Option_Spending * `T+3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T4_NFI_gdp = Sub.Option_Spending * `T+4_NFI_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_3_NCG_gdp = Sub.Option_Spending * `T-3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_2_NCG_gdp = Sub.Option_Spending * `T-2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_1_NCG_gdp = Sub.Option_Spending * `T-1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_NCG_gdp =  Sub.Option_Spending * T_NCG_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T1_NCG_gdp = Sub.Option_Spending * `T+1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T2_NCG_gdp = Sub.Option_Spending * `T+2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T3_NCG_gdp = Sub.Option_Spending * `T+3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T4_NCG_gdp = Sub.Option_Spending * `T+4_NCG_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_3_NCP_gdp = Sub.Option_Spending * `T-3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_2_NCP_gdp = Sub.Option_Spending * `T-2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_1_NCP_gdp = Sub.Option_Spending * `T-1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T_NCP_gdp =  Sub.Option_Spending * T_NCP_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T1_NCP_gdp = Sub.Option_Spending * `T+1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T2_NCP_gdp = Sub.Option_Spending * `T+2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T3_NCP_gdp = Sub.Option_Spending * `T+3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Spending_T4_NCP_gdp = Sub.Option_Spending * `T+4_NCP_gdp`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_ngdp_1 = Sub.Option_Other * QPC_Original_Amount_ngdp_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_ngdp_2 = Sub.Option_Other * QPC_Original_Amount_ngdp_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_ngdp_3 = Sub.Option_Other * QPC_Original_Amount_ngdp_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_ngdp_t = Sub.Option_Other * QPC_Original_Amount_ngdp_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_ngdp1 = Sub.Option_Other * `QPC_Original_Amount_ngdp+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_ngdp2 = Sub.Option_Other * `QPC_Original_Amount_ngdp+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_ngdp3 = Sub.Option_Other * `QPC_Original_Amount_ngdp+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_ngdp4 = Sub.Option_Other * `QPC_Original_Amount_ngdp+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_NCG_1 = Sub.Option_Other * QPC_Original_Amount_NCG_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_NCG_2 = Sub.Option_Other * QPC_Original_Amount_NCG_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_NCG_t = Sub.Option_Other * QPC_Original_Amount_NCG_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_NCG1 = Sub.Option_Other * `QPC_Original_Amount_NCG+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_NCG2 = Sub.Option_Other * `QPC_Original_Amount_NCG+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_QPC_Original_Amount_NCG3 = Sub.Option_Other * `QPC_Original_Amount_NCG+3`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_3_NFI_gdp = Sub.Option_Other * `T-3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_2_NFI_gdp = Sub.Option_Other * `T-2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_1_NFI_gdp = Sub.Option_Other * `T-1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_NFI_gdp = Sub.Option_Other * T_NFI_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T1_NFI_gdp = Sub.Option_Other * `T+1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T2_NFI_gdp = Sub.Option_Other * `T+2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T3_NFI_gdp = Sub.Option_Other * `T+3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T4_NFI_gdp = Sub.Option_Other * `T+4_NFI_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_3_NCG_gdp = Sub.Option_Other * `T-3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_2_NCG_gdp = Sub.Option_Other * `T-2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_1_NCG_gdp = Sub.Option_Other * `T-1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_NCG_gdp =  Sub.Option_Other * T_NCG_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T1_NCG_gdp = Sub.Option_Other * `T+1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T2_NCG_gdp = Sub.Option_Other * `T+2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T3_NCG_gdp = Sub.Option_Other * `T+3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T4_NCG_gdp = Sub.Option_Other * `T+4_NCG_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_3_NCP_gdp = Sub.Option_Other * `T-3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_2_NCP_gdp = Sub.Option_Other * `T-2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_1_NCP_gdp = Sub.Option_Other * `T-1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T_NCP_gdp =  Sub.Option_Other * T_NCP_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T1_NCP_gdp = Sub.Option_Other * `T+1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T2_NCP_gdp = Sub.Option_Other * `T+2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T3_NCP_gdp = Sub.Option_Other * `T+3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Other_T4_NCP_gdp = Sub.Option_Other * `T+4_NCP_gdp`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_ngdp_1 = Sub.Option_Domestic_arrears_stock * QPC_Original_Amount_ngdp_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_ngdp_2 = Sub.Option_Domestic_arrears_stock * QPC_Original_Amount_ngdp_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_ngdp_3 = Sub.Option_Domestic_arrears_stock * QPC_Original_Amount_ngdp_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_ngdp_t = Sub.Option_Domestic_arrears_stock * QPC_Original_Amount_ngdp_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_ngdp1 = Sub.Option_Domestic_arrears_stock * `QPC_Original_Amount_ngdp+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_ngdp2 = Sub.Option_Domestic_arrears_stock * `QPC_Original_Amount_ngdp+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_ngdp3 = Sub.Option_Domestic_arrears_stock * `QPC_Original_Amount_ngdp+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_ngdp4 = Sub.Option_Domestic_arrears_stock * `QPC_Original_Amount_ngdp+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_NCG_1 = Sub.Option_Domestic_arrears_stock * QPC_Original_Amount_NCG_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_NCG_2 = Sub.Option_Domestic_arrears_stock * QPC_Original_Amount_NCG_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_NCG_t = Sub.Option_Domestic_arrears_stock * QPC_Original_Amount_NCG_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_NCG1 = Sub.Option_Domestic_arrears_stock * `QPC_Original_Amount_NCG+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_NCG2 = Sub.Option_Domestic_arrears_stock * `QPC_Original_Amount_NCG+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_QPC_Original_Amount_NCG3 = Sub.Option_Domestic_arrears_stock * `QPC_Original_Amount_NCG+3`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_3_NFI_gdp = Sub.Option_Domestic_arrears_stock * `T-3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_2_NFI_gdp = Sub.Option_Domestic_arrears_stock * `T-2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_1_NFI_gdp = Sub.Option_Domestic_arrears_stock * `T-1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_NFI_gdp = Sub.Option_Domestic_arrears_stock * T_NFI_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T1_NFI_gdp = Sub.Option_Domestic_arrears_stock * `T+1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T2_NFI_gdp = Sub.Option_Domestic_arrears_stock * `T+2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T3_NFI_gdp = Sub.Option_Domestic_arrears_stock * `T+3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T4_NFI_gdp = Sub.Option_Domestic_arrears_stock * `T+4_NFI_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_3_NCG_gdp = Sub.Option_Domestic_arrears_stock * `T-3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_2_NCG_gdp = Sub.Option_Domestic_arrears_stock * `T-2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_1_NCG_gdp = Sub.Option_Domestic_arrears_stock * `T-1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_NCG_gdp =  Sub.Option_Domestic_arrears_stock * T_NCG_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T1_NCG_gdp = Sub.Option_Domestic_arrears_stock * `T+1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T2_NCG_gdp = Sub.Option_Domestic_arrears_stock * `T+2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T3_NCG_gdp = Sub.Option_Domestic_arrears_stock * `T+3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T4_NCG_gdp = Sub.Option_Domestic_arrears_stock * `T+4_NCG_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_3_NCP_gdp = Sub.Option_Domestic_arrears_stock * `T-3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_2_NCP_gdp = Sub.Option_Domestic_arrears_stock * `T-2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_1_NCP_gdp = Sub.Option_Domestic_arrears_stock * `T-1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T_NCP_gdp =  Sub.Option_Domestic_arrears_stock * T_NCP_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T1_NCP_gdp = Sub.Option_Domestic_arrears_stock * `T+1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T2_NCP_gdp = Sub.Option_Domestic_arrears_stock * `T+2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T3_NCP_gdp = Sub.Option_Domestic_arrears_stock * `T+3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_stock_T4_NCP_gdp = Sub.Option_Domestic_arrears_stock * `T+4_NCP_gdp`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_ngdp_1 = Sub.Option_Domestic_arrears_accum * QPC_Original_Amount_ngdp_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_ngdp_2 = Sub.Option_Domestic_arrears_accum * QPC_Original_Amount_ngdp_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_ngdp_3 = Sub.Option_Domestic_arrears_accum * QPC_Original_Amount_ngdp_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_ngdp_t = Sub.Option_Domestic_arrears_accum * QPC_Original_Amount_ngdp_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_ngdp1 = Sub.Option_Domestic_arrears_accum * `QPC_Original_Amount_ngdp+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_ngdp2 = Sub.Option_Domestic_arrears_accum * `QPC_Original_Amount_ngdp+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_ngdp3 = Sub.Option_Domestic_arrears_accum * `QPC_Original_Amount_ngdp+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_ngdp4 = Sub.Option_Domestic_arrears_accum * `QPC_Original_Amount_ngdp+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_NCG_1 = Sub.Option_Domestic_arrears_accum * QPC_Original_Amount_NCG_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_NCG_2 = Sub.Option_Domestic_arrears_accum * QPC_Original_Amount_NCG_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_NCG_t = Sub.Option_Domestic_arrears_accum * QPC_Original_Amount_NCG_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_NCG1 = Sub.Option_Domestic_arrears_accum * `QPC_Original_Amount_NCG+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_NCG2 = Sub.Option_Domestic_arrears_accum * `QPC_Original_Amount_NCG+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_QPC_Original_Amount_NCG3 = Sub.Option_Domestic_arrears_accum * `QPC_Original_Amount_NCG+3`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_3_NFI_gdp = Sub.Option_Domestic_arrears_accum * `T-3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_2_NFI_gdp = Sub.Option_Domestic_arrears_accum * `T-2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_1_NFI_gdp = Sub.Option_Domestic_arrears_accum * `T-1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_NFI_gdp = Sub.Option_Domestic_arrears_accum * T_NFI_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T1_NFI_gdp = Sub.Option_Domestic_arrears_accum * `T+1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T2_NFI_gdp = Sub.Option_Domestic_arrears_accum * `T+2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T3_NFI_gdp = Sub.Option_Domestic_arrears_accum * `T+3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T4_NFI_gdp = Sub.Option_Domestic_arrears_accum * `T+4_NFI_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_3_NCG_gdp = Sub.Option_Domestic_arrears_accum * `T-3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_2_NCG_gdp = Sub.Option_Domestic_arrears_accum * `T-2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_1_NCG_gdp = Sub.Option_Domestic_arrears_accum * `T-1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_NCG_gdp =  Sub.Option_Domestic_arrears_accum * T_NCG_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T1_NCG_gdp = Sub.Option_Domestic_arrears_accum * `T+1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T2_NCG_gdp = Sub.Option_Domestic_arrears_accum * `T+2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T3_NCG_gdp = Sub.Option_Domestic_arrears_accum * `T+3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T4_NCG_gdp = Sub.Option_Domestic_arrears_accum * `T+4_NCG_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_3_NCP_gdp = Sub.Option_Domestic_arrears_accum * `T-3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_2_NCP_gdp = Sub.Option_Domestic_arrears_accum * `T-2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_1_NCP_gdp = Sub.Option_Domestic_arrears_accum * `T-1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T_NCP_gdp =  Sub.Option_Domestic_arrears_accum * T_NCP_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T1_NCP_gdp = Sub.Option_Domestic_arrears_accum * `T+1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T2_NCP_gdp = Sub.Option_Domestic_arrears_accum * `T+2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T3_NCP_gdp = Sub.Option_Domestic_arrears_accum * `T+3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Domestic_arrears_accum_T4_NCP_gdp = Sub.Option_Domestic_arrears_accum * `T+4_NCP_gdp`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_1 = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_2 = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_3 = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_t = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp1 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp2 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp3 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp4 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG_1 = Sub.Option_Credit_Domestic * QPC_Original_Amount_NCG_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG_2 = Sub.Option_Credit_Domestic * QPC_Original_Amount_NCG_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG_t = Sub.Option_Credit_Domestic * QPC_Original_Amount_NCG_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG1 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_NCG+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG2 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_NCG+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG3 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_NCG+3`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_3_NFI_gdp = Sub.Option_Credit_Domestic * `T-3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_2_NFI_gdp = Sub.Option_Credit_Domestic * `T-2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_1_NFI_gdp = Sub.Option_Credit_Domestic * `T-1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_NFI_gdp = Sub.Option_Credit_Domestic * T_NFI_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T1_NFI_gdp = Sub.Option_Credit_Domestic * `T+1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T2_NFI_gdp = Sub.Option_Credit_Domestic * `T+2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T3_NFI_gdp = Sub.Option_Credit_Domestic * `T+3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T4_NFI_gdp = Sub.Option_Credit_Domestic * `T+4_NFI_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_3_NCG_gdp = Sub.Option_Credit_Domestic * `T-3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_2_NCG_gdp = Sub.Option_Credit_Domestic * `T-2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_1_NCG_gdp = Sub.Option_Credit_Domestic * `T-1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_NCG_gdp =  Sub.Option_Credit_Domestic * T_NCG_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T1_NCG_gdp = Sub.Option_Credit_Domestic * `T+1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T2_NCG_gdp = Sub.Option_Credit_Domestic * `T+2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T3_NCG_gdp = Sub.Option_Credit_Domestic * `T+3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T4_NCG_gdp = Sub.Option_Credit_Domestic * `T+4_NCG_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_3_NCP_gdp = Sub.Option_Credit_Domestic * `T-3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_2_NCP_gdp = Sub.Option_Credit_Domestic * `T-2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_1_NCP_gdp = Sub.Option_Credit_Domestic * `T-1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T_NCP_gdp =  Sub.Option_Credit_Domestic * T_NCP_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T1_NCP_gdp = Sub.Option_Credit_Domestic * `T+1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T2_NCP_gdp = Sub.Option_Credit_Domestic * `T+2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T3_NCP_gdp = Sub.Option_Credit_Domestic * `T+3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_T4_NCP_gdp = Sub.Option_Credit_Domestic * `T+4_NCP_gdp`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_1 = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_2 = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_3 = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_t = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp1 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp2 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp3 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp4 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG_1 = Sub.Option_Credit_Domestic * QPC_Original_Amount_NCG_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG_2 = Sub.Option_Credit_Domestic * QPC_Original_Amount_NCG_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG_t = Sub.Option_Credit_Domestic * QPC_Original_Amount_NCG_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG1 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_NCG+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG2 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_NCG+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG3 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_NCG+3`)

tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_3_NFI_gdp = Sub.Option_CentralBank_Gov * `T-3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_2_NFI_gdp = Sub.Option_CentralBank_Gov * `T-2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_1_NFI_gdp = Sub.Option_CentralBank_Gov * `T-1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_NFI_gdp = Sub.Option_CentralBank_Gov * T_NFI_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T1_NFI_gdp = Sub.Option_CentralBank_Gov * `T+1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T2_NFI_gdp = Sub.Option_CentralBank_Gov * `T+2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T3_NFI_gdp = Sub.Option_CentralBank_Gov * `T+3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T4_NFI_gdp = Sub.Option_CentralBank_Gov * `T+4_NFI_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_3_NCG_gdp = Sub.Option_CentralBank_Gov * `T-3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_2_NCG_gdp = Sub.Option_CentralBank_Gov * `T-2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_1_NCG_gdp = Sub.Option_CentralBank_Gov * `T-1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_NCG_gdp =  Sub.Option_CentralBank_Gov * T_NCG_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T1_NCG_gdp = Sub.Option_CentralBank_Gov * `T+1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T2_NCG_gdp = Sub.Option_CentralBank_Gov * `T+2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T3_NCG_gdp = Sub.Option_CentralBank_Gov * `T+3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T4_NCG_gdp = Sub.Option_CentralBank_Gov * `T+4_NCG_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_3_NCP_gdp = Sub.Option_CentralBank_Gov * `T-3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_2_NCP_gdp = Sub.Option_CentralBank_Gov * `T-2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_1_NCP_gdp = Sub.Option_CentralBank_Gov * `T-1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T_NCP_gdp =  Sub.Option_CentralBank_Gov * T_NCP_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T1_NCP_gdp = Sub.Option_CentralBank_Gov * `T+1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T2_NCP_gdp = Sub.Option_CentralBank_Gov * `T+2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T3_NCP_gdp = Sub.Option_CentralBank_Gov * `T+3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_CentralBank_Gov_T4_NCP_gdp = Sub.Option_CentralBank_Gov * `T+4_NCP_gdp`)

#

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_1 = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_2 = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_3 = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_3)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp_t = Sub.Option_Credit_Domestic * QPC_Original_Amount_ngdp_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp1 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp2 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp3 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+3`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_ngdp4 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_ngdp+4`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG_1 = Sub.Option_Credit_Domestic * QPC_Original_Amount_NCG_1)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG_2 = Sub.Option_Credit_Domestic * QPC_Original_Amount_NCG_2)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG_t = Sub.Option_Credit_Domestic * QPC_Original_Amount_NCG_t)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG1 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_NCG+1`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG2 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_NCG+2`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Domestic_QPC_Original_Amount_NCG3 = Sub.Option_Credit_Domestic * `QPC_Original_Amount_NCG+3`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_3_NFI_gdp = Sub.Option_Credit_Central_bank * `T-3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_2_NFI_gdp = Sub.Option_Credit_Central_bank * `T-2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_1_NFI_gdp = Sub.Option_Credit_Central_bank * `T-1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_NFI_gdp = Sub.Option_Credit_Central_bank * T_NFI_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T1_NFI_gdp = Sub.Option_Credit_Central_bank * `T+1_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T2_NFI_gdp = Sub.Option_Credit_Central_bank * `T+2_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T3_NFI_gdp = Sub.Option_Credit_Central_bank * `T+3_NFI_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T4_NFI_gdp = Sub.Option_Credit_Central_bank * `T+4_NFI_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_3_NCG_gdp = Sub.Option_Credit_Central_bank * `T-3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_2_NCG_gdp = Sub.Option_Credit_Central_bank * `T-2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_1_NCG_gdp = Sub.Option_Credit_Central_bank * `T-1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_NCG_gdp =  Sub.Option_Credit_Central_bank * T_NCG_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T1_NCG_gdp = Sub.Option_Credit_Central_bank * `T+1_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T2_NCG_gdp = Sub.Option_Credit_Central_bank * `T+2_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T3_NCG_gdp = Sub.Option_Credit_Central_bank * `T+3_NCG_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T4_NCG_gdp = Sub.Option_Credit_Central_bank * `T+4_NCG_gdp`)

tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_3_NCP_gdp = Sub.Option_Credit_Central_bank * `T-3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_2_NCP_gdp = Sub.Option_Credit_Central_bank * `T-2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_1_NCP_gdp = Sub.Option_Credit_Central_bank * `T-1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T_NCP_gdp =  Sub.Option_Credit_Central_bank * T_NCP_gdp)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T1_NCP_gdp = Sub.Option_Credit_Central_bank * `T+1_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T2_NCP_gdp = Sub.Option_Credit_Central_bank * `T+2_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T3_NCP_gdp = Sub.Option_Credit_Central_bank * `T+3_NCP_gdp`)
tabla6 <- tabla6 %>% mutate(Sub.Option_Credit_Central_bank_T4_NCP_gdp = Sub.Option_Credit_Central_bank * `T+4_NCP_gdp`)

#############################################

# Buscamos NAs

colSums(is.na(tabla6))

sapply(tabla6, function(x) sum(is.infinite(x)))

# Imputamos la mediana

library(dplyr)

tabla6 <- tabla6 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))


###

library(dplyr)
library(purrr)

# Función mejorada para reemplazar valores problemáticos
limpiar_columna <- function(x) {
  # Manejar columnas numéricas
  if(is.numeric(x)) {
    x[x == "" | is.infinite(x)] <- NA
    med <- median(x, na.rm = TRUE)
    x[is.na(x)] <- med
    return(x)
  }
  
  # Manejar columnas de caracteres/factores
  if(is.character(x) | is.factor(x)) {
    x[x == ""] <- NA
    moda <- names(which.max(table(x)))
    x[is.na(x)] <- moda
    return(x)
  }
  
  # Manejar columnas lógicas
  if(is.logical(x)) {
    x[is.na(x)] <- FALSE
    return(x)
  }
  
  return(x)
}

# Aplicar a todo el dataframe
tabla6 <- tabla6 %>%
  mutate(across(everything(), limpiar_columna))

# Verificación final
colSums(is.na(tabla6))

sapply(tabla6, function(x) sum(is.infinite(x)))

tabla6 <- tabla6 %>% distinct()

#############################################

tabla6 <- tabla6 %>% select(-QPC_Units_)
tabla6 <- tabla6 %>% select(-Adjusted.Amount)
tabla6 <- tabla6 %>% select(-Actual.Amount)

glimpse(tabla6)
colnames(tabla6)

###########################################################################################

tabla6 <- tabla6[, -c(164:331)]

###########################################################################################

datos_filtrados <- tabla6 %>%
  filter(Key_larga == "213_2018_2021_770_R1_16")

tabla6 <- tabla6 %>% select(-Key_larga)
###########################################################################################

tabla7 <- tabla6

tabla6 <- tabla6 %>%
  filter(QPC_Criteria_Order %in% c(135, 32))


###########################################################################################

base <- tabla6

library(ggplot2)
library(dplyr)
library(lubridate)

# 1. Filtrar por rango de años (2003-2024)
base <- base %>%
  mutate(Year = year(QPC_Test_Date)) %>%
  filter(Year >= 2003 & Year <= 2024)

# Primero asegurémonos de que los datos estén en el formato correcto
datos_anuales <- base %>%
  count(Year, Status) %>%  # Crear conteos si no existen
  mutate(
    Year = factor(Year),
    Status = factor(Status)
  )

# Crear etiquetas en la posición correcta (acumuladas)
etiquetas <- datos_anuales %>%
  group_by(Year) %>%
  arrange(Year, desc(Status)) %>%
  mutate(
    posicion_cumulativa = cumsum(n),
    posicion_etiqueta = posicion_cumulativa - (n / 2)
  )

# Obtener número único de estados para la paleta de colores
num_estados <- nlevels(datos_anuales$Status)

ggplot(datos_anuales, aes(x = Year, y = n, fill = Status)) +
  geom_col(position = position_stack(), color = "black", width = 0.7) +
  geom_text(
    data = etiquetas,
    aes(y = posicion_etiqueta, label = ifelse(n > 0, n, "")),
    size = 3.5,
    family = "Times New Roman",
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = colorRampPalette(c("#3A5F7F", "#A83232"))(num_estados)
  ) +
  labs(x = "Año", y = "Número de QPCs", fill = "Status") +
  theme_minimal(base_family = "Times New Roman", base_size = 16) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 11
    ),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.margin = unit(c(10, 10, 20, 10), "points"),
    legend.position = "top"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

tabla6 <- tabla6 %>%
  relocate(QPC_Test_Date, .before = 3)

colnames(tabla6)

###############################

tabla6 <- tabla6 %>%
  mutate(Year = year(QPC_Test_Date))

tabla6 <- tabla6 %>%
  mutate(shock_I = ifelse(Year %in% c(2010, 2011, 2014, 2018, 2019, 2020, 2022, 2023), 1, 0))

tabla6 <- tabla6 %>%
  mutate(shock_II = ifelse(Year %in% c(2009, 2010, 2014, 2015, 2016, 2022, 2023, 2024), 1, 0))

#tabla6 <- tabla6 %>% select(-Year)

######################################################################################


tabla6 <- tabla6 %>%
  mutate(GRA_redux = if_else(`Arrangement.Type_EFF` == 1 | 
                               `Arrangement.Type_SBA` == 1, 1, 0))

tabla6 <- tabla6 %>%
  mutate(PRGT_redux = if_else(`Arrangement.Type_ECF` == 1 | 
                                `Arrangement.Type_SCF` == 1, 1, 0))

########################################################################################

names(tabla6) <- gsub("-", "_", names(tabla6))
names(tabla6) <- gsub("\\+", "X", names(tabla6))

##########################################################################################

tabla6$QPC_Amount_span_perc <- (tabla6$QPC_Actual_Amount_real - tabla6$QPC_Original_Amount_real) / tabla6$QPC_Original_Amount_real

tabla6$Sesgo <- ifelse(tabla6$QPC_Amount_span_perc > 0, 1, -1)

###########################################################################################

tabla6 <- tabla6 %>%
  mutate(across(everything(), limpiar_columna))

###########################################################################################

colnames(tabla6)

tabla6 <- tabla6[, -c(15:33)]

tabla6 <- tabla6[, -c(19:22)]

tabla6 <- tabla6[, -c(24:25)]

tabla6 <- tabla6[, -c(703:919)]

tabla6 <- tabla6[, -c(813:1000)]

tabla6 <- tabla6[, -c(813:1000)]

tabla6 <- tabla6[, -c(813:953)]

tabla6 <- tabla6[, -c(814:815)]

tabla6 <- tabla6[, -c(816)]

tabla6 <- tabla6[, -c(37:134)]

tabla6 <- tabla6[, -c(160:295)]

tabla6 <- tabla6[, -c(438:468)]

tabla6 <- tabla6[, -c(493:547)]

###########################################################################################

# Pasar a excel la table
write_xlsx(tabla6, "C:/Users/HP/Desktop/Tesis UDESA/2026 03 23/base2.xlsx")


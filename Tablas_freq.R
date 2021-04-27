rm(list = ls(all = TRUE))

# #Sacara frecuencias de datos de Lalo
# abril 2020 @tonasantiago


# Tabla gt ----------------------------------------------------------------
library(survey)
gp1 <- as.data.frame(round(prop.table(svytable(~P52, 
                                               design=design))*100,1))

gp2 <- as.data.frame(round(prop.table(svytable(~P49, 
                                               design=design))*100,1))

gp3 <- as.data.frame(round(prop.table(svytable(~P12, 
                                               design=design))*100,1))

gp4 <- as.data.frame(round(prop.table(svytable(~P13, 
                                               design=design))*100,1))
library(gt)
freq52 <-gp1 %>%
  gt() %>%
  tab_header(
    title = md("Frecuencia de la pregunta 52"),
    subtitle = "Con relación a este último delito del que fue víctima:
¿Presentó la denuncia ante el Ministerio Público? "
  ) %>%
  tab_source_note(md("Fuente: INVENTARIO CDMX. PRESENTE Y FUTURO DE SU GENTE. ENCUESTA DE JUSTICIA Y SEGURIDAD"))
freq52

crsst <- na.omit(svytable(~P13+P12, design))
crsstab <-crsst %>%gt()
crsstab


  
pander((na.omit(prop.table(na.omit(svytable(~P13+P12, design)),1)))*100)





crsst2 <- 
crsstab <-crsst %>%gt()
crsstab

pander(
  (prop.table(crsst2)*100
)
# As-----------------------------------------------------------------------



# Almacén de funciones  ---------------------------------------------------

#rowid_to_column()
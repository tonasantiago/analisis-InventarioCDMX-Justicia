rm(list = ls(all = TRUE))
# Cargar el dataset -------------------------------------------------------

jus  <- data.table::fread('Base_SPyJ.csv')

# Declarar el diseño de la muestra ---------------------------------------

library(survey)
design = svydesign(id=~jus$UPM,strata=~jus$REGIÓN, 
                   weights=~jus$PONDI1, data=jus)


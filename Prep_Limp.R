rm(list = ls(all = TRUE))

# #Preparar y limpiar los datos de Lalo
# abril 2020 @tonasantiago

# Cargar el dataset -------------------------------------------------------
jus <- tibble::as_tibble(foreign::read.spss("Base_Seguridad_Pública_y_Justicia.sav", 
                   to.data.frame = TRUE, use.missings = TRUE))

# Salvar las etiquetas de las variables --------------------------------------------------

library(papeR)
et <- as.data.frame(labels(jus))
detach("package:papeR", unload=TRUE)

# Acomodar el mugrero para declarar estratos y UPM ------------------------

library(tidyverse)
jus <- jus %>% unite("UPM", ESTADO:AGEB, remove = FALSE)
detach("package:tidyverse", unload=TRUE)
#names(mydata) <- c("x1","age","y", "ses") NOMBRES VARIABLES
# Declarar el diseño de la encuesta ---------------------------------------

library(survey)
design = svydesign(id=~jus$UPM,strata=~jus$REGIÓN, 
                   weights=~jus$PONDI1, data=jus)


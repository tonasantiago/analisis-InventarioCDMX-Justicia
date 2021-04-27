
# Índice excepción a la ley----

# Transformar las variables
attach(jus)
jus$P5con[P5 == "  Sí tienen el derecho "] <- 0
jus$P5con[P5 == "  No tienen el derecho "] <- 4
jus$P5con[P5 == "  Tienen el derecho, en parte  "] <- 2
detach(jus)

attach(jus)
jus$P6con[P6 == "  Muy de acuerdo "] <- 0
jus$P6con[P6 == "  De acuerdo " ] <- 1
jus$P6con[P6 == "  En desacuerdo "] <- 3
jus$P6con[P6 == "  Muy en desacuerdo "] <- 4
jus$P6con[P6 == "  Ni de acuerdo ni en desacuerdo "] <- 2
detach(jus)

attach(jus)
jus$P7con[P7 == "  Muy de acuerdo "] <- 0
jus$P7con[P7 == "  De acuerdo " ] <- 1
jus$P7con[P7 == "  En desacuerdo "] <- 3
jus$P7con[P7 == "  Muy en desacuerdo "] <- 4
jus$P7con[P7 == "  Ni de acuerdo ni en desacuerdo "] <- 2
detach(jus)

# Sumar las columnas de transformación 
sumar <- select(jus,P5con:P7con)
jus$expley <- rowSums(sumar, na.rm = TRUE)

# K-means
tranclus <- kmeans(jus$expley, 3, nstart = 999)
jus$ex_ley <- tranclus$cluster

centrinos <- tibble::as_tibble(tranclus$centers)
centrinos <- tibble::rowid_to_column(centrinos)
centrinos2 <- arrange(centrinos, V1)

attach(jus)
jus$cl_ley[ex_ley == centrinos2$rowid[1]] <- "Alta excepción"
jus$cl_ley[ex_ley == centrinos2$rowid[2]] <- "Media excepción"
jus$cl_ley[ex_ley == centrinos2$rowid[3]] <- "Baja excepción"
detach(jus)


# Ordenar etiquetas
jus$cl_ley <- factor(jus$cl_ley,levels = c("Alta excepción","Media excepción",
                                           "Baja excepción"))


# Borrar objetos innecesarios y volver a declarar el diseño de la encuesta
rm(tranclus)
rm(design)
rm(centrinos2)
rm(centrinos)
rm(sumar)

library(survey)
design = svydesign(id=~jus$UPM,strata=~jus$REGIÓN, 
                   weights=~jus$PONDI1, data=jus)

#Validación 
jus%>%group_by(P6)%>%summarize(puntuacion=mean(expley))
vtree::vtree(jus, "expley cl_ley")

pander(na.omit(prop.table(na.omit(svytable(~cl_ley+P5, design)),1))*100)
pander(na.omit(prop.table(na.omit(svytable(~cl_ley+P6, design)),1))*100)
pander(na.omit(prop.table(na.omit(svytable(~cl_ley+P7, design)),1))*100)


#  Índice de confianza en los operadores jurídicos y los tribunales----

# Transformar las variables
attach(jus)
jus$P16con[P16=="  Todas las personas reciben igual trato "] <- 4
jus$P16con[P16=="  Solamente con dinero y relaciones se puede ganar un juicio "] <- 0
detach(jus)

attach(jus)
jus$P18con[P18=="  Muy independientes  "] <- 4
jus$P18con[P18=="  Algo independientes "] <- 3
jus$P18con[P18=="  Poco independientes "] <- 1
jus$P18con[P18=="  Nada independientes "] <- 0
jus$P18con[P18=="   Ni dependiente, ni independiente  "] <- 2
detach(jus)

attach(jus)
jus$P20_1con[P20_1==" De acuerdo "] <- 4
jus$P20_1con[P20_1==" En desacuerdo  " ] <- 0
jus$P20_1con[P20_1==" De acuerdo en pate  "] <- 3
jus$P20_1con[P20_1==" Descuerdo en parte  "] <- 1
detach(jus)

attach(jus)
jus$P20_3con[P20_3==" De acuerdo "] <- 4
jus$P20_3con[P20_3==" En desacuerdo  " ] <- 0
jus$P20_3con[P20_3==" De acuerdo en pate  "] <- 3
jus$P20_3con[P20_3==" Descuerdo en parte  "] <- 1
detach(jus)

attach(jus)
jus$P20_4con[P20_4==" De acuerdo "] <- 0
jus$P20_4con[P20_4==" En desacuerdo  " ] <- 4
jus$P20_4con[P20_4==" De acuerdo en pate  "] <- 1
jus$P20_4con[P20_4==" Descuerdo en parte  "] <- 3
detach(jus)

attach(jus)
jus$P20_5con[P20_5==" De acuerdo "] <- 4
jus$P20_5con[P20_5==" En desacuerdo  " ] <- 0
jus$P20_5con[P20_5==" De acuerdo en pate  "] <- 3
jus$P20_5con[P20_5==" Descuerdo en parte  "] <- 1
detach(jus)

attach(jus)
jus$P21con[P21=="  Recibe un castigo "] <- 4
jus$P21con[P21=="  Queda impune " ] <- 0
detach(jus)

attach(jus)
jus$P23con[P23=="  Sí vale la pena "] <- 4
jus$P23con[P23=="  No vale la pena "] <- 0
jus$P23con[P23=="   Sí vale la pena, en parte "] <- 2
detach(jus)


# Sumar las columnas de transformación 
sumar <- select(jus,P16con:P23con)
jus$confijusticia <- rowSums(sumar, na.rm = TRUE)

# K-means
tranclus<- kmeans(jus$confijusticia, 3, nstart = 999)
jus$tcl_confijusticia<-tranclus$cluster

centrinos <- tibble::as_tibble(tranclus$centers)
centrinos <- tibble::rowid_to_column(centrinos)
centrinos2 <- arrange(centrinos, V1)

attach(jus)
jus$cl_justicia[tcl_confijusticia == centrinos2$rowid[1]] <- "Baja confianza"
jus$cl_justicia[tcl_confijusticia == centrinos2$rowid[2]] <- "Media confianza"
jus$cl_justicia[tcl_confijusticia == centrinos2$rowid[3]] <- "Alta confianza"
detach(jus)

# Ordenar etiquetas
jus$cl_justicia <- factor(jus$cl_justicia,levels = c("Baja confianza","Media confianza",
                                           "Alta confianza"))

# Borrar objetos innecesarios y volver a declarar el diseño de la encuesta
rm(tranclus)
rm(centrinos)
rm(design)
rm(centrinos2)
rm(sumar)
library(survey)
design = svydesign(id=~jus$UPM,strata=~jus$REGIÓN, 
                   weights=~jus$PONDI1, data=jus)

#Validación 
jus%>%group_by(P23)%>%summarize(puntuacion=mean(confijusticia))
vtree::vtree(jus, "confijusticia cl_justicia")

#  Tipologías----
attach(jus)
jus$P3_1con[P3_1=="  Iglesia  "] <- "Religiosa"
jus$P3_1con[P3_1=="  Familia  "] <- "Social"
jus$P3_1con[P3_1=="  La ley  "] <- "Jurídica"
jus$P3_1con[P3_1=="  El gobierno  "] <- "Jurídica"
jus$P3_1con[P3_1=="  Uno mismo  "] <- "Moral"
detach(jus)


# Confianza en las resoluciones judiciales ------------------------------------------------------


attach(jus)
jus$P14con[P14=="  La cumple sin cuestionarla "] <- 4
jus$P14con[P14=="  Busca la manera de no cumplirla "] <- 0
jus$P14con[P14=="   Acude a un juez superior para pedirle que la cambie "] <- 2
detach(jus)


attach(jus)
jus$P19con[P19=="  Considera que la persona es, efectivamente, culpable "] <- 4
jus$P19con[P19=="  Tiene dudas de la culpabilidad de la persona " ] <- 3
jus$P19con[P19=="  Tiene seguridad de que la persona no es culpable "] <- 1
jus$P19con[P19=="  Piensa que no hay manera de saber si es culpable o no lo es "] <- 0
detach(jus)

# Sumar las columnas de transformación 
sumar <- select(jus,P14con:P19con)
jus$resoluciones <- rowSums(sumar, na.rm = TRUE)

# K-means
tranclus<- kmeans(jus$resoluciones, 3, nstart = 999)
jus$tcl_resoluciones<-tranclus$cluster

centrinos <- tibble::as_tibble(tranclus$centers)
centrinos <- tibble::rowid_to_column(centrinos)
centrinos2 <- arrange(centrinos, V1)

attach(jus)
jus$cl_resoluciones[tcl_resoluciones == centrinos2$rowid[1]] <- "Baja confianza"
jus$cl_resoluciones[tcl_resoluciones == centrinos2$rowid[2]] <- "Media confianza"
jus$cl_resoluciones[tcl_resoluciones == centrinos2$rowid[3]] <- "Alta confianza"
detach(jus)

jus$cl_resoluciones<- factor(jus$cl_resoluciones,levels = c("Baja confianza","Media confianza",
                                                     "Alta confianza"))

# Borrar objetos innecesarios y volver a declarar el diseño de la encuesta
rm(tranclus)
rm(design)
rm(centrinos2)
rm(centrinos)
rm(sumar)

library(survey)
design = svydesign(id=~jus$UPM,strata=~jus$REGIÓN, 
                   weights=~jus$PONDI1, data=jus)

#Validación 
jus%>%group_by(P19)%>%summarize(puntuacion=mean(resoluciones))
vtree::vtree(jus, "resoluciones cl_resoluciones")


# Resolución del conflicto ------------------------------------------------

# Transformar las variables
attach(jus)
jus$P13con[P13=="  Que las personas se arreglen entre ellas "] <- 4
jus$P13con[P13=="  Que las personas acudan a un tribunal "] <- 0
detach(jus)

attach(jus)
jus$P12con[P12=="  Muy de acuerdo "] <- 4
jus$P12con[P12=="  En desacuerdo " ] <- 1
jus$P12con[P12=="  De acuerdo "] <- 3
jus$P12con[P12=="  Muy en desacuerdo "] <- 0
jus$P12con[P12=="   Ni de acuerdo ni en desacuerdo  "] <- 2
detach(jus)

# Sumar las columnas de transformación 
sumar <- select(jus,P13con:P12con)
jus$conflicto <- rowSums(sumar, na.rm = TRUE)

# K-means
tranclus<- kmeans(jus$conflicto, 3, nstart = 999)
jus$tcl_conflicto <-tranclus$cluster

centrinos <- tibble::as_tibble(tranclus$centers)
centrinos <- tibble::rowid_to_column(centrinos)
centrinos2 <- arrange(centrinos, V1)

attach(jus)
jus$cl_conflicto[tcl_conflicto == centrinos2$rowid[1]] <- "Baja autocomposición"
jus$cl_conflicto[tcl_conflicto == centrinos2$rowid[2]] <- "Media autocomposición"
jus$cl_conflicto[tcl_conflicto == centrinos2$rowid[3]] <- "Alta autocomposición"
detach(jus)

# Ordenar etiquetas
jus$cl_conflicto <- factor(jus$cl_conflicto,levels = c("Baja autocomposición",
                                                       "Media autocomposición",
                                                     "Alta autocomposición"))

# Borrar objetos innecesarios y volver a declarar el diseño de la encuesta
rm(tranclus)
rm(centrinos)
rm(design)
rm(centrinos2)
rm(sumar)
library(survey)
design = svydesign(id=~jus$UPM,strata=~jus$REGIÓN, 
                   weights=~jus$PONDI1, data=jus)

#Validación 
jus%>%group_by(P13)%>%summarize(puntuacion=mean(conflicto))
vtree::vtree(jus, "conflicto cl_conflicto")


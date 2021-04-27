
# Indicé cultura de la legalidad----

# Transformar las variables
attach(jus)
jus$P5con[P5=="  Sí tienen el derecho "] <- 0
jus$P5con[P5=="  No tienen el derecho "] <- 4
jus$P5con[P5=="  Tienen el derecho, en parte  "] <- 2
detach(jus)

attach(jus)
jus$P14con[P14=="  La cumple sin cuestionarla "] <- 4
jus$P14con[P14=="  Busca la manera de no cumplirla "] <- 0
jus$P14con[P14=="   Acude a un juez superior para pedirle que la cambie "] <- 2
detach(jus)

attach(jus)
jus$P6con[P6=="  Muy de acuerdo "] <- 0
jus$P6con[P6=="  De acuerdo " ] <- 1
jus$P6con[P6=="  En desacuerdo "] <- 3
jus$P6con[P6=="  Muy en desacuerdo "] <- 4
jus$P6con[P6=="  Ni de acuerdo ni en desacuerdo "] <- 2
detach(jus)

attach(jus)
jus$P7con[P7=="  Muy de acuerdo "] <- 0
jus$P7con[P7=="  De acuerdo " ] <- 1
jus$P7con[P7=="  En desacuerdo "] <- 3
jus$P7con[P7=="  Muy en desacuerdo "] <- 4
jus$P7con[P7=="  Ni de acuerdo ni en desacuerdo "] <- 2
detach(jus)

# Sumar las columnas de transformación 
jus$legalidad <- rowSums(jus[,310:313], na.rm = TRUE)

# K-means
tranclus<- kmeans(jus$legalidad, 3, nstart = 999)
jus$tcl_legal<-tranclus$cluster

# Reajustar centrinos
attach(jus)
jus$cl_legal[tcl_legal==3] <- "Media legalidad"
jus$cl_legal[tcl_legal==2] <- "Baja legalidad"
jus$cl_legal[tcl_legal==1] <- "Alta legalidad"
detach(jus)

# Borrar objetos innecesarios y volver a declarar el diseño de la encuesta
rm(tranclus)
rm(design)
library(survey)
design = svydesign(id=~jus$UPM,strata=~jus$REGIÓN, 
                   weights=~jus$PONDI1, data=jus)

#Validación 
jus%>%group_by(P14)%>%summarize(puntuacion=mean(legalidad))
vtree::vtree(jus, "legalidad cl_legal")

pander(na.omit(prop.table(na.omit(svytable(~cl_legal+P5, design)),1))*100)
pander(na.omit(prop.table(na.omit(svytable(~cl_legal+P6, design)),1))*100)
pander(na.omit(prop.table(na.omit(svytable(~cl_legal+P7, design)),1))*100)
pander(na.omit(prop.table(na.omit(svytable(~cl_legal+P14, design)),1))*100)

#  Indicé cultura de confianza en la justicia----

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
jus$P19con[P19=="  Considera que la persona es, efectivamente, culpable "] <- 4
jus$P19con[P19=="  Tiene dudas de la culpabilidad de la persona " ] <- 3
jus$P19con[P19=="  Tiene seguridad de que la persona no es culpable "] <- 1
jus$P19con[P19=="  Piensa que no hay manera de saber si es culpable o no lo es "] <- 0
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
jus$confijusticia<- rowSums(jus[,318:325], na.rm = TRUE)

# K-means
tranclus<- kmeans(jus$confijusticia, 3, nstart = 999)
jus$tcl_confijusticia<-tranclus$cluster

# Reajustar centrinos
attach(jus)
jus$cl_confijusticia[tcl_confijusticia==2] <- "Media confianza"
jus$cl_confijusticia[tcl_confijusticia==1] <- "Baja confianza"
jus$cl_confijusticia[tcl_confijusticia==3] <- "Alta confianza"
detach(jus)

# Borrar objetos innecesarios y volver a declarar el diseño de la encuesta
rm(tranclus)
rm(design)
library(survey)
design = svydesign(id=~jus$UPM,strata=~jus$REGIÓN, 
                   weights=~jus$PONDI1, data=jus)

#Validación 
jus%>%group_by(P23)%>%summarize(puntuacion=mean(confijusticia))
vtree::vtree(jus, "confijusticia cl_confijusticia")

#  Tipologías----
attach(jus)
jus$P3_1con[P3_1=="  Iglesia  "] <- "Religiosa"
jus$P3_1con[P3_1=="  Familia  "] <- "Social"
jus$P3_1con[P3_1=="  La ley  "] <- "Jurídica"
jus$P3_1con[P3_1=="  El gobierno  "] <- "Jurídica"
jus$P3_1con[P3_1=="  Uno mismo  "] <- "Moral"
detach(jus)

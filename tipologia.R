
# Transformar las variables
attach(jus)
jus$P2_1con[P2_1==" No está dispuesto "] <- "Social"
jus$P2_2con[P2_2==" No está dispuesto "] <- "Social"
jus$P2_3con[P2_3==" No está dispuesto "] <- "Religosa"
jus$P2_4con[P2_4==" No está dispuesto "] <- "Jurídica"
detach(jus)

attach(jus)
jus$P3_1con[P3_1=="  Iglesia  "] <- "Religiosa"
jus$P3_1con[P3_1=="  Familia  "] <- "Social"
jus$P3_1con[P3_1=="  La ley  "] <- "Jurídica"
jus$P3_1con[P3_1=="  El gobierno  "] <- "Jurídica"
jus$P3_1con[P3_1=="  Uno mismo  "] <- "Moral"
detach(jus)

attach(jus)
jus$P3_2con[P3_2=="  Iglesia  "] <- "Religiosa"
jus$P3_2con[P3_2=="  Familia  "] <- "Social"
jus$P3_2con[P3_2=="  La ley  "] <- "Jurídica"
jus$P3_2con[P3_2=="  El gobierno  "] <- "Jurídica"
jus$P3_2con[P3_2=="  Uno mismo  "] <- "Moral"
detach(jus)

attach(jus)
jus$P4con[P4=="  Para no ser criticado por los demás  "] <- "Social"
jus$P4con[P4=="  Porque es un deber moral "] <- "Moral"
jus$P4con[P4=="  Porque cumplir la ley nos beneficia a todos  "] <- "Social"
jus$P4con[P4=="  Para evitar daños a mi familia y amistades "] <- "Social"
jus$P4con[P4=="  Para evitar castigos "] <- "Jurídica"
detach(jus)


# Sumar las columnas de transformación 

for(jus$ID in jus$ID){
  a<-c(jus$P3_2con,jus$P3_1con, jus$P4con)
  b<-c(jus$ID, jus$ID, jus$ID)
}

df <- data.frame(a, b)

calculate_mode <- function(x) {
  uniqx <- na.omit(unique(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

library(dplyr)
c <- df %>% 
  group_by(b) %>% 
  summarise(moda = calculate_mode(a))

rm(design)
library(survey)
design = svydesign(id=~jus$UPM,strata=~jus$REGIÓN, 
                   weights=~jus$PONDI1, data=jus)


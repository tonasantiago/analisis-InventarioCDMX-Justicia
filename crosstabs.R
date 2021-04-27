
# Reconvertir Ingreso -----------------------------------------------------

attach(jus)
jus$S111B[S11==" De 0 a 2 SM ($0 hasta $6,161 al mes) "] <- " De 0 a 2 SM ($0 hasta $6,161 al mes) "
jus$S111B[S11==" De 2 a 4 SM De ($6,162 hasta $12,322 al mes) "] <- " De 2 a 4 SM De ($6,162 hasta $12,322 al mes) "
jus$S111B[S11==" De 4 a 6 SM De ($12,323 hasta $18,482 al mes) "] <- " De 4 a 6 SM De ($12,323 hasta $18,482 al mes) "
jus$S111B[S11==" De 6 a 8 SM De ($18,483 hasta $24,643 al mes) "] <- " Más de 6 SM $18,483 o más mensuales) "
jus$S111B[S11==" De 8 a 10 SM De ($24,644 hasta $30,804 al mes) "] <- " Más de 6 SM $18,483 o más mensuales) "
jus$S111B[S11==" De 10 a 12 SM De ($30,805 hasta $36,965 al mes) "] <- " Más de 6 SM $18,483 o más mensuales) "
jus$S111B[S11==" De 12 a 14 SM De ($36,966 hasta $43,126 al mes) "] <- " Más de 6 SM $18,483 o más mensuales) "
jus$S111B[S11==" De 14 a 16 SM De ($43,127 hasta $49,286 al mes) "] <- " Más de 6 SM $18,483 o más mensuales) "
jus$S111B[S11==" De 16 a 18 SM De ($49,287 hasta $55,447 al mes) "] <- " Más de 6 SM $18,483 o más mensuales) "
jus$S111B[S11==" De 18 a 20 SM De ($55,448 hasta $61,608 al mes) "] <- " Más de 6 SM $18,483 o más mensuales) "
jus$S111B[S11==" Más de 20 SM ($61,609 o más mensuales) "] <- " Más de 6 SM $18,483 o más mensuales) "
jus$S111B[S11==" NS "] <- NA
jus$S111B[S11==" NC "] <- NA
detach(jus)

rm(design)
design = svydesign(id=~jus$UPM,strata=~jus$REGIÓN, 
                   weights=~jus$PONDI1, data=jus)


# Excpeción a la ley --------------------------------------------------------
    library(pander)
    pander(round(prop.table(svytable(~cl_ley, design=design))*100,1))
    pander((na.omit(prop.table(na.omit(svytable(~SEXO+cl_ley, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~EDAD+cl_ley, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~ESCOLARIDAD+cl_ley, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~S111B+cl_ley, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~P49+cl_ley, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~P9+cl_ley, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~cl_conflicto+cl_ley, design)),1)))*100)


# Confianza operadores---------------------------------------------------------------

    library(pander)
    pander(round(prop.table(svytable(~cl_justicia, design=design))*100,1))
    pander((na.omit(prop.table(na.omit(svytable(~SEXO+cl_justicia, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~EDAD+cl_justicia, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~ESCOLARIDAD+cl_justicia, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~S111B+cl_justicia, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~P49+cl_justicia, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~P9+cl_justicia, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~cl_conflicto+cl_justicia, design)),1)))*100)
    

# Resoluciones ------------------------------------------------------------

    library(pander)
    pander(round(prop.table(svytable(~cl_resoluciones, design=design))*100,1))
    pander((na.omit(prop.table(na.omit(svytable(~SEXO+cl_resoluciones, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~EDAD+cl_resoluciones, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~ESCOLARIDAD+cl_resoluciones, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~S111B+cl_resoluciones, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~P49+cl_resoluciones, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~P9+cl_resoluciones, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~cl_conflicto+cl_resoluciones, design)),1)))*100)

# Autocomposición ---------------------------------------------------------

    library(pander)
    pander(round(prop.table(svytable(~cl_conflicto, design=design))*100,1))
    pander((na.omit(prop.table(na.omit(svytable(~SEXO+cl_conflicto, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~EDAD+cl_conflicto, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~ESCOLARIDAD+cl_conflicto, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~S111B+cl_conflicto, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~P49+cl_conflicto, design)),1)))*100)
    pander((na.omit(prop.table(na.omit(svytable(~P9+cl_conflicto, design)),1)))*100)
    
    
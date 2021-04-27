sumar <- select(jus,P5con:P7con)
jus$expley <- rowSums(sumar, na.rm = TRUE)

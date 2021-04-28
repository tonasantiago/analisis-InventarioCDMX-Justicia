# Análisis de la Encuesta sobre Seguridad y Justicia del proyecto "Invetario CDMX. Presente y futuro de su gente"

Este proyecto contiene el script necesario para realizar los análisis del documento *La influencia de las percepciones de los habitantes de la CDMX en el uso y aceptación social de los mecanismos alternativos de solución de controversias en materia penal* de Daniel H. Gaona.

El proyecto busca relacionar las percepeciones de los habitantes de la Ciudad de México sobre los nuevos mecanismos de impartición de justicia.


El script realiza las siguientes operaciones: 

1. Con el conjunto de datos proporcionados, junto con el diseño muestral, se crea una nueva variable ("UPM") que servirá para declarar el diseño muestral.
2. Al ser datos que provienen de un diseño muestral se necesita declara dicho diseño para hacer inferencias válidas, para eso su usa el paquete  ```survey 
```.
3. Se guardan algunas frecuencias simples de interés para el estudio
4. Se crean algunos índices de interés para el estudio
5. Se cruzan algunos índices con algunas otras preguntas del cuestionario de la encuesta así como con algunas datos sociodemográficos 

Los resultados finales, así como al discusión teórica que sustenta estos análisis puede encontrarse en: *La influencia de las percepciones de los habitantes de la CDMX en el uso y aceptación social de los mecanismos alternativos de solución de controversias en materia penal* / tesis que para optar por el grado de Maestro en Derecho, presenta Eduardo Daniel Hernández Gaona ; tutor principal de tesis Carlos Silva Forné --  2021


### Replication Package for "Income Contingent University Loans: Policy Design and an Application to Spain"
#### by Antonio Cabrales, Maia Güell, Rocio Madera, and Analia Viola


Go to the end for instructions on how to obtain the data.

### MCVL-Datos personales+afiliados+cotización+fiscales
**Prueba año 2012**

Los ficheros de la MCVL

Utilizamos 4 ficheros de la MCVL con la siguiente información:
•	Fichero personal: información sociodemográfica de cada individuo (sexo, nacionalidad, provincia y país de nacimiento, nivel educativo, fecha de nacimiento y fallecimiento además del sexo y fecha de nacimiento de cada conviviente, etc.).
•	Fichero afiliados:  altas y bajas de cada contrato y el coeficiente de parcialidad (como base para construir la variable horas trabajadas),  el régimen de cotización como base para generar la situación laboral de cada individuo (asalariado, autónomo, parado, etc.) y el tipo de contrato como base para construir el porcentaje de tiempo en contrato temporal o indefinido.
•	Fichero cotización: bases de cotización total (anual) tanto para los asalariados como para los autónomos. Así tenemos todos los ingresos anuales para cada individuo, principalmente para poder distinguir a aquellos individuos que no declaran y no figuran en el fichero fiscal.
•	Fichero fiscal: ingresos anuales por cada categoría (clave de percepción) y los ingresos totales (todas las claves de percepción) de cada individuo.


Preparación fichero personal

Objetivo: Necesitamos las variables sociodemográficas de los individuos. Las variables que contiene este fichero (datos originales sin ninguna modificación) son:
-	Identificador de la persona (ident)
-	Sexo (sexo)
-	Nacionalidad (naciona)
-	Provincia de nacimiento (provnac)
-	Provincia de primera afiliación (provnaf)
-	Domicilio de residencia habitual (dom)
-	País de nacimiento (paisnac)
-	Nivel educativo alcanzado (educa)
-	Fecha de nacimiento (fnacim)
-	Fecha de fallecimiento (ffallec)
-	Sexo de los convivientes (sexo1 para el primer conviviente y así sucesivamente hasta sexo10, décimo conviviente).
-	Fecha de nacimiento de los convivientes (fnacim1 para el primer conviviente y así sucesivamente hasta fnacim10, décimo conviviente).

Preparación fichero afiliados

Objetivo: queremos generar un fichero con 4 variables claves relacionadas con el tiempo trabajado por cada individuo:
-	Identificador de la persona (1 observación por persona) (ident)
-	Número total de horas trabajadas (horastotales)
-	Porcentaje de tiempo trabajado en contrato temporal (Ptemporal)
-	Porcentaje de tiempo que ha trabajado como autónomo respecto del tiempo total trabajado (Pautonomo).


Procedimiento:

1)	Unimos todos los ficheros de afiliados de la MCVL 2012 (los datos de afiliados se encuentran distribuidos en 3 ficheros diferentes, por eso es necesario unirlos en un único fichero) creando MCVLafiliados12.dta.  
2)	Generamos la situación laboral de cada observación y una dummy de asalariados, autónomo, parado y convenio especial (resto) a partir de la variable régimen de cotización y tipo de relación laboral para la categoría de parados (luego al final estas variables generadas no aparecen en el fichero final).
3)	Nos quedamos solamente con las observaciones (contratos) de asalariados y autónomos.
4)	Queremos disponer de los contratos que están activos en 2011 porque la declaración de la renta del fichero fiscal 2012 se ha hace respecto del año anterior.
5)	Los contratos que empezaron antes de 2011 les pondremos como fecha inicial 1/1/2011 (FALTA11). Y con los contratos que terminan después de 2011 les pondremos como fecha final 31/12/2011 (FBAJA11). Esto lo hacemos para contabilizar el número de días de 2011 del contrato.
6)	Los contratos que no cumplen el punto 5 les dejamos su fecha de alta y/o baja original en las mismas variables FALTA11 y FBAJA11.
7)	Cuando restamos FALTA11 y FBAJA11 obtenemos el número de días trabajados durante el año 2011 (dias).
8)	Generamos la variable porcentaje que representa el porcentaje de la jornada completa. Por ejemplo, si es igual a 100 se trabaja jornada completa y si es igual a 50 se trabaja media jornada. Esta variable se genera a partir de la variable coef (que aparece en el fichero original de afiliados) y que significa el coeficiente de parcialidad que va de 0 (jornada completa) a 999 (mínimo de horas trabajadas, aproximadamente 1 hora a la semana).
9)	Generamos la variable horas que es el resultado de trabajar 8 horas al día multiplicado por los días de trabajo (dias) por el porcentaje de la jornada completa que trabaje (porcentaje).
10)	Para tener en cuenta los festivos, fines de semana y vacaciones calculamos una variable que se llama ponderacion que se obtiene de dividir el número máximo de horas legales (estipulado en 1750 ) por el número de horas totales que obtiene en la variable anterior un trabajador full time con contrato indefinido en un año (365 días por 8 horas= 2920). La variable ponderacion arroja un valor de 0,60.
11)	Hemos llamado horaslegales al número de horas trabajadas (horas) por la ponderacion para tener en cuenta los días de descanso (festivos y vacaciones). Si los contratos duran menos de 1 mes no hacemos esta operación dado que antes del mes de trabajo no hay derecho a vacaciones . Por otro lado, si la suma de horas totales legales supera el máximo (100%) acotaremos a 100% esta variable .
12)	Sumamos todas las horas legales (horastotales) de todos los contratos para cada persona. 
13)	Para calcular el porcentaje de tiempo empleado en contrato temporal (Ptemporal) debemos reestructurar primero todos los tipos de contratos que aparecen en este fichero para poder clasificarlo en indefinido o temporal (se crea la variable tipo). Para lograrlo se utiliza una guía del Ministerio de Empleo y Seguridad Social  y se realiza la búsqueda online sobre las diversas regulaciones o real decretos para conocer la modalidad de contratación.
14)	La variable tipo también admite otra categoría “no consta” (se encuentra estipulado así en la guía MCVL del fichero de afiliados y figura como “000”). En este caso, hemos decidido que en aquellos casos que no conste el tipo de contrato (“000”) pero a la vez llevan más de 5 años trabajando, asumimos que es un contrato indefinido.
15)	De esta manera, construimos las variables tiempotemp y tiempoindef que significan respectivamente el tiempo de trabajo en el año 2011 para cada tipo de contrato. Luego, sumamos el tiempo total de todos los contratos temporales y lo mismo para los contratos indefinidos (tiempototaltemp y tiempototalindef, respectivamente). Construimos otra variable que se llame tiempo que toma el valor de la variable tiempototaltemp si es un contrato temporal o bien toma el valor de la variable tiempototalindef si es un contrato indefinido. Finalmente construimos la variable tiempototal que es la suma de la variable tiempo.
16)	Con estas variables construidas se puede proceder a generar la variable porcentaje de tiempo empleado en contrato temporal (Ptemporal) que es el resultado de multiplicar la variable tiempototaltemp por 100 y posteriormente dividirlo por la variable tiempototal. 
17)	Por otro lado, realizamos el mismo procedimiento para calcular el porcentaje de tiempo empleado como autónomo (Pautonomo). Primero construimos la variable tiempoautonomo al restar FALTA11 y FBAJA11 en el caso de ser autónomo. Posteriormente,  construimos la variable tiempoasalariado restando también FALTA11 y FBAJA11 en el caso de ser asalariado. Sumamos todas las observaciones para tener el tiempo total de autónomos (tiempototalautonomo) y el tiempo total de asalariados (tiempototalasalariado). De esta manera se puede obtener al sumar ambas variables el tiempo total trabajado (tiempototal2). Construimos finalmente la variable porcentaje de tiempo autónomo (Pautonomo) realizando la división entre el tiempo total de autónomo (tiempototalautonomo) sobre el tiempo total trabajado (tiempototal2).
18)	Dejamos una observación por persona.


Preparación fichero cotización

Objetivo: queremos generar un fichero con 3 variables claves para obtener la base de cotización anual del año 2011 para asalariados y autónomos. Esto sirve para tener información salarial de los que no declaran renta y por tanto no figuran en el fichero fiscal. De todos modos, dejaremos la información de todas las personas, no solo las que no declaran renta en el fichero fiscal.
-	Identificador de la persona (1 observación por persona) (ident)
-	Base de cotización anual por cuenta ajena (Baseajena)
-	Base de cotización anual por cuenta propia (Basepropia)

Procedimiento:

1)	Unimos todos los ficheros de cotización de la MCVL 2012 por cuenta ajena (son 12 ficheros originales que simplificamos en uno único) y por cuenta propia, creando MCVcotiztodos12.dta.
2)	Mantenemos solo los datos del 2011.
3)	Dejamos el identificador y las dos variables de base de cotización.
4)	Nos quedamos con una observación por persona.


Preparación fichero fiscal 

1)	Aquí sumamos todos los ingresos por cada clave de percepción (A hasta la L) para cada individuo.
2)	También sumamos todos los ingresos de cada clave para tener el ingreso total de cada individuo.
3)	Dejamos solamente las variables de ingreso por cada categoría y el ingreso total.
4)	Nos quedamos con una observación por persona.


Preparación fichero final

1)	Al fichero fiscal le añadimos el fichero de datos personales, el de afiliados y el cotización. 
2)	Eliminamos aquellas observaciones donde no haya información ni de ingresos totales, ni de bases de cotización ajena ni propias (count if Itotal==. & Baseajena==. & Basepropia==.)


Nota: Al final también hemos eliminado los datos de convivientes (sexo y fecha de nacimiento).


### How to obtain the MCVL data

There are two versions of the MCVL data: with and without income tax data. For the first one it is referred as the MCVL CDF and for the latter as the MCVL SDF. 

Obtaining the MCVL data requires completing two separated forms, namely:

1.	“Condiciones de utilización” which specify the terms and conditions of use for the MCVL CDF and the MCVL SDF.
2.	“Ficha de usuario” which contains information of:
a.	the user, who requests the information and is responsible for the compliance with the terms and conditions. It can be a physical person or an entity, such as a university department.
b.	the technical project manager, the individual involved in processing the data; if it is the same as the user, it can be left blank.
c.	description of the project.

These forms can be downloaded in the following link:
http://www.seg-social.es/wps/portal/wss/internet/EstadisticasPresupuestosEstudios/Estadisticas/EST211/1459, accessed December 18, 2018.

Note that for each edition of the MCVL and for every version (CFD or SDF) is needed a different form. Hence, for this paper we had to submit 11 forms, one for each edition of the MCVL CDF (2004 to 2013) plus the “ficha de usuario” form.

The forms properly completed and signed must be sent to:

Dirección General de Ordenación de la Seguridad Social
Subdirección General de Seguimiento Económico
C/Jorge Juan 59
28001 Madrid

Once the application is approved, the user receives a DVD or CD with the MCVL data.












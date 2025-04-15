#Retail MM

En esta solución se aplican los principios de Paralelismo dentro de SAP, dividiendo los procesos entre los recursos disponibles para optimizar al maximo.
Funciones principales para el procesamiento:
- SPBT_INITIALIZE
- SPBT_GET_CURR_RESOURCE_INFO

En esta solucion se obtiene informacion de las tablas y al momento de procesarla se crea una funcion que se llama de forma remota (de fondo) para terminar mostrando los registros.

Esto ha incrementado el rendimiento del programa hasta en un 40%.
(Esto es mas visible cuanto mayor sea el volumen de informacion y de acuerdo a los recursos en el sistema disponible)

* Diccionario de datos *
Estructuras:
ZTS_A700_RDISP
ZTS_KUNNR_RDISP
ZTS_MARAX_RDIPS
ZTS_MARD_RDISP
ZTS_PART_RDISP

Tipo tabla:
ZTT_A700_RDISP
ZTT_KUNNR_RDISP
ZTT_MARAX_RDIPS
ZTT_MARD_RDISP
ZTT_PART_RDISP

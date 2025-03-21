*------------------------------------------------------------------------*
* Programa   : zppre_pendientes_surtir                                   *
* Descripción: Reporte para consultar la información detallada           *
* de los Pendientes Restantes por surtir y sus materiales.               *
* Programador: David Navoa Acevedo                                       *
* Fecha      : 21.10.2024                                                *
*------------------------------------------------------------------------*
REPORT zppre_pendientes_surtir.

INCLUDE zppre_pendientes_surtir_01.
INCLUDE zppre_pendientes_surtir_02.

INITIALIZATION.
  CREATE OBJECT: obj_catalogo,
                 obj_reporte.

START-OF-SELECTION.

  REFRESH obj_catalogo->lt_catalogo.

  "Obtener informacion
  obj_catalogo->get_data( ).
  "Mostrar informacion
*  obj_reporte->m_show_alv_factory( obj_catalogo->lt_catalogo ).
  obj_reporte->m_show_alv( obj_catalogo->lt_catalogo ).

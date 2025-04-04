*------------------------------------------------------------------------*
* Programa   : zmmre_clasificacion_atributos                             *
* Descripción: Reporte para consultar la información detallada
* del maestro del material con sus diferentes carácteristicas y atributos*
* Programador: David Navoa Acevedo                                       *
* Fecha      : 20.11.2023                                                *
*------------------------------------------------------------------------*

REPORT zmmre_clasificacion_atributos.

INCLUDE zmmre_clasificacion_atb_ss.
INCLUDE zmmre_clasificacion_atb_cl.

"Evento de Inicializacion

INITIALIZATION.
  CREATE OBJECT: ob_catalog,
                 ob_report.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tprod-low.

  PERFORM f_sugestion_low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tprod-high.

  PERFORM f_sugestion_high.

  "Evento de Inicio de Seleccion

START-OF-SELECTION.

  IF p2_check EQ abap_false AND p_check EQ abap_false.
    MESSAGE 'Seleccione una de las dos opciones Generico / Variantes' TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.

  ob_catalog->m_get_data( ).
  IF  <gt_dyntable> IS ASSIGNED.
    IF <gt_dyntable> IS NOT INITIAL.
      ob_report->m_show_alv( ).
    ELSE.
      MESSAGE 'No hay datos con los parametros seleccionados' TYPE 'I'.
    ENDIF.
  ENDIF.

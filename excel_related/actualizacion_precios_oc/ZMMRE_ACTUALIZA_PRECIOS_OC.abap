*&-----------------------------------------------------------------------*
*& Report ZMMRE_ACTUALIZA_PRECIOS_OC
*------------------------------------------------------------------------*
* Programa   : ZMMRE_ACTUALIZA_PRECIOS_OC                                *
* Descripción: Reporte para cambiar precios en Orden de Compra           *
* Programador: David Navoa Acevedo                                       *
* Fecha      : 06.06.2024                                                *
*------------------------------------------------------------------------*
REPORT zmmre_actualiza_precios_oc.

INCLUDE zmmre_actualiza_precios_oc_01.
INCLUDE zmmre_actualiza_precios_oc_02.
INCLUDE zmmre_actualiza_precios_oc_0f.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

START-OF-SELECTION.

  CREATE OBJECT obj_catalog.
  CREATE OBJECT obj_reporte.

  obj_catalog->m_update_data( ).
  obj_catalog->m_update_po( ).

  obj_reporte->m_show_alv( obj_catalog->m_get_data( ) ).

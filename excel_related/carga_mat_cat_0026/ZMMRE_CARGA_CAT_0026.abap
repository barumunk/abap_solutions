*&---------------------------------------------------------------------*
*& Report ZMMRE_CARGA_CAT_0026
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmre_carga_cat_0026.

INCLUDE zmmre_carga_cat_0026_top.
INCLUDE zmmre_carga_cat_0026_c01.


"Evento de inicializacion

INITIALIZATION.
  CREATE OBJECT: ob_catalog,
                 ob_report.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

  "Evento de START-OF-SELECTION

START-OF-SELECTION.

  "FREE MEMORY.
  ob_catalog->m_get_data( ).
  ob_catalog->m_fill_data( ).
  IF  ob_catalog->gt_alv IS NOT INITIAL.
    ob_report->m_show_alv( ).
  ENDIF.

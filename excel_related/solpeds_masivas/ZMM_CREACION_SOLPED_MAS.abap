*&---------------------------------------------------------------------*
*& Report ZMM_CREACION_SOLPED_MAS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_creacion_solped_mas.

INCLUDE zmm_creacion_solped_mas_01.
INCLUDE zmm_creacion_solped_mas_02.

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


  obj_catalog->m_procesar_layout( ).
  obj_catalog->m_obtener_data( obj_catalog->lt_catalogo ).

  obj_reporte->m_show_alv( it_catalogo = obj_catalog->lt_catalogo_alv ).

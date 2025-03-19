*&---------------------------------------------------------------------*
*& Report ZMM_GENERACION_MASIVA_PH
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_generacion_masiva_ph.

INCLUDE zmm_generacion_masiva_ph_01.
INCLUDE zmm_generacion_masiva_ph_02.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

START-OF-SELECTION.

  CREATE OBJECT obj_catalogo.

  CLEAR gv_check.

  obj_catalogo->m_process( ).

*&---------------------------------------------------------------------*
*& Include          ZMM_CREACION_SOLPED_MAS_01
*&---------------------------------------------------------------------*

INCLUDE zmm_creacion_solped_mas_c00.

*------------------------------------------------------------------------*
* DISEÑO PANTALLA DE SELECCIÓN
*------------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS:  p_file TYPE localfile OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

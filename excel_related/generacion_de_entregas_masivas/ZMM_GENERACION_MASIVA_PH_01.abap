*&---------------------------------------------------------------------*
*& Include          ZMM_GENERACION_MASIVA_PH_01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   e_group_opened.
*       message texts
TABLES: t100.

INCLUDE zmm_generacion_masiva_ph_c00.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS:  p_file TYPE localfile OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

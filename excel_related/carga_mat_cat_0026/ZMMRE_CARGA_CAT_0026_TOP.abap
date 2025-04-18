*&---------------------------------------------------------------------*
*& Include          ZMMRE_CARGA_CAT_0026_TOP
*&---------------------------------------------------------------------*
INCLUDE zmmre_carga_cat_0026_c00.

"TABLES: bkpf.

DATA : ob_catalog TYPE REF TO lcl_catalogo,
       ob_report  TYPE REF TO lcl_report.


************************************************************************
*  Pantalla de Selección                                               *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE localfile OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_carg RADIOBUTTON GROUP tipo DEFAULT 'X', "Carga
              p_sobr RADIOBUTTON GROUP tipo. "Sobreescribir
SELECTION-SCREEN END OF BLOCK c1.

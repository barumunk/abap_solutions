*&---------------------------------------------------------------------*
*& Include          ZMMRE_ACTUALIZA_PRECIOS_OC_01
*&---------------------------------------------------------------------*
* Programa   : ZMMRE_ACTUALIZA_PRECIOS_OC                                *
* Descripción: Reporte para cambiar precios en Orden de Compra           *
* Programador: David Navoa Acevedo                                       *
* Fecha      : 06.06.2024                                                *
*------------------------------------------------------------------------*

INCLUDE zmmre_actualiza_precios_oc_c00.
INCLUDE zmmre_actualiza_precios_oc_m00.

*------------------------------------------------------------------------*
* DISEÑO PANTALLA DE SELECCIÓN
*------------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  "SELECT-OPTIONS:  s_lifnr FOR ekko-lifnr OBLIGATORY.
  PARAMETERS:  p_file TYPE localfile OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

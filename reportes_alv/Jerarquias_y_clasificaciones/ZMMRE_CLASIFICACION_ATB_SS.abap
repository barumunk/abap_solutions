*&---------------------------------------------------------------------*
*& Include          ZMMRE_CLASIFICACION_ATB_SS
*&---------------------------------------------------------------------*
*------------------------------------------------------------------------*
* Programa   : zmmre_clasificacion_atributos                             *
* Descripción: Reporte para consultar la información detallada           *
* del maestro del material con sus diferentes carácteristicas y atributos*
* Programador: David Navoa Acevedo                                       *
* Fecha      : 20.11.2023                                                *
*------------------------------------------------------------------------*

INCLUDE zmmre_clasificacion_atb_c00.

*------------------------------------------------------------------------*
* DISEÑO PANTALLA DE SELECCIÓN
*------------------------------------------------------------------------*

TABLES: mara,klah, makt, t179t, maw1, wrf_fiber_codest, marm, fsh_seasons, fsh_seasons_mat.
FIELD-SYMBOLS:
  <gt_dyntable>     TYPE STANDARD TABLE,
  <gs_dyntable>,
  <gt_dyntable_aux> TYPE STANDARD TABLE,
  <gs_dyntable_aux>,
  <gv_fldval_aux>   TYPE any,
  <gv_fldval>       TYPE any.

DATA : ob_catalog TYPE REF TO lcl_catalogo,
       ob_report  TYPE REF TO lcl_reporte.
DATA: gv_string TYPE string.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:
  s_temp  FOR fsh_seasons-fsh_season, "OBLIGATORY, "Temporada
  s_gpo_a FOR mara-matkl," OBLIGATORY, "Grupo Articulos
  s_model FOR mara-normt," OBLIGATORY, "Modelo
  s_matrl FOR mara-matnr OBLIGATORY, "Material SAP
  s_cod_f FOR mara-ferth," OBLIGATORY, "Codigo Fabricante
  s_marcb FOR mara-lvorm," OBLIGATORY, "Marcado Borrado
  s_tprod FOR klah-class OBLIGATORY. "Tipo Producto

  SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

    PARAMETERS: p_fiscal AS CHECKBOX.

  SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS: p2_check AS CHECKBOX DEFAULT 'X',
              p_check  AS CHECKBOX.

SELECTION-SCREEN: END OF BLOCK b2.

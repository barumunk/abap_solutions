*----------------------------------------------------------------------*
* DISEÑO PANTALLA DE SELECCIÓN
*----------------------------------------------------------------------*
"
TABLES: mara, fsh_seasons_mat, mard, marc, a700.

DATA: gv_completo         TYPE i,
      gv_tareas_paralelas TYPE i,
      gv_destino          TYPE rfc_sysid,
      gv_nombre           TYPE rzllitab-classname VALUE 'parallel_generators'.

FIELD-SYMBOLS: <gt_datos> TYPE table.

DATA: lt_marax        TYPE STANDARD TABLE OF zts_marax_rdips,
      lt_mard         TYPE ztt_mard_rdisp,
      lt_alv          TYPE TABLE OF zts_salida_reportedisp,
      ti_datos_rel    TYPE STANDARD TABLE OF zts_pos_pps,
      lt_a700         TYPE STANDARD TABLE OF zts_a700_rdisp,
      lt_twewt        TYPE TABLE OF twewt,
      lt_kunnr        TYPE STANDARD TABLE OF zts_kunnr_rdisp,
      lt_fsh_sc_vconv TYPE TABLE OF fsh_sc_vconv,
      lt_part         TYPE STANDARD TABLE OF zts_part_rdisp.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:
  s_vkorg FOR a700-vkorg OBLIGATORY, "Org. de ventas.
  s_werks FOR marc-werks OBLIGATORY MATCHCODE OBJECT zh_repdisp_centro, "Centro
  s_lgort FOR mard-lgort, "Almacén
  s_matkl FOR mara-matkl, "Grupo de articulos
  s_matnr FOR mara-matnr, "Materiales
  s_spart FOR mara-spart, "Sector
  s_season FOR fsh_seasons_mat-fsh_season. "Temporada

  PARAMETERS:
  p_stesp AS CHECKBOX DEFAULT ''.

SELECTION-SCREEN: END OF BLOCK b1.

*------------------------------------------------------------------------*
* Programa   : zppre_pendientes_surtir                                   *
* Descripción: Reporte para consultar la información detallada           *
* de los Pendientes Restantes por surtir y sus materiales.               *
* Programador: David Navoa Acevedo                                       *
* Fecha      : 21.10.2024                                                *
*------------------------------------------------------------------------*

TABLES: vbak, vbap, mara.


INCLUDE zppre_pendientes_surtir_00.

**********************************
** Screen
**********************************

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS s_vkorg FOR vbak-vkorg.
  SELECT-OPTIONS s_werks FOR vbap-werks.
  SELECT-OPTIONS s_vtweg FOR vbak-vtweg.
  SELECT-OPTIONS s_spart FOR vbak-spart.
  SELECT-OPTIONS s_auart FOR vbak-auart OBLIGATORY.
  SELECT-OPTIONS s_brand FOR mara-brand_id.
  SELECT-OPTIONS s_matkl FOR vbap-matkl.
  SELECT-OPTIONS s_matnr FOR vbap-matnr OBLIGATORY.
  SELECT-OPTIONS s_kunnr FOR vbak-kunnr.
  SELECT-OPTIONS s_fsh_6 FOR vbak-fsh_kvgr6.
  SELECT-OPTIONS s_vdatu FOR vbap-vdatu_ana.
  SELECT-OPTIONS s_vbeln FOR vbak-vbeln.
  SELECT-OPTIONS s_season FOR vbap-fsh_season.
SELECTION-SCREEN END OF BLOCK b0.

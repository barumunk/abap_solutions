*&---------------------------------------------------------------------*
*& Include          ZMM_CREACION_SOLPED_MAS_C00
*&---------------------------------------------------------------------*

TABLES: eban.

CLASS: lcl_catalogo DEFINITION DEFERRED.
CLASS: lcl_reporte  DEFINITION DEFERRED.
DATA: obj_catalog TYPE REF TO lcl_catalogo.
DATA: obj_reporte TYPE REF TO lcl_reporte.

TYPES: BEGIN OF ty_item,
         matnr_o TYPE matnr,
         code    TYPE string,
         bednr   TYPE bednr,
         banfn   TYPE banfn,
         bsart   TYPE bsart,
         epstp   TYPE epstp,
         matnr   TYPE matnr,
         eeind   TYPE eeind,
         werks   TYPE werks_d,
         lgort   TYPE lgort_d,
         ekgrp   TYPE ekgrp,
         afnam   TYPE afnam,
         ekorg   TYPE ekorg,
         reswk   TYPE reswk,
         menge   TYPE bstmg,
         preis   TYPE preis,
         waers   TYPE waers,
         netpr   TYPE netpr,
       END OF ty_item,

       BEGIN OF ty_catalogo_alv,
         icon(1) TYPE c,
         banfn   TYPE banfn, "Pedido
         bsart   TYPE bsart, "Clase Doc
         matnr   TYPE matnr, "Material
         eeind   TYPE eeind, "Fecha de entrega
         werks   TYPE werks_d, "Centro
         lgort   TYPE lgort_d, "Almacen
         reswk   TYPE reswk, "Centro suministrador
         ekgrp   TYPE ekgrp,
         afnam   TYPE afnam,
         ekorg   TYPE ekorg,
         menge   TYPE bstmg, "Cantidad
         bednr   TYPE bednr,
         mensage TYPE string, "Mensage
       END OF ty_catalogo_alv.

TYPES: tt_items TYPE STANDARD TABLE OF bapimereqitemimp WITH NON-UNIQUE DEFAULT KEY.
TYPES: tt_itemsx TYPE STANDARD TABLE OF bapimereqitemx WITH NON-UNIQUE DEFAULT KEY.

TYPES: BEGIN OF ts_request,
         matnr     TYPE matnr,
         code      TYPE string,
         bsart     TYPE bsart,
         bednr     TYPE bednr,
         error(1)  TYPE c,
         mensage   TYPE string,
         lt_items  TYPE tt_items,
         lt_itemsx TYPE tt_itemsx,
       END OF ts_request.

DATA: gt_request TYPE TABLE OF ts_request.

CLASS lcl_catalogo DEFINITION.

  PUBLIC SECTION.

    DATA: lt_catalogo     TYPE TABLE OF ty_item,
          lt_excel_data   TYPE TABLE OF zalsmex_tabline,
          lt_catalogo_alv TYPE TABLE OF ty_catalogo_alv,
          lt_return_alv   TYPE TABLE OF bapiret2.

    METHODS: m_procesar_layout.

    METHODS: m_obtener_data
      IMPORTING
        i_data TYPE STANDARD TABLE,
      m_cargar_data
        IMPORTING
          i_bsart          TYPE bsart
          i_items          TYPE STANDARD TABLE
          i_itemsx         TYPE STANDARD TABLE
        CHANGING
          c_mensage        TYPE string
        RETURNING
          VALUE(rv_solped) TYPE bapimereqheader-preq_no,
      m_procesar_data
        IMPORTING
          i_request TYPE ts_request
        CHANGING
          c_solped  TYPE eban-banfn.

  PRIVATE SECTION.

    CONSTANTS: p_scol TYPE i VALUE '1',
               p_srow TYPE i VALUE '2',
               p_ecol TYPE i VALUE '5',
               p_erow TYPE i VALUE '2500',
               c_x    TYPE c VALUE 'X'.

    METHODS:
      m_date_format
        IMPORTING
          i_date         TYPE eeind
        RETURNING
          VALUE(rv_date) TYPE datum,
      m_get_matnr_identif
        IMPORTING
          i_matnr         TYPE matnr
        RETURNING
          VALUE(rv_matnr) TYPE matnr.

ENDCLASS.

CLASS lcl_reporte DEFINITION.

  PUBLIC SECTION.

    DATA: ob_alv TYPE REF TO cl_salv_table,
          ob_evt TYPE REF TO cl_salv_events_table.

    DATA: ob_columns TYPE REF TO cl_salv_columns_table,
          ob_column  TYPE REF TO cl_salv_column_table.

    METHODS:
      m_show_alv
        IMPORTING it_catalogo TYPE STANDARD TABLE.

    METHODS:
      m_set_pf_status
        CHANGING
          co_alv TYPE REF TO cl_salv_table. " Default Pf Status

ENDCLASS.

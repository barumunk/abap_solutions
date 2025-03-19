*&---------------------------------------------------------------------*
*& Include          ZMMRE_ACTUALIZA_PRECIOS_OC_C00
*&---------------------------------------------------------------------*
* Programa   : ZMMRE_ACTUALIZA_PRECIOS_OC                                *
* Descripción: Reporte para cambiar precios en Orden de Compra           *
* Programador: David Navoa Acevedo                                       *
* Fecha      : 06.06.2024                                                *
*------------------------------------------------------------------------*
TABLES: ekko, ekpo, konp.
DATA: ok_code  TYPE sy-ucomm.

CLASS: lcl_catalogo DEFINITION DEFERRED.
CLASS: lcl_reporte  DEFINITION DEFERRED.
DATA: obj_catalog TYPE REF TO lcl_catalogo.
DATA: obj_reporte TYPE REF TO lcl_reporte.

DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

CLASS lcl_catalogo DEFINITION.

  PUBLIC SECTION.

    DATA: lt_catalogo     TYPE ztt_catalogo_oc,
          lt_catalogo_alv TYPE ztt_catalogo_oc.

    METHODS: m_get_data RETURNING VALUE(rt_catalogo_alv) TYPE ztt_catalogo_oc,
      m_update_data,
      m_update_po.

  PRIVATE SECTION.

    CONSTANTS: p_scol TYPE i VALUE '1',
               p_srow TYPE i VALUE '2',
               p_ecol TYPE i VALUE '5',
               p_erow TYPE i VALUE '2500',
               c_x    TYPE c VALUE 'X'.


ENDCLASS.

CLASS lcl_reporte DEFINITION.

  PUBLIC SECTION.

    DATA: ob_alv TYPE REF TO cl_salv_table,
          ob_evt TYPE REF TO cl_salv_events_table.

    DATA: ob_columns TYPE REF TO cl_salv_columns_table,
          ob_column  TYPE REF TO cl_salv_column_table.

    METHODS:
      m_show_alv IMPORTING it_alv TYPE ztt_catalogo_oc.

    METHODS:
      m_set_pf_status
        CHANGING
          co_alv TYPE REF TO cl_salv_table. " Default Pf Status

ENDCLASS.

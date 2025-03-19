*&---------------------------------------------------------------------*
*& Include          ZMM_GENERACION_MASIVA_PH_C00
*&---------------------------------------------------------------------*

INCLUDE <icon>.

CLASS: lcl_catalogo DEFINITION DEFERRED.
CLASS: lcl_reporte  DEFINITION DEFERRED.
DATA: ok_code  TYPE sy-ucomm.
DATA: obj_catalogo  TYPE REF TO lcl_catalogo.
DATA: obj_reporte TYPE REF TO lcl_reporte.
DATA: gv_check      TYPE c.
TYPES:
  BEGIN OF ty_catalogo,
    index         TYPE string,
    pedido        TYPE vbeln_va,
    werks         TYPE werks_d,
    no_pedido     TYPE string,
    matkl         TYPE matkl,
    cliente       TYPE kunnr,
    destinatario  TYPE kunnr,
    material      TYPE matnr,
    variante      TYPE satnr,
    descripcion   TYPE string,
    talla         TYPE laeng,
    texto_cliente TYPE string,
    cantidad      TYPE i,
    fecha         TYPE datum,
  END OF ty_catalogo.

DATA: lt_data         TYPE TABLE OF ty_catalogo.

CLASS lcl_catalogo DEFINITION.

  PUBLIC SECTION.

    TYPES: tt_pedidos TYPE STANDARD TABLE OF ty_catalogo WITH KEY pedido werks no_pedido.

    TYPES: BEGIN OF ts_entrega,
             icon          TYPE icon_d,
             pedido        TYPE ebeln,
             werks         TYPE werks_d,
             no_pedido     TYPE ebeln,
             texto_cliente TYPE string,
             pedido_t      TYPE tt_pedidos,
             mensage       TYPE string,
           END OF ts_entrega.

    DATA: lt_entregas  TYPE TABLE OF ts_entrega,
          lt_alv_check TYPE TABLE OF ty_catalogo.

    METHODS:
      m_process,
      m_create.

  PRIVATE SECTION.

    DATA:
          lt_catalogo_alv TYPE TABLE OF ty_catalogo.

    CONSTANTS: c_scol       TYPE i VALUE '1',
               c_srow       TYPE i VALUE '2',
               c_ecol       TYPE i VALUE '5',
               c_erow       TYPE i VALUE '2500',
               c_x          TYPE c VALUE 'X',
               cp_puesto(4) TYPE c VALUE '0011'.
    METHODS:
      m_load_data
        IMPORTING
          i_werks   TYPE werks_d
          i_date    TYPE bapidlvcreateheader-due_date
          i_request TYPE STANDARD TABLE
          i_entrega TYPE ts_entrega,
      m_check_layout
        RETURNING
          VALUE(r_check) TYPE string,
      m_read_layout,
      m_process_layout,
      m_date_format
        IMPORTING
          i_date         TYPE char50
        RETURNING
          VALUE(rv_date) TYPE datum.


ENDCLASS.

CLASS lcl_reporte DEFINITION.

  PUBLIC SECTION.

    DATA: ob_alv TYPE REF TO cl_salv_table,
          ob_evt TYPE REF TO cl_salv_events_table.

    DATA: ob_columns TYPE REF TO cl_salv_columns_table,
          ob_column  TYPE REF TO cl_salv_column_table.

    METHODS:
      m_show_alv
        IMPORTING
          it_alv TYPE STANDARD TABLE,
      m_show_alv_check
        IMPORTING
          it_alv TYPE STANDARD TABLE.

    METHODS:
      m_set_pf_status
        CHANGING
          co_alv TYPE REF TO cl_salv_table. " Default Pf Status

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS       handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object
                e_interactive.

ENDCLASS.

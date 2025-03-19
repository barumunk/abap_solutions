*------------------------------------------------------------------------*
* Programa   : zppre_pendientes_surtir                                   *
* Descripción: Reporte para consultar la información detallada           *
* de los Pendientes Restantes por surtir y sus materiales.               *
* Programador: David Navoa Acevedo                                       *
* Fecha      : 21.10.2024                                                *
*------------------------------------------------------------------------*

CLASS: lcl_catalogo DEFINITION DEFERRED,
       lcl_alv      DEFINITION DEFERRED.

DATA: obj_catalogo TYPE REF TO lcl_catalogo,
      obj_reporte  TYPE REF TO lcl_alv.

CLASS lcl_catalogo DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_catalogo,
             centro             TYPE vbap-werks,
             almacen            TYPE vbap-lgort,
             temporada          TYPE vbap-fsh_season,
             gpo_articulos      TYPE vbap-matkl,
             doc_ventas         TYPE vbak-vbeln,
             auart              TYPE vbak-auart,
             ref_cli            TYPE vbkd-bstkd,
             num_pos_p          TYPE vbap-posnr,
             mat_gen            TYPE vbap-pmatn,
             material           TYPE vbap-matnr,
             ean                TYPE char40,
             descripcion        TYPE vbap-arktx,
             talla              TYPE vbap-wrf_charstc2,
             f_entrega          TYPE vbap-vdatu_ana,
             modelo             TYPE mara-normt,
             color              TYPE string,
             silueta            TYPE string,
             familia            TYPE mara-extwg,
             solicitante        TYPE kna1-kunnr,
             talla_mx           TYPE fsh_sc_entx8,
             nombre_clte        TYPE string,
             destinatario       TYPE kna1-kunnr,
             n_destinatario     TYPE string,
             s_especial_mat     TYPE mara-mstae,
             mes_ano            TYPE string,
             c_pedido           TYPE vbep-wmeng,
             c_confirmada       TYPE vbep-bmeng,
             c_pendiente        TYPE vbbe-omeng,
             c_no_conf          TYPE vbep-bmeng,
             p_wholesome(12)    TYPE p DECIMALS 2,
             p_unitario_ret(12) TYPE p DECIMALS 2,
             pend_entr_who(12)  TYPE p DECIMALS 2,
             pend_entr_ret(12)  TYPE p DECIMALS 2,
             c_asignada         TYPE arun_bdbs-alloc_qty,
             imp_asig_who(12)   TYPE p DECIMALS 2,
             imp_asig_ret(12)   TYPE p DECIMALS 2,
             unid_picking       TYPE lips-lfimg,
             imp_pick_who(12)   TYPE p DECIMALS 2,
             imp_pick_ret(12)   TYPE p DECIMALS 2,
             f_creacion_ped     TYPE vbak-erdat,
           END OF ty_catalogo,

           BEGIN OF ty_entrega,
             sign   TYPE ddsign,
             option TYPE ddoption,
             low    TYPE char200,
             high   TYPE char200,
           END OF ty_entrega.

    DATA: lt_catalogo TYPE TABLE OF ty_catalogo.

    METHODS:
      get_data.

  PRIVATE SECTION.

    METHODS:
      get_color
        IMPORTING
          ps_mara     TYPE mara
        CHANGING
          ps_catalogo TYPE ty_catalogo,
      get_silueta
        IMPORTING
          ps_mara     TYPE mara
        CHANGING
          ps_catalogo TYPE ty_catalogo,
      remover_ceros
        CHANGING
          ls_catalogo TYPE ty_catalogo.


ENDCLASS.

CLASS lcl_alv DEFINITION.

  PUBLIC SECTION.

    DATA: ob_alv TYPE REF TO cl_salv_table,
          ob_evt TYPE REF TO cl_salv_events_table.

    DATA: ob_columns TYPE REF TO cl_salv_columns_table,
          ob_column  TYPE REF TO cl_salv_column_table.

    DATA: gs_slis_fieldcat TYPE slis_t_fieldcat_alv,
          gt_slis_fieldcat TYPE slis_t_fieldcat_alv,
          gs_slis_layout   TYPE slis_layout_alv..

    METHODS:
      m_show_alv
        IMPORTING
          it_alv TYPE STANDARD TABLE,
      m_show_alv_factory
        IMPORTING
          it_alv TYPE STANDARD TABLE.

    METHODS:
      m_set_pf_status
        CHANGING
          co_alv TYPE REF TO cl_salv_table. " Default Pf Status

ENDCLASS.

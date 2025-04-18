*&---------------------------------------------------------------------*
*& Include          ZMMRE_CLASIFICACION_ATB_C00
*&---------------------------------------------------------------------*
*------------------------------------------------------------------------*
* Programa   : zmmre_clasificacion_atributos                             *
* Descripción: Reporte para consultar la información detallada           *
* del maestro del material con sus diferentes carácteristicas y atributos*
* Programador: David Navoa Acevedo                                       *
* Fecha      : 20.11.2023                                                *
*------------------------------------------------------------------------*

    CLASS lcl_catalogo DEFINITION.

      PUBLIC SECTION.

        TYPES:

          BEGIN OF ty_caract,
            clint  TYPE atwtb,
            atinn  TYPE atinn,
            atnam  TYPE atnam,
            atfor  TYPE atfor,
            atein  TYPE atein,
            atwrt  TYPE atwrt,
            spras  TYPE spras,
            atwtb  TYPE atwtb,
            klart  TYPE klassenart,
            class  TYPE klasse_d,
            clint1 TYPE clint,
            posnr  TYPE kposnr,
            adzhl  TYPE adzhl,
            klart1 TYPE klassenart,
          END OF ty_caract,

          BEGIN OF ty_caracteristicas,
            matnr TYPE matnr,
            class TYPE klasse_d,
            atinn TYPE atinn,
            atnam TYPE atnam,
            atwrt TYPE atwrt,
          END OF ty_caracteristicas.

        DATA:
              gt_fcat            TYPE lvc_t_fcat,
              gt_fcat_aux        TYPE lvc_t_fcat,
              gt_caracteristicas TYPE TABLE OF ty_caracteristicas.

        METHODS: m_get_data,
          m_create_catalog,
          m_create_table IMPORTING pt_fcat TYPE lvc_t_fcat,
          m_create_aux,
          m_create_structure,
          m_create_catalog_head CHANGING pt_fcat TYPE lvc_t_fcat,
          m_fill_clasf IMPORTING ps_mara TYPE mara.

    ENDCLASS.

    CLASS lcl_reporte DEFINITION.

      PUBLIC SECTION.

        DATA: ob_alv TYPE REF TO cl_salv_table,
              ob_evt TYPE REF TO cl_salv_events_table.

        DATA: ob_columns TYPE REF TO cl_salv_columns_table,
              ob_column  TYPE REF TO cl_salv_column_table.

        METHODS:
          m_show_alv.

        METHODS:
          m_set_pf_status
            CHANGING
              co_alv TYPE REF TO cl_salv_table, " Default Pf Status
          m_set_top_of_page
            CHANGING
              co_alv TYPE REF TO cl_salv_table. " Set Top of page

    ENDCLASS.

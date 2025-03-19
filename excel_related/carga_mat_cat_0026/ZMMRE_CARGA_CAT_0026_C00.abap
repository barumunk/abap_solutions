*&---------------------------------------------------------------------*
*& Include          ZMMRE_CARGA_CAT_0026_C00
*&---------------------------------------------------------------------*
CLASS lcl_catalogo DEFINITION.

  PUBLIC SECTION.
    "Constants
    CONSTANTS: p_scol TYPE i VALUE '1',
               p_srow TYPE i VALUE '1',
               p_ecol TYPE i VALUE '5',
               p_erow TYPE i VALUE '2500',
               c_x    TYPE c VALUE 'X'.

    DATA: gv_material TYPE char30.

    DATA:
      ls_organizational_level TYPE wrfscreenvalues..
    DATA: lt_forecastparameters  TYPE TABLE OF bapie1mpoprt.
    DATA: lt_forecastparametersx TYPE TABLE OF bapie1mpoprtx.
    DATA: ls_headdata             TYPE bapie1mathead,                       "Header data
          lt_clientdata           TYPE TABLE OF bapie1marart,             "Client data CLIENTEXT CLIENTEXTX ##########
          lt_clientdatax          TYPE TABLE OF bapie1marartx,
          lt_addnlclientdata      TYPE TABLE OF bapie1maw1rt,        "basic addition data
          lt_addnlclientdatax     TYPE TABLE OF bapie1maw1rtx,
          lt_materialdescription  TYPE TABLE OF bapie1maktrt,    "material description data
          lt_plantdata            TYPE TABLE OF bapie1marcrt,              "plant level data PLANTEXT PLANTEXTX ##########
          lt_plantdatax           TYPE TABLE OF bapie1marcrtx,
          lt_plantext             TYPE TABLE OF bapie1marcextrt,
          lt_plantextx            TYPE TABLE OF bapie1marcextrtx,
          lt_valuationdata        TYPE TABLE OF bapie1mbewrt,          "valuation data
          lt_valuationdatax       TYPE TABLE OF bapie1mbewrtx,
          lt_storagelocationdata  TYPE TABLE OF bapie1mardrt,    "Storage Location  STORAGELOCATIONEXT STORAGELOCATIONEXTX ##########
          lt_storagelocationdatax TYPE TABLE OF bapie1mardrtx,
          lt_unitsofmeasure       TYPE TABLE OF bapie1marmrt,          "####### & ##
          lt_unitsofmeasurex      TYPE TABLE OF bapie1marmrtx,
          lt_internationalartnos  TYPE TABLE OF bapie1meanrt,     "#####(GTINs) global level
          lt_vendorean            TYPE TABLE OF bapie1mleart,                                  "vendor level
          lt_inforecord_general   TYPE TABLE OF bapieina,          "###### PIR
          lt_inforecord_purchorg  TYPE TABLE OF wrfbapieine,
          lt_listingconditions    TYPE TABLE OF wlk1_ueb,           "execute listing data
*        lt_plantkeys TYPE TABLE OF bapie1wrkkey,               "
*        lt_storagelocationkeys TYPE TABLE OF bapie1lgokey,
*        lt_valuationtypekeys TYPE TABLE OF bapie1bwakey,
          lt_variantskeys         TYPE TABLE OF bapie1varkey,
          lt_characteristicvalue  TYPE TABLE OF bapie1ausprt,
          lt_characteristicvaluex TYPE TABLE OF bapie1ausprtx,
          lt_salesdata            TYPE TABLE OF bapie1mvkert,
          lt_salesdatax           TYPE TABLE OF bapie1mvkertx,
          lt_return               TYPE TABLE OF bapireturn1,        "Global return table
          lt_allocvaluescharnew   TYPE TABLE OF bapi1003_alloc_values_char,
          lt_allocvaluesnumnew    TYPE TABLE OF  bapi1003_alloc_values_num,
          lt_allocvaluescurrnew   TYPE TABLE OF  bapi1003_alloc_values_curr,
          lt_return_001           TYPE TABLE OF bapiret2,

          wa_clientdata           TYPE bapie1marart,
          wa_clientdatax          TYPE bapie1marartx,
          wa_addnlclientdata      TYPE bapie1maw1rt,
          wa_addnlclientdatax     TYPE bapie1maw1rtx,
          wa_materialdescription  TYPE bapie1maktrt,
          wa_plantdata            TYPE bapie1marcrt,
          wa_plantdatax           TYPE bapie1marcrtx,
          wa_plantext             TYPE bapie1marcextrt,
          wa_plantextx            TYPE bapie1marcextrtx,
          wa_valuationdata        TYPE bapie1mbewrt,
          wa_valuationdatax       TYPE bapie1mbewrtx,
          wa_storagelocationdata  TYPE bapie1mardrt,
          wa_storagelocationdatax TYPE bapie1mardrtx,
          wa_unitsofmeasure       TYPE bapie1marmrt,
          wa_unitsofmeasurex      TYPE bapie1marmrtx,
          wa_internationalartnos  TYPE bapie1meanrt,
          wa_vendorean            TYPE bapie1mleart,
          wa_layoutmoduleassgmt   TYPE bapie1malgrt,
          wa_layoutmoduleassgmtx  TYPE bapie1malgrtx,
          wa_inforecord_general   TYPE bapieina,
          wa_inforecord_purchorg  TYPE wrfbapieine,
          wa_listingconditions    TYPE wlk1_ueb,
          wa_variantskeys         TYPE bapie1varkey,
          wa_characteristicvalue  TYPE bapie1ausprt,
          wa_characteristicvaluex TYPE bapi1003_alloc_values_char,
*        wa_plantkeys TYPE bapie1wrkkey,
*        wa_storagelocationkeys TYPE bapie1lgokey,
*        wa_valuationtypekeys TYPE bapie1bwakey,
          wa_return               TYPE bapireturn1,

          wa_return_commit        TYPE bapiret2,
          wa_allocvaluescharnew   TYPE bapi1003_alloc_values_char,
          wa_return_001           TYPE bapiret2.

    DATA: lt_bapimatinr   TYPE TABLE OF bapimatinr,
          lt_intern_excel TYPE TABLE OF alsmex_tabline,
          wa_bapimatinr   TYPE bapimatinr.

    DATA: lt_matreturn TYPE TABLE OF merrdat,
          wa_matreturn TYPE merrdat.

    DATA: l_conditions TYPE string.

    DATA: lt_mean TYPE TABLE OF mean,
          wa_mean TYPE mean,
          lt_mlea TYPE TABLE OF mlea,
          wa_mlea TYPE mlea,
          l_tabix TYPE sy-tabix.

    DATA: lt_bomheader TYPE TABLE OF wstr_in,
          ls_matnr_new TYPE matnr.

    TYPES: BEGIN OF ty_data,
             icon(1),
             material TYPE string,
             clase    TYPE string,
             "clasificacion TYPE string, "Material ya cuenta con clasificacion, Material no cuenta con clasificacion
             "estatus       TYPE string,
             return   TYPE string,
             type     TYPE string,
           END OF ty_data.

    "typo del ALV
    TYPES: BEGIN OF ty_alv,
             index TYPE sy-tabix.
             INCLUDE TYPE ty_data.
    TYPES: END OF ty_alv.

    "tablas
    DATA: gt_alv TYPE TABLE OF ty_alv.
    "Rangos

    DATA: lv_mes          TYPE char10,
          lv_tipo_reporte TYPE char50,
          lv_periodo      TYPE char2,
          lv_anio         TYPE char4,
          lv_fecha_ini    TYPE budat,
          lv_fecha_fin    TYPE budat,
          lv_honor        TYPE char6 VALUE 'HONOR%'.

    METHODS: m_get_data,
      m_validation_data,
      m_fill_data,
      m_upload_data.

  PRIVATE SECTION.
    CONSTANTS: co_vbrk    TYPE tcode VALUE 'VBRK',
               co_mkpf    TYPE awtyp VALUE 'MKPF',
               co_anuak   TYPE awtyp VALUE 'ANUAK',
               co_prchg   TYPE awtyp VALUE 'PRCHG',
               co_afru    TYPE awtyp VALUE 'AFRU',
               co_stat_z  TYPE bstat VALUE 'Z',
               co_stat_v  TYPE bstat VALUE 'V',
               co_stat_w  TYPE bstat VALUE 'W',
               co_stat_s  TYPE bstat VALUE 'S',
               co_koart_s TYPE koart VALUE 'S',
               co_index1  TYPE i VALUE '1',
               co_mxn     TYPE waers VALUE 'MXN',
               co_cero    TYPE char4 VALUE '0.00',
               co_50      TYPE i VALUE '50'.

ENDCLASS.

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    TYPE-POOLS: icon.

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

  PRIVATE SECTION.
    CONSTANTS: co_50(2) TYPE c VALUE '50'.


ENDCLASS.

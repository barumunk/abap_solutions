*&---------------------------------------------------------------------*
*& Include          ZMM_CREACION_SOLPED_MAS_02
*&---------------------------------------------------------------------*


CLASS lcl_catalogo IMPLEMENTATION.

  METHOD m_procesar_layout.

    DATA: ls_catalogo  TYPE ty_item.
    DATA: lv_row TYPE i.

    IF p_file IS NOT INITIAL.

      CLEAR: lt_excel_data[], lv_row.

      SELECT SINGLE low FROM tvarvc INTO @DATA(lv_low) WHERE name EQ 'Z_EXCEL_ROW'.
      IF sy-subrc EQ 0.
        lv_row = lv_low.
      ELSE.
        lv_row = '3000'.
      ENDIF.

      CALL FUNCTION 'ZALSMEX_TABLINE_2_TT'
        EXPORTING
          filename                = p_file
          i_begin_col             = p_scol
          i_begin_row             = p_srow
          i_end_col               = '100'
          i_end_row               = lv_row
        TABLES
          intern                  = lt_excel_data
        EXCEPTIONS
          inconsistent_parameters = 1
          upload_ole              = 2
          OTHERS                  = 3.
      IF sy-subrc EQ 0.
        LOOP AT lt_excel_data INTO DATA(ls_excel_data).
          AT NEW row.
            CLEAR ls_catalogo.
          ENDAT.

          CASE ls_excel_data-col.
            WHEN '0001'.
              ls_catalogo-bsart = ls_excel_data-value.
            WHEN '0002'.
              ls_catalogo-epstp = ls_excel_data-value.
            WHEN '0003'.
              ls_catalogo-matnr = ls_excel_data-value.
              DATA(lv_len) = strlen( ls_catalogo-matnr ).
*              ls_catalogo-matnr = ls_catalogo-matnr_o = COND #(
*              WHEN lv_len EQ 12 THEN ls_catalogo-matnr
*              WHEN lv_len GT 12 THEN ls_catalogo-matnr(12) ).

              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input  = ls_catalogo-matnr
                IMPORTING
                  output = ls_catalogo-matnr.
              ls_catalogo-matnr_o = me->m_get_matnr_identif( i_matnr = ls_catalogo-matnr ) .
              IF ls_catalogo-matnr_o IS INITIAL.
                ls_catalogo-matnr_o = ls_catalogo-matnr_o = COND #(
                WHEN lv_len EQ 12 THEN ls_catalogo-matnr
                WHEN lv_len GT 12 THEN ls_catalogo-matnr(12) ).
              ENDIF.
            WHEN '0004'.
              ls_catalogo-eeind = ls_excel_data-value.
              ls_catalogo-eeind = me->m_date_format( i_date = ls_catalogo-eeind ).
              DATA(lv_matnr_out) = |{ ls_catalogo-matnr_o ALPHA = OUT }|.
              ls_catalogo-code = |{ ls_catalogo-bsart }{ lv_matnr_out }{ ls_catalogo-eeind }|.
              CONDENSE ls_catalogo-code NO-GAPS.
            WHEN '0005'.
              ls_catalogo-werks = ls_excel_data-value.
            WHEN '0006'.
              ls_catalogo-lgort = ls_excel_data-value.
            WHEN '0007'.
              ls_catalogo-ekgrp = ls_excel_data-value.
            WHEN '0008'.
              ls_catalogo-afnam = ls_excel_data-value.
            WHEN '0009'.
              ls_catalogo-ekorg = ls_excel_data-value.
            WHEN '0010'.
              ls_catalogo-reswk = ls_excel_data-value.
            WHEN '0011'.
              ls_catalogo-menge = ls_excel_data-value.
            WHEN '0012'.
              ls_catalogo-preis = ls_excel_data-value.
            WHEN '0013'.
              ls_catalogo-waers = ls_excel_data-value.
            WHEN '0014'.
              ls_catalogo-netpr = ls_excel_data-value.
            WHEN '0015'.
              ls_catalogo-bednr = ls_excel_data-value.
          ENDCASE.

          AT END OF row.
            APPEND ls_catalogo TO lt_catalogo.
          ENDAT.
        ENDLOOP.
      ENDIF.
    ELSE.
      MESSAGE TEXT-001 TYPE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD m_get_matnr_identif.
    DATA: lv_material  TYPE matnr,
          lv_categoria TYPE attyp.

    CLEAR: lv_material, lv_categoria.

    SELECT SINGLE satnr, attyp FROM mara INTO (@lv_material, @lv_categoria)
      WHERE matnr EQ @i_matnr.
    IF lv_categoria = '02'. "variante
      rv_matnr = lv_material.
    ELSEIF lv_categoria = '01'. "generico
      rv_matnr = i_matnr.
    ENDIF.

  ENDMETHOD.

  METHOD m_date_format.
    DATA: lv_string_d TYPE string.

    lv_string_d = i_date.
    REPLACE ALL OCCURRENCES OF '/' IN lv_string_d WITH '.'.
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lv_string_d
      IMPORTING
        date_internal            = rv_date
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ENDMETHOD.

  METHOD m_obtener_data.
    DATA: ls_item_cat     TYPE ty_item,
          lt_items        TYPE TABLE OF bapimereqitemimp,
          lt_itemsx       TYPE TABLE OF bapimereqitemx,
          lv_solped       TYPE bapimereqheader-preq_no,
          lv_preq_item    TYPE bnfpo,
          lv_banfn        TYPE banfn,
          lt_return       TYPE TABLE OF bapiret2,
          ls_request      TYPE ts_request,
          ls_item         TYPE bapimereqitemimp,
          ls_itemx        TYPE bapimereqitemx,
          lt_catalogo_aux TYPE TABLE OF ty_item,
          lv_flag         TYPE c,
          pr_header       TYPE bapimereqheader.

    CLEAR: lv_solped, lv_preq_item, lt_items[], gt_request[].

    lt_catalogo_aux[] = i_data[].

    LOOP AT lt_catalogo_aux INTO DATA(ls_data).
      AT NEW code.
        lv_flag = abap_true.
        ls_request-error = abap_false.
        CLEAR ls_request.
      ENDAT.
      CLEAR: ls_item.
      IF lv_flag = abap_true.
        lv_preq_item = '00010'.
        lv_flag = abap_false.
      ENDIF.
      ls_request-bsart = ls_data-bsart.
      ls_item-ctrl_ind = 'R'.

      ls_item-preq_item = lv_preq_item.
      ls_item-item_cat = ls_data-epstp.
      ls_item-material = ls_data-matnr.
      SELECT SINGLE matnr FROM mara INTO @DATA(lv_matnr) WHERE matnr EQ @ls_data-matnr.
      IF sy-subrc NE 0 AND ls_request-error EQ abap_false.
        ls_request-error = abap_true.
        ls_request-mensage = |El material { ls_data-matnr } no existe|.
      ENDIF.
      ls_request-matnr = ls_data-matnr_o.

      CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
        EXPORTING
          input  = ls_data-eeind
        IMPORTING
          output = ls_item-deliv_date.

      ls_item-deliv_date = ls_data-eeind.
      ls_request-code = ls_data-code.

      ls_itemx-preq_item = lv_preq_item.
      ls_itemx-item_cat = abap_true.
      ls_itemx-material = abap_true.
      ls_itemx-deliv_date = abap_true.

      ls_item-plant = ls_data-werks.
      ls_item-store_loc = ls_data-lgort.
      ls_item-pur_group = ls_data-ekgrp.
      ls_item-preq_name = ls_data-afnam.

      ls_itemx-plant = abap_true.
      ls_itemx-store_loc = abap_true.
      ls_itemx-pur_group = abap_true.
      ls_itemx-preq_name = abap_true.

      ls_item-purch_org = ls_data-ekorg.
      ls_item-suppl_plnt = ls_data-reswk.
      ls_item-quantity = ls_data-menge.


      ls_itemx-purch_org = abap_true.
      ls_itemx-suppl_plnt = abap_true.
      ls_itemx-quantity = abap_true.

      ls_item-preq_price = ls_data-preis.
      ls_itemx-preq_price = abap_true.

      ls_item-currency = ls_data-waers.
      IF ls_data-netpr IS INITIAL.
        ls_data-netpr = '1'.
      ENDIF.

      ls_item-price_unit = ls_data-netpr.
      ls_itemx-price_unit = abap_true.

      ls_itemx-currency = abap_true.
      ls_item-trackingno = ls_request-bednr = ls_data-bednr.

      ls_itemx-trackingno = abap_true.

      lv_preq_item += 10.

      APPEND ls_item TO ls_request-lt_items.
      APPEND ls_itemx TO ls_request-lt_itemsx.
      AT END OF code.
        APPEND ls_request TO gt_request.
      ENDAT.
    ENDLOOP.

    LOOP AT gt_request ASSIGNING FIELD-SYMBOL(<fs_request>).

      IF <fs_request>-error NE abap_true.
        lv_solped = me->m_cargar_data( EXPORTING i_bsart = <fs_request>-bsart i_items = <fs_request>-lt_items i_itemsx = <fs_request>-lt_itemsx CHANGING c_mensage = <fs_request>-mensage ).
      ENDIF.

      lv_banfn = lv_solped.
      me->m_procesar_data(
      EXPORTING
        i_request = <fs_request>
      CHANGING
        c_solped = lv_banfn ).

    ENDLOOP.

  ENDMETHOD.

  METHOD m_cargar_data.

    DATA: pr_solped  TYPE bapimereqheader-preq_no,
          lt_return  TYPE TABLE OF bapiret2,
          lt_items   TYPE TABLE OF bapimereqitemimp,
          lt_itemsx  TYPE TABLE OF bapimereqitemx,
          ls_header  TYPE bapimereqheader,
          ls_headerx TYPE bapimereqheaderx,
          pr_header  TYPE bapimereqheader.

    lt_items[] = i_items[].
    lt_itemsx[] = i_itemsx[].

    ls_header-pr_type = i_bsart.
    ls_headerx-pr_type = abap_true.

    CALL FUNCTION 'BAPI_PR_CREATE'
      EXPORTING
        prheader  = ls_header
        prheaderx = ls_headerx
      IMPORTING
        number    = pr_solped
*       prheaderexp = pr_header
      TABLES
        return    = lt_return
        pritem    = lt_items
        pritemx   = lt_itemsx.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      IF lt_items IS NOT INITIAL.
        LOOP AT lt_return INTO DATA(ls_return) WHERE type = 'E'.
          c_mensage = ls_return-message.
        ENDLOOP.
      ELSE.
        ls_return-id = 'E'.
        c_mensage = ls_return-message = 'Material No Existe'.
        "APPEND ls_return TO lt_return_alv.

      ENDIF.

    ELSE.

      rv_solped = pr_solped.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.

  ENDMETHOD.

  METHOD m_procesar_data.
    DATA: lv_menge_sum TYPE i,
          lv_menge     TYPE i,
          ls_item      TYPE ty_catalogo_alv.

    CLEAR ls_item.

    SELECT * FROM eban INTO TABLE @DATA(lt_eban)
      WHERE banfn EQ @c_solped.
    IF sy-subrc EQ 0.
*      DELETE ADJACENT DUPLICATES FROM lt_eban COMPARING banfn.
      CLEAR: lv_menge_sum, lv_menge.
      LOOP AT lt_eban INTO DATA(ls_eban).
        ls_item-icon    = 3.
        ls_item-banfn   = ls_eban-banfn.
        ls_item-bsart   = ls_eban-bsart.

        SELECT SINGLE * FROM mara INTO @DATA(ls_mara)
          WHERE matnr EQ @ls_eban-matnr.
        IF ls_mara-attyp = '02'.
          ls_item-matnr  = ls_mara-satnr.
        ELSEIF ls_mara-attyp = '01'.
          ls_item-matnr = ls_eban-matnr.
        ENDIF.
        ls_item-matnr = |{ ls_item-matnr ALPHA = OUT }|.

        ls_item-eeind     = ls_eban-lfdat.
        ls_item-werks = ls_eban-werks.
        ls_item-lgort = ls_eban-lgort.
        ls_item-ekgrp = ls_eban-ekgrp.

        ls_item-reswk   = ls_eban-reswk.
        lv_menge = ls_eban-menge.
        lv_menge_sum = lv_menge_sum + lv_menge.

        ls_item-bednr = ls_eban-bednr.
        ls_item-mensage = TEXT-010.
      ENDLOOP.
      ls_item-menge = lv_menge_sum.
      APPEND ls_item TO lt_catalogo_alv.
    ELSE.

      DESCRIBE TABLE lt_return_alv LINES DATA(lv_lines).
      DESCRIBE TABLE lt_catalogo LINES DATA(lv_lines_alv).

      DATA(lv_tabix) = sy-tabix + 1.
      IF lv_lines LE lv_lines_alv.
        lv_tabix = lv_tabix - 1.
      ENDIF.

      ls_item-icon    = 1.
      ls_item-bsart   = i_request-bsart.
      ls_item-matnr = i_request-matnr.

      READ TABLE i_request-lt_items INTO DATA(ls_items) INDEX 1.
      IF sy-subrc EQ 0.
        ls_item-werks = ls_items-plant.
        ls_item-lgort = ls_items-store_loc.
        ls_item-ekorg = ls_items-purch_org.
        ls_item-reswk   = ls_items-suppl_plnt.
        ls_item-menge = ls_items-quantity.
      ENDIF.

      ls_item-bednr = i_request-bednr.
      ls_item-mensage = i_request-mensage.
      APPEND ls_item TO lt_catalogo_alv.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_reporte IMPLEMENTATION.

  METHOD m_show_alv.

    DATA: lc_msg          TYPE REF TO cx_salv_msg,
          lo_sorts        TYPE REF TO cl_salv_sorts,
          lv_col_name     TYPE lvc_fname,
          lv_name         TYPE string,
          lt_fcat         TYPE lvc_t_fcat,
          lt_catalogo_alv TYPE TABLE OF ty_catalogo_alv,
          lv_name_txt     TYPE scrtext_l.

    lt_catalogo_alv[] = it_catalogo[].

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = ob_alv
          CHANGING
            t_table      = lt_catalogo_alv[].
      CATCH cx_salv_msg INTO lc_msg .
    ENDTRY.

    DATA: gr_layout TYPE REF TO cl_salv_layout.
    DATA: key TYPE salv_s_layout_key.

    gr_layout = ob_alv->get_layout( ).
    key-report = sy-repid.
    gr_layout->set_key( key ).

    gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    DATA: o_selections TYPE REF TO cl_salv_selections.
    o_selections = ob_alv->get_selections( ).
    o_selections->set_selection_mode( cl_salv_selections=>multiple ).

    TRY.
        DATA(lr_columnas) = ob_alv->get_columns( ).
        lr_columnas->set_exception_column( value = 'ICON' ).
      CATCH cx_salv_data_error INTO DATA(lv_error).
        "e_code = lv_error->get_text( ).
    ENDTRY.

* columns
    ob_columns = ob_alv->get_columns( ).

    ob_columns->set_key_fixation( value = abap_true ).

    ob_column ?= ob_columns->get_column( 'BANFN' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'BANFN' position = 1 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'Clase de documento' ).
    ob_column->set_long_text( 'Clase de documento' ).
    ob_column->set_key( if_salv_c_bool_sap=>true ).

    ob_column ?= ob_columns->get_column( 'BSART' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'BSART' position = 2 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'Clase de Pedido' ).
    ob_column->set_long_text( 'Clase de Pedido' ).
    ob_column->set_key( if_salv_c_bool_sap=>true ).

    ob_column ?= ob_columns->get_column( 'MATNR' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MATNR').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Material' ).
    ob_column->set_medium_text( 'Material' ).
    ob_column->set_long_text( 'Material' ).

    ob_column ?= ob_columns->get_column( 'EEIND' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'EEIND').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_long_text( 'Fecha de Entrega' ).

    ob_column ?= ob_columns->get_column( 'WERKS' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'WERKS  ').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Centro' ).
    ob_column->set_medium_text( 'Centro' ).
    ob_column->set_long_text( 'Centro' ).

    ob_column ?= ob_columns->get_column( 'LGORT' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'LGORT').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Almacen' ).
    ob_column->set_medium_text( 'Almacen' ).
    ob_column->set_long_text( 'Almacen' ).

    ob_column ?= ob_columns->get_column( 'RESWK' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'RESWK').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_long_text( 'Centro Suministrador' ).

    ob_column ?= ob_columns->get_column( 'MENGE' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MENGE').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'Ctd. Pedido' ).
    ob_column->set_long_text( 'Cantidad Pedido' ).

    ob_column ?= ob_columns->get_column( 'AFNAM' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'AFNAM').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'Solicitante' ).
    ob_column->set_long_text( 'Solicitante' ).
    ob_column->set_visible( value  = if_salv_c_bool_sap=>false ).

    ob_column ?= ob_columns->get_column( 'EKORG' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'EKORG').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'Org Compras' ).
    ob_column->set_long_text( 'Organisación de Compras' ).
    ob_column->set_visible( value  = if_salv_c_bool_sap=>false ).

    ob_column ?= ob_columns->get_column( 'EKGRP' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'EKGRP').
    ob_column->set_visible( value  = if_salv_c_bool_sap=>false ).

    ob_column ?= ob_columns->get_column( 'BEDNR' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'BEDNR').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'N. Necesidad' ).
    ob_column->set_long_text( 'N. Necesidad' ).
    ob_column->set_visible( value  = if_salv_c_bool_sap=>false ).

    ob_column ?= ob_columns->get_column( 'MENSAGE' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MENSAGE').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'Mensage' ).
    ob_column->set_long_text( 'Mensage' ).

* Calling Set PF status method
    CALL METHOD m_set_pf_status
      CHANGING
        co_alv = ob_alv.

    ob_alv->display( ).

  ENDMETHOD.

  METHOD m_set_pf_status.
    SET PF-STATUS 'STANDARD'.
    ob_alv->set_screen_status(
    pfstatus      =  'STANDARD'
    report       = sy-repid
    set_functions = ob_alv->c_functions_all ).
  ENDMETHOD.

ENDCLASS.

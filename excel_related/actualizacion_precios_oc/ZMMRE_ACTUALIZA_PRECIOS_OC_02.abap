*&---------------------------------------------------------------------*
*& Include          ZMMRE_ACTUALIZA_PRECIOS_OC_02
*&---------------------------------------------------------------------*
* Programa   : ZMMRE_ACTUALIZA_PRECIOS_OC                                *
* Descripción: Reporte para cambiar precios en Orden de Compra           *
* Programador: David Navoa Acevedo                                       *
* Fecha      : 06.06.2024                                                *
*------------------------------------------------------------------------*

CLASS lcl_catalogo IMPLEMENTATION.

  METHOD m_update_data.

    DATA: lt_intern_excel TYPE TABLE OF alsmex_tabline,
          lt_konm_staf    TYPE STANDARD TABLE OF condscale,
          lt_excel_data   TYPE TABLE OF ztvconst,
          lt_bapi_cond    TYPE STANDARD TABLE OF mewicondition,
          lt_bapi_ret     TYPE STANDARD TABLE OF bapireturn,
          ls_bapi_cond    TYPE mewicondition,
          ls_excel_data   TYPE ztvconst,
          ls_catalogo     TYPE zes_catalogo_oc,
          lx_konpdb       TYPE konpdb,
          lt_konpdb       TYPE STANDARD TABLE OF konpdb,
          lt_konhdb       TYPE STANDARD TABLE OF konhdb,
          ls_bapi_eina    TYPE mewieina,
          ls_bapi_einax   TYPE mewieinax,
          ls_bapi_eine    TYPE mewieine,
          ls_bapi_einex   TYPE mewieinex,
          ls_eina         TYPE eina,
          ls_eine         TYPE eine,
          lv_datbi        TYPE c LENGTH 10,
          lv_konp_kbetr   TYPE char30,
          lv_langu        TYPE char2.

    CLEAR: lt_excel_data[], lt_intern_excel[].

    IF p_file IS NOT INITIAL.

      CALL FUNCTION 'ZALSMEX_TABLINE_2_TT'
        EXPORTING
          filename                = p_file
          i_begin_col             = p_scol
          i_begin_row             = p_srow
          i_end_col               = '100'
          i_end_row               = '500'
        TABLES
          intern                  = lt_intern_excel
        EXCEPTIONS
          inconsistent_parameters = 1
          upload_ole              = 2
          OTHERS                  = 3.
      IF sy-subrc EQ 0.

        LOOP AT lt_intern_excel INTO DATA(ls_excel).
          AT NEW row.
            CLEAR ls_catalogo.
          ENDAT.

          CASE ls_excel-col.
            WHEN '0001'.
              ls_catalogo-bsart = ls_excel-value.
            WHEN '0002'.
              ls_catalogo-ebeln = ls_excel-value.
            WHEN '0003'.
              ls_catalogo-ebelp = ls_excel-value.
            WHEN '0004'.
              ls_catalogo-matnr = ls_excel-value.
              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input  = ls_catalogo-matnr
                IMPORTING
                  output = ls_catalogo-matnr.
            WHEN '0005'.
              ls_catalogo-txz01 = ls_excel-value.
            WHEN '0006'.
              ls_catalogo-netpr = ls_excel-value.
            WHEN '0007'.
              ls_catalogo-waers = ls_excel-value.
          ENDCASE.

          AT END OF row.
            APPEND ls_catalogo TO lt_catalogo.
          ENDAT.
        ENDLOOP.

        "Actualizacion de info
        LOOP AT lt_catalogo INTO ls_catalogo.

          "Asignar condición a tratar en base a tipo de documento.
          DATA(lv_condx) = COND #( WHEN ls_catalogo-bsart = 'ZUB4' THEN 'P101' ELSE 'PB00' ).

          SELECT SINGLE * INTO @DATA(wa_ekko) FROM ekko
            WHERE ebeln = @ls_catalogo-ebeln.

          SELECT * FROM eina INTO TABLE @DATA(lt_eina) UP TO 1 ROWS
            WHERE matnr = @ls_catalogo-matnr
            "AND lifnr IN @s_lifnr
            AND lifnr = @wa_ekko-lifnr
            ORDER BY erdat DESCENDING.
          IF sy-subrc EQ 0.
            READ TABLE lt_eina INTO ls_eina INDEX 1.

            SELECT SINGLE * FROM eine INTO @ls_eine
               WHERE infnr = @ls_eina-infnr.
*                 AND ekorg = @i_ordenes-ekorg.

          ENDIF.

          SELECT SINGLE * FROM mara INTO @DATA(ls_mara)
            WHERE matnr = @ls_catalogo-matnr.
          IF sy-subrc EQ 0 AND ls_mara-attyp EQ '01'.
            SELECT SINGLE meins FROM mara
              INTO @DATA(lv_meins)
              WHERE pmata = @ls_catalogo-matnr.
            IF sy-subrc <> 0.
              lv_meins = ls_mara-meins.
            ENDIF.
          ELSEIF sy-subrc EQ 0.
            lv_meins = ls_mara-meins.
          ENDIF.

          IF ls_eina-meins IS NOT INITIAL.
            lv_meins = ls_eina-meins.
          ENDIF.

          SELECT SINGLE bedat FROM ekko INTO @DATA(lv_bedat_aux)
            WHERE ebeln = @ls_catalogo-ebeln.
          IF sy-subrc EQ 0.
            lv_datbi = |{ lv_bedat_aux+6(2) }.{ lv_bedat_aux+4(2) }.{ lv_bedat_aux(4) }|.
          ENDIF.

          CLEAR: ls_bapi_eina, ls_bapi_einax, ls_bapi_eine, ls_bapi_einex.
**    ls_bapi_eina-info_rec  = ls_eina-infnr.
          ls_bapi_eina-material  = ls_eina-matnr.
          ls_bapi_eina-vendor    = ls_eina-lifnr.
          ls_bapi_eina-base_uom  = lv_meins.
          ls_bapi_eina-po_unit   = lv_meins.
          ls_bapi_eina-conv_num1 = ls_eina-umrez.
          ls_bapi_eina-conv_den1 = ls_eina-umren.

**    ls_bapi_einax-info_rec  = abap_true.
          ls_bapi_einax-material  = abap_true.
          ls_bapi_einax-vendor    = abap_true.
          ls_bapi_einax-base_uom  = abap_true.
          ls_bapi_einax-po_unit   = abap_true.
          ls_bapi_einax-conv_num1 = abap_true.
          ls_bapi_einax-conv_den1 = abap_true.

**    ls_bapi_eine-info_rec  = ls_eine-infnr.
          ls_bapi_eine-purch_org = ls_eine-ekorg.
          ls_bapi_eine-info_type = ls_eine-esokz.
          ls_bapi_eine-pur_group = ls_eine-ekgrp.
          ls_bapi_eine-currency = ls_catalogo-waers.
          ls_bapi_eine-nrm_po_qty = 1.
          ls_bapi_eine-net_price = ls_catalogo-netpr.
          ls_bapi_eine-plnd_delry = ls_eine-aplfz.
          ls_bapi_eine-price_unit = 1.
          ls_bapi_eine-price_date = lv_bedat_aux.
          ls_bapi_eine-conv_num1 = ls_eina-umrez.
          ls_bapi_eine-conv_den1 = ls_eina-umren.
          ls_bapi_eine-eff_price = ls_catalogo-netpr.
*    ls_bapi_eine-pricedate = abap_true.
          ls_bapi_eine-vendor = ls_eina-lifnr.


**    ls_bapi_einx-info_rec  = abap_true.
          ls_bapi_einex-purch_org = abap_true.
          ls_bapi_einex-info_type = abap_true.
          ls_bapi_einex-pur_group = abap_true.
          ls_bapi_einex-currency = abap_true.
          ls_bapi_einex-nrm_po_qty = abap_true.
          ls_bapi_einex-net_price = abap_true.
          ls_bapi_einex-plnd_delry = abap_true.
          ls_bapi_einex-price_unit = abap_true.
          ls_bapi_einex-price_date = abap_true.
          ls_bapi_einex-conv_num1 = abap_true.
          ls_bapi_einex-conv_den1 = abap_true.
          ls_bapi_einex-eff_price = abap_true.
*    ls_bapi_einex-pricedate = abap_true.
          ls_bapi_einex-vendor = abap_true.

          CLEAR: ls_bapi_cond, lt_bapi_cond[].
          ls_bapi_cond-cond_type = lv_condx.
          ls_bapi_cond-cond_value = ls_catalogo-netpr.
          ls_bapi_cond-change_id = abap_true.
          APPEND ls_bapi_cond TO lt_bapi_cond.

          CLEAR lt_bapi_ret[].
          CALL FUNCTION 'ME_INFORECORD_MAINTAIN'
            EXPORTING
              i_eina    = ls_bapi_eina
              i_einax   = ls_bapi_einax
              i_eine    = ls_bapi_eine
              i_einex   = ls_bapi_einex
            TABLES
              condition = lt_bapi_cond
              return    = lt_bapi_ret.
          IF lt_bapi_ret IS NOT INITIAL.

            READ TABLE lt_bapi_ret INTO DATA(ls_bapi_ret)
            WITH KEY type = 'E'.
            IF sy-subrc NE 0.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
              DATA(lv_modif) = abap_true.

            ENDIF.

          ENDIF.

          SELECT  * FROM a018 INTO TABLE @DATA(lt_a018) UP TO 1 ROWS
            WHERE kappl = 'M'
            AND kschl = @lv_condx
            AND lifnr = @ls_eina-lifnr
            AND matnr = @ls_eina-matnr
            AND ekorg = @ls_eine-ekorg
            ORDER BY datab DESCENDING.
          IF sy-subrc = 0.


            READ TABLE lt_a018 INTO DATA(ls_a018) INDEX 1.
            IF sy-subrc = 0.
              ls_bapi_cond-serial_id = ls_a018-knumh.
              ls_bapi_cond-cond_type = lv_condx.
              ls_bapi_cond-cond_value = ls_eine-netpr.
              ls_bapi_cond-change_id = abap_true.
              APPEND ls_bapi_cond TO lt_bapi_cond.
              SELECT SINGLE * FROM konp
                INTO @DATA(ls_konp)
               WHERE knumh = @ls_a018-knumh
                 AND kschl = @lv_condx.
              IF sy-subrc = 0.
                MOVE-CORRESPONDING ls_konp TO lx_konpdb.
                lx_konpdb-updkz = 'UPDATE'.
                lx_konpdb-kbetr = ls_catalogo-netpr.
                APPEND lx_konpdb TO lt_konpdb.
              ENDIF.

            ENDIF.
          ELSE.

            CLEAR bdcdata.  REFRESH bdcdata.
            PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'EINE-EKORG'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '/00'.
            PERFORM bdc_field       USING 'EINA-LIFNR'
                                          ls_eina-lifnr.
            PERFORM bdc_field       USING 'EINA-MATNR'
                                          ls_eina-matnr.
            PERFORM bdc_field       USING 'EINE-EKORG'
                                          ls_eine-ekorg.
            PERFORM bdc_field       USING 'RM06I-NORMB'
                                          'X'.

            PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'EINA-MAHN1'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=EINE'.
            SELECT SINGLE lands FROM ekko INTO lv_langu
              WHERE ebeln = ls_catalogo-ebeln.

            PERFORM bdc_field       USING 'EINA-URZLA'
                                          lv_langu.
            PERFORM bdc_field       USING 'EINA-MEINS'
                                          lv_meins.
            PERFORM bdc_field       USING 'EINA-UMREZ'
                                          '1'.
            PERFORM bdc_field       USING 'EINA-UMREN'
                                          '1'.

            PERFORM bdc_dynpro      USING 'SAPMM06I' '0102'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'EINE-MEPRF'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '/00'.
            PERFORM bdc_field       USING 'EINE-APLFZ'
                                          ls_eine-aplfz.
            PERFORM bdc_field       USING 'EINE-EKGRP'
                                          ls_eine-ekgrp.
            PERFORM bdc_field       USING 'EINE-KZABS'
                                          'X'.
            PERFORM bdc_field       USING 'EINE-NORBM'
                                          '1'.
*        PERFORM bdc_field       USING 'EINE-BSTAE'
*                                      i_ordenes-bstae.
            PERFORM bdc_field       USING 'EINE-WEBRE'
                                          'X'.
            PERFORM bdc_field       USING 'EINE-IPRKZ'
                                          'D'.
**        PERFORM bdc_field       USING 'EINE-TRANSPORT_CHAIN'
**                                      ''.
            PERFORM bdc_field       USING 'EINE-STAGING_TIME'
                                          '1'.

            PERFORM bdc_dynpro      USING 'SAPMM06I' '0105'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'EINE-ANGNR'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '/00'.

            PERFORM bdc_dynpro      USING 'SAPMM06I' '0103'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RM06I-LTEX1(01)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=KO'.

            PERFORM bdc_dynpro      USING 'SAPMV13A' '1018'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RV13A-DATBI(01)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '/00'.
            lv_konp_kbetr = ls_catalogo-netpr.
            CONDENSE lv_konp_kbetr NO-GAPS.
            REPLACE ALL OCCURRENCES OF '.' IN lv_konp_kbetr WITH ','.
            PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                          lv_konp_kbetr.
            PERFORM bdc_field       USING 'RV13A-DATAB(01)'
                                          lv_datbi.

            PERFORM bdc_dynpro      USING 'SAPMV13A' '1018'.
            PERFORM bdc_field       USING 'BDC_CURSOR'
                                          'RV13A-DATAB(01)'.
            PERFORM bdc_field       USING 'BDC_OKCODE'
                                          '=SICH'.

            REFRESH messtab.

            CALL TRANSACTION 'ME12' WITH AUTHORITY-CHECK USING bdcdata
                             MODE   'N'
                             UPDATE 'L'
                             MESSAGES INTO messtab.

            READ TABLE messtab INTO DATA(ls_mess)
            WITH KEY msgtyp = 'E'.
            IF sy-subrc NE 0.
              COMMIT WORK AND WAIT.
            ENDIF.

          ENDIF.

          IF lv_modif = abap_true.

            CALL FUNCTION 'CND_PRICES_DETAILS_SAVE'
              TABLES
                db_xkonh = lt_konhdb
                db_xkonp = lt_konpdb
                db_xstaf = lt_konm_staf
              EXCEPTIONS
                OTHERS   = 1.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
            ELSE.
              "Error
            ENDIF.

          ELSE.

            CLEAR lt_bapi_ret.
            CALL FUNCTION 'ME_INFORECORD_MAINTAIN'
              EXPORTING
                i_eina    = ls_bapi_eina
                i_einax   = ls_bapi_einax
                i_eine    = ls_bapi_eine
                i_einex   = ls_bapi_einex
              TABLES
                condition = lt_bapi_cond
                return    = lt_bapi_ret.

            IF lt_bapi_ret IS NOT INITIAL.

              CLEAR ls_bapi_ret.
              READ TABLE lt_bapi_ret INTO ls_bapi_ret
              WITH KEY type = 'E'.
              IF sy-subrc NE 0.

                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.

                "Save Updates to KONP Table
                CALL FUNCTION 'CND_PRICES_DETAILS_SAVE'
                  TABLES
                    db_xkonh = lt_konhdb
                    db_xkonp = lt_konpdb
                    db_xstaf = lt_konm_staf
                  EXCEPTIONS
                    OTHERS   = 1.
                IF sy-subrc = 0.
                  COMMIT WORK AND WAIT.
                ELSE.
                  "Error
                ENDIF.

              ELSE.
                "Error
              ENDIF.

            ENDIF.

          ENDIF.

        ENDLOOP.
*****

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD m_update_po.

    CONSTANTS: lc_id TYPE c VALUE 'I'.

    DATA: lt_items     TYPE TABLE OF bapimepoitem,
          lt_items_aux TYPE TABLE OF bapimepoitem,
          lt_return    TYPE TABLE OF bapiret2,
          lt_return1   TYPE TABLE OF bapiret2,
          lt_itemsx    TYPE TABLE OF bapimepoitemx,
          lt_cond      TYPE TABLE OF bapimepocond,
*          ls_cond      TYPE bapimepocond,
          lt_cond_aux  TYPE TABLE OF bapimepocond,
          lt_condx     TYPE TABLE OF bapimepocondx.
    DATA: ls_poheader  TYPE bapimepoheader,
          ls_poheaderx TYPE bapimepoheaderx,
          ls_itemsx    TYPE bapimepoitemx,
          ls_condx     TYPE bapimepocondx,
          lv_flag      TYPE c,
          lv_p2        TYPE p DECIMALS 2,
          ls_return    TYPE bapiret2.

    TYPES: BEGIN OF t_ebeln,
             ebeln TYPE ekko-ebeln,
           END OF t_ebeln.
    DATA: it_ebeln TYPE STANDARD TABLE OF t_ebeln,
          ls_ebeln TYPE t_ebeln.

    LOOP AT lt_catalogo INTO DATA(ls_catalogo).
      ls_ebeln-ebeln = ls_catalogo-ebeln.
      APPEND ls_ebeln TO it_ebeln.
      "Asignar condición a tratar en base a tipo de documento.
      DATA(lv_condx) = COND #( WHEN ls_catalogo-bsart = 'ZUB4' THEN 'P101' ELSE 'PB00' ).
    ENDLOOP.

    SORT it_ebeln BY ebeln.
    DELETE ADJACENT DUPLICATES FROM it_ebeln.


    LOOP AT it_ebeln INTO ls_ebeln.

      CLEAR: lt_itemsx[], lt_return1[], lt_items[], lt_cond[], lt_cond_aux[], lt_items_aux[].

*      READ TABLE lt_catalogo INTO ls_catalogo
*      WITH  KEY ebeln = ls_ebeln-ebeln.

      "IF sy-subrc EQ 0.

      CALL FUNCTION 'BAPI_PO_GETDETAIL1'
        EXPORTING
          purchaseorder = ls_ebeln-ebeln
        IMPORTING
          poheader      = ls_poheader
        TABLES
          return        = lt_return1
          poitem        = lt_items
          pocond        = lt_cond.

      LOOP AT lt_catalogo INTO DATA(ls_data) WHERE ebeln = ls_ebeln-ebeln.
        READ TABLE lt_items INTO DATA(ls_item_aux) WITH KEY po_item = ls_data-ebelp.
        IF sy-subrc EQ 0.
          APPEND ls_item_aux TO lt_items_aux.
        ENDIF.
      ENDLOOP.
      lt_items[] = lt_items_aux[].

      READ TABLE lt_return1 TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        DELETE lt_cond WHERE cond_type NE lv_condx.

        LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<fs_items>).
          DATA(lv_tabix) = sy-tabix.
          CLEAR ls_itemsx.

          "READ TABLE lt_catalogo INTO ls_catalogo INDEX lv_tabix.

          READ TABLE lt_catalogo INTO ls_catalogo
           WITH  KEY ebeln = ls_ebeln-ebeln
           ebelp = <fs_items>-po_item.

          IF sy-subrc EQ 0.
            IF lv_condx <> 'P101'.
              <fs_items>-net_price = ls_catalogo-netpr.
              ls_itemsx-net_price = abap_true.
            ENDIF.
            ls_itemsx-po_item = <fs_items>-po_item.
            APPEND ls_itemsx TO lt_itemsx.

            DATA(lv_cont) = 0.

            LOOP AT lt_cond INTO DATA(ls_cond) WHERE itm_number = <fs_items>-po_item AND cond_type = lv_condx.

              AT END OF itm_number.
                lv_flag = 'X'.
              ENDAT.
              lv_cont = lv_cont + 1.
              IF lv_flag = abap_true.
                CLEAR: ls_condx.
                lv_p2 = ls_cond-cond_value.
                ls_cond-cond_value = ls_catalogo-netpr.
**                ls_cond-cond_count = lv_cont.

                ls_condx-cond_value = abap_true.
                ls_condx-cond_count = abap_true.
                ls_condx-itm_number = <fs_items>-po_item.
                ls_condx-cond_st_no = ls_cond-cond_st_no.
                ls_condx-change_id  = abap_true.
                IF ls_catalogo-netpr GT lv_p2.
                  ls_cond-change_id  = 'U'.
                ELSEIF ls_catalogo-netpr LT lv_p2.
                  lv_cont = lv_cont - 1.
                  ls_cond-change_id  = 'U'.
                ENDIF.

                IF ls_cond-change_id IS NOT INITIAL.
                  APPEND ls_cond TO lt_cond_aux.
                  APPEND ls_condx TO lt_condx.
                ENDIF.
              ENDIF.
              lv_flag = abap_false.

            ENDLOOP.

            IF lt_cond[] IS INITIAL.
              CLEAR: ls_condx.
              lv_p2 = ls_cond-cond_value.

              ls_cond-itm_number = <fs_items>-po_item.
              ls_cond-cond_st_no = '001'.
              ls_cond-cond_value = ls_catalogo-netpr.
              ls_cond-cond_count = 1.
              ls_cond-cond_p_unt = 1.
              ls_cond-currency = ls_poheader-currency.
              ls_cond-currency_iso = ls_poheader-currency_iso.


              ls_condx-cond_value = abap_true.
              ls_condx-cond_count = abap_true.
              ls_condx-itm_number = <fs_items>-po_item.
              ls_condx-cond_st_no = '001'.
              ls_condx-cond_count = abap_true.
              ls_condx-cond_p_unt = abap_true.
              ls_condx-currency = abap_true.
              ls_condx-currency_iso = abap_true.

              ls_condx-change_id  = abap_true.
              IF ls_catalogo-netpr GT lv_p2.
                ls_cond-change_id  = 'I'.
              ELSEIF ls_catalogo-netpr LT lv_p2.
                lv_cont = lv_cont - 1.
                ls_cond-change_id  = 'I'.
              ENDIF.

              IF ls_cond-change_id IS NOT INITIAL.
                APPEND ls_cond TO lt_cond_aux.
                APPEND ls_condx TO lt_condx.
              ENDIF.
            ENDIF.


          ENDIF.
        ENDLOOP.

        CALL FUNCTION 'BAPI_PO_CHANGE'
          EXPORTING
            purchaseorder = ls_catalogo-ebeln
          TABLES
            return        = lt_return1
            poitem        = lt_items
            poitemx       = lt_itemsx
            pocond        = lt_cond_aux
            pocondx       = lt_condx.
        READ TABLE lt_return1 TRANSPORTING NO FIELDS WITH KEY type = 'E'.
        IF sy-subrc NE 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ELSE.

        ENDIF.
        LOOP AT lt_return1 INTO ls_return.
          APPEND ls_return TO lt_return.
        ENDLOOP.
      ELSE.
        LOOP AT lt_return1 INTO ls_return.
          APPEND ls_return TO lt_return.
        ENDLOOP.
      ENDIF.

      "ENDIF.

    ENDLOOP.

    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = lt_return.

  ENDMETHOD.

  METHOD m_get_data.

    DATA: ls_catalogo TYPE zes_catalogo_oc,
          lt_ekko     TYPE TABLE OF ekko,
          lv_kposn    TYPE kposn.

    CLEAR: lt_catalogo_alv[], lt_ekko[].

    lt_catalogo_alv[] = lt_catalogo[].
    DELETE ADJACENT DUPLICATES FROM lt_catalogo_alv COMPARING ebeln.

    SELECT * FROM ekko INTO TABLE lt_ekko
      FOR ALL ENTRIES IN lt_catalogo_alv
      WHERE ebeln = lt_catalogo_alv-ebeln.

    "SELECT a~ebeln, a~bsart, b~ebelp, b~matnr, b~txz01, b~netpr, a~waers FROM ekpo AS b
    SELECT a~bsart,a~ebeln, b~ebelp, b~matnr, b~txz01, b~netpr, a~waers FROM ekpo AS b
      JOIN @lt_ekko AS a
      ON a~ebeln = b~ebeln
      INTO TABLE @DATA(lt_catalogo_aux).
    IF sy-subrc EQ 0.

      rt_catalogo_alv[] = lt_catalogo_aux[].

    ENDIF.

    SELECT knumv, kposn, stunr, zaehk, kschl, kbetr, waers FROM prcd_elements INTO TABLE @DATA(lt_prcd_elements)
      FOR ALL ENTRIES IN @lt_ekko
      WHERE knumv = @lt_ekko-knumv.
    IF sy-subrc EQ 0.

      LOOP AT lt_catalogo_aux ASSIGNING FIELD-SYMBOL(<ls_catalogo>) WHERE netpr IS INITIAL.

        DATA(lv_condx) = COND #( WHEN <ls_catalogo>-bsart = 'ZUB4' THEN 'P101' ELSE 'PB00' ).

        lv_kposn = |{ <ls_catalogo>-ebelp ALPHA = IN }|.

        DATA(ls_prcd_elements) = lt_prcd_elements[ kposn = lv_kposn zaehk = '001' kschl = lv_condx ].
        <ls_catalogo>-netpr = ls_prcd_elements-kbetr.

      ENDLOOP.

      rt_catalogo_alv[] = lt_catalogo_aux[].

    ENDIF.

    SORT rt_catalogo_alv BY ebeln ebelp.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_reporte IMPLEMENTATION.

  METHOD m_show_alv.
    DATA: lc_msg      TYPE REF TO cx_salv_msg,
          lo_sorts    TYPE REF TO cl_salv_sorts,
          lv_col_name TYPE lvc_fname,
          lv_name     TYPE string,
          lt_fcat     TYPE lvc_t_fcat,
          lv_name_txt TYPE scrtext_l.

    DATA(it_alv_1) = it_alv[].

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = ob_alv
          CHANGING
            t_table      = it_alv_1.
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

    ob_column ?= ob_columns->get_column( 'BSART' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'BSART' position = 1 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'Clase de documento' ).
    ob_column->set_long_text( 'Clase de documento' ).
    ob_column->set_key( if_salv_c_bool_sap=>true ).

    ob_column ?= ob_columns->get_column( 'EBELN' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'EBELN' position = 2 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'Orden de Compra' ).
    ob_column->set_long_text( 'Orden de Compra' ).
    ob_column->set_key( if_salv_c_bool_sap=>true ).

    ob_column ?= ob_columns->get_column( 'EBELP' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'EBELP').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Posicion' ).
    ob_column->set_medium_text( 'Posicion' ).
    ob_column->set_long_text( 'Posicion' ).

    ob_column ?= ob_columns->get_column( 'MATNR' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MATNR').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Material' ).
    ob_column->set_medium_text( 'Material' ).
    ob_column->set_long_text( 'Material' ).

    ob_column ?= ob_columns->get_column( 'TXZ01' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'TXZ01').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'Texto breve' ).
    ob_column->set_long_text( 'Texto breve' ).

    ob_column ?= ob_columns->get_column( 'NETPR' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'NETPR').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Condicion' ).
    ob_column->set_medium_text( 'Condicion' ).
    ob_column->set_long_text( 'Condicion' ).

    ob_column ?= ob_columns->get_column( 'WAERS' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'WAERS').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Moneda' ).
    ob_column->set_medium_text( 'Moneda' ).
    ob_column->set_long_text( 'Moneda' ).

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

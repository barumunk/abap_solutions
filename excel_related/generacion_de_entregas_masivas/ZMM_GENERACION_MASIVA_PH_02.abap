*&---------------------------------------------------------------------*
*& Include          ZMM_GENERACION_MASIVA_PH_02
*&---------------------------------------------------------------------*


CLASS lcl_catalogo IMPLEMENTATION.

  METHOD m_process.

    DATA: lv_delivery TYPE bapishpdelivnumb-deliv_numb,
          ls_ship     TYPE bapidlvcreateheader,
          lt_items    TYPE TABLE OF bapidlvreftosalesorder,
          lv_response TYPE c.

    CREATE OBJECT obj_reporte.

    me->m_read_layout( ).

    lv_response = me->m_check_layout( ).

    obj_reporte->m_show_alv_check( it_alv = lt_alv_check ).

  ENDMETHOD.

  METHOD m_create.

    me->m_process_layout( ).
    obj_reporte->m_show_alv( it_alv = lt_entregas ).

  ENDMETHOD.

  METHOD m_load_data.

    DATA:
      lt_return        TYPE TABLE OF bapiret2,
      lt_created_itms  TYPE TABLE OF bapideliciouscreateditems,
      ls_entrega       TYPE ts_entrega,
      lv_date_external TYPE lv50c-datbi,
      lv_delivery      TYPE bapishpdelivnumb-deliv_numb,
      lt_request       TYPE TABLE OF bapideliciousrequest.

    CLEAR: lt_return[], lt_request[], lt_created_itms[], ls_entrega.

    lt_request[] = i_request[].
    ls_entrega = i_entrega.

    CALL FUNCTION 'BAPI_DELIVERYPROCESSING_EXEC'
      TABLES
        request      = lt_request
        return       = lt_return
        createditems = lt_created_itms.

    READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      ls_entrega-mensage = ls_return-message.
      ls_entrega-icon    = icon_red_light.
    ELSE.
      READ TABLE lt_created_itms INTO DATA(ls_created_item) INDEX 1.
      IF sy-subrc EQ 0.
        ls_entrega-mensage = |La entrega { ls_created_item-document_numb } se ha creado con exito|.
        ls_entrega-icon = icon_green_light.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        READ TABLE lt_request INTO DATA(ls_request_aux) INDEX 1.
        DESCRIBE TABLE lt_request LINES DATA(lv_lines).
        "Se Ejecuta el Batch para obtener los mensajes de error.
        PERFORM bdc_dynpro      USING 'SAPMV50A' '4001'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'LV50C-BIPOS'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=ENT2'.
        PERFORM bdc_field       USING 'LIKP-VSTEL'
                                      i_werks.
        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal = ls_request_aux-delivery_date
          IMPORTING
            date_external = lv_date_external.

        PERFORM bdc_field       USING 'LV50C-DATBI'
                                      lv_date_external.
        PERFORM bdc_field       USING 'LV50C-VBELN'
                                      ls_request_aux-document_numb.
        PERFORM bdc_field       USING 'LV50C-ABPOS'
                                      '000010'.
        READ TABLE lt_request INTO ls_request_aux INDEX lv_lines.
        PERFORM bdc_field       USING 'LV50C-BIPOS'
                                      ls_request_aux-document_item.
        PERFORM bdc_dynpro      USING 'SAPMV50A' '4001'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '/EBACKE_T'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'LIKP-VSTEL'.
        DATA lv_mode TYPE c VALUE 'N'.
        CALL TRANSACTION 'VL01N' USING bdcdata
                                 MODE lv_mode
                                 MESSAGES INTO messtab.

        READ TABLE messtab INTO DATA(ls_messtab) INDEX 1.
        IF sy-subrc EQ 0.

          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              id        = ls_messtab-msgid
              lang      = sy-langu
              no        = ls_messtab-msgnr
              v1        = ls_messtab-msgv1
              v2        = ls_messtab-msgv2
              v3        = ls_messtab-msgv3
              v4        = ls_messtab-msgv4
            IMPORTING
              msg       = ls_entrega-mensage
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

          ls_entrega-icon    = icon_red_light.
        ENDIF.
      ENDIF.

    ENDIF.

    APPEND ls_entrega TO lt_entregas.

  ENDMETHOD.

  METHOD m_process_layout.
    DATA: lv_flag       TYPE c,
          ls_entrega    TYPE ts_entrega,
          lt_data_aux   TYPE TABLE OF ty_catalogo,
          lt_request    TYPE TABLE OF bapideliciousrequest,
          ls_request    TYPE bapideliciousrequest,
          lv_deliveries TYPE bapidlvcreateheader-num_deliveries,
          lv_dat        TYPE lfdat_a,
          lv_date       TYPE bapidlvcreateheader-due_date,
          lv_werks      TYPE bapidlvcreateheader-ship_point,
          lv_posnr      TYPE posnr,
          lv_cantidad   TYPE i,
          lv_mensage    TYPE string,
          lv_vbeln      TYPE vbeln_va.

    CLEAR: ls_entrega, lt_entregas[].

    SORT lt_data BY pedido werks no_pedido.

    lt_data_aux[] = lt_data[].

    DELETE ADJACENT DUPLICATES FROM lt_data_aux COMPARING pedido werks no_pedido texto_cliente.
    LOOP AT lt_data_aux INTO DATA(ls_data_aux).

      lv_posnr = '000010'.

      ls_entrega-pedido = ls_data_aux-pedido.
      ls_entrega-werks = ls_data_aux-werks.
      lv_werks = ls_entrega-werks.
      ls_entrega-no_pedido = ls_data_aux-no_pedido.
      ls_entrega-texto_cliente = ls_data_aux-texto_cliente.

      ls_data_aux-pedido = |{ ls_data_aux-pedido ALPHA = IN }|.

*Validacion de existencia de pedido referencia
      SELECT SINGLE *
        INTO @DATA(ls_vbak)
        FROM vbak
        WHERE vbeln = @ls_data_aux-pedido.
      IF sy-subrc NE 0.
        ls_entrega-mensage = 'El pedido no existe'.
        APPEND ls_entrega TO lt_entregas.
        CONTINUE.
      ENDIF.

      CLEAR: ls_request, lt_request[].

      LOOP AT lt_data INTO DATA(ls_data)
        WHERE pedido EQ ls_entrega-pedido
        AND werks EQ ls_data_aux-werks
        AND no_pedido EQ ls_data_aux-no_pedido
        AND texto_cliente EQ ls_data_aux-texto_cliente.

*        APPEND ls_data TO ls_entrega-pedido_t.
        SELECT SINGLE *
          INTO @DATA(ls_vbap)
          FROM vbap
          WHERE vbeln = @ls_data_aux-pedido
          AND posnr = @lv_posnr.

        ls_request-document_numb =  ls_vbap-vbeln.
        ls_request-document_item =  ls_vbap-posnr.
        ls_request-ship_to = ls_data-destinatario.
        ls_request-sold_to = ls_data-cliente.

        ls_request-sales_organisation = ls_vbak-vkorg.
        ls_request-distribution_channel = ls_vbak-vtweg.
        ls_request-division = ls_vbak-spart.
        ls_request-plant = ls_vbap-werks.
        lv_cantidad = ls_data-cantidad.
        ls_request-quantity_sales_uom = lv_cantidad.

        ls_request-sales_unit = ls_vbap-vrkme.
        ls_request-base_uom = ls_vbap-meins.
        ls_request-material = ls_vbap-matnr.
        ls_request-matl_group = ls_data-matkl.
        lv_dat = ls_data-fecha.
        ls_request-delivery_date = lv_dat. "
        lv_date = lv_dat.
*        ls_request-delivery_time = ''.
        ls_request-transp_plan_date = ls_vbak-bstdk. "
        ls_request-loading_date = ls_vbak-bstdk.
        ls_request-goods_issue_date = ls_vbak-bstdk.
        ls_request-plant = ls_data-werks.

*        ls_request-extdelv_no = '98765'.
        ls_request-document_type = 'A'. "Delivery
        ls_request-document_type_predecessor = 'A'. "Sales Ord
        ls_request-document_type_delivery = 'ZBLF'.

        APPEND ls_request TO lt_request.
        lv_posnr += 10.

      ENDLOOP.

      me->m_load_data( i_werks = ls_entrega-werks i_date = lv_dat i_request = lt_request i_entrega = ls_entrega ).
      CLEAR ls_entrega.


    ENDLOOP.

  ENDMETHOD.

  METHOD m_read_layout.

    DATA: lt_intern_excel TYPE TABLE OF zalsmex_tabline,
          ls_catalogo     TYPE ty_catalogo.
    DATA: lv_row TYPE i.
    REFRESH lt_intern_excel.

    CHECK p_file IS NOT INITIAL.


    CLEAR lv_row.

    SELECT SINGLE low FROM tvarvc INTO @DATA(lv_low) WHERE name EQ 'Z_EXCEL_ROW'.
    IF sy-subrc EQ 0.
      lv_row = lv_low.
    ELSE.
      lv_row = '3000'.
    ENDIF.

    CALL FUNCTION 'ZALSMEX_TABLINE_2_TT'
      EXPORTING
        filename                = p_file
        i_begin_col             = c_scol
        i_begin_row             = c_srow
        i_end_col               = '100'
        i_end_row               = lv_row
      TABLES
        intern                  = lt_intern_excel
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc NE 0.
      MESSAGE 'Error en la lectura del Layout' TYPE 'E'.
    ELSE.
      LOOP AT lt_intern_excel INTO DATA(ls_excel).
        AT NEW row.
          CLEAR ls_catalogo.
        ENDAT.

        CASE ls_excel-col.
          WHEN '0001'.
            ls_catalogo-pedido = ls_excel-value.
          WHEN '0002'.
            ls_catalogo-werks = ls_excel-value.
          WHEN '0003'.
            ls_catalogo-no_pedido = ls_excel-value.
          WHEN '0004'.
            ls_catalogo-matkl = ls_excel-value.
          WHEN '0005'.
            ls_catalogo-cliente = ls_excel-value.
          WHEN '0006'.
            ls_catalogo-destinatario = ls_excel-value.
          WHEN '0007'.
            ls_catalogo-material = ls_excel-value.
          WHEN '0008'.
            ls_catalogo-variante = ls_excel-value.
          WHEN '0009'.
            ls_catalogo-descripcion = ls_excel-value.
          WHEN '0010'.
            ls_catalogo-talla = ls_excel-value.
          WHEN '0011'.
            ls_catalogo-texto_cliente = ls_excel-value.
          WHEN '0012'.
            ls_catalogo-cantidad = ls_excel-value.
          WHEN '0013'.
            DATA: lv_date(50).
            lv_date = ls_excel-value.
            CONDENSE lv_date.
            ls_catalogo-fecha = me->m_date_format( i_date = lv_date ).
            CLEAR lv_date.
        ENDCASE.

        AT END OF row.
          APPEND ls_catalogo TO lt_data.
        ENDAT.
      ENDLOOP.
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

  METHOD m_check_layout.

    DATA: ls_entrega     TYPE ts_entrega,
          lt_data_aux    TYPE TABLE OF ty_catalogo,
          ls_catalogo    TYPE ty_catalogo,
          ls_data        TYPE ty_catalogo,
          lv_cont        TYPE i,
          lt_entrega_aux TYPE TABLE OF ts_entrega.

    lt_data_aux[] = lt_data[].

    CLEAR: lv_cont, lt_alv_check[].

    DELETE ADJACENT DUPLICATES FROM lt_data_aux COMPARING pedido werks no_pedido texto_cliente.
    LOOP AT lt_data_aux INTO DATA(ls_data_aux).
      CLEAR ls_data.
      DATA(lv_pedido) = ls_data_aux-pedido.
      ls_data_aux-pedido = |{ ls_data_aux-pedido ALPHA = IN }|.

*Validacion de existencia de pedido referencia
      SELECT SINGLE *
        INTO @DATA(ls_vbak)
        FROM vbak
        WHERE vbeln = @ls_data_aux-pedido.
      IF sy-subrc NE 0 AND sy-sysid NE 'DS4'.
*** Añadir mensage tipo Return comentando que el pedido X no existe...
        CONTINUE.
      ENDIF.

      lv_cont += 1.
      LOOP AT lt_data INTO ls_data
        WHERE pedido EQ lv_pedido
        AND werks EQ ls_data_aux-werks
        AND no_pedido EQ ls_data_aux-no_pedido
        AND texto_cliente EQ ls_data_aux-texto_cliente.
        ls_data-index = lv_cont.
        APPEND ls_data TO lt_alv_check.
        CLEAR ls_data-index.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_reporte IMPLEMENTATION.

  METHOD m_show_alv.
    DATA: lc_msg       TYPE REF TO cx_salv_msg,
          lo_sorts     TYPE REF TO cl_salv_sorts,
          lv_col_name  TYPE lvc_fname,
          it_excluding TYPE STANDARD TABLE OF ui_func,
          lv_name      TYPE string,
          lt_fcat      TYPE lvc_t_fcat,
          lv_name_txt  TYPE scrtext_l,
          obj_alv_grid TYPE REF TO cl_gui_alv_grid,
          vg_container TYPE REF TO cl_gui_custom_container.

    DATA(ti_data) = obj_catalogo->lt_entregas[].
    ti_data[] = it_alv[].

    DATA: it_fcat TYPE STANDARD TABLE OF lvc_s_fcat,
          wa_fcat TYPE lvc_s_fcat.

    CREATE OBJECT vg_container
      EXPORTING
        container_name = 'CC_ALV2'.

    CREATE OBJECT obj_alv_grid
      EXPORTING
        i_parent = vg_container.

    REFRESH: it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'PEDIDO'."'BSART'.
    wa_fcat-scrtext_l = 'Pedido'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'WERKS'."'BSART'.
    wa_fcat-scrtext_l = 'Centro'.
    APPEND wa_fcat TO it_fcat.


    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'TEXTO_CLIENTE'."'BSART'.
    wa_fcat-scrtext_l = 'Texto Cliente'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'MENSAGE'."'BSART'.
    wa_fcat-scrtext_l = 'Mensage'.
    APPEND wa_fcat TO it_fcat.

    CALL METHOD obj_alv_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = it_excluding
*       is_layout            = vl_layout
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = ti_data.


    CALL SCREEN 0101.

  ENDMETHOD.

  METHOD m_show_alv_check.
    DATA: lc_msg       TYPE REF TO cx_salv_msg,
          lo_sorts     TYPE REF TO cl_salv_sorts,
          lv_col_name  TYPE lvc_fname,
          lv_name      TYPE string,
          lt_fcat      TYPE lvc_t_fcat,
          lv_name_txt  TYPE scrtext_l,
          it_excluding TYPE STANDARD TABLE OF ui_func,
          wa_exclude   TYPE ui_func,
          obj_alv_grid TYPE REF TO cl_gui_alv_grid,
          vg_container TYPE REF TO cl_gui_custom_container.

    DATA: it_fcat TYPE STANDARD TABLE OF lvc_s_fcat,
          wa_fcat TYPE lvc_s_fcat.

    DATA(ti_data) = obj_catalogo->lt_alv_check[].
    ti_data[] = it_alv[].

    CREATE OBJECT vg_container
      EXPORTING
        container_name = 'CC_ALV'.

    CREATE OBJECT obj_alv_grid
      EXPORTING
        i_parent = vg_container.

    REFRESH: it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'INDEX'."'BSART'.
    wa_fcat-scrtext_l = 'Index'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'PEDIDO'."'BSART'.
    wa_fcat-scrtext_l = 'Pedido'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'WERKS'."'BSART'.
    wa_fcat-scrtext_l = 'Centro'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'NO_PEDIDO'."'BSART'.
    wa_fcat-scrtext_l = 'No. Pedidos del Cliente OC'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'MATKL'."'BSART'.
    wa_fcat-scrtext_l = 'Grupo de Articulo'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'CLIENTE'."'BSART'.
    wa_fcat-scrtext_l = 'Cliente'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'DESTINATARIO'."'BSART'.
    wa_fcat-scrtext_l = 'Destinatario'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'MATERIAL'."'BSART'.
    wa_fcat-scrtext_l = 'Material'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'VARIANTE'."'BSART'.
    wa_fcat-scrtext_l = 'Variante'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'DESCRIPCION'."'BSART'.
    wa_fcat-scrtext_l = 'Descripcion'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'TALLA'."'BSART'.
    wa_fcat-scrtext_l = 'Talla'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'TEXTO_CLIENTE'."'BSART'.
    wa_fcat-scrtext_l = 'Texto Cliente'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'CANTIDAD'."'BSART'.
    wa_fcat-scrtext_l = 'Cantidad'.
    APPEND wa_fcat TO it_fcat.

    CLEAR: wa_fcat.
    wa_fcat-fieldname = 'FECHA'."'BSART'.
    wa_fcat-scrtext_l = 'Fecha'.
    APPEND wa_fcat TO it_fcat.

    CALL METHOD obj_alv_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    SET HANDLER handle_toolbar      FOR obj_alv_grid.
    SET HANDLER handle_user_command FOR obj_alv_grid.

    CALL METHOD obj_alv_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = it_excluding
*       is_layout            = vl_layout
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = ti_data.


    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD handle_toolbar.
    DATA: wa_button TYPE stb_button.

    wa_button-function = 'ENVIAR'.
    wa_button-icon     = icon_okay.
    wa_button-text     = 'Enviar'.
    wa_button-quickinfo = 'Crear Entrega'.
*    wa_button-disabled = space.
    APPEND wa_button TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tool>).

      CASE <fs_tool>-function.
        WHEN '&LOCAL&APPEND' OR '&LOCAL&INSERT_ROW'
          OR '&LOCAL&DELETE_ROW' OR '&LOCAL&COPY_ROW'.
          <fs_tool>-disabled = abap_true.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD m_set_pf_status.

    SET PF-STATUS 'STANDARD'.
    ob_alv->set_screen_status(
    pfstatus      =  'STANDARD'
    report       = sy-repid
    set_functions = ob_alv->c_functions_all ).

  ENDMETHOD.

  METHOD handle_user_command.

    CREATE OBJECT obj_catalogo.

    DATA lv_answer TYPE c.

    CASE e_ucomm.
      WHEN 'ENVIAR'.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmacion'
            text_question         = 'Desea confirmar el envio de informacion?'
            text_button_1         = 'Sí'
            text_button_2         = 'No'
            default_button        = '1'
            display_cancel_button = 'X'
            start_column          = 25
            start_row             = 6
          IMPORTING
            answer                = lv_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF lv_answer EQ 1.
          obj_catalogo->m_create( ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.

MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO TRANSACTION 'ZPMAYOREO'.
    WHEN 'EXIT'.
      LEAVE TO TRANSACTION 'ZPMAYOREO'.
    WHEN 'CANCEL'.
      LEAVE TO TRANSACTION 'ZPMAYOREO'.
    WHEN '%EX'.
      LEAVE TO TRANSACTION 'ZPMAYOREO'.
  ENDCASE.

ENDMODULE.
MODULE status_0100 OUTPUT.
* SET PF-STATUS 'STANDARD'.
* SET TITLEBAR 'TITULO_001'.
ENDMODULE.
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.

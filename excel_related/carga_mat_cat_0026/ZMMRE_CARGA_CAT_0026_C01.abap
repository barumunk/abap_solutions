*&---------------------------------------------------------------------*
*& Include          ZMMRE_CARGA_CAT_0026_C01
*&---------------------------------------------------------------------*
CLASS lcl_catalogo IMPLEMENTATION.

  METHOD m_get_data.

    CLEAR gv_material.
    REFRESH lt_intern_excel.
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

    IF lt_intern_excel[] IS INITIAL.

      MESSAGE 'Error al Leer el Excel' TYPE 'E' DISPLAY LIKE 'I'.

      "    ELSEIF sy-subrc EQ 0.

      "      READ TABLE lt_intern_excel INTO DATA(ls_excel) WITH KEY row = '0001' col = '0009'.
      "      IF sy-subrc EQ 0.
      "        gv_material = ls_excel-value.
      "      ENDIF.

    ENDIF.


  ENDMETHOD.

  METHOD m_fill_data.

    DATA: lt_excel_tab TYPE TABLE OF alsmex_tabline.

    DATA: ls_plantdata          TYPE bapie1marcrt,
          ls_plantext           TYPE bapie1marcextrt,
          ls_plantextx          TYPE bapie1marcextrtx,
          ls_plantdatax         TYPE bapie1marcrtx,
          ls_valuationdata      TYPE bapie1mbewrt,
          ls_valuationdatax     TYPE bapie1mbewrtx,
          ls_salesdata          TYPE bapie1mvkert,
          ls_salesdatax         TYPE bapie1mvkertx,
          ls_inforecordgeneral  TYPE bapieina,
          ls_inforecordpurchorg TYPE wrfbapieine,
          ls_alv                TYPE ty_alv,
          wa_ausp               TYPE ausp,
          wa_klah               TYPE klah,
          wa_ksml               TYPE ksml.

    DATA: lv_matnr  TYPE matnr,
          lv_matnr2 TYPE matnr18,
          lv_value  TYPE char200,
          lv_count  TYPE p DECIMALS 0,
          lv_name   TYPE string.

    DATA:objectkey   TYPE  bapi1003_key-object,
         objecttable TYPE  bapi1003_key-objecttable,
         classnum    TYPE  bapi1003_key-classnum,
         classtype   TYPE  bapi1003_key-classtype.

    CLEAR: ls_headdata.
    "REFRESH: lt_return, LT_unitsofmeasure, LT_unitsofmeasurex, lt_materialdescription, lt_CHARACTERISTICVALUE, lt_CHARACTERISTICVALUEx, gt_alv, lt_VARIANTSKEYS.
    REFRESH: lt_return[], lt_unitsofmeasure[], lt_unitsofmeasurex[],
     lt_materialdescription[], lt_characteristicvalue[], lt_characteristicvaluex[],
      gt_alv[], lt_variantskeys[].

    lt_excel_tab[] = lt_intern_excel[].

    DELETE ADJACENT DUPLICATES FROM lt_excel_tab COMPARING row.
    IF sy-subrc EQ 0.
      DESCRIBE TABLE lt_excel_tab LINES DATA(lv_lines).
    ENDIF.

    "Validacion de datos.

    LOOP AT lt_excel_tab INTO DATA(ls_excel1) FROM 2.
      DATA(lv_tabix1) = sy-tabix.
      "head
      CLEAR: lv_value, ls_alv.

      PERFORM f_read_data TABLES lt_intern_excel
             USING ls_excel1-row
                   '0002'
            CHANGING lv_value.

      DATA(material) = lv_value.

      LOOP AT lt_intern_excel INTO DATA(ls_intern_excel1) WHERE row = ls_excel1-row AND row NE '0001' AND col GT '0003'.

        CLEAR: lv_value, lv_name.
        PERFORM f_read_data TABLES lt_intern_excel
        USING ls_excel1-row
              ls_intern_excel1-col
       CHANGING lv_value.

        READ TABLE lt_intern_excel INTO DATA(ls_intern_excel_aux3) WITH KEY row = '0001' col = ls_intern_excel1-col.
        lv_name = ls_intern_excel_aux3-value.

        DATA: err TYPE  i.

        SELECT SINGLE * FROM cabn INTO @DATA(wa_cabn)
          WHERE atnam = @lv_name.

        IF wa_cabn-atein = 'X'. " LA CARACTERISTICA ES DE UN SOLO VALOR.

          err = 1.
          CONDENSE lv_value.

          SELECT SINGLE * FROM wrf_charvalt INTO @DATA(wa_charval)
            WHERE atinn = @wa_cabn-atinn
            AND atwtb LIKE @lv_value
            AND spras = @sy-langu.

          IF sy-subrc EQ 0.
            err = 0.
          ELSE.
            SELECT SINGLE * FROM zvcaracterist INTO @DATA(wa_view)
              WHERE atnam = @lv_name
              AND atwtb LIKE @lv_value.

            IF sy-subrc = 0.
              err = 0.
            ENDIF.
          ENDIF.

          IF err = 1.

            ls_alv-index = lv_tabix1.
            ls_alv-type = 'E'.
            ls_alv-icon = '1'.
            ls_alv-material = material.
            CONCATENATE  'Valor ' lv_value 'para la caracteristica' lv_name 'no es valido.'  INTO ls_alv-return SEPARATED BY space.
            APPEND ls_alv TO gt_alv.

          ENDIF.

        ELSE.

          SPLIT lv_value AT ',' INTO TABLE DATA(lt_str_split).

          LOOP AT lt_str_split INTO DATA(lv_value1).
            err = 1.

            CONDENSE lv_value1.

            SELECT SINGLE * FROM wrf_charvalt INTO @DATA(wa_charval1)
              WHERE atinn = @wa_cabn-atinn
              AND atwtb LIKE @lv_value1
              AND spras = @sy-langu.

            IF sy-subrc EQ 0.
              err = 0.
            ELSE.
              SELECT SINGLE * FROM zvcaracterist INTO @DATA(wa_view1)
                WHERE atnam = @lv_name
                AND atwtb LIKE @lv_value1.

              IF sy-subrc = 0.
                err = 0.
              ENDIF.
            ENDIF.

            IF err = 1.

              ls_alv-index = lv_tabix1.
              ls_alv-type = 'E'.
              ls_alv-icon = '1'.
              ls_alv-material = material.
              CONCATENATE  'Valor ' lv_value1 'para la caracteristica' lv_name 'no es valido.'  INTO ls_alv-return SEPARATED BY space.
              APPEND ls_alv TO gt_alv.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

    IF NOT gt_alv[] IS INITIAL.
      RETURN.
    ENDIF.

*-------------------------------------------
    LOOP AT lt_excel_tab INTO DATA(ls_excel) FROM 2.
      DATA(lv_tabix) = sy-tabix.
      "head
      CLEAR: lv_value, ls_alv.

**
*      ls_headdata-function = '026'.
      PERFORM f_read_data TABLES lt_excel_tab
           USING ls_excel-row
                 '0001'
          CHANGING lv_value.

      ls_headdata-matl_group = lv_value.
      ls_headdata-config_class_name = ls_headdata-matl_group.
      ls_headdata-config_class_type = '026'.

      PERFORM f_read_data TABLES lt_intern_excel
                   USING ls_excel-row
                         '0002'
                  CHANGING lv_value.

      ls_alv-material = lv_value.
      lv_matnr2 = |{ lv_value ALPHA = IN }|.
      lv_matnr = |{ lv_value ALPHA = IN }|.
      ls_headdata-material        = ls_headdata-material_long = lv_matnr2.
      ls_headdata-no_appl_log = 'X'.
      ls_headdata-basic_view = abap_true.

      SELECT SINGLE * FROM mara INTO @DATA(ls_mara) WHERE matnr EQ @lv_matnr2.
      IF sy-subrc EQ 0.
        ls_headdata-matl_type  = ls_mara-mtart.
        ls_headdata-matl_cat   = ls_mara-attyp.
      ENDIF.
**



**
      LOOP AT lt_intern_excel INTO DATA(ls_intern_excel) WHERE row = ls_excel-row AND row NE '0001' AND col GT '0003'.

        CLEAR: lv_value, lv_name.
        PERFORM f_read_data TABLES lt_intern_excel
        USING ls_excel-row
              ls_intern_excel-col
       CHANGING lv_value.

        READ TABLE lt_intern_excel INTO DATA(ls_intern_excel_aux2) WITH KEY row = '0001' col = ls_intern_excel-col.

        SELECT SINGLE atinn FROM cabn INTO @DATA(xatinn)
          WHERE atnam = @ls_intern_excel_aux2-value.

        SELECT SINGLE * FROM ksml INTO wa_ksml
          WHERE imerk = xatinn.

        IF wa_ksml-klart = '026'.
          lv_name = ls_intern_excel_aux2-value.
          PERFORM f_load_data TABLES lt_characteristicvalue lt_characteristicvaluex lt_variantskeys USING lv_name lv_value lv_matnr2.
        ENDIF.

        IF wa_ksml-klart = '001'.

          lv_name = ls_intern_excel_aux2-value.

          IF lt_allocvaluescharnew[] IS INITIAL. "se cargan todos los datos ya en tabla

            IF ls_mara-spart = '11'.
              SELECT SINGLE * FROM klah INTO wa_klah
                WHERE class = 'CALZADO_IMP'
                AND klart = '001'.
              classnum = 'CALZADO_IMP'.
            ELSE.
              SELECT SINGLE * FROM klah INTO wa_klah
                WHERE class = 'ACCESORIOS_IMP'
                AND klart = '001'.
              classnum = 'ACCESORIOS_IMP'.
            ENDIF.

            SELECT * FROM ksml INTO TABLE  @DATA(lt_ksml)
              WHERE clint = @wa_klah-clint.

            LOOP AT lt_ksml INTO wa_ksml.
              wa_allocvaluescharnew-charact = wa_ksml-imerk.
              wa_allocvaluescharnew-value_char = ''.
              wa_allocvaluescharnew-value_char_long = ''.
              APPEND wa_allocvaluescharnew TO lt_allocvaluescharnew.
            ENDLOOP.



            SELECT * FROM ausp INTO TABLE @DATA(lt_ausp)
              WHERE objek = @lv_matnr2.

            LOOP AT lt_allocvaluescharnew INTO wa_allocvaluescharnew.

              READ TABLE lt_ausp INTO wa_ausp
              WITH KEY objek = lv_matnr2
              atinn = wa_allocvaluescharnew-charact.

              IF sy-subrc = 0 AND p_carg = 'X'.
                wa_allocvaluescharnew-value_char = wa_ausp-atwrt.
                wa_allocvaluescharnew-value_char_long = wa_ausp-atwrt.
              ENDIF.

              SELECT SINGLE atnam FROM cabn INTO wa_allocvaluescharnew-charact
                WHERE atinn = wa_allocvaluescharnew-charact.

              MODIFY lt_allocvaluescharnew FROM wa_allocvaluescharnew.

            ENDLOOP.

          ENDIF.

*          SELECT SINGLE atinn FROM cabn INTO xatinn
*            WHERE atnam = lv_name.

          FIELD-SYMBOLS <wa>  TYPE bapi1003_alloc_values_char.

          READ TABLE lt_allocvaluescharnew ASSIGNING <wa>
           WITH KEY charact = lv_name.

          IF sy-subrc = 0. " AND ( ( p_carg = 'X' AND  <wa>-value_char IS INITIAL ) OR ( p_carg = '' ) ).
            <wa>-value_char = lv_value.
            <wa>-value_char_long = lv_value.
          ENDIF.

        ENDIF.


*        IF sy-subrc EQ 0.
*
*          lv_name = ls_intern_excel_aux2-value.
*          PERFORM f_load_data TABLES lt_CHARACTERISTICVALUE lt_CHARACTERISTICVALUEx lt_VARIANTSKEYS USING lv_name lv_value lv_matnr2.
*
*        ENDIF.

      ENDLOOP.
**

      IF NOT lt_allocvaluescharnew[] IS INITIAL.

        DATA(lt_allocvaluescharnew_tmp) = lt_allocvaluescharnew[].

        objectkey = lv_matnr2.
        objecttable = 'MARA'.
        classtype = '001'.

        CALL FUNCTION 'BAPI_OBJCL_CHANGE'
          EXPORTING
            objectkey          = objectkey
            objecttable        = objecttable
            classnum           = classnum
            classtype          = classtype
          TABLES
            allocvaluesnumnew  = lt_allocvaluesnumnew
            allocvaluescharnew = lt_allocvaluescharnew
            allocvaluescurrnew = lt_allocvaluescurrnew
            return             = lt_return_001.

        IF sy-subrc EQ 0.

          ls_alv-clase = '001'.
          ls_alv-material = lv_matnr2.
          ls_alv-index = lv_tabix.
          CLEAR ls_alv-return.
          LOOP AT lt_return_001 INTO DATA(ls_return_001).

            ls_alv-type = ls_return_001-type.
            CONCATENATE ls_return_001-message '' INTO ls_alv-return SEPARATED BY space.
            APPEND ls_alv TO gt_alv.

          ENDLOOP.


          READ TABLE lt_return_001 INTO wa_return WITH KEY type = 'E'.
          IF sy-subrc NE 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ENDIF.

        ENDIF.

        "se recorren las variantes.
        SELECT * FROM mara INTO TABLE @DATA(it_mara)
          WHERE satnr = @lv_matnr2.

        "DATA: wa_mara TYPE mara.
        LOOP AT it_mara INTO DATA(wa_mara).

          objectkey = wa_mara-matnr.
          objecttable = 'MARA'.
          classtype = '001'.

          lt_allocvaluescharnew[] = lt_allocvaluescharnew_tmp[].

          CALL FUNCTION 'BAPI_OBJCL_CHANGE'
            EXPORTING
              objectkey          = objectkey
              objecttable        = objecttable
              classnum           = classnum
              classtype          = classtype
            TABLES
              allocvaluesnumnew  = lt_allocvaluesnumnew
              allocvaluescharnew = lt_allocvaluescharnew
              allocvaluescurrnew = lt_allocvaluescurrnew
              return             = lt_return_001.

          IF sy-subrc EQ 0.

            ls_alv-clase = '001'.
            ls_alv-material = wa_mara-matnr.
            ls_alv-index = lv_tabix.
            CLEAR ls_alv-return.
            LOOP AT lt_return_001 INTO DATA(ls_return_002).

              ls_alv-type = ls_return_002-type.
              CONCATENATE ls_return_002-message '' INTO ls_alv-return SEPARATED BY space.
              APPEND ls_alv TO gt_alv.

            ENDLOOP.


            READ TABLE lt_return_001 INTO wa_return WITH KEY type = 'E'.
            IF sy-subrc NE 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDIF.

      "ls_alv-index = lv_tabix.

      IF NOT lt_characteristicvalue[] IS INITIAL.

        CALL FUNCTION 'WRF_MATERIAL_MAINTAINDATA_RT'
          EXPORTING
            headdata             = ls_headdata
          TABLES
            return               = lt_return[]
*           materialdescription  = lt_materialdescription[]
            variantskeys         = lt_variantskeys
            characteristicvalue  = lt_characteristicvalue[]
            characteristicvaluex = lt_characteristicvaluex[].

        IF sy-subrc EQ 0.

*          DESCRIBE TABLE lt_return LINES DATA(lines2).
*
*          READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
**          IF sy-subrc EQ 0.
**            "ls_alv-estatus  = 'Clasificacion incorrecta, favor de verificarla'.
**            IF lines2 GT 1.
**              ls_alv-clasificacion = 'Material ya cuenta con clasificacion'.
**            ENDIF.
**          ELSE.
**            ls_alv-estatus  = 'Cargado/Actualizado'.
**          ENDIF.
*
*          CLEAR ls_alv-return.
*          LOOP AT lt_return INTO DATA(ls_return).
*            CONCATENATE ls_alv-return ls_return-message INTO ls_alv-return SEPARATED BY space.
*          ENDLOOP.
*
*        ELSE.
*
*          "ls_alv-estatus         = 'Error'.
*          READ TABLE lt_return INTO ls_return INDEX 1.
*          IF sy-subrc EQ 0.
*            ls_alv-return          = ls_return-message.
*          ENDIF.
*
*        ENDIF.

          ls_alv-clase = '026'.
          ls_alv-material = lv_matnr2.
          ls_alv-index = lv_tabix.
          CLEAR ls_alv-return.
          LOOP AT lt_return INTO DATA(ls_return).

            ls_alv-type = ls_return-type.
            CONCATENATE ls_return-message '' INTO ls_alv-return SEPARATED BY space.
            APPEND ls_alv TO gt_alv.

          ENDLOOP.

          READ TABLE lt_return INTO wa_return WITH KEY type = 'E'.
          IF sy-subrc NE 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ENDIF.

*      ELSE.
*        ls_alv-estatus = 'Ningun dato que actualizar'.
        ENDIF.
      ENDIF.


      CLEAR: ls_headdata.
      REFRESH: lt_return[], lt_unitsofmeasure[], lt_unitsofmeasurex[],
       lt_materialdescription[], lt_characteristicvalue[], lt_characteristicvaluex[],
       lt_variantskeys[], lt_allocvaluescharnew[].

    ENDLOOP.

    LOOP AT gt_alv INTO DATA(wa_alv).

      IF wa_alv-type = 'E'.
        wa_alv-icon = '1'.
      ELSE.
        wa_alv-icon = '3'.
      ENDIF.
      MODIFY gt_alv FROM wa_alv.
    ENDLOOP.

  ENDMETHOD.

  METHOD m_validation_data.

  ENDMETHOD.

  METHOD m_upload_data.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD m_show_alv.

    DATA: lc_msg TYPE REF TO cx_salv_msg.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = ob_alv
          CHANGING
            t_table      = ob_catalog->gt_alv[].
      CATCH cx_salv_msg INTO lc_msg .
    ENDTRY.

* Columns
    ob_columns = ob_alv->get_columns( ).

    ob_columns->set_key_fixation( value = abap_true ).

    ob_column ?= ob_columns->get_column( 'INDEX' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'INDEX' position = 1 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_key( if_salv_c_bool_sap=>true ).

    ob_column ?= ob_columns->get_column( 'MATERIAL' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MATERIAL' position = 2 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Material' ).
    ob_column->set_medium_text( 'Material' ).
    ob_column->set_long_text( 'Material' ).
    ob_column->set_key( if_salv_c_bool_sap=>true ).

*    ob_column ?= ob_columns->get_column( 'CLASIFICACION' ).
*    ob_columns->set_optimize( abap_true ).
**    ob_column->set_short_text( 'Clasificacion' ).
*    ob_column->set_medium_text( 'Clasificacion' ).
*    ob_column->set_long_text( 'Clasificacion' ).
*
*    ob_column ?= ob_columns->get_column( 'ESTATUS' ).
*    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Estatus' ).
*    ob_column->set_medium_text( 'Estatus' ).
*    ob_column->set_long_text( 'Estatus' ).

    ob_column ?= ob_columns->get_column( 'CLASE' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'CLASE' position = 3 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Clase' ).
    ob_column->set_medium_text( 'Clase' ).
    ob_column->set_long_text( 'Clase' ).

*    ob_column ?= ob_columns->get_column( 'TYPE' ).
*    ob_alv->get_columns( )->set_column_position( columnname = 'TYPE' position = 4 ).
*    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Type' ).
*    ob_column->set_medium_text( 'Type' ).
*    ob_column->set_long_text( 'Type' ).

    TRY.
        DATA(lr_columnas) = ob_alv->get_columns( ).
        lr_columnas->set_exception_column( value = 'ICON' ).
      CATCH cx_salv_data_error INTO DATA(lv_error).
        "e_code = lv_error->get_text( ).
    ENDTRY.


    ob_column ?= ob_columns->get_column( 'RETURN' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'RETURN' position = 5 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Mensaje' ).
    ob_column->set_medium_text( 'Mensaje' ).
    ob_column->set_long_text( 'Mensaje' ).

    ob_column ?= ob_columns->get_column( 'TYPE' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'TYPE' position = 6 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Type' ).
    ob_column->set_medium_text( 'Type' ).
    ob_column->set_long_text( 'Type' ).
    ob_column->set_technical( value = if_salv_c_bool_sap=>true ).


* Calling Set PF status method
    CALL METHOD m_set_pf_status
      CHANGING
        co_alv = ob_alv.

    "Calling the top of page method
    CALL METHOD m_set_top_of_page
      CHANGING
        co_alv = ob_alv.

    ob_alv->display( ).
  ENDMETHOD.

*  * Setting Default PF-Status
  METHOD m_set_pf_status.
    SET PF-STATUS 'STANDARD'.
    ob_alv->set_screen_status(
    pfstatus      =  'STANDARD'
    report       = sy-repid
    set_functions = ob_alv->c_functions_all ).

  ENDMETHOD.                    "set_pf_status

  METHOD m_set_top_of_page.
    DATA: ob_header  TYPE REF TO cl_salv_form_layout_grid,
          ob_h_label TYPE REF TO cl_salv_form_label,
          ob_h_flow  TYPE REF TO cl_salv_form_layout_flow.

    CREATE OBJECT ob_header.

*    ob_h_label = ob_header->create_label( row = 1 column = 1 ).
*    ob_h_label->set_text('').
*    ob_h_flow = ob_header->create_flow( row = 2 column = 1 ).
*    ob_h_flow->create_text( text = 'Reporte del mes' && | | && ob_catalog->lv_mes && | | && 'De' && | | && ob_catalog->lv_anio ).
*    ob_h_flow = ob_header->create_flow( row = 2 column = 2 ).
*    ob_h_flow = ob_header->create_flow( row = 3 column = 1 ).
*    ob_h_flow->create_text( text = 'Tipo de reporte:' && | | && ob_catalog->lv_tipo_reporte ).
*    ob_h_flow = ob_header->create_flow( row = 3 column = 2 ).
*    ob_h_flow = ob_header->create_flow( row = 4 column = 1 ).
*    ob_h_flow->create_text( text = 'Periodo:' && | | && ob_catalog->lv_periodo ).
*    ob_h_flow = ob_header->create_flow( row = 4 column = 2 ).
*    ob_h_flow = ob_header->create_flow( row = 5 column = 1 ).
*    ob_h_flow->create_text( text = 'Fechas' && | | &&  ob_catalog->lv_fecha_ini  && | | && ob_catalog->lv_fecha_fin ).
*    ob_h_flow = ob_header->create_flow( row = 5 column = 2 ).
*    ob_h_flow = ob_header->create_flow( row = 6 column = 1 ).
* Set the top of list using the header for Online
*    co_alv->set_top_of_list( ob_header ).
* Set the top of list using the header for Print
*    co_alv->set_top_of_list_print( ob_header ).
  ENDMETHOD.
ENDCLASS.

FORM f_read_data TABLES pt_table
                 USING p_row TYPE n
                       p_col TYPE n
                CHANGING p_value TYPE char200.

  DATA: lt_excel TYPE TABLE OF alsmex_tabline,
        lv_col   TYPE char4.

  lt_excel[] = pt_table[].

  "valor
  READ TABLE lt_excel INTO DATA(ls_table) WITH KEY row = p_row col = p_col.
  IF sy-subrc EQ 0.
    p_value = ls_table-value.
  ENDIF.

  CONDENSE p_value.

ENDFORM.

FORM f_load_data TABLES pt_caracteristic
                        pt_caracteristicx
                        pt_variants
                 USING p_name TYPE string
                       p_value TYPE char200
                       pv_matnr TYPE char18.

  DATA: lv_value TYPE char40,
        lv_atinn TYPE atinn.

  DATA: wa_characteristicvalue  TYPE bapie1ausprt,
        wa_characteristicvaluex TYPE bapie1ausprtx,
        wa_variantskeys         TYPE bapie1varkey.

  DATA: lt_characteristicvalue  TYPE TABLE OF bapie1ausprt,
        lt_characteristicvaluex TYPE TABLE OF bapie1ausprtx,
        lt_variantskeys         TYPE TABLE OF bapie1varkey.

  CLEAR:wa_characteristicvalue, wa_characteristicvaluex, wa_characteristicvalue.

  lt_characteristicvalue[]  = pt_caracteristic[].
  lt_characteristicvaluex[] = pt_caracteristicx[].
  lt_variantskeys[]         = pt_variants[].

  SPLIT p_value AT ',' INTO TABLE DATA(lt_str_split).

  LOOP AT lt_str_split INTO lv_value.

    IF p_carg EQ abap_true.
      CLEAR lv_atinn.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = p_name
        IMPORTING
          output = lv_atinn.

      SELECT SINGLE * FROM zvmatnrcalss26 INTO @DATA(ls_class26)
        WHERE matnr EQ @pv_matnr
        AND   atinn EQ @lv_atinn.
      IF sy-subrc EQ 0 AND ls_class26-atwrt IS NOT INITIAL OR ls_class26-atwrt NE space.
        CONTINUE.
      ENDIF.
    ENDIF.


    "CARACTERISTICAS MATERIAL GENERICO
*    wa_CHARACTERISTICVALUE-function = '026'.
    wa_characteristicvalue-material = pv_matnr.
    wa_characteristicvalue-char_name = p_name.
    wa_characteristicvalue-char_value = lv_value.
    wa_characteristicvalue-char_value_long = lv_value.
    "wa_characteristicvalue-char_val_dependency_code = 'X'.
    CONDENSE: wa_characteristicvalue-char_value, wa_characteristicvalue-char_name, wa_characteristicvalue-char_value_long.
    APPEND wa_characteristicvalue TO lt_characteristicvalue.

*    wa_CHARACTERISTICVALUEx-function = '026'.
    wa_characteristicvaluex-material = pv_matnr.
    wa_characteristicvaluex-char_name = p_name.
    wa_characteristicvaluex-char_value = abap_true.
    wa_characteristicvaluex-char_value_long = abap_true.
    "wa_characteristicvaluex-char_val_dependency_code = 'X'.
    CONDENSE: wa_characteristicvaluex-char_name.
    APPEND wa_characteristicvaluex TO lt_characteristicvaluex.

    PERFORM f_variantes TABLES lt_variantskeys lt_characteristicvalue lt_characteristicvaluex USING pv_matnr lv_value p_name.

  ENDLOOP.

  pt_caracteristic[]  = lt_characteristicvalue[].
  pt_caracteristicx[] = lt_characteristicvaluex[].
  pt_variants[] = lt_variantskeys[].

ENDFORM.

FORM f_variantes TABLES pt_variants pt_caracteristic pt_caracteristicx USING pv_matnr TYPE char18 p_value TYPE char40 p_name TYPE string.

  DATA: wa_characteristicvalue  TYPE bapie1ausprt,
        wa_characteristicvaluex TYPE bapie1ausprtx,
        wa_variantskeys         TYPE bapie1varkey.

  DATA: lt_characteristicvalue  TYPE TABLE OF bapie1ausprt,
        lt_characteristicvaluex TYPE TABLE OF bapie1ausprtx,
        lt_variantskeys         TYPE TABLE OF bapie1varkey.

  lt_characteristicvalue[]  = pt_caracteristic[].
  lt_characteristicvaluex[] = pt_caracteristicx[].
  lt_variantskeys[]         = pt_variants[].

  SELECT * FROM mara INTO TABLE @DATA(lt_mara) WHERE satnr EQ @pv_matnr.
  IF sy-subrc EQ 0.
    LOOP AT lt_mara INTO DATA(ls_mara).
      CLEAR wa_variantskeys.

*      wa_VARIANTSKEYS-function = '026'.
      wa_variantskeys-material = pv_matnr.
      wa_variantskeys-variant = ls_mara-matnr.

      READ TABLE lt_variantskeys TRANSPORTING NO FIELDS WITH KEY variant = ls_mara-matnr.
      IF sy-subrc NE 0.
        APPEND wa_variantskeys TO lt_variantskeys.
      ENDIF.

*      wa_CHARACTERISTICVALUE-function = '026'.
      wa_characteristicvalue-material = wa_variantskeys-variant.
      wa_characteristicvalue-char_name = p_name.
      wa_characteristicvalue-char_value = p_value.
      wa_characteristicvalue-char_value_long = p_value.
      "wa_characteristicvalue-char_val_dependency_code = 'X'.
      CONDENSE: wa_characteristicvalue-char_name, wa_characteristicvalue-char_value, wa_characteristicvalue-char_value_long.
      APPEND wa_characteristicvalue TO lt_characteristicvalue.

*      wa_CHARACTERISTICVALUEx-function = '026'.
      wa_characteristicvaluex-material = wa_variantskeys-variant.
      wa_characteristicvaluex-char_name = p_name.
      wa_characteristicvaluex-char_value = abap_true.
      wa_characteristicvaluex-char_value_long = abap_true.
      "wa_characteristicvaluex-char_val_dependency_code = 'X'.
      CONDENSE wa_characteristicvaluex-char_name.
      APPEND wa_characteristicvaluex TO lt_characteristicvaluex.

    ENDLOOP.
  ENDIF.

  pt_caracteristic[]  = lt_characteristicvalue[].
  pt_caracteristicx[] = lt_characteristicvaluex[].
  pt_variants[] = lt_variantskeys[].

ENDFORM.

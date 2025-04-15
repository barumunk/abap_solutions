*----------------------------------------------------------------------*
* CLASE
*----------------------------------------------------------------------*

CLASS zcl_reporte DEFINITION.

  PUBLIC SECTION.

    METHODS: obtener_datos,
      mostrar_alv,
      recibir_datos IMPORTING p_task TYPE clike,
      proceso_paralelo.

  PRIVATE SECTION.

    CONSTANTS: c_imp TYPE p DECIMALS 2 VALUE '1.16'.

    "Estructura del reporte
    TYPES:
      BEGIN OF ty_vbbe,
        matnr TYPE vbbe-matnr,
        werks TYPE vbbe-werks,
        lgort TYPE vbbe-lgort,
        vbtyp TYPE vbbe-vbtyp,
        sobkz TYPE vbbe-sobkz,
        omeng TYPE vbbe-omeng,
      END OF ty_vbbe,

      BEGIN OF ls_mardmsku,
        matnr TYPE mard-matnr,
        werks TYPE mard-werks,
        lgort TYPE mard-lgort,
        labst TYPE mard-labst,
        umlme TYPE mard-umlme,
        insme TYPE mard-insme,
        speme TYPE mard-speme,
        sobkz TYPE msku-sobkz,
      END OF ls_mardmsku,

      BEGIN OF ls_vbbecustom,
        omeng TYPE vbbe-omeng,
        matnr TYPE vbbe-matnr,
        werks TYPE vbbe-werks,
        lgort TYPE vbbe-lgort,
        vbtyp TYPE vbbe-vbtyp,
        sobkz TYPE vbbe-sobkz,
      END OF ls_vbbecustom,

      BEGIN OF ls_maraprecios,
        matnr TYPE mara-matnr,
        satnr TYPE mara-satnr,
        attyp TYPE mara-attyp,
      END OF ls_maraprecios.
******
    TYPES tt_vbbe TYPE ty_vbbe.

    "Para guardar nuestros datos
    DATA:
      wa_alv              TYPE zts_salida_reportedisp,
      wa_mard             TYPE zts_mard_rdisp,
      lt_msku             TYPE TABLE OF msku,
      wa_msku             LIKE LINE OF lt_msku,
      lt_zvmatnrcalss26   TYPE TABLE OF zvmatnrcalss26,
      wa_zvmatnrcalss26   LIKE LINE OF lt_zvmatnrcalss26,
      lt_wrf_brands_t     TYPE TABLE OF wrf_brands_t,
      wa_wrf_brands_t     LIKE LINE OF lt_wrf_brands_t,
      lt_wrf_charvalt     TYPE TABLE OF wrf_charvalt,
      wa_wrf_charvalt     LIKE LINE OF lt_wrf_charvalt,
      lt_ztmm_prodh       TYPE TABLE OF ztmm_prodh,
      wa_ztmm_prodh       LIKE LINE OF lt_ztmm_prodh,
      lt_wrf_textile_fibr TYPE TABLE OF wrf_textile_fibr,
      wa_wrf_textile_fibr LIKE LINE OF lt_wrf_textile_fibr,
      lt_wrf_fiber_codest TYPE TABLE OF wrf_fiber_codest,
      wa_wrf_fiber_codest LIKE LINE OF lt_wrf_fiber_codest,
      wa_fsh_sc_vconv     LIKE LINE OF lt_fsh_sc_vconv,
      lt_makt             TYPE TABLE OF makt,
      wa_makt             LIKE LINE OF lt_makt,
      wa_twewt            LIKE LINE OF lt_twewt,
      lt_maraprecios      TYPE TABLE OF ls_maraprecios,
      wa_maraprecios      LIKE LINE OF lt_maraprecios,
      lt_maravariantes    TYPE TABLE OF mara,
      wa_maravariantes    LIKE LINE OF lt_maravariantes,
      lt_fsh_seasons_mat  TYPE TABLE OF fsh_seasons_mat,
      wa_fsh_seasons_mat  LIKE LINE OF lt_fsh_seasons_mat,
      lt_alvsinesp        TYPE TABLE OF zts_salida_reportedisp,
      wa_alvsinesp        LIKE LINE OF lt_alvsinesp,
      lt_alvpuroesp       TYPE TABLE OF zts_salida_reportedisp,
      wa_alvpuroesp       LIKE LINE OF lt_alvpuroesp,
      lt_alv1             TYPE TABLE OF zts_salida_reportedisp,
      lt_alv2             TYPE TABLE OF zts_salida_reportedisp,
      wa_alv2             LIKE LINE OF lt_alv,
      wa_alvanterior      LIKE LINE OF lt_alv,
      wa_alvultimo        LIKE LINE OF lt_alv.

    "Para el alv
    DATA: gr_table     TYPE REF TO cl_salv_table,
          gr_settings  TYPE REF TO cl_salv_display_settings,
          gr_layout    TYPE REF TO cl_salv_layout,
          gr_events    TYPE REF TO cl_salv_events_table,
          gr_functions TYPE REF TO cl_salv_functions_list,
          gr_columns   TYPE REF TO cl_salv_columns_table,
          gr_column    TYPE REF TO cl_salv_column_table,
          wa_key       TYPE salv_s_layout_key.

ENDCLASS.

CLASS zcl_reporte IMPLEMENTATION.

  METHOD obtener_datos.

    REFRESH lt_alv.

    "Consulta del Color (WRF_CHARVALT-ATWRT)
    DATA: lv_atinn   TYPE atinn.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'COLOR'
      IMPORTING
        output = lv_atinn.

    SELECT a~matnr, a~matkl, a~normt, a~spart, a~ean11, a~prdha, a~extwg, a~satnr, a~attyp, a~mstae, a~mstav, a~size1_atinn, a~size1, a~brand_id,
           c~brand_descr, d~maktx, e~texto4,
           f~atinn, f~atwrt, g~atwtb,
           h~fiber_code, i~fiber_code_descr,
           MAX( j~fsh_season ) AS fsh_season
      FROM ( mara AS a
      LEFT OUTER JOIN fsh_seasons_mat AS j ON ( a~matnr = j~matnr OR a~satnr = j~matnr ) )
      INNER JOIN wrf_brands_t AS c ON a~brand_id = c~brand_id
      INNER JOIN makt AS d ON a~matnr = d~matnr
      INNER JOIN ztmm_prodh AS e ON e~prodh = a~prdha
      INNER JOIN zvmatnrcalss26 AS f ON a~matnr = f~matnr
      INNER JOIN wrf_charvalt AS g ON ( f~atinn = g~atinn AND f~atwrt = g~atwrt )
      INNER JOIN wrf_textile_fibr AS h ON a~matnr = h~matnr
      INNER JOIN wrf_fiber_codest AS i ON h~fiber_code = i~fiber_code
*      INTO TABLE @DATA(lt_marax)
      INTO TABLE @lt_marax
     WHERE ( a~matnr IN @s_matnr OR a~satnr IN @s_matnr )
       AND a~matkl IN @s_matkl
       AND a~spart IN @s_spart
       AND d~spras = 'S'
       AND g~spras = 'S'
       AND f~atinn = @lv_atinn
       AND h~textil_comp_pos = 1
       AND h~textil_fibr_pos = 1
       AND i~language = 'S'
       AND j~fsh_season IN @s_season
       AND e~texto4 <> @space
       GROUP BY a~matnr, a~matkl, a~normt, a~spart, a~ean11, a~prdha, a~extwg, a~satnr, a~attyp, a~mstae, a~mstav, a~size1_atinn, a~size1, a~brand_id,
           c~brand_descr, d~maktx, e~texto4,
           f~atinn, f~atwrt, g~atwtb,
           h~fiber_code, i~fiber_code_descr.

    IF sy-subrc = 0.

      SORT lt_marax BY matnr.

      REFRESH lt_mard.
      SELECT m~matnr, m~werks, m~lgort, m~labst, m~umlme, m~insme, m~speme, o~sobkz
      INTO TABLE @lt_mard
      FROM ( mard AS m LEFT OUTER JOIN vbbe AS o ON o~matnr = m~matnr AND o~werks = m~werks AND o~lgort = m~lgort )
      FOR ALL ENTRIES IN @lt_marax
      WHERE ( m~matnr = @lt_marax-matnr OR m~matnr = @lt_marax-satnr )
      AND m~werks IN @s_werks
      AND m~lgort IN @s_lgort.
      IF sy-subrc = 0.

        SORT lt_mard BY matnr werks lgort ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_mard COMPARING matnr werks lgort.

        REFRESH lt_kunnr.
        SELECT matnr, werks, kunnr
        FROM msku
        INTO TABLE @lt_kunnr
         FOR ALL ENTRIES IN @lt_mard
       WHERE matnr = @lt_mard-matnr
         AND werks = @lt_mard-werks.
        IF sy-subrc = 0.
          SORT lt_kunnr BY matnr werks kunnr.

          REFRESH lt_part.
          SELECT partner, name_org1, name_org2 FROM but000
            INTO TABLE @lt_part
             FOR ALL ENTRIES IN @lt_kunnr
           WHERE partner = @lt_kunnr-kunnr.
          IF sy-subrc = 0.
            SORT lt_part BY partner.
          ENDIF.

        ENDIF.
      ENDIF.

      "Consulta precio(konp-kbetr * 1.16)
      REFRESH lt_a700.
      SELECT a~matnr, a~knumh, p~kbetr
        INTO TABLE @lt_a700
        FROM a700 AS a
        INNER JOIN konp AS p ON a~knumh = p~knumh
        FOR ALL ENTRIES IN @lt_marax
        WHERE a~vkorg IN @s_vkorg
        AND a~kschl = 'ZPR0'
        AND a~vtweg = '10'
        AND a~pltyp = '00'
        AND ( a~matnr = @lt_marax-satnr OR a~matnr = @lt_marax-matnr )
        AND a~datbi >= @sy-datum
        AND a~datab <= @sy-datum
        AND p~loevm_ko <> 'X'.
      IF sy-subrc = 0.
        SORT lt_a700 BY matnr knumh kbetr.
      ENDIF.

      "Consulta de Talla MX (FSH_SC_VCONV-FSH_SC_ENTX8)
      REFRESH lt_fsh_sc_vconv.
      SELECT fsh_sc_entx8, fsh_sc_bezg, fsh_sc_stxt
        INTO CORRESPONDING FIELDS OF TABLE @lt_fsh_sc_vconv
        FROM fsh_sc_vconv
        WHERE fsh_sc_vctyp = 'MX'.
      IF sy-subrc = 0.
        SORT lt_fsh_sc_vconv BY fsh_sc_stxt fsh_sc_bezg.
      ENDIF.

      "Consulta de datos texto de extwg
      REFRESH lt_twewt.
      SELECT extwg, ewbez
        INTO CORRESPONDING FIELDS OF TABLE @lt_twewt
        FROM twewt
        FOR ALL ENTRIES IN @lt_marax
        WHERE extwg = @lt_marax-extwg
        AND spras = 'S'.
      IF sy-subrc = 0.
        SORT lt_twewt BY extwg.
      ENDIF.

      IF p_stesp <> ''.

        SELECT DISTINCT matnr, werks
          INTO TABLE @DATA(lt_msku)
          FROM msku
           FOR ALL ENTRIES IN @lt_marax
          WHERE matnr = @lt_marax-matnr
            AND werks IN @s_werks.
        IF sy-subrc = 0.
          SORT lt_msku BY matnr werks.
        ENDIF.

        LOOP AT lt_msku INTO DATA(wa_msku).

          READ TABLE lt_mard WITH KEY matnr = wa_msku-matnr
                                      werks = wa_msku-werks TRANSPORTING NO FIELDS BINARY SEARCH.
          IF sy-subrc = 0.
            "si el material ya esta en la tabla pues ya no se pasa.
          ELSE.

            lt_mard = VALUE #( BASE lt_mard ( matnr = wa_msku-matnr
                                              werks = wa_msku-werks
                                              lgort = space
                                              sobkz = 'W') ).

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    "Llamar a procesamiento paralelo
    me->proceso_paralelo( ).

    "Borrar registros que están en ceros
    DELETE lt_alv WHERE labst = 0 AND ordcompra = 0 AND umlme = 0 AND insme = 0 AND speme = 0 AND pedido = 0 AND entrega = 0.

  ENDMETHOD.

  METHOD mostrar_alv.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = gr_table
          CHANGING
            t_table      = lt_alv.
      CATCH cx_salv_msg.
    ENDTRY.

    "Obtenemos la instancia de la clase CL_SALV_COLUMNS_TABLE
    CALL METHOD gr_table->get_columns
      RECEIVING
        value = gr_columns.

    "Cambio de nombres a columnas
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'STOCKESP' ).
        gr_column->set_short_text( 'StockEsp' ).
        gr_column->set_medium_text( 'Stock especial' ).
        gr_column->set_long_text( 'Stock especial' ).
        gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'SATNR' ).
        gr_column->set_short_text( 'MatGen' ).
        gr_column->set_medium_text( 'Material Generico' ).
        gr_column->set_long_text( 'Material Generico' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "brand_descr
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'BRAND_DESCR' ).
        gr_column->set_short_text( 'MarcaTxt' ).
        gr_column->set_medium_text( 'Marca Texto' ).
        gr_column->set_long_text( 'Marca Texto' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "maktx
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'MAKTX' ).
        gr_column->set_short_text( 'Texto' ).
        gr_column->set_medium_text( 'Texto' ).
        gr_column->set_long_text( 'Texto' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "texto4
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'TEXTO4' ).
        gr_column->set_short_text( 'Sil. Gen.' ).
        gr_column->set_medium_text( 'Silueta General' ).
        gr_column->set_long_text( 'Silueta General' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "atwtb
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'ATWTB' ).
        gr_column->set_short_text( 'Color' ).
        gr_column->set_medium_text( 'Color' ).
        gr_column->set_long_text( 'Color' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "fiber_code_descr
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'FIBER_CODE_DESCR' ).
        gr_column->set_short_text( 'Txt. Corte' ).
        gr_column->set_medium_text( 'Texto Corte' ).
        gr_column->set_long_text( 'Texto Corte' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "size1
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'SIZE1' ).
        gr_column->set_short_text( 'Talla Base' ).
        gr_column->set_medium_text( 'Talla Base' ).
        gr_column->set_long_text( 'Talla Base' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "fsh_sc_entx8
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'FSH_SC_ENTX8' ).
        gr_column->set_short_text( 'Talla MX' ).
        gr_column->set_medium_text( 'Talla MX' ).
        gr_column->set_long_text( 'Talla MX' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "kbetr
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'KBETR' ).
        gr_column->set_short_text( 'Precio' ).
        gr_column->set_medium_text( 'Precio' ).
        gr_column->set_long_text( 'Precio' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "labst
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'LABST' ).
        gr_column->set_short_text( 'Existencia' ).
        gr_column->set_medium_text( 'Existencia' ).
        gr_column->set_long_text( 'Existencia' ).
        gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        gr_column->set_edit_mask( '==DEC0' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "ordcompra
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'ORDCOMPRA' ).
        gr_column->set_short_text( 'Ord. Comp.' ).
        gr_column->set_medium_text( 'Orden de compra' ).
        gr_column->set_long_text( 'Orden de compra' ).
        gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        gr_column->set_edit_mask( '==DEC0' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "Cliente
    TRY.
        IF p_stesp EQ abap_true.
          gr_column ?= gr_columns->get_column( columnname = 'KUNNR' ).
          gr_column->set_short_text( 'Cliente' ).
          gr_column->set_medium_text( 'Cliente' ).
          gr_column->set_long_text( 'Cliente' ).
          gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        ENDIF.
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        IF p_stesp EQ abap_true.
          gr_column ?= gr_columns->get_column( columnname = 'PARTNER' ).
          gr_column->set_medium_text( 'Descripcion' ).
          gr_column->set_long_text( 'Descripcion' ).
          gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        ENDIF.
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'UMLME' ).
        gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        gr_column->set_edit_mask( '==DEC0' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'INSME' ).
        gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        gr_column->set_edit_mask( '==DEC0' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'SPEME' ).
        gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        gr_column->set_edit_mask( '==DEC0' ).
      CATCH cx_salv_not_found.
    ENDTRY.


    "ewbez
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'EWBEZ' ).
        gr_column->set_short_text( 'Familia' ).
        gr_column->set_medium_text( 'Familia' ).
        gr_column->set_long_text( 'Familia' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "MSTAE
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'MSTAE' ).
        gr_column->set_short_text( 'Est. Mat.' ).
        gr_column->set_medium_text( 'Estatus Material' ).
        gr_column->set_long_text( 'Estatus Material' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "MSTDV
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'MSTAV' ).
        gr_column->set_short_text( 'Est. Esp.' ).
        gr_column->set_medium_text( 'Estatus Especial' ).
        gr_column->set_long_text( 'Estatus Especial' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "disponible
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'DISPONIBLE' ).
        gr_column->set_short_text( 'Disponible' ).
        gr_column->set_medium_text( 'Disponible' ).
        gr_column->set_long_text( 'Disponible' ).
        gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        gr_column->set_edit_mask( '==DEC0' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "porcubrir
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'PORCUBRIR' ).
        gr_column->set_short_text( 'Por cubrir' ).
        gr_column->set_medium_text( 'Por cubrir' ).
        gr_column->set_long_text( 'Por cubrir' ).
        gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        gr_column->set_edit_mask( '==DEC0' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "pedido
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'PEDIDO' ).
        gr_column->set_short_text( 'Pedido' ).
        gr_column->set_medium_text( 'Pedido' ).
        gr_column->set_long_text( 'Pedido' ).
        gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        gr_column->set_edit_mask( '==DEC0' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "entrega
    TRY.
        gr_column ?= gr_columns->get_column( columnname = 'ENTREGA' ).
        gr_column->set_short_text( 'Entrega' ).
        gr_column->set_medium_text( 'Entrega' ).
        gr_column->set_long_text( 'Entrega' ).
        gr_column->set_optimized( value = if_salv_c_bool_sap=>true ).
        gr_column->set_edit_mask( '==DEC0' ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "Optimiza Columnas
    CALL METHOD gr_columns->set_optimize
      EXPORTING
        value = 'X'.

    "Ocultar columna
    IF p_stesp = ''.
      "indicador de stock especial
      TRY.
          gr_column ?= gr_columns->get_column( columnname = 'STOCKESP' ).
          gr_column->set_visible( value  = if_salv_c_bool_sap=>false ).
        CATCH cx_salv_not_found.
      ENDTRY.
    ENDIF.

    "Activar barra de herramientas.
    TRY.
        gr_table->get_functions( )->set_all( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.

    "Activar el layout para las variantes
    TRY.
        gr_table->get_layout( )->set_key( VALUE #( report = sy-repid ) ).
        gr_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
      CATCH cx_salv_not_found.
    ENDTRY.

    CALL METHOD gr_table->display.

  ENDMETHOD.

  METHOD proceso_paralelo.

    DATA: lv_free_wp  TYPE i,
          lv_rfctask  TYPE char15,
          lt_datos    TYPE STANDARD TABLE OF zts_mard_rdisp,
          cont        TYPE i,
          r           TYPE i,
          t           TYPE i,
          max_pbt_wps TYPE i.

    gv_tareas_paralelas = 1.
    cont = 1.
    t = 1.
    DESCRIBE TABLE lt_mard LINES DATA(lv_total).

    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name                     = gv_nombre
      IMPORTING
        free_pbt_wps                   = lv_free_wp
      EXCEPTIONS
        invalid_group_name             = 1
        internal_error                 = 2
        pbt_env_already_initialized    = 3
        currently_no_resources_avail   = 4
        no_pbt_resources_found         = 5
        cant_init_different_pbt_groups = 6
        OTHERS                         = 7.

    DO.

      CALL FUNCTION 'SPBT_GET_CURR_RESOURCE_INFO'
        IMPORTING
          max_pbt_wps     = max_pbt_wps
          free_pbt_wps    = lv_free_wp
        EXCEPTIONS
          no_free_session = 1
          OTHERS          = 2.

      IF lv_free_wp > 1 AND sy-subrc = 0.

        r = 0.
        REFRESH lt_datos.

        WHILE cont <= lv_total AND r <= 1000.

          READ TABLE lt_mard INTO DATA(wa_datos) INDEX cont.
          APPEND wa_datos TO lt_datos.

          cont +=  1.
          r += 1.
        ENDWHILE.

        lv_rfctask = |TASK{ t }|.

        CALL FUNCTION 'ZMF_RDISPONIBILIDAD_CPAR'
          STARTING NEW TASK lv_rfctask
          DESTINATION IN GROUP gv_nombre
          CALLING recibir_datos ON END OF TASK
          EXPORTING
            p_stesp        = p_stesp
          TABLES
            i_salida       = lt_alv
            i_mard         = lt_datos
            i_marax        = lt_marax
            i_fsh_sc_vconv = lt_fsh_sc_vconv
            i_twewt        = lt_twewt
            i_a700         = lt_a700
            i_kunnr        = lt_kunnr
            i_part         = lt_part.
        IF sy-subrc = 0.

          gv_tareas_paralelas += 1.

          t += 1.

          WAIT UP TO 1 SECONDS.

        ENDIF.

      ELSE.
        WAIT UP TO 5 SECONDS.
      ENDIF.

      IF cont >  lv_total.
        EXIT.
      ENDIF.


    ENDDO.

    WAIT UNTIL gv_tareas_paralelas = 0.

  ENDMETHOD.

  METHOD recibir_datos.

    DATA lt_salida TYPE STANDARD TABLE OF zts_salida_reportedisp.
    FIELD-SYMBOLS <lf_datos> TYPE table.

    RECEIVE RESULTS FROM FUNCTION 'ZMF_RDISPONIBILIDAD_CPAR'
    TABLES
      t_salida = lt_salida.
    IF sy-subrc = 0.

      gv_tareas_paralelas -= 1.

      LOOP AT lt_salida INTO DATA(ls_salidax).

        MOVE-CORRESPONDING ls_salidax TO wa_alv.
        APPEND wa_alv TO lt_alv.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

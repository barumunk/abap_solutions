FUNCTION zmf_rdisponibilidad_cpar.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(P_STESP) TYPE  CHECKBOX
*"  TABLES
*"      T_SALIDA TYPE  ZTT_SALIDA_REPORTEDISP OPTIONAL
*"      I_MARD TYPE  ZTT_MARD_RDISP
*"      I_MARAX TYPE  ZTT_MARAX_RDIPS
*"      I_FSH_SC_VCONV STRUCTURE  FSH_SC_VCONV
*"      I_TWEWT STRUCTURE  TWEWT
*"      I_A700 TYPE  ZTT_A700_RDISP
*"      I_KUNNR TYPE  ZTT_KUNNR_RDISP
*"      I_PART TYPE  ZTT_PART_RDISP
*"----------------------------------------------------------------------

  CONSTANTS: c_imp TYPE p DECIMALS 2 VALUE '1.16'.

  DATA:        wa_alv TYPE zts_salida_reportedisp.

  LOOP AT i_mard INTO DATA(ls_mard).

    READ TABLE i_marax INTO DATA(ls_marax)
    WITH KEY matnr = ls_mard-matnr BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ELSE.
      DATA(lv_flag) = abap_true.

      "Actualizamos el material
      IF ls_marax-attyp <> '02'.
        ls_marax-satnr  = ls_marax-matnr.
      ENDIF.

      wa_alv-spart = ls_marax-spart.
      wa_alv-satnr = ls_marax-satnr.

      "traduccion de brand_id a brand_descr
      wa_alv-brand_descr = ls_marax-brand_descr.

      wa_alv-matkl = ls_marax-matkl.
      wa_alv-normt = ls_marax-normt.

      "ZTMM_PRODH-Texto4
      wa_alv-texto4 = ls_marax-texto4.

      wa_alv-size1 = ls_marax-size1.

      "Talla MX (FSH_SC_VCONV-FSH_SC_ENTX8)
      READ TABLE i_fsh_sc_vconv WITH KEY fsh_sc_stxt = ls_marax-size1
                                          fsh_sc_bezg = ls_marax-size1_atinn INTO DATA(wa_fsh_sc_vconv)
                                          BINARY SEARCH.
      IF sy-subrc = 0.
        wa_alv-fsh_sc_entx8 = wa_fsh_sc_vconv-fsh_sc_entx8.
      ENDIF.
      CLEAR wa_fsh_sc_vconv.

      "Texto de extwg (twewt-ewbez)
      READ TABLE i_twewt WITH KEY extwg = ls_marax-extwg INTO DATA(wa_twewt)
      BINARY SEARCH.
      IF sy-subrc = 0.
        wa_alv-ewbez = wa_twewt-ewbez.
      ENDIF.
      CLEAR wa_twewt.

      wa_alv-ean11 = ls_marax-ean11.

      wa_alv-fsh_season = ls_marax-fsh_season.

      wa_alv-mstav = ls_marax-mstav.
      wa_alv-mstae = ls_marax-mstae.

      "sacando el precio del material generico.
      READ TABLE i_a700 INTO DATA(wa_a700)
      WITH  KEY matnr = ls_marax-satnr
      BINARY SEARCH.

      IF sy-subrc = 0.
        CALL FUNCTION 'ROUND'
          EXPORTING
            decimals = 2
            input    = wa_a700-kbetr * c_imp
          IMPORTING
            output   = wa_alv-kbetr.
      ENDIF.

      wa_alv-maktx = ls_marax-maktx.

      "sacando el color.
      wa_alv-atwtb = ls_marax-atwtb.

      "sacando el material composicion
      wa_alv-fiber_code_descr = ls_marax-fiber_code_descr.

      IF ls_mard-sobkz <> 'W'.

        ls_mard-labst = ls_mard-labst + ls_mard-insme.

        wa_alv-matnr = ls_mard-matnr.
        wa_alv-werks = ls_mard-werks.
        wa_alv-lgort = ls_mard-lgort.
        wa_alv-labst = ls_mard-labst.
        wa_alv-insme = ls_mard-insme.
        wa_alv-speme = ls_mard-speme.

        DATA: c_transito   TYPE quan1_12.

        PERFORM calcular_disponible USING  ls_mard-matnr
                                           ls_mard-werks
                                           ls_mard-lgort
                                           ls_mard-labst
                                 CHANGING  wa_alv-ordcompra
                                           wa_alv-pedido
                                           wa_alv-entrega
                                           wa_alv-disponible
                                           wa_alv-porcubrir
                                           c_transito.

        wa_alv-umlme = ls_mard-umlme + c_transito.



        APPEND wa_alv TO t_salida.

        CLEAR wa_alv.

      ELSE.

        wa_alv-matnr = ls_mard-matnr.
        wa_alv-werks = ls_mard-werks.
        wa_alv-lgort = ls_mard-lgort.
        wa_alv-stockesp = 'W'.
        wa_alv-speme =  0.
        wa_alv-ordcompra =  0.

        PERFORM calcular_stock_esp USING ls_mard-matnr
                                         ls_mard-werks
                                         wa_alv-kunnr
                                CHANGING wa_alv-labst
                                         wa_alv-umlme
                                         wa_alv-insme
                                         wa_alv-pedido
                                         wa_alv-entrega
                                         wa_alv-disponible.

        LOOP AT i_kunnr INTO DATA(lv_kunnr)
        WHERE matnr = wa_alv-matnr
          AND werks = wa_alv-werks.
          wa_alv-kunnr = lv_kunnr-kunnr.
          READ TABLE i_part INTO DATA(ls_part)
          WITH KEY partner = wa_alv-kunnr BINARY SEARCH.
          IF sy-subrc EQ 0.
            CONCATENATE ls_part-name_org1 ls_part-name_org2 INTO wa_alv-partner SEPARATED BY space.
            CONDENSE wa_alv-partner.
          ENDIF.

          PERFORM calcular_stock_esp USING ls_mard-matnr
                                 ls_mard-werks
                                 wa_alv-kunnr
                        CHANGING wa_alv-labst
                                 wa_alv-umlme
                                 wa_alv-insme
                                 wa_alv-pedido
                                 wa_alv-entrega
                                 wa_alv-disponible.
          APPEND wa_alv TO t_salida.
          CLEAR: ls_part, lv_kunnr.
        ENDLOOP.
        IF sy-subrc <> 0.
          APPEND wa_alv TO t_salida.
        ENDIF.

        CLEAR wa_alv.

      ENDIF.

    ENDIF.

    CLEAR:
     wa_a700, wa_twewt, wa_fsh_sc_vconv, ls_marax, ls_mard, lv_flag.

  ENDLOOP.

  "Se ordena para la lógica de stock especial
  SORT t_salida BY matnr werks lgort ASCENDING.

  "Lógica stock especial
  IF p_stesp <> ''.

    "se mueven los que no tienen w a otra tabla y se trabaja sobre esa para sacarles su stock especial
    "Se borran los duplicados por material y centro
    DATA(lt_sinstockesp) = t_salida[].
    DELETE lt_sinstockesp WHERE stockesp = 'W'.
    SORT lt_sinstockesp BY matnr werks.
    DELETE ADJACENT DUPLICATES FROM lt_sinstockesp COMPARING matnr werks.

    SELECT matnr, werks, kunnr
      FROM msku
      INTO TABLE @DATA(lt_kunnr_ant)
       FOR ALL ENTRIES IN @lt_sinstockesp
     WHERE matnr = @lt_sinstockesp-matnr
       AND werks = @lt_sinstockesp-werks.
    IF sy-subrc = 0.
      SORT lt_kunnr_ant BY matnr werks kunnr.

      SELECT partner, name_org1, name_org2 FROM but000
        INTO TABLE @DATA(lt_partner)
         FOR ALL ENTRIES IN @lt_kunnr_ant
       WHERE partner = @lt_kunnr_ant-kunnr.
      IF sy-subrc = 0.
        SORT lt_partner BY partner.
      ENDIF.

    ENDIF.

    "Cada vez que cambia de combinación de material y centro en el loop se hace la consulta con ese material
    " y se realiza el procedimiento del calculo de stock especial.
    "con esta nueva información se añade a la tabla original.
    LOOP AT lt_sinstockesp INTO DATA(ls_sinstock).

      MOVE-CORRESPONDING ls_sinstock TO wa_alv.

      wa_alv-lgort = ''.
      wa_alv-stockesp = 'W'.
      wa_alv-speme =  0.
      wa_alv-ordcompra =  0.
      lv_matnr = wa_alv-matnr.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = lv_matnr
        IMPORTING
          output = lv_matnr.

      PERFORM calcular_stock_esp USING wa_alv-matnr
                                       wa_alv-werks
                                       wa_alv-kunnr
                              CHANGING wa_alv-labst
                                       wa_alv-umlme
                                       wa_alv-insme
                                       wa_alv-pedido
                                       wa_alv-entrega
                                       wa_alv-disponible.

      LOOP AT lt_kunnr_ant INTO DATA(lv_kunnr_ant)
      WHERE matnr = wa_alv-matnr
        AND werks = wa_alv-werks.
        wa_alv-kunnr = lv_kunnr_ant-kunnr.
        READ TABLE lt_partner INTO DATA(ls_partner)
        WITH KEY partner = wa_alv-kunnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          CONCATENATE ls_partner-name_org1 ls_partner-name_org2 INTO wa_alv-partner SEPARATED BY space.
          CONDENSE wa_alv-partner.
        ENDIF.

        PERFORM calcular_stock_esp USING wa_alv-matnr
                                         wa_alv-werks
                                         wa_alv-kunnr
                                CHANGING wa_alv-labst
                                         wa_alv-umlme
                                         wa_alv-insme
                                         wa_alv-pedido
                                         wa_alv-entrega
                                         wa_alv-disponible.

        APPEND wa_alv TO t_salida.
        CLEAR: ls_partner, lv_kunnr_ant.
      ENDLOOP.
      IF sy-subrc <> 0.
        APPEND wa_alv TO t_salida.
      ENDIF.

      CLEAR: ls_sinstock, wa_alv.

    ENDLOOP.

    SORT t_salida BY matnr werks lgort ASCENDING.

  ENDIF.



ENDFUNCTION.

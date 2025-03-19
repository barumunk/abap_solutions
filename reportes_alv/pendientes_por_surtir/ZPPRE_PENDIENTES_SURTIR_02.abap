*------------------------------------------------------------------------*
* Programa   : zppre_pendientes_surtir                                   *
* Descripción: Reporte para consultar la información detallada           *
* de los Pendientes Restantes por surtir y sus materiales.               *
* Programador: David Navoa Acevedo                                       *
* Fecha      : 21.10.2024                                                *
*------------------------------------------------------------------------*

CLASS lcl_catalogo IMPLEMENTATION.

  METHOD get_data.

    DATA: lt_rango_entrega TYPE TABLE OF ty_entrega,
          ls_rango_entrega TYPE ty_entrega,
          ls_catalogo      TYPE ty_catalogo,
          lv_fecha         TYPE string.

    DATA:total      TYPE i,
         msg(200),
         stabix(10),
         stotal(10).

    CLEAR: lt_catalogo[], lt_rango_entrega[], ls_rango_entrega.

    SELECT b~vbeln, a~posnr, a~lgort, a~fsh_season, a~matkl, a~werks, a~pmatn, a~arktx, a~wrf_charstc2, a~cmtd_deliv_date AS vdatu_ana, a~matnr, a~vkorg_ana, a~vgbel, a~vgpos,
           a~ean11, b~auart, b~kunnr AS kunnr_s, b~vkorg, b~vtweg, b~spart, b~erdat, b~bstnk, c~kunnr AS kunnr_d, c~parvw, m~brand_id, m~attyp, m~satnr, f~labst,
           '0000000.000' AS c_asignada,
            j~bstkd, k~kbetr AS p_wholesale,
            '0000000.000' AS c_pedido, '0000000.000' AS c_confirmada, '0000000.000' AS c_pendiente
      FROM vbap AS a
      LEFT JOIN vbak AS b ON a~vbeln = b~vbeln
      LEFT JOIN prcd_elements AS k ON ( b~knumv = k~knumv AND a~posnr = k~kposn AND k~kschl EQ 'ZPR0' )
      LEFT JOIN mara AS m ON ( a~matnr = m~matnr )
      LEFT JOIN vbpa AS c ON ( a~vbeln = c~vbeln AND c~parvw EQ 'WE' )
      LEFT OUTER JOIN vbkd AS j ON ( a~vbeln = j~vbeln AND a~posnr = j~posnr )
      LEFT OUTER JOIN mard AS f ON ( a~matnr = f~matnr AND a~werks = f~werks AND f~labst NE 0 )
      INTO TABLE @DATA(lt_vbapx)
      WHERE b~vbeln      IN @s_vbeln
      AND   b~auart      IN @s_auart
      AND   b~fsh_kvgr6  IN @s_fsh_6
      AND   a~matnr      IN @s_matnr
      AND   a~werks      IN @s_werks
      AND   a~matkl      IN @s_matkl
      AND   a~cmtd_deliv_date  IN @s_vdatu
      AND   a~fsh_season IN @s_season
      AND   b~vkorg      IN @s_vkorg
      AND   b~vtweg      IN @s_vtweg
      AND   b~spart      IN @s_spart
      AND   b~kunnr      IN @s_kunnr
      AND   a~gbsta      NE 'C'
      AND   m~brand_id   IN @s_brand
      GROUP BY a~vbeln, a~posnr, a~matnr, a~werks, a~lgort, a~fsh_season, a~matkl, a~pmatn, a~arktx, a~wrf_charstc2, a~cmtd_deliv_date, a~vkorg_ana,
       b~vbeln, b~kunnr, b~vkorg, b~vtweg, b~spart, c~kunnr, c~parvw, f~labst, b~auart, a~ean11,
      b~erdat, j~bstkd, k~kbetr, m~brand_id, b~bstnk, a~vgbel, a~vgpos, m~attyp, m~satnr.", g~alloc_qty.
    IF sy-subrc EQ 0.
      SORT lt_vbapx BY vbeln posnr.

      "Validacion de Pedido abierto y separacion de registros
      SELECT a~vbelv AS vbeln, a~vbeln AS entrega, b~wbstk, a~posnv AS posnr, a~posnn AS entrega_pos  FROM vbfa AS a
        JOIN likp AS b ON a~vbeln = b~vbeln
        FOR ALL ENTRIES IN @lt_vbapx
        WHERE a~vbtyp_n = 'J'
        AND a~vbelv = @lt_vbapx-vbeln
        AND a~posnv = @lt_vbapx-posnr
        INTO TABLE @DATA(lt_val_lips).
      IF sy-subrc NE 0.
      ENDIF.

      SORT lt_val_lips BY vbeln entrega posnr.

      DATA(lt_vbap_aux1) = lt_vbapx[].
      REFRESH lt_vbap_aux1.

      "Ciclo para comparar la cantidad existente y asignada (Solo se procesan los que coinciden)
      LOOP AT lt_vbapx INTO DATA(ls_vbapx).
        IF ls_vbapx-labst GE ls_vbapx-c_asignada.
          APPEND ls_vbapx TO lt_vbap_aux1.
        ENDIF.
      ENDLOOP.

      DATA(lt_val_lips_aux1) = lt_val_lips[].
      REFRESH lt_val_lips_aux1.

      "INI ajuste DNAVOA 18.12.2024 pedido - vbbe
      "Se obtiene la cantidad de pedido (entra con Pedido)
      LOOP AT lt_vbap_aux1 ASSIGNING FIELD-SYMBOL(<fs_vbap_aux2>).
        SELECT SINGLE SUM( omeng ) FROM vbbe INTO @DATA(lv_omeng)
          WHERE vbeln EQ @<fs_vbap_aux2>-vbeln
          AND posnr EQ @<fs_vbap_aux2>-posnr.
        IF sy-subrc EQ 0 AND lv_omeng IS NOT INITIAL.
          <fs_vbap_aux2>-c_pedido += lv_omeng.
        ENDIF.
      ENDLOOP.
      "FIN ajuste DNAVOA 18.12.2024 pedido - vbbe

      "Se dejan unicamente las entregas diferentes de C
      LOOP AT lt_val_lips INTO DATA(ls_val_lips_aux).
        READ TABLE lt_val_lips_aux1 ASSIGNING FIELD-SYMBOL(<fs_val_lips_aux2>) WITH KEY vbeln = ls_val_lips_aux-vbeln.
        IF sy-subrc NE 0.
          APPEND ls_val_lips_aux TO lt_val_lips_aux1.
        ELSEIF sy-subrc EQ 0 AND ls_val_lips_aux-wbstk NE 'C'.
          <fs_val_lips_aux2>-wbstk = ls_val_lips_aux-wbstk.
          <fs_val_lips_aux2>-entrega = ls_val_lips_aux-entrega.
        ENDIF.

        "INI ajuste DNAVOA 18.12.2024 pedido - vbbe
        "Se lee la tabla principal para obtener la cantidad de pedido
        "(entra con entrega)
        READ TABLE lt_vbap_aux1 ASSIGNING <fs_vbap_aux2>
          WITH KEY vbeln = ls_val_lips_aux-vbeln
                   posnr = ls_val_lips_aux-posnr.
        IF sy-subrc EQ 0 AND <fs_vbap_aux2>-auart NE 'ZOR1'.
          SELECT SINGLE SUM( omeng ) FROM vbbe INTO lv_omeng
            WHERE vbeln EQ ls_val_lips_aux-entrega
            AND   posnr EQ ls_val_lips_aux-entrega_pos.
          IF sy-subrc EQ 0.
            <fs_vbap_aux2>-c_pedido += lv_omeng.
          ENDIF.
        ENDIF.
        "FIN ajuste DNAVOA 18.12.2024 pedido - vbbe

      ENDLOOP.

      "Se eliminan todos los pedidos que tengan entrega tipo C
      LOOP AT lt_val_lips_aux1 INTO ls_val_lips_aux WHERE wbstk EQ 'C'.
        DELETE lt_vbap_aux1 WHERE vbeln = ls_val_lips_aux-vbeln.
      ENDLOOP.

      "Error por existencia
      IF lt_vbap_aux1 IS INITIAL.
        MESSAGE TEXT-e02 TYPE 'I' DISPLAY LIKE 'E'.
      ENDIF.

      "Cantidad asignada
      "Se suman todas las cantidades asignadas que se encuentren y que coincidan
      SELECT * FROM arun_bdbs INTO TABLE @DATA(lt_arun_bds)
          FOR ALL ENTRIES IN @lt_vbap_aux1
          WHERE salesdoc_num EQ @lt_vbap_aux1-vbeln
          AND   salesdoc_item EQ @lt_vbap_aux1-posnr
          AND   plant            EQ @lt_vbap_aux1-werks
          AND   material         EQ @lt_vbap_aux1-matnr.
      IF sy-subrc EQ 0.
        LOOP AT lt_arun_bds INTO DATA(ls_arun_bds).
          READ TABLE lt_vbap_aux1 ASSIGNING FIELD-SYMBOL(<fs_vbapx>)
          WITH KEY
          vbeln = ls_arun_bds-salesdoc_num
          matnr = ls_arun_bds-material
          werks = ls_arun_bds-plant
          posnr = ls_arun_bds-salesdoc_item.
          IF sy-subrc EQ 0.
            <fs_vbapx>-c_asignada += ls_arun_bds-alloc_qty.
            CONDENSE <fs_vbapx>-c_asignada NO-GAPS.
          ENDIF.
        ENDLOOP.
      ENDIF.

      "Cantidad Confirmada
      "Obtencion de cantidad confirmada, se suma igual por reparto
      SELECT vbeln, posnr, bmeng, wmeng, ordqty_bu, ordqty_su FROM vbep
        INTO TABLE @DATA(lt_vbep)
        FOR ALL ENTRIES IN @lt_vbap_aux1
        WHERE vbeln = @lt_vbap_aux1-vbeln
        AND posnr = @lt_vbap_aux1-posnr.
      IF sy-subrc EQ 0.
        LOOP AT lt_vbep INTO DATA(ls_vbep).
          READ TABLE lt_vbap_aux1 ASSIGNING <fs_vbapx>
          WITH KEY vbeln = ls_vbep-vbeln
                   posnr = ls_vbep-posnr.
          IF sy-subrc EQ 0.
            <fs_vbapx>-c_confirmada += ls_vbep-bmeng.
            CONDENSE <fs_vbapx>-c_confirmada NO-GAPS.
* INI            "Ajuste por cantidad pendiente
            IF ls_vbep-ordqty_bu IS NOT INITIAL.
              <fs_vbapx>-c_pendiente += ls_vbep-ordqty_bu.
            ELSE.
              <fs_vbapx>-c_pendiente += ls_vbep-ordqty_su.
            ENDIF.
            CONDENSE <fs_vbapx>-c_pendiente NO-GAPS.
          ENDIF.
        ENDLOOP.
      ENDIF.

      "Ciclo para los pedidos abiertos
      "Obtencion de la informacion restante
      DESCRIBE TABLE lt_vbap_aux1 LINES total.
      LOOP AT lt_vbap_aux1 INTO DATA(ls_vbap_aux).
        CLEAR: ls_catalogo.

        "Mensage de procesamiento en tiempo de carga
        WRITE: sy-tabix TO stabix, total TO stotal.
        CONDENSE: stabix, stotal.
        CONCATENATE 'Procesando:'  ls_vbap_aux-vbeln '[' stabix ' de ' stotal ']'  INTO msg SEPARATED BY space.
        cl_progress_indicator=>progress_indicate( EXPORTING
                                   i_text               = msg
                                   i_processed          = sy-tabix
                                   i_total              = total
                                   i_output_immediately =  'X' ).

        MOVE:
          ls_vbap_aux-werks TO ls_catalogo-centro,
          ls_vbap_aux-lgort TO ls_catalogo-almacen,
          ls_vbap_aux-fsh_season TO ls_catalogo-temporada,
          ls_vbap_aux-matkl TO ls_catalogo-gpo_articulos,
          ls_vbap_aux-vbeln TO ls_catalogo-doc_ventas,
          ls_vbap_aux-bstnk TO ls_catalogo-ref_cli,
          ls_vbap_aux-posnr TO ls_catalogo-num_pos_p,
          ls_vbap_aux-matnr TO ls_catalogo-material,
          ls_vbap_aux-arktx TO ls_catalogo-descripcion,
          ls_vbap_aux-wrf_charstc2 TO ls_catalogo-talla,
          ls_vbap_aux-vdatu_ana TO ls_catalogo-f_entrega,
          ls_vbap_aux-kunnr_s TO ls_catalogo-solicitante,
          ls_vbap_aux-kunnr_d TO ls_catalogo-destinatario,
          ls_vbap_aux-c_pedido TO ls_catalogo-c_pedido,
          ls_vbap_aux-c_confirmada TO ls_catalogo-c_confirmada,
          ls_vbap_aux-c_pendiente TO ls_catalogo-c_pendiente,
          ls_vbap_aux-c_asignada TO ls_catalogo-c_asignada,
          ls_vbap_aux-erdat TO ls_catalogo-f_creacion_ped,
          ls_vbap_aux-p_wholesale TO ls_catalogo-p_wholesome,
          ls_vbap_aux-auart     TO ls_catalogo-auart,
          ls_vbap_aux-ean11     TO ls_catalogo-ean.

        SELECT SINGLE * FROM mara INTO @DATA(ls_mara) WHERE matnr EQ @ls_catalogo-material.
        IF sy-subrc EQ 0.

          MOVE: ls_mara-normt TO ls_catalogo-modelo.

          "Metodo para la obtencion de color en clase 026.
          me->get_color(
            EXPORTING ps_mara = ls_mara
            CHANGING ps_catalogo = ls_catalogo ).

          "Metodo para la obtencion de la silueta generica.
          me->get_silueta(
            EXPORTING ps_mara = ls_mara
            CHANGING ps_catalogo = ls_catalogo ).

          "Familia, estatus material
          MOVE: ls_mara-mstae TO ls_catalogo-s_especial_mat.
          SELECT SINGLE ewbez FROM twewt INTO ls_catalogo-familia
            WHERE spras EQ sy-langu
            AND   extwg EQ ls_mara-extwg.

          "Talla MX
          SELECT SINGLE fsh_sc_entx8 FROM fsh_sc_vconv INTO ls_catalogo-talla_mx
            WHERE fsh_sc_vctyp EQ 'MX'
            AND   fsh_sc_bezg EQ ls_mara-size1_atinn
            AND   fsh_sc_stxt EQ ls_mara-size1.

        ENDIF.

        "Asignacion de material generico
        IF ls_vbap_aux-attyp EQ '02'.
          MOVE: ls_vbap_aux-satnr TO ls_catalogo-mat_gen.
        ELSEIF ls_vbap_aux-attyp EQ '00'.
          MOVE: ls_vbap_aux-matnr TO ls_catalogo-mat_gen.
        ENDIF.

        IF ls_vbap_aux-vgbel IS NOT INITIAL.

          "Esto aplica para los pedidos Hijo (unidades picking)
          SELECT * FROM vbfa INTO TABLE @DATA(lt_vbfa)
            WHERE vbelv EQ @ls_vbap_aux-vbeln
            AND   posnv EQ @ls_vbap_aux-vgpos
            AND   vbtyp_n EQ 'J'.
          IF sy-subrc EQ 0.
            LOOP AT lt_vbfa INTO DATA(ls_rfmng).
              SELECT SINGLE kostk, wbstk FROM likp INTO @DATA(ls_likp)
                WHERE vbeln EQ @ls_rfmng-vbeln.
              IF sy-subrc EQ 0 AND ls_likp-wbstk NE 'C'.
                ls_catalogo-unid_picking += ls_rfmng-rfmng.
              ENDIF.
            ENDLOOP.
          ENDIF.

        ELSE.

          "En caso de ser padre, se obtienen todos los hijos y se hace la resta de las cantidades que ellos tienen al padre.
          SELECT * FROM vbfa INTO TABLE @DATA(lt_vbfa_aux)
            WHERE vbelv EQ @ls_vbap_aux-vbeln
            AND  posnv EQ @ls_vbap_aux-posnr
            AND  vbtyp_n EQ 'C'.
          IF sy-subrc EQ 0 AND ls_vbap_aux-auart EQ 'ZOR1'.
            SELECT vbeln, posnr, bmeng, wmeng, ordqty_bu, ordqty_su FROM vbep INTO TABLE @lt_vbep
              FOR ALL ENTRIES IN @lt_vbfa_aux
              WHERE vbeln = @lt_vbfa_aux-vbeln
              AND posnr = @lt_vbfa_aux-posnn.
            IF sy-subrc EQ 0.
              LOOP AT lt_vbep INTO ls_vbep.
                ls_catalogo-c_confirmada -= ls_vbep-bmeng.
                IF ls_vbep-ordqty_bu IS NOT INITIAL.
                  ls_catalogo-c_pendiente -= ls_vbep-ordqty_bu.
                ELSE.
                  ls_catalogo-c_pendiente -= ls_vbep-ordqty_su.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.

          "Unidades picking, calculo Entregas
          SELECT * FROM vbfa INTO TABLE @lt_vbfa
            WHERE vbelv EQ @ls_vbap_aux-vbeln
            AND   posnv EQ @ls_vbap_aux-posnr
            AND   vbtyp_n EQ 'J'.
          IF sy-subrc EQ 0.
            LOOP AT lt_vbfa INTO ls_rfmng.
              SELECT SINGLE kostk, wbstk FROM likp INTO @ls_likp
                WHERE vbeln EQ @ls_rfmng-vbeln.
              "En caso de ser padre
              IF ls_likp-wbstk NE 'C' AND ls_vbap_aux-auart EQ 'ZOR1' .
                ls_catalogo-c_pendiente -= ls_rfmng-rfmng.

              "En caso de no ser padre
              ELSEIF ls_likp-wbstk NE 'C' AND ls_vbap_aux-auart NE 'ZOR1'.
                ls_catalogo-unid_picking += ls_rfmng-rfmng.

              "Cuando ya se trato completamente la unidad picking
              ELSEIF ls_likp-kostk EQ 'C' AND ls_vbap_aux-auart EQ 'ZOR1'.
                ls_catalogo-c_pendiente -= ls_rfmng-rfmng.

              "Independiente de la relacion
              ELSEIF ls_likp-wbstk EQ 'C'.
                ls_catalogo-c_confirmada -= ls_rfmng-rfmng.
              ENDIF.
            ENDLOOP.
          ENDIF.

          "En caso de que venga algun valor negativo no se muestre
          ls_catalogo-c_pendiente = COND #( WHEN ls_catalogo-c_pendiente LT 0 THEN 0 ELSE ls_catalogo-c_pendiente ).
          ls_catalogo-c_pedido = COND #( WHEN ls_catalogo-c_pedido LT 0 THEN 0 ELSE ls_catalogo-c_pedido ).
          ls_catalogo-c_confirmada = COND #( WHEN ls_catalogo-c_confirmada LT 0 THEN 0 ELSE ls_catalogo-c_confirmada ).

          IF ls_catalogo-auart EQ 'ZOR1' AND ls_catalogo-c_pendiente EQ 0 AND ls_catalogo-c_pedido EQ 0 AND ls_catalogo-c_confirmada EQ 0 AND ls_catalogo-unid_picking EQ 0.
            ls_catalogo-c_asignada = '0'.
          ENDIF.

        ENDIF.

        "cantidad no confirmada
        ls_catalogo-c_no_conf = ls_catalogo-c_pedido - ls_catalogo-c_confirmada.

        "Obtencion del nombre del solicitante y destinatario
        SELECT SINGLE * FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr EQ @ls_catalogo-solicitante.
        IF sy-subrc EQ 0.
          ls_catalogo-nombre_clte = |{ ls_kna1-name1 } { ls_kna1-name2 }|.
        ENDIF.

        SELECT SINGLE * FROM kna1 INTO ls_kna1 WHERE kunnr EQ ls_catalogo-destinatario.
        IF sy-subrc EQ 0.
          ls_catalogo-n_destinatario = |{ ls_kna1-name1 } { ls_kna1-name2 }|.
        ENDIF.

        "Mes - Año
        IF ls_catalogo-f_entrega IS INITIAL.
          SELECT SINGLE erdat FROM vbap INTO ls_catalogo-f_entrega
            WHERE vbeln EQ ls_catalogo-doc_ventas
            AND   posnr EQ ls_catalogo-num_pos_p.
        ENDIF.

        CLEAR: lv_fecha.
        CALL FUNCTION 'CONVERSION_EXIT_LDATE_OUTPUT'
          EXPORTING
            input  = ls_catalogo-f_entrega
          IMPORTING
            output = lv_fecha.

        ls_catalogo-mes_ano = lv_fecha+4.
        CONDENSE ls_catalogo-mes_ano.

        "Precio retail
        SELECT SINGLE a~knumh, b~kbetr FROM a700 AS a
          JOIN konp AS b ON  a~knumh = b~knumh
          INTO @DATA(ls_a700)
          WHERE a~matnr EQ @ls_catalogo-mat_gen
          AND a~kschl EQ 'ZPR0'
          AND a~vkorg EQ @ls_vbap_aux-vkorg_ana
          AND a~vtweg EQ '10'
          AND a~pltyp EQ '00'
          AND b~kopos EQ '01'.
        IF sy-subrc EQ 0.

          ls_catalogo-p_unitario_ret  = ls_a700-kbetr * '1.16'.

        ENDIF.

        "pendiente entrega wholesale
        ls_catalogo-pend_entr_who = ls_catalogo-p_wholesome * ls_vbap_aux-c_pendiente.

        "pendiente entrega retail
        ls_catalogo-pend_entr_ret = ls_catalogo-p_unitario_ret * ls_vbap_aux-c_pendiente.

        "importe asignado wholesale
        ls_catalogo-imp_asig_who = ls_catalogo-p_wholesome * ls_vbap_aux-c_asignada.

        "importe asignado retail
        ls_catalogo-imp_asig_ret = ls_catalogo-p_unitario_ret * ls_vbap_aux-c_asignada.

        "importe picking wholesale
        ls_catalogo-imp_pick_who = ls_catalogo-p_wholesome * ls_catalogo-unid_picking.

        "importe picking retail
        ls_catalogo-imp_pick_ret = ls_catalogo-p_unitario_ret * ls_catalogo-unid_picking.

        "Remover ceros
        me->remover_ceros( CHANGING ls_catalogo = ls_catalogo ).

        READ TABLE lt_catalogo INTO DATA(ls_catalogo_aux) WITH KEY doc_ventas = ls_catalogo-doc_ventas
        num_pos_p = ls_catalogo-num_pos_p
        material = ls_catalogo-material
        mat_gen = ls_catalogo-mat_gen
        centro = ls_catalogo-centro
        almacen = ls_catalogo-almacen.
        IF sy-subrc EQ 0.
          ls_catalogo-c_asignada += ls_catalogo_aux-c_asignada.
          MODIFY TABLE lt_catalogo FROM ls_catalogo TRANSPORTING c_asignada.
        ELSE.
          APPEND ls_catalogo TO lt_catalogo.
        ENDIF.

      ENDLOOP.

    ELSE.
      MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.

    "Se eliminan todos los documentos padre que se hayan procesado por completo
    SORT lt_catalogo BY centro almacen doc_ventas num_pos_p.
    LOOP AT lt_catalogo INTO DATA(ls_catalogo_aux2)
      WHERE c_confirmada IS INITIAL
      AND   c_pendiente  IS INITIAL
      AND   c_pedido     IS INITIAL
      AND   c_asignada   IS INITIAL.
      DELETE lt_catalogo WHERE doc_ventas EQ ls_catalogo_aux2-doc_ventas AND num_pos_p EQ ls_catalogo_aux2-num_pos_p.
    ENDLOOP.

  ENDMETHOD.

  METHOD remover_ceros.

    ls_catalogo-doc_ventas = |{ ls_catalogo-doc_ventas ALPHA = OUT }|.
    ls_catalogo-num_pos_p = |{ ls_catalogo-num_pos_p ALPHA = OUT }|.
    ls_catalogo-solicitante = |{ ls_catalogo-solicitante ALPHA = OUT }|.
    ls_catalogo-destinatario = |{ ls_catalogo-destinatario ALPHA = OUT }|.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = ls_catalogo-material
      IMPORTING
        output = ls_catalogo-material.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = ls_catalogo-mat_gen
      IMPORTING
        output = ls_catalogo-mat_gen.


  ENDMETHOD.

  METHOD get_color.

    DATA: lv_atinn      TYPE char30.

    "En caso de que el ATINN para el color cambie este valor se debe cambiar o ajustar
*    lv_atinn = '0000000817'. "COLOR

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'COLOR'
      IMPORTING
        output = lv_atinn.


    SELECT SINGLE * FROM zvmatnrcalss26 INTO @DATA(ls_class26)
     WHERE  matnr EQ @ps_mara-matnr
      AND   mandt EQ @sy-mandt
      AND   klart NE '300'
      AND   atinn EQ @lv_atinn.
    IF sy-subrc EQ 0.
      SELECT SINGLE atwtb FROM zvcarac_charval INTO ps_catalogo-color
          WHERE atinn EQ ls_class26-atinn
          AND   atwrt EQ ls_class26-atwrt
          AND   spras EQ sy-langu
          AND   mandt EQ sy-mandt.
      IF sy-subrc NE 0.
        SELECT SINGLE atwtb FROM zvcaracterist INTO ps_catalogo-color
          WHERE atinn EQ ls_class26-atinn
          AND   atwrt EQ ls_class26-atwrt
          AND   spras EQ sy-langu
          AND   mandt EQ sy-mandt.
      ENDIF.
*      ps_catalogo-color = ls_class26-atwrt.
    ENDIF.

  ENDMETHOD.

  METHOD get_silueta.

    SELECT SINGLE vtext FROM t179t
      INTO @ps_catalogo-silueta
      WHERE prodh EQ (
        SELECT prodh
        FROM t179
          WHERE prodh EQ @ps_mara-prdha(10)
          AND stufe EQ '4' )
      AND   spras EQ 'S'.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.

  METHOD m_show_alv_factory.

    DATA: lc_msg      TYPE REF TO cx_salv_msg,
          lo_sorts    TYPE REF TO cl_salv_sorts,
          lv_col_name TYPE lvc_fname,
          lv_name     TYPE string,
          lt_fcat     TYPE lvc_t_fcat,
          lv_name_txt TYPE scrtext_l.

    DATA: gr_layout TYPE REF TO cl_salv_layout.
    DATA: key TYPE salv_s_layout_key.
    DATA: gr_functions TYPE REF TO   cl_salv_functions_list.
    DATA(lt_alv_aux) = obj_catalogo->lt_catalogo[].

    lt_alv_aux[] = it_alv[].

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = ob_alv
          CHANGING
            t_table      = lt_alv_aux[].
      CATCH cx_salv_msg INTO lc_msg .
    ENDTRY.

    CALL METHOD ob_alv->get_columns
      RECEIVING
        value = ob_columns.
*
    gr_functions = ob_alv->get_functions( ).
    gr_functions->set_all( abap_true ).

    gr_layout = ob_alv->get_layout( ).
    key-report = sy-repid.
    gr_layout->set_key( key ).

    gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    DATA: o_selections TYPE REF TO cl_salv_selections.
    o_selections = ob_alv->get_selections( ).
    o_selections->set_selection_mode( cl_salv_selections=>multiple ).

*    TRY.
*        DATA(lr_columnas) = ob_alv->get_columns( ).
*        lr_columnas->set_exception_column( value = 'ICON' ).
*      CATCH cx_salv_data_error INTO DATA(lv_error).
*        "e_code = lv_error->get_text( ).
*    ENDTRY.

* Columns
    ob_columns = ob_alv->get_columns( ).

    ob_columns->set_key_fixation( value = abap_true ).

    ob_column ?= ob_columns->get_column( 'CENTRO' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'CENTRO' position = 1 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Centro' ).
    ob_column->set_medium_text( 'Centro' ).
    ob_column->set_long_text( 'Centro' ).
    ob_column->set_key( if_salv_c_bool_sap=>true ).

    ob_column ?= ob_columns->get_column( 'ALMACEN' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'ALMACEN' position = 2 ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Almacen' ).
    ob_column->set_medium_text( 'Almacen' ).
    ob_column->set_long_text( 'Almacen' ).
    ob_column->set_key( if_salv_c_bool_sap=>true ).

    ob_column ?= ob_columns->get_column( 'TEMPORADA' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'TEMPORADA').
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Modelo' ).
    ob_column->set_medium_text( 'Temporada' ).
    ob_column->set_long_text( 'Temporada' ).

    ob_column ?= ob_columns->get_column( 'GPO_ARTICULOS' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'GPO_ARTICULOS').
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Texto Breve' ).
    ob_column->set_medium_text( 'Gpo Articulos' ).
    ob_column->set_long_text( 'Gpo Articulos' ).

    ob_column ?= ob_columns->get_column( 'DOC_VENTAS' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'DOC_VENTAS' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Documento de Ventas' ).
*    ob_column->set_medium_text( 'Documento de Ventas' ).
    ob_column->set_long_text( 'Documento de Ventas' ).

    ob_column ?= ob_columns->get_column( 'REF_CLI' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'REF_CLI' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'EAN' ).
*    ob_column->set_medium_text( 'EAN' ).
    ob_column->set_long_text( 'Referencia Cliente' ).

    ob_column ?= ob_columns->get_column( 'NUM_POS_P' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'NUM_POS_P' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'UM Base' ).
*    ob_column->set_medium_text( 'UM Base' ).
    ob_column->set_long_text( 'Núm Posición Pedido' ).

    ob_column ?= ob_columns->get_column( 'MAT_GEN' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MAT_GEN' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Familia' ).
*    ob_column->set_medium_text( 'Familia' ).
    ob_column->set_long_text( 'Material Generico' ).

    ob_column ?= ob_columns->get_column( 'MATERIAL' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MATERIAL' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Material' ).
    ob_column->set_medium_text( 'Material' ).
    ob_column->set_long_text( 'Material' ).

    ob_column ?= ob_columns->get_column( 'DESCRIPCION' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'DESCRIPCION' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Producto' ).
    ob_column->set_medium_text( 'Descripción' ).
    ob_column->set_long_text( 'Descripción' ).

    ob_column ?= ob_columns->get_column( 'TALLA' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'TALLA' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Talla' ).
    ob_column->set_medium_text( 'Talla' ).
    ob_column->set_long_text( 'Talla' ).

    ob_column ?= ob_columns->get_column( 'F_ENTREGA' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'F_ENTREGA' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Silueta M' ).
*    ob_column->set_medium_text( 'Silueta Macro' ).
    ob_column->set_long_text( 'Fecha de Entrega' ).

    ob_column ?= ob_columns->get_column( 'MODELO' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MODELO' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Modelo' ).
    ob_column->set_medium_text( 'Modelo' ).
    ob_column->set_long_text( 'Modelo' ).

    ob_column ?= ob_columns->get_column( 'COLOR' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'COLOR' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Color' ).
    ob_column->set_medium_text( 'Color' ).
    ob_column->set_long_text( 'Color' ).

    ob_column ?= ob_columns->get_column( 'SILUETA' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'SILUETA' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Silueta' ).
    ob_column->set_medium_text( 'Silueta' ).
    ob_column->set_long_text( 'Silueta' ).

    ob_column ?= ob_columns->get_column( 'FAMILIA' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'FAMILIA' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Familia' ).
    ob_column->set_medium_text( 'Familia' ).
    ob_column->set_long_text( 'Familia' ).

    ob_column ?= ob_columns->get_column( 'SOLICITANTE' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'SOLICITANTE' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Solicitante' ).
    ob_column->set_medium_text( 'Solicitante' ).
    ob_column->set_long_text( 'Solicitante' ).

    ob_column ?= ob_columns->get_column( 'NOMBRE_CLTE' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'NOMBRE_CLTE' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Nombre Clte' ).
    ob_column->set_medium_text( 'Nombre Clte' ).
    ob_column->set_long_text( 'Nombre Clte' ).

    ob_column ?= ob_columns->get_column( 'DESTINATARIO' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'DESTINATARIO' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Destinatario' ).
    ob_column->set_medium_text( 'Destinatario' ).
    ob_column->set_long_text( 'Destinatario' ).

    ob_column ?= ob_columns->get_column( 'N_DESTINATARIO' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'N_DESTINATARIO' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Division M' ).
*    ob_column->set_medium_text( 'Nombr' ).
    ob_column->set_long_text( 'Nombre Destinatario' ).

    ob_column ?= ob_columns->get_column( 'S_ESPECIAL_MAT' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'S_ESPECIAL_MAT' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Colaboración' ).
*    ob_column->set_medium_text( 'Colaboración' ).
    ob_column->set_long_text( 'Estatus Especial Material' ).

    ob_column ?= ob_columns->get_column( 'MES_ANO' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MES_ANO' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Subdivision' ).
    ob_column->set_medium_text( 'Mes - Año' ).
    ob_column->set_long_text( 'Mes - Año' ).

    ob_column ?= ob_columns->get_column( 'C_PEDIDO' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'C_PEDIDO' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Cantidad Pedido' ).
    ob_column->set_medium_text( 'Cantidad Pedido' ).
    ob_column->set_long_text( 'Cantidad Pedido' ).

    ob_column ?= ob_columns->get_column( 'C_CONFIRMADA' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'C_CONFIRMADA' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Temporada' ).
    ob_column->set_medium_text( 'Cantidad Confirmada' ).
    ob_column->set_long_text( 'Cantidad Confirmada' ).

    ob_column ?= ob_columns->get_column( 'C_PENDIENTE' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'C_PENDIENTE' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Market' ).
    ob_column->set_medium_text( 'Cantidad Pendiente' ).
    ob_column->set_long_text( 'Cantidad Pendiente' ).

    ob_column ?= ob_columns->get_column( 'P_WHOLESOME' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'P_WHOLESOME').
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Tema Temp' ).
    ob_column->set_medium_text( 'Precio Wholesale' ).
    ob_column->set_long_text( 'Precio Wholesale' ).

    ob_column ?= ob_columns->get_column( 'P_UNITARIO_RET' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'P_UNITARIO_RET').
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Alto' ).
*    ob_column->set_medium_text( '' ).
    ob_column->set_long_text( 'Precio Unitario Retail' ).

    ob_column ?= ob_columns->get_column( 'PEND_ENTR_WHO' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'PEND_ENTR_WHO' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Largo' ).
*    ob_column->set_medium_text( 'Largo' ).
    ob_column->set_long_text( 'Pendiente de Entrega Wholesale' ).

    ob_column ?= ob_columns->get_column( 'PEND_ENTR_RET' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'PEND_ENTR_RET' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Ancho' ).
*    ob_column->set_medium_text( 'Ancho' ).
    ob_column->set_long_text( 'Pendiente de Entrega Retail' ).

    ob_column ?= ob_columns->get_column( 'C_ASIGNADA' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'C_ASIGNADA' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Peso Bruto' ).
*    ob_column->set_medium_text( 'Peso Bruto' ).
    ob_column->set_long_text( 'Cantidad Asignada (LU)' ).

    ob_column ?= ob_columns->get_column( 'IMP_ASIG_WHO' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'IMP_ASIG_WHO' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Peso Neto' ).
*    ob_column->set_medium_text( 'Peso Neto' ).
    ob_column->set_long_text( 'Importe Asignado Wholesale' ).

    ob_column ?= ob_columns->get_column( 'IMP_ASIG_RET' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'IMP_ASIG_RET' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_medium_text( 'Dimensiones' ).
    ob_column->set_long_text( 'Importe Asignado Retail' ).

    ob_column ?= ob_columns->get_column( 'UNID_PICKING' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'UNID_PICKING' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_medium_text( 'Fraccion Arancelaria' ).
    ob_column->set_long_text( 'Unidades en Picking' ).

    ob_column ?= ob_columns->get_column( 'IMP_PICK_WHO' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'IMP_PICKING_WHO' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_medium_text( 'Texto Pedido Compras' ).
    ob_column->set_long_text( 'Importe en Picking wholesale' ).

    ob_column ?= ob_columns->get_column( 'IMP_PICK_RET' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'IMP_PICKING_RET' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_medium_text( 'Texto Pedido Compras' ).
    ob_column->set_long_text( 'Importe en Picking Retail' ).

    ob_column ?= ob_columns->get_column( 'F_CREACION_PED' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'F_CREACION_PED' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_medium_text( 'Texto Pedido Compras' ).
    ob_column->set_long_text( 'Fecha Creación Pedido' ).

* Calling Set PF status method
*    CALL METHOD m_set_pf_status
*      CHANGING
*        co_alv = ob_alv.

    ob_alv->display( ).

  ENDMETHOD.

  METHOD m_show_alv.

    DATA(gt_data) = obj_catalogo->lt_catalogo[].

    gt_data[] = it_alv[].

    APPEND VALUE #( fieldname = 'CENTRO'
                    seltext_s = 'Centro'
                    seltext_l = 'Centro')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'ALMACEN'
                    seltext_s = 'Almacen'
                    seltext_l = 'Almacen')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'TEMPORADA'
                    seltext_s = 'Temporada'
                    seltext_l = 'Temporada')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'GPO_ARTICULOS'
                    intlen    = 9
                    seltext_s = 'Gpo Articulos'
                    seltext_l = 'Gpo Articulos')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'DOC_VENTAS'
                    seltext_l = 'Documento de Ventas')
                    TO gt_slis_fieldcat.

    APPEND VALUE #( fieldname = 'AUART'
                    seltext_l = 'Clase doc. Ventas')
                    TO gt_slis_fieldcat.

    APPEND VALUE #( fieldname = 'REF_CLI'
                    intlen    = 35
                    seltext_l = 'Referencia Cliente')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'NUM_POS_P'
                    seltext_l = 'Núm Posición Pedido')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'MAT_GEN'
                    intlen    = 40
                    seltext_l = 'Material Generico')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'MATERIAL'
                    intlen    = 40
                    seltext_l = 'Material')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'EAN'
                    intlen = 20
                    seltext_l = 'EAN')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'DESCRIPCION'
                    intlen = 40
                    seltext_l = 'Descripción')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'TALLA'
                    intlen    = 18
                    seltext_l = 'Talla')
                    TO gt_slis_fieldcat.

    APPEND VALUE #( fieldname = 'TALLA_MX'
                    intlen    = 18
                    seltext_l = 'Talla MX')
                    TO gt_slis_fieldcat.

    APPEND VALUE #( fieldname = 'F_ENTREGA'
                    seltext_l = 'Fecha de Entrega')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'MODELO'
                    intlen    = 18
                    seltext_l = 'Modelo')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'COLOR'
                    seltext_l = 'Color')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'SILUETA'
                    seltext_l = 'Silueta')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'FAMILIA'
*                    seltext_m = 'Familia'
                    seltext_l = 'Familia')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'SOLICITANTE'
                    intlen    = 50
                    seltext_l = 'Solicitante')
                    TO gt_slis_fieldcat.

    APPEND VALUE #( fieldname = 'NOMBRE_CLTE'
                    intlen    = 50
                    seltext_l = 'Nombre Clte')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'DESTINATARIO'
                    intlen    = 50
                    seltext_l = 'Destinatario')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'N_DESTINATARIO'
                    intlen    = 50
                    seltext_l = 'Nombre Destinatario')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'S_ESPECIAL_MAT'
                    seltext_l = 'Estatus Especial Material')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'MES_ANO'
                    seltext_l = 'Mes - Año')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'C_PEDIDO'
                    seltext_l = 'Cantidad Pedido')
                    TO gt_slis_fieldcat.

    APPEND VALUE #( fieldname = 'C_CONFIRMADA'
                    seltext_l = 'Cantidad Confirmada')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'C_PENDIENTE'
                    seltext_l = 'Cantidad Pendiente sin Entrega')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'C_NO_CONF'
                    seltext_l = 'C. No Confirmada')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'P_WHOLESOME'
                    seltext_l = 'Precio Wholesale')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'P_UNITARIO_RET'
                    seltext_l = 'Precio Unitario Retail')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'PEND_ENTR_WHO'
                    seltext_l = 'Pendiente de Entrega Wholesale')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'PEND_ENTR_RET'
                    seltext_l = 'Pendiente de Entrega Retail')
                    TO gt_slis_fieldcat.

    APPEND VALUE #( fieldname = 'C_ASIGNADA'
                    seltext_l = 'Cantidad Asignada (LU)')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'IMP_ASIG_WHO'
                    seltext_l = 'Importe Asignado Wholesale')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'IMP_ASIG_RET'
                    seltext_l = 'Importe Asignado Retail')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'UNID_PICKING'
                    seltext_l = 'Unidades en Picking')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'IMP_PICK_WHO'
                    seltext_l = 'Importe en Picking wholesale')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'IMP_PICK_RET'
                    seltext_l = 'Importe en Picking Retail')
                    TO gt_slis_fieldcat.
    APPEND VALUE #( fieldname = 'F_CREACION_PED'
                    seltext_l = 'Fecha Creación Pedido')
                    TO gt_slis_fieldcat.

    gs_slis_layout-colwidth_optimize   = 'X'.
    gs_slis_layout-zebra = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = sy-cprog
        i_structure_name        = 'RETURN2'
        is_layout               = gs_slis_layout
        it_fieldcat             = gt_slis_fieldcat
        i_callback_user_command = 'USER_COMMAND'
        i_save                  = 'A'
      TABLES
        t_outtab                = gt_data
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE 'Program Error' TYPE 'i'.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

*  * Setting Default PF-Status
  METHOD m_set_pf_status.
    SET PF-STATUS 'STANDARD2'.
    ob_alv->set_screen_status(
    pfstatus      =  'STANDARD2'
    report       = sy-repid
    set_functions = ob_alv->c_functions_all ).

  ENDMETHOD.                    "set_pf_status

ENDCLASS.

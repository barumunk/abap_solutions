*&---------------------------------------------------------------------*
*& Include          ZMMRE_CLASIFICACION_ATB_CL
*&---------------------------------------------------------------------*
*------------------------------------------------------------------------*
* Programa   : zmmre_clasificacion_atributos                             *
* Descripción: Reporte para consultar la información detallada
* del maestro del material con sus diferentes carácteristicas y atributos*
* Programador: David Navoa Acevedo                                       *
* Fecha      : 20.11.2023                                                *
*------------------------------------------------------------------------*

CLASS lcl_catalogo IMPLEMENTATION.

  METHOD m_get_data.

    DATA: lv_fiber_code TYPE wrf_fiber_codest-fiber_code_descr.
    DATA: lv_matr_str   TYPE mara-matnr,
          lv_matr_satrn TYPE mara-matnr.

    "Variante
    IF p_check EQ abap_true AND p2_check EQ abap_false.
      SELECT * FROM mara INTO TABLE @DATA(lt_mara)
        WHERE matkl IN @s_gpo_a
        AND   satnr IN @s_matrl
        AND   normt IN @s_model
        AND   ferth IN @s_cod_f
        AND   lvorm IN @s_marcb.
      "Variante y generico
    ELSEIF p_check EQ abap_true AND p2_check EQ abap_true.
      SELECT * FROM mara INTO TABLE @lt_mara
        WHERE matkl IN @s_gpo_a
        AND   (
        satnr IN @s_matrl OR matnr IN @s_matrl )
        AND   normt IN @s_model
        AND   ferth IN @s_cod_f
        AND   lvorm IN @s_marcb.
      "generico
    ELSE .
      SELECT * FROM mara INTO TABLE @lt_mara
        WHERE matkl IN @s_gpo_a
        AND   matnr IN @s_matrl
        AND   normt IN @s_model
        AND   ferth IN @s_cod_f
        AND   lvorm IN @s_marcb
        AND   satnr EQ ''.
    ENDIF.
    IF sy-subrc EQ 0.

      me->m_create_catalog( ).

      me->m_create_table( EXPORTING pt_fcat = gt_fcat ).

      me->m_create_aux( ).

      me->m_create_structure( ).

      LOOP AT lt_mara INTO DATA(ls_mara) WHERE prdha IS NOT INITIAL AND prdha NE ' '.
        CLEAR: <gs_dyntable>, lv_fiber_code.
        "filtro para unicamente variantes
        IF p2_check EQ abap_false AND ls_mara-satnr IS INITIAL.
          CONTINUE.
        ENDIF.
        "Filtro para el tipo de producto
        DATA(lv_class_aux1) = ls_mara-prdha+2(2).
        IF lv_class_aux1 NOT IN s_tprod.
          "check para clase 001
          SELECT SINGLE klart, clint FROM kssk INTO @DATA(ls_klart_aux1)
            WHERE objek EQ @ls_mara-matnr.
          IF sy-subrc EQ 0 AND ls_klart_aux1-klart EQ '001'.
            SELECT SINGLE class FROM klah INTO @DATA(lv_class)
              WHERE clint EQ @ls_klart_aux1-clint.
            IF sy-subrc NE 0.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        "gpo Articulo
        ASSIGN COMPONENT 'MAKTL' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-matkl.
        "Material SAP
        ASSIGN COMPONENT 'MATNR' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = |{ ls_mara-matnr ALPHA = OUT }|.
        "modelo
        ASSIGN COMPONENT 'NORMT' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-normt.
        "Texto breve
        DATA: lv_maktx(40).
        SELECT SINGLE maktx FROM makt INTO @lv_maktx WHERE matnr EQ @ls_mara-matnr.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'MAKTX' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = lv_maktx.
        ENDIF.
        "um base
        ASSIGN COMPONENT 'MEINS' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-meins.
        "familia
        SELECT SINGLE ewbez FROM twewt INTO @DATA(lv_familia_desc)
          WHERE extwg EQ @ls_mara-extwg
          AND   spras EQ @sy-langu.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'EXTWG' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = lv_familia_desc."ls_mara-extwg.
        ENDIF.
        "marca
        SELECT SINGLE brand_descr FROM wrf_brands_t INTO @DATA(lv_brand_desc)
          WHERE  brand_id EQ @ls_mara-brand_id
          AND    language EQ @sy-langu.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'BRAND_ID' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = lv_brand_desc."ls_mara-brand_id.
        ENDIF.
        "producto
        SELECT SINGLE vtext FROM t179t INTO @DATA(lv_prdha2) WHERE spras EQ 'S' AND prodh EQ ( SELECT prodh FROM t179 WHERE prodh EQ @ls_mara-prdha(4) AND stufe EQ '2' ).
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'PRDHA2' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = lv_prdha2.
        ENDIF.
        "Genero
        SELECT SINGLE vtext FROM t179t INTO @DATA(lv_prdha3) WHERE spras EQ 'S' AND prodh EQ ( SELECT prodh FROM t179 WHERE prodh EQ @ls_mara-prdha(6) AND stufe EQ '3' ).
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'PRDHA3' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = lv_prdha3.
        ENDIF.
        "Silueta marco
        SELECT SINGLE vtext FROM t179t INTO @DATA(lv_prdha4) WHERE spras EQ 'S' AND prodh EQ ( SELECT prodh FROM t179 WHERE prodh EQ @ls_mara-prdha(10) AND stufe EQ '4' ).
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'PRDHA4' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = lv_prdha4.
        ENDIF.
        "Silueta Detallada
        SELECT SINGLE vtext FROM t179t INTO @DATA(lv_prdha5) WHERE spras EQ 'S' AND prodh EQ ( SELECT prodh FROM t179 WHERE prodh EQ @ls_mara-prdha AND stufe EQ '5' ).
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'PRDHA5' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = lv_prdha5.
        ENDIF.
        "pais origen
        SELECT SINGLE wherl FROM maw1 INTO @DATA(lv_wherl) WHERE matnr EQ @ls_mara-matnr.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'WHERL' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = lv_wherl.
        ENDIF.
        "Marcado para borrado
        ASSIGN COMPONENT 'LVORM' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-lvorm.
        "Codigo Fabricacion
        ASSIGN COMPONENT 'FERTH' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-ferth.
        "modelo padre
        ASSIGN COMPONENT 'ZEINR' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-zeinr.
        "Composision de material
        ASSIGN COMPONENT 'WRKST' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-wrkst.
        "Material Generico
        SELECT * FROM wrf_textile_fibr INTO TABLE @DATA(lt_fbr_text)
          WHERE matnr EQ @ls_mara-matnr.
        IF sy-subrc EQ 0.
          SELECT * FROM wrf_fiber_codest INTO TABLE @DATA(lt_fbr_codest)
            FOR ALL ENTRIES IN @lt_fbr_text
            WHERE fiber_code EQ @lt_fbr_text-fiber_code
            AND language EQ 'S'.
          IF sy-subrc EQ 0.
            LOOP AT lt_fbr_codest INTO DATA(ls_fbr_codest).
              IF lv_fiber_code IS INITIAL .
                lv_fiber_code = ls_fbr_codest-fiber_code_descr.
              ELSEIF lv_fiber_code CP ls_fbr_codest-fiber_code_descr.
              ELSE.
                lv_fiber_code = |{ lv_fiber_code }, { ls_fbr_codest-fiber_code_descr }|.
              ENDIF.
            ENDLOOP.
            ASSIGN COMPONENT 'FIBER_CODE' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
            <gv_fldval> = lv_fiber_code.
          ENDIF.
        ENDIF.

        "division marca
        ASSIGN COMPONENT 'FSH_MG_AT1' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-fsh_mg_at1.
        "colaboracion
        ASSIGN COMPONENT 'FSH_MG_AT2' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-fsh_mg_at2.
        "subdivision
        ASSIGN COMPONENT 'FSH_MG_AT3' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-fsh_mg_at3.

        SELECT SINGLE * FROM fsh_seasons_mat INTO @DATA(ls_seasons)
          WHERE matnr EQ @ls_mara-matnr
          AND   fsh_season IN @s_temp.
        IF sy-subrc EQ 0.
          "Año
          ASSIGN COMPONENT 'YEAR' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = ls_seasons-fsh_season_year.
          "Temporada
          ASSIGN COMPONENT 'TEMPORADA' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = ls_seasons-fsh_season.
          "Market
          ASSIGN COMPONENT 'MARKET' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = ls_seasons-fsh_collection.
          "Tema temporada
          ASSIGN COMPONENT 'TEMA_TEMP' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = ls_seasons-fsh_theme.
        ELSEIF sy-subrc NE 0 AND ls_mara-satnr IS NOT INITIAL.

          "Logica para variante
          DATA(lv_strlen) = strlen( ls_mara-matnr ).
          lv_strlen = lv_strlen - 3.
          lv_matr_str = ls_mara-matnr(lv_strlen).
          lv_matr_str = |{ lv_matr_str ALPHA = OUT }|.
          lv_matr_satrn = |{ ls_mara-satnr ALPHA = OUT }|.
          IF lv_matr_str = lv_matr_satrn.
            SELECT SINGLE * FROM fsh_seasons_mat INTO @ls_seasons
              WHERE matnr EQ @ls_mara-satnr
              AND   fsh_season IN @s_temp.
            IF sy-subrc EQ 0.
              "Año
              ASSIGN COMPONENT 'YEAR' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
              <gv_fldval> = ls_seasons-fsh_season_year.
              "Temporada
              ASSIGN COMPONENT 'TEMPORADA' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
              <gv_fldval> = ls_seasons-fsh_season.
              "Market
              ASSIGN COMPONENT 'MARKET' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
              <gv_fldval> = ls_seasons-fsh_collection.
              "Tema temporada
              ASSIGN COMPONENT 'TEMA_TEMP' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
              <gv_fldval> = ls_seasons-fsh_theme.
            ELSEIF sy-subrc NE 0 AND s_temp IS NOT INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSEIF sy-subrc NE 0 AND s_temp IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        SELECT SINGLE * FROM marm INTO @DATA(ls_marm) WHERE matnr EQ @ls_mara-matnr.
        IF sy-subrc EQ 0.
          "Alto
          ASSIGN COMPONENT 'HOEHE' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = ls_marm-hoehe.
          "Largo
          ASSIGN COMPONENT 'LAENG' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = ls_marm-laeng.
          "Ancho
          ASSIGN COMPONENT 'BREIT' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = ls_marm-breit.
          "Peso Bruto
          ASSIGN COMPONENT 'BRGEW' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
          <gv_fldval> = ls_marm-brgew.
        ENDIF.

        "Peso Neto
        ASSIGN COMPONENT 'NTGEW' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-ntgew.
        "Dimensiones
        ASSIGN COMPONENT 'GROES' OF STRUCTURE <gs_dyntable> TO <gv_fldval>.
        <gv_fldval> = ls_mara-groes.
        "clasificaciones
        me->m_fill_clasf( EXPORTING ps_mara = ls_mara ).

      ENDLOOP.

      "me->m_move_data( ).

    ELSE.
      MESSAGE 'No se encontraron datos' TYPE 'I'.
    ENDIF.



  ENDMETHOD.

  METHOD m_create_aux.
    DATA lo_newtable TYPE REF TO data.
    DATA lo_newline TYPE REF TO data.

    "creacion de tabla auxiliar
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = gt_fcat_aux
      IMPORTING
        ep_table        = lo_newtable.

    ASSIGN lo_newtable->* TO <gt_dyntable_aux>.

    "Creacion de estructura auxiliar
    CREATE DATA lo_newline LIKE LINE OF <gt_dyntable_aux>.
    ASSIGN lo_newline->* TO <gs_dyntable_aux>.


  ENDMETHOD.

  METHOD m_create_catalog.

    TYPES: BEGIN OF ty_class,
             klart       TYPE klassenart,
             atnam       TYPE atnam,
             class       TYPE klasse_d,
             descripcion TYPE zde_descripcion,
           END OF ty_class.

    DATA: lt_caracteristicas_aux TYPE TABLE OF ty_caracteristicas,
          lt_classes             TYPE TABLE OF ty_caracteristicas,
          lt_caract              TYPE TABLE OF ty_class.

    DATA: ls_fcat     TYPE lvc_s_fcat, "área de trabajo local para el catálogo de campos
          ls_fcat_aux TYPE lvc_s_fcat.
    DATA: ls_caract_aux TYPE ty_class.
    DATA: lv_tabix TYPE char5,
          lv_atinn TYPE char30.

    me->m_create_catalog_head( CHANGING pt_fcat = gt_fcat ).
    me->m_create_catalog_head( CHANGING pt_fcat = gt_fcat_aux ).

    "Obtencion de caracteristicas existentes del material
    SELECT DISTINCT a~matnr, a~prdha, b~atinn, b~klart INTO TABLE @DATA(gt_caracteristicas)
      FROM mara AS a
      LEFT JOIN zvmatnrcalss26 AS b
      ON a~matnr EQ b~matnr
      WHERE a~matnr IN @s_matrl.
    IF sy-subrc EQ 0.

      LOOP AT gt_caracteristicas ASSIGNING FIELD-SYMBOL(<ls_caracteristicas>).
        <ls_caracteristicas>-prdha = <ls_caracteristicas>-prdha+2(2).

        CLEAR ls_caract_aux.
        SELECT SINGLE atnam FROM wrf_charval_head INTO CORRESPONDING FIELDS OF ls_caract_aux
          WHERE atinn EQ <ls_caracteristicas>-atinn.
        IF sy-subrc EQ 0.

          SELECT SINGLE * FROM ztt_claves_clas INTO CORRESPONDING FIELDS OF ls_caract_aux
            WHERE clase EQ <ls_caracteristicas>-prdha.

          ls_caract_aux-klart = <ls_caracteristicas>-klart.
          ls_caract_aux-class = <ls_caracteristicas>-prdha.

          APPEND ls_caract_aux TO lt_caract.

        ENDIF.


      ENDLOOP.

      DATA(lt_classes_aux) = gt_caracteristicas[].

      SORT lt_classes_aux BY prdha.
      <ls_caracteristicas>-prdha = 'CALZADO_IMP'.
      APPEND <ls_caracteristicas> TO lt_classes_aux.
      <ls_caracteristicas>-prdha = 'ACCESORIOS_IMP'.
      APPEND <ls_caracteristicas> TO lt_classes_aux.
      DELETE ADJACENT DUPLICATES FROM lt_classes_aux COMPARING prdha.
      UNASSIGN <ls_caracteristicas>.
      SELECT a~klart, a~atnam, a~class, b~descripcion FROM zvcaracterist AS a
        LEFT JOIN ztt_claves_clas AS b
        ON a~class EQ b~clase
        APPENDING TABLE @lt_caract
        FOR ALL ENTRIES IN @lt_classes_aux
        WHERE class = @lt_classes_aux-prdha
        AND klart IN ('001', '026').

      SORT lt_caract BY class.
      DELETE lt_caract WHERE class NOT IN s_tprod.

      "Llenado de caracteristica
      SORT lt_caract BY descripcion class.
      " DELETE ADJACENT DUPLICATES FROM lt_caract COMPARING descripcion class.
      LOOP AT lt_caract INTO DATA(ls_caracteristicas) WHERE class IS NOT INITIAL.
        CLEAR: ls_fcat, ls_fcat_aux.
        ls_fcat-fieldname = |{ ls_caracteristicas-class }-{ ls_caracteristicas-atnam }|.
        ls_fcat-datatype = gv_string.
        ls_fcat-intlen = 70.

        ls_fcat_aux-fieldname = |{ ls_caracteristicas-descripcion }-{ ls_caracteristicas-atnam }|.
        ls_fcat_aux-datatype = gv_string.
        ls_fcat_aux-intlen = 70.

        READ TABLE gt_fcat TRANSPORTING NO FIELDS WITH KEY fieldname = ls_fcat-fieldname.
        IF sy-subrc NE 0.
          APPEND ls_fcat TO gt_fcat.
        ENDIF.

        READ TABLE gt_fcat_aux TRANSPORTING NO FIELDS WITH KEY fieldname = ls_fcat_aux-fieldname.
        IF sy-subrc NE 0.
          APPEND ls_fcat_aux TO gt_fcat_aux.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD m_create_table.
    DATA lo_newtable TYPE REF TO data. "Para la tabla interna dinámica
    FIELD-SYMBOLS <fs_int> TYPE any.
    "Invocamos al método 'create_dynamic_table' de la clase cl_alv_table_create
    "Como es una clase estática accedemos directamente desde la clase al método.
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = pt_fcat
      IMPORTING
        ep_table        = lo_newtable.

    ASSIGN lo_newtable->* TO <gt_dyntable>.
  ENDMETHOD.

  METHOD m_create_structure.
    DATA lo_newline TYPE REF TO data. "Para el área de trabajo de la tabla interna dinámica.

    CREATE DATA lo_newline LIKE LINE OF <gt_dyntable>.
    ASSIGN lo_newline->* TO <gs_dyntable>.
  ENDMETHOD.

  METHOD m_fill_clasf.

    DATA: lv_atinn      TYPE char30,
          lv_clas_atinn TYPE atinn,
          lv_flag       TYPE c,
          lv_name       TYPE string,
          lv_name_aux   TYPE string.
    DATA : idetails TYPE abap_compdescr_tab.
    DATA : ref_table_des TYPE REF TO cl_abap_structdescr.
    DATA: ls_caract TYPE ty_caract.
    DATA: lt_caract TYPE TABLE OF ty_caract.

    MOVE-CORRESPONDING <gs_dyntable> TO <gs_dyntable_aux>.

    CLEAR: lt_caract[].
    SELECT * FROM zvmatnrcalss26 INTO TABLE @DATA(lt_class26)
      WHERE matnr EQ @ps_mara-matnr
      AND   klart NE '300'. "Se filtra unicamente 001 y 026
    IF sy-subrc EQ 0.

      SELECT * FROM zvcaracterist INTO CORRESPONDING FIELDS OF TABLE @lt_caract
        FOR ALL ENTRIES IN @lt_class26
        WHERE atinn EQ @lt_class26-atinn
        AND   atwrt EQ @lt_class26-atwrt.
      "AND   klart EQ @lt_class26-klart.

      SELECT SINGLE prdha FROM mara INTO @DATA(lv_prdha)
        WHERE matnr EQ @ps_mara-matnr.
      IF sy-subrc EQ 0.
        lv_prdha = lv_prdha+2(2).
      ENDIF.

      SORT lt_caract BY clint atinn.
      LOOP AT lt_class26 INTO DATA(ls_class26).
        READ TABLE lt_caract TRANSPORTING NO FIELDS WITH KEY atinn = ls_class26-atinn .
        IF sy-subrc NE 0.
          SELECT SINGLE atwtb FROM wrf_charvalt INTO @DATA(lv_charvalt) WHERE atinn = @ls_class26-atinn AND atwrt EQ @ls_class26-atwrt AND spras EQ @sy-langu.
          IF sy-subrc EQ 0.
            CLEAR ls_caract.
            ls_caract-atinn = ls_class26-atinn.
            ls_caract-atwrt = ls_class26-atwrt.
            ls_caract-atwtb = lv_charvalt.
            ls_caract-class = lv_prdha.
            ls_caract-klart = ls_caract-klart1 = ls_class26-klart.
            CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
              EXPORTING
                input  = ls_class26-atinn
              IMPORTING
                output = ls_caract-atnam.
            APPEND ls_caract TO lt_caract.
          ENDIF.
        ENDIF.
      ENDLOOP.

      CLEAR lv_clas_atinn.
      LOOP AT lt_caract INTO DATA(ls_clas) WHERE class = lv_prdha.

        DATA(lv_tabix) = sy-tabix.
        CLEAR lv_flag.

        "Flag para el multivalor
        IF lv_tabix NE 1 AND ls_clas-atinn EQ lv_clas_atinn.
          lv_flag = abap_true.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
          EXPORTING
            input  = ls_clas-atinn
          IMPORTING
            output = lv_atinn.

        lv_name = |{ ls_clas-class }-{ lv_atinn }|.

        SELECT SINGLE * FROM ztt_claves_clas INTO @DATA(ls_claves)
          WHERE clase EQ @ls_clas-class.
        IF sy-subrc EQ 0.
          lv_name_aux = |{ ls_claves-descripcion }-{ lv_atinn }|.
          ASSIGN COMPONENT lv_name_aux OF STRUCTURE <gs_dyntable_aux> TO <gv_fldval_aux>.
        ENDIF.

        ASSIGN COMPONENT lv_name OF STRUCTURE <gs_dyntable> TO <gv_fldval>.


        IF lv_flag EQ abap_true AND <gv_fldval> IS NOT INITIAL.
          IF <gv_fldval> CP ls_clas-atwrt.
          ELSE.
            <gv_fldval> = |{ <gv_fldval> }, { ls_clas-atwtb }|.
            IF <gv_fldval_aux> IS ASSIGNED.
              <gv_fldval_aux> = |{ <gv_fldval> }, { ls_clas-atwtb }|.
            ENDIF.
          ENDIF.
        ELSE.
          <gv_fldval> = ls_clas-atwtb.
          IF <gv_fldval_aux> IS ASSIGNED.
            <gv_fldval_aux> = ls_clas-atwtb.
          ENDIF.
        ENDIF.

        lv_clas_atinn = ls_clas-atinn.

      ENDLOOP.

    ENDIF.
*  Ajuste por objetos clase 001
    SELECT SINGLE klart, clint FROM kssk INTO @DATA(ls_klart_aux1)
      WHERE objek EQ @ps_mara-matnr.
    IF sy-subrc EQ 0 AND ls_klart_aux1-klart EQ '001'.
      SELECT SINGLE class FROM klah INTO @DATA(lv_class)
        WHERE clint EQ @ls_klart_aux1-clint.

      SELECT objek, atinn, klart, atwrt FROM ausp
        INTO TABLE @DATA(lt_class_001)
        WHERE objek EQ @ps_mara-matnr.
      IF sy-subrc EQ 0.

        SELECT * FROM zvcaracterist INTO TABLE @DATA(lt_caract_001)
          FOR ALL ENTRIES IN @lt_class_001
          WHERE atinn EQ @lt_class_001-atinn
          AND   atwrt EQ @lt_class_001-atwrt
          AND   klart EQ @lt_class_001-klart.
        IF sy-subrc EQ 0.

          SORT lt_caract_001 BY class.
          LOOP AT lt_caract_001 INTO DATA(ls_class_001) WHERE class EQ lv_class.
            lv_tabix = sy-tabix.
            CLEAR lv_flag.

            "Flag para el multivalor
            IF lv_tabix NE 1 AND ls_class_001-atinn EQ lv_clas_atinn.
              lv_flag = abap_true.
            ENDIF.

            CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
              EXPORTING
                input  = ls_class_001-atinn
              IMPORTING
                output = lv_atinn.

            lv_name = |{ ls_class_001-class }-{ lv_atinn }|.

            SELECT SINGLE * FROM ztt_claves_clas INTO @ls_claves
              WHERE clase EQ @lv_class.
            IF sy-subrc EQ 0.
              lv_name_aux = |{ ls_claves-descripcion }-{ lv_atinn }|.
              ASSIGN COMPONENT lv_name_aux OF STRUCTURE <gs_dyntable_aux> TO <gv_fldval_aux>.
            ENDIF.

            READ TABLE gt_fcat TRANSPORTING NO FIELDS WITH KEY fieldname = lv_name.
            IF sy-subrc NE 0.
              CONTINUE.
            ENDIF.

            ASSIGN COMPONENT lv_name OF STRUCTURE <gs_dyntable> TO <gv_fldval>.

            IF lv_flag EQ abap_true AND <gv_fldval> IS NOT INITIAL.
              IF <gv_fldval> CP ls_class_001-atwtb.
              ELSE.
                <gv_fldval> = |{ <gv_fldval> }, { ls_class_001-atwtb }|.
                IF <gv_fldval_aux> IS ASSIGNED.
                  <gv_fldval_aux> = |{ <gv_fldval> }, { ls_class_001-atwtb }|.
                ENDIF.
              ENDIF.
            ELSE.
              <gv_fldval> = ls_class_001-atwtb.
              IF <gv_fldval_aux> IS ASSIGNED.
                <gv_fldval_aux> = ls_class_001-atwtb.
              ENDIF.
            ENDIF.

            lv_clas_atinn = ls_class_001-atinn.

          ENDLOOP.

        ENDIF.

      ENDIF.
    ENDIF.
*Fin objetos clase 001
    APPEND <gs_dyntable> TO <gt_dyntable>.
    APPEND <gs_dyntable_aux> TO <gt_dyntable_aux>.

  ENDMETHOD.

  METHOD m_create_catalog_head.

    DATA: ls_fcat TYPE lvc_s_fcat. "área de trabajo local para el catálogo de campos

    ls_fcat-fieldname = 'MAKTL'.
    ls_fcat-datatype = mara-matkl.
    ls_fcat-intlen = 30.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'MATNR'.
    ls_fcat-datatype = mara-matnr.
    ls_fcat-intlen = 40.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'NORMT'.
    ls_fcat-datatype = mara-normt.
    ls_fcat-intlen = 18.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'MAKTX'.
    ls_fcat-datatype = makt-maktx.
    ls_fcat-intlen = 40.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'MEINS'.
    ls_fcat-datatype = mara-meins.
    ls_fcat-intlen = 3.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'EXTWG'.
    ls_fcat-datatype = mara-extwg.
    ls_fcat-intlen = 18.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'BRAND_ID'.
    ls_fcat-datatype = mara-brand_id.
    ls_fcat-intlen = 4.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'PRDHA2'.
    ls_fcat-datatype = t179t-vtext.
    ls_fcat-intlen = 40.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'PRDHA3'.
    ls_fcat-datatype = t179t-vtext.
    ls_fcat-intlen = 40.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'PRDHA4'.
    ls_fcat-datatype = t179t-vtext.
    ls_fcat-intlen = 40.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'PRDHA5'.
    ls_fcat-datatype = t179t-vtext.
    ls_fcat-intlen = 40.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'WHERL'.
    ls_fcat-datatype = maw1-wherl.
    ls_fcat-intlen = 3.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'LVORM'.
    ls_fcat-datatype = mara-lvorm.
    ls_fcat-intlen = 1.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'FERTH'.
    ls_fcat-datatype = mara-ferth.
    ls_fcat-intlen = 18.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'ZEINR'.
    ls_fcat-datatype = mara-zeinr.
    ls_fcat-intlen = 22.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'WRKST'.
    ls_fcat-datatype = mara-wrkst.
    ls_fcat-intlen = 48.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'FIBER_CODE'.
    ls_fcat-datatype = wrf_fiber_codest-fiber_code_descr.
    ls_fcat-intlen = 50.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'FSH_MG_AT1'.
    ls_fcat-datatype = mara-fsh_mg_at1.
    ls_fcat-intlen = 10.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'FSH_MG_AT2'.
    ls_fcat-datatype = mara-fsh_mg_at2.
    ls_fcat-intlen = 10.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'FSH_MG_AT3'.
    ls_fcat-datatype = mara-fsh_mg_at3.
    ls_fcat-intlen = 6.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'YEAR'.
    ls_fcat-datatype = fsh_seasons_mat-fsh_season_year.
    ls_fcat-intlen = 4.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'TEMPORADA'.
    ls_fcat-datatype = fsh_seasons_mat-fsh_season.
    ls_fcat-intlen = 10.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'MARKET'.
    ls_fcat-datatype = fsh_seasons_mat-fsh_collection.
    ls_fcat-intlen = 10.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'TEMA_TEMP'.
    ls_fcat-datatype = fsh_seasons_mat-fsh_theme.
    ls_fcat-intlen = 10.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'HOEHE'.
    ls_fcat-datatype = marm-hoehe.
    ls_fcat-intlen = 13.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'LAENG'.
    ls_fcat-datatype = marm-laeng.
    ls_fcat-intlen = 13.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'BREIT'.
    ls_fcat-datatype = marm-breit.
    ls_fcat-intlen = 17.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'BRGEW'.
    ls_fcat-datatype = marm-brgew.
    ls_fcat-intlen = 17.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'NTGEW'.
    ls_fcat-datatype = mara-ntgew.
    ls_fcat-intlen = 17.
    APPEND ls_fcat TO pt_fcat.

    ls_fcat-fieldname = 'GROES'.
    ls_fcat-datatype = mara-groes.
    ls_fcat-intlen = 32.
    APPEND ls_fcat TO pt_fcat.

    IF  p_fiscal EQ abap_true.

      ls_fcat-fieldname = 'PRODUCTO_SAT'.
      ls_fcat-datatype = gv_string.
      ls_fcat-intlen = 30.
      APPEND ls_fcat TO pt_fcat.

      ls_fcat-fieldname = 'DESCRIPCION_SAT'.
      ls_fcat-datatype = gv_string.
      ls_fcat-intlen = 30.
      APPEND ls_fcat TO pt_fcat.

      ls_fcat-fieldname = 'UNIDAD_MEDIDA_SAT'.
      ls_fcat-datatype = gv_string.
      ls_fcat-intlen = 30.
      APPEND ls_fcat TO pt_fcat.

    ENDIF.

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

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = ob_alv
          CHANGING
            t_table      = <gt_dyntable_aux>."<gt_dyntable>. "ob_catalog->gt_alv[].
      CATCH cx_salv_msg INTO lc_msg .
    ENDTRY.

    TRY.
        DATA(lr_columnas) = ob_alv->get_columns( ).
        lr_columnas->set_exception_column( value = 'ICON' ).
      CATCH cx_salv_data_error INTO DATA(lv_error).
        "e_code = lv_error->get_text( ).
    ENDTRY.

* Columns
    ob_columns = ob_alv->get_columns( ).

    ob_columns->set_key_fixation( value = abap_true ).

    ob_column ?= ob_columns->get_column( 'MAKTL' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MAKTL' position = 1 ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Gpo Articulo' ).
    ob_column->set_medium_text( 'Gpo Articulo' ).
    ob_column->set_long_text( 'Gpo Articulo' ).
    ob_column->set_key( if_salv_c_bool_sap=>true ).

    ob_column ?= ob_columns->get_column( 'MATNR' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MATNR' position = 2 ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Material SAP' ).
    ob_column->set_medium_text( 'Material SAP' ).
    ob_column->set_long_text( 'Material SAP' ).
    ob_column->set_key( if_salv_c_bool_sap=>true ).

    ob_column ?= ob_columns->get_column( 'NORMT' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'NORMT').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Modelo' ).
    ob_column->set_medium_text( 'Modelo' ).
    ob_column->set_long_text( 'Modelo' ).

    ob_column ?= ob_columns->get_column( 'MAKTX' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MAKTX').
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Texto Breve' ).
    ob_column->set_medium_text( 'Texto Breve' ).
    ob_column->set_long_text( 'Texto Breve' ).

    ob_column ?= ob_columns->get_column( 'MEINS' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MEINS' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'UM Base' ).
    ob_column->set_medium_text( 'UM Base' ).
    ob_column->set_long_text( 'UM Base' ).

    ob_column ?= ob_columns->get_column( 'EXTWG' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'EXTWG' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Familia' ).
    ob_column->set_medium_text( 'Familia' ).
    ob_column->set_long_text( 'Familia' ).

    ob_column ?= ob_columns->get_column( 'BRAND_ID' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'BRAND_ID' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Marca' ).
    ob_column->set_medium_text( 'Marca' ).
    ob_column->set_long_text( 'Marca' ).

    ob_column ?= ob_columns->get_column( 'PRDHA2' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'PRDHA2' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Producto' ).
    ob_column->set_medium_text( 'Producto' ).
    ob_column->set_long_text( 'Producto' ).

    ob_column ?= ob_columns->get_column( 'PRDHA3' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'PRDHA3' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Genero' ).
    ob_column->set_medium_text( 'Genero' ).
    ob_column->set_long_text( 'Genero' ).

    ob_column ?= ob_columns->get_column( 'PRDHA4' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'PRDHA4' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Silueta M' ).
    ob_column->set_medium_text( 'Silueta Macro' ).
    ob_column->set_long_text( 'Silueta Macro' ).

    ob_column ?= ob_columns->get_column( 'PRDHA5' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'PRDHA5' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Silueta D' ).
    ob_column->set_medium_text( 'Silueta Detallada' ).
    ob_column->set_long_text( 'Silueta Detallada' ).

    ob_column ?= ob_columns->get_column( 'WHERL' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'WHERL' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Pais Origen' ).
    ob_column->set_medium_text( 'Pais Origen' ).
    ob_column->set_long_text( 'Pais Origen' ).

    ob_column ?= ob_columns->get_column( 'LVORM' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'LVORM' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Marcado PB' ).
    ob_column->set_medium_text( 'Marcado Para B' ).
    ob_column->set_long_text( 'Marcado Para Borrado' ).

    ob_column ?= ob_columns->get_column( 'FERTH' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'FERTH' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Codigo F' ).
    ob_column->set_medium_text( 'Codigo Fabricante' ).
    ob_column->set_long_text( 'Codigo Fabricante' ).

    ob_column ?= ob_columns->get_column( 'ZEINR' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'ZEINR' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Modelo P' ).
    ob_column->set_medium_text( 'Modelo Padre' ).
    ob_column->set_long_text( 'Modelo Padre' ).

    ob_column ?= ob_columns->get_column( 'WRKST' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'WRKST' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Material C' ).
    ob_column->set_medium_text( 'Material Composicion' ).
    ob_column->set_long_text( 'Material Composicion' ).

    ob_column ?= ob_columns->get_column( 'FIBER_CODE' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'FIBER_CODE' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Material G' ).
    ob_column->set_medium_text( 'Material Generico' ).
    ob_column->set_long_text( 'Material Generico' ).

    ob_column ?= ob_columns->get_column( 'FSH_MG_AT1' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'FSH_MG_AT1' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Division M' ).
    ob_column->set_medium_text( 'Division Marca' ).
    ob_column->set_long_text( 'Division Marca' ).

    ob_column ?= ob_columns->get_column( 'FSH_MG_AT2' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'FSH_MG_AT2' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Colaboración' ).
    ob_column->set_medium_text( 'Colaboración' ).
    ob_column->set_long_text( 'Colaboración' ).

    ob_column ?= ob_columns->get_column( 'FSH_MG_AT3' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'FSH_MG_AT3' ).
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Subdivision' ).
    ob_column->set_medium_text( 'Subdivision' ).
    ob_column->set_long_text( 'Subdivision' ).

    ob_column ?= ob_columns->get_column( 'YEAR' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'YEAR' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Año' ).
    ob_column->set_medium_text( 'Año' ).
    ob_column->set_long_text( 'Año' ).

    ob_column ?= ob_columns->get_column( 'TEMPORADA' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'TEMPORADA' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Temporada' ).
    ob_column->set_medium_text( 'Temporada' ).
    ob_column->set_long_text( 'Temporada' ).

    ob_column ?= ob_columns->get_column( 'MARKET' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'MARKET' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Market' ).
    ob_column->set_medium_text( 'Market' ).
    ob_column->set_long_text( 'Market' ).

    ob_column ?= ob_columns->get_column( 'TEMA_TEMP' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'TEMA_TEMP').
    ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Tema Temp' ).
    ob_column->set_medium_text( 'Tema Temporada' ).
    ob_column->set_long_text( 'Tema Temporada' ).

    ob_column ?= ob_columns->get_column( 'HOEHE' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'HOEHE').
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Alto' ).
    ob_column->set_medium_text( 'Alto' ).
    ob_column->set_long_text( 'Alto' ).

    ob_column ?= ob_columns->get_column( 'LAENG' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'LAENG' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Largo' ).
    ob_column->set_medium_text( 'Largo' ).
    ob_column->set_long_text( 'Largo' ).

    ob_column ?= ob_columns->get_column( 'BREIT' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'BREIT' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Ancho' ).
    ob_column->set_medium_text( 'Ancho' ).
    ob_column->set_long_text( 'Ancho' ).

    ob_column ?= ob_columns->get_column( 'BRGEW' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'BRGEW' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Peso Bruto' ).
    ob_column->set_medium_text( 'Peso Bruto' ).
    ob_column->set_long_text( 'Peso Bruto' ).

    ob_column ?= ob_columns->get_column( 'NTGEW' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'NTGEW' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_short_text( 'Peso Neto' ).
    ob_column->set_medium_text( 'Peso Neto' ).
    ob_column->set_long_text( 'Peso Neto' ).

    ob_column ?= ob_columns->get_column( 'GROES' ).
    ob_alv->get_columns( )->set_column_position( columnname = 'GROES' ).
    ob_columns->set_optimize( abap_true ).
    ob_column->set_medium_text( 'Dimensiones' ).
    ob_column->set_long_text( 'Dimensiones' ).

***************************
**** Caracteristicas del material por clase

    lt_fcat = ob_catalog->gt_fcat_aux[].

    LOOP AT lt_fcat INTO DATA(ls_fcat) FROM 31.
      lv_name = lv_col_name = lv_name_txt = ls_fcat-fieldname.
      TRANSLATE lv_name_txt TO LOWER CASE.

      ob_column ?= ob_columns->get_column( lv_col_name ).
      ob_alv->get_columns( )->set_column_position( columnname = lv_col_name ).
      ob_columns->set_optimize( abap_true ).
      ob_column->set_optimized( abap_true ).
      ob_column->set_long_text( lv_name_txt ).
    ENDLOOP.

***************************

    IF  p_fiscal EQ abap_true.

      ob_column ?= ob_columns->get_column( 'PRODUCTO_SAT' ).
      ob_alv->get_columns( )->set_column_position( columnname = 'PRODUCTO_SAT' ).
      ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Producto SAT' ).
      ob_column->set_medium_text( 'Producto SAT' ).
      ob_column->set_long_text( 'Producto SAT' ).

      ob_column ?= ob_columns->get_column( 'DESCRIPCION_SAT' ).
      ob_alv->get_columns( )->set_column_position( columnname = 'DESCRIPCION_SAT' ).
      ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Descripcion SAT' ).
      ob_column->set_medium_text( 'Descripcion SAT' ).
      ob_column->set_long_text( 'Descripcion SAT' ).

      ob_column ?= ob_columns->get_column( 'UNIDAD_MEDIDA_SAT' ).
      ob_alv->get_columns( )->set_column_position( columnname = 'UNIDAD_MEDIDA_SAT' ).
      ob_columns->set_optimize( abap_true ).
*    ob_column->set_short_text( 'Unidad Medida' ).
      ob_column->set_medium_text( 'Unidad de Medida SAT' ).
      ob_column->set_long_text( 'Unidad de Medida SAT' ).

    ENDIF.

* Calling Set PF status method
    CALL METHOD m_set_pf_status
      CHANGING
        co_alv = ob_alv.

    "Calling the top of page method
    CALL METHOD m_set_top_of_page
      CHANGING
        co_alv = ob_alv.

    CALL METHOD ob_alv->get_sorts
      RECEIVING
        value = lo_sorts.

**// 2.Specify the column for sorting
    TRY.
        CALL METHOD lo_sorts->add_sort
          EXPORTING
            columnname = 'MATNR'
*           position   =
            sequence   = if_salv_c_sort=>sort_up.
      CATCH cx_salv_not_found .
      CATCH cx_salv_existing .
      CATCH cx_salv_data_error .
    ENDTRY.

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

  ENDMETHOD.

ENDCLASS.

FORM f_sugestion_low.
  DATA: lt_return TYPE TABLE OF ddshretval.


*SELECT DISTINCT class FROM zvcaracterist INTO TABLE @DATA(lt_class)
*    WHERE klart IN ('001', '026').
  SELECT DISTINCT * FROM ztt_claves_clas INTO TABLE @DATA(lt_class).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CLASE'
      window_title    = 'Producto'
      value_org       = 'S'
    TABLES
      value_tab       = lt_class
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0.
    READ TABLE lt_return INTO DATA(ls_ret) INDEX 1.
    IF sy-subrc EQ 0.
      s_tprod-low = ls_ret-fieldval.
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_sugestion_high.
  DATA: lt_return TYPE TABLE OF ddshretval.

*  SELECT DISTINCT class FROM zvcaracterist INTO TABLE @DATA(lt_class)
*    WHERE klart IN ('001', '026').
  SELECT DISTINCT * FROM ztt_claves_clas INTO TABLE @DATA(lt_class).


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CLASE'
      window_title    = 'Producto'
      value_org       = 'S'
    TABLES
      value_tab       = lt_class
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0.
    READ TABLE lt_return INTO DATA(ls_ret) INDEX 1.
    IF sy-subrc EQ 0.
      s_tprod-high = ls_ret-fieldval.
    ENDIF.
  ENDIF.
ENDFORM.

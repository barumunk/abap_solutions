*&---------------------------------------------------------------------*
*& Include          ZFIR001_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form CHANGE_XLS
*&---------------------------------------------------------------------*
*& Carga de Documento Excel
*&---------------------------------------------------------------------*
*  -->  IM_FILE  Documento Excel de Creación BPS
*&---------------------------------------------------------------------*
FORM change_xls USING im_file TYPE rlgrap-filename .

  DATA lv_filename TYPE string.
*---------------------------------------------------
  MOVE 1 TO gv_rc.

  lv_filename = im_file.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename = lv_filename
      filetype = 'BIN'
    CHANGING
      data_tab = t_bindata
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid
    TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.        "  CHANGE_XLS

*&---------------------------------------------------------------------*
*& Form CHANGE_DOC
*&---------------------------------------------------------------------*
*& Seleccion de Documento Excel
*&---------------------------------------------------------------------*
FORM change_doc.

  MOVE 1 TO gv_rc.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title      = 'Upload XLS file'
      default_extension = '*.xlsx'
      file_filter       = '*.xlsx'
    CHANGING
      file_table        = t_file
      rc                = gv_rc
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3.

  CASE sy-subrc.
    WHEN 0.

      READ TABLE t_file INTO gs_file INDEX 1.
      IF sy-subrc EQ 0.
        p_file = gs_file-filename.
      ENDIF.

    WHEN OTHERS.
      MESSAGE ID sy-msgid
      TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDCASE.

ENDFORM.        "  CHANGE_DOC

*&---------------------------------------------------------------------*
*& Form CHANGE_DOC
*&---------------------------------------------------------------------*
*&  Carga de Estructuras para Ejecucion de BPS
*&  Pestañas:
*&  1.  Instrucciones.
*&  2.  Datos Generales.
*&  3.  Info. Fiscal.
*&  4.  Anexos.
*&  5.  Cliente
*&  6.  Dirección Entrega.
*&  7.  Direc. Entrega Empresa
*&  8.  Direc. Entrega Herramientas.
*&  9.  Direc. Entrega Instrucciones.
*&  10. Cliente Retención.
*&  11. Cliente Ventas
*&  12. Clientes Interlocutor
*&  13. Proveedor.
*&  14. Proveedor Retención.
*&  15. Proveedor Interlocutor.
*&---------------------------------------------------------------------*
*& <-- EX_FLAG Control de Ejecucion Validando Tablas Iniciadas
*&---------------------------------------------------------------------*
FORM change_estruc CHANGING ex_flag TYPE c.

  DATA flag_gnral   TYPE c.
  DATA flag_inffisc TYPE c.
  DATA flag_anexos  TYPE c.
  DATA flag_cliente TYPE c.
  DATA flag_direntg TYPE c.
  DATA flag_dirempr TYPE c.
  DATA flag_dirherr TYPE c.
  DATA flag_dirinst TYPE c.
  DATA flag_cntret  TYPE c.
  DATA flag_cntvnt  TYPE c.
  DATA flag_cntint  TYPE c.
  DATA flag_proved  TYPE c.
  DATA flag_prvrnt  TYPE c.
  DATA flag_prvint  TYPE c.
  DATA flag_prvcla  TYPE c.
*---------------------------------------------------------

* Get first workseet name
  DATA lv_lines TYPE i.
  DATA lv_ciclo TYPE i.

*   SOLIX TO XSTRING
  DATA(lv_bindata) = cl_bcs_convert=>solix_to_xstring( it_solix = t_bindata ).

* Create spreadsheet ref object
  TRY.
      DATA(o_excel) = NEW cl_fdt_xl_spreadsheet(
        document_name = CONV #( t_file[ 1 ]-filename )
        xdocument     = lv_bindata ).


    CATCH cx_sy_itab_line_not_found INTO DATA(e_text).
      MESSAGE e_text->get_text( )
      TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.

  ENDTRY.

  o_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = lt_worknames ).

  IF lines( lt_worknames ) > 0.

    " --> Consulta de Pestañas
    CLEAR lv_lines.
    DESCRIBE TABLE lt_worknames LINES lv_lines.

    " --> Control de Ejecucion de Pestañas
    PERFORM f_control_flag
   CHANGING flag_gnral   flag_inffisc flag_anexos
            flag_cliente flag_direntg flag_dirempr
            flag_dirherr flag_dirinst flag_cntret
            flag_cntvnt  flag_cntint  flag_proved
            flag_prvrnt  flag_prvint  flag_prvcla.

    " Ciclo de Pestañas EXCEL
    " Carga de Tablas Internas
    CLEAR lv_ciclo.
    DO lv_lines TIMES.

      ADD 1 TO lv_ciclo.

      CASE lv_ciclo.
        WHEN 1.
          " Definir Instrucciones
        WHEN 2 ." Pestaña Datos Generales

          CHECK flag_gnral EQ abap_true.

          DATA(o_workitab) =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                   lt_worknames[ 2 ] ).

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstgnral) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_gnrales. " <- Carga Pestaña
          ENDIF.

        WHEN 3." Información Fiscal

          CHECK flag_inffisc EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                   lt_worknames[ 3 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstfiscal) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_fiscal." <- Carga Pestaña
          ENDIF.

        WHEN 4." Anexos

          CHECK flag_anexos EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 4 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstanexo) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_anexo." <- Carga Pestaña
          ENDIF.

        WHEN 5." Cliente

          CHECK flag_cliente EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 5 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstcliente) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_cliente." <- Carga Pestaña
          ENDIF.

        WHEN 6." Direccion Entrega

          CHECK flag_direntg EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 6 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstentrega) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_entrega." <- Carga Pestaña
          ENDIF.

        WHEN 7." Direccion Entrega - Empresa

          CHECK flag_dirempr EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 7 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstempresa) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_empresa." <- Carga Pestaña
          ENDIF.

        WHEN 8." Direccion Entrega - Herramienta

          CHECK flag_dirherr EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 8 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstherram) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_herramnta." <- Carga Pestaña
          ENDIF.

        WHEN 9." Direccion Entrega - Instruccion

          CHECK flag_dirinst EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 9 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstinstruc) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_instruc." <- Carga Pestaña
          ENDIF.

        WHEN 10." Cliente - Retencion

          CHECK flag_cntret  EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 10 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstcntret) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_retencion." <- Carga Pestaña
          ENDIF.

        WHEN 11." Cliente - Ventas

          CHECK flag_cntvnt EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 11 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstcntevta) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_cnteventa." <- Carga Pestaña
          ENDIF.

        WHEN 12." Cliente - Interlocutor

          CHECK flag_cntint EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 12 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstcnteint) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_cnteinter." <- Carga Pestaña
          ENDIF.

        WHEN 13." Proveedor

          CHECK flag_proved EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 13 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstprov) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_provedor." <- Carga Pestaña
          ENDIF.

        WHEN 14." Proveedor - Retencion

          CHECK flag_prvrnt EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 14 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstprovrnt) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_provreten." <- Carga Pestaña
          ENDIF.

        WHEN 15." Proveedor - Interlocutor

          CHECK flag_prvint EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 15 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstprovint) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_provinter." <- Carga Pestaña

          ENDIF.

        WHEN 16." Proveedor - Clasificacion "t_provherram

          CHECK flag_prvcla EQ abap_true.

          o_workitab =
          o_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                  lt_worknames[ 16 ] ).

          IF  <fs_struc> IS ASSIGNED
          AND <ft_xls>   IS ASSIGNED.
            CLEAR <fs_struc>.
            REFRESH <ft_xls>.
          ENDIF.

          ASSIGN o_workitab->* TO <ft_xls>.
          ASSIGN (gs_pstprovcla) TO <fs_struc>.

          IF <ft_xls> IS ASSIGNED.
            DELETE ADJACENT DUPLICATES FROM <ft_xls>.
            PERFORM carga_provclasif." <- Carga Pestaña

          ENDIF.

      ENDCASE.

    ENDDO.

  ENDIF.

* ==========> Carga de Reportes ALV

  "---> Datos Generales: Generales / Info. Fiscal
  IF t_pstgnral  IS NOT INITIAL
  OR t_pstfiscal IS NOT INITIAL.

    PERFORM carga_alvgnrales.
    ex_flag = abap_true.

  ENDIF.

  "---> Anexo
  IF t_pstanexo IS NOT INITIAL.

    PERFORM carga_alvanexos.
    ex_flag = abap_true.

  ENDIF.

  "---> Cliente Financiero: Cliente / Cliente Retencion
  IF t_pstcliente IS NOT INITIAL
  OR t_pstcntret  IS NOT INITIAL.

    PERFORM carga_alvgclntefin.
    ex_flag = abap_true.

  ENDIF.

  "---> Cliente Ventas: Cliente Ventas / Cliente Interlocutor
  IF t_pstcntevta IS NOT INITIAL
  OR t_pstcnteint IS NOT INITIAL.

    PERFORM carga_alvclntevnta.
    ex_flag = abap_true.

  ENDIF.

  "---> Direccion Entrega:
  "     Entrega / Empresa / Herramientas / Instruccion
  IF t_pstentrega IS NOT INITIAL
  OR t_pstempresa IS NOT INITIAL
  OR t_pstherram  IS NOT INITIAL
  OR t_pstinstruc IS NOT INITIAL.

    PERFORM carga_alvdirentga.
    ex_flag = abap_true.

  ENDIF.

  "---> Proveedor: Proveedor y Proveedor Retencion
  IF t_pstprov    IS NOT INITIAL
  OR t_pstprovrnt IS NOT INITIAL.

    PERFORM carga_alvproveedor.
    ex_flag = abap_true.

  ENDIF.

  "---> Proveedor Compras: Proveedor Y Proveedor Interlocutor
  IF t_pstprov    IS NOT INITIAL
  OR t_pstprovint IS NOT INITIAL.

    PERFORM carga_alvprvcompras.
    ex_flag = abap_true.

  ENDIF.

ENDFORM.        " CHANGE_ESTRUC
*&---------------------------------------------------------------------*
*& Form CARGA_GNRALES
*&---------------------------------------------------------------------*
*& Carga Pestaña de Generales
*&---------------------------------------------------------------------*
FORM carga_gnrales.

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-typereg = <fs_value>.
    CONDENSE gs_pstgnral-typereg.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-code    = <fs_value>.
    CONDENSE gs_pstgnral-code.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-razon   = <fs_value>.
    CONDENSE gs_pstgnral-razon.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-tipo    = <fs_value>.
    CONDENSE gs_pstgnral-tipo.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-grp_bp  = <fs_value>.
    CONDENSE gs_pstgnral-grp_bp.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-aped1   = <fs_value>.
    CONDENSE gs_pstgnral-aped1.                                "grp_bp.

    ASSIGN COMPONENT 'G' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-aped2   = <fs_value>.
    CONDENSE gs_pstgnral-aped2.                                "aped1.

    ASSIGN COMPONENT 'H' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-name1   = <fs_value>.
    CONDENSE gs_pstgnral-name1.                                "aped2.

    ASSIGN COMPONENT 'I' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-name2   = <fs_value>.
    CONDENSE gs_pstgnral-name2.                                "name1.

    ASSIGN COMPONENT 'J' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-namect  = <fs_value>.
    CONDENSE gs_pstgnral-namect.

    ASSIGN COMPONENT 'K' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-calle   = <fs_value>.
    CONDENSE gs_pstgnral-calle.

    ASSIGN COMPONENT 'L' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-numext  = <fs_value>.
    CONDENSE gs_pstgnral-numext.

    ASSIGN COMPONENT 'M' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-numeint = <fs_value>.
    CONDENSE gs_pstgnral-numeint.

    ASSIGN COMPONENT 'N' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-colonia = <fs_value>.
    CONDENSE gs_pstgnral-colonia.

    ASSIGN COMPONENT 'O' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-local   = <fs_value>.
    CONDENSE gs_pstgnral-local.

    ASSIGN COMPONENT 'P' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-munic   = <fs_value>.
    CONDENSE gs_pstgnral-munic.

    ASSIGN COMPONENT 'Q' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-refer   = <fs_value>.
    CONDENSE gs_pstgnral-refer.

    ASSIGN COMPONENT 'R' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-estado  = <fs_value>.
    CONDENSE gs_pstgnral-estado.

    ASSIGN COMPONENT 'S' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-pais    = <fs_value>.
    CONDENSE gs_pstgnral-pais.

    ASSIGN COMPONENT 'T' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-c_p_    = <fs_value>.
    CONDENSE gs_pstgnral-c_p_.

    ASSIGN COMPONENT 'U' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-idioma  = <fs_value>.
    CONDENSE gs_pstgnral-idioma.

    ASSIGN COMPONENT 'V' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-telef   = <fs_value>.
    CONDENSE gs_pstgnral-telef.

    ASSIGN COMPONENT 'W' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-email1  = <fs_value>.
    CONDENSE gs_pstgnral-email1.

    ASSIGN COMPONENT 'X' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-email2  = <fs_value>.
    CONDENSE gs_pstgnral-email2.

    ASSIGN COMPONENT 'Y' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-email3  = <fs_value>.
    CONDENSE gs_pstgnral-email3.

    ASSIGN COMPONENT 'Z' OF STRUCTURE <line> TO <fs_value>.
    gs_pstgnral-email4  = <fs_value>.
    CONDENSE gs_pstgnral-email4.

    IF  gs_pstgnral-typereg IS INITIAL
    AND gs_pstgnral-code    IS INITIAL
    AND gs_pstgnral-razon   IS INITIAL
    AND gs_pstgnral-tipo    IS INITIAL
    AND gs_pstgnral-aped1   IS INITIAL
    AND gs_pstgnral-aped2   IS INITIAL
    AND gs_pstgnral-name1   IS INITIAL
    AND gs_pstgnral-name2   IS INITIAL
    AND gs_pstgnral-namect  IS INITIAL
    AND gs_pstgnral-calle   IS INITIAL
    AND gs_pstgnral-numext  IS INITIAL
    AND gs_pstgnral-numeint IS INITIAL
    AND gs_pstgnral-colonia IS INITIAL
    AND gs_pstgnral-local   IS INITIAL
    AND gs_pstgnral-munic   IS INITIAL
    AND gs_pstgnral-refer   IS INITIAL
    AND gs_pstgnral-estado  IS INITIAL
    AND gs_pstgnral-pais    IS INITIAL
    AND gs_pstgnral-c_p_    IS INITIAL
    AND gs_pstgnral-idioma  IS INITIAL
    AND gs_pstgnral-telef   IS INITIAL
    AND gs_pstgnral-email1  IS INITIAL
    AND gs_pstgnral-email2  IS INITIAL
    AND gs_pstgnral-email3  IS INITIAL
    AND gs_pstgnral-email4  IS INITIAL.

      CLEAR gs_pstgnral.

    ELSE.

      APPEND gs_pstgnral TO t_pstgnral.
      CLEAR gs_pstgnral.

    ENDIF.

  ENDLOOP.

  IF t_pstgnral[] IS NOT INITIAL.
    SORT t_pstgnral BY code.
  ENDIF.

ENDFORM.    " CARGA_GNRALES
*&---------------------------------------------------------------------*
*& Form CARGA_PROVINTER
*&---------------------------------------------------------------------*
*& Carga Pestaña Proveedor Interlocutor
*&---------------------------------------------------------------------*
FORM carga_provinter.

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovint-empresa = <fs_value>.
    CONDENSE gs_pstprovint-empresa.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovint-centro = <fs_value>.
    CONDENSE gs_pstprovint-centro.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovint-code_minor = <fs_value>.
    CONDENSE gs_pstprovint-code_minor.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovint-tipo_inter = <fs_value>.
    CONDENSE gs_pstprovint-tipo_inter.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovint-interlcutr = <fs_value>.
    CONDENSE gs_pstprovint-interlcutr.

    IF  gs_pstprovint-empresa    IS INITIAL
    AND gs_pstprovint-centro     IS INITIAL
    AND gs_pstprovint-code_minor IS INITIAL
    AND gs_pstprovint-tipo_inter IS INITIAL
    AND gs_pstprovint-interlcutr IS INITIAL.

      CONDENSE gs_pstprovint.

    ELSE.

      APPEND gs_pstprovint TO t_pstprovint.
      CLEAR gs_pstprovint.

    ENDIF.

  ENDLOOP.

  IF t_pstprovint[] IS NOT INITIAL.
    SORT t_pstprovint BY code_minor.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_PROVCLASIF
*&---------------------------------------------------------------------*
*& Carga Pestaña Proveedor Clasificacion
*&---------------------------------------------------------------------*
FORM carga_provclasif.


  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovcla-code = <fs_value>.
    CONDENSE gs_pstprovcla-code.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovcla-tipo_prov = <fs_value>.
    CONDENSE gs_pstprovcla-tipo_prov.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovcla-region = <fs_value>.
    CONDENSE gs_pstprovcla-region.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovcla-esp_verd = <fs_value>.
    CONDENSE gs_pstprovcla-esp_verd.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovcla-index = <fs_value>.
    CONDENSE gs_pstprovcla-index.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovcla-clave_imp = <fs_value>.
    CONDENSE gs_pstprovcla-clave_imp.

    ASSIGN COMPONENT 'G' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovcla-apl_desc = <fs_value>.
    CONDENSE gs_pstprovcla-apl_desc.

    ASSIGN COMPONENT 'H' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovcla-promotor = <fs_value>.
    CONDENSE gs_pstprovcla-promotor.

    IF  gs_pstprovcla-code       IS INITIAL
    AND gs_pstprovcla-tipo_prov  IS INITIAL
    AND gs_pstprovcla-region     IS INITIAL
    AND gs_pstprovcla-esp_verd   IS INITIAL
    AND gs_pstprovcla-index      IS INITIAL
    AND gs_pstprovcla-clave_imp  IS INITIAL
    AND gs_pstprovcla-apl_desc   IS INITIAL
    AND gs_pstprovcla-promotor   IS INITIAL.

      CONDENSE gs_pstprovcla.

    ELSE.

      APPEND gs_pstprovcla TO t_pstprovcla.
      CLEAR gs_pstprovcla.

    ENDIF.

  ENDLOOP.

  IF t_pstprovcla[] IS NOT INITIAL.
    SORT t_pstprovcla BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_PROVRETEN
*&---------------------------------------------------------------------*
*& Carga Pestaña Proveedor Retencion
*&---------------------------------------------------------------------*
FORM carga_provreten.

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovrnt-empresa = <fs_value>.
    CONDENSE gs_pstprovrnt-empresa.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovrnt-code = <fs_value>.
    CONDENSE gs_pstprovrnt-code.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstprovrnt-impuesto = <fs_value>.
    CONDENSE gs_pstprovrnt-impuesto.

    APPEND gs_pstprovrnt TO t_pstprovrnt.
    CLEAR gs_pstprovrnt.

  ENDLOOP.

  IF t_pstprovrnt[] IS NOT INITIAL.
    SORT t_pstprovrnt BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_PROVEDOR
*&---------------------------------------------------------------------*
*& Carga Pestaña Proveedor
*&---------------------------------------------------------------------*
FORM carga_provedor .

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-empresa = <fs_value>.
    CONDENSE gs_provedor-empresa.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-code = <fs_value>.
    CONDENSE gs_provedor-code.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-gpoteso = <fs_value>.
    CONDENSE gs_provedor-gpoteso.

*    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
*    gs_provedor-gpoprvsap = <fs_value>.
*    CONDENSE gs_provedor-gpoprvsap.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-cntasaldo = <fs_value>.
    CONDENSE gs_provedor-cntasaldo.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-condpago = <fs_value>.
    CONDENSE gs_provedor-condpago.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-paisbank = <fs_value>.
    CONDENSE gs_provedor-paisbank.

    ASSIGN COMPONENT 'G' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-banco = <fs_value>.
    CONDENSE gs_provedor-banco.

    ASSIGN COMPONENT 'H' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-suc_bank  = <fs_value>.
    CONDENSE gs_provedor-suc_bank.

    ASSIGN COMPONENT 'I' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-cnta_bank = <fs_value>.
    CONDENSE gs_provedor-cnta_bank.

    ASSIGN COMPONENT 'J' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-incoterm = <fs_value>.
    CONDENSE gs_provedor-incoterm.

    ASSIGN COMPONENT 'K' OF STRUCTURE <line> TO <fs_value>.
    gs_provedor-mon_pedid = <fs_value>.
    CONDENSE gs_provedor-mon_pedid.

    IF  gs_provedor-empresa   IS INITIAL
    AND gs_provedor-code      IS INITIAL
    AND gs_provedor-gpoteso   IS INITIAL
*    AND gs_provedor-gpoprvsap IS INITIAL
    AND gs_provedor-cntasaldo IS INITIAL
    AND gs_provedor-condpago  IS INITIAL
    AND gs_provedor-paisbank  IS INITIAL
    AND gs_provedor-banco     IS INITIAL
    AND gs_provedor-suc_bank  IS INITIAL
    AND gs_provedor-cnta_bank IS INITIAL
    AND gs_provedor-incoterm  IS INITIAL
    AND gs_provedor-mon_pedid IS INITIAL.

      CLEAR gs_provedor.

    ELSE.

      APPEND gs_provedor TO t_provedor.
      CLEAR gs_provedor.

    ENDIF.

  ENDLOOP.

  IF t_provedor[] IS NOT INITIAL.
    SORT t_provedor BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_CNTEINTER
*&---------------------------------------------------------------------*
*& Carga Pestaña Cuenta Interlocutor
*&---------------------------------------------------------------------*
FORM carga_cnteinter .

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcnteint-code = <fs_value>.
    CONDENSE gs_pstcnteint-code.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcnteint-orgvnta = <fs_value>.
    CONDENSE gs_pstcnteint-orgvnta.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcnteint-canal = <fs_value>.
    CONDENSE gs_pstcnteint-canal.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcnteint-sector = <fs_value>.
    CONDENSE gs_pstcnteint-sector.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcnteint-tipointer = <fs_value>.
    CONDENSE gs_pstcnteint-tipointer.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcnteint-interloc = <fs_value>.
    CONDENSE gs_pstcnteint-interloc.

    IF  gs_pstcnteint-code      IS INITIAL
    AND gs_pstcnteint-orgvnta   IS INITIAL
    AND gs_pstcnteint-canal     IS INITIAL
    AND gs_pstcnteint-sector    IS INITIAL
    AND gs_pstcnteint-tipointer IS INITIAL
    AND gs_pstcnteint-interloc  IS INITIAL.

      CLEAR gs_pstcnteint.

    ELSE.

      APPEND gs_pstcnteint TO t_pstcnteint. "t_pstprovrnt.
      CLEAR gs_pstcnteint. "gs_pstprovrnt.

    ENDIF.

  ENDLOOP.

  IF t_pstcnteint[] IS NOT INITIAL.
    SORT t_pstcnteint BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_CNTEVENTA
*&---------------------------------------------------------------------*
*& Carga Pestaña Cliente Venta
*&---------------------------------------------------------------------*
FORM carga_cnteventa.

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-code = <fs_value>.
    CONDENSE gs_pstcntevta-code.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-orgvnta = <fs_value>.
    CONDENSE gs_pstcntevta-orgvnta.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-canal = <fs_value>.
    CONDENSE gs_pstcntevta-canal.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-sector = <fs_value>.
    CONDENSE gs_pstcntevta-sector.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-grpclte = <fs_value>.
    CONDENSE gs_pstcntevta-grpclte.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-zonavnta = <fs_value>.
    CONDENSE gs_pstcntevta-zonavnta.

    ASSIGN COMPONENT 'G' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-ofcventa = <fs_value>.
    CONDENSE gs_pstcntevta-ofcventa.

    ASSIGN COMPONENT 'H' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-grpovnta = <fs_value>.
    CONDENSE gs_pstcntevta-grpovnta.

    ASSIGN COMPONENT 'I' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-clas_abc = <fs_value>.
    CONDENSE gs_pstcntevta-clas_abc.

    ASSIGN COMPONENT 'J' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-mone_ped = <fs_value>.
    CONDENSE gs_pstcntevta-mone_ped.

    ASSIGN COMPONENT 'K' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-condentg = <fs_value>.
    CONDENSE gs_pstcntevta-condentg.

    ASSIGN COMPONENT 'L' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-cedis = <fs_value>.
    CONDENSE gs_pstcntevta-cedis.

    ASSIGN COMPONENT 'M' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-tol_exce = <fs_value>.
    CONDENSE gs_pstcntevta-tol_exce.

    ASSIGN COMPONENT 'N' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-incoterm = <fs_value>.
    CONDENSE gs_pstcntevta-incoterm.

    ASSIGN COMPONENT 'O' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-locincot = <fs_value>.
    CONDENSE gs_pstcntevta-locincot.

*** INI DNAVOA 06.13.2025
    ASSIGN COMPONENT 'P' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-tip_cots = <fs_value>.
    CONDENSE gs_pstcntevta-tip_cots.

    ASSIGN COMPONENT 'Q' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-gp_prec = <fs_value>.
    CONDENSE gs_pstcntevta-gp_prec.

    ASSIGN COMPONENT 'R' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-esq_clte = <fs_value>.
    CONDENSE gs_pstcntevta-esq_clte.

    ASSIGN COMPONENT 'S' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-cond_exp = <fs_value>.
    CONDENSE gs_pstcntevta-cond_exp.

    ASSIGN COMPONENT 'T' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-gpo_impt = <fs_value>.
    CONDENSE gs_pstcntevta-gpo_impt.

    ASSIGN COMPONENT 'U' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-clas_fis = <fs_value>.
    CONDENSE gs_pstcntevta-clas_fis.

    ASSIGN COMPONENT 'V' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntevta-gpo_cli3 = <fs_value>.
    CONDENSE gs_pstcntevta-gpo_cli3.

*** FIN DNAVOA 06.13.2025

    IF  gs_pstcntevta-code     IS INITIAL
    AND gs_pstcntevta-orgvnta  IS INITIAL
    AND gs_pstcntevta-canal    IS INITIAL
    AND gs_pstcntevta-sector   IS INITIAL
    AND gs_pstcntevta-grpclte  IS INITIAL
    AND gs_pstcntevta-zonavnta IS INITIAL
    AND gs_pstcntevta-ofcventa IS INITIAL
    AND gs_pstcntevta-grpovnta IS INITIAL
    AND gs_pstcntevta-clas_abc IS INITIAL
    AND gs_pstcntevta-mone_ped IS INITIAL
    AND gs_pstcntevta-condentg IS INITIAL
    AND gs_pstcntevta-cedis    IS INITIAL
    AND gs_pstcntevta-tol_exce IS INITIAL
    AND gs_pstcntevta-incoterm IS INITIAL
    AND gs_pstcntevta-locincot IS INITIAL.

      CLEAR gs_pstcntevta.

    ELSE.

      APPEND gs_pstcntevta TO t_pstcntevta.
      CLEAR gs_pstcntevta.

    ENDIF.

  ENDLOOP.

  IF t_pstcntevta[] IS NOT INITIAL.
    SORT t_pstcntevta BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_RETENCION
*&---------------------------------------------------------------------*
*& Carga Pestaña Cliente Retencion
*&---------------------------------------------------------------------*
FORM carga_retencion .

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntret-empresa  = <fs_value>.
    CONDENSE gs_pstcntret-empresa.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntret-code = <fs_value>.
    CONDENSE gs_pstcntret-code.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcntret-impuesto = <fs_value>.
    CONDENSE gs_pstcntret-impuesto.

    IF  gs_pstcntret-code     IS INITIAL
    AND gs_pstcntret-empresa  IS INITIAL
    AND gs_pstcntret-impuesto IS INITIAL.
      CLEAR gs_pstcntret.
    ELSE.
      APPEND gs_pstcntret TO t_pstcntret.
      CLEAR gs_pstcntret.
    ENDIF.

  ENDLOOP.

  IF t_pstcntret[] IS NOT INITIAL.
    SORT t_pstcntret BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_INSTRUC
*&---------------------------------------------------------------------*
*& Carga Pestaña Direcccion Entrega - Instruccion
*&---------------------------------------------------------------------*
FORM carga_instruc .

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstinstruc-code = <fs_value>.
    CONDENSE gs_pstinstruc-code.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstinstruc-orgventa = <fs_value>.
    CONDENSE gs_pstinstruc-orgventa.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstinstruc-canaldbt = <fs_value>.
    CONDENSE gs_pstinstruc-canaldbt.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstinstruc-sector = <fs_value>.
    CONDENSE gs_pstinstruc-sector.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstinstruc-tipo = <fs_value>.
    CONDENSE gs_pstinstruc-tipo.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_pstinstruc-codedirc = <fs_value>.
    CONDENSE gs_pstinstruc-codedirc.

    ASSIGN COMPONENT 'G' OF STRUCTURE <line> TO <fs_value>.
    gs_pstinstruc-instrucc = <fs_value>.
    CONDENSE gs_pstinstruc-instrucc.

    IF  gs_pstinstruc-code     IS INITIAL
    AND gs_pstinstruc-orgventa IS INITIAL
    AND gs_pstinstruc-canaldbt IS INITIAL
    AND gs_pstinstruc-sector   IS INITIAL
    AND gs_pstinstruc-tipo     IS INITIAL
    AND gs_pstinstruc-codedirc IS INITIAL
    AND gs_pstinstruc-instrucc IS INITIAL.

      CLEAR gs_pstinstruc.

    ELSE.

      APPEND gs_pstinstruc TO t_pstinstruc.
      CLEAR gs_pstinstruc.

    ENDIF.

  ENDLOOP.

  IF t_pstinstruc[] IS NOT INITIAL.
    SORT t_pstinstruc BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_HERRAMNTA
*&---------------------------------------------------------------------*
*& Carga Pestaña Carga Pestaña Direcccion Entrega - Herramienta
*&---------------------------------------------------------------------*
FORM carga_herramnta .

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstherram-code = <fs_value>.
    CONDENSE gs_pstherram-code.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstherram-orgventa = <fs_value>.
    CONDENSE gs_pstherram-orgventa.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstherram-canaldbt = <fs_value>.
    CONDENSE gs_pstherram-canaldbt.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstherram-sector = <fs_value>.
    CONDENSE gs_pstherram-sector.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstherram-tipo = <fs_value>.
    CONDENSE gs_pstherram-tipo.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_pstherram-namecrto = <fs_value>.
    CONDENSE gs_pstherram-namecrto.

    ASSIGN COMPONENT 'G' OF STRUCTURE <line> TO <fs_value>.
    gs_pstherram-herrmnta = <fs_value>.
    CONDENSE gs_pstherram-herrmnta.

    IF  gs_pstherram-code     IS INITIAL
    AND gs_pstherram-orgventa IS INITIAL
    AND gs_pstherram-canaldbt IS INITIAL
    AND gs_pstherram-sector   IS INITIAL
    AND gs_pstherram-tipo     IS INITIAL
    AND gs_pstherram-namecrto IS INITIAL
    AND gs_pstherram-herrmnta IS INITIAL.

      CLEAR gs_pstherram.

    ELSE.

      APPEND gs_pstherram TO t_pstherram.
      CLEAR gs_pstherram.

    ENDIF.

  ENDLOOP.

  IF t_pstherram[] IS NOT INITIAL.
    SORT t_pstherram BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_EMPRESA
*&---------------------------------------------------------------------*
*& Carga Pestaña Direcccion Entrega - Empresa
*&---------------------------------------------------------------------*
FORM carga_empresa .

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-code = <fs_value>.
    CONDENSE gs_pstempresa-code.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-orgventa = <fs_value>.
    CONDENSE gs_pstempresa-orgventa.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-canaldbt = <fs_value>.
    CONDENSE gs_pstempresa-canaldbt.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-sector = <fs_value>.
    CONDENSE gs_pstempresa-sector.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-namecrto = <fs_value>.
    CONDENSE gs_pstempresa-namecrto.

    TRANSLATE gs_pstempresa-namecrto TO UPPER CASE.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-cedisaten = <fs_value>.
    CONDENSE gs_pstempresa-cedisaten.

    ASSIGN COMPONENT 'G' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-respons = <fs_value>.
    CONDENSE gs_pstempresa-respons.

    ASSIGN COMPONENT 'H' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-diarecp = <fs_value>.
    CONDENSE gs_pstempresa-diarecp.

    ASSIGN COMPONENT 'I' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-horarecp = <fs_value>.
    CONDENSE gs_pstempresa-horarecp.

    ASSIGN COMPONENT 'J' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-tipocarga = <fs_value>.
    CONDENSE gs_pstempresa-tipocarga.

    ASSIGN COMPONENT 'K' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-tipomano = <fs_value>.
    CONDENSE gs_pstempresa-tipomano.

    ASSIGN COMPONENT 'L' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-zona_exp = <fs_value>.
    CONDENSE gs_pstempresa-zona_exp.

    ASSIGN COMPONENT 'M' OF STRUCTURE <line> TO <fs_value>.
    gs_pstempresa-cond_exp = <fs_value>.
    CONDENSE gs_pstempresa-cond_exp.

    IF  gs_pstempresa-code      IS INITIAL
    AND gs_pstempresa-orgventa  IS INITIAL
    AND gs_pstempresa-canaldbt  IS INITIAL
    AND gs_pstempresa-sector    IS INITIAL
    AND gs_pstempresa-namecrto  IS INITIAL
    AND gs_pstempresa-cedisaten IS INITIAL
    AND gs_pstempresa-respons   IS INITIAL
    AND gs_pstempresa-diarecp   IS INITIAL
    AND gs_pstempresa-horarecp  IS INITIAL
    AND gs_pstempresa-tipocarga IS INITIAL
    AND gs_pstempresa-tipomano  IS INITIAL.

      CLEAR gs_pstempresa.

    ELSE.

      APPEND gs_pstempresa TO t_pstempresa.
      CLEAR gs_pstempresa.

    ENDIF.

  ENDLOOP.

  IF t_pstempresa[] IS NOT INITIAL.
    SORT t_pstempresa BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_ENTREGA
*&---------------------------------------------------------------------*
*& Carga Pestaña Direcccion Entrega
*&---------------------------------------------------------------------*
FORM carga_entrega .

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-code = <fs_value>.
    CONDENSE gs_pstentrega-code.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-namect = <fs_value>.
    CONDENSE gs_pstentrega-namect.

    TRANSLATE gs_pstentrega-namect TO UPPER CASE.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-descdir = <fs_value>.
    CONDENSE gs_pstentrega-descdir.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-calle = <fs_value>.
    CONDENSE gs_pstentrega-calle.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-numext = <fs_value>.
    CONDENSE gs_pstentrega-numext.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-numint = <fs_value>.
    CONDENSE gs_pstentrega-numint.

    ASSIGN COMPONENT 'G' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-colonia = <fs_value>.
    CONDENSE gs_pstentrega-colonia.

    ASSIGN COMPONENT 'H' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-localid = <fs_value>.
    CONDENSE gs_pstentrega-localid.

    ASSIGN COMPONENT 'I' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-municpo = <fs_value>.
    CONDENSE gs_pstentrega-municpo.

    ASSIGN COMPONENT 'J' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-refercn = <fs_value>.
    CONDENSE gs_pstentrega-refercn.

    ASSIGN COMPONENT 'K' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-estado = <fs_value>.
    CONDENSE gs_pstentrega-estado.

    ASSIGN COMPONENT 'L' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-pais = <fs_value>.
    CONDENSE gs_pstentrega-pais.

    ASSIGN COMPONENT 'M' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-codepos = <fs_value>.
    CONDENSE gs_pstentrega-codepos.

    ASSIGN COMPONENT 'N' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-idioma = <fs_value>.
    CONDENSE gs_pstentrega-idioma.

    ASSIGN COMPONENT 'O' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-telefon = <fs_value>.
    CONDENSE gs_pstentrega-telefon.

    ASSIGN COMPONENT 'P' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-email1 = <fs_value>.
    CONDENSE gs_pstentrega-email1.

    ASSIGN COMPONENT 'Q' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-email2 = <fs_value>.
    CONDENSE gs_pstentrega-email2.

    ASSIGN COMPONENT 'R' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-email3 = <fs_value>.
    CONDENSE gs_pstentrega-email3.

    ASSIGN COMPONENT 'S' OF STRUCTURE <line> TO <fs_value>.
    gs_pstentrega-email4 = <fs_value>.
    CONDENSE gs_pstentrega-email4.

    IF  gs_pstentrega-code    IS INITIAL
    AND gs_pstentrega-namect  IS INITIAL
    AND gs_pstentrega-descdir IS INITIAL
    AND gs_pstentrega-calle   IS INITIAL
    AND gs_pstentrega-numext  IS INITIAL
    AND gs_pstentrega-numint  IS INITIAL
    AND gs_pstentrega-colonia IS INITIAL
    AND gs_pstentrega-localid IS INITIAL
    AND gs_pstentrega-municpo IS INITIAL
    AND gs_pstentrega-refercn IS INITIAL
    AND gs_pstentrega-estado  IS INITIAL
    AND gs_pstentrega-pais    IS INITIAL
    AND gs_pstentrega-idioma  IS INITIAL
    AND gs_pstentrega-codepos IS INITIAL
    AND gs_pstentrega-telefon IS INITIAL
    AND gs_pstentrega-email2  IS INITIAL
    AND gs_pstentrega-email1  IS INITIAL
    AND gs_pstentrega-email4  IS INITIAL
    AND gs_pstentrega-email3  IS INITIAL.

      CLEAR gs_pstentrega.

    ELSE.

      APPEND gs_pstentrega TO t_pstentrega.
      CLEAR gs_pstentrega.

    ENDIF.

  ENDLOOP.

  IF t_pstentrega[] IS NOT INITIAL.
    SORT t_pstentrega BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_CLIENTE
*&---------------------------------------------------------------------*
*& Carga Pestaña Cliente
*&---------------------------------------------------------------------*
FORM carga_cliente .

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-id_empre = <fs_value>.
    CONDENSE gs_pstcliente-id_empre.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-code = <fs_value>.
    CONDENSE gs_pstcliente-code.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-grptesor = <fs_value>.
    CONDENSE gs_pstcliente-grptesor.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-cntasap = <fs_value>.
    CONDENSE gs_pstcliente-cntasap.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-condpago = <fs_value>.
    CONDENSE gs_pstcliente-condpago.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-typecred = <fs_value>.
    CONDENSE gs_pstcliente-typecred.

    ASSIGN COMPONENT 'G' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-moncred = <fs_value>.
    CONDENSE gs_pstcliente-moncred.

    ASSIGN COMPONENT 'H' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-limitcred = <fs_value>.
    CONDENSE gs_pstcliente-limitcred.

    ASSIGN COMPONENT 'I' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-perfcred = <fs_value>.
    CONDENSE gs_pstcliente-perfcred.

    ASSIGN COMPONENT 'J' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-tipogrant = <fs_value>.
    CONDENSE gs_pstcliente-tipogrant.
    ASSIGN COMPONENT 'K' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-monegrant = <fs_value>.
    CONDENSE gs_pstcliente-monegrant.

    ASSIGN COMPONENT 'L' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-valorgrnt = <fs_value>.
    CONDENSE gs_pstcliente-valorgrnt.

    ASSIGN COMPONENT 'M' OF STRUCTURE <line> TO <fs_value>.
    gs_pstcliente-vencgrant = <fs_value>.
    CONDENSE gs_pstcliente-vencgrant.

    IF  gs_pstcliente-id_empre  IS INITIAL
    AND gs_pstcliente-code      IS INITIAL
    AND gs_pstcliente-grptesor  IS INITIAL
    AND gs_pstcliente-cntasap   IS INITIAL
    AND gs_pstcliente-condpago  IS INITIAL
    AND gs_pstcliente-typecred  IS INITIAL
    AND gs_pstcliente-moncred   IS INITIAL
    AND gs_pstcliente-limitcred IS INITIAL
    AND gs_pstcliente-perfcred  IS INITIAL
    AND gs_pstcliente-monegrant IS INITIAL
    AND gs_pstcliente-tipogrant IS INITIAL
    AND gs_pstcliente-valorgrnt IS INITIAL
    AND gs_pstcliente-vencgrant IS INITIAL.

      CLEAR gs_pstcliente.

    ELSE.

      APPEND gs_pstcliente TO t_pstcliente.
      CLEAR gs_pstcliente.

    ENDIF.

  ENDLOOP.

  IF t_pstcliente[] IS NOT INITIAL.
    SORT t_pstcliente BY code.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_anexo
*&---------------------------------------------------------------------*
*& Carga Pestaña Generalaes Anexos
*&---------------------------------------------------------------------*
FORM carga_anexo .

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstanexo-code_anexo = <fs_value>.
    CONDENSE gs_pstanexo-code_anexo.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstanexo-typearch = <fs_value>.
    CONDENSE gs_pstanexo-typearch.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstanexo-ruta = <fs_value>.
    CONDENSE gs_pstanexo-ruta.

    IF  gs_pstanexo-code_anexo IS INITIAL
    AND gs_pstanexo-typearch   IS INITIAL
    AND gs_pstanexo-ruta       IS INITIAL.

      CLEAR gs_pstanexo.

    ELSE.

      APPEND gs_pstanexo TO t_pstanexo.
      CLEAR gs_pstanexo.

    ENDIF.

  ENDLOOP.

  IF t_pstanexo[] IS NOT INITIAL.
    SORT t_pstanexo BY code_anexo.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CARGA_FISCAL
*&---------------------------------------------------------------------*
*& Carga Pestaña Generales Informacion Fiscal
*&---------------------------------------------------------------------*
FORM carga_fiscal .

  LOOP AT <ft_xls> ASSIGNING FIELD-SYMBOL(<line>).

    CHECK sy-tabix GT 1.

    ASSIGN COMPONENT 'A' OF STRUCTURE <line> TO <fs_value>.
    gs_pstfiscal-codesap = <fs_value>.
    CONDENSE gs_pstfiscal-codesap.

    ASSIGN COMPONENT 'B' OF STRUCTURE <line> TO <fs_value>.
    gs_pstfiscal-tiperfc = <fs_value>.
    CONDENSE gs_pstfiscal-tiperfc.

    ASSIGN COMPONENT 'C' OF STRUCTURE <line> TO <fs_value>.
    gs_pstfiscal-id_rfc = <fs_value>.
    CONDENSE gs_pstfiscal-id_rfc.

    ASSIGN COMPONENT 'D' OF STRUCTURE <line> TO <fs_value>.
    gs_pstfiscal-pagosat = <fs_value>.
    CONDENSE gs_pstfiscal-pagosat.

    ASSIGN COMPONENT 'E' OF STRUCTURE <line> TO <fs_value>.
    gs_pstfiscal-metodo = <fs_value>.
    CONDENSE gs_pstfiscal-metodo.

    ASSIGN COMPONENT 'F' OF STRUCTURE <line> TO <fs_value>.
    gs_pstfiscal-regimen = <fs_value>.
    CONDENSE gs_pstfiscal-regimen.

    ASSIGN COMPONENT 'G' OF STRUCTURE <line> TO <fs_value>.
    gs_pstfiscal-cfdifac = <fs_value>.
    CONDENSE gs_pstfiscal-cfdifac.

    ASSIGN COMPONENT 'H' OF STRUCTURE <line> TO <fs_value>.
    gs_pstfiscal-cfdinrc = <fs_value>.
    CONDENSE gs_pstfiscal-cfdinrc.

    ASSIGN COMPONENT 'I' OF STRUCTURE <line> TO <fs_value>.
    gs_pstfiscal-tip_op  = <fs_value>.
    CONDENSE gs_pstfiscal-tip_op .

    ASSIGN COMPONENT 'J' OF STRUCTURE <line> TO <fs_value>.
    gs_pstfiscal-tip_ind  = <fs_value>.
    CONDENSE gs_pstfiscal-tip_ind.

    IF  gs_pstfiscal-codesap IS INITIAL
    AND gs_pstfiscal-tiperfc IS INITIAL
    AND gs_pstfiscal-id_rfc  IS INITIAL
    AND gs_pstfiscal-pagosat IS INITIAL
    AND gs_pstfiscal-metodo  IS INITIAL
    AND gs_pstfiscal-regimen IS INITIAL
    AND gs_pstfiscal-cfdifac IS INITIAL
    AND gs_pstfiscal-cfdinrc IS INITIAL
    AND gs_pstfiscal-tip_ind IS INITIAL
    AND gs_pstfiscal-tip_op IS INITIAL.

      CLEAR gs_pstfiscal.

    ELSE.

      APPEND gs_pstfiscal TO t_pstfiscal.
      CLEAR gs_pstfiscal.

    ENDIF.

  ENDLOOP.

  IF t_pstfiscal[] IS NOT INITIAL.
    SORT t_pstfiscal BY codesap.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form carga_alvgnrales
*&---------------------------------------------------------------------*
*& Carga de Tabla Datos Generales para Ejecucion de Clase
*&---------------------------------------------------------------------*
FORM carga_alvgnrales.

  SORT t_pstfiscal BY codesap.

  LOOP AT t_pstgnral INTO gs_pstgnral.

    " Datos Generales
    gs_gnrales-typereg = gs_pstgnral-typereg. " Tipo Registro
    gs_gnrales-code    = gs_pstgnral-code   . " Codigo
    gs_gnrales-razon   = gs_pstgnral-razon  . " Razon Social
    gs_gnrales-tipo    = gs_pstgnral-tipo   . " Tipo
    gs_gnrales-grp_bp  = gs_pstgnral-grp_bp . " Grupo
    gs_gnrales-aped1   = gs_pstgnral-aped1  . " Primero Apellido
    gs_gnrales-aped2   = gs_pstgnral-aped2  . " Segundo Apellido
    gs_gnrales-name1   = gs_pstgnral-name1  . " Primero Nombre
    gs_gnrales-name2   = gs_pstgnral-name2  . " Segundo Nombre
    gs_gnrales-namect  = gs_pstgnral-namect . " Nombre Corto
    gs_gnrales-calle   = gs_pstgnral-calle  . " Calle
    gs_gnrales-numext  = gs_pstgnral-numext . " Numero Exterior
    gs_gnrales-numeint = gs_pstgnral-numeint. " Numero Interior
    gs_gnrales-colonia = gs_pstgnral-colonia. " Colonia
    gs_gnrales-local   = gs_pstgnral-local  . " Localidad
    gs_gnrales-munic   = gs_pstgnral-munic  . " Municipio
    gs_gnrales-refer   = gs_pstgnral-refer  . " Referencia
    gs_gnrales-estado  = gs_pstgnral-estado . " Estado
    gs_gnrales-pais    = gs_pstgnral-pais   . " Pais
    gs_gnrales-c_p_    = gs_pstgnral-c_p_   . " Codigo Postal
    gs_gnrales-idioma  = gs_pstgnral-idioma . " Idioma
    gs_gnrales-telef   = gs_pstgnral-telef  . " Telefono
    gs_gnrales-email1  = gs_pstgnral-email1 . " Correo Electronico  1
    gs_gnrales-email2  = gs_pstgnral-email2 . " Correo Electronico  2
    gs_gnrales-email3  = gs_pstgnral-email3 . " Correo Electronico  3
    gs_gnrales-email4  = gs_pstgnral-email4 . " Correo Electronico  4
    " Informacion Fiscal

    CLEAR gs_pstfiscal.
    READ TABLE t_pstfiscal INTO gs_pstfiscal
    WITH KEY codesap = gs_pstgnral-code.
    IF sy-subrc EQ 0.
      gs_gnrales-codesap = gs_pstfiscal-codesap. " Codigo SAP
      gs_gnrales-tiperfc = gs_pstfiscal-tiperfc.    " Tipo RFC
      gs_gnrales-id_rfc  = gs_pstfiscal-id_rfc . " Ident Fiscal (RFC)
      gs_gnrales-pagosat = gs_pstfiscal-pagosat. " Forma de Pago SAT
      gs_gnrales-metodo  = gs_pstfiscal-metodo . " Metodo de Pago SAT
      gs_gnrales-regimen = gs_pstfiscal-regimen. " Regimen Fiscal SAT
      gs_gnrales-cfdifac = gs_pstfiscal-cfdifac. " USO CFDI FAC SAT
      gs_gnrales-cfdinrc = gs_pstfiscal-cfdinrc. " USO CFDI NCR SAT
      gs_gnrales-tip_op  = gs_pstfiscal-tip_op. " Tipo operación
      gs_gnrales-tip_ind = gs_pstfiscal-tip_ind. " Tipo de industri
    ENDIF.

    APPEND gs_gnrales TO t_gnrales.
    CLEAR gs_gnrales.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_alvgclntefin
*&---------------------------------------------------------------------*
*& Carga de Tabla Cliente Financiero para Ejecucion de Clase
*&---------------------------------------------------------------------*
FORM carga_alvgclntefin .

  LOOP AT t_pstcliente INTO gs_pstcliente.

    gs_cntefinan-id_empre  = gs_pstcliente-id_empre . " Sociedad / Empresa
    gs_cntefinan-code      = gs_pstcliente-code     . " Codigo Empresa
    gs_cntefinan-grptesor  = gs_pstcliente-grptesor . " Grupo de Tesoreria
    gs_cntefinan-cntasap   = gs_pstcliente-cntasap  . " Cuenta Saldo SAP
    gs_cntefinan-condpago  = gs_pstcliente-condpago . " Condicion Pago
    gs_cntefinan-typecred  = gs_pstcliente-typecred . " Tipo Credito
    gs_cntefinan-moncred   = gs_pstcliente-moncred  . " Moneda Crédito
    gs_cntefinan-limitcred = gs_pstcliente-limitcred. " Limite Credito
    gs_cntefinan-perfcred  = gs_pstcliente-perfcred . " Perfil de Crédito
    gs_cntefinan-tipogrant = gs_pstcliente-tipogrant. " Tipo Garantia
    gs_cntefinan-monegrant = gs_pstcliente-monegrant. " Moneda Garantia
    gs_cntefinan-valorgrnt = gs_pstcliente-valorgrnt. " Valor Garantia
    gs_cntefinan-vencgrant = gs_pstcliente-vencgrant. " Vencimiento Garantia

    " Retencion
    READ TABLE t_pstcntret INTO gs_pstcntret
    WITH KEY empresa = gs_pstcliente-id_empre
             code    = gs_pstcliente-code.
    IF sy-subrc EQ 0.
      gs_cntefinan-impuesto = gs_pstcntret-impuesto. " Impuesto
    ENDIF.

    APPEND gs_cntefinan TO t_cntefinan.
    CLEAR gs_cntefinan.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_alvclntevnta
*&---------------------------------------------------------------------*
*& Carga de Tabla Cliente Ventas para Ejecucion de Clase
*&---------------------------------------------------------------------*
FORM carga_alvclntevnta .

  LOOP AT t_pstcntevta INTO gs_pstcntevta.

    gs_cntevntas-code     = gs_pstcntevta-code    . " Codigo SAP
    gs_cntevntas-orgvnta  = gs_pstcntevta-orgvnta . " Organización de Ventas
    gs_cntevntas-canal    = gs_pstcntevta-canal   . " Canal de Distribución
    gs_cntevntas-sector   = gs_pstcntevta-sector  . " Sector
    gs_cntevntas-grpclte  = gs_pstcntevta-grpclte . " Grupo de Clientes SD
    gs_cntevntas-zonavnta = gs_pstcntevta-zonavnta. " Zona de Ventas
    gs_cntevntas-ofcventa = gs_pstcntevta-ofcventa. " Oficina de Ventas
    gs_cntevntas-grpovnta = gs_pstcntevta-grpovnta. " Grupo de Ventas
    gs_cntevntas-clas_abc = gs_pstcntevta-clas_abc. " Clasificacion ABC
    gs_cntevntas-mone_ped = gs_pstcntevta-mone_ped. " Moneda Pedido
    gs_cntevntas-condentg = gs_pstcntevta-condentg. " Condición de Entrega
    gs_cntevntas-cedis    = gs_pstcntevta-cedis   . " Cedis Atend
    gs_cntevntas-tol_exce = gs_pstcntevta-tol_exce. " Tolerancia de Exceso
    gs_cntevntas-incoterm = gs_pstcntevta-incoterm. " Incoterm
    gs_cntevntas-locincot = gs_pstcntevta-locincot. " Local Responsabilidad Incoterm

    CLEAR gs_pstcnteint.
    READ TABLE t_pstcnteint INTO gs_pstcnteint
    WITH KEY code    = gs_pstcntevta-code
             orgvnta = gs_pstcntevta-orgvnta.
    IF sy-subrc EQ 0.
      gs_cntevntas-canal     = gs_pstcnteint-canal    .  " Canal de Distribución
      gs_cntevntas-sector    = gs_pstcnteint-sector   .  " Sector
      gs_cntevntas-tipointer = gs_pstcnteint-tipointer.  " Tipo Interlocutor
      gs_cntevntas-interloc  = gs_pstcnteint-interloc .  " Interlocutor
    ENDIF.

    APPEND gs_cntevntas TO t_cntevntas.
    CLEAR gs_cntevntas.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_alvdirentga
*&---------------------------------------------------------------------*
*& Carga de Tabla Direccion de Entrega para Ejecucion de Clase
*&---------------------------------------------------------------------*
FORM carga_alvdirentga.

  LOOP AT t_pstentrega INTO gs_pstentrega.

    " Entrega
    gs_direntg-code    = gs_pstentrega-code   .   " Codigo
    gs_direntg-namect  = gs_pstentrega-namect .   " Nombre Corto
    gs_direntg-descdir = gs_pstentrega-descdir.   " Descripcion Direccion
    gs_direntg-calle   = gs_pstentrega-calle  .   " Calle
    gs_direntg-numext  = gs_pstentrega-numext .   " Numero Exterior
    gs_direntg-numint  = gs_pstentrega-numint .   " Numero Interior
    gs_direntg-colonia = gs_pstentrega-colonia.   " Colonia
    gs_direntg-localid = gs_pstentrega-localid.   " Localidad
    gs_direntg-municpo = gs_pstentrega-municpo.   " Municipio
    gs_direntg-refercn = gs_pstentrega-refercn.   " Referencia
    gs_direntg-estado  = gs_pstentrega-estado .   " Estado
    gs_direntg-pais    = gs_pstentrega-pais   .   " Pais
    gs_direntg-codepos = gs_pstentrega-codepos.   " Codigo Postal
    gs_direntg-telefon = gs_pstentrega-telefon.   " Telefono
    gs_direntg-email1  = gs_pstentrega-email1 .   " Correo Electronico 1
    gs_direntg-email2  = gs_pstentrega-email2 .   " Correo Electronico 2
    gs_direntg-email3  = gs_pstentrega-email3 .   " Correo Electronico 3
    gs_direntg-email4  = gs_pstentrega-email4 .   " Correo Electronico 4

    " Empresa
    CLEAR gs_pstempresa.
    READ TABLE t_pstempresa INTO gs_pstempresa
    WITH KEY code = gs_pstentrega-code.
    IF sy-subrc EQ 0.
      gs_direntg-cedisaten = gs_pstempresa-cedisaten. " Cedis Atend
      gs_direntg-respons   = gs_pstempresa-respons  . " Responsable
      gs_direntg-diarecp   = gs_pstempresa-diarecp  . " Dias Recepcion
      gs_direntg-horarecp  = gs_pstempresa-horarecp . " Hora Recepcion
      gs_direntg-tipocarga = gs_pstempresa-tipocarga. " Tipo de Carga
      gs_direntg-tipomano  = gs_pstempresa-tipomano . " Tipo Maniobra
***  INI DNAVOA 13.06.2025
*      gs_direntg-zona_exp  = gs_pstempresa-zona_exp.  " Zona de ventas
*      gs_direntg-cond_exp  = gs_pstempresa-cond_exp.  " Condiciones de Expedicion
***  FIN DNAVOA 13.06.2025
    ENDIF.

    " Herramienta
    CLEAR gs_pstherram.
    READ TABLE t_pstherram INTO gs_pstherram
    WITH KEY code = gs_pstentrega-code.
    IF sy-subrc EQ 0.
      gs_direntg-orgventa = gs_pstherram-orgventa.  " Organizacion de Venta
      gs_direntg-canaldbt = gs_pstherram-canaldbt.  " Canal Distribucion
      gs_direntg-sector   = gs_pstherram-sector  .  " Sector
      gs_direntg-namecrto = gs_pstherram-namecrto.  " Nombre Corto
      gs_direntg-herrmnta = gs_pstherram-herrmnta.  " Herramental
    ENDIF.

    " Instruccion
    CLEAR gs_pstinstruc.
    READ TABLE t_pstinstruc INTO gs_pstinstruc
    WITH KEY code = gs_pstentrega-code.
    IF sy-subrc EQ 0.
      gs_direntg-codedirc = gs_pstinstruc-codedirc.  " Codigo Direccion
      gs_direntg-instrucc = gs_pstinstruc-instrucc.  " Instruccion
    ENDIF.

    APPEND gs_direntg TO t_direntg.
    CLEAR gs_direntg.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_alvproveedor
*&---------------------------------------------------------------------*
*& Carga de Tabla Proveedor para Ejecucion de Clase
*&---------------------------------------------------------------------*
FORM carga_alvproveedor .

  LOOP AT t_pstprov INTO gs_pstprov.

    gs_provedor-empresa   = gs_pstprov-empresa  ." Empresa
    gs_provedor-code      = gs_pstprov-code     ." Codigo
    gs_provedor-gpoteso   = gs_pstprov-gpoteso  ." Grupo Tesoreria
*    gs_provedor-gpoprvsap = gs_pstprov-gpoprvsap." Grupo Proveedor SAP
    gs_provedor-cntasaldo = gs_pstprov-cntasaldo." Cuenta Saldo
    gs_provedor-condpago  = gs_pstprov-condpago ." Condicion Pago
    gs_provedor-paisbank  = gs_pstprov-paisbank ." País Banco
    gs_provedor-banco     = gs_pstprov-banco    ." Banco
    gs_provedor-suc_bank  = gs_pstprov-suc_bank ." Sucursal Bancaria
    gs_provedor-cnta_bank = gs_pstprov-cnta_bank." Cuenta Bancaria
    gs_provedor-incoterm  = gs_pstprov-incoterm ." Incoterm
    gs_provedor-mon_pedid = gs_pstprov-mon_pedid." Moneda Pedido

    " Proveedor Retencion
    CLEAR gs_pstprovrnt.
    READ TABLE t_pstprovrnt INTO gs_pstprovrnt
    WITH KEY empresa = gs_pstprov-empresa
             code    = gs_pstprov-code.
    IF sy-subrc EQ 0.
      gs_provedor-impuesto = gs_pstprovrnt-impuesto.
    ENDIF.

    APPEND gs_provedor TO t_provedor.
    CLEAR gs_provedor.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_alvprvcompras
*&---------------------------------------------------------------------*
*& Carga de Tabla Proveedor Compras para Ejecucion de Clase
*&---------------------------------------------------------------------*
FORM carga_alvprvcompras .

  LOOP AT t_pstprov INTO gs_pstprov.

    gs_provcomp-empresa   = gs_pstprov-empresa  ." Empresa
    gs_provcomp-code      = gs_pstprov-code     ." Codigo
    gs_provcomp-gpoteso   = gs_pstprov-gpoteso  ." Grupo Tesoreria
    gs_provcomp-gpoprvsap = gs_pstprov-gpoprvsap." Grupo Proveedor SAP
    gs_provcomp-cntasaldo = gs_pstprov-cntasaldo." Cuenta Saldo
    gs_provcomp-condpago  = gs_pstprov-condpago ." Condicion Pago
    gs_provcomp-paisbank  = gs_pstprov-paisbank ." País Banco
    gs_provcomp-banco     = gs_pstprov-banco    ." Banco
    gs_provcomp-suc_bank  = gs_pstprov-suc_bank ." Sucursal Bancaria
    gs_provcomp-cnta_bank = gs_pstprov-cnta_bank." Cuenta Bancaria
    gs_provcomp-incoterm  = gs_pstprov-incoterm ." Incoterm
    gs_provcomp-mon_pedid = gs_pstprov-mon_pedid." Moneda Pedido

    " Proveedor Retencion
    CLEAR gs_pstprovint.
    READ TABLE t_pstprovint INTO gs_pstprovint
    WITH KEY empresa    = gs_pstprov-empresa
             code_minor = gs_pstprov-code.
    IF sy-subrc EQ 0.
      gs_provcomp-tipo_inter = gs_pstprovint-tipo_inter. " Tipo
      gs_provcomp-interlcutr = gs_pstprovint-interlcutr. " Interlocutor
    ENDIF.

    APPEND gs_provcomp TO t_provcomp.
    CLEAR gs_provcomp.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_alvanexos
*&---------------------------------------------------------------------*
*& Carga de Tabla Anexos para Ejecucion de Clase
*&---------------------------------------------------------------------*
FORM carga_alvanexos .

  LOOP AT t_pstanexo INTO gs_pstanexo.

    gs_anexo-code_anexo = gs_pstanexo-code_anexo. " Codigo Anexo
    gs_anexo-typearch   = gs_pstanexo-typearch.   " Tipo Archivo
    gs_anexo-ruta       = gs_pstanexo-ruta    .   " Ruta

    APPEND gs_anexo TO t_anexo.
    CLEAR gs_anexo.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form creacion_bps
*&---------------------------------------------------------------------*
*& Creación Masiva de BPS por Carga Excel
*&---------------------------------------------------------------------*
FORM creacion_bps .

  CASE abap_true.
    WHEN r_crbps. " Alta de BPS

      TRY.
          PERFORM alta_bps.
        CATCH cx_root INTO DATA(lx).

          WRITE: 'Ocurrieron errores en el procesamiento, favor de correr el programa en prueba y corregirlos'.
          EXIT.
      ENDTRY.

    WHEN r_mdbps. " Modificacion BPS

      TRY.
          PERFORM modificar_bps.
        CATCH cx_root INTO lx.

          WRITE: 'Ocurrieron errores en el procesamiento, favor de correr el programa en prueba y corregirlos'.
          EXIT.
      ENDTRY.

    WHEN r_acint. " Actualizacion Interlocutor

      TRY.
          PERFORM actualiza_inter.
        CATCH cx_root INTO lx.

          WRITE: 'Ocurrieron errores en el procesamiento, favor de correr el programa en prueba y corregirlos'.
          EXIT.
      ENDTRY.

      " Carga de Herramientas
      IF t_pstherram IS INITIAL.

        TRY.
            PERFORM carga_herramientas.
          CATCH cx_root INTO lx.

            WRITE: 'Ocurrieron errores en el procesamiento, favor de correr el programa en prueba y corregirlos'.
            EXIT.
        ENDTRY.

      ENDIF.

      " Carta de Instruccion
      IF t_pstinstruc IS INITIAL.

        TRY.
            PERFORM carga_instruccion.
          CATCH cx_root INTO lx.

            WRITE: 'Ocurrieron errores en el procesamiento, favor de correr el programa en prueba y corregirlos'.
            EXIT.
        ENDTRY.

      ENDIF.

    WHEN r_atdir. " Alta Direcciones

      TRY.
          PERFORM alta_direcc.
        CATCH cx_root INTO lx.

          WRITE: 'Ocurrieron errores en el procesamiento, favor de correr el programa en prueba y corregirlos'.
          EXIT.
      ENDTRY.

    WHEN r_cranx. " Alta Anexos

      TRY.
          PERFORM alta_anexo.
        CATCH cx_root INTO lx.

          WRITE: 'Ocurrieron errores en el procesamiento, favor de correr el programa en prueba y corregirlos'.
          EXIT.
      ENDTRY.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alta_bps
*&---------------------------------------------------------------------*
*& Alta Masiva de BPS por Ejecucion de Clase Estandar
*&---------------------------------------------------------------------*
FORM alta_bps .

  " Data Local
  DATA lv_tiposat TYPE c.
  DATA lv_rolecat TYPE c LENGTH 6.

  DATA: lv_mino TYPE char30.

  DATA lv_index  TYPE i.
  DATA lv_nombre TYPE c LENGTH 30.
  DATA lv_uk_tabix TYPE sy-tabix.

  DATA lv_validate TYPE c.
  DATA: lv_parza TYPE parza.

  CONSTANTS lc_group(3) TYPE c VALUE 'ZDM'.
  CONSTANTS lc_z01(3)   TYPE c VALUE 'Z01'.
  CONSTANTS lc_fisi(6)  TYPE c VALUE 'Física'.
  CONSTANTS lc_juri(8)  TYPE c VALUE 'Jurídico'.

  CONSTANTS lc_s     TYPE c VALUE 'S'.
  CONSTANTS lc_e     TYPE c VALUE 'E'.
  CONSTANTS lc_1     TYPE c VALUE '1'.
  CONSTANTS lc_2     TYPE c VALUE '2'.
  CONSTANTS lc_crear TYPE c VALUE 'I'.
*&-------------------------------------------------------

  SORT: t_pstcliente BY code,                 "Tabla Clientes
        t_pstcnteint BY code,                 "Tabla Cliente Interlocutor
        t_pstcntret  BY code empresa,         "Tabla Cliente Retención
        t_cntefinan  BY code,                 "Tabla Cliente Financiero
        t_pstcntevta BY code,                 "Tabla Cliente Ventas
        t_provedor   BY code,                 "Tabla Proveedor
        t_pstprovint BY code_minor empresa,   "Tabla Proveedor Interlocutor
        t_pstprovrnt BY code empresa,         "Tabla Proveedor Retención
        t_pstentrega BY code.                 "Tabla Dirección Entrega

  DATA: lt_functions_aux TYPE TABLE OF cmds_ei_functions.

  LOOP AT t_gnrales INTO gs_gnrales.
    REFRESH: lt_com, lt_address, lt_tax, lt_bank, lt_seg, lt_sales,
             lt_functions, lt_company, lt_purchasing, lt_func_prov, lt_email, lt_phone.

    lv_bp = gs_gnrales-code.
    ls_data_deep-code =  gs_gnrales-code.

    CLEAR lv_index.
    lv_index = sy-tabix.

    IF chk_test IS INITIAL.
      PERFORM mantenimiento_vmap USING lv_index.
    ENDIF.
    " ---> HEADERC / CENTRAL DATA  ---
    " --> Tipo de Categoria

    CLEAR lv_tiposat.
    CASE gs_gnrales-tipo.
      WHEN lc_juri.
        lv_tiposat = lc_2.
      WHEN lc_fisi.
        lv_tiposat = lc_1.
    ENDCASE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_gnrales-code
      IMPORTING
        output = gs_gnrales-code.

    ls_data-partner-header-object_task                                          = lc_crear.
    ls_data-partner-central_data-common-data-bp_control-category                = lv_tiposat.
    ls_data-partner-central_data-common-data-bp_control-grouping                = gs_gnrales-grp_bp.
    ls_data-partner-central_data-common-data-bp_centraldata-searchterm1         = gs_gnrales-namect.

    ls_phone-contact-data-telephone                                             = gs_gnrales-telef.
    APPEND ls_phone TO lt_phone.
    ls_data-partner-central_data-communication-phone-phone                      = lt_phone.

    IF gs_gnrales-email1 IS NOT INITIAL.
      ls_email-contact-data-e_mail                                              = gs_gnrales-email1.
      APPEND ls_email TO lt_email. CLEAR ls_email.
    ENDIF.

    IF gs_gnrales-email2 IS NOT INITIAL.
      ls_email-contact-data-e_mail                                              = gs_gnrales-email2.
      APPEND ls_email TO lt_email. CLEAR ls_email.
    ENDIF.

    IF gs_gnrales-email3 IS NOT INITIAL.
      ls_email-contact-data-e_mail                                              = gs_gnrales-email3.
      APPEND ls_email TO lt_email. CLEAR ls_email.
    ENDIF.

    IF gs_gnrales-email4 IS NOT INITIAL.
      ls_email-contact-data-e_mail                                              = gs_gnrales-email4.
      APPEND ls_email TO lt_email. CLEAR ls_email.
    ENDIF.
    ls_data-partner-central_data-communication-smtp-smtp                        = lt_email.

    IF lv_tiposat EQ lc_1.
      ls_data-partner-central_data-common-data-bp_centraldata-partnerlanguageiso = gs_gnrales-idioma.
    ENDIF.

    ls_data-partner-header-object_instance-bpartner                             = gs_gnrales-code.
    ls_data-partner-header-object_instance-bpartnerguid                         = gs_gnrales-code.

    ls_data-partner-central_data-common-data-bp_control-category                = lv_tiposat.

    "ESTO ES SI SÓLO ES PERSONA (CATEGORY 1)
    IF lv_tiposat EQ lc_1.

      ls_data-partner-central_data-common-data-bp_person-firstname             = gs_gnrales-name1.
      ls_data-partner-central_data-common-data-bp_person-secondname            = gs_gnrales-aped2.
      ls_data-partner-central_data-common-data-bp_person-lastname              = gs_gnrales-aped1.
      ls_data-partner-central_data-common-data-bp_person-middlename            = gs_gnrales-name2.
      ls_data-partner-central_data-common-data-bp_person-fullname              = |{ gs_gnrales-aped1 } { gs_gnrales-aped2 } { gs_gnrales-name1 } { gs_gnrales-name2 }|.
      ls_data-partner-central_data-common-data-bp_person-namcountry            = gs_gnrales-pais.
      ls_data-partner-central_data-common-data-bp_person-namcountryiso         = gs_gnrales-pais.
      ls_data-partner-central_data-common-data-bp_person-correspondlanguageiso = gs_gnrales-idioma.

      ls_data-partner-central_data-common-datax-bp_person-firstname             = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-firstname             IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-secondname            = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-secondname            IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-lastname              = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-lastname              IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-middlename            = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-middlename            IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-fullname              = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-fullname              IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-namcountry            = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-namcountry            IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-namcountryiso         = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-namcountryiso         IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-correspondlanguageiso = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-correspondlanguageiso IS NOT INITIAL THEN abap_true ELSE abap_false ).

      "ESTO ES SI SÓLO ES ORGANIZACIÓN(CATEGORY 2)
    ELSE.


      DO.
        DATA(lv_indx) = sy-index.

        CASE lv_indx.
          WHEN 1.
            DATA(lv_long) = strlen( gs_gnrales-razon ).
            IF lv_long > 40.
              ls_data-partner-central_data-common-data-bp_organization-name1             = gs_gnrales-razon+0(40).
              ls_data-partner-central_data-common-datax-bp_organization-name1             = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name1 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              gs_gnrales-razon = gs_gnrales-razon+40.
            ELSE.
              ls_data-partner-central_data-common-data-bp_organization-name1             = gs_gnrales-razon.
              ls_data-partner-central_data-common-datax-bp_organization-name1             = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name1 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              EXIT.
            ENDIF.
          WHEN 2.
            lv_long = strlen( gs_gnrales-razon ).
            IF lv_long > 40.
              ls_data-partner-central_data-common-data-bp_organization-name2             = gs_gnrales-razon+0(40).
              ls_data-partner-central_data-common-datax-bp_organization-name2             = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name2 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              gs_gnrales-razon = gs_gnrales-razon+40.
            ELSE.
              ls_data-partner-central_data-common-data-bp_organization-name2             = gs_gnrales-razon.
              ls_data-partner-central_data-common-datax-bp_organization-name2             = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name2 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              EXIT.
            ENDIF.
          WHEN 3.
            lv_long = strlen( gs_gnrales-razon ).
            IF lv_long > 40.
              ls_data-partner-central_data-common-data-bp_organization-name3             = gs_gnrales-razon+0(40).
              ls_data-partner-central_data-common-datax-bp_organization-name3             = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name3 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              gs_gnrales-razon = gs_gnrales-razon+40.
            ELSE.
              ls_data-partner-central_data-common-data-bp_organization-name3             = gs_gnrales-razon.
              ls_data-partner-central_data-common-datax-bp_organization-name3             = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name3 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              EXIT.
            ENDIF.
          WHEN 4.
            lv_long = strlen( gs_gnrales-razon ).
            IF lv_long > 40.
              ls_data-partner-central_data-common-data-bp_organization-name4             = gs_gnrales-razon+0(40).
              ls_data-partner-central_data-common-datax-bp_organization-name4             = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name4 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              gs_gnrales-razon = gs_gnrales-razon+40.
            ELSE.
              ls_data-partner-central_data-common-data-bp_organization-name4             = gs_gnrales-razon.
              ls_data-partner-central_data-common-datax-bp_organization-name4             = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name4 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              EXIT.
            ENDIF.
          WHEN OTHERS.
            EXIT.
        ENDCASE.

      ENDDO.


    ENDIF.

    "--> ROLES
    REFRESH lt_roles.

    CLEAR gs_pstcliente.
    READ TABLE t_pstcliente INTO gs_pstcliente
    WITH KEY code = lv_bp.
    IF sy-subrc EQ 0.
      ls_roles-data-rolecategory = lc_flcu00.
      APPEND ls_roles TO lt_roles.
      CLEAR ls_roles.

      ls_roles-data-rolecategory = lc_flcu01.
      APPEND ls_roles TO lt_roles.
      CLEAR ls_roles.

      ls_roles-data-rolecategory = lc_ukm000.
      APPEND ls_roles TO lt_roles.
      CLEAR ls_roles.

      DATA(lv_flag) = abap_true.
    ENDIF.

    CLEAR gs_pstentrega.
    READ TABLE t_pstentrega INTO gs_pstentrega
    WITH KEY code = lv_bp.
    IF sy-subrc EQ 0.
      DATA(lv_bp_d) = abap_true.
    ENDIF.

    CLEAR gs_provedor.
    READ TABLE t_provedor INTO gs_provedor
    WITH KEY code = lv_bp.
    IF sy-subrc EQ 0.
      ls_roles-data-rolecategory = lc_flvn00.
      APPEND ls_roles TO lt_roles.
      CLEAR ls_roles.

      ls_roles-data-rolecategory = lc_flvn01.
      APPEND ls_roles TO lt_roles.
      CLEAR ls_roles.
    ENDIF.

    ls_data-partner-central_data-role-roles = lt_roles.
    SORT lt_roles BY data-rolecategory.

    "--> BANK DETAILS
    CLEAR gs_provedor.
*    READ TABLE t_provedor INTO gs_provedor
*    WITH KEY code = lv_bp.
*    IF sy-subrc EQ 0.

*
    CLEAR: lt_bank[], ls_bank.

*
    LOOP AT t_provedor INTO gs_provedor
      WHERE banco IS NOT INITIAL
      AND   code = lv_bp.

      ls_bank-data-bank_ctry     = gs_provedor-paisbank.
      ls_bank-data-bank_ctryiso  = gs_provedor-paisbank.
      ls_bank-data-bank_key      = gs_provedor-banco.
      ls_bank-data-bank_acct     = gs_provedor-cnta_bank.
      ls_bank-data-bank_ref      = gs_provedor-suc_bank.
      ls_bank-data-bankdetailvalidfrom = '20000101'. "Ajuste fecha validez banco
      ls_bank-data-bankdetailvalidto = '99991231'. "Ajuste fecha validez banco

      ls_bank-datax-bankdetailvalidto   = COND #( WHEN ls_bank-data-bankdetailvalidto   IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_bank-datax-bankdetailvalidfrom = COND #( WHEN ls_bank-data-bankdetailvalidfrom IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_bank-datax-bank_ctry           = COND #( WHEN ls_bank-data-bank_ctry           IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_bank-datax-bank_ctryiso        = COND #( WHEN ls_bank-data-bank_ctryiso        IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_bank-datax-bank_key            = COND #( WHEN ls_bank-data-bank_key            IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_bank-datax-bank_acct           = COND #( WHEN ls_bank-data-bank_acct           IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_bank-datax-bank_ref            = COND #( WHEN ls_bank-data-bank_ref            IS NOT INITIAL THEN abap_true ELSE abap_false ).

      APPEND ls_bank TO lt_bank.
      CLEAR ls_bank.

      EXIT.

    ENDLOOP.

    IF lt_bank[] IS NOT INITIAL.
      ls_data-partner-central_data-bankdetail-bankdetails = lt_bank.
    ENDIF.

*  ENDIF.

    " --> TAXNUMBER
    IF gs_gnrales-tiperfc IS NOT INITIAL AND gs_gnrales-id_rfc IS NOT INITIAL.

      ls_tax-data_key-taxtype   = gs_gnrales-tiperfc.
      ls_tax-data_key-taxnumber = gs_gnrales-id_rfc.
      ls_data-vendor-central_data-central-data-j_1kftbus = gs_gnrales-tip_op. "DNAVOA
      ls_data-vendor-central_data-central-data-j_1kftind = gs_gnrales-tip_ind. "DNAVOA

      CASE lv_tiposat.
        WHEN lc_1.
          ls_data-partner-central_data-taxnumber-common-data-nat_person  = abap_true.
          ls_data-partner-central_data-taxnumber-common-datax-nat_person = COND #( WHEN ls_data-partner-central_data-taxnumber-common-data-nat_person IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ENDCASE.

      REFRESH lt_tax.
      APPEND ls_tax TO lt_tax.
      CLEAR ls_tax.

      ls_data-partner-central_data-taxnumber-taxnumbers   = lt_tax.

    ENDIF.

    "-->ADDRESSESS
    ls_address-data-postal-data-city         = gs_gnrales-munic.
    ls_address-data-postal-data-district     = gs_gnrales-colonia.
    ls_address-data-postal-data-postl_cod1   = gs_gnrales-c_p_.
    ls_address-data-postal-data-street       = gs_gnrales-calle.
    ls_address-data-postal-data-house_no     = gs_gnrales-numext.
    ls_address-data-postal-data-country      = gs_gnrales-pais.
    ls_address-data-postal-data-countryiso   = gs_gnrales-pais.
    ls_address-data-postal-data-region       = gs_gnrales-estado.

    CLEAR: lv_indx, lv_long.
    lv_long = strlen( gs_gnrales-calle ).
    IF lv_long > 60.

      gs_gnrales-calle = gs_gnrales-calle+60.

      DO.
        lv_indx = sy-index.

        CASE lv_indx.
          WHEN 1.
            lv_long = strlen( gs_gnrales-calle ).
            IF lv_long > 40.
              ls_address-data-postal-data-str_suppl3             = gs_gnrales-calle+0(40).
              ls_address-data-postal-datax-str_suppl3             = COND #( WHEN ls_address-data-postal-data-str_suppl3 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              gs_gnrales-calle = gs_gnrales-calle+40.
            ELSE.
              ls_address-data-postal-data-str_suppl3             = gs_gnrales-calle.
              ls_address-data-postal-datax-str_suppl3             = COND #( WHEN ls_address-data-postal-data-str_suppl3 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              EXIT.
            ENDIF.
          WHEN 2.
            lv_long = strlen( gs_gnrales-calle ).
            IF lv_long > 40.
              ls_address-data-postal-data-location             = gs_gnrales-calle+0(40).
              ls_address-data-postal-datax-location             = COND #( WHEN ls_address-data-postal-data-location IS NOT INITIAL THEN abap_true ELSE abap_false ).
              gs_gnrales-calle = gs_gnrales-calle+40.
            ELSE.
              ls_address-data-postal-data-location             = gs_gnrales-calle.
              ls_address-data-postal-datax-location             = COND #( WHEN ls_address-data-postal-data-location IS NOT INITIAL THEN abap_true ELSE abap_false ).
              EXIT.
            ENDIF.
          WHEN OTHERS.
            EXIT.
        ENDCASE.

      ENDDO.

    ENDIF.

    IF  lv_tiposat EQ lc_2.
      CASE gs_gnrales-pais.
        WHEN 'MX'.
          ls_address-data-postal-data-langu    = ''.
          ls_address-data-postal-data-languiso  = 'ES'.
        WHEN OTHERS.
          ls_address-data-postal-data-langu    = ''.
          ls_address-data-postal-data-languiso = 'EN'.
      ENDCASE.
    ENDIF.

    ls_address-data-postal-datax-city        = COND #( WHEN ls_address-data-postal-data-city       IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_address-data-postal-datax-district    = COND #( WHEN ls_address-data-postal-data-district   IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_address-data-postal-datax-postl_cod1  = COND #( WHEN ls_address-data-postal-data-postl_cod1 IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_address-data-postal-datax-street      = COND #( WHEN ls_address-data-postal-data-street     IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_address-data-postal-datax-house_no    = COND #( WHEN ls_address-data-postal-data-house_no   IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_address-data-postal-datax-country     = COND #( WHEN ls_address-data-postal-data-country    IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_address-data-postal-datax-countryiso  = COND #( WHEN ls_address-data-postal-data-countryiso IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_address-data-postal-datax-region      = COND #( WHEN ls_address-data-postal-data-region     IS NOT INITIAL THEN abap_true ELSE abap_false ).

    IF  lv_tiposat EQ lc_2.
      ls_address-data-postal-datax-langu       = COND #( WHEN ls_address-data-postal-data-langu          IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_address-data-postal-datax-langu_iso   = COND #( WHEN ls_address-data-postal-data-languiso       IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ENDIF.

    APPEND ls_address TO ls_data-partner-central_data-address-addresses.


    " -->SOLO CUANDO EL ROL ES FLCU00 y FLCU01 Y UKM000, Ademas SÓLO SI ES CLIENTE Y TRAE CRÉDITO
    READ TABLE lt_roles WITH KEY data-rolecategory = lc_flcu00
    TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.

      READ TABLE lt_roles WITH KEY data-rolecategory = lc_flcu01
      TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        READ TABLE lt_roles WITH KEY data-rolecategory = lc_ukm000
        TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          lv_validate = abap_true.
        ENDIF.
      ENDIF.

    ENDIF.

    IF lv_validate IS NOT INITIAL.

      "---> UKMBP_DATA
      CLEAR: gs_cntefinan, lv_uk_tabix.
      LOOP AT t_cntefinan INTO gs_cntefinan
        WHERE code = lv_bp.
        lv_uk_tabix += 1.

        IF gs_cntefinan-typecred IS NOT INITIAL.

          IF lv_uk_tabix EQ 1.
            ls_data-partner-ukmbp_data-profile-data-check_rule  = '01'.
            ls_data-partner-ukmbp_data-profile-data-limit_rule  = 'B2C-EXIST'.

            CASE gs_cntefinan-typecred.
              WHEN 'Crédito'.
                ls_data-partner-ukmbp_data-profile-data-risk_class = 'D'.
              WHEN 'Preferencial'.
                ls_data-partner-ukmbp_data-profile-data-risk_class = 'A'.
                ls_data-partner-ukmbp_data-profile-data-check_rule  = 'Z1'.
              WHEN 'Contado'.
                ls_data-partner-ukmbp_data-profile-data-risk_class = 'F'.
              WHEN 'Anticipado'.
                ls_data-partner-ukmbp_data-profile-data-risk_class = 'E'.
              WHEN 'Contraentrega'.
                ls_data-partner-ukmbp_data-profile-data-risk_class = 'C'.
              WHEN 'Suspendido'.
                ls_data-partner-ukmbp_data-profile-data-risk_class = 'D'.
                ls_seg-data-xblocked = abap_true.
                ls_seg-datax-xblocked = COND #( WHEN ls_seg-data-xblocked IS NOT INITIAL THEN abap_true ELSE abap_false ).
            ENDCASE.

*            ls_data-partner-ukmbp_data-profile-datax-rating_val_date = abap_true.
            ls_data-partner-ukmbp_data-profile-datax-check_rule = COND #( WHEN ls_data-partner-ukmbp_data-profile-data-check_rule IS NOT INITIAL THEN abap_true ELSE abap_false ).
            ls_data-partner-ukmbp_data-profile-datax-limit_rule = COND #( WHEN ls_data-partner-ukmbp_data-profile-data-limit_rule IS NOT INITIAL THEN abap_true ELSE abap_false ).
            ls_data-partner-ukmbp_data-profile-datax-risk_class = COND #( WHEN ls_data-partner-ukmbp_data-profile-data-risk_class IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ENDIF.

          "--->     SEGMENTS
          ls_seg-data_key-partner       = |{ gs_cntefinan-code ALPHA = IN }|.
          ls_seg-data_key-credit_sgmnt  = gs_cntefinan-id_empre.
          ls_seg-data-credit_limit      = gs_cntefinan-limitcred.

          IF gs_cntefinan-typecred EQ 'Preferencial'.
            ls_seg-data-credit_limit      = '999999999999.00'.
          ELSE.
            ls_seg-data-credit_limit      = gs_cntefinan-limitcred.
          ENDIF.

          ls_seg-data-limit_valid_date  = '99991231'.

          IF ls_seg-data-credit_limit IS INITIAL OR ls_seg-data-credit_limit EQ 0.
            ls_seg-data-x_limit_zero      = abap_true.

            ls_seg-datax-x_limit_zero      = COND #( WHEN ls_seg-data-x_limit_zero IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ENDIF.

          ls_seg-datax-credit_limit     = COND #( WHEN ls_seg-data-credit_limit     IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_seg-datax-limit_valid_date = COND #( WHEN ls_seg-data-limit_valid_date IS NOT INITIAL THEN abap_true ELSE abap_false ).

          APPEND ls_seg TO ls_data-partner-ukmbp_data-segments-segments.
          CLEAR ls_seg.
        ENDIF.
*      ENDIF.
      ENDLOOP.
    ENDIF.

    "SÓLO CUANDO ROL FLCU00 Y FLCU01 (CUSTOMER)
    CLEAR: gs_cntefinan, lv_validate.
    READ TABLE lt_roles WITH KEY data-rolecategory = lc_flcu00
    TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      READ TABLE lt_roles WITH KEY data-rolecategory = lc_flcu01
      TRANSPORTING NO FIELDS.
      IF sy-subrc  EQ 0.
        lv_validate = abap_true.
      ENDIF.
    ENDIF.

    IF lv_validate IS NOT INITIAL.


      LOOP AT t_cntefinan INTO gs_cntefinan
        WHERE code = lv_bp.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_cntefinan-code
          IMPORTING
            output = ls_data-customer-header-object_instance-kunnr.

        ls_data-customer-header-object_task           = lc_crear.

        " -->     Customer - Company Data
        ls_com-task           = lc_crear.
        ls_com-data_key-bukrs = gs_cntefinan-id_empre.
        ls_com-data-zuawa     = lc_z01.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_cntefinan-cntasap
          IMPORTING
            output = ls_com-data-akont.

        ls_com-data-zterm     = gs_cntefinan-condpago. "DNAVOA
        ls_com-data-fdgrv     = gs_cntefinan-grptesor. "DNAVOA


        ls_com-datax-zuawa    = COND #( WHEN ls_com-data-zuawa    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_com-datax-akont    = COND #( WHEN ls_com-data-akont    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_com-datax-zterm    = COND #( WHEN ls_com-data-zterm    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_com-datax-fdgrv    = COND #( WHEN ls_com-data-fdgrv    IS NOT INITIAL THEN abap_true ELSE abap_false ).

        " --> WTAX_TYPE
        LOOP AT t_pstcntret INTO gs_pstcntret
                   WHERE code    = lv_bp AND
                         empresa = ls_com-data_key-bukrs.

          ls_wax_type-data_key-witht = gs_pstcntret-impuesto.

          ls_wax_type-data-wt_withcd = 'V1'.
          ls_wax_type-data-wt_agent  = abap_true.
          ls_wax_type-data-wt_agtdf  = '20000101'.
          ls_wax_type-data-wt_agtdt  = '99991231'.
*          ls_wax_type-data-wt_exdf = '20000101'. "ajuste fecha inicio de validez
*          ls_wax_type-data-wt_exdt = '99991231'. "ajuste fecha fin de validez
*
*          ls_wax_type-datax-wt_exdf = abap_true.
*          ls_wax_type-datax-wt_exdt = abap_true.
          ls_wax_type-datax-wt_withcd = COND #( WHEN ls_wax_type-data-wt_withcd    IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_wax_type-datax-wt_agent  = COND #( WHEN ls_wax_type-data-wt_agent     IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_wax_type-datax-wt_agtdf  = COND #( WHEN ls_wax_type-data-wt_agtdf     IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_wax_type-datax-wt_agtdt  = COND #( WHEN ls_wax_type-data-wt_agtdt     IS NOT INITIAL THEN abap_true ELSE abap_false ).

          APPEND ls_wax_type TO ls_wax_type_s-wtax_type.

        ENDLOOP.
        ls_com-wtax_type = ls_wax_type_s.
        FREE ls_wax_type_s.
        APPEND ls_com TO lt_com.

      ENDLOOP.
      ls_data-customer-company_data-company = lt_com.

      " --> SALES
      CLEAR: lt_taxind[].
      LOOP AT t_pstcntevta INTO gs_pstcntevta
          WHERE code = lv_bp.
        CLEAR: ls_taxind, lv_parza, lt_taxind[], ls_sales-functions-functions, lt_functions[].

        ls_sales-data_key-vkorg = gs_pstcntevta-orgvnta.
        ls_sales-data_key-vtweg = gs_pstcntevta-canal.
        ls_sales-data_key-spart = gs_pstcntevta-sector.
** INI DNAVOA 16.06.2025
        READ TABLE t_cntefinan INTO gs_cntefinan
          WITH KEY code = gs_pstcntevta-code
                   id_empre = gs_pstcntevta-orgvnta+1.
        ls_sales-data-zterm     = gs_cntefinan-condpago.

        ls_sales-data-kvgr4         = gs_gnrales-cfdifac.
        ls_sales-data-kvgr5         = gs_gnrales-cfdinrc.
** FIN DNAVOA 16.06.2025
        ls_sales-data-bzirk     = gs_pstcntevta-zonavnta.
        ls_sales-data-vkbur     = gs_pstcntevta-ofcventa.
        ls_sales-data-vkgrp     = gs_pstcntevta-grpovnta.
        ls_sales-data-klabc     = gs_pstcntevta-clas_abc.
        ls_sales-data-waers     = gs_pstcntevta-mone_ped.
        ls_sales-data-vsbed     = gs_pstcntevta-condentg.
        ls_sales-data-vwerk     = gs_pstcntevta-cedis.
        ls_sales-data-kurst     = 'M'.
        ls_sales-data-uebto     = gs_pstcntevta-tol_exce.

*** INI DNAVOA 16.06.2025
        SELECT incotermsversion FROM aincotermsv
          INTO TABLE @DATA(lt_ainco).
        IF sy-subrc EQ 0.
          DESCRIBE TABLE lt_ainco LINES DATA(lv_ainco_l).
          READ TABLE lt_ainco INTO DATA(ls_ainco) INDEX lv_ainco_l.
          IF sy-subrc EQ 0.
            ls_sales-data-incov = ls_ainco.
          ENDIF.
        ENDIF.
        ls_sales-data-inco1     = gs_pstcntevta-incoterm.
*** FIN DNAVOA 16.06.2025

        ls_sales-data-inco2_l   = gs_pstcntevta-locincot.
        ls_sales-data-kalks     = '1'.
        ls_sales-data-konda     = gs_pstcntevta-grpclte.

*** INI DNAVOA 06.13.2025
        ls_sales-data-kurst     = gs_pstcntevta-tip_cots.
        ls_sales-data-konda     = gs_pstcntevta-gp_prec.
        ls_sales-data-kalks     = gs_pstcntevta-esq_clte.
        ls_sales-data-vsbed     = gs_pstcntevta-cond_exp.
        ls_sales-data-ktgrd     = gs_pstcntevta-gpo_impt.
        ls_sales-data-kvgr3     = gs_pstcntevta-gpo_cli3.

        ls_sales-datax-kurst     = COND #( WHEN ls_sales-data-kurst    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-konda     = COND #( WHEN ls_sales-data-konda    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-kalks     = COND #( WHEN ls_sales-data-kalks    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-vsbed     = COND #( WHEN ls_sales-data-vsbed    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-ktgrd     = COND #( WHEN ls_sales-data-ktgrd    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-kvgr3     = COND #( WHEN ls_sales-data-kvgr3    IS NOT INITIAL THEN abap_true ELSE abap_false ).

        ls_taxind-task           = 'I'.
        ls_taxind-data_key-aland = 'MX'.
        ls_taxind-data_key-tatyp = 'TMX1'.
        ls_taxind-data-taxkd     =  gs_pstcntevta-clas_fis.
        ls_taxind-datax-taxkd    = COND #( WHEN ls_taxind-data-taxkd    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        APPEND ls_taxind TO lt_taxind.

        ls_taxind-data_key-tatyp = 'TMX2'.
        APPEND ls_taxind TO lt_taxind.

*** FIN DNAVOA 06.13.2025

        ls_sales-datax-zterm     = COND #( WHEN ls_sales-data-zterm    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-inco1     = COND #( WHEN ls_sales-data-inco1    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-bzirk     = COND #( WHEN ls_sales-data-bzirk    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-vkbur     = COND #( WHEN ls_sales-data-vkbur    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-vkgrp     = COND #( WHEN ls_sales-data-vkgrp    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-klabc     = COND #( WHEN ls_sales-data-klabc    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-waers     = COND #( WHEN ls_sales-data-waers IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-vsbed     = COND #( WHEN ls_sales-data-vsbed      IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-vwerk     = COND #( WHEN ls_sales-data-vwerk      IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-kurst     = COND #( WHEN ls_sales-data-kurst      IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-uebto     = COND #( WHEN ls_sales-data-uebto      IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-incov     = COND #( WHEN ls_sales-data-incov      IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-inco2_l   = COND #( WHEN ls_sales-data-inco2_l    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-kalks     = COND #( WHEN ls_sales-data-kalks      IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_sales-datax-konda     = COND #( WHEN ls_sales-data-konda      IS NOT INITIAL THEN abap_true ELSE abap_false ).

        "SÓLO SI HAY DATA RELACIONADA EN PESTAÑA CLIENTE-INTERLOCUTOR
        CLEAR gs_pstcnteint.
        "--->     FUNCTIONS
** INI DNAVOA

        "Se añade a El mismo como interlocutor
        ls_functions-task = 'I'. "DNAVOA
        ls_functions-data_key-parvw = 'WE'.

        ls_functions-data-partner   = gs_pstcntevta-code.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_functions-data-partner
          IMPORTING
            output = ls_functions-data-partner.
        ls_functions-datax-partner  = COND #( WHEN ls_functions-data-partner      IS NOT INITIAL THEN abap_true ELSE abap_false ).

        APPEND ls_functions TO lt_functions.
        CLEAR: ls_functions-task , ls_functions-data_key-parvw, ls_functions-data-partner.

        LOOP AT t_pstcnteint INTO gs_pstcnteint
          WHERE code    = lv_bp
            AND orgvnta = gs_pstcntevta-orgvnta.

          ls_functions-task = 'I'. "DNAVOA
          ls_functions-data_key-parvw = gs_pstcnteint-tipointer.

          SELECT * FROM knvp
            INTO TABLE @DATA(lt_parza)
            WHERE kunnr EQ @gs_gnrales-code
            AND   vkorg EQ @ls_sales-data_key-vkorg
            AND   parvw EQ @gs_pstcnteint-tipointer.
          IF sy-subrc EQ 0.
            SORT lt_parza BY parza ASCENDING.
            READ TABLE lt_parza INTO DATA(ls_parza) INDEX 1.
            IF sy-subrc EQ 0.
              ls_functions-data_key-parza = ls_parza-parza + 1.
            ENDIF.
          ELSE.
            lv_parza += 1.
            ls_functions-data_key-parza = lv_parza.
          ENDIF.

          ls_functions-data-partner   = gs_pstcnteint-interloc.

          DATA(lv_abcde_min) = sy-abcde.
          TRANSLATE lv_abcde_min TO LOWER CASE.
          IF ls_functions-data-partner CA sy-abcde OR ls_functions-data-partner CA lv_abcde_min.
          ELSE.
            ls_functions-data-partner = |{ ls_functions-data-partner ALPHA = IN }|.
          ENDIF.

          ls_functions-datax-partner  = COND #( WHEN ls_functions-data-partner      IS NOT INITIAL THEN abap_true ELSE abap_false ).

          APPEND ls_functions TO lt_functions.

          ls_sales-task = 'I'. "DNAVOA
          ls_sales-functions-functions = lt_functions.

        ENDLOOP.
*** FIN DNAVOA

        APPEND ls_sales TO lt_sales.
      ENDLOOP.
      ls_data-customer-sales_data-sales = lt_sales.

      ls_data-customer-central_data-tax_ind-tax_ind = lt_taxind. "DNAVOA 13.06.2025

    ENDIF.


    "SOLO SI TRAE EL ROL FLVN00 Y FLVN01
    CLEAR: lv_validate.
    READ TABLE lt_roles WITH KEY data-rolecategory = lc_flvn00
    TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      READ TABLE lt_roles WITH KEY data-rolecategory = lc_flvn01
      TRANSPORTING NO FIELDS.
      IF sy-subrc  EQ 0.
        lv_validate = abap_true.
      ENDIF.
    ENDIF.

    IF lv_validate IS NOT INITIAL.
      ls_data-vendor-header-object_instance-lifnr     = gs_gnrales-code.
      ls_data-vendor-header-object_task               = lc_crear.

      "---> Vendor - Company Data
      CLEAR: gs_provedor, gs_pstprovrnt, ls_func_prov-data_key-parza.

      LOOP AT t_provedor INTO gs_provedor
          WHERE code = lv_bp.
**
        ls_company-data_key-bukrs = gs_provedor-empresa.

        ls_company-data-zuawa     = lc_z01.
        ls_company-datax-zuawa     = abap_true.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_provedor-cntasaldo
          IMPORTING
            output = ls_company-data-akont.
        ls_company-datax-akont = COND #( WHEN ls_company-data-akont      IS NOT INITIAL THEN abap_true ELSE abap_false ).

        ls_company-data-zterm     = gs_provedor-condpago.
        ls_company-datax-zterm     = COND #( WHEN ls_company-data-zterm      IS NOT INITIAL THEN abap_true ELSE abap_false ).

        ls_company-data-fdgrv     = gs_provedor-gpoteso.
        ls_company-datax-fdgrv     = COND #( WHEN ls_company-data-fdgrv      IS NOT INITIAL THEN abap_true ELSE abap_false ).

        "--->WTAX_TYPE
        LOOP AT t_pstprovrnt INTO gs_pstprovrnt
                    WHERE code    = lv_bp AND
                          empresa  = gs_provedor-empresa.

          ls_company_wax_type-data_key-witht = gs_pstprovrnt-impuesto.
          ls_company_wax_type-data-wt_withcd = '01'.
          ls_company_wax_type-data-wt_subjct = abap_true.
*          ls_company_wax_type-data-wt_exdf = '20000101'.
*          ls_company_wax_type-data-wt_exdt = '99991231'.

*          ls_company_wax_type-datax-wt_exdt = abap_true.
*          ls_company_wax_type-datax-wt_exdf = abap_true.
*          ls_company_wax_type-data-QSREC     "Se queda comentado para su futura implementacion
          ls_company_wax_type-datax-wt_subjct = abap_true.
*          ls_company_wax_type-datax-QSREC     "Se queda comentado para su futura implementacion
          ls_company_wax_type-datax-wt_withcd = abap_true.

          APPEND ls_company_wax_type TO ls_company_wax_type_s-wtax_type.

        ENDLOOP.
        ls_company-wtax_type = ls_company_wax_type_s.
        FREE ls_company_wax_type_s.


        APPEND ls_company TO lt_company.
        ls_data-vendor-company_data-company = lt_company.

        "---> PURCHASING
** Se incluye validacion por t024E
        SELECT SINGLE ekorg FROM t024e
          INTO @DATA(lv_ekorg_aux1)
          WHERE ekorg EQ @gs_provedor-empresa.
        IF sy-subrc EQ 0 AND lv_ekorg_aux1 NE '100'.

          ls_purchasing-data_key-ekorg  = gs_provedor-empresa.
          ls_purchasing-data-waers      = gs_provedor-mon_pedid.
          ls_purchasing-data-incov      = gs_provedor-incoterm.

          "Agregar Cond pago - Vista Proveedor(Compras)
          ls_purchasing-data-zterm = gs_provedor-condpago.
          ls_purchasing-datax-zterm = COND #( WHEN ls_purchasing-data-zterm IS NOT INITIAL THEN abap_true ELSE abap_false ).

          ls_purchasing-datax-waers     = COND #( WHEN ls_purchasing-data-waers IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_purchasing-datax-incov     = COND #( WHEN ls_purchasing-data-incov IS NOT INITIAL THEN abap_true ELSE abap_false ).

** INI DNAVOA
          "Se añade a El mismo como interlocutor
          ls_func_prov-task = 'I'. "DNAVOA
          ls_func_prov-data_key-parvw  = 'RS'.
          ls_func_prov-data_key-parza  += 1.
          ls_func_prov-data-partner    = gs_provedor-code.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_func_prov-data-partner
            IMPORTING
              output = ls_func_prov-data-partner.

          ls_func_prov-datax-partner   = COND #( WHEN ls_func_prov-data-partner IS NOT INITIAL THEN abap_true ELSE abap_false ).

          APPEND ls_func_prov TO lt_func_prov.
          CLEAR: ls_func_prov-task, ls_func_prov-data_key-werks, ls_func_prov-data_key-parvw,
                 ls_func_prov-data-partner.
** FIN DNAVOA

          SELECT * FROM tvarvc "DNAVOA
            INTO TABLE @DATA(lt_tvarvc_aux1)
            WHERE name EQ 'ZBPTRANSPORTE'.
          IF sy-subrc EQ 0 .
            READ TABLE lt_tvarvc_aux1 TRANSPORTING NO FIELDS WITH KEY low = ls_company-data-fdgrv.
            IF sy-subrc EQ 0.
              "Se agrega Rol CRM010
              ls_roles-data-rolecategory = 'CRM010'.
              APPEND ls_roles TO ls_data-partner-central_data-role-roles.
            ENDIF.
          ENDIF.


          ls_purchasing-functions-functions = lt_func_prov.
          FREE lt_func_prov.

          APPEND ls_purchasing TO lt_purchasing.

        ENDIF.

      ENDLOOP.

** INI DNAVOA 13.06.2025

      DELETE ADJACENT DUPLICATES FROM ls_data-partner-central_data-role-roles.

      SELECT * FROM tvarvc
        INTO TABLE @DATA(lt_tvarvc)
        WHERE name EQ 'ZBP_EMP_ESPJO'.
      IF sy-subrc EQ 0.

        LOOP AT lt_purchasing INTO DATA(ls_purchasing).
          READ TABLE lt_tvarvc INTO DATA(ls_tvarvc_aux) WITH KEY low = ls_purchasing-data_key-ekorg.
          IF sy-subrc EQ 0.
            ls_purchasing-data_key-ekorg = ls_tvarvc_aux-high.
            APPEND ls_purchasing TO lt_purchasing.
          ENDIF.
        ENDLOOP.

      ENDIF.

** FIN DNAVOA 13.06.2025
      IF lt_purchasing[] IS NOT INITIAL.
        ls_data-vendor-purchasing_data-purchasing = lt_purchasing.
      ENDIF.
*      ENDIF.

    ENDIF.

    REFRESH lt_data.
    APPEND ls_data TO lt_data.

    ls_data_deep-lt_data = lt_data.

    APPEND ls_data_deep TO lt_data_deep.
    CLEAR: ls_data_deep,lv_bp.

    CLEAR ls_data.

    CALL METHOD cl_md_bp_maintain=>maintain
      EXPORTING
        i_data     = lt_data
        i_test_run = chk_test
      IMPORTING
        e_return   = t_return.


    IF t_return IS NOT INITIAL.
      CLEAR gs_return.
      READ TABLE t_return INTO gs_return INDEX 1.
      IF sy-subrc EQ 0.

        PERFORM carga_alv
         TABLES gs_return-object_msg
          USING gs_return-object_key
                lv_index cx-atbps"--> Alta de BPS
       CHANGING lv_tipomsje.

      ENDIF.

    ELSE.

      IF chk_test IS INITIAL.
        gv_mess  = 'BP Creado'.
      ELSE.
        gv_mess  = 'BP Listo para Creación'.
      ENDIF.

      gs_mess = VALUE #( type    = 'S'
                         message = gv_mess
                      ).
      APPEND gs_mess TO gt_mess.

      t_return = VALUE #( ( object_idx = 1
                            object_key = gs_gnrales-code
                            object_msg = gt_mess
                        ) ).
      CLEAR gv_mess.

      READ TABLE t_return INTO gs_return INDEX 1.
      IF sy-subrc EQ 0.

        PERFORM carga_alv
         TABLES gs_return-object_msg
          USING gs_return-object_key
                lv_index cx-atbps"--> Alta de BPS
       CHANGING lv_tipomsje.

      ENDIF.
    ENDIF.

  ENDLOOP.

  IF lv_bp_d IS NOT INITIAL.

    TRY.

        " Rutina para crear direcciones de Entrega
        PERFORM f_crear_direcciones_entrega TABLES t_gnrales "Se cambia para ajuste con estructura deep
                                            USING  chk_test.

      CATCH cx_root INTO DATA(lx).

        WRITE: 'Ocurrieron errores en el procesamiento, favor de correr el programa en prueba y corregirlos'.
        EXIT.
    ENDTRY.

  ENDIF.

  IF chk_test IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

** INI DNAVOA
  IF chk_test IS INITIAL.

    "Loop a estructura deep con los registros
    LOOP AT lt_data_deep INTO DATA(ls_data_deep).

      READ TABLE ls_data_deep-lt_data ASSIGNING FIELD-SYMBOL(<fs_data>) INDEX 1.
      CLEAR: ls_data.

      ls_data-partner-header-object_task = 'U'.
      ls_data-partner-header-object = <fs_data>-partner-header-object_instance-bpartner.
      ls_data-partner-header-object_instance-bpartner = <fs_data>-partner-header-object_instance-bpartner.
      ls_data-partner-header-object_instance-bpartnerguid = <fs_data>-partner-header-object_instance-bpartner.

      ls_data-customer-header-object_instance-kunnr = <fs_data>-partner-header-object_instance-bpartner.
      ls_data-customer-header-object_task = 'U'.

** Funciones de interlocutor cliente

      IF <fs_data>-customer-sales_data-sales[] IS NOT INITIAL AND ls_data_deep-lt_data_d[] IS NOT INITIAL.

        DATA(lv_data_flag) = abap_true.

        ls_data-customer-sales_data-sales[] = <fs_data>-customer-sales_data-sales[].

        LOOP AT <fs_data>-customer-sales_data-sales ASSIGNING FIELD-SYMBOL(<fs_sales>).
          DATA(lv_tabix) = sy-tabix.
          READ TABLE ls_data-customer-sales_data-sales ASSIGNING FIELD-SYMBOL(<fs_sales_aux>) INDEX lv_tabix.
          IF sy-subrc EQ 0.
            <fs_sales_aux>-task = 'U'.
            CLEAR: <fs_sales_aux>-functions, <fs_sales_aux>-data, <fs_sales_aux>-datax.
          ENDIF.

          DATA(lv_first) = abap_false.
          CLEAR: lt_functions[], lv_parza.
          LOOP AT ls_data_deep-lt_data_d INTO DATA(ls_data_dp_aux).
            AT FIRST.
              lv_first = abap_true.
            ENDAT.

            READ TABLE ls_data_dp_aux-lt_direcc_d INTO DATA(ls_data_d) INDEX 1.
            IF sy-subrc EQ 0.

              lv_bp = <fs_data>-partner-header-object_instance-bpartner.
              SHIFT lv_bp LEFT DELETING LEADING '0'.

              READ TABLE t_pstempresa
                WITH KEY code     = lv_bp
                         namecrto = ls_data_d-partner-central_data-common-data-bp_centraldata-searchterm1
                   INTO DATA(ls_empresa_aux).
              IF sy-subrc EQ 0.

                IF ls_empresa_aux-orgventa NE <fs_sales>-data_key-vkorg.
                  CONTINUE.
                ENDIF.

                CLEAR: lt_functions_aux[].
                IF lv_first EQ abap_true.
                  lt_functions_aux[] = <fs_sales>-functions-functions[].
                  DELETE lt_functions_aux WHERE data_key-parvw NE 'WE'.
                ELSE.
                  lt_functions[] = <fs_sales_aux>-functions-functions[].
                ENDIF.

                ls_functions-task = 'I'.
                ls_functions-data_key-parvw = 'WE'.

                SELECT * FROM knvp
                  INTO TABLE @lt_parza
                  WHERE kunnr EQ @<fs_data>-partner-header-object_instance-bpartner
                  AND   vkorg EQ @<fs_sales>-data_key-vkorg
                  AND   parvw EQ 'WE'.
                IF sy-subrc EQ 0 AND lv_first EQ  abap_true.

                  SORT lt_parza BY parza ASCENDING.
                  READ TABLE lt_parza INTO ls_parza INDEX 1.
                  IF sy-subrc EQ 0.
                    lv_parza = ls_parza-parza + 1.
                  ENDIF.

                ELSEIF sy-subrc NE 0 AND lv_first EQ  abap_true.

                  DESCRIBE TABLE lt_functions_aux LINES DATA(ls_lines_funct).
                  READ TABLE lt_functions_aux INTO DATA(ls_functions_aux1) INDEX ls_lines_funct.
                  IF sy-subrc EQ 0 AND ls_functions_aux1-data_key-parvw EQ 'WE'.
                    ls_functions-data_key-parza = ls_functions_aux1-data_key-parza + 1.
                  ELSE.
                    lv_parza += 1.
                  ENDIF.

                  lv_first = abap_false.

                ELSE.

                  lv_parza += 1.

                ENDIF.

                ls_functions-data_key-parza = lv_parza.
                ls_functions-data-partner   = ls_data_d-partner-header-object_instance-bpartner.
                ls_functions-datax-partner  = COND #( WHEN ls_functions-data-partner IS NOT INITIAL THEN abap_true ELSE abap_false ).

                APPEND ls_functions TO lt_functions.

                <fs_sales_aux>-functions-functions = lt_functions.
                DELETE <fs_sales_aux>-functions-functions WHERE task = ''.

              ENDIF.

            ENDIF.

          ENDLOOP.

        ENDLOOP.

      ENDIF.

*** Funciones de interlocutor de proveedor
      IF t_pstprovint IS NOT INITIAL.
        CLEAR: lt_purchasing[].

        DATA(lv_code_aux) = |{ <fs_data>-partner-header-object_instance-bpartner ALPHA = OUT }|.

        READ TABLE t_pstprovint TRANSPORTING NO FIELDS WITH KEY code_minor = lv_code_aux.
        IF sy-subrc EQ 0.
          lv_data_flag = abap_true.


          ls_data-vendor-header-object_instance-lifnr     = <fs_data>-partner-header-object_instance-bpartner.
          ls_data-vendor-header-object_task               = 'U'. "Modificar

          CLEAR gs_pstprovint.
          "--->FUNCTIONS interlocutor
          lv_bp = <fs_data>-partner-header-object_instance-bpartner.
          CLEAR: ls_purchasing.
          lv_parza = 1.
          LOOP AT <fs_data>-vendor-purchasing_data-purchasing ASSIGNING FIELD-SYMBOL(<fs_purchasing>).

            ls_purchasing-task = 'U'.
            ls_purchasing-data_key-ekorg = <fs_purchasing>-data_key-ekorg.

            CLEAR: lv_mino.
            lv_mino = |{ <fs_data>-partner-header-object_instance-bpartner ALPHA = OUT }|.

            LOOP AT t_pstprovint INTO gs_pstprovint
                WHERE code_minor EQ lv_mino
                AND   empresa    EQ <fs_purchasing>-data_key-ekorg.

              ls_func_prov-task = 'I'. "DNAVOA
              ls_func_prov-data_key-werks  = gs_pstprovint-centro.
              ls_func_prov-data_key-parvw  = gs_pstprovint-tipo_inter.
*            ls_func_prov-data_key-parza  += 1.
              ls_func_prov-data-partner    = gs_pstprovint-interlcutr.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = ls_func_prov-data-partner
                IMPORTING
                  output = ls_func_prov-data-partner.

              ls_func_prov-datax-partner   = COND #( WHEN ls_func_prov-data-partner IS NOT INITIAL THEN abap_true ELSE abap_false ).

              "Validacion de asignacion campo PARZA
              SELECT * FROM wyt3
                INTO TABLE @lt_parza
                WHERE lifnr EQ @<fs_data>-partner-header-object_instance-bpartner
                AND   ekorg EQ @<fs_purchasing>-data_key-ekorg
                AND   parvw EQ 'RS'.
              IF sy-subrc EQ 0.

                SORT lt_parza BY parza ASCENDING.
                READ TABLE lt_parza INTO ls_parza INDEX 1.
                IF sy-subrc EQ 0.
                  lv_parza = ls_parza-parza + 1.
                ENDIF.

              ELSE.

                lv_parza += 1.

              ENDIF.

              ls_func_prov-data_key-parza = lv_parza.

              APPEND ls_func_prov TO lt_func_prov.

            ENDLOOP.

            IF lt_func_prov[] IS NOT INITIAL.

              ls_purchasing-functions-functions = lt_func_prov.
              FREE lt_func_prov.

              APPEND ls_purchasing TO lt_purchasing.

            ENDIF.

          ENDLOOP.
          ls_data-vendor-purchasing_data-purchasing = lt_purchasing.
        ENDIF.
      ENDIF.

      IF ls_data IS NOT INITIAL AND lv_data_flag IS NOT INITIAL.

        CLEAR: t_return[], lt_data[], lv_data_flag.
        APPEND ls_data TO lt_data.

        CALL METHOD cl_md_bp_maintain=>maintain
          EXPORTING
            i_data     = lt_data
            i_test_run = chk_test
          IMPORTING
            e_return   = t_return.

        IF t_return IS NOT INITIAL.
          CLEAR gs_return.
          READ TABLE t_return INTO gs_return INDEX 1.
          IF sy-subrc EQ 0.

            PERFORM carga_alv
             TABLES gs_return-object_msg
              USING gs_return-object_key
                    lv_index cx-atbps"--> Alta de BPS
           CHANGING lv_tipomsje.

          ENDIF.

        ELSE.

          IF chk_test IS INITIAL.
            gv_mess  = 'BP Creado'.
          ELSE.
            gv_mess  = 'BP Listo para Creación'.
          ENDIF.

          gs_mess = VALUE #( type    = 'S'
                             message = gv_mess
                          ).
          APPEND gs_mess TO gt_mess.

          t_return = VALUE #( ( object_idx = 1
                                object_key = gs_gnrales-code
                                object_msg = gt_mess
                            ) ).
          CLEAR gv_mess.

          READ TABLE t_return INTO gs_return INDEX 1.
          IF sy-subrc EQ 0.

            PERFORM carga_alv
             TABLES gs_return-object_msg
              USING gs_return-object_key
                    lv_index cx-atbps"--> Alta de BPS
           CHANGING lv_tipomsje.

          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.


** INI DNAVOA 13.06.2025

    PERFORM f_anexos.

    LOOP AT t_gnrales INTO gs_gnrales.
      PERFORM f_herramientas_prov       USING gs_gnrales-code.
    ENDLOOP.

** FIN DNAVOA 13.06.2025
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

** FIN DNAVOA

  " ---> Log Principal
  CALL SCREEN 001.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form mantenimiento_vmap
*&---------------------------------------------------------------------*
*& Se ejecuta una actualizacion de la tabla que se actualiza desde la
*& vista de mantenimiento en la transaccion /n/aif/vmap
*&---------------------------------------------------------------------*
FORM mantenimiento_vmap
  USING it_index TYPE i.

  DATA: ls_mvmap TYPE /aif/t_mvmapval,
        lv_kunnr TYPE char10.

  READ TABLE t_gnrales INTO DATA(ls_gnrl_aux1) INDEX it_index.
  IF sy-subrc EQ 0 AND ls_gnrl_aux1-regimen IS NOT INITIAL.

    CLEAR: ls_mvmap, lv_kunnr.
    "valiacion de existencia
    SELECT * FROM /aif/t_mvmapval
      INTO TABLE @DATA(lt_regimen)
      WHERE       ns  EQ '/EDOMX'
      AND   vmapname  EQ 'RECEIVER_TAX_REGIME'.
    IF sy-subrc EQ 0.

      DESCRIBE TABLE lt_regimen LINES DATA(lv_lines_regimen).

      ls_mvmap-ns        = '/EDOMX'.
      ls_mvmap-vmapname  = 'RECEIVER_TAX_REGIME'.
      ls_mvmap-vmapvalnr = lv_lines_regimen + 1.
      ls_mvmap-ext_value = ls_gnrl_aux1-regimen.
      lv_kunnr = |{ ls_gnrl_aux1-code ALPHA = IN }|.
      ls_mvmap-int_value = lv_kunnr.

      READ TABLE lt_regimen INTO DATA(ls_regimen_aux1)
        WITH KEY
          ns = ls_mvmap-ns
          vmapname  = ls_mvmap-vmapname
          int_value = ls_mvmap-int_value.
      IF sy-subrc EQ 0.
        ls_mvmap-vmapvalnr = ls_regimen_aux1-vmapvalnr.
      ENDIF.

      MODIFY /aif/t_mvmapval FROM ls_mvmap.

    ENDIF.

  ENDIF.

  "EL COMMIT SE EJECUTA UNA VEZ TERMINADO EL PROCESO
  "EN CASO DE NO COMPLETARSE SE EJECUTA UN ROLLBACK
ENDFORM.
*&---------------------------------------------------------------------*
*& Form modificar_bps
*&---------------------------------------------------------------------*
*& Modificar BPS por Ejecucion de Clase Estandar
*&---------------------------------------------------------------------*
FORM modificar_bps .


  " Data Local
  DATA lv_tiposat TYPE c.
  DATA lv_rolecat TYPE c LENGTH 6.
  DATA: ls_object_msg TYPE bapiretc.
  DATA ls_com     TYPE cmds_ei_company.
  DATA lt_com     TYPE cmds_ei_company_t.
  DATA ls_roles   TYPE bus_ei_bupa_roles.
  DATA lt_roles   TYPE bus_ei_bupa_roles_t.
  DATA ls_data    TYPE cvis_ei_extern.

  DATA lt_data    TYPE TABLE OF cvis_ei_extern.

  DATA ls_address TYPE bus_ei_bupa_address.
  DATA lt_address TYPE bus_ei_bupa_address_t.
  DATA ls_tax     TYPE bus_ei_bupa_taxnumber.
  DATA lt_tax     TYPE bus_ei_bupa_taxnumber_t.
  DATA ls_bank    TYPE bus_ei_bupa_bankdetail.
  DATA lt_bank    TYPE bus_ei_bupa_bankdetail_t.

  DATA ls_seg     TYPE ukm_ei_bp_cms_sgm.
  DATA lt_seg     TYPE ukmt_ei_bp_cms_sgm.

  DATA lt_return  TYPE bapiretm.
  DATA ls_return  TYPE bapireti.
  DATA ls_message TYPE bapiret2.

  DATA lv_index   TYPE i.
  DATA lv_nombre  TYPE c LENGTH 30.

  DATA lv_tipomsje TYPE c.
  DATA lv_validate TYPE c.
  DATA lv_uk_tabix TYPE sy-tabix.

  CONSTANTS lc_group(3) TYPE c VALUE 'ZDM'.
  CONSTANTS lc_z01(3)   TYPE c VALUE 'Z01'.
  CONSTANTS lc_fisi(6)  TYPE c VALUE 'Física'.
  CONSTANTS lc_juri(8)  TYPE c VALUE 'Jurídico'.

  CONSTANTS lc_s     TYPE c VALUE 'S'.
  CONSTANTS lc_e     TYPE c VALUE 'E'.
  CONSTANTS lc_1     TYPE c VALUE '1'.
  CONSTANTS lc_2     TYPE c VALUE '2'.
  CONSTANTS lc_crear TYPE c VALUE 'U'. "DNAVOA 05.06.2025

  CONSTANTS lc_crm002(6) TYPE c VALUE 'CRM002'.
  CONSTANTS lc_flcu01(6) TYPE c VALUE 'FLCU01'.
*&-------------------------------------------------------

  LOOP AT t_gnrales INTO gs_gnrales.

    lv_bp = gs_gnrales-code.

    CLEAR lv_index.
    lv_index = sy-tabix.

    " ---> HEADERC / CENTRAL DATA  ---
    " --> Tipo de Categoria

    CLEAR lv_tiposat.
    CASE gs_gnrales-tipo.
      WHEN lc_juri.
        lv_tiposat = lc_2.
      WHEN lc_fisi.
        lv_tiposat = lc_1.
    ENDCASE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_gnrales-code
      IMPORTING
        output = gs_gnrales-code.

    " Dejar en blanco si será generado por SAP
    ls_data-partner-header-object_task                                  = 'U'.
    ls_data-partner-header-object_instance-bpartner                     = space.
    ls_data-partner-header-object_instance-bpartnerguid                 = gs_gnrales-code.
    "Validacion de actualizacion
    IF gs_gnrales-tipo IS NOT INITIAL AND gs_gnrales-grp_bp IS NOT INITIAL.

      ls_data-partner-central_data-common-data-bp_control-category        = lv_tiposat.
      ls_data-partner-central_data-common-data-bp_control-grouping        = gs_gnrales-grp_bp.
      ls_data-partner-central_data-common-data-bp_centraldata-searchterm1 = gs_gnrales-namect.

      CLEAR lv_rolecat.
      CASE lc_1.
        WHEN 1.

          lv_rolecat = lc_flcu01.

          CLEAR lv_nombre.
          CONCATENATE gs_gnrales-name1 "gs_gnrales-name2
                      gs_gnrales-name2 gs_gnrales-aped2
          INTO lv_nombre SEPARATED BY space.

          ls_data-partner-central_data-common-data-bp_person-fullname   = lv_nombre.
          ls_data-partner-central_data-common-data-bp_person-firstname  = gs_gnrales-name1.
          ls_data-partner-central_data-common-data-bp_person-secondname = gs_gnrales-name2.
          ls_data-partner-central_data-common-data-bp_person-lastname   = gs_gnrales-aped1.
          ls_data-partner-central_data-common-data-bp_person-middlename = gs_gnrales-aped2.

          ls_data-partner-central_data-common-datax-bp_person-fullname              = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-fullname IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_data-partner-central_data-common-datax-bp_person-firstname             = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-firstname IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_data-partner-central_data-common-datax-bp_person-secondname            = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-secondname  IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_data-partner-central_data-common-datax-bp_person-lastname              = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-lastname IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_data-partner-central_data-common-datax-bp_person-middlename            = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-middlename IS NOT INITIAL THEN abap_true ELSE abap_false ).

        WHEN 2.
          lv_rolecat = lc_crm002.
          ls_data-partner-central_data-common-data-bp_organization-name1 = gs_gnrales-name1.
          ls_data-partner-central_data-common-datax-bp_organization-name1 = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name1 IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ENDCASE.

      " --> TAX
      ls_tax-data_key-taxtype  = gs_gnrales-tipo.
      ls_tax-data_key-taxnumber = gs_gnrales-id_rfc.
      REFRESH lt_tax.
      APPEND ls_tax TO lt_tax.
      CLEAR ls_tax.

      ls_data-partner-central_data-taxnumber-taxnumbers = lt_tax.

    ENDIF.

    " ---> Cliente o Proveedor
    CASE lv_rolecat.
      WHEN lc_flcu01. " Cliente

        CLEAR gs_cntefinan.
        READ TABLE t_cntefinan INTO gs_cntefinan
        WITH KEY code = gs_gnrales-code.
        IF sy-subrc EQ 0.

          ls_data-customer-header-object_instance-kunnr = gs_cntefinan-code.

          " --> Customer Company Data
          ls_com-data-zuawa     = lc_z01.
          ls_com-task           = 'M'.
          ls_com-data-fdgrv     = gs_cntefinan-grptesor. "gs_cntevntas-grpclte.
          ls_com-data-akont     = gs_cntefinan-cntasap.
          ls_com-data_key-bukrs = gs_cntefinan-id_empre.
          ls_com-data-zterm     = gs_cntefinan-condpago. "gs_cntevntas-condentg.

          ls_com-datax-zuawa    = COND #( WHEN ls_com-data-zuawa  IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_com-datax-zterm    = COND #( WHEN ls_com-data-zterm IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_com-datax-fdgrv    = COND #( WHEN ls_com-data-fdgrv IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_com-datax-akont    = COND #( WHEN ls_com-data-akont IS NOT INITIAL THEN abap_true ELSE abap_false ).
          APPEND ls_com TO lt_com.

          ls_data-customer-company_data-company = lt_com.

        ENDIF.

        "  ---> Cliente Ventas
        CLEAR gs_cntevntas.
        READ TABLE t_cntevntas INTO gs_cntevntas
        WITH KEY code = gs_gnrales-code.
        IF sy-subrc EQ 0.
          ls_seg-data_key-partner                             = gs_cntevntas-code.
          ls_data-partner-ukmbp_data-profile-data-check_rule  = gs_cntevntas-tipointer.
          ls_seg-data-credit_limit                            = gs_cntevntas-sector.
          ls_seg-data_key-credit_sgmnt                        = gs_cntevntas-sector.

*            ls_seg-data-                                  = gs_cntefinan-cntasap.
*            ls_data-partner-ukmbp_data-profile-data-limit_rule  = gs_cntevntas-.
*            ls_data-partner-ukmbp_data-profile-data-risk_class  = gs_cntevntas-.

          ls_data-partner-ukmbp_data-profile-datax-check_rule = COND #( WHEN ls_data-partner-ukmbp_data-profile-data-check_rule  IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_data-partner-ukmbp_data-profile-datax-limit_rule = COND #( WHEN ls_data-partner-ukmbp_data-profile-data-limit_rule  IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_data-partner-ukmbp_data-profile-datax-risk_class = COND #( WHEN ls_data-partner-ukmbp_data-profile-data-risk_class  IS NOT INITIAL THEN abap_true ELSE abap_false ).
          APPEND ls_seg TO lt_seg.

        ENDIF.

        ls_data-partner-ukmbp_data-segments-segments = lt_seg.

      WHEN lc_crm002. " Proveedor

        " --> BANK
        CLEAR gs_provedor.
        READ TABLE t_provedor INTO gs_provedor
        WITH KEY code = gs_gnrales-code.
        IF sy-subrc EQ 0.
          ls_bank-task = 'M'.
          ls_bank-data-bank_ctry     = gs_provedor-paisbank.
          ls_bank-data-bank_ctryiso  = gs_provedor-paisbank.
          ls_bank-data-bank_key      = gs_provedor-banco.
          ls_bank-data-bank_acct     = gs_provedor-cnta_bank.
          ls_bank-data-bankdetailvalidfrom = '20000101'. "Ajuste fecha validez banco

          ls_bank-datax-bankdetailvalidfrom = COND #( WHEN ls_bank-data-bankdetailvalidfrom IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_bank-datax-bank_ctry           = COND #( WHEN ls_bank-data-bank_ctry           IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_bank-datax-bank_ctryiso        = COND #( WHEN ls_bank-data-bank_ctryiso        IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_bank-datax-bank_key            = COND #( WHEN ls_bank-data-bank_key            IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_bank-datax-bank_acct           = COND #( WHEN ls_bank-data-bank_acct           IS NOT INITIAL THEN abap_true ELSE abap_false ).
          REFRESH lt_bank.
          APPEND ls_bank TO lt_bank.
          CLEAR ls_bank.

          ls_data-partner-central_data-bankdetail-bankdetails = lt_bank.

        ENDIF.

    ENDCASE.



    IF lv_rolecat IS NOT INITIAL.

      " --> Role
      ls_roles-data-rolecategory = lv_rolecat.
      APPEND ls_roles TO lt_roles.
      CLEAR ls_roles.

      REFRESH lt_roles.
      ls_data-partner-central_data-role-roles = lt_roles.

    ENDIF.

**** INI DNAVOA 06.05.2025
    PERFORM f_llenado_data_mod
      USING lv_tiposat lc_1 lc_2 lv_validate  lv_uk_tabix lc_crear lc_z01
      CHANGING ls_data.
**** FIN DNAVOA 06.05.2025

    REFRESH lt_data.
    APPEND ls_data TO lt_data.
    CLEAR ls_data.

*    BREAK-POINT.
    " ---> LLAMAMOS AL MÉTODO ---

    REFRESH t_return.

    CALL FUNCTION 'BUFFER_REFRESH_ALL'.

    CALL METHOD cl_md_bp_maintain=>maintain
      EXPORTING
        i_data     = lt_data
        i_test_run = chk_test
      IMPORTING
        e_return   = t_return.

    LOOP AT t_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      LOOP AT <ls_return>-object_msg ASSIGNING FIELD-SYMBOL(<ls_message>).
        IF <ls_message>-type = /shcm/cl_ee_sync_constants=>message_type_error OR
           <ls_message>-type = /shcm/cl_ee_sync_constants=>message_type_abort OR
           <ls_message>-type = /shcm/cl_ee_sync_constants=>message_type_exit  OR
           <ls_message>-type = /shcm/cl_ee_sync_constants=>message_type_info.
          DATA(lv_error) = abap_true.
        ENDIF.

*        MOVE-CORRESPONDING <ls_message> TO ls_message.
*        APPEND ls_message TO et_messages.

      ENDLOOP.  "ls_result-object_msg
    ENDLOOP.  "lt_return

    IF lv_error IS NOT INITIAL.
*    IF t_return IS NOT INITIAL.
      CLEAR lv_error.
      CLEAR gs_return.
      READ TABLE t_return INTO gs_return INDEX 1.
      IF sy-subrc EQ 0.

        PERFORM carga_alv
         TABLES gs_return-object_msg
          USING gs_return-object_key
                lv_index cx-atbps"--> Alta de BPS
       CHANGING lv_tipomsje.

      ENDIF.

** INI DNAVOA 06.05.2025
    ELSE.

*      COMMIT WORK AND WAIT.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      CLEAR gs_return.

      ls_object_msg-type = 'S'.
      ls_object_msg-message = 'BP acturalizado con éxito'.
      gs_return-object_key = gs_gnrales-code.

      APPEND ls_object_msg TO gs_return-object_msg.

      PERFORM carga_alv
        TABLES gs_return-object_msg
          USING gs_return-object_key
                lv_index cx-atbps"--> Alta de BPS
       CHANGING lv_tipomsje.
*** FIN DNAVOA 06.05.2025
      CALL FUNCTION 'BUFFER_REFRESH_ALL'.
    ENDIF.

  ENDLOOP.

  IF chk_test IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

  " ---> Log Principal
  CALL SCREEN 001.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_llenado_data_mod
*&---------------------------------------------------------------------*
*& agrega la informacion de las pestañas para la opcion de modificacion
*&---------------------------------------------------------------------*
FORM f_llenado_data_mod
  USING lv_tiposat  TYPE c
        lc_1        TYPE c
        lc_2        TYPE c
        lv_validate TYPE c
        lv_uk_tabix TYPE sy-tabix
        lc_crear    TYPE c
        lc_z01      TYPE char3
  CHANGING ls_data TYPE cvis_ei_extern.

  DATA: lv_parza TYPE parza.

  IF gs_gnrales-tipo IS NOT INITIAL AND gs_gnrales-grp_bp IS NOT INITIAL.

    ls_phone-contact-data-telephone                                             = gs_gnrales-telef.
    APPEND ls_phone TO lt_phone.
    ls_data-partner-central_data-communication-phone-phone                      = lt_phone.

    IF gs_gnrales-email1 IS NOT INITIAL.
      ls_email-contact-data-e_mail                                              = gs_gnrales-email1.
      APPEND ls_email TO lt_email. CLEAR ls_email.
    ENDIF.

    IF gs_gnrales-email2 IS NOT INITIAL.
      ls_email-contact-data-e_mail                                              = gs_gnrales-email2.
      APPEND ls_email TO lt_email. CLEAR ls_email.
    ENDIF.

    IF gs_gnrales-email3 IS NOT INITIAL.
      ls_email-contact-data-e_mail                                              = gs_gnrales-email3.
      APPEND ls_email TO lt_email. CLEAR ls_email.
    ENDIF.

    IF gs_gnrales-email4 IS NOT INITIAL.
      ls_email-contact-data-e_mail                                              = gs_gnrales-email4.
      APPEND ls_email TO lt_email. CLEAR ls_email.
    ENDIF.
    ls_data-partner-central_data-communication-smtp-smtp                        = lt_email.

    IF lv_tiposat EQ lc_1.
      ls_data-partner-central_data-common-data-bp_centraldata-partnerlanguageiso = gs_gnrales-idioma.
    ENDIF.

    ls_data-partner-header-object_instance-bpartner                             = gs_gnrales-code.
    ls_data-partner-header-object_instance-bpartnerguid                         = gs_gnrales-code.

    ls_data-partner-central_data-common-data-bp_control-category                = lv_tiposat.

    "ESTO ES SI SÓLO ES PERSONA (CATEGORY 1)
    IF lv_tiposat EQ lc_1.

      ls_data-partner-central_data-common-data-bp_person-firstname             = gs_gnrales-name1.
      ls_data-partner-central_data-common-data-bp_person-secondname            = gs_gnrales-aped2.
      ls_data-partner-central_data-common-data-bp_person-lastname              = gs_gnrales-aped1.
      ls_data-partner-central_data-common-data-bp_person-middlename            = gs_gnrales-name2.
      ls_data-partner-central_data-common-data-bp_person-fullname              = |{ gs_gnrales-aped1 } { gs_gnrales-aped2 } { gs_gnrales-name1 } { gs_gnrales-name2 }|.
      ls_data-partner-central_data-common-data-bp_person-namcountry            = gs_gnrales-pais.
      ls_data-partner-central_data-common-data-bp_person-namcountryiso         = gs_gnrales-pais.
      ls_data-partner-central_data-common-data-bp_person-correspondlanguageiso = gs_gnrales-idioma.

      ls_data-partner-central_data-common-datax-bp_person-firstname             = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-firstname IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-secondname            = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-secondname  IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-lastname              = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-lastname IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-middlename            = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-middlename IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-fullname              = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-fullname IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-namcountry            = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-namcountry IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-namcountryiso         = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-namcountryiso IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_data-partner-central_data-common-datax-bp_person-correspondlanguageiso = COND #( WHEN ls_data-partner-central_data-common-data-bp_person-correspondlanguageiso IS NOT INITIAL THEN abap_true ELSE abap_false ).

      "ESTO ES SI SÓLO ES ORGANIZACIÓN(CATEGORY 2)
    ELSE.


      DO.
        DATA(lv_indx) = sy-index.

        CASE lv_indx.
          WHEN 1.
            DATA(lv_long) = strlen( gs_gnrales-razon ).
            IF lv_long > 40.
              ls_data-partner-central_data-common-data-bp_organization-name1             = gs_gnrales-razon+0(40).
              ls_data-partner-central_data-common-datax-bp_organization-name1            = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name1 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              gs_gnrales-razon = gs_gnrales-razon+40.
            ELSE.
              ls_data-partner-central_data-common-data-bp_organization-name1             = gs_gnrales-razon.
              ls_data-partner-central_data-common-datax-bp_organization-name1            = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name1 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              EXIT.
            ENDIF.
          WHEN 2.
            lv_long = strlen( gs_gnrales-razon ).
            IF lv_long > 40.
              ls_data-partner-central_data-common-data-bp_organization-name2             = gs_gnrales-razon+0(40).
              ls_data-partner-central_data-common-datax-bp_organization-name2            = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name2 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              gs_gnrales-razon = gs_gnrales-razon+40.
            ELSE.
              ls_data-partner-central_data-common-data-bp_organization-name2             = gs_gnrales-razon.
              ls_data-partner-central_data-common-datax-bp_organization-name2            = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name2 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              EXIT.
            ENDIF.
          WHEN 3.
            lv_long = strlen( gs_gnrales-razon ).
            IF lv_long > 40.
              ls_data-partner-central_data-common-data-bp_organization-name3             = gs_gnrales-razon+0(40).
              ls_data-partner-central_data-common-datax-bp_organization-name3            = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name3 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              gs_gnrales-razon = gs_gnrales-razon+40.
            ELSE.
              ls_data-partner-central_data-common-data-bp_organization-name3             = gs_gnrales-razon.
              ls_data-partner-central_data-common-datax-bp_organization-name3            = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name3 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              EXIT.
            ENDIF.
          WHEN 4.
            lv_long = strlen( gs_gnrales-razon ).
            IF lv_long > 40.
              ls_data-partner-central_data-common-data-bp_organization-name4             = gs_gnrales-razon+0(40).
              ls_data-partner-central_data-common-datax-bp_organization-name4            = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name4 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              gs_gnrales-razon = gs_gnrales-razon+40.
            ELSE.
              ls_data-partner-central_data-common-data-bp_organization-name4             = gs_gnrales-razon.
              ls_data-partner-central_data-common-datax-bp_organization-name4            = COND #( WHEN ls_data-partner-central_data-common-data-bp_organization-name4 IS NOT INITIAL THEN abap_true ELSE abap_false ).
              EXIT.
            ENDIF.
          WHEN OTHERS.
            EXIT.
        ENDCASE.

      ENDDO.


    ENDIF.

  ENDIF.

  "--> ROLES
  REFRESH lt_roles.

  CLEAR gs_pstcliente.
  READ TABLE t_pstcliente INTO gs_pstcliente
  WITH KEY code = lv_bp.
  IF sy-subrc EQ 0.
    ls_roles-data-rolecategory = lc_flcu00.
    APPEND ls_roles TO lt_roles.
    CLEAR ls_roles.

    ls_roles-data-rolecategory = lc_flcu01.
    APPEND ls_roles TO lt_roles.
    CLEAR ls_roles.

    ls_roles-data-rolecategory = lc_ukm000.
    APPEND ls_roles TO lt_roles.
    CLEAR ls_roles.

    DATA(lv_flag) = abap_true.
  ENDIF.

  CLEAR gs_pstentrega.
  READ TABLE t_pstentrega INTO gs_pstentrega
  WITH KEY code = lv_bp.
  IF sy-subrc EQ 0.
    DATA(lv_bp_d) = abap_true.
  ENDIF.

  CLEAR gs_provedor.
  READ TABLE t_provedor INTO gs_provedor
  WITH KEY code = lv_bp.
  IF sy-subrc EQ 0.
    ls_roles-data-rolecategory = lc_flvn00.
    APPEND ls_roles TO lt_roles.
    CLEAR ls_roles.

    ls_roles-data-rolecategory = lc_flvn01.
    APPEND ls_roles TO lt_roles.
    CLEAR ls_roles.
  ENDIF.

  IF lt_roles[] IS NOT INITIAL.

    ls_data-partner-central_data-role-roles = lt_roles.
    SORT lt_roles BY data-rolecategory.

  ENDIF.

  "--> BANK DETAILS
  CLEAR gs_provedor.
*    READ TABLE t_provedor INTO gs_provedor
*    WITH KEY code = lv_bp.
*    IF sy-subrc EQ 0.

*
  CLEAR: lt_bank[], ls_bank, ls_data-partner-central_data-bankdetail-bankdetails[].

*
  LOOP AT t_provedor INTO gs_provedor
    WHERE banco IS NOT INITIAL
    AND   code = lv_bp.

    ls_bank-task = 'M'.
    ls_bank-data-bank_ctry     = gs_provedor-paisbank.
    ls_bank-data-bank_ctryiso  = gs_provedor-paisbank.
    ls_bank-data-bank_key      = gs_provedor-banco.
    ls_bank-data-bank_acct     = gs_provedor-cnta_bank.
    ls_bank-data-bank_ref      = gs_provedor-suc_bank.
    ls_bank-data-bankdetailvalidfrom = '20000101'. "Ajuste fecha validez banco
    ls_bank-data-bankdetailvalidto = '99991231'.

    ls_bank-datax-bankdetailvalidfrom = COND #( WHEN ls_bank-data-bankdetailvalidfrom IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_bank-datax-bankdetailvalidto   = COND #( WHEN ls_bank-data-bankdetailvalidto IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_bank-datax-bank_ctry           = COND #( WHEN ls_bank-data-bank_ctry IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_bank-datax-bank_ctryiso        = COND #( WHEN ls_bank-data-bank_ctryiso IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_bank-datax-bank_key            = COND #( WHEN ls_bank-data-bank_key IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_bank-datax-bank_acct           = COND #( WHEN ls_bank-data-bank_acct IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_bank-datax-bank_ref            = COND #( WHEN ls_bank-data-bank_ref IS NOT INITIAL THEN abap_true ELSE abap_false ).

    APPEND ls_bank TO lt_bank.
    CLEAR ls_bank.

    EXIT.

  ENDLOOP.

  IF lt_bank[] IS NOT INITIAL.
    ls_data-partner-central_data-bankdetail-bankdetails = lt_bank.
  ENDIF.

*  ENDIF.

  " --> TAXNUMBER
  IF gs_gnrales-tiperfc IS NOT INITIAL AND gs_gnrales-id_rfc IS NOT INITIAL.

    ls_tax-data_key-taxtype   = gs_gnrales-tiperfc.
    ls_tax-data_key-taxnumber = gs_gnrales-id_rfc.
    ls_data-vendor-central_data-central-data-j_1kftbus = gs_gnrales-tip_op. "DNAVOA
    ls_data-vendor-central_data-central-data-j_1kftind = gs_gnrales-tip_ind. "DNAVOA

    CASE lv_tiposat.
      WHEN lc_1.
        ls_data-partner-central_data-taxnumber-common-data-nat_person  = abap_true.
        ls_data-partner-central_data-taxnumber-common-datax-nat_person = COND #( WHEN ls_data-partner-central_data-taxnumber-common-data-nat_person IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ENDCASE.

    REFRESH lt_tax.
    APPEND ls_tax TO lt_tax.
    CLEAR ls_tax.

    ls_data-partner-central_data-taxnumber-taxnumbers   = lt_tax.

  ENDIF.

  "-->ADDRESSESS
  ls_address-data-postal-data-city         = gs_gnrales-munic.
  ls_address-data-postal-data-district     = gs_gnrales-colonia.
  ls_address-data-postal-data-postl_cod1   = gs_gnrales-c_p_.
  ls_address-data-postal-data-street       = gs_gnrales-calle.
  ls_address-data-postal-data-house_no     = gs_gnrales-numext.
  ls_address-data-postal-data-country      = gs_gnrales-pais.
  ls_address-data-postal-data-countryiso   = gs_gnrales-pais.
  ls_address-data-postal-data-region       = gs_gnrales-estado.

  CLEAR: lv_indx, lv_long.
  lv_long = strlen( gs_gnrales-calle ).
  IF lv_long > 60.

    gs_gnrales-calle = gs_gnrales-calle+60.

    DO.
      lv_indx = sy-index.

      CASE lv_indx.
        WHEN 1.
          lv_long = strlen( gs_gnrales-calle ).
          IF lv_long > 40.
            ls_address-data-postal-data-str_suppl3             = gs_gnrales-calle+0(40).
            ls_address-data-postal-datax-str_suppl3             = COND #( WHEN ls_address-data-postal-data-str_suppl3 IS NOT INITIAL THEN abap_true ELSE abap_false ).
            gs_gnrales-calle = gs_gnrales-calle+40.
          ELSE.
            ls_address-data-postal-data-str_suppl3             = gs_gnrales-calle.
            ls_address-data-postal-datax-str_suppl3             = COND #( WHEN ls_address-data-postal-data-str_suppl3 IS NOT INITIAL THEN abap_true ELSE abap_false ).
            EXIT.
          ENDIF.
        WHEN 2.
          lv_long = strlen( gs_gnrales-calle ).
          IF lv_long > 40.
            ls_address-data-postal-data-location             = gs_gnrales-calle+0(40).
            ls_address-data-postal-datax-location             = COND #( WHEN ls_address-data-postal-data-location IS NOT INITIAL THEN abap_true ELSE abap_false ).
            gs_gnrales-calle = gs_gnrales-calle+40.
          ELSE.
            ls_address-data-postal-data-location             = gs_gnrales-calle.
            ls_address-data-postal-datax-location             = COND #( WHEN ls_address-data-postal-data-location IS NOT INITIAL THEN abap_true ELSE abap_false ).
            EXIT.
          ENDIF.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

    ENDDO.

  ENDIF.

  IF  lv_tiposat EQ lc_2.
    CASE gs_gnrales-pais.
      WHEN 'MX'.
        ls_address-data-postal-data-langu    = ''.
        ls_address-data-postal-data-languiso  = 'ES'.
      WHEN OTHERS.
        ls_address-data-postal-data-langu    = ''.
        ls_address-data-postal-data-languiso = 'EN'.
    ENDCASE.
  ENDIF.

  ls_address-task = 'M'. "Modificar direcciones
  ls_address-data-postal-datax-city        = COND #( WHEN ls_address-data-postal-data-city       IS NOT INITIAL THEN abap_true ELSE abap_false ).
  ls_address-data-postal-datax-district    = COND #( WHEN ls_address-data-postal-data-district   IS NOT INITIAL THEN abap_true ELSE abap_false ).
  ls_address-data-postal-datax-postl_cod1  = COND #( WHEN ls_address-data-postal-data-postl_cod1 IS NOT INITIAL THEN abap_true ELSE abap_false ).
  ls_address-data-postal-datax-street      = COND #( WHEN ls_address-data-postal-data-street     IS NOT INITIAL THEN abap_true ELSE abap_false ).
  ls_address-data-postal-datax-house_no    = COND #( WHEN ls_address-data-postal-data-house_no   IS NOT INITIAL THEN abap_true ELSE abap_false ).
  ls_address-data-postal-datax-country     = COND #( WHEN ls_address-data-postal-data-country    IS NOT INITIAL THEN abap_true ELSE abap_false ).
  ls_address-data-postal-datax-countryiso  = COND #( WHEN ls_address-data-postal-data-countryiso IS NOT INITIAL THEN abap_true ELSE abap_false ).
  ls_address-data-postal-datax-region      = COND #( WHEN ls_address-data-postal-data-region     IS NOT INITIAL THEN abap_true ELSE abap_false ).

  IF  lv_tiposat EQ lc_2.
    ls_address-data-postal-datax-langu       = COND #( WHEN ls_address-data-postal-data-langu IS NOT INITIAL THEN abap_true ELSE abap_false ).
    ls_address-data-postal-datax-langu_iso   = COND #( WHEN ls_address-data-postal-data-languiso IS NOT INITIAL THEN abap_true ELSE abap_false ).
  ENDIF.

*  ls_data-partner-central_data-address-current_state = abap_true. "para que se modifiquen direcciones

*  APPEND ls_address TO ls_data-partner-central_data-address-addresses.


  " -->SOLO CUANDO EL ROL ES FLCU00 y FLCU01 Y UKM000, Ademas SÓLO SI ES CLIENTE Y TRAE CRÉDITO
  READ TABLE lt_roles WITH KEY data-rolecategory = lc_flcu00
  TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.

    READ TABLE lt_roles WITH KEY data-rolecategory = lc_flcu01
    TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      READ TABLE lt_roles WITH KEY data-rolecategory = lc_ukm000
      TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        lv_validate = abap_true.
      ENDIF.
    ENDIF.

  ENDIF.

  IF lv_validate IS NOT INITIAL.

    "---> UKMBP_DATA
    CLEAR: gs_cntefinan, lv_uk_tabix.
    LOOP AT t_cntefinan INTO gs_cntefinan
      WHERE code = lv_bp.
      lv_uk_tabix += 1.

      IF gs_cntefinan-typecred IS NOT INITIAL.

        IF lv_uk_tabix EQ 1.
          ls_data-partner-ukmbp_data-profile-data-check_rule  = '01'.
          ls_data-partner-ukmbp_data-profile-data-limit_rule  = 'B2C-EXIST'.

          CASE gs_cntefinan-typecred.
            WHEN 'Crédito'.
              ls_data-partner-ukmbp_data-profile-data-risk_class = 'D'.
            WHEN 'Preferencial'.
              ls_data-partner-ukmbp_data-profile-data-risk_class = 'A'.
            WHEN 'Contado'.
              ls_data-partner-ukmbp_data-profile-data-risk_class = 'F'.
            WHEN 'Anticipado'.
              ls_data-partner-ukmbp_data-profile-data-risk_class = 'E'.
            WHEN 'Contraentrega'.
              ls_data-partner-ukmbp_data-profile-data-risk_class = 'C'.
            WHEN 'Suspendido'.
              ls_data-partner-ukmbp_data-profile-data-risk_class = 'D'.
              ls_seg-data-xblocked = abap_true.
              ls_seg-datax-xblocked = COND #( WHEN ls_seg-data-xblocked IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ENDCASE.

          ls_data-partner-ukmbp_data-profile-datax-check_rule = COND #( WHEN ls_data-partner-ukmbp_data-profile-data-check_rule  IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_data-partner-ukmbp_data-profile-datax-limit_rule = COND #( WHEN ls_data-partner-ukmbp_data-profile-data-limit_rule  IS NOT INITIAL THEN abap_true ELSE abap_false ).
          ls_data-partner-ukmbp_data-profile-datax-risk_class = COND #( WHEN ls_data-partner-ukmbp_data-profile-data-risk_class  IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ENDIF.

        "--->     SEGMENTS
        ls_seg-task = 'M'.
        ls_seg-data_key-partner       = |{ gs_cntefinan-code ALPHA = IN }|.
        ls_seg-data_key-credit_sgmnt  = gs_cntefinan-id_empre.
        ls_seg-data-credit_limit      = gs_cntefinan-limitcred.
        ls_seg-data-limit_valid_date  = '99991231'.

        IF ls_seg-data-credit_limit IS INITIAL OR ls_seg-data-credit_limit EQ 0.
          ls_seg-data-x_limit_zero      = abap_true.

          ls_seg-datax-x_limit_zero      = COND #( WHEN ls_seg-data-x_limit_zero  IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ENDIF.

        ls_seg-datax-credit_limit     = COND #( WHEN ls_seg-data-credit_limit IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_seg-datax-limit_valid_date = COND #( WHEN ls_seg-data-limit_valid_date IS NOT INITIAL THEN abap_true ELSE abap_false ).
        APPEND ls_seg TO ls_data-partner-ukmbp_data-segments-segments.
        CLEAR ls_seg.
      ENDIF.
*      ENDIF.
    ENDLOOP.
  ENDIF.

  "SÓLO CUANDO ROL FLCU00 Y FLCU01 (CUSTOMER)
  CLEAR: gs_cntefinan, lv_validate.
  READ TABLE lt_roles WITH KEY data-rolecategory = lc_flcu00
  TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    READ TABLE lt_roles WITH KEY data-rolecategory = lc_flcu01
    TRANSPORTING NO FIELDS.
    IF sy-subrc  EQ 0.
      lv_validate = abap_true.
    ENDIF.
  ENDIF.

  IF lv_validate IS NOT INITIAL.


    LOOP AT t_cntefinan INTO gs_cntefinan
      WHERE code = lv_bp.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_cntefinan-code
        IMPORTING
          output = ls_data-customer-header-object_instance-kunnr.

      ls_data-customer-header-object_task           = 'U'."lc_crear.

      " -->     Customer - Company Data
      ls_com-task           = 'M'."lc_crear.
      ls_com-data_key-bukrs = gs_cntefinan-id_empre.
      ls_com-data-zuawa     = lc_z01.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_cntefinan-cntasap
        IMPORTING
          output = ls_com-data-akont.

      ls_com-data-zterm     = gs_cntefinan-condpago. "DNAVOA
      ls_com-data-fdgrv     = gs_cntefinan-grptesor. "DNAVOA


      ls_com-datax-zuawa    = COND #( WHEN ls_com-data-zuawa IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_com-datax-akont    = COND #( WHEN ls_com-data-akont IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_com-datax-zterm    = COND #( WHEN ls_com-data-zterm IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_com-datax-fdgrv    = COND #( WHEN ls_com-data-fdgrv IS NOT INITIAL THEN abap_true ELSE abap_false ).

      " --> WTAX_TYPE
      LOOP AT t_pstcntret INTO gs_pstcntret
                 WHERE code    = lv_bp AND
                       empresa = ls_com-data_key-bukrs.

        ls_wax_type-data_key-witht = gs_pstcntret-impuesto.

        ls_wax_type-data-wt_withcd = 'V1'.
        ls_wax_type-data-wt_agent  = abap_true.
        ls_wax_type-data-wt_agtdf  = sy-datum.
        ls_wax_type-data-wt_agtdt  = '99991231'.
        ls_wax_type-data-wt_exdf = '20000101'. "ajuste fecha inicio de validez
        ls_wax_type-data-wt_exdt = '99991231'. "ajuste fecha fin de validez

        ls_wax_type-datax-wt_exdf   = COND #( WHEN ls_wax_type-data-wt_exdf     IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_wax_type-datax-wt_exdt   = COND #( WHEN ls_wax_type-data-wt_exdt     IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_wax_type-datax-wt_withcd = COND #( WHEN ls_wax_type-data-wt_withcd   IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_wax_type-datax-wt_agent  = COND #( WHEN ls_wax_type-data-wt_agent    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_wax_type-datax-wt_agtdf  = COND #( WHEN ls_wax_type-data-wt_agtdf    IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_wax_type-datax-wt_agtdt  = COND #( WHEN ls_wax_type-data-wt_agtdt    IS NOT INITIAL THEN abap_true ELSE abap_false ).

        APPEND ls_wax_type TO ls_wax_type_s-wtax_type.

      ENDLOOP.
      ls_com-wtax_type = ls_wax_type_s.
      FREE ls_wax_type_s.
      APPEND ls_com TO lt_com.

    ENDLOOP.
    ls_data-customer-company_data-company = lt_com.

    " --> SALES
    CLEAR: lt_taxind[].
    LOOP AT t_pstcntevta INTO gs_pstcntevta
        WHERE code = lv_bp.
      CLEAR: ls_taxind, lv_parza, lt_taxind[], ls_sales-functions-functions, lt_functions[].

      ls_sales-task = 'M'.
      ls_sales-data_key-vkorg = gs_pstcntevta-orgvnta.
      ls_sales-data_key-vtweg = gs_pstcntevta-canal.
      ls_sales-data_key-spart = gs_pstcntevta-sector.
** INI DNAVOA 16.06.2025
      READ TABLE t_cntefinan INTO gs_cntefinan
        WITH KEY code = gs_pstcntevta-code
                 id_empre = gs_pstcntevta-orgvnta+1.
      ls_sales-data-zterm     = gs_cntefinan-condpago.

      ls_sales-data-kvgr4         = gs_gnrales-cfdifac.
      ls_sales-data-kvgr5         = gs_gnrales-cfdinrc.
** FIN DNAVOA 16.06.2025
      ls_sales-data-bzirk     = gs_pstcntevta-zonavnta.
      ls_sales-data-vkbur     = gs_pstcntevta-ofcventa.
      ls_sales-data-vkgrp     = gs_pstcntevta-grpovnta.
      ls_sales-data-klabc     = gs_pstcntevta-clas_abc.
      ls_sales-data-waers     = gs_pstcntevta-mone_ped.
      ls_sales-data-vsbed     = gs_pstcntevta-condentg.
      ls_sales-data-vwerk     = gs_pstcntevta-cedis.
      ls_sales-data-kurst     = 'M'.
      ls_sales-data-uebto     = gs_pstcntevta-tol_exce.

*** INI DNAVOA 16.06.2025
      SELECT incotermsversion FROM aincotermsv
        INTO TABLE @DATA(lt_ainco).
      IF sy-subrc EQ 0.
        DESCRIBE TABLE lt_ainco LINES DATA(lv_ainco_l).
        READ TABLE lt_ainco INTO DATA(ls_ainco) INDEX lv_ainco_l.
        IF sy-subrc EQ 0.
          ls_sales-data-incov = ls_ainco.
        ENDIF.
      ENDIF.
      ls_sales-data-inco1     = gs_pstcntevta-incoterm.
*** FIN DNAVOA 16.06.2025

      ls_sales-data-inco2_l   = gs_pstcntevta-locincot.
      ls_sales-data-kalks     = '1'.
      ls_sales-data-konda     = gs_pstcntevta-grpclte.

*** INI DNAVOA 06.13.2025
      ls_sales-data-kurst     = gs_pstcntevta-tip_cots.
      ls_sales-data-konda     = gs_pstcntevta-gp_prec.
      ls_sales-data-kalks     = gs_pstcntevta-esq_clte.
      ls_sales-data-vsbed     = gs_pstcntevta-cond_exp.
      ls_sales-data-ktgrd     = gs_pstcntevta-gpo_impt.
      ls_sales-data-kvgr3     = gs_pstcntevta-gpo_cli3.

      ls_sales-datax-kurst     = COND #( WHEN ls_sales-data-kurst    IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-konda     = COND #( WHEN ls_sales-data-konda    IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-kalks     = COND #( WHEN ls_sales-data-kalks    IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-vsbed     = COND #( WHEN ls_sales-data-vsbed    IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-ktgrd     = COND #( WHEN ls_sales-data-ktgrd    IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-kvgr3     = COND #( WHEN ls_sales-data-kvgr3    IS NOT INITIAL THEN abap_true ELSE abap_false ).

      ls_taxind-task           = 'M'.
      ls_taxind-data_key-aland = 'MX'.
      ls_taxind-data_key-tatyp = 'TMX1'.
      ls_taxind-data-taxkd     =  gs_pstcntevta-clas_fis.
      ls_taxind-datax-taxkd    = COND #( WHEN ls_taxind-data-taxkd       IS NOT INITIAL THEN abap_true ELSE abap_false ).
      APPEND ls_taxind TO lt_taxind.

      ls_taxind-data_key-tatyp = 'TMX2'.
      APPEND ls_taxind TO lt_taxind.

*** FIN DNAVOA 06.13.2025

      ls_sales-datax-zterm     = COND #( WHEN ls_sales-data-zterm      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-inco1     = COND #( WHEN ls_sales-data-inco1      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-bzirk     = COND #( WHEN ls_sales-data-bzirk      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-vkbur     = COND #( WHEN ls_sales-data-vkbur      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-vkgrp     = COND #( WHEN ls_sales-data-vkgrp      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-klabc     = COND #( WHEN ls_sales-data-klabc      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-waers     = COND #( WHEN ls_sales-data-waers      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-vsbed     = COND #( WHEN ls_sales-data-vsbed      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-vwerk     = COND #( WHEN ls_sales-data-vwerk      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-kurst     = COND #( WHEN ls_sales-data-kurst      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-uebto     = COND #( WHEN ls_sales-data-uebto      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-incov     = COND #( WHEN ls_sales-data-incov      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-inco2_l   = COND #( WHEN ls_sales-data-inco2_l    IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-kalks     = COND #( WHEN ls_sales-data-kalks      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_sales-datax-konda     = COND #( WHEN ls_sales-data-konda      IS NOT INITIAL THEN abap_true ELSE abap_false ).

      CLEAR gs_pstcnteint.

      APPEND ls_sales TO lt_sales.
    ENDLOOP.
    ls_data-customer-sales_data-sales = lt_sales.

    ls_data-customer-central_data-tax_ind-tax_ind = lt_taxind. "DNAVOA 13.06.2025

  ENDIF.


  "SOLO SI TRAE EL ROL FLVN00 Y FLVN01
  CLEAR: lv_validate.
  READ TABLE lt_roles WITH KEY data-rolecategory = lc_flvn00
  TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    READ TABLE lt_roles WITH KEY data-rolecategory = lc_flvn01
    TRANSPORTING NO FIELDS.
    IF sy-subrc  EQ 0.
      lv_validate = abap_true.
    ENDIF.
  ENDIF.

  IF lv_validate IS NOT INITIAL.
    ls_data-vendor-header-object_instance-lifnr     = gs_gnrales-code.
    ls_data-vendor-header-object_task               = 'U'."lc_crear.

    "---> Vendor - Company Data
    CLEAR: gs_provedor, gs_pstprovrnt, ls_func_prov-data_key-parza, ls_company_wax_type_s, ls_company_wax_type, lt_purchasing[], ls_purchasing, ls_data-vendor-company_data-company[], lt_company[].

    LOOP AT t_provedor INTO gs_provedor
        WHERE code = lv_bp.
**
      CLEAR: ls_company, ls_purchasing, ls_company_wax_type_s.
      ls_company-task = 'M'.
      ls_company-data_key-bukrs = gs_provedor-empresa.

      ls_company-data-zuawa     = lc_z01.
      ls_company-datax-zuawa     = abap_true.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_provedor-cntasaldo
        IMPORTING
          output = ls_company-data-akont.
      ls_company-datax-akont = COND #( WHEN ls_company-data-akont IS NOT INITIAL THEN abap_true ELSE abap_false ).

      ls_company-data-zterm     = gs_provedor-condpago.
      ls_company-datax-zterm     = COND #( WHEN ls_company-data-zterm      IS NOT INITIAL THEN abap_true ELSE abap_false ).

      ls_company-data-fdgrv     = gs_provedor-gpoteso.
      ls_company-datax-fdgrv     = COND #( WHEN ls_company-data-fdgrv      IS NOT INITIAL THEN abap_true ELSE abap_false ).

      "--->WTAX_TYPE
      LOOP AT t_pstprovrnt INTO gs_pstprovrnt
                  WHERE code    = lv_bp AND
                        empresa  = gs_provedor-empresa.
        CLEAR: ls_company_wax_type.

        ls_company_wax_type-task = 'M'.
        ls_company_wax_type-data_key-witht = gs_pstprovrnt-impuesto.
        ls_company_wax_type-data-wt_withcd = '01'.
        ls_company_wax_type-data-wt_subjct = abap_true.
*        ls_company_wax_type-data-wt_exdf = '20000101'. "ajuste fecha inicio de validez
*        ls_company_wax_type-data-wt_exdt = '99991231'. "ajuste fecha fin de validez
*
*        ls_company_wax_type-datax-wt_exdf = abap_true.
*        ls_company_wax_type-datax-wt_exdt = abap_true.
*          ls_company_wax_type-data-QSREC     "Se queda comentado para su futura implementacion
        ls_company_wax_type-datax-wt_subjct = abap_true.
*          ls_company_wax_type-datax-QSREC     "Se queda comentado para su futura implementacion
        ls_company_wax_type-datax-wt_withcd = abap_true.

        APPEND ls_company_wax_type TO ls_company_wax_type_s-wtax_type.

      ENDLOOP.
      ls_company-wtax_type = ls_company_wax_type_s.
      FREE ls_company_wax_type_s.


      APPEND ls_company TO lt_company.
      ls_data-vendor-company_data-company = lt_company.

      "---> PURCHASING
      SELECT SINGLE ekorg FROM t024e
        INTO @DATA(lv_ekorg_aux1)
        WHERE ekorg EQ @gs_provedor-empresa.
      IF sy-subrc EQ 0 . " AND lv_ekorg_aux1 NE '100'.

        ls_purchasing-task = 'M'.
        ls_purchasing-data_key-ekorg  = gs_provedor-empresa.
        ls_purchasing-data-waers      = gs_provedor-mon_pedid.
        ls_purchasing-data-incov      = gs_provedor-incoterm.

        "Agregar Cond pago - Vista Proveedor(Compras)
        ls_purchasing-data-zterm = gs_provedor-condpago.
        ls_purchasing-datax-zterm = COND #( WHEN ls_purchasing-data-zterm      IS NOT INITIAL THEN abap_true ELSE abap_false ).

        ls_purchasing-datax-waers     = COND #( WHEN ls_purchasing-data-waers IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ls_purchasing-datax-incov     = COND #( WHEN ls_purchasing-data-incov      IS NOT INITIAL THEN abap_true ELSE abap_false ).

        CLEAR: ls_func_prov-task, ls_func_prov-data_key-werks, ls_func_prov-data_key-parvw,
               ls_func_prov-data-partner.
** FIN DNAVOA

        SELECT * FROM tvarvc "DNAVOA
          INTO TABLE @DATA(lt_tvarvc_aux1)
          WHERE name EQ 'ZBPTRANSPORTE'.
        IF sy-subrc EQ 0 .
          READ TABLE lt_tvarvc_aux1 TRANSPORTING NO FIELDS WITH KEY low = ls_company-data-fdgrv.
          IF sy-subrc EQ 0.
            "Se agrega Rol CRM010
            ls_roles-data-rolecategory = 'CRM010'.
            APPEND ls_roles TO ls_data-partner-central_data-role-roles.
          ENDIF.
        ENDIF.


*        ls_purchasing-functions-functions = lt_func_prov.
        FREE lt_func_prov.

        APPEND ls_purchasing TO lt_purchasing.
      ENDIF.
    ENDLOOP.

** INI DNAVOA 13.06.2025

    DELETE ADJACENT DUPLICATES FROM ls_data-partner-central_data-role-roles.

    SELECT * FROM tvarvc
      INTO TABLE @DATA(lt_tvarvc)
      WHERE name EQ 'ZBP_EMP_ESPJO'.
    IF sy-subrc EQ 0.

      LOOP AT lt_purchasing INTO DATA(ls_purchasing).
        READ TABLE lt_tvarvc INTO DATA(ls_tvarvc_aux) WITH KEY low = ls_purchasing-data_key-ekorg.
        IF sy-subrc EQ 0.
          ls_purchasing-data_key-ekorg = ls_tvarvc_aux-high.
          APPEND ls_purchasing TO lt_purchasing.
        ENDIF.
      ENDLOOP.

    ENDIF.

** FIN DNAVOA 13.06.2025

    ls_data-vendor-purchasing_data-purchasing = lt_purchasing.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_anexos
*&---------------------------------------------------------------------*
*& Agrega la informacion de la pestaña de anexos al documento
*&---------------------------------------------------------------------*
FORM f_anexos.

  DATA: ls_url     TYPE sdokcntasc,
        pro_ls     TYPE sdokpropty,
        url        TYPE sdokcntascs,
        pro        TYPE sdokproptys,
        ls_bus_obj TYPE sibflporb,
        lt_loio    TYPE  skwf_io,
        lt_phio    TYPE  skwf_io,
        lt_error   TYPE  skwf_error.

  DATA ls_return   TYPE bapiretc.
  DATA lt_return   TYPE bapiretct.
  DATA lv_tipomsje TYPE c.
  DATA lv_index    TYPE i.
  DATA lv_anexo10  TYPE char10.

  DATA: lt_objcont     TYPE TABLE OF soli WITH HEADER LINE,
        lt_objhead     TYPE TABLE OF soli WITH HEADER LINE,

        lw_sood1       TYPE sood1,
        lw_folder      TYPE sofdk,
        lw_object      TYPE borident,
        lw_document_id TYPE sofmk,
        lw_reldoc      TYPE borident,

        lv_user        TYPE sy-uname,
        lv_leyenda     TYPE so_text255,
        lv_obj_id      TYPE soodk.

  CONSTANTS lc_s TYPE c VALUE 'S'.
  CONSTANTS lc_e TYPE c VALUE 'E'.

  CONSTANTS lc_exito TYPE c LENGTH 20 VALUE 'Carga Exitosa Anexo'.
  CONSTANTS lc_error TYPE c LENGTH 20 VALUE 'Carga Erronea Anexo'.

  WAIT UP TO 1 SECONDS.

  CLEAR: gs_anexo.
  LOOP AT t_anexo INTO gs_anexo.

    CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
      EXPORTING
        region    = 'B'
      IMPORTING
        folder_id = lw_folder
      EXCEPTIONS
        OTHERS    = 1.

    lw_sood1-objla     = sy-langu.
    lw_sood1-objdes    = gs_anexo-typearch.
    lw_sood1-objsns    = 'O'.

    CONCATENATE '&KEY&' gs_anexo-ruta INTO lt_objcont.
    APPEND lt_objcont.

    CALL FUNCTION 'SO_OBJECT_INSERT'
      EXPORTING
        folder_id                  = lw_folder
        object_type                = 'URL'
        object_hd_change           = lw_sood1
      IMPORTING
        object_id                  = lv_obj_id
      TABLES
        objhead                    = lt_objhead
        objcont                    = lt_objcont
      EXCEPTIONS
        active_user_not_exist      = 1
        folder_not_exist           = 2
        object_type_not_exist      = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        OTHERS                     = 7.


    lw_object-objtype = 'BUS1006'.
    is_object-objtype = 'BUS1006'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_anexo-code_anexo
      IMPORTING
        output = lv_anexo10.

    lw_object-objkey = lv_anexo10.

    lw_document_id-foltp = lw_folder-foltp.
    lw_document_id-folyr = lw_folder-folyr.
    lw_document_id-folno = lw_folder-folno.
    lw_document_id-doctp = lv_obj_id-objtp.
    lw_document_id-docyr = lv_obj_id-objyr.
    lw_document_id-docno = lv_obj_id-objno.

    " Objeto recién creado en SAPOffice
    lw_reldoc-objtype = 'MESSAGE'.
    lw_reldoc-objkey  = lw_document_id.

    CALL FUNCTION 'BINARY_RELATION_CREATE'
      EXPORTING
        obj_rolea      = lw_object
        obj_roleb      = lw_reldoc
        relationtype   = 'URL'
      EXCEPTIONS
        no_model       = 1
        internal_error = 2
        unknown        = 3
        OTHERS         = 4.

    IF sy-subrc NE 0.
      lv_tipomsje = lc_e.
    ELSE.
      lv_tipomsje = lc_s.
    ENDIF.

    CLEAR ls_return.
    REFRESH lt_return.
    CASE lv_tipomsje.
      WHEN lc_e.
        ls_return-type    = lc_e.
        ls_return-message = lc_error.
      WHEN lc_s.
        ls_return-type    = lc_s.
        ls_return-message = lc_exito.
    ENDCASE.

    APPEND ls_return TO lt_return.
    CLEAR ls_return.

    PERFORM carga_alv
     TABLES lt_return
      USING is_object-objtype  " ID Code BPS
            lv_index cx-cgranx "--> Alta de BPS
   CHANGING lv_tipomsje.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_direcciones_adicionales
*&---------------------------------------------------------------------*
*& Agrega la informacion adicional de las direcciones Herramienta é Instruc
*&---------------------------------------------------------------------*
FORM f_direcciones_adicionales
  USING ls_obj_k        TYPE ty_object_key
        lv_code_general TYPE char10.

  DATA: lt_charvalues TYPE TABLE OF bapi1003_alloc_values_char,
        ls_charvalues LIKE LINE OF lt_charvalues,
        lt_return2    TYPE TABLE OF bapiret2,
        lv_class_num  TYPE bapi1003_key-classnum.

  DATA: lv_bp    TYPE bapi1003_key-object,
        lv_kna1  TYPE bapi1003_key-objecttable,
        lv_011   TYPE bapi1003_key-classtype,
        lv_1     TYPE bapi1003_key-status,
        lv_datum TYPE bapi1003_key-keydate.

  CONSTANTS: lc_herramientas_sd(15)  TYPE c VALUE 'SD_HERRAMIENTAS',
             lc_instrucciones_sd(14) TYPE c VALUE 'SD_INSTRUCCIONES',
             lc_kna1(4)              TYPE c VALUE 'KNA1',
             lc_011(3)               TYPE c VALUE '011',
             lc_1                    TYPE c VALUE '1',
             lc_herramientas(12)     TYPE c VALUE 'HERRAMIENTAS',
             lc_instrucciones(11)    TYPE c VALUE 'INSTRUCCIONES'.

  lv_bp     = ls_obj_k-key.
  lv_kna1   = lc_kna1.
  lv_011    = lc_011.
  lv_1      = lc_1.
  lv_datum  = sy-datum.

  WAIT UP TO 1 SECONDS.

***  Direccion Entrega - Herramientas
  IF t_pstherram IS NOT INITIAL.
    CLEAR: lt_charvalues[], lt_return2[], ls_charvalues.
    LOOP AT t_pstherram INTO DATA(ls_pstherram) WHERE code EQ lv_code_general AND tipo = ls_obj_k-type.
      CLEAR: ls_charvalues.
      ls_charvalues-charact = lc_herramientas.

      TRANSLATE ls_pstherram-herrmnta TO UPPER CASE.

      SELECT SINGLE value_char FROM ztbp_dir_add
        INTO ls_charvalues-value_char
        WHERE charact EQ lc_herramientas_sd
        AND   descripcion EQ ls_pstherram-herrmnta.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      APPEND ls_charvalues TO lt_charvalues.

      lv_class_num = lc_herramientas_sd.

    ENDLOOP.

    IF lt_charvalues IS NOT INITIAL.

      CLEAR: lt_return2[].
      CALL FUNCTION 'BAPI_OBJCL_CREATE'
        EXPORTING
          objectkeynew    = lv_bp
          objecttablenew  = lv_kna1
          classnumnew     = lv_class_num
          classtypenew    = lv_011
          status          = lv_1
          keydate         = lv_datum
        TABLES
          allocvalueschar = lt_charvalues
          return          = lt_return2.
*** Manejo de errores
*      IF lt_return2 IS NOT INITIAL.
*        READ TABLE lt_return2 TRANSPORTING NO FIELDS WITH KEY 'E'.
*        IF sy-subrc EQ 0.
*          PERFORM carga_alv_bapi_err
*           TABLES lt_return2
*           USING  cx-aherrm.
*        ENDIF.
*      ENDIF.
    ENDIF.
  ENDIF.

**** Direccion Entrega - Instrucciones
  IF t_pstinstruc IS NOT INITIAL.
    CLEAR: lt_charvalues[], lt_return2[], ls_charvalues.
    LOOP AT t_pstinstruc INTO DATA(ls_pstinstruc) WHERE code EQ lv_code_general AND tipo = ls_obj_k-type.
      CLEAR: ls_charvalues.
      ls_charvalues-charact = lc_herramientas.

      TRANSLATE ls_pstinstruc-instrucc TO UPPER CASE.

      SELECT SINGLE value_char FROM ztbp_dir_add
        INTO ls_charvalues-value_char
        WHERE charact EQ lc_herramientas_sd
        AND   descripcion EQ ls_pstinstruc-instrucc.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      APPEND ls_charvalues TO lt_charvalues.

    ENDLOOP.

    lv_class_num = lc_instrucciones_sd.
    IF lt_charvalues IS NOT INITIAL.
      CLEAR lt_return2[].
      CALL FUNCTION 'BAPI_OBJCL_CREATE'
        EXPORTING
          objectkeynew    = lv_bp
          objecttablenew  = lv_kna1
          classnumnew     = lv_class_num
          classtypenew    = lv_011
          status          = lv_1
          keydate         = lv_datum
        TABLES
          allocvalueschar = lt_charvalues
          return          = lt_return2.
*      IF lt_return2 IS NOT INITIAL.
*        READ TABLE lt_return2 TRANSPORTING NO FIELDS WITH KEY 'E'.
*        IF sy-subrc EQ 0.
*          PERFORM carga_alv_bapi_err
*           TABLES lt_return2
*           USING  cx-ainstr.
*        ENDIF.
*      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form actualiza_inter
*&---------------------------------------------------------------------*
*& Actualizar Interlocutor por Ejecucion de Clase Estandar
*&---------------------------------------------------------------------*
FORM actualiza_inter .

  " Data Local
  DATA lv_tiposat TYPE c.
  DATA lv_rolecat TYPE c LENGTH 6.

  DATA ls_com TYPE cmds_ei_company.
  DATA lt_com TYPE cmds_ei_company_t.

  DATA ls_roles TYPE bus_ei_bupa_roles.
  DATA lt_roles TYPE bus_ei_bupa_roles_t.

  DATA ls_data TYPE cvis_ei_extern.
  DATA lt_data TYPE TABLE OF cvis_ei_extern.

  DATA ls_address TYPE bus_ei_bupa_address.
  DATA lt_address TYPE bus_ei_bupa_address_t.

  DATA ls_tax TYPE bus_ei_bupa_taxnumber.
  DATA lt_tax TYPE bus_ei_bupa_taxnumber_t.

  DATA ls_bank TYPE bus_ei_bupa_bankdetail.
  DATA lt_bank TYPE bus_ei_bupa_bankdetail_t.

  DATA: lv_flag_code     TYPE c,
        lv_flag_vkorg    TYPE c,
        lt_return_commit TYPE TABLE OF bapiret2.

  DATA ls_seg TYPE ukm_ei_bp_cms_sgm.
  DATA lt_seg TYPE ukmt_ei_bp_cms_sgm.

  DATA lt_return  TYPE bapiretm.
  DATA ls_return  TYPE bapireti.
  DATA ls_message TYPE bapiret2.

  DATA lv_index  TYPE i.
  DATA lv_nombre TYPE c LENGTH 30.

  DATA lv_tipomsje TYPE c.

  CONSTANTS lc_group(3) TYPE c VALUE 'ZDM'.
  CONSTANTS lc_z01(3)   TYPE c VALUE 'Z01'.
  CONSTANTS lc_fisi(6)  TYPE c VALUE 'Fisica'.
  CONSTANTS lc_juri(8)  TYPE c VALUE 'Juridica'.

  CONSTANTS lc_s     TYPE c VALUE 'S'.
  CONSTANTS lc_e     TYPE c VALUE 'E'.
  CONSTANTS lc_1     TYPE c VALUE '1'.
  CONSTANTS lc_2     TYPE c VALUE '2'.
  CONSTANTS lc_crear TYPE c VALUE 'I'.

  CONSTANTS lc_crm002(6) TYPE c VALUE 'CRM002'.
  CONSTANTS lc_flcu01(6) TYPE c VALUE 'FLCU01'.
*&-------------------------------------------------------

*** Actualizacion locutor cliente
  CLEAR: gs_pstcnteint, lv_flag_vkorg, lv_flag_code.
  LOOP AT t_pstcnteint INTO gs_pstcnteint.
    AT NEW code.
      lv_flag_code = abap_true.
    ENDAT.

    CLEAR: ls_data, lt_data[], ls_sales, lt_sales[]. "primer limpieza para el primer ciclo de borrado
    CLEAR:  ls_sales, lt_functions[], ls_functions.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_pstcnteint-code
      IMPORTING
        output = ls_data-partner-header-object_instance-bpartner.

    ls_data-partner-header-object_task                  = 'U'.
    ls_data-partner-header-object                       = ls_data-partner-header-object_instance-bpartner.
    ls_data-partner-header-object_instance-bpartnerguid = ls_data-partner-header-object.

    ls_data-customer-header-object_instance-kunnr       = ls_data-partner-header-object.
    ls_data-customer-header-object_task = 'U'.

    "Primer ciclo de borrado
    IF lv_flag_code EQ abap_true.

      SELECT * FROM knvp
        INTO TABLE @DATA(lt_knvp)
        WHERE kunnr EQ @ls_data-partner-header-object.
      IF sy-subrc EQ 0.

*        DELETE lt_knvp WHERE parza IS INITIAL.
        DELETE lt_knvp
          WHERE kunn2 EQ ls_data-partner-header-object_instance-bpartner
          AND   parza IS INITIAL
          AND   ( parvw EQ 'AG' OR parvw EQ 'RE' OR parvw EQ 'RG' OR parvw EQ 'WE' ).

        IF lt_knvp[] IS NOT INITIAL.
          CLEAR:  ls_sales, lt_functions[], ls_functions.

          SORT lt_knvp BY vkorg.

          CLEAR: lv_flag_vkorg.
          LOOP AT lt_knvp INTO DATA(ls_knvp).
            AT END OF vkorg.
              lv_flag_vkorg = abap_true.
            ENDAT.
            CLEAR: ls_functions.

            ls_functions-task = 'D'.
            ls_functions-data_key-parvw = ls_knvp-parvw.
            ls_functions-data_key-parza = ls_knvp-parza.
            ls_functions-data-partner = ls_knvp-kunn2.
            ls_functions-datax-partner  = COND #( WHEN ls_functions-data-partner      IS NOT INITIAL THEN abap_true ELSE abap_false ).

            APPEND ls_functions TO lt_functions.

            IF lv_flag_vkorg EQ abap_true.

              ls_sales-task = 'U'.
              ls_sales-data_key-vkorg = ls_knvp-vkorg.
              ls_sales-data_key-vtweg = ls_knvp-vtweg.
              ls_sales-data_key-spart = ls_knvp-spart.

              ls_sales-functions-functions = lt_functions.

              lv_flag_vkorg = abap_false.
              CLEAR: lt_functions[].

              APPEND ls_sales TO lt_sales.

            ENDIF.

          ENDLOOP.

          ls_data-customer-sales_data-sales = lt_sales.

          lv_flag_code = abap_false.

          APPEND ls_data  TO lt_data.

          CALL METHOD cl_md_bp_maintain=>maintain
            EXPORTING
              i_data     = lt_data
              i_test_run = chk_test
            IMPORTING
              e_return   = t_return.

          IF t_return IS NOT INITIAL.

            CLEAR gs_return.
            READ TABLE t_return INTO gs_return INDEX 1.
            IF sy-subrc EQ 0.

              DELETE ADJACENT DUPLICATES FROM gs_return-object_msg.

              PERFORM carga_alv
               TABLES gs_return-object_msg
                USING gs_return-object_key
                      lv_index cx-atbps"--> Alta de BPS
             CHANGING lv_tipomsje.

            ENDIF.

          ELSE.

            IF chk_test IS INITIAL.
              gv_mess  = 'Funciones de interlocutor borradas'.
            ELSE.
              gv_mess  = 'BP Listo para la Actualizacion (A)'.
            ENDIF.

            gs_mess = VALUE #( type    = 'S'
                               message = gv_mess
                            ).
            APPEND gs_mess TO gt_mess.

            t_return = VALUE #( ( object_idx = 1
                                  object_key = gs_gnrales-code
                                  object_msg = gt_mess
                              ) ).
            CLEAR gv_mess.

            READ TABLE t_return INTO gs_return INDEX 1.
            IF sy-subrc EQ 0.

              DELETE ADJACENT DUPLICATES FROM gs_return-object_msg.

              PERFORM carga_alv
               TABLES gs_return-object_msg
                USING gs_return-object_key
                      lv_index cx-atbps"--> Alta de BPS
             CHANGING lv_tipomsje.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR: lt_data[], lt_functions, ls_functions, ls_sales, lt_sales[], ls_data-customer-sales_data-sales. "segunda limpieza para el update

    ENDIF.
    CLEAR: lt_sales[], lt_functions[].
    "Segundo ciclo para insertar la nueva informacion

    ls_sales-task = 'M'.
    ls_sales-data_key-vkorg = gs_pstcnteint-orgvnta.
    ls_sales-data_key-vtweg = gs_pstcnteint-canal.
    ls_sales-data_key-spart = gs_pstcnteint-sector.

    ls_functions-task = 'I'.
    ls_functions-data_key-parvw = gs_pstcnteint-tipointer.
*    ls_functions-data_key-parza = ls_knvp-parza.

    SELECT SINGLE partner FROM but000
      INTO @DATA(lv_partner)
      WHERE partner EQ @gs_pstcnteint-interloc.
    IF sy-subrc NE 0.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_pstcnteint-interloc
        IMPORTING
          output = lv_partner.

    ENDIF.

    ls_functions-data-partner = lv_partner.
    ls_functions-datax-partner  = COND #( WHEN ls_functions-data-partner      IS NOT INITIAL THEN abap_true ELSE abap_false ).
    APPEND ls_functions TO lt_functions.

    ls_sales-functions-functions = lt_functions.
    APPEND ls_sales TO lt_sales.

    ls_data-customer-sales_data-sales = lt_sales.

    APPEND ls_data  TO lt_data.

    CALL METHOD cl_md_bp_maintain=>maintain
      EXPORTING
        i_data     = lt_data
        i_test_run = chk_test
      IMPORTING
        e_return   = t_return.

    IF t_return IS NOT INITIAL.
      CLEAR gs_return.
      READ TABLE t_return INTO gs_return INDEX 1.
      IF sy-subrc EQ 0.

        DELETE ADJACENT DUPLICATES FROM gs_return-object_msg.

        PERFORM carga_alv
         TABLES gs_return-object_msg
          USING gs_return-object_key
                lv_index cx-atbps"--> Alta de BPS
       CHANGING lv_tipomsje.

      ENDIF.

    ELSE.

      IF chk_test IS INITIAL.
        gv_mess  = 'Funciones de interlocutor Actualizadas'.
      ELSE.
        gv_mess  = 'BP Listo para la Actualizacion (B)'.
      ENDIF.

      gs_mess = VALUE #( type    = 'S'
                         message = gv_mess
                      ).
      APPEND gs_mess TO gt_mess.

      t_return = VALUE #( ( object_idx = 1
                            object_key = gs_gnrales-code
                            object_msg = gt_mess
                        ) ).
      CLEAR gv_mess.

      READ TABLE t_return INTO gs_return INDEX 1.
      IF sy-subrc EQ 0.

        DELETE ADJACENT DUPLICATES FROM gs_return-object_msg.

        PERFORM carga_alv
         TABLES gs_return-object_msg
          USING gs_return-object_key
                lv_index cx-atbps"--> Alta de BPS
       CHANGING lv_tipomsje.

      ENDIF.
    ENDIF.

  ENDLOOP.

*** Actualizacion locutor proveedor

  "SÓLO SI HAY INFO EN PESTAÑA PROVEEDOR-INTERLOCUTOR

  "--->FUNCTIONS
  CLEAR: gs_pstprovint, lv_flag_code.
  LOOP AT t_pstprovint  INTO gs_pstprovint.
    AT NEW code_minor.
      lv_flag_code = abap_true.
    ENDAT.

    CLEAR: ls_data, lt_data[]. "primer limpieza para el primer ciclo de borrado
    CLEAR: lt_purchasing[], ls_purchasing, lt_func_prov[], ls_func_prov.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_pstprovint-code_minor
      IMPORTING
        output = ls_data-partner-header-object_instance-bpartner.

    ls_data-partner-header-object_task                  = 'U'.
    ls_data-partner-header-object                       = ls_data-partner-header-object_instance-bpartner.
    ls_data-partner-header-object_instance-bpartnerguid = ls_data-partner-header-object.

    ls_data-customer-header-object_instance-kunnr       = ls_data-partner-header-object.
    ls_data-customer-header-object_task = 'U'.

    ls_data-vendor-header-object_task = 'U'.
    ls_data-vendor-header-object_instance-lifnr = ls_data-partner-header-object_instance-bpartner.

    "Primer ciclo de borrado
    IF lv_flag_code EQ abap_true.

      SELECT * FROM wyt3
        INTO TABLE @DATA(lt_wyt3)
        WHERE lifnr EQ @ls_data-partner-header-object.
      IF sy-subrc EQ 0.

*        DELETE lt_wyt3 WHERE parza IS INITIAL.
        DELETE lt_wyt3
          WHERE lifn2 EQ ls_data-partner-header-object_instance-bpartner
          AND   parza IS INITIAL
          AND   ( parvw EQ 'BA' OR parvw EQ 'LF' OR parvw EQ 'RS' ).

        IF lt_wyt3 IS NOT INITIAL.

          SORT lt_wyt3 BY ekorg.

          CLEAR: lv_flag_vkorg.
          LOOP AT lt_wyt3 INTO DATA(ls_wyt3).
            AT END OF ekorg.
              lv_flag_vkorg = abap_true.
            ENDAT.

            ls_func_prov-task = 'D'.
            ls_func_prov-data-partner = ls_wyt3-lifn2.
            ls_func_prov-data_key-werks = ls_wyt3-werks.
            ls_func_prov-data_key-parvw = ls_wyt3-parvw.
            ls_func_prov-data_key-parza = ls_wyt3-parza.

            ls_func_prov-datax-partner   = COND #( WHEN ls_func_prov-data-partner      IS NOT INITIAL THEN abap_true ELSE abap_false ).

            APPEND ls_func_prov TO lt_func_prov.

            IF lv_flag_vkorg EQ abap_true.

              ls_purchasing-task = 'U'.
              ls_purchasing-data_key-ekorg = ls_wyt3-ekorg.
              ls_purchasing-functions-functions = lt_func_prov.

              lv_flag_vkorg = abap_false.

              CLEAR: lt_func_prov[].
              APPEND ls_purchasing TO lt_purchasing.

            ENDIF.

          ENDLOOP.

          ls_data-vendor-purchasing_data-purchasing = lt_purchasing.

          CLEAR: lt_purchasing[], ls_purchasing.

          lv_flag_code = abap_false.

          APPEND ls_data  TO lt_data.

          CALL METHOD cl_md_bp_maintain=>maintain
            EXPORTING
              i_data     = lt_data
              i_test_run = chk_test
            IMPORTING
              e_return   = t_return.

          IF t_return IS NOT INITIAL.

            CLEAR gs_return.
            READ TABLE t_return INTO gs_return INDEX 1.
            IF sy-subrc EQ 0.

              DELETE ADJACENT DUPLICATES FROM gs_return-object_msg.

              PERFORM carga_alv
               TABLES gs_return-object_msg
                USING gs_return-object_key
                      lv_index cx-atbps"--> Alta de BPS
             CHANGING lv_tipomsje.

            ENDIF.

          ELSE.

            IF chk_test IS INITIAL.
              gv_mess  = 'Funciones de interlocutor borradas'.
            ELSE.
              gv_mess  = 'BP Listo para la Actualizacion (A)'.
            ENDIF.

            gs_mess = VALUE #( type    = 'S'
                               message = gv_mess
                            ).
            APPEND gs_mess TO gt_mess.

            t_return = VALUE #( ( object_idx = 1
                                  object_key = gs_gnrales-code
                                  object_msg = gt_mess
                              ) ).
            CLEAR gv_mess.

            READ TABLE t_return INTO gs_return INDEX 1.
            IF sy-subrc EQ 0.

              DELETE ADJACENT DUPLICATES FROM gs_return-object_msg.

              PERFORM carga_alv
               TABLES gs_return-object_msg
                USING gs_return-object_key
                      lv_index cx-atbps"--> Alta de BPS
             CHANGING lv_tipomsje.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    SELECT SINGLE partner FROM but000
      INTO @lv_partner
      WHERE partner EQ @gs_pstprovint-interlcutr.
    IF sy-subrc NE 0.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_pstprovint-interlcutr
        IMPORTING
          output = lv_partner.

    ENDIF.

    ls_func_prov-task = 'I'.
    ls_func_prov-data-partner = lv_partner.
    ls_func_prov-data_key-werks = gs_pstprovint-centro.
    ls_func_prov-data_key-parvw = gs_pstprovint-tipo_inter.
    ls_func_prov-datax-partner   = COND #( WHEN ls_func_prov-data-partner      IS NOT INITIAL THEN abap_true ELSE abap_false ).

    APPEND ls_func_prov TO lt_func_prov.

    ls_purchasing-task = 'U'. "DNAVOA
    ls_purchasing-data_key-ekorg = gs_pstprovint-empresa.
    ls_purchasing-functions-functions = lt_func_prov.

    APPEND ls_purchasing TO lt_purchasing.

    ls_data-vendor-purchasing_data-purchasing = lt_purchasing.

    APPEND ls_data  TO lt_data.

    CALL METHOD cl_md_bp_maintain=>maintain
      EXPORTING
        i_data     = lt_data
        i_test_run = chk_test
      IMPORTING
        e_return   = t_return.

    IF t_return IS NOT INITIAL.

      CLEAR gs_return.
      READ TABLE t_return INTO gs_return INDEX 1.
      IF sy-subrc EQ 0.

        DELETE ADJACENT DUPLICATES FROM gs_return-object_msg.

        PERFORM carga_alv
         TABLES gs_return-object_msg
          USING gs_return-object_key
                lv_index cx-atbps"--> Alta de BPS
       CHANGING lv_tipomsje.

      ENDIF.

    ELSE.

      IF chk_test IS INITIAL.
        gv_mess  = 'Funciones de interlocutor borradas'.
      ELSE.
        gv_mess  = 'BP Listo para la Actualizacion (B)'.
      ENDIF.

      gs_mess = VALUE #( type    = 'S'
                         message = gv_mess
                      ).
      APPEND gs_mess TO gt_mess.

      t_return = VALUE #( ( object_idx = 1
                            object_key = gs_gnrales-code
                            object_msg = gt_mess
                        ) ).
      CLEAR gv_mess.

      READ TABLE t_return INTO gs_return INDEX 1.
      IF sy-subrc EQ 0.

        DELETE ADJACENT DUPLICATES FROM gs_return-object_msg.

        PERFORM carga_alv
         TABLES gs_return-object_msg
          USING gs_return-object_key
                lv_index cx-atbps"--> Alta de BPS
       CHANGING lv_tipomsje.

      ENDIF.
    ENDIF.

  ENDLOOP.

  IF chk_test IS INITIAL.

    CLEAR: lt_return_commit.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDIF.

** INI DNAVOA 13.06.2025

  " ---> Log Principal
  CALL SCREEN 001.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form crea_catalog
*&---------------------------------------------------------------------*
*&  Tipó de Catalogo Segun Ventana
*&---------------------------------------------------------------------*
*&   --> IM_TYPECAT Tipo Catalogo
*&---------------------------------------------------------------------*
FORM crea_catalog  USING im_typecat TYPE c.

  DATA lv_struct TYPE dd02l-tabname.

  CONSTANTS lc_log(3)    TYPE c VALUE 'LOG'.
  CONSTANTS lc_dir(5)    TYPE c VALUE 'DIREC'.
  CONSTANTS lc_anexo(5)  TYPE c VALUE 'ANEXO'.
  CONSTANTS lc_gnral(5)  TYPE c VALUE 'GNRAL'.
  CONSTANTS lc_prove(5)  TYPE c VALUE 'PROVE'.
  CONSTANTS lc_prvcmp(6) TYPE c VALUE 'PRVCMP'.
  CONSTANTS lc_cntfin(6) TYPE c VALUE 'CNTFIN'.
  CONSTANTS lc_cntvnt(6) TYPE c VALUE 'CNTVNT'.
*&--------------------------------------------

  CASE im_typecat.
    WHEN lc_log.
      lv_struct = 'ZST_LOGPMASIVO_BPS'.

    WHEN lc_gnral.
      lv_struct = 'ZST_GNRALESBPS'.

    WHEN lc_dir.
      lv_struct = 'ZST_DIRCENTREGABPS'.

    WHEN lc_prove.
      lv_struct = 'ZST_PROVEEDOR_BPS'.

    WHEN lc_prvcmp.
      lv_struct = 'ZST_PROVCOMPRAS_BPS'.

    WHEN lc_cntfin.
      lv_struct = 'ZST_CNTEFINAN_BPS'.

    WHEN lc_cntvnt.
      lv_struct = 'ZST_CNTEVNTAS_BPS'.

    WHEN lc_anexo.
      lv_struct = 'ZST_ANEXO_BPS'.

  ENDCASE.

  REFRESH t_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = lv_struct
    CHANGING
      ct_fieldcat            = t_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT t_fcat INTO gs_fcat.

    IF gs_fcat-fieldname EQ 'REGISTRO'.

      gs_fcat-no_out = abap_true.
      MODIFY t_fcat FROM gs_fcat
      TRANSPORTING no_out.

    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form crea_layout
*&---------------------------------------------------------------------*
*& Layout del Reporte ALV
*&---------------------------------------------------------------------*
FORM crea_layout .

  gs_layout-zebra      = abap_true.
  gs_layout-cwidth_opt = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form totales_regis
*&---------------------------------------------------------------------*
*& Totales de Registros Ejecutados
*&---------------------------------------------------------------------*
*&      <-- IM_TABLA  Tabla Ejecutadps
*&      <-- EX_ADVERT Registros Advertencia
*&      <-- EX_ERROR  Registros Erroneos
*&      <-- EX_EXITO  Registros Exitosos
*&      <-- EX_TOTAL  Totales Registros
*&---------------------------------------------------------------------*
FORM totales_regis     USING im_tabla  TYPE c
                    CHANGING ex_advert TYPE c
                             ex_error  TYPE c
                             ex_exito  TYPE c
                             ex_total  TYPE c.

  DATA lv_regis TYPE i.

  DATA lt_filtros TYPE TABLE OF zst_logpmasivo_bps.

  CONSTANTS lc_a         TYPE c VALUE 'A'.
  CONSTANTS lc_s         TYPE c VALUE 'S'.
  CONSTANTS lc_e         TYPE c VALUE 'E'.
  CONSTANTS lc_log(3)    TYPE c VALUE 'LOG'.
  CONSTANTS lc_anexo(5)  TYPE c VALUE 'ANEXO'.
  CONSTANTS lc_gnral(5)  TYPE c VALUE 'GNRAL'.
  CONSTANTS lc_prove(5)  TYPE c VALUE 'PROVE'.
  CONSTANTS lc_prvcmp(6) TYPE c VALUE 'PRVCMP'.
  CONSTANTS lc_cntfin(6) TYPE c VALUE 'CNTFIN'.
  CONSTANTS lc_cntvnt(6) TYPE c VALUE 'CNTVNT'.
  CONSTANTS lc_dirent(6) TYPE c VALUE 'DIRENT'.
*-------------------------------------------------

  CASE im_tabla.
    WHEN lc_log.

      DESCRIBE TABLE t_alvlog LINES lv_regis.
      MOVE lv_regis TO ex_total.
      CONDENSE ex_total.

      CLEAR lv_regis.
      APPEND LINES OF t_alvlog TO lt_filtros.
      SORT lt_filtros BY type_msje.
      DELETE lt_filtros WHERE type_msje NE lc_s.

      DESCRIBE TABLE lt_filtros LINES lv_regis.
      MOVE lv_regis TO ex_exito." <-- Exitosos
      CONDENSE ex_exito.

      CLEAR lv_regis.
      REFRESH lt_filtros.
      APPEND LINES OF t_alvlog TO lt_filtros.
      SORT lt_filtros BY type_msje.
      DELETE lt_filtros WHERE type_msje NE lc_e AND type_msje NE lc_a.

      DESCRIBE TABLE lt_filtros LINES lv_regis.
      MOVE lv_regis TO ex_error." <-- Error
      CONDENSE ex_error.

      CLEAR lv_regis.
      REFRESH lt_filtros.
      APPEND LINES OF t_alvlog TO lt_filtros.
      SORT lt_filtros BY type_msje.
      DELETE lt_filtros WHERE type_msje EQ lc_s.
      DELETE lt_filtros WHERE type_msje EQ lc_e.

      DESCRIBE TABLE lt_filtros LINES lv_regis.
      MOVE lv_regis TO ex_advert." <-- Advertencia
      CONDENSE ex_advert.

    WHEN lc_gnral.
      DESCRIBE TABLE t_gnrales LINES lv_regis.
      MOVE lv_regis TO ex_total.

    WHEN lc_prove.
      DESCRIBE TABLE t_provedor LINES lv_regis.
      MOVE lv_regis TO ex_total.

    WHEN lc_prvcmp.
      DESCRIBE TABLE t_provcomp LINES lv_regis.
      MOVE lv_regis TO ex_total.

    WHEN lc_cntfin.
      DESCRIBE TABLE t_cntefinan LINES lv_regis.
      MOVE lv_regis TO ex_total.

    WHEN lc_cntvnt.
      DESCRIBE TABLE t_cntevntas LINES lv_regis.
      MOVE lv_regis TO ex_total.

    WHEN lc_anexo.
      DESCRIBE TABLE t_gnrales LINES lv_regis.
      MOVE lv_regis TO ex_total.

    WHEN lc_dirent.
      DESCRIBE TABLE t_direntg LINES lv_regis.
      MOVE lv_regis TO ex_total.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form load_pic_from_db
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- URL
*&---------------------------------------------------------------------*
FORM load_pic_from_db  CHANGING url.

  DATA pic_size       TYPE i.
  DATA html_table     LIKE w3html OCCURS 1.
  DATA return_code    LIKE w3param-ret_code.
  DATA content_type   LIKE w3param-cont_type.
  DATA content_length LIKE w3param-cont_len.
  DATA pic_data       LIKE w3mime  OCCURS 0.
  DATA query_table    LIKE w3query OCCURS 1 WITH HEADER LINE.

  REFRESH query_table.
  query_table-name  = '_OBJECT_ID'.
  query_table-value = 'ZCOPA_LOGO'.   "Nuestra imagen
  APPEND query_table.

  CALL FUNCTION 'WWW_GET_MIME_OBJECT'
    TABLES
      query_string        = query_table
      html                = html_table
      mime                = pic_data
    CHANGING
      return_code         = return_code
      content_type        = content_type
      content_length      = content_length
    EXCEPTIONS
      object_not_found    = 1
      parameter_not_found = 2
      OTHERS              = 3.

  IF sy-subrc = 0.
    pic_size = content_length.
  ENDIF.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'image'
      subtype  = cndp_sap_tab_unknown
      size     = pic_size
      lifetime = cndp_lifetime_transaction
    TABLES
      data     = pic_data
    CHANGING
      url      = url
    EXCEPTIONS
      OTHERS   = 1.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_control_flag
*&---------------------------------------------------------------------*
*& Control Carga para Pestañas de Excel
*&---------------------------------------------------------------------*
*&      <-- EX_GNRAL    GENERALES
*&      <-- EX_INFFISC  INFORMACION FINANCIERA
*&      <-- EX_ANEXOS   ANEXOS
*&      <-- EX_CLIENTE  CLIENTE
*&      <-- EX_DIRENTG  DIRECCION DE ENTREGA
*&      <-- EX_DIREMPR  DIRECCION DE EMPRESA
*&      <-- EX_DIRHERR  DIRECCION DE HERRAMIENTA
*&      <-- EX_DIRINST  DIRECCION DE INSTRUCCION
*&      <-- EX_CNTRET   CLIENTE RETENCION
*&      <-- EX_CNTVNT   CLIENTE VENTA
*&      <-- EX_CNTINT   CLIENTE INTERLOCUTOR
*&      <-- EX_PROVED   PROVEEDOR
*&      <-- EX_PRVRNT   PROVEEDOR RENTENCION
*&      <-- EX_PRVINT   PROVEEDOR INTERLOCUTOR
*&---------------------------------------------------------------------*
FORM f_control_flag  CHANGING p_gnral   TYPE c
                              p_inffisc TYPE c
                              p_anexos  TYPE c
                              p_cliente TYPE c
                              p_direntg TYPE c
                              p_dirempr TYPE c
                              p_dirherr TYPE c
                              p_dirinst TYPE c
                              p_cntret  TYPE c
                              p_cntvnt  TYPE c
                              p_cntint  TYPE c
                              p_proved  TYPE c
                              p_prvrnt  TYPE c
                              p_prvint  TYPE c
                              p_prvcla  TYPE c.

  CASE abap_true.
    WHEN r_crbps. " Alta de BPS

      p_gnral   = abap_true.
      p_inffisc = abap_true.
      p_anexos  = abap_true.
      p_cliente = abap_true.
      p_direntg = abap_true.
      p_dirempr = abap_true.
      p_dirherr = abap_true.
      p_dirinst = abap_true.
      p_cntret  = abap_true.
      p_cntvnt  = abap_true.
      p_cntint  = abap_true.
      p_proved  = abap_true.
      p_prvrnt  = abap_true.
      p_prvint  = abap_true.
      p_prvcla  = abap_true.

    WHEN r_mdbps. " Modificacion BPS

      p_gnral   = abap_true.
      p_inffisc = abap_true.
      p_anexos  = abap_true.
      p_cliente = abap_true.
      p_cntret  = abap_true.
      p_cntvnt  = abap_true.
      p_proved  = abap_true.
      p_prvrnt  = abap_true.
      p_prvcla  = abap_true.

    WHEN r_acint. " Actualizacion Interlocutor

      p_cntint  = abap_true.
      p_prvint  = abap_true.

    WHEN r_atdir. " Alta Direcciones

      p_direntg = abap_true.
      p_dirempr = abap_true.
      p_dirherr = abap_true.
      p_dirinst = abap_true.

    WHEN r_cranx. " Carga Anexos

      p_anexos  = abap_true.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alta_direcc
*&---------------------------------------------------------------------*
FORM alta_direcc .

  " Data Local
  DATA lv_tiposat TYPE c.
  DATA lv_rolecat TYPE c LENGTH 6.
  DATA: lv_flag TYPE c.

  DATA ls_com TYPE cmds_ei_company.
  DATA lt_com TYPE cmds_ei_company_t.

  DATA ls_roles TYPE bus_ei_bupa_roles.
  DATA lt_roles TYPE bus_ei_bupa_roles_t.

  DATA ls_data TYPE cvis_ei_extern.
  DATA lt_data TYPE TABLE OF cvis_ei_extern.

  DATA ls_address TYPE bus_ei_bupa_address.
  DATA lt_address TYPE bus_ei_bupa_address_t.

  DATA ls_tax TYPE bus_ei_bupa_taxnumber.
  DATA lt_tax TYPE bus_ei_bupa_taxnumber_t.

  DATA ls_bank TYPE bus_ei_bupa_bankdetail.
  DATA lt_bank TYPE bus_ei_bupa_bankdetail_t.

  DATA ls_seg TYPE ukm_ei_bp_cms_sgm.
  DATA lt_seg TYPE ukmt_ei_bp_cms_sgm.

  DATA lt_return  TYPE bapiretm.
  DATA ls_return  TYPE bapireti.
  DATA ls_message TYPE bapiret2.

  DATA lv_index  TYPE i.
  DATA lv_nombre TYPE c LENGTH 30.

  DATA lv_tipomsje TYPE c.

  DATA: lt_object_keys TYPE STANDARD TABLE OF bds_objid. "DNAVOA

  DATA: lt_but000    TYPE TABLE OF but000,
        lt_select    LIKE t_gnrales,
        lv_cont      TYPE n LENGTH 3,
        lv_cont_cre  TYPE sy-tabix,
        lv_bp_format TYPE char6,
        lv_bp_direc  TYPE kunnr.

  CONSTANTS lc_group(3) TYPE c VALUE 'ZDM'.
  CONSTANTS lc_z01(3)   TYPE c VALUE 'Z01'.
  CONSTANTS lc_fisi(6)  TYPE c VALUE 'Fisica'.
  CONSTANTS lc_juri(8)  TYPE c VALUE 'Juridica'.

  CONSTANTS lc_s     TYPE c VALUE 'S'.
  CONSTANTS lc_e     TYPE c VALUE 'E'.
  CONSTANTS lc_1     TYPE c VALUE '1'.
  CONSTANTS lc_2     TYPE c VALUE '2'.
  CONSTANTS lc_crear TYPE c VALUE 'I'.

  CONSTANTS lc_crm002(6) TYPE c VALUE 'CRM002'.
  CONSTANTS lc_flcu01(6) TYPE c VALUE 'FLCU01'.
*&-------------------------------------------------------

  DATA(ls_aux_sales) = ls_data-customer-sales_data-sales.

  CLEAR: t_gnrales[].
  LOOP AT t_pstentrega INTO DATA(ls_entrega).
    AT NEW code.
      lv_flag = abap_true.
    ENDAT.
    IF lv_flag = abap_true.
      gs_gnrales-code = ls_entrega-code.
      APPEND gs_gnrales TO t_gnrales.
      lv_flag = abap_false.
    ENDIF.
  ENDLOOP.

  " Rutina para crear direcciones de Entrega
  PERFORM f_crear_direcciones_entrega TABLES t_gnrales
                                      USING  chk_test.

  IF chk_test IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

  CALL SCREEN 001.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form alta_anexo
*&---------------------------------------------------------------------*
*  Carga de URL de Anexo
*&---------------------------------------------------------------------*
FORM alta_anexo .

  DATA lv_codebp TYPE swo_typeid.
  DATA lv_url    TYPE so_url.
  DATA rel_doc   TYPE borident.
  DATA folder_id TYPE sofdk.
  DATA is_object TYPE borident.
  DATA lv_title  TYPE so_obj_des.
  DATA ls_object TYPE sibflporb.

  DATA lv_tipomsje TYPE c.
  DATA lv_index    TYPE i.
  DATA ls_return   TYPE bapiretc.
  DATA lt_return   TYPE bapiretct.

  CONSTANTS lc_s TYPE c VALUE 'S'.
  CONSTANTS lc_e TYPE c VALUE 'E'.

  CONSTANTS lc_exito TYPE c LENGTH 20 VALUE 'Carga Exitosa Anexo'.
  CONSTANTS lc_error TYPE c LENGTH 20 VALUE 'Carga Erronea Anexo'.
*-----------------------------------------------------------------

*** INI DNAVOA 11.06.2025
  PERFORM f_anexos.

  IF chk_test IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
*** FIN DNAVOA 11.06.2025

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_alv
*&---------------------------------------------------------------------*
*&  Carga de ALV de Log de Actualizacion o Alta de BPS por Clase
*&---------------------------------------------------------------------*
*&      --> IM_TLOG    Tabla de Retorno de Proceso
*&      --> IM_IDSBPS  ID de BPS Ejecutado
*&      --> IM_REGIS   N° Registro
*&      --> IM_PROCES  Proceso de Ejecucion
*&      --> EX_TIPOMSJ Tipo de Mensaje de Resultado
*&---------------------------------------------------------------------*
*FORM carga_alv_bapi_err TABLES im_tlog   TYPE /eacc/t_bapiret2
*                        USING  im_proces TYPE c.
*
*  DATA: lv_error  TYPE c.
*  DATA  lv_semaf  TYPE c LENGTH 4.
*
*  CONSTANTS lc_exito(4) TYPE c VALUE '@08@'.
*  CONSTANTS lc_adver(4) TYPE c VALUE '@09@'.
*  CONSTANTS lc_error(4) TYPE c VALUE '@0A@'.
*
*  CONSTANTS lc_a TYPE c VALUE 'A'.
*  CONSTANTS lc_s TYPE c VALUE 'S'.
*  CONSTANTS lc_w TYPE c VALUE 'W'.
*  CONSTANTS lc_e TYPE c VALUE 'E'.
*  CONSTANTS lc_icon(4) TYPE c VALUE '@0P@'.
*
*  DATA  ls_return_aux2 TYPE bapiret2.
*
*  READ TABLE im_tlog TRANSPORTING NO FIELDS WITH KEY type = lc_e.
*  IF sy-subrc EQ 0.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*  ENDIF.
*
*  LOOP AT im_tlog INTO ls_return_aux2.
*
*    CLEAR: lv_semaf.
*
*    CASE ls_return_aux2-type.
*
*      WHEN lc_a.
*        lv_semaf = lc_error.
*        ls_return_aux2-type = lc_w.
*
*      WHEN lc_s.
*        lv_semaf = lc_exito.
*
*      WHEN lc_e.
*        lv_semaf = lc_error.
*
*        ls_return_aux2-type = lc_w.
*      WHEN OTHERS.
*        lv_semaf = lc_adver.
*    ENDCASE.
*
*    gs_alvlog-semaforo  = lv_semaf.
*    gs_alvlog-id_bps    = im_idbps.
*    gs_alvlog-proceso   = im_proces.
*    gs_alvlog-registro  = '1'.
*    gs_alvlog-icono     = lc_icon.
*    gs_alvlog-type_msje = ls_return_aux2-type.
*    gs_alvlog-mensaje   = ls_return_aux2-message.
*
*    APPEND gs_alvlog TO t_alvlog.
*    CLEAR gs_alvlog.
*
*  ENDLOOP.
*
*  SORT t_alvlog BY type_msje id_bps proceso mensaje.
*
*  DELETE ADJACENT DUPLICATES FROM t_alvlog.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_alv
*&---------------------------------------------------------------------*
*&  Carga de ALV de Log de Actualizacion o Alta de BPS por Clase
*&---------------------------------------------------------------------*
*&      --> IM_TLOG    Tabla de Retorno de Proceso
*&      --> IM_IDSBPS  ID de BPS Ejecutado
*&      --> IM_REGIS   N° Registro
*&      --> IM_PROCES  Proceso de Ejecucion
*&      --> EX_TIPOMSJ Tipo de Mensaje de Resultado
*&---------------------------------------------------------------------*
FORM carga_alv TABLES im_tlog   TYPE bapiretct
                USING im_idbps  TYPE c
                      im_regis  TYPE i
                      im_proces TYPE c
             CHANGING ex_tipomsj TYPE c.

  DATA  ls_return TYPE bapiretc.
  DATA  lv_semaf  TYPE c LENGTH 4.
  DATA: lv_error  TYPE c.

  CONSTANTS lc_exito(4) TYPE c VALUE '@08@'.
  CONSTANTS lc_adver(4) TYPE c VALUE '@09@'.
  CONSTANTS lc_error(4) TYPE c VALUE '@0A@'.

  CONSTANTS lc_a TYPE c VALUE 'A'.
  CONSTANTS lc_s TYPE c VALUE 'S'.
  CONSTANTS lc_w TYPE c VALUE 'W'.
  CONSTANTS lc_e TYPE c VALUE 'E'.
  CONSTANTS lc_icon(4) TYPE c VALUE '@0P@'.
*--------------------------------------------

  SORT im_tlog BY type.
  READ TABLE im_tlog WITH KEY type = lc_e TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    lv_error = abap_true.
  ENDIF.

  IF lv_error IS INITIAL.
    READ TABLE im_tlog WITH KEY type = lc_a TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      lv_error = abap_true.
    ENDIF.
  ENDIF.

** INI DNAVOA 16.06.2025

  IF lv_error EQ abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

  DELETE im_tlog WHERE type = 'I' AND id = 'CVI_EI' AND number = '072'.

  SORT im_tlog BY type id number message.

  DELETE ADJACENT DUPLICATES FROM im_tlog.
** FIN DNAVOA 16.06.2025

  LOOP AT im_tlog INTO ls_return.


    CLEAR: lv_semaf.

    CASE ls_return-type.

      WHEN lc_a.
        lv_semaf = lc_error.
        ex_tipomsj = lc_e.

      WHEN lc_s.
        lv_semaf = lc_exito.
        ex_tipomsj = lc_s.

      WHEN lc_e.
        lv_semaf = lc_error.
        ex_tipomsj = lc_e.
      WHEN OTHERS.
        lv_semaf = lc_adver.
    ENDCASE.

    gs_alvlog-semaforo  = lv_semaf.
    gs_alvlog-id_bps    = im_idbps.
    gs_alvlog-proceso   = im_proces.
    gs_alvlog-registro  = im_regis.
    gs_alvlog-icono     = lc_icon.
    gs_alvlog-type_msje = ls_return-type.
    gs_alvlog-mensaje   = ls_return-message.

    APPEND gs_alvlog TO t_alvlog.
    CLEAR gs_alvlog.

  ENDLOOP.

  SORT t_alvlog BY type_msje id_bps proceso mensaje.

  DELETE ADJACENT DUPLICATES FROM t_alvlog.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_url_doc
*&---------------------------------------------------------------------*
*& Crea URL DOC
*&---------------------------------------------------------------------*
FORM create_url_doc USING im_url   TYPE so_url
                          im_title TYPE sood-objdes.

  DATA l_tab_size TYPE i.
  DATA ls_objcont TYPE soli.
  DATA l_obj_id   TYPE soodk.
  DATA l_obj_data TYPE sood1.
  DATA l_url_id   TYPE so_url.
  DATA l_url      TYPE so_url.
  DATA lt_objhead TYPE STANDARD TABLE OF soli.
  DATA lt_objcont TYPE STANDARD TABLE OF soli.
  DATA lt_urltab  TYPE STANDARD TABLE OF sood-objdes.

  DATA ls_url_detail TYPE if_cmis_outbound_api_type_def=>ty_s_url_detail.
*---------------------------------------------------------------------------

  IF url_save(132) NE url(132).
    l_url_id = url.
  ELSE.
    l_url_id = url_save.
  ENDIF.

  l_url = l_url_id.

  WHILE NOT l_url_id IS INITIAL.

    CONCATENATE '&KEY&' l_url_id(250) INTO ls_objcont.
    APPEND ls_objcont TO lt_objcont.
    SHIFT l_url_id LEFT BY 250 PLACES.

  ENDWHILE.

  l_obj_data-objsns = 'O'.
  l_obj_data-objla  = sy-langu.

  IF im_title IS INITIAL.

    SPLIT url AT '/' INTO TABLE lt_urltab.
    DESCRIBE TABLE lt_urltab LINES l_tab_size.

    READ TABLE lt_urltab
    INDEX l_tab_size INTO im_title.

  ENDIF.

  l_obj_data-objdes = im_title.

  CALL FUNCTION 'SO_OBJECT_INSERT'
    EXPORTING
      folder_id             = folder_id
      object_type           = 'URL'
      object_hd_change      = l_obj_data
    IMPORTING
      object_id             = l_obj_id
    TABLES
      objhead               = lt_objhead
      objcont               = lt_objcont
    EXCEPTIONS
      active_user_not_exist = 35
      folder_not_exist      = 6
      object_type_not_exist = 17
      owner_not_exist       = 22
      parameter_error       = 23
      OTHERS                = 1000.

  IF sy-subrc EQ 0.

    document_id-foltp = folder_id-foltp.
    document_id-folyr = folder_id-folyr.
    document_id-folno = folder_id-folno.
    document_id-doctp = l_obj_id-objtp.
    document_id-docyr = l_obj_id-objyr.
    document_id-docno = l_obj_id-objno.

    ls_url_detail-url = l_url.
    ls_url_detail-description = im_title.

    TRY.

        cl_gensrvc_cmis_integ_api=>get_instance( )->create_url(
        iv_sapoffice_doc_id = document_id+17(17)
        is_url_detail       = ls_url_detail ).

      CATCH cx_cmis_outbound_api.

    ENDTRY.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_herramientas
*&---------------------------------------------------------------------*
*& Carga de Herramientas
*&---------------------------------------------------------------------*
FORM carga_herramientas .

  DATA: lt_allocvaluesnum  TYPE TABLE OF bapi1003_alloc_values_num,
        lt_allocvalueschar TYPE TABLE OF bapi1003_alloc_values_char,
        lt_allocvaluescurr TYPE TABLE OF bapi1003_alloc_values_curr,
        lt_return          TYPE TABLE OF bapiret2,
        lv_objectkey       TYPE bapi1003_key-object,
        lv_classnum        TYPE bapi1003_key-classnum,
        lv_classtype       TYPE bapi1003_key-classtype.

  " Confirmar cambios si no hubo errores
  DATA lv_error TYPE abap_bool VALUE abap_false.
  DATA lv_date  TYPE bapi_keydate.

  CONSTANTS lc_kna1     TYPE c LENGTH 4  VALUE 'KNA1'.
  CONSTANTS lc_hramnta  TYPE c LENGTH 12 VALUE 'HERRAMIENTAS'.
  CONSTANTS lc_sdhrmnta TYPE c LENGTH 15 VALUE 'SD_HERRAMIENTAS'.

  LOOP AT t_pstherram INTO gs_pstherram.

    CLEAR: lv_classnum, lv_objectkey, lv_classtype.
    lv_classnum  = lc_sdhrmnta.
    lv_objectkey = gs_pstherram-code.
    lv_classtype = gs_pstherram-tipo.

    " Característica tipo CHAR
    APPEND VALUE #( charact       = lc_hramnta
                    value_char    = '2'
                    value_neutral = '2' ) TO lt_allocvalueschar.

    CLEAR lv_date.
    lv_date = sy-datum.
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        objectkeynew    = lv_objectkey
        objecttablenew  = 'KNA1'
        classnumnew     = lv_classnum
        classtypenew    = lv_classtype
        status          = '1'  " Opcional, 1 = activo
        keydate         = lv_date
      TABLES
        allocvaluesnum  = lt_allocvaluesnum
        allocvalueschar = lt_allocvalueschar
        allocvaluescurr = lt_allocvaluescurr
        return          = lt_return.

    LOOP AT lt_return INTO DATA(ls_return).
      IF ls_return-type EQ 'E'
      OR ls_return-type EQ 'A'.
        lv_error = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_error = abap_false.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_instruccion
*&---------------------------------------------------------------------*
*& Carga de Instruccion
*&---------------------------------------------------------------------*
FORM carga_instruccion .

  DATA: lt_allocvaluesnum  TYPE TABLE OF bapi1003_alloc_values_num,
        lt_allocvalueschar TYPE TABLE OF bapi1003_alloc_values_char,
        lt_allocvaluescurr TYPE TABLE OF bapi1003_alloc_values_curr,
        lt_return          TYPE TABLE OF bapiret2,
        lv_objectkey       TYPE bapi1003_key-object,
        lv_classnum        TYPE bapi1003_key-classnum,
        lv_classtype       TYPE bapi1003_key-classtype.

  " Confirmar cambios si no hubo errores
  DATA lv_error TYPE abap_bool VALUE abap_false.
  DATA lv_date  TYPE bapi_keydate.

  CONSTANTS lc_kna1     TYPE c LENGTH 4  VALUE 'KNA1'.
  CONSTANTS lc_instrucn TYPE c LENGTH 13 VALUE 'INSTRUCCIONES'.
  CONSTANTS lc_sdinstrc TYPE c LENGTH 15 VALUE 'SD_HERRAMIENTAS'.

  LOOP AT t_pstinstruc INTO gs_pstinstruc.

    CLEAR lv_classnum.
    CLEAR: lv_objectkey, lv_classtype.
    lv_classnum  = lc_instrucn.
    lv_objectkey = gs_pstinstruc-code.
    lv_classtype = gs_pstinstruc-tipo.

    " Característica tipo CHAR
    APPEND VALUE #( charact       = 'INSTRUCCIONES'
                    value_char    = gs_pstinstruc-tipo
                    value_neutral = gs_pstinstruc-tipo ) TO lt_allocvalueschar.

    CLEAR lv_date.
    lv_date = sy-datum.
    CALL FUNCTION 'BAPI_OBJCL_CREATE'
      EXPORTING
        objectkeynew    = lv_objectkey
        objecttablenew  = 'KNA1'
        classnumnew     = lv_classnum
        classtypenew    = lv_classtype
        status          = '1'  " Opcional, 1 = activo
        keydate         = lv_date
      TABLES
        allocvaluesnum  = lt_allocvaluesnum
        allocvalueschar = lt_allocvalueschar
        allocvaluescurr = lt_allocvaluescurr
        return          = lt_return.

    LOOP AT lt_return INTO DATA(ls_return).
      IF ls_return-type EQ 'E'
      OR ls_return-type EQ 'A'.
        lv_error = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_error = abap_false.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_crear_direcciones_entrega
*&---------------------------------------------------------------------*
FORM f_crear_direcciones_entrega  TABLES   lt_gnrales STRUCTURE gs_gnrales
                                           "_t_data STRUCTURE gt_data
                                  USING    chk_test.

  TYPES:
    "! Range
    BEGIN OF ty_range,
      sign   TYPE c LENGTH 1,
      option TYPE c LENGTH 2,
      low    TYPE string,
      high   TYPE string,
    END OF ty_range.

  DATA: lt_but000    TYPE TABLE OF but000,
        lt_select    LIKE t_gnrales,
        lt_ranges    TYPE TABLE OF ty_range,
        lv_cont      TYPE n LENGTH 3,
        lv_cont_cre  TYPE sy-tabix,
        lv_bp_format TYPE char6,
        lv_bp_direc  TYPE kunnr.

  DATA: lt_object_keys TYPE STANDARD TABLE OF ty_object_key WITH HEADER LINE. "DNAVOA
  DATA: ls_data_d TYPE ty_data_d.

  DESCRIBE TABLE lt_gnrales LINES DATA(lv_cant_bp).

  LOOP AT  lt_gnrales ASSIGNING FIELD-SYMBOL(<lfs_data>).
    CLEAR: lt_object_keys[], lt_object_keys[], lv_cont, lv_cont_cre. "DNAVOA
    FREE: lt_select, lt_ranges, lt_but000.

    READ TABLE lt_data_deep WITH KEY code = <lfs_data>-code INTO DATA(ls_data_aux). "DNAVOA
    IF sy-subrc EQ 0.
      READ TABLE ls_data_aux-lt_data INDEX 1 INTO ls_data.
    ENDIF.

    DATA(ls_aux_sales) = ls_data-customer-sales_data-sales. "DNAVOA

    lv_bp_format = |{ <lfs_data>-code ALPHA = IN }|. " Ajuste por digitos DNAVOA

    lv_bp = <lfs_data>-code.

    lt_select = VALUE #( FOR wa IN lt_gnrales ( code = wa-code    typereg = |{ 'D' }{ wa-code }{ '*' }| ) ).
    lt_ranges = VALUE #( FOR wa_range IN lt_select ( sign   = 'I' option = 'CP' low = wa_range-typereg ) ).

    "Obtener N° Bp's de Direcciones creadas.
    SELECT client partner FROM but000
      INTO TABLE lt_but000
      WHERE partner IN lt_ranges.
    IF sy-subrc EQ 0.
      SORT lt_but000 BY partner DESCENDING.

      DELETE lt_but000 WHERE partner+1(6) <> lv_bp_format. "DNAVOA

      READ TABLE lt_but000 INDEX 1 INTO DATA(lwa_but000).
      lv_cont = lwa_but000-partner+7(3).
      lv_cont += 1.
      lv_bp_direc = |{ 'D' }{ lv_bp_format }{ lv_cont }|.
    ELSE.
      lv_cont += 1.
      lv_bp_direc = |{ 'D' }{ lv_bp_format }{ lv_cont }|.
    ENDIF.

    SORT t_pstempresa BY code orgventa.

    "Direcciones De Entrega
    LOOP AT t_pstentrega INTO gs_pstentrega
      WHERE code = lv_bp.

      READ TABLE ls_data_aux-lt_data
        INDEX 1 INTO ls_data. "DNAVOA se comenta para mantener coherencia.

      "limpiar las estructuras no necesarias para Creción de BP Direcciones
      FREE:
            ls_data-partner-central_data-role-roles,
            ls_data-partner-central_data-bankdetail,
            ls_data-partner-central_data-taxnumber,
            ls_data-partner-central_data-address-addresses,
            ls_data-partner-ukmbp_data,
            ls_data-customer-company_data,
            ls_data-customer-company_data,
            ls_data-vendor,
            lt_roles, lt_sales.

      lv_cont_cre += 1.
      IF lv_cont_cre GT 1.
        lv_cont += 1.
        lv_bp_direc = |{ 'D' }{ lv_bp_format }{ lv_cont }|.
      ENDIF.

      ls_data-partner-header-object_instance-bpartner                     = lv_bp_direc.
      ls_data-partner-header-object_instance-bpartnerguid                 = lv_bp_direc.

      ls_data-partner-central_data-common-data-bp_control-grouping        = 'ZDM'.

      ls_data-partner-central_data-common-data-bp_centraldata-searchterm1 = gs_pstentrega-namect.
      ls_data-partner-central_data-common-data-bp_centraldata-searchterm2 = gs_pstentrega-descdir.

      "Cambio en nombre de direcciones de entrega
      ls_data-partner-central_data-common-data-bp_organization-name2 = ls_data-partner-central_data-common-data-bp_organization-name1.
      ls_data-partner-central_data-common-data-bp_organization-name1 = ls_data-partner-central_data-common-data-bp_centraldata-searchterm1.

      ""--> ADDRESSESS <--""
      ls_address-data-postal-data-city                                    = gs_pstentrega-municpo.
      ls_address-data-postal-data-district                                = gs_pstentrega-colonia.
      ls_address-data-postal-data-postl_cod1                              = gs_pstentrega-codepos.
      ls_address-data-postal-data-street                                  = gs_pstentrega-calle.
      ls_address-data-postal-data-house_no                                = gs_pstentrega-numext.
      ls_address-data-postal-data-country                                 = gs_pstentrega-pais.
      ls_address-data-postal-data-countryiso                              = gs_pstentrega-pais.
      ls_address-data-postal-data-region                                  = gs_pstentrega-estado.
      ls_address-data-postal-data-languiso                                = gs_pstentrega-idioma.

      ls_address-data-postal-datax-city                                   = COND #( WHEN ls_address-data-postal-data-city            IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_address-data-postal-datax-district                               = COND #( WHEN ls_address-data-postal-data-district        IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_address-data-postal-datax-postl_cod1                             = COND #( WHEN ls_address-data-postal-data-postl_cod1      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_address-data-postal-datax-street                                 = COND #( WHEN ls_address-data-postal-data-street          IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_address-data-postal-datax-house_no                               = COND #( WHEN ls_address-data-postal-data-house_no        IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_address-data-postal-datax-country                                = COND #( WHEN ls_address-data-postal-data-country         IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_address-data-postal-datax-countryiso                             = COND #( WHEN ls_address-data-postal-data-countryiso      IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_address-data-postal-datax-region                                 = COND #( WHEN ls_address-data-postal-data-region          IS NOT INITIAL THEN abap_true ELSE abap_false ).
      ls_address-data-postal-datax-langu_iso                              = COND #( WHEN ls_address-data-postal-data-languiso       IS NOT INITIAL THEN abap_true ELSE abap_false ).

      APPEND ls_address TO ls_data-partner-central_data-address-addresses.


      ""--> ROLES <--""
      ls_roles-data-rolecategory = lc_crm002.
      APPEND ls_roles TO lt_roles.
      CLEAR ls_roles.

      ls_roles-data-rolecategory = lc_flcu01.
      APPEND ls_roles TO lt_roles.
      CLEAR ls_roles.
      ls_data-partner-central_data-role-roles = lt_roles.


      ""--> CUSTOMER <--""
      ls_data-customer-header-object_instance-kunnr                       = lv_bp_direc.

      ls_data-customer-sales_data-sales = ls_aux_sales.
      " --> SALES <--""
      LOOP AT ls_data-customer-sales_data-sales ASSIGNING FIELD-SYMBOL(<lwa_sales>).
        DATA(lv_key) = sy-tabix.

** INI DNAVOA 13.06.2025
        CLEAR <lwa_sales>-functions-functions.

        "Consulta Itab Direccion Entrega-Empresa
        READ TABLE t_pstempresa WITH KEY code = gs_pstentrega-code orgventa = <lwa_sales>-data_key-vkorg
             TRANSPORTING NO FIELDS BINARY SEARCH.
        IF sy-subrc NE 0.
          DELETE ls_data-customer-sales_data-sales INDEX lv_key.
        ELSEIF sy-subrc EQ 0.
          <lwa_sales>-data-bzirk = gs_pstempresa-zona_exp.
          <lwa_sales>-data-vsbed = gs_pstempresa-cond_exp.

          <lwa_sales>-datax-bzirk = COND #( WHEN <lwa_sales>-data-bzirk IS NOT INITIAL THEN abap_true ELSE abap_false ).
          <lwa_sales>-datax-vsbed = COND #( WHEN <lwa_sales>-data-vsbed IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ENDIF.
** FIN DNAVOA 13.06.2025
      ENDLOOP.

      REFRESH lt_data.
      APPEND ls_data TO lt_data.
      CLEAR ls_data.

** INI DNAVOA
      "Llenado de direcciones con D a estructura deep
      READ TABLE lt_data_deep ASSIGNING FIELD-SYMBOL(<fs_data_deep>) WITH KEY code = <lfs_data>-code.
      IF sy-subrc EQ 0.
        CLEAR ls_data_d.
        ls_data_d-lt_direcc_d = lt_data.
        APPEND ls_data_d TO <fs_data_deep>-lt_data_d.
      ENDIF.
** FIN DNAVOA

      " ---> LLAMAMOS CREACIÓN DEL BP---
      CALL METHOD cl_md_bp_maintain=>maintain
        EXPORTING
          i_data     = lt_data
          i_test_run = chk_test
        IMPORTING
          e_return   = t_return_d.

      IF t_return_d IS INITIAL.

        IF chk_test IS INITIAL.
          gv_mess  = 'BP Creado'.
        ELSE.
          gv_mess  = 'BP Listo para Creación'.
        ENDIF.

        gs_mess = VALUE #( type    = 'S'
                           message = gv_mess ).
        APPEND gs_mess TO gt_mess.

        t_return_d = VALUE #( ( object_idx = 1
                                object_key = gs_pstentrega-code
                                object_msg = gt_mess
                          ) ).
        CLEAR gv_mess.
      ENDIF.

      READ TABLE t_return_d INTO gs_return INDEX 1.
      lv_cant_bp += lv_key.
      PERFORM carga_alv
       TABLES gs_return-object_msg
        USING gs_return-object_key
              lv_cant_bp cx-atbps"--> Alta de BPS
     CHANGING lv_tipomsje.

      CLEAR lt_object_keys-key.
      lt_object_keys-key = gs_return-object_key.
      lt_object_keys-type = gs_pstentrega-namect.
      APPEND lt_object_keys.

    ENDLOOP.

** INI DNAVOA 06.06.2025
    IF lt_object_keys IS NOT INITIAL AND chk_test IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      LOOP AT lt_object_keys INTO DATA(ls_object_key_aux).
        PERFORM f_direcciones_adicionales USING ls_object_key_aux <lfs_data>-code.
      ENDLOOP.
    ENDIF.
** FIN DNAVOA 06.06.2025

  ENDLOOP.

ENDFORM.

FORM f_herramientas_prov
    USING
          lv_code_general TYPE char10.

  DATA: lt_charvalues TYPE TABLE OF bapi1003_alloc_values_char,
        ls_charvalues LIKE LINE OF lt_charvalues,
        lt_return2    TYPE TABLE OF bapiret2,
        lv_class_num  TYPE bapi1003_key-classnum.

  DATA: lv_bp    TYPE bapi1003_key-object,
        lv_lfa1  TYPE bapi1003_key-objecttable,
        lv_010   TYPE bapi1003_key-classtype,
        lv_1     TYPE bapi1003_key-status,
        lv_datum TYPE bapi1003_key-keydate,
        lv_code  TYPE char10.

  CONSTANTS: lc_class_prov(10) TYPE c VALUE 'CLASS_PROV',
             lc_lfa1(4)        TYPE c VALUE 'LFA1',
             lc_010(3)         TYPE c VALUE '010',
             lc_1              TYPE c VALUE '1'.


  lv_code   = lv_code_general.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_code_general
    IMPORTING
      output = lv_code_general.

  lv_bp     = lv_code_general.
  lv_lfa1   = lc_lfa1.
  lv_010    = lc_010.
  lv_1      = lc_1.
  lv_datum  = sy-datum.

***  ¨Proveedor - Clasificacion
  IF t_pstprovcla IS NOT INITIAL.

    WAIT UP TO 1 SECONDS.

    CLEAR: lt_charvalues[], lt_return2[], ls_charvalues, gs_pstprovcla.
    LOOP AT t_pstprovcla INTO gs_pstprovcla WHERE code EQ lv_code.
      CLEAR: ls_charvalues.

      IF gs_pstprovcla-tipo_prov IS NOT INITIAL.
        ls_charvalues-charact = 'TIPO_PROVEEDOR'.
        ls_charvalues-value_char = gs_pstprovcla-tipo_prov.
        APPEND ls_charvalues TO lt_charvalues.
      ENDIF.

      IF gs_pstprovcla-region IS NOT INITIAL.
        ls_charvalues-charact = 'REGION'.
        ls_charvalues-value_char = gs_pstprovcla-region.
        APPEND ls_charvalues TO lt_charvalues.
      ENDIF.

      IF gs_pstprovcla-esp_verd IS NOT INITIAL.
        ls_charvalues-charact = 'ESPACIO_VERDE'.
        ls_charvalues-value_char = gs_pstprovcla-esp_verd.
        APPEND ls_charvalues TO lt_charvalues.
      ENDIF.

      IF gs_pstprovcla-index IS NOT INITIAL.
        ls_charvalues-charact = 'INDEX'.
        ls_charvalues-value_char = gs_pstprovcla-index.
        APPEND ls_charvalues TO lt_charvalues.
      ENDIF.

      IF gs_pstprovcla-clave_imp IS NOT INITIAL.
        ls_charvalues-charact = 'CL_IMPUESTO'.
        ls_charvalues-value_char = gs_pstprovcla-clave_imp.
        APPEND ls_charvalues TO lt_charvalues.
      ENDIF.

      IF gs_pstprovcla-apl_desc IS NOT INITIAL.
        ls_charvalues-charact = 'APLICA_DESCUENTOS'.
        ls_charvalues-value_char = gs_pstprovcla-apl_desc.
        APPEND ls_charvalues TO lt_charvalues.
      ENDIF.

      IF gs_pstprovcla-promotor IS NOT INITIAL.
        ls_charvalues-charact = 'PROMOTOR'.
        ls_charvalues-value_char = gs_pstprovcla-promotor.
        APPEND ls_charvalues TO lt_charvalues.
      ENDIF.

      lv_class_num = lc_class_prov.

    ENDLOOP.

    IF lt_charvalues IS NOT INITIAL.

      CALL FUNCTION 'BAPI_OBJCL_CREATE'
        EXPORTING
          objectkeynew    = lv_bp
          objecttablenew  = lv_lfa1
          classnumnew     = lv_class_num
          classtypenew    = lv_010
          status          = lv_1
          keydate         = lv_datum
        TABLES
          allocvalueschar = lt_charvalues
          return          = lt_return2.

    ENDIF.

  ENDIF.

ENDFORM.

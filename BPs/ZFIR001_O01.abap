*&---------------------------------------------------------------------*
*& Include          ZFIR001_O01
*&---------------------------------------------------------------------*

MODULE bps_active_tab_set OUTPUT.

  bps-activetab = g_bps-pressed_tab.

  CASE g_bps-pressed_tab.
    WHEN c_bps-tab1.
      g_bps-subscreen = '0002'.
    WHEN c_bps-tab2.
      g_bps-subscreen = '0003'.
    WHEN c_bps-tab3.
      g_bps-subscreen = '0004'.
    WHEN c_bps-tab4.
      g_bps-subscreen = '0005'.
    WHEN c_bps-tab5.
      g_bps-subscreen = '0006'.
    WHEN c_bps-tab6.
      g_bps-subscreen = '0007'.
    WHEN c_bps-tab7.
      g_bps-subscreen = '0008'.
    WHEN c_bps-tab8.
      g_bps-subscreen = '0009'.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0001 OUTPUT
*&---------------------------------------------------------------------*
*& Status de Dynpro de Log de Proceso
*&---------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  DATA lv_regis TYPE i.
  DATA vg_ejec  TYPE c LENGTH 15.

  DATA lc_prb TYPE c LENGTH 11 VALUE 'Tipo Prueba'. " Tipo Prueba
  DATA lc_crg TYPE c LENGTH 11 VALUE 'Tipo Carga'. " Modo Carga o Creacion.


  CONSTANTS lc_alta(11)  TYPE c VALUE 'Alta de BPS'.
  CONSTANTS lc_carga(12) TYPE c VALUE 'Carga Anexos'.
  CONSTANTS lc_modif(13) TYPE c VALUE 'Modificar BPS'.
  CONSTANTS lc_altad(14) TYPE c VALUE 'Alta Direccion'.
  CONSTANTS lc_acint(23) TYPE c VALUE 'Actualizar Interlocutor'.
*-----------------------------------------------------------------

  SET PF-STATUS 'ZSTATUS_LOG'.

  " --> Fecha de Ejecucion
  CONCATENATE sy-datum+6(2)
              sy-datum+4(2)
              sy-datum(4)
         INTO gv_fecha SEPARATED BY '/'.

  " --> Total de Registros
  DESCRIBE TABLE t_alvlog LINES lv_regis.

  MOVE lv_regis TO gv_total.
  CONDENSE gv_total.

  " --> Nombre de Usuario
  SELECT SINGLE name_text
  FROM user_addrp INTO gv_usuario
  WHERE bname EQ sy-uname.

  " --> Proceso
  CASE abap_true.
    WHEN r_crbps. " Alta de BPS
      gv_proceso = lc_alta.
    WHEN r_mdbps. " Modificacion BPS
      gv_proceso = lc_modif.
    WHEN r_acint. " Actualizacion Interlocutor
      gv_proceso = lc_acint.
    WHEN r_atdir. " Alta Direcciones
      gv_proceso = lc_altad.
    WHEN r_cranx. " Carga Anexos
      gv_proceso = lc_carga.
  ENDCASE.

  CASE abap_true.
    WHEN chk_test.
      vg_ejec = lc_prb. " Tipo Prueba
    WHEN OTHERS.
      vg_ejec = lc_crg. " Modo Carga o Creacion.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DISPLAY_ALV OUTPUT
*&---------------------------------------------------------------------*
*& Muestra de Reporte ALV - Log de Proceso Principal
*&---------------------------------------------------------------------*
MODULE display_alv OUTPUT.

  DATA vg_error  TYPE c LENGTH 10.
  DATA vg_exito  TYPE c LENGTH 10.
  DATA vg_total  TYPE c LENGTH 10.
  DATA vg_advert TYPE c LENGTH 10.
*&-----------------------------------------------------------------
  IF obj_contlog IS INITIAL.

    CREATE OBJECT o_event.

    CREATE OBJECT obj_contlog "Creating container object
      EXPORTING
        container_name = gv_contlog.

    CREATE OBJECT obj_gridlog     "Creating AlV Grid Object
      EXPORTING
        i_parent = obj_contlog.

    " Catalogo
    PERFORM crea_catalog USING 'LOG'.

    " Layout
    PERFORM crea_layout.

    IF  t_alvlog  IS NOT INITIAL
    AND vg_advert IS INITIAL
    AND vg_error  IS INITIAL
    AND vg_exito  IS INITIAL
    AND vg_total  IS INITIAL.

      PERFORM totales_regis
      USING 'LOG'
      CHANGING vg_advert vg_error vg_exito vg_total.

    ENDIF.

    CALL METHOD obj_gridlog->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        is_layout       = gs_layout
      CHANGING
        it_outtab       = t_alvlog
        it_fieldcatalog = t_fcat.


    SET HANDLER o_event->handle_double_click FOR obj_gridlog.

  ENDIF.

  CALL METHOD obj_gridlog->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DISPLAY_ALVGRNALES OUTPUT
*&---------------------------------------------------------------------*
*& Muestra ALV de Pestañas Generales
*&---------------------------------------------------------------------*
MODULE display_alvgrnales OUTPUT.

  DATA vg_totgnral TYPE c LENGTH 10.
* -----------------------------------------------
  IF obj_contgnral IS INITIAL.

    CREATE OBJECT obj_contgnral "Creating container object
      EXPORTING
        container_name = gv_contgnral.

    CREATE OBJECT obj_gridgnral     "Creating AlV Grid Object
      EXPORTING
        i_parent = obj_contgnral.

    " Catalogo
    PERFORM crea_catalog USING 'GNRAL'.

    " Layout
    IF gs_layout IS INITIAL.

      PERFORM crea_layout.

    ENDIF.

    IF  t_gnrales  IS NOT INITIAL
    AND vg_total  IS INITIAL.

      PERFORM totales_regis
        USING 'GNRAL'
     CHANGING vg_advert vg_error
              vg_exito  vg_totgnral.

    ENDIF.

    CALL METHOD obj_gridgnral->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        is_layout       = gs_layout
      CHANGING
        it_outtab       = t_gnrales
        it_fieldcatalog = t_fcat.

  ENDIF.

  CALL METHOD obj_gridgnral->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DISPLAY_ALVDIRECENTGA OUTPUT
*&---------------------------------------------------------------------*
*&  Display ALV - Direccion de Entrega
*&---------------------------------------------------------------------*
MODULE display_alvdirecentga OUTPUT.

  DATA vg_totdirentg TYPE c LENGTH 10.
*-----------------------------------------------------*
    IF obj_contdirentga IS INITIAL.

  CREATE OBJECT obj_contdirentga "Creating container object
    EXPORTING
      container_name = gv_contdiren.

  CREATE OBJECT obj_griddirentga     "Creating AlV Grid Object
    EXPORTING
      i_parent = obj_contdirentga.

  " Catalogo
  PERFORM crea_catalog USING 'DIREC'.

  " Layout
  PERFORM crea_layout.

  IF  t_direntg IS NOT INITIAL
  AND vg_total  IS INITIAL.

    PERFORM totales_regis
      USING 'DIRENT'
   CHANGING vg_advert vg_error
            vg_exito  vg_totdirentg.

  ENDIF.

  CALL METHOD obj_griddirentga->set_table_for_first_display
    EXPORTING
      i_save          = abap_true
      is_layout       = gs_layout
    CHANGING
      it_outtab       = t_direntg
      it_fieldcatalog = t_fcat.

ENDIF.

CALL METHOD obj_griddirentga->set_ready_for_input
  EXPORTING
    i_ready_for_input = 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DISPLAY_ALVCNTEFINAN OUTPUT
*&---------------------------------------------------------------------*
*& Mostrar Reporte ALV de Pestaña Cliente Financiero
*&---------------------------------------------------------------------*
MODULE display_alvcntefinan OUTPUT.

  IF obj_contcntfin IS INITIAL.

    CREATE OBJECT obj_contcntfin "Creating container object
      EXPORTING
        container_name = gv_contcntfn.

    CREATE OBJECT obj_gridcntfin     "Creating AlV Grid Object
      EXPORTING
        i_parent = obj_contcntfin.

    " Catalogo
    PERFORM crea_catalog USING 'CNTFIN'.

    " Layout
    IF gs_layout IS INITIAL.

      PERFORM crea_layout.

    ENDIF.

    IF  t_cntefinan  IS NOT INITIAL
    AND vg_total  IS INITIAL.

      PERFORM totales_regis
        USING 'CNTFIN'
     CHANGING vg_advert vg_error
              vg_exito  vg_total.

    ENDIF.


    CALL METHOD obj_gridcntfin->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        is_layout       = gs_layout
      CHANGING
        it_outtab       = t_cntefinan
        it_fieldcatalog = t_fcat.

  ENDIF.

  CALL METHOD obj_gridcntfin->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.


ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DISPLAY_ALVCNTEVNTAS OUTPUT
*&---------------------------------------------------------------------*
*& Display de Reporte ALV - Cliente de Ventas
*&---------------------------------------------------------------------*
MODULE display_alvcntevntas OUTPUT.

  DATA vg_totcntvnta TYPE C LENGTH 10.
*---------------------------------------------
  IF obj_contcntvnta IS INITIAL.

    CREATE OBJECT obj_contcntvnta "Creating container object
      EXPORTING
        container_name = gv_contcntvn.

    CREATE OBJECT obj_gridcntvnta     "Creating AlV Grid Object
      EXPORTING
        i_parent = obj_contcntvnta.

    " Catalogo
    PERFORM crea_catalog USING 'CNTVNT'.

    " Layout
    IF gs_layout IS INITIAL.

      PERFORM crea_layout.

    ENDIF.

    IF  t_cntevntas IS NOT INITIAL
    AND vg_total    IS INITIAL.

      PERFORM totales_regis
        USING 'CNTVNT'
     CHANGING vg_advert vg_error
              vg_exito  vg_totcntvnta.

    ENDIF.

    CALL METHOD obj_gridcntvnta->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        is_layout       = gs_layout
      CHANGING
        it_outtab       = t_cntevntas
        it_fieldcatalog = t_fcat.

  ENDIF.

  CALL METHOD obj_gridcntvnta->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.


ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DISPLAY_ALVCNTEVNTAS OUTPUT
*&---------------------------------------------------------------------*
*& Display de Reporte ALV - Proveedor
*&---------------------------------------------------------------------*
MODULE display_alvproveedor OUTPUT.

  DATA vg_totprov TYPE c LENGTH 10.
*------------------------------------------------
  IF obj_contprov IS INITIAL.

    CREATE OBJECT obj_contprov "Creating container object
      EXPORTING
        container_name = gv_contprove.

    CREATE OBJECT obj_gridprov     "Creating AlV Grid Object
      EXPORTING
        i_parent = obj_contprov.

    " Catalogo
    PERFORM crea_catalog USING 'PROVE'.

    " Layout
    IF gs_layout IS INITIAL.

      PERFORM crea_layout.

    ENDIF.

    IF  t_provedor IS NOT INITIAL
    AND vg_total  IS INITIAL.

      PERFORM totales_regis
        USING 'PROVE'
     CHANGING vg_advert vg_error
              vg_exito  vg_totprov.

    ENDIF.


    CALL METHOD obj_gridprov->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        is_layout       = gs_layout
      CHANGING
        it_outtab       = t_provedor
        it_fieldcatalog = t_fcat.

  ENDIF.

  CALL METHOD obj_gridprov->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.


ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DISPLAY_ALVPROVCOM OUTPUT
*&---------------------------------------------------------------------*
*& Display de Reporte ALV - Proveedor de Compras
*&---------------------------------------------------------------------*
MODULE display_alvprovcom OUTPUT.

  DATA vg_totprvcmp TYPE C LENGTH 10.
*------------------------------------------------------
  IF obj_contprvcomp IS INITIAL.

    CREATE OBJECT obj_contprvcomp "Creating container object
      EXPORTING
        container_name = gv_contpvcom.

    CREATE OBJECT obj_gridprvcomp     "Creating AlV Grid Object
      EXPORTING
        i_parent = obj_contprvcomp.

    " Catalogo
    PERFORM crea_catalog USING 'PRVCMP'.

    " Layout
    IF gs_layout IS INITIAL.

      PERFORM crea_layout.

    ENDIF.

    IF  t_provcomp IS NOT INITIAL
    AND vg_total   IS INITIAL.

      PERFORM totales_regis
        USING 'PRVCMP'
     CHANGING vg_advert vg_error
              vg_exito  vg_totprvcmp.

    ENDIF.

    CALL METHOD obj_gridprvcomp->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        is_layout       = gs_layout
      CHANGING
        it_outtab       = t_provcomp
        it_fieldcatalog = t_fcat.

  ENDIF.

  CALL METHOD obj_gridprvcomp->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.


ENDMODULE.


*&---------------------------------------------------------------------*
*& Module DISPLAY_ALVANEXO OUTPUT
*&---------------------------------------------------------------------*
*& Display de Reporte ALV - ANEXO
*&---------------------------------------------------------------------*
MODULE display_alvanexo OUTPUT.

  DATA vg_totanexo TYPE c LENGTH 10.
*--------------------------------------------------
  IF obj_contanexo IS INITIAL.

    CREATE OBJECT obj_contanexo "Creating container object
      EXPORTING
        container_name = gv_contanexo.

    CREATE OBJECT obj_gridanexo     "Creating AlV Grid Object
      EXPORTING
        i_parent = obj_contanexo.

    " Catalogo
    PERFORM crea_catalog USING 'ANEXO'.

    " Layout
    IF gs_layout IS INITIAL.

      PERFORM crea_layout.

    ENDIF.

    IF  t_provcomp IS NOT INITIAL
    AND vg_total   IS INITIAL.

      PERFORM totales_regis
        USING 'ANEXO'
     CHANGING vg_advert vg_error
              vg_exito  vg_totanexo.

    ENDIF.

    CALL METHOD obj_gridanexo->set_table_for_first_display
      EXPORTING
        i_save          = abap_true
        is_layout       = gs_layout
      CHANGING
        it_outtab       = t_anexo
        it_fieldcatalog = t_fcat.

  ENDIF.

  CALL METHOD obj_gridanexo->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Module IMAGEN_LOGO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE imagen_logo OUTPUT.

  IF init IS INITIAL.

*   create the custom container
    CREATE OBJECT container
      EXPORTING
        container_name = 'CUSTOM'.

*   create the picture control
    CREATE OBJECT picture
      EXPORTING
        parent = container.

*   Request an URL from the data provider.
    CLEAR url.
    PERFORM load_pic_from_db CHANGING url.

*   load picture
    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    init = 'X'.
    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.

    IF sy-subrc <> 0.
    ENDIF.

  ENDIF.

ENDMODULE.

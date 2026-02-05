*&---------------------------------------------------------------------*
*& Include          ZFIR001_I01
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TS 'BPS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE bps_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_bps-tab1.
      g_bps-pressed_tab = c_bps-tab1.
    WHEN c_bps-tab2.
      g_bps-pressed_tab = c_bps-tab2.
    WHEN c_bps-tab3.
      g_bps-pressed_tab = c_bps-tab3.
    WHEN c_bps-tab4.
      g_bps-pressed_tab = c_bps-tab4.
    WHEN c_bps-tab5.
      g_bps-pressed_tab = c_bps-tab5.
    WHEN c_bps-tab6.
      g_bps-pressed_tab = c_bps-tab6.
    WHEN c_bps-tab7.
      g_bps-pressed_tab = c_bps-tab7.
    WHEN c_bps-tab8.
      g_bps-pressed_tab = c_bps-tab8.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*     Control de Comandos de Usuario Dynpro Principal
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CLEAR ok_code.

  ok_code = sy-ucomm.

  CASE ok_code.
   WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

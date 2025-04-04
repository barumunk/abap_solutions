*&---------------------------------------------------------------------*
*& Include          ZMMRE_ACTUALIZA_PRECIOS_OC_M00
*&---------------------------------------------------------------------*
* Programa   : ZMMRE_ACTUALIZA_PRECIOS_OC                                *
* Descripción: Reporte para cambiar precios en Orden de Compra           *
* Programador: David Navoa Acevedo                                       *
* Fecha      : 06.06.2024                                                *
*------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
  SET TITLEBAR 'TITULO_001'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

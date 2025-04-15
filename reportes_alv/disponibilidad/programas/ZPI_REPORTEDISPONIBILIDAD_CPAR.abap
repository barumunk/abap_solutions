REPORT zpi_reportedisponibilidad_cpar.

INCLUDE zpi_rdisponibilidad_sspar2.
INCLUDE zpi_rdisponibilidad_clpar2.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name EQ 'S_WERKS'.
      screen-required = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name EQ 'S_VKORG'.
      screen-required = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

  DATA(lo_reporte) = NEW zcl_reporte( ).

  lo_reporte->obtener_datos( ).

  lo_reporte->mostrar_alv( ).

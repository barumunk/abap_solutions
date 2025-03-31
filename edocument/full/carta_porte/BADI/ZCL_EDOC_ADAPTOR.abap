  METHOD if_edoc_adaptor~set_output_data.
    FIELD-SYMBOLS: <fs_data>   TYPE edo_mx_cfdsubmit_request_type6,
                   <fs_source> TYPE edoc_src_data_sd_invoice.

    DATA: lv_xstring TYPE xstring.

    "Solo para Modulo SD / Invoice
*    IF iv_edoc_type EQ 'MX_INVOICE' AND sy-uname EQ 'E_SDRLWAY'.
    IF iv_edoc_type EQ 'MX_DELN' AND sy-uname EQ 'E_SDRLWAY'.
*    IF 2 EQ 1.

      "Data de tablas obtenidas en el EDOC
      DATA(ld_data) = io_source->get_data_reference( ).
      ASSIGN ld_data->* TO <fs_source>.

      "Data del XML
      ASSIGN cs_output_data TO <fs_data>.


      READ TABLE <fs_source>-partner_data INTO DATA(ls_partner_data) WITH KEY PARVw = 'WE'.
      IF sy-subrc EQ 0.

        "Modificacion de regimen fiscal receptor
        SELECT SINGLE bran5 FROM kna1
          INTO @DATA(lv_bran5)
          WHERE kunnr EQ @ls_partner_data-kunnr.
        IF sy-subrc EQ 0 AND lv_bran5 IS NOT INITIAL AND lv_bran5 NE ' '.
          <fs_data>-comprobante-comprobante-receptor-regimen_fiscal_receptor = lv_bran5.
        ENDIF.

        "Uso del CFDI y metodo de pago
        SELECT SINGLE * FROM knvv
          INTO @DATA(ls_knvv)
          WHERE vkorg EQ @<fs_source>-document_header-vkorg
          AND kunnr EQ @ls_partner_data-kunnr
          AND vtweg EQ @<fs_source>-document_header-vtweg
          AND spart EQ @<fs_source>-document_header-spart.
        IF sy-subrc EQ 0 AND ls_knvv-kvgr2 IS NOT INITIAL AND ls_knvv-kvgr2 NE ' '.
          <fs_data>-comprobante-comprobante-receptor-uso_cfdi = ls_knvv-kvgr2.
          <fs_data>-comprobante-comprobante-metodo_pago = ls_knvv-kvgr1.
        ENDIF.

        CALL FUNCTION 'ZSD_EDOC_CARTAPORTE'
          EXPORTING
            source  = <fs_source>
            data    = <fs_data>
          IMPORTING
            xstring = lv_xstring.

        APPEND lv_xstring TO <fs_data>-comprobante-comprobante-complemento-any.

      ENDIF.

    ENDIF.
  ENDMETHOD.

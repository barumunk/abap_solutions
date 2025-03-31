FUNCTION zsd_edoc_cartaporte.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(SOURCE) TYPE  EDOC_SRC_DATA_SD_INVOICE
*"     VALUE(DATA) TYPE  EDO_MX_CFDSUBMIT_REQUEST_TYPE6
*"  EXPORTING
*"     REFERENCE(XSTRING) TYPE  XSTRING
*"----------------------------------------------------------------------

  CONSTANTS: lc_version(3) TYPE c VALUE '3.1',
             lc_no(2)      TYPE c VALUE 'No',
             lc_origen(6)  TYPE c VALUE 'Origen',
             lc_destino(7) TYPE c VALUE 'Destino',
             lc_01(2)      TYPE c VALUE '01'.

  DATA: lv_uuid    TYPE string,
        lv_GUID_32 TYPE guid_32,
        lv_idccp   TYPE char32.

  DATA: lv_xml             TYPE string,
        lv_node            TYPE string,
        lv_value           TYPE string,
        lv_xstring         TYPE xstring,
        lv_distrec         TYPE distz,
        lv_idu             TYPE string,
        lv_num_mercancias  TYPE gsgew,
        lv_unidad_peso     TYPE gewei,
        "        lv_num_mercancias  TYPE lfimg,
        lv_bienes_trans    TYPE string,
        lv_desc_merc       TYPE arktx,
        lv_cant_merc       TYPE string,
        lv_clave_und_merc  TYPE vrkme,
        lv_peso_merc       TYPE brgew,
        lv_cantidad_transp TYPE string,
        lv_idorigen        TYPE string,
        lv_iddestino       TYPE string,
        lv_rfc_remitente.

  TYPES:
    BEGIN OF ty_identificacion_vehi,
      config_vehic   TYPE string,
      peso_vehicular TYPE string,
      placa          TYPE string,
      anio_mod       TYPE string,
    END OF ty_identificacion_vehi.

  TYPES:
    BEGIN OF ty_seguros,
      asegura TYPE string,
      poliza  TYPE string,
    END OF ty_seguros.

  TYPES:
    BEGIN OF ty_remolque,
      subtiporem TYPE string,
      placa      TYPE string,
    END OF ty_remolque.

  TYPES: tt_remolque TYPE STANDARD TABLE OF ty_remolque WITH KEY subtiporem placa.

  TYPES:
    BEGIN OF ty_autotransporte,
      permsct             TYPE string,
      num_permisosct      TYPE string,
      identificacion_vehi TYPE ty_identificacion_vehi,
      seguros             TYPE ty_seguros,
      remolque            TYPE tt_remolque,
    END OF ty_autotransporte.

  TYPES:
    BEGIN OF ty_cantidad_transportada,
      cantidad   TYPE string,
      um         TYPE string,
      id_origen  TYPE string,
      id_destino TYPE string,
    END OF ty_cantidad_transportada.

  TYPES:
    BEGIN OF ty_mercancia,
      bienes_transp         TYPE string,
      clave_unidad          TYPE string,
      descripcion           TYPE string,
      cantidad              TYPE string,
      peso_en_kg            TYPE string,
      cantidad_transportada TYPE ty_cantidad_transportada,
    END OF ty_mercancia.

  TYPES: tt_mercancia TYPE STANDARD TABLE OF ty_mercancia WITH KEY bienes_transp clave_unidad.

  TYPES:
    BEGIN OF ty_mercancias,
      peso_bruto      TYPE string,
      unidad_peso     TYPE string,
      num_total_merc  TYPE string,
      mercancia       TYPE tt_mercancia,
      auto_transporte TYPE ty_autotransporte,
    END OF ty_mercancias.

  TYPES:
    BEGIN OF ty_tipos_figura,
      tipo_figura TYPE string,
      rfc_figura  TYPE string,
      num_lic     TYPE string,
    END OF ty_tipos_figura.

  TYPES:
    BEGIN OF ty_domicilio,
      municipio     TYPE string,
      estado        TYPE string,
      pais          TYPE string,
      codigo_postal TYPE string,
    END OF ty_domicilio.

  TYPES:
    BEGIN OF ty_ubicacion,
      tipo_ubicacion TYPE string,
      id_ubicacion   TYPE string,
      rfc_remitente  TYPE string,
      fecha_hora_s_l TYPE string,
      domicilio      TYPE ty_domicilio,
    END OF ty_ubicacion.

  TYPES:
    BEGIN OF ty_carta_p,
      version    TYPE char3,
      id_ccp     TYPE xstring,
      transp     TYPE char2,
      total_dist TYPE distz,
    END OF ty_carta_p.

  TYPES:
    BEGIN OF ty_carta_porte,
      carta_porte       TYPE ty_carta_p,
      ubicacion_origen  TYPE ty_ubicacion,
      ubicacion_destino TYPE ty_ubicacion,
      mercancias        TYPE ty_mercancias,
      tipos_figura      TYPE ty_tipos_figura,
    END OF ty_carta_porte.

  DATA: lt_carta_porte TYPE TABLE OF ty_carta_porte,
        ls_carta_porte TYPE ty_carta_porte,
        ls_mercancia   TYPE ty_mercancia,
        ls_remolque    TYPE ty_remolque.

  REFRESH lt_carta_porte.
  CLEAR: ls_carta_porte, ls_mercancia.

*************** Obtencion de Datos **************************

*** idCCP
  " Generar UUID
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_32 = lv_GUID_32.

  lv_uuid = lv_GUID_32.

  " Reemplazar los primeros 3 caracteres por 'CCC'
  lv_idccp = 'CCC' && lv_uuid+3.
  ls_carta_porte-carta_porte-id_ccp = lv_idccp.

  "version
  ls_carta_porte-carta_porte-version = lc_version.
  "transpIntern
  ls_carta_porte-carta_porte-transp = lc_no.
  "TotalDistRec
  ls_carta_porte-carta_porte-total_dist = '0.00'.

  SELECT SINGLE * FROM likp INTO @DATA(ls_likp)
    WHERE vbeln EQ @source-document_header-vbeln.
  IF sy-subrc EQ 0.

    SELECT * FROM lips
      INTO TABLE @DATA(lt_lips)
      WHERE vbeln EQ @ls_likp-vbeln.
    IF sy-subrc EQ 0.

      "peso_bruto
      ls_carta_porte-mercancias-peso_bruto = ls_likp-btgew.
      "unidad_peso
      ls_carta_porte-mercancias-unidad_peso = ls_likp-gewei.

      LOOP AT lt_lips INTO DATA(ls_lips).

        "Ubicacion
        SELECT SINGLE * FROM t001w
          INTO @DATA(ls_t001w)
          WHERE werks EQ @ls_lips-werks.
        IF sy-subrc EQ 0.

          "tipo_ubicacion
          ls_carta_porte-ubicacion_origen-tipo_ubicacion = lc_origen.
          "id_ubicacion
          ls_carta_porte-ubicacion_origen-id_ubicacion = 'OR' && ls_t001w-pstlz.
          "rfc_remitente
          ls_carta_porte-ubicacion_origen-rfc_remitente = data-comprobante-comprobante-emisor-rfc.
          "fecha_saliente
          ls_carta_porte-ubicacion_origen-fecha_hora_s_l = ls_likp-wadat.

          "municipio
          SELECT SINGLE sat  FROM ztsat_municipios
            INTO @ls_carta_porte-ubicacion_origen-domicilio-municipio
            WHERE land1 EQ @ls_t001w-land1
            AND   regio EQ @ls_t001w-regio.
          IF sy-subrc NE 0.
            ls_carta_porte-ubicacion_origen-domicilio-municipio = '000'.
          ENDIF.
          "Estado
          ls_carta_porte-ubicacion_origen-domicilio-estado           = ls_t001w-regio.
          "pais
          ls_carta_porte-ubicacion_origen-domicilio-pais             = ls_t001w-land1.
          "codigo_postal
          ls_carta_porte-ubicacion_origen-domicilio-codigo_postal    = ls_t001w-pstlz.


          SELECT SINGLE * FROM kna1
            INTO @DATA(ls_kna1)
            WHERE kunnr EQ @ls_likp-kunnr."source-document_header-kunag.
          IF sy-subrc EQ 0.
            "tipo ubicacion
            ls_carta_porte-ubicacion_destino-tipo_ubicacion = lc_destino.
            "id_ubicacion
            ls_carta_porte-ubicacion_destino-id_ubicacion = 'DE' && ls_kna1-pstlz.
            "remitente
            ls_carta_porte-ubicacion_destino-rfc_remitente = ls_kna1-stcd1.

            ls_carta_porte-ubicacion_origen-fecha_hora_s_l = ls_likp-wadat_ist && ls_likp-spe_wauhr_ist.

            SELECT SINGLE sat  FROM ztsat_municipios
              INTO @ls_carta_porte-ubicacion_destino-domicilio-municipio
              WHERE land1 EQ @ls_kna1-land1
              AND   regio EQ @ls_kna1-regio.
            IF sy-subrc NE 0.
              ls_carta_porte-ubicacion_destino-domicilio-municipio = '000'.
            ENDIF.

            ls_carta_porte-ubicacion_destino-domicilio-estado = ls_kna1-regio.
            ls_carta_porte-ubicacion_destino-domicilio-pais = ls_kna1-land1.
            ls_carta_porte-ubicacion_destino-domicilio-codigo_postal   = ls_kna1-pstlz.

          ENDIF.

        ENDIF.

        "numTotalMercancias
        ls_carta_porte-mercancias-num_total_merc += ls_lips-lfimg.
        "BienesTransp
        SELECT SINGLE * FROM mara INTO @DATA(ls_mara)
          WHERE matnr EQ @ls_lips-matnr.
        IF sy-subrc EQ 0.
          ls_mercancia-bienes_transp = ls_mara-normt.
        ELSE.
          ls_mercancia-bienes_transp = '10121600'. "informacion dummy
        ENDIF.

        "Descripcion
        ls_mercancia-descripcion = ls_lips-arktx.
        "Cantidad
        ls_mercancia-cantidad = ls_lips-lfimg.
        "ClaveUnidad
        ls_mercancia-clave_unidad = ls_lips-vrkme.
        "PesoEnKG
        ls_mercancia-peso_en_kg = ls_lips-brgew.

        "cantidad
        ls_mercancia-cantidad_transportada-cantidad = ls_lips-lfimg.
        "unidad de medida
        ls_mercancia-cantidad_transportada-um       = ls_lips-meins.
        "IDOrigen
        ls_mercancia-cantidad_transportada-id_origen = ls_carta_porte-ubicacion_origen-id_ubicacion.
        "IDDestino
        ls_mercancia-cantidad_transportada-id_destino = ls_carta_porte-ubicacion_destino-id_ubicacion.

        APPEND ls_mercancia TO ls_carta_porte-mercancias-mercancia.

      ENDLOOP.

    ENDIF.

    CONDENSE ls_likp-bolnr NO-GAPS.
    SPLIT ls_likp-bolnr AT '/' INTO TABLE DATA(lt_vehiculos).

    "Auto Transporte
    LOOP AT lt_vehiculos INTO DATA(ls_vehiculo).
      DATA(lv_tabix) = sy-tabix.

      SELECT SINGLE * FROM equi INTO @DATA(ls_equi)
        WHERE equnr EQ @ls_vehiculo.
      IF sy-subrc EQ 0  AND lv_tabix EQ 1.
        ls_carta_porte-mercancias-auto_transporte-permsct = ls_equi-zzpersct.
        ls_carta_porte-mercancias-auto_transporte-num_permisosct = ls_equi-zznupsct.

        ls_carta_porte-mercancias-auto_transporte-seguros-asegura = ls_equi-zzpolsun.
        ls_carta_porte-mercancias-auto_transporte-seguros-poliza = ls_equi-zznomseg.
        ls_carta_porte-mercancias-auto_transporte-identificacion_vehi-config_vehic = ls_equi-zzconveh.

        SELECT SINGLE license_num FROM fleet
          INTO ls_carta_porte-mercancias-auto_transporte-identificacion_vehi-placa
          WHERE objnr EQ ls_equi-objnr.

        DATA: lv_equi_brgew TYPE char30.

        CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
          EXPORTING
            input                = ls_equi-brgew
            unit_in              = ls_equi-gewei
            unit_out             = 'KG'
          IMPORTING
            output               = lv_equi_brgew
          EXCEPTIONS
            conversion_not_found = 1
            division_by_zero     = 2
            input_invalid        = 3
            output_invalid       = 4
            overflow             = 5
            type_invalid         = 6
            units_missing        = 7
            unit_in_not_found    = 8
            unit_out_not_found   = 9
            OTHERS               = 10.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        CLEAR ls_mercancia.
        LOOP AT ls_carta_porte-mercancias-mercancia ASSIGNING FIELD-SYMBOL(<ls_mercancia>).

          DATA(lv_lips_mein) = <ls_mercancia>-cantidad_transportada-um.

          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input                = <ls_mercancia>-cantidad_transportada-cantidad
              unit_in              = <ls_mercancia>-cantidad_transportada-um
              unit_out             = 'KG'
            IMPORTING
              output               = lv_lips_mein
            EXCEPTIONS
              conversion_not_found = 1
              division_by_zero     = 2
              input_invalid        = 3
              output_invalid       = 4
              overflow             = 5
              type_invalid         = 6
              units_missing        = 7
              unit_in_not_found    = 8
              unit_out_not_found   = 9
              OTHERS               = 10.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.


          ls_carta_porte-mercancias-auto_transporte-identificacion_vehi-peso_vehicular = lv_equi_brgew + lv_lips_mein.

        ENDLOOP.

      ELSE.

        CLEAR ls_remolque.

        SELECT SINGLE * FROM equi INTO @ls_equi
          WHERE equnr EQ @ls_vehiculo.
        IF sy-subrc EQ 0.

          ls_remolque-subtiporem = ls_vehiculo.

          SELECT SINGLE license_num FROM fleet INTO ls_remolque-placa
            WHERE objnr EQ ls_equi-objnr.

          APPEND ls_remolque TO ls_carta_porte-mercancias-auto_transporte-remolque.

        ENDIF.

      ENDIF.

      "    ENDIF.

    ENDLOOP.

    ls_carta_porte-tipos_figura-tipo_figura = lc_01.

    SELECT SINGLE * FROM dfkkbptaxnum
      INTO @DATA(ls_taxnum)
      WHERE partner EQ @ls_likp-xabln.
    IF sy-subrc EQ 0.
      ls_carta_porte-tipos_figura-rfc_figura = ls_taxnum-taxnum.
      SELECT SINGLE * FROM but0id
        INTO @DATA(ls_but0id)
        WHERE partner EQ @ls_likp-xabln.
      IF sy-subrc EQ 0.
        ls_carta_porte-tipos_figura-num_lic = ls_but0id-idnumber.
      ENDIF.
    ENDIF.



  ENDIF.

********************** ARMADO DE XML *********************************

  " Crear la estructura básica del XML

  lv_xml = '<?xml version="1.0" encoding="UTF-8"?>'.

*xmlns:n0="http://www.ejemplo.com/esquema"

  lv_xml = lv_xml && '<CartaPorte Version="' && ls_carta_porte-carta_porte-version && '" IdCCP="' && ls_carta_porte-carta_porte-id_ccp && '" TranspInternac="' &&
    ls_carta_porte-carta_porte-transp && '" TotalDistRec="' && ls_carta_porte-carta_porte-total_dist && '">'.

  "Ubicaciones
  lv_xml = lv_xml && '<Ubicaciones>'.

  " ubicacion origen
  lv_xml = lv_xml && '<Ubicacion TipoUbicacion="' && ls_carta_porte-ubicacion_origen-tipo_ubicacion && '" IDUbicacion="' &&  ls_carta_porte-ubicacion_origen-id_ubicacion &&
    '" RFCRemitenteDestinatario="' && ls_carta_porte-ubicacion_origen-rfc_remitente && '" FechaHoraSalidaLlegada="' && ls_carta_porte-ubicacion_origen-fecha_hora_s_l && '">'.

  lv_xml = lv_xml && '<Domicilio Municipio="' && ls_carta_porte-ubicacion_origen-domicilio-municipio && '" Estado="' && ls_carta_porte-ubicacion_origen-domicilio-estado &&
    '" Pais="' && ls_carta_porte-ubicacion_origen-domicilio-pais && '" CodigoPostal="' && ls_carta_porte-ubicacion_origen-domicilio-codigo_postal && '"/>'.
  lv_xml = lv_xml && '</Ubicacion>'.

  " ubicacion destino
  lv_xml = lv_xml && '<Ubicacion TipoUbicacion="' && ls_carta_porte-ubicacion_destino-tipo_ubicacion && '" IDUbicacion="' && ls_carta_porte-ubicacion_destino-id_ubicacion &&
    '" RFCRemitenteDestinatario="' && ls_carta_porte-ubicacion_destino-rfc_remitente && '" FechaHoraSalidaLlegada="' && ls_carta_porte-ubicacion_destino-fecha_hora_s_l && '">'.

  lv_xml = lv_xml && '<Domicilio Municipio="' && ls_carta_porte-ubicacion_destino-domicilio-municipio && '" Estado="' && ls_carta_porte-ubicacion_destino-domicilio-estado &&
    '" Pais="' && ls_carta_porte-ubicacion_destino-domicilio-pais && '" CodigoPostal="' && ls_carta_porte-ubicacion_destino-domicilio-codigo_postal && '"/>'.
  lv_xml = lv_xml && '</Ubicacion>'.

  " / Ubicaciones
  lv_xml = lv_xml && '</Ubicaciones>'.

  "Mercancias
  lv_xml = lv_xml && '<Mercancias PesoBrutoTotal="' && ls_carta_porte-mercancias-peso_bruto && '" UnidadPeso="' && ls_carta_porte-mercancias-unidad_peso &&
    '" NumTotalMercancias="' && ls_carta_porte-mercancias-num_total_merc && '">'.

  LOOP AT ls_carta_porte-mercancias-mercancia INTO DATA(ls_mercancia_aux).

    " mercancia
    lv_xml = lv_xml && '<Mercancia BienesTransp="' && ls_mercancia_aux-bienes_transp && '" Descripcion="' && ls_mercancia_aux-descripcion && '" Cantidad="' && ls_mercancia_aux-cantidad &&
      '" ClaveUnidad="' && ls_mercancia_aux-clave_unidad && '" PesoEnKg="' && ls_mercancia_aux-peso_en_kg && '">'.
    lv_xml = lv_xml && '<CantidadTransporta Cantidad="' && ls_mercancia_aux-cantidad_transportada-cantidad && '" IDOrigen="' && ls_mercancia_aux-cantidad_transportada-id_origen &&
      '" IDDestino="' && ls_mercancia_aux-cantidad_transportada-id_destino && '"/>'.

    " / Mercancias
    lv_xml = lv_xml && '</Mercancia>'.

  ENDLOOP.

  "Autotransporte
  lv_xml = lv_xml && '<Autotransporte PermSCT="' && ls_carta_porte-mercancias-auto_transporte-permsct &&
    '" NumPermisoSCT="' && ls_carta_porte-mercancias-auto_transporte-num_permisosct &&'">'.

  "Identificador
  lv_xml = lv_xml && '<IdentificacionVehicular ConfigVehicular="' && ls_carta_porte-mercancias-auto_transporte-identificacion_vehi-config_vehic &&
    '" PesoBrutoVehicular="'&& ls_carta_porte-mercancias-auto_transporte-identificacion_vehi-peso_vehicular &&
    '" PlacaVM="' && ls_carta_porte-mercancias-auto_transporte-identificacion_vehi-placa && '" AnioModeloVM="' && ls_carta_porte-mercancias-auto_transporte-identificacion_vehi-anio_mod && '"/>'.
  "Seguros
  lv_xml = lv_xml && '<Seguros AseguraRespCivil="'&& ls_carta_porte-mercancias-auto_transporte-seguros-asegura &&
  '" PolizaRespCivil="'&& ls_carta_porte-mercancias-auto_transporte-seguros-poliza && '"/>'.

  "Remolques
  DESCRIBE TABLE ls_carta_porte-mercancias-auto_transporte-remolque LINES DATA(lv_lines).
  IF lv_lines GE 1.

    lv_xml = lv_xml && '<Remolques>'.

    LOOP AT ls_carta_porte-mercancias-auto_transporte-remolque INTO DATA(ls_remolque_aux).
      "Remolque 1
      lv_xml = lv_xml && '<Remolque SubTipoRem="'&& ls_remolque_aux-subtiporem && '" Placa="'&& ls_remolque_aux-placa &&'"/>'.

    ENDLOOP.

    " / Remolques
    lv_xml = lv_xml && '</Remolques>'.

  ENDIF.

  " / Autotransporte
  lv_xml = lv_xml && '</Autotransporte>'.

  " / Mercancias
  lv_xml = lv_xml && '</Mercancias>'.

  " FiguraTransporte
  lv_xml = lv_xml && '<FiguraTransporte>'.

  "TiposFigura
  lv_xml = lv_xml && '<TiposFigura TipoFigura="'&& ls_carta_porte-tipos_figura-tipo_figura &&'" RFCFigura="'&& ls_carta_porte-tipos_figura-rfc_figura &&
    '" NumLicencia="'&& ls_carta_porte-tipos_figura-num_lic &&'"/>'.

  " / Figura Transporte
  lv_xml = lv_xml && '</FiguraTransporte>'.

  " / CartaPorte
  lv_xml = lv_xml && '</CartaPorte>'.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_xml
    IMPORTING
      buffer = xstring.

ENDFUNCTION.

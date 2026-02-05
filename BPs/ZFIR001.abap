*&-------------------------------------------------------------------------------------*
*& ==================== F I R M A   D E L   P R O G R A M A ===========================*
*&-------------------------------------------------------------------------------------*
*& Nombre:      ZFIR001                                                                *
*& Descripción: CREACION MASIVA DE BPS POR DOCUMENTO .XLS                              *
*&-------------------------------------------------------------------------------------*
*& ID PDS     | Transporte   | Fecha      | Autor            | Solicitante             *
*&-------------------------------------------------------------------------------------*
*& CPXFIDE048 | DS4K900938   | 26.03.2025 | Daniel B.Sanchez | Rigoberto Almaguer      *
*&-------------------------------------------------------------------------------------*
*& ================== L O G   D E   M O D I F I C A C I O N E S =======================*
*&-------------------------------------------------------------------------------------*
*& ID PDS     | Transporte | Fecha      | Desarrollador    | Funcional                 *
*&-------------------------------------------------------------------------------------*
*& CPXFIDE048 | DS4K900938 | 01.06.2025 | David Navoa A.   | Rigoberto Almaguer        *
*&-------------------------------------------------------------------------------------*
REPORT zfir001.

INCLUDE zfir001_top. " Declaracion Globales
INCLUDE zfri001_cl1. " Clases Locales
INCLUDE zfir001_f01. " Sub Rutinas
INCLUDE zfir001_o01. " PBO - Process Before OutPut
INCLUDE zfir001_i01. " PAI - Process After Input
*&---------------------------------------------------------------------------*
*&-->  S T A R T   P R O G R A M
AT SELECTION-SCREEN OUTPUT.
  CLEAR: p_file.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  " --> Seleccion de Documento
  PERFORM change_doc.

START-OF-SELECTION.

  " --> Carga Excel
  PERFORM change_xls USING p_file.

  IF t_bindata IS NOT INITIAL.

    " --> Carga Estructuras BPS
    PERFORM change_estruc USING gv_ejecflag.

    CASE gv_ejecflag.
      WHEN abap_true.

        " --> Creacion Masiva BPS
        PERFORM creacion_bps.

    ENDCASE.

  ENDIF.
*&---------------------------------------------------------------------------*

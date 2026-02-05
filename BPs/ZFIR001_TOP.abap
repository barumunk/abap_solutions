*&---------------------------------------------------------------------*
*& Include          ZFIR001_TOP
*&---------------------------------------------------------------------------*
*&  I N C L U D E S

*&---------------------------------------------------------------------------*
*&  T Y P E S /  S T R U C T U R E S

" --> Datos Generales / Info. Fiscal / Anexos.
TYPES: BEGIN OF ty_gnrales,
         " Datos Generales
         typereg    TYPE c LENGTH 30, " Tipo Registro
         code       TYPE c LENGTH 10, " Codigo
         razon      TYPE c LENGTH 160, " Razon Social
**         razon      TYPE c LENGTH 30, " Razon Social
         tipo       TYPE c LENGTH 30, " Tipo
         grp_bp     TYPE c LENGTH 30, " Grupo BP
         aped1      TYPE c LENGTH 30, " Primero Apellido
         aped2      TYPE c LENGTH 30, " Segundo Apellido
         name1      TYPE c LENGTH 30, " Primero Nombre
         name2      TYPE c LENGTH 30, " Segundo Nombre
         namect     TYPE c LENGTH 30, " Nombre Corto
         calle      TYPE c LENGTH 160, " Calle
**         calle      TYPE c LENGTH 30, " Calle
         numext     TYPE c LENGTH 30, " Numero Exterior
         numeint    TYPE c LENGTH 30, " Numero Interior
         colonia    TYPE c LENGTH 40, " Colonia
**         colonia    TYPE c LENGTH 30, " Colonia
         local      TYPE c LENGTH 40, " Localidad
**         local      TYPE c LENGTH 30, " Localidad
         munic      TYPE c LENGTH 40, " Municipio
**         munic      TYPE c LENGTH 30, " Municipio
         refer      TYPE c LENGTH 30, " Referencia
         estado     TYPE c LENGTH 30, " Estado
         pais       TYPE c LENGTH 30, " Pais
         c_p_       TYPE c LENGTH 30, " Codigo Postal
         idioma     TYPE c LENGTH 30, " Idioma
         telef      TYPE c LENGTH 30, " Telefono
         email1     TYPE c LENGTH 70, " Correo Electronico  1
         email2     TYPE c LENGTH 70, " Correo Electronico  2
         email3     TYPE c LENGTH 70, " Correo Electronico  3
         email4     TYPE c LENGTH 70, " Correo Electronico  4
         " Informacion Fiscal
         codesap    TYPE c LENGTH 30, " Codigo SAP
         tiperfc    TYPE c LENGTH 30, " Tipo RFC
         id_rfc     TYPE c LENGTH 30, " Ident Fiscal (RFC)
         pagosat    TYPE c LENGTH 30, " Forma de Pago SAT
         metodo     TYPE c LENGTH 30, " Metodo de Pago SAT
         regimen    TYPE c LENGTH 30, " Regimen Fiscal SAT
         cfdifac    TYPE c LENGTH 30, " USO CFDI FAC SAT
         cfdinrc    TYPE c LENGTH 30, " USO CFDI NCR SAT
         tip_op  TYPE c LENGTH 30, " Tipo operación
         tip_ind TYPE c LENGTH 30, " Tipo de industri
         " Anexos
         code_anexo TYPE c LENGTH 50, " Codigo Anexo
         typearch   TYPE c LENGTH 50, " Tipo Archivo
         ruta       TYPE c LENGTH 50, " Ruta
       END OF ty_gnrales.

" -->  Anexos.
TYPES: BEGIN OF ty_anexo,
         code_anexo TYPE c LENGTH 50, " Codigo Anexo
         typearch   TYPE c LENGTH 50, " Tipo Archivo
         ruta       TYPE c LENGTH 50, " Ruta
       END OF ty_anexo.

" --> Clientes Financieros: Cliente / Cliente Retencion
TYPES: BEGIN OF ty_cntefinan,
         "  Cliente
         id_empre  TYPE c LENGTH 30, " Empresa
         code      TYPE c LENGTH 30, " Codigo Empresa
         grptesor  TYPE c LENGTH 30, " Grupo de Tesoreria
         cntasap   TYPE c LENGTH 30, " Cuenta Saldo SAP
         condpago  TYPE c LENGTH 30, " Condicion Pago
         typecred  TYPE c LENGTH 30, " Tipo Credito
         moncred   TYPE c LENGTH 30, " Moneda Crédito
         limitcred TYPE c LENGTH 30, " Limite Credito
         perfcred  TYPE c LENGTH 30, " Perfil de Crédito
         tipogrant TYPE c LENGTH 30, " Tipo Garantia
         monegrant TYPE c LENGTH 30, " Moneda Garantia
         valorgrnt TYPE c LENGTH 30, " Valor Garantia
         vencgrant TYPE c LENGTH 30, " Vencimiento Garantia
         " Retencion
         impuesto  TYPE c LENGTH 30, " Impuesto
       END OF ty_cntefinan.

" -->  Clientes Ventas:  Ventas / Interlocutor
TYPES: BEGIN OF ty_cntevntas,
         " Clientes Ventas
         code      TYPE c LENGTH 30, " Codigo
         orgvnta   TYPE c LENGTH 30, " Organización de Ventas
         canal     TYPE c LENGTH 30, " Canal de Distribución
         sector    TYPE c LENGTH 30, " Sector
         grpclte   TYPE c LENGTH 30, " Grupo de Clientes SD
         zonavnta  TYPE c LENGTH 30, " Zona de Ventas
         ofcventa  TYPE c LENGTH 30, " Oficina de Ventas
         grpovnta  TYPE c LENGTH 30, " Grupo de Ventas
         clas_abc  TYPE c LENGTH 30, " Clasificacion ABC
         mone_ped  TYPE c LENGTH 30, " Moneda Pedido
         condentg  TYPE c LENGTH 30, " Condición de Entrega
         cedis     TYPE c LENGTH 30, " Cedis Atend
         tol_exce  TYPE c LENGTH 30, " Tolerancia de Exceso
         incoterm  TYPE c LENGTH 30, " Incoterm
         locincot  TYPE c LENGTH 30, " Local Responsabilidad Incoterm
         tip_cots  TYPE c LENGTH 30, " Tipo Cotizacion "DNAVOA
         gp_prec   TYPE c LENGTH 30, " Grupo de Precios "DNAVOA
         esq_clte  TYPE c LENGTH 30, " Esquema de Cliente "DNAVOA
         cond_exp  TYPE c LENGTH 30, " Condiciones de Expedicion "DNAVOA
         gpo_impt  TYPE c LENGTH 30, " Grupo de Imputacion "DNAVOA
         clas_fis  TYPE c LENGTH 30, " Clasificacion Fiscal "DNAVOA
         gpo_cli3  TYPE c LENGTH 30, " Grupo Clientes 3 "DNAVOA
         " Cliente Interlocutor
         tipointer TYPE c LENGTH 30, " Tipo Interlocutor
         interloc  TYPE c LENGTH 30, " Interlocutor
       END OF ty_cntevntas.


" -->  Direcciones Entrega: Entrega / Empresa / Herramientas / Instruccion
TYPES: BEGIN OF ty_direntg,
         " Entrega
         code      TYPE c LENGTH 30, " Codigo
         namect    TYPE c LENGTH 30, " Nombre Corto
         descdir   TYPE c LENGTH 30, " Descripcion Direccion
         calle     TYPE c LENGTH 30, " Calle
         numext    TYPE c LENGTH 30, " Numero Exterior
         numint    TYPE c LENGTH 30, " Numero Interior
         colonia   TYPE c LENGTH 30, " Colonia
         localid   TYPE c LENGTH 30, " Localidad
         municpo   TYPE c LENGTH 30, " Municipio
         refercn   TYPE c LENGTH 30, " Referencia
         estado    TYPE c LENGTH 30, " Estado
         pais      TYPE c LENGTH 30, " Pais
         codepos   TYPE c LENGTH 30, " Codigo Postal
         telefon   TYPE c LENGTH 30, " Telefono
         email1    TYPE c LENGTH 70, " Correo Electronico 1
         email2    TYPE c LENGTH 70, " Correo Electronico 2
         email3    TYPE c LENGTH 70, " Correo Electronico 3
         email4    TYPE c LENGTH 70, " Correo Electronico 4
         " Empresa
         cedisaten TYPE c LENGTH 30, " Cedis Atend
         respons   TYPE c LENGTH 30, " Responsable
         diarecp   TYPE c LENGTH 30, " Dias Recepcion
         horarecp  TYPE c LENGTH 30, " Hora Recepcion
         tipocarga TYPE c LENGTH 30, " Tipo de Carga
         tipomano  TYPE c LENGTH 30, " Tipo Maniobra
         zona_exp  TYPE c LENGTH 30, " Zona de Ventas "DNAVOA
         cond_exp  TYPE c LENGTH 30, " Condiciones de Expedicion "DNAVOA
         " Herramienta
         orgventa  TYPE c LENGTH 30, " Organizacion de Venta
         canaldbt  TYPE c LENGTH 30, " Canal Distribucion
         sector    TYPE c LENGTH 30, " Sector
         namecrto  TYPE c LENGTH 30, " Nombre Corto
         herrmnta  TYPE c LENGTH 30, " Herramental
         " Instruccion
         codedirc  TYPE c LENGTH 30, " Codigo Direccion
         instrucc  TYPE c LENGTH 30, " Instruccion
       END OF ty_direntg.

" -->  Proveedor: Proveedor / Retencion
TYPES: BEGIN OF ty_provedor,
         " Proveedor
         empresa   TYPE c LENGTH 30, " Empresa
         code      TYPE c LENGTH 30, " Codigo
         gpoteso   TYPE c LENGTH 30, " Grupo Tesoreria
*         gpoprvsap TYPE c LENGTH 30, " Grupo Proveedor SAP
         cntasaldo TYPE c LENGTH 30, " Cuenta Saldo
         condpago  TYPE c LENGTH 30, " Condicion Pago
         paisbank  TYPE c LENGTH 30, " País Banco
         banco     TYPE c LENGTH 30, " Banco
         suc_bank  TYPE c LENGTH 30, " Sucursal Bancaria
         cnta_bank TYPE c LENGTH 30, " Cuenta Bancaria
         incoterm  TYPE c LENGTH 30, " Incoterm
         mon_pedid TYPE c LENGTH 30, " Moneda Pedido
         " Retencion
         impuesto  TYPE c LENGTH 30, " Impuesto
       END OF ty_provedor.

" -->  Proveedor Compras: Proveedor / Interlocutor / Clasificacion
TYPES: BEGIN OF ty_provcomp,
         " Proveedor
         empresa    TYPE c LENGTH 30, " Empresa
         code       TYPE c LENGTH 30, " Codigo
         gpoteso    TYPE c LENGTH 30, " Grupo Tesoreria
         gpoprvsap  TYPE c LENGTH 30, " Grupo Proveedor SAP
         cntasaldo  TYPE c LENGTH 30, " Cuenta Saldo
         condpago   TYPE c LENGTH 30, " Condicion Pago
         paisbank   TYPE c LENGTH 30, " País Banco
         banco      TYPE c LENGTH 30, " Banco
         suc_bank   TYPE c LENGTH 30, " Sucursal Bancaria
         cnta_bank  TYPE c LENGTH 30, " Cuenta Bancaria
         incoterm   TYPE c LENGTH 30, " Incoterm
         mon_pedid  TYPE c LENGTH 30, " Moneda Pedido
         " Interlocutor
         code_minor TYPE c LENGTH 30, " Codigo Minorista
         tipo_inter TYPE c LENGTH 30, " Tipo
         interlcutr TYPE c LENGTH 30, " Interlocutor
         " Clasificacion
       END OF ty_provcomp.


*==========> PESTAÑAS DE EXCEL <=================

" -> Pestaña Datos Generales
TYPES: BEGIN OF ty_pstgnral,
         typereg TYPE c LENGTH 30, " Tipo Registro
         code    TYPE c LENGTH 30, " Codigo
         razon   TYPE c LENGTH 160, " Razon Social
**         razon   TYPE c LENGTH 30, " Razon Social
         tipo    TYPE c LENGTH 30, " Tipo
         grp_bp  TYPE c LENGTH 30, " Grupo BP
         aped1   TYPE c LENGTH 30, " Primero Apellido
         aped2   TYPE c LENGTH 30, " Segundo Apellido
         name1   TYPE c LENGTH 30, " Primero Nombre
         name2   TYPE c LENGTH 30, " Segundo Nombre
         namect  TYPE c LENGTH 30, " Nombre Corto
         calle   TYPE c LENGTH 160, " Calle
**         calle   TYPE c LENGTH 30, " Calle
         numext  TYPE c LENGTH 30, " Numero Exterior
         numeint TYPE c LENGTH 30, " Numero Interior
         colonia TYPE c LENGTH 40, " Colonia
**         colonia TYPE c LENGTH 30, " Colonia
         local   TYPE c LENGTH 40, " Localidad
**         local   TYPE c LENGTH 30, " Localidad
         munic   TYPE c LENGTH 40, " Municipio
**         munic   TYPE c LENGTH 30, " Municipio
         refer   TYPE c LENGTH 30, " Referencia
         estado  TYPE c LENGTH 30, " Estado
         pais    TYPE c LENGTH 30, " Pais
         c_p_    TYPE c LENGTH 30, " Codigo Postal
         idioma  TYPE c LENGTH 30, " Idioma
         telef   TYPE c LENGTH 30, " Telefono
         email1  TYPE c LENGTH 70, " Correo Electronico  1
         email2  TYPE c LENGTH 70, " Correo Electronico  2
         email3  TYPE c LENGTH 70, " Correo Electronico  3
         email4  TYPE c LENGTH 70, " Correo Electronico  4
       END OF ty_pstgnral.

" --> Pestaña Informacion Fiscal
TYPES: BEGIN OF ty_pstfiscal,
         codesap TYPE c LENGTH 30, " Codigo SAP
         tiperfc TYPE c LENGTH 30, " Tipo RFC
         id_rfc  TYPE c LENGTH 30, " Ident Fiscal (RFC)
         pagosat TYPE c LENGTH 30, " Forma de Pago SAT
         metodo  TYPE c LENGTH 30, " Metodo de Pago SAT
         regimen TYPE c LENGTH 30, " Regimen Fiscal SAT
         cfdifac TYPE c LENGTH 30, " USO CFDI FAC SAT
         cfdinrc TYPE c LENGTH 30, " USO CFDI NCR S
         tip_op  TYPE c LENGTH 30, " Tipo operación
         tip_ind TYPE c LENGTH 30, " Tipo de industri
       END OF ty_pstfiscal.

" --> Pestaña Anexo
TYPES: BEGIN OF ty_pstanexo,
         code_anexo TYPE c LENGTH 50, " Codigo Anexo
         typearch   TYPE c LENGTH 50, " Tipo Archivo
         ruta       TYPE c LENGTH 50, " Ruta
       END OF ty_pstanexo.

" --> Pestaña Clientes
TYPES: BEGIN OF ty_pstcliente,
         "  Cliente
         id_empre  TYPE c LENGTH 30, " Empresa / Sociedad
         code      TYPE c LENGTH 30, " Codigo Empresa
         grptesor  TYPE c LENGTH 30, " Grupo de Tesoreria
         cntasap   TYPE c LENGTH 30, " Cuenta Saldo SAP
         condpago  TYPE c LENGTH 30, " Condicion Pago
         typecred  TYPE c LENGTH 30, " Tipo Credito
         moncred   TYPE c LENGTH 30, " Moneda Crédito
         limitcred TYPE c LENGTH 30, " Limite Credito
         perfcred  TYPE c LENGTH 30, " Perfil de Crédito
         tipogrant TYPE c LENGTH 30, " Tipo Garantia
         monegrant TYPE c LENGTH 30, " Moneda Garantia
         valorgrnt TYPE c LENGTH 30, " Valor Garantia
         vencgrant TYPE c LENGTH 30, " Vencimiento Garantia
       END OF ty_pstcliente.

" -->  Pestaña Direccio Entrega
TYPES: BEGIN OF ty_pstentrega,
         " Entrega
         code    TYPE c LENGTH 30, " Codigo
         namect  TYPE c LENGTH 30, " Nombre Corto
         descdir TYPE c LENGTH 30, " Descripcion Direccion
         calle   TYPE c LENGTH 40, " Calle
**         calle   TYPE c LENGTH 30, " Calle
         numext  TYPE c LENGTH 30, " Numero Exterior
         numint  TYPE c LENGTH 30, " Numero Interior
         colonia TYPE c LENGTH 40, " Colonia
**         colonia TYPE c LENGTH 30, " Colonia
         localid TYPE c LENGTH 40, " Localidad
**         localid TYPE c LENGTH 30, " Localidad
         municpo TYPE c LENGTH 30, " Municipio
         refercn TYPE c LENGTH 30, " Referencia
         estado  TYPE c LENGTH 30, " Estado
         pais    TYPE c LENGTH 30, " Pais
         codepos TYPE c LENGTH 30, " Codigo Postal
         idioma  TYPE c LENGTH 30, " Idioma
         telefon TYPE c LENGTH 30, " Telefono
         email1  TYPE c LENGTH 70, " Correo Electronico 1
         email2  TYPE c LENGTH 70, " Correo Electronico 2
         email3  TYPE c LENGTH 70, " Correo Electronico 3
         email4  TYPE c LENGTH 70, " Correo Electronico 4
       END OF ty_pstentrega.

" -->  Pestaña Direccion Entrega - Empresa
TYPES: BEGIN OF ty_pstempresa,
         code      TYPE c LENGTH 30, " Codigo
         orgventa  TYPE c LENGTH 30, " Organizacion de Venta
         canaldbt  TYPE c LENGTH 30, " Canal Distribucion
         sector    TYPE c LENGTH 30, " Sector
         namecrto  TYPE c LENGTH 30, " Nombre Corto
         cedisaten TYPE c LENGTH 30, " Cedis Atend
         respons   TYPE c LENGTH 30, " Responsable
         diarecp   TYPE c LENGTH 30, " Dias Recepcion
         horarecp  TYPE c LENGTH 30, " Hora Recepcion
         tipocarga TYPE c LENGTH 30, " Tipo de Carga
         tipomano  TYPE c LENGTH 30, " Tipo Maniobra
         zona_exp  TYPE c LENGTH 30, " Zona de Ventas "DNAVOA
         cond_exp  TYPE c LENGTH 30, " Condiciones de Expedicion "DNAVOA
       END OF ty_pstempresa.

" -->  Pestaña Direccion Entrega - Herramienta
TYPES: BEGIN OF ty_pstherram,
         code     TYPE c LENGTH 30, " Codigo
         orgventa TYPE c LENGTH 30, " Responsable
         canaldbt TYPE c LENGTH 30, " Dias Recepcion
         sector   TYPE c LENGTH 30, " Hora Recepcion
         tipo     TYPE c LENGTH 30, " Tipo Herramienta
         namecrto TYPE c LENGTH 30, " Tipo de Carga
         herrmnta TYPE c LENGTH 30, " Tipo Maniobra
       END OF ty_pstherram.


" -->  Pestaña Direccion Entrega - Instruccion
TYPES: BEGIN OF ty_pstinstruc,
         code     TYPE c LENGTH 30, " Codigo
         orgventa TYPE c LENGTH 30, " Responsable
         canaldbt TYPE c LENGTH 30, " Dias Recepcion
         sector   TYPE c LENGTH 30, " Hora Recepcion
         tipo     TYPE c LENGTH 30, " Tipo Instruccion
         codedirc TYPE c LENGTH 30, " Codigo Direccion
         instrucc TYPE c LENGTH 30, " Instruccion
       END OF ty_pstinstruc.

" -->  Pestaña Cliente Retencion
TYPES: BEGIN OF ty_pstcntret,
         empresa  TYPE c LENGTH 30, " Empresa
         code     TYPE c LENGTH 30, " Codigo
         impuesto TYPE c LENGTH 30, " Impuesto
       END OF ty_pstcntret.

" -->  Pestaña Cliente Ventas
TYPES: BEGIN OF ty_pstcntevta,
         code     TYPE c LENGTH 30, " Codigo SAP
         orgvnta  TYPE c LENGTH 30, " Organización de Ventas
         canal    TYPE c LENGTH 1, " Canal de Distribución
         sector   TYPE c LENGTH 30, " Sector
         grpclte  TYPE c LENGTH 30, " Grupo de Clientes SD
         zonavnta TYPE c LENGTH 30, " Zona de Ventas
         ofcventa TYPE c LENGTH 30, " Oficina de Ventas
         grpovnta TYPE c LENGTH 30, " Grupo de Ventas
         clas_abc TYPE c LENGTH 30, " Clasificacion ABC
         mone_ped TYPE c LENGTH 30, " Moneda Pedido
         condentg TYPE c LENGTH 30, " Condición de Entrega
         cedis    TYPE c LENGTH 30, " Cedis Atend
         tol_exce TYPE c LENGTH 30, " Tolerancia de Exceso
         incoterm TYPE c LENGTH 30, " Incoterm
         locincot TYPE c LENGTH 30, " Local Responsabilidad Incoterm
         tip_cots TYPE c LENGTH 30, " Tipo Cotizacion "DNAVOA
         gp_prec  TYPE c LENGTH 30, " Grupo de Precios "DNAVOA
         esq_clte TYPE c LENGTH 30, " Esquema de Cliente "DNAVOA
         cond_exp TYPE c LENGTH 30, " Condiciones de Expedicion "DNAVOA
         gpo_impt TYPE c LENGTH 30, " Grupo de Imputacion "DNAVOA
         clas_fis TYPE c LENGTH 30, " Clasificacion Fiscal "DNAVOA
         gpo_cli3 TYPE c LENGTH 30, " Grupo Clientes 3 "DNAVOA
       END OF ty_pstcntevta.

" -->  Pestaña Cliente Interlocutor
TYPES: BEGIN OF ty_pstcnteint,
         code      TYPE c LENGTH 30, " Codigo
         orgvnta   TYPE c LENGTH 30, " Organización de Ventas
         canal     TYPE c LENGTH 30, " Canal de Distribución
         sector    TYPE c LENGTH 30, " Sector
         tipointer TYPE c LENGTH 30, " Tipo Interlocutor
         interloc  TYPE c LENGTH 30, " Interlocutor
       END OF ty_pstcnteint.

" -->  Proveedor
TYPES: BEGIN OF ty_pstprov,
         " Proveedor
         empresa   TYPE c LENGTH 30, " Empresa
         code      TYPE c LENGTH 30, " Codigo
         gpoteso   TYPE c LENGTH 30, " Grupo Tesoreria
         gpoprvsap TYPE c LENGTH 30, " Grupo Proveedor SAP
         cntasaldo TYPE c LENGTH 30, " Cuenta Saldo
         condpago  TYPE c LENGTH 30, " Condicion Pago
         paisbank  TYPE c LENGTH 30, " País Banco
         banco     TYPE c LENGTH 30, " Banco
         suc_bank  TYPE c LENGTH 30, " Sucursal Bancaria
         cnta_bank TYPE c LENGTH 30, " Cuenta Bancaria
         incoterm  TYPE c LENGTH 30, " Incoterm
         mon_pedid TYPE c LENGTH 30, " Moneda Pedido
       END OF ty_pstprov.

" -->  Proveedor Retencion
TYPES: BEGIN OF ty_pstprovrnt,
         empresa  TYPE c LENGTH 30, " Empresa
         code     TYPE c LENGTH 30, " Codigo
         impuesto TYPE c LENGTH 30, " Impuesto
       END OF ty_pstprovrnt.

" -->  Proveedor Interlocutor
TYPES: BEGIN OF ty_pstprovint,
         empresa    TYPE c LENGTH 30, " Empresa
         centro     TYPE c LENGTH 30, " Centro
         code_minor TYPE c LENGTH 30, " Codigo Minorista
         tipo_inter TYPE c LENGTH 30, " Tipo
         interlcutr TYPE c LENGTH 30, " Interlocutor
       END OF ty_pstprovint.

" -->  Proveedor Clasificacion
TYPES: BEGIN OF ty_pstprovcla,
         code      TYPE c LENGTH 30, " codigo
         tipo_prov TYPE c LENGTH 30, " Tipo Proveedor
         region    TYPE c LENGTH 30, " Region
         esp_verd  TYPE c LENGTH 30, " Espacio Verde
         index     TYPE c LENGTH 30, " Index
         clave_imp TYPE c LENGTH 30, "Clave Impuesto
         apl_desc  TYPE c LENGTH 30, "Aplica Descuento
         promotor  TYPE c LENGTH 30, "Promotor
       END OF ty_pstprovcla.

TYPES: BEGIN OF ty_object_keys,
         code TYPE c LENGTH 10,
         key  TYPE bds_objid,
       END OF ty_object_keys.

" -->  Reporte ALV Log. de Proceso
DATA gv_proceso TYPE c LENGTH 40.
DATA gv_total   TYPE c LENGTH 30.
DATA gv_usuario TYPE c LENGTH 50.
DATA gv_fecha   TYPE c LENGTH 10.

DATA gv_contlog   TYPE scrfname VALUE 'CONTLOG'.
DATA gv_contgnral TYPE scrfname VALUE 'CONTGNRAL'.
DATA gv_contcntfn TYPE scrfname VALUE 'CONTCNTFN'.
DATA gv_contcntvn TYPE scrfname VALUE 'CONTCNTVN'.
DATA gv_contdiren TYPE scrfname VALUE 'CONTDIREN'.
DATA gv_contprove TYPE scrfname VALUE 'CONTPROVE'.
DATA gv_contpvcom TYPE scrfname VALUE 'CONTPVCOM'.
DATA gv_contanexo TYPE scrfname VALUE 'CONTANEXO'.

" --> Log de Proceso
DATA obj_gridlog TYPE REF TO cl_gui_alv_grid.
DATA obj_contlog TYPE REF TO cl_gui_custom_container.

" --> Generales
DATA obj_gridgnral TYPE REF TO cl_gui_alv_grid.
DATA obj_contgnral TYPE REF TO cl_gui_custom_container.

" --> Cliente Financiero
DATA obj_gridcntfin TYPE REF TO cl_gui_alv_grid.
DATA obj_contcntfin TYPE REF TO cl_gui_custom_container.

" --> Cliente Ventas
DATA obj_gridcntvnta TYPE REF TO cl_gui_alv_grid.
DATA obj_contcntvnta TYPE REF TO cl_gui_custom_container.

" --> Direccion de Entrega
DATA obj_griddirentga TYPE REF TO cl_gui_alv_grid.
DATA obj_contdirentga TYPE REF TO cl_gui_custom_container.

" --> Proveedor
DATA obj_gridprov TYPE REF TO cl_gui_alv_grid.
DATA obj_contprov TYPE REF TO cl_gui_custom_container.

" --> Proveedor Compras
DATA obj_gridprvcomp TYPE REF TO cl_gui_alv_grid.
DATA obj_contprvcomp TYPE REF TO cl_gui_custom_container.

DATA gt_data TYPE cvis_ei_extern.

" --> Anexo de Formato
DATA url_save    TYPE so_url.
DATA document_id TYPE sofmk.
DATA is_object   TYPE borident.
DATA folder_id   TYPE sofdk.

DATA obj_gridanexo TYPE REF TO cl_gui_alv_grid.
DATA obj_contanexo TYPE REF TO cl_gui_custom_container.

DATA t_fcat         TYPE lvc_t_fcat.
DATA gs_fcat        LIKE lvc_s_fcat.
DATA gs_layout      TYPE lvc_s_layo.
DATA i_variant      TYPE disvariant.
DATA i_index_rows   TYPE lvc_t_row.
DATA i_selected_row LIKE lvc_s_row.

TYPES: BEGIN OF ty_items,
         folio TYPE c LENGTH 4,
         campo TYPE c LENGTH 50,
       END OF ty_items.

TYPES:

  BEGIN OF ty_object_key,
    key  TYPE bds_objid,
    type TYPE char30,
  END OF ty_object_key.

DATA gs_items TYPE ty_items.
DATA t_items  TYPE TABLE OF ty_items.

DATA gs_alvlog TYPE zst_logpmasivo_bps.
DATA t_alvlog  TYPE TABLE OF zst_logpmasivo_bps.
*&---------------------------------------------------------------------------*
*&  INTERNAL TABLES / W O R K   A R E A S
DATA: gt_object_keys TYPE TABLE OF ty_object_keys. "DNAVOA
DATA gs_file TYPE file_table.
DATA t_file TYPE TABLE OF file_table.

DATA t_bindata TYPE w3mimetabtype.

DATA lt_worknames TYPE if_fdt_doc_spreadsheet=>t_worksheet_names.

DATA gs_return  TYPE bapireti.
DATA t_return   TYPE bapiretm.
DATA t_return_d TYPE bapiretm.
DATA t_msje     TYPE bapiretct.
DATA gs_message TYPE bapiret2.
DATA gs_mess    TYPE bapiretc.
DATA gt_mess    TYPE bapiretct.

"========> Reportes Log ALV
" -> Generales
DATA gs_gnrales  TYPE ty_gnrales.
DATA t_gnrales   TYPE STANDARD TABLE OF ty_gnrales."TYPE TABLE OF ty_gnrales.

"--> Anexo
DATA gs_anexo TYPE ty_anexo.
DATA t_anexo  TYPE TABLE OF ty_anexo.

" -> Direccion Entregas
DATA gs_direntg  TYPE ty_direntg.
DATA t_direntg   TYPE TABLE OF ty_direntg.

" -> Proveedor
DATA gs_provedor TYPE ty_provedor.
DATA t_provedor  TYPE TABLE OF ty_provedor.

" -> Proveedor Compras
DATA gs_provcomp TYPE ty_provcomp.
DATA t_provcomp  TYPE TABLE OF ty_provcomp.

" -> Cliente Financiero
DATA gs_cntefinan TYPE ty_cntefinan.
DATA t_cntefinan  TYPE TABLE OF ty_cntefinan.

" -> Cliente Ventas
DATA gs_cntevntas TYPE ty_cntevntas.
DATA t_cntevntas  TYPE TABLE OF ty_cntevntas.

"=========> Pestañas de EXCEL

DATA gs_pstgnral  TYPE ty_pstgnral.
DATA t_pstgnral   TYPE TABLE OF ty_pstgnral.

DATA gs_pstfiscal TYPE ty_pstfiscal.
DATA t_pstfiscal  TYPE TABLE OF ty_pstfiscal.

DATA gs_pstanexo  TYPE ty_pstanexo.
DATA t_pstanexo   TYPE TABLE OF ty_pstanexo.

DATA gs_pstcliente TYPE ty_pstcliente.
DATA t_pstcliente  TYPE TABLE OF ty_pstcliente.

DATA gs_pstentrega TYPE ty_pstentrega.
DATA t_pstentrega  TYPE TABLE OF ty_pstentrega.

DATA gs_pstempresa TYPE ty_pstempresa.
DATA t_pstempresa  TYPE TABLE OF ty_pstempresa.

DATA gs_pstherram  TYPE ty_pstherram.
DATA t_pstherram   TYPE TABLE OF ty_pstherram.

DATA gs_pstinstruc TYPE ty_pstinstruc.
DATA t_pstinstruc  TYPE TABLE OF ty_pstinstruc.

DATA gs_pstcntret  TYPE ty_pstcntret.
DATA t_pstcntret   TYPE TABLE OF ty_pstcntret.

DATA gs_pstcntevta TYPE ty_pstcntevta.
DATA t_pstcntevta  TYPE TABLE OF ty_pstcntevta.

DATA gs_pstcnteint TYPE ty_pstcnteint.
DATA t_pstcnteint  TYPE TABLE OF ty_pstcnteint.

DATA gs_pstprov    TYPE ty_pstprov.
DATA t_pstprov     TYPE TABLE OF ty_pstprov.

DATA gs_pstprovrnt TYPE ty_pstprovrnt.
DATA t_pstprovrnt  TYPE TABLE OF ty_pstprovrnt.

DATA gs_pstprovint TYPE ty_pstprovint.
DATA t_pstprovint  TYPE TABLE OF ty_pstprovint.

** INI DNAVOA 13.06.2025
DATA gs_pstprovcla TYPE ty_pstprovcla.
DATA t_pstprovcla  TYPE TABLE OF ty_pstprovcla.
** FIN DNAVOA 13.06.2025

DATA ls_com TYPE cmds_ei_company.
DATA lt_com TYPE cmds_ei_company_t.

DATA ls_roles TYPE bus_ei_bupa_roles.
DATA lt_roles TYPE bus_ei_bupa_roles_t.

DATA ls_data TYPE cvis_ei_extern.
DATA lt_data TYPE TABLE OF cvis_ei_extern.

*** INI DNAVOA 06.13.2025
DATA: ls_taxind TYPE cmds_ei_tax_ind,
      lt_taxind TYPE cmds_ei_tax_ind_t.
*** FIN DNAVOA 06.13.2025

DATA ls_address TYPE bus_ei_bupa_address.
DATA lt_address TYPE bus_ei_bupa_address_t.

DATA ls_tax TYPE bus_ei_bupa_taxnumber.
DATA lt_tax TYPE bus_ei_bupa_taxnumber_t.

DATA ls_bank TYPE bus_ei_bupa_bankdetail.
DATA lt_bank TYPE bus_ei_bupa_bankdetail_t.

DATA ls_seg TYPE ukm_ei_bp_cms_sgm.
DATA lt_seg TYPE ukmt_ei_bp_cms_sgm.

DATA ls_wax_type   TYPE cmds_ei_wtax_type.
DATA ls_wax_type_s TYPE cmds_ei_wtax_type_s.

DATA: ls_sales  TYPE cmds_ei_sales.
DATA: lt_sales  TYPE cmds_ei_sales_t.

DATA: ls_functions  TYPE cmds_ei_functions.
DATA: lt_functions  TYPE cmds_ei_functions_t.

DATA ls_company     TYPE vmds_ei_company.
DATA lt_company     TYPE vmds_ei_company_t.

DATA ls_company_wax_type   TYPE vmds_ei_wtax_type.
DATA ls_company_wax_type_s TYPE vmds_ei_wtax_type_s.

DATA ls_purchasing   TYPE  vmds_ei_purchasing.
DATA lt_purchasing   TYPE  vmds_ei_purchasing_t.

DATA: ls_func_prov  TYPE vmds_ei_functions.
DATA: lt_func_prov  TYPE vmds_ei_functions_t.

DATA ls_phone TYPE bus_ei_bupa_telephone.
DATA lt_phone TYPE bus_ei_bupa_telephone_t.

DATA ls_email TYPE bus_ei_bupa_smtp.
DATA lt_email TYPE bus_ei_bupa_smtp_t.


DATA lt_return  TYPE bapiretm.
DATA ls_return  TYPE bapireti.
DATA ls_message TYPE bapiret2.

** INI DNAVOA

TYPES: begin of ty_data_d,
  lt_direcc_d TYPE TABLE OF cvis_ei_extern WITH DEFAULT KEY,
  END OF ty_data_d.

TYPES: BEGIN OF ty_data_deep,
         code      TYPE c LENGTH 30,
         lt_data   TYPE TABLE OF cvis_ei_extern WITH DEFAULT KEY,
         lt_data_d TYPE TABLE OF ty_data_d WITH DEFAULT KEY,
       END OF ty_data_deep.

DATA: lt_data_deep TYPE TABLE OF ty_data_deep,
      ls_data_deep TYPE ty_data_deep.

** FIN DNAVOA
*&---------------------------------------------------------------------------*
*&  C O N S T A N T S                                                        *
*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'BPS'
CONSTANTS: BEGIN OF c_bps,
             tab1 LIKE sy-ucomm VALUE 'BPS_FC1',
             tab2 LIKE sy-ucomm VALUE 'BPS_FC2',
             tab3 LIKE sy-ucomm VALUE 'BPS_FC3',
             tab4 LIKE sy-ucomm VALUE 'BPS_FC4',
             tab5 LIKE sy-ucomm VALUE 'BPS_FC5',
             tab6 LIKE sy-ucomm VALUE 'BPS_FC6',
             tab7 LIKE sy-ucomm VALUE 'BPS_FC7',
             tab8 LIKE sy-ucomm VALUE 'BPS_FC8',
           END OF c_bps.

CONSTANTS: BEGIN OF cx,
             atbps(5)  TYPE c VALUE 'ATBPS',   " Alta de BPS
             modbps(6) TYPE c VALUE 'MODBPS',  " Modificación BPS
             actbps(6) TYPE c VALUE 'ACTBPS',  " Actualizar Interlocutor Comercial
             altdir(6) TYPE c VALUE 'ALTDIR',  " Alta DireccioneEntrega
             cgranx(6) TYPE c VALUE 'CGRANX',  " Carga de Anexo
             aherrm(6) TYPE c VALUE 'AHERRM',  " Alta Herramentales
             ainstr(6) TYPE c VALUE 'AINSTR',  " Alta Instrucciones
             provcl(6) TYPE c VALUE 'PROVCL',  " Provedor clasificacion
           END OF cx.

CONSTANTS lc_flcu00(6) TYPE c VALUE 'FLCU00'.
CONSTANTS lc_flcu01(6) TYPE c VALUE 'FLCU01'.
CONSTANTS lc_ukm000(6) TYPE c VALUE 'UKM000'.
CONSTANTS lc_flvn00(6) TYPE c VALUE 'FLVN00'.
CONSTANTS lc_flvn01(6) TYPE c VALUE 'FLVN01'.
CONSTANTS lc_crm002(6) TYPE c VALUE 'CRM002'.


*&---------------------------------------------------------------------------*
*&  V A R I A B L E S                                                        *
DATA gv_rc       TYPE i.
DATA gv_filename TYPE rlgrap-filename.
DATA lv_bp       TYPE c LENGTH 30.
DATA lv_tipomsje TYPE c.

*&SPWIZARD: DATA FOR TABSTRIP 'BPS'
CONTROLS:  bps TYPE TABSTRIP.

DATA: BEGIN OF g_bps,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZFIR001',
        pressed_tab LIKE sy-ucomm VALUE c_bps-tab1,
      END OF g_bps.

DATA gv_ejecflag TYPE c.
DATA ok_code LIKE sy-ucomm.

* Custom container data
DATA:
  container TYPE REF TO cl_gui_custom_container,
  picture   TYPE REF TO cl_gui_picture,
  url(132),
  init.

DATA gv_mess TYPE char20.
CLASS cl_gui_cfw DEFINITION LOAD.
*&---------------------------------------------------------------------------*
*&  F I E L D   S Y M B O L S                                                *
FIELD-SYMBOLS <fs_value> TYPE any.
FIELD-SYMBOLS <fs_struc> TYPE any.
FIELD-SYMBOLS <ft_xls>   TYPE STANDARD TABLE.

*&---------------------------------------------------------------------------*
*&  R A N G E S                                                              *

*&---------------------------------------------------------------------------*
*&  S E L E C T I O N   S C R E E N                                          *

SELECTION-SCREEN BEGIN OF BLOCK b1.

  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN SKIP.

  PARAMETERS: p_file TYPE rlgrap-filename.

  SELECTION-SCREEN SKIP.

  PARAMETERS chk_test AS CHECKBOX DEFAULT 'X'.

  SELECTION-SCREEN SKIP.

  PARAMETERS: r_crbps RADIOBUTTON GROUP gr1 DEFAULT 'X', " Alta de BPS
              r_mdbps RADIOBUTTON GROUP gr1, " Modificacion BPS
              r_acint RADIOBUTTON GROUP gr1, " Actualizacion Interlocutor
              r_atdir RADIOBUTTON GROUP gr1, " Alta Direcciones
              r_cranx RADIOBUTTON GROUP gr1. " Carga Anexos

SELECTION-SCREEN END OF BLOCK b1.

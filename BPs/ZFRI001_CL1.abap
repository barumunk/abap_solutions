*&---------------------------------------------------------------------*
*& Include          ZFRI001_CL1
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA o_event TYPE REF TO lcl_event_receiver. "Events

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:

      handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid.


ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_double_click.

    DATA lt_num  TYPE lvc_t_roid. "No. of rows selected
    DATA lt_rows TYPE lvc_t_row.

    DATA lt_cell TYPE	lvc_t_cell.

*    CALL METHOD obj_gridlog->get_selected_rows
*      IMPORTING
*        et_index_rows = lt_rows
*        et_row_no     = lt_num.

    CALL METHOD obj_gridlog->get_selected_cells
      IMPORTING
        et_cell = lt_cell.

  ENDMETHOD.

ENDCLASS.

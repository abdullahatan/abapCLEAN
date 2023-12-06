*&---------------------------------------------------------------------*
*& Program YCLEAN_P006
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM yclean_p006.

INCLUDE yclean_p006_clsdat.

LOAD-OF-PROGRAM.
  DATA(_application) = lcl_sqlhelp=>instance_app( ).

AT SELECTION-SCREEN OUTPUT.
  _application->at_selection_screen_output( ).

AT SELECTION-SCREEN.
  _application->at_selection_screen( ).

START-OF-SELECTION.
  _application->write_sql_resulst( _application->run_sql_query(
    EXPORTING
      im_cobk = p_cobk
      im_coep = p_coep
      im_cosp = p_cosp
      im_coka = p_coka
      im_acdo = p_acdo ) ).

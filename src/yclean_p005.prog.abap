*&---------------------------------------------------------------------*
*& Program YCLEAN_P005
*&---------------------------------------------------------------------*
*& @author Abdullah ATAN <abdullah.atan@forcode.com.tr>
*&---------------------------------------------------------------------*
PROGRAM yclean_p005.

INCLUDE yclean_p005_clsdat.

LOAD-OF-PROGRAM.
  app_session = application=>app_instance( ).

INITIALIZATION.
  app_session->initialization( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_rz12.
  app_session->at_selection_screen_request(
    EXPORTING
      iv_fieldname = app_session->mc_cons-rz12 ).

AT SELECTION-SCREEN.
  app_session->at_selection_screen( ).

AT SELECTION-SCREEN OUTPUT.
  app_session->at_selection_screen_output( ).

START-OF-SELECTION.
  app_session->start_of_selection( ).

END-OF-SELECTION.
  app_session->end_of_selection( ).

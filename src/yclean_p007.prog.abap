*&---------------------------------------------------------------------*
*& Program YCLEAN_P007
*&---------------------------------------------------------------------*
*& @author: Abdullah ATAN <abdullah.atan@forcode.com.tr>
*&---------------------------------------------------------------------*
PROGRAM yclean_p007.

INCLUDE yclean_p007_clsdat.

LOAD-OF-PROGRAM.
  app_session = lcl_times=>app_instance( ).

INITIALIZATION.
  app_session->initialization( ).

AT SELECTION-SCREEN.
  app_session->at_selection_screen( ).

START-OF-SELECTION.
  app_session->start_of_selection( ).

end-OF-SELECTION.
  app_session->end_of_selection( ).

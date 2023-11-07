FUNCTION yclean_fm11.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_JAHRPER) TYPE  YCLEAN_TT05
*"     VALUE(IV_CURTP) TYPE  FINS_CURTYPE
*"     VALUE(IV_DOCREF) TYPE  YCLEAN_TT07 OPTIONAL
*"     VALUE(IV_LOGID) TYPE  BALNREXT
*"  EXCEPTIONS
*"      CONTAINS_ERROR
*"----------------------------------------------------------------------
  DATA: _msg TYPE c LENGTH 150.
  DATA(_logger) = yclean_bc_logger_toolkit=>get_instance(
  EXPORTING
    iv_log_object = mc_logger-_object
    iv_log_subobject = mc_logger-_subobject
    iv_extnumber = iv_logid ).

  _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '002' iv_msgv1 = TEXT-t14 iv_msgv2 = |Yıl:{ VALUE #( iv_jahrper[ 1 ]-jahrper(4) OPTIONAL ) };PB türü:{ iv_curtp }| ).
  _logger->post( iv_refresh = abap_true ).
  TRY.
      yclean_cl03=>_rb3_rundat(
        EXPORTING
          iv_jahrper = iv_jahrper
          iv_curtp   = iv_curtp
          iv_docref  = iv_docref
        IMPORTING
          ev_subrc  = DATA(_subrc)
          ev_result = DATA(_result) ).
      IF _subrc IS INITIAL.
        COMMIT WORK AND WAIT.
        _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '019' iv_msgv1 = TEXT-t14 ).
      ELSE.
        _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '004' iv_msgv1 = TEXT-t14 iv_msgv2 = _result ).
      ENDIF.
    CATCH cx_amdp_error INTO DATA(lv_exp).
      CLEAR: _msg. _msg = lv_exp->get_longtext( ).
      _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t14 iv_msgv2 = _msg+0(50) iv_msgv3 = _msg+50(50) iv_msgv4 = _msg+100(50) ).
  ENDTRY.
  _logger->post( iv_refresh = abap_true ).

ENDFUNCTION.

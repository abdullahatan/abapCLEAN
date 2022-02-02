FUNCTION yclean_fm03.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_RLDNR) TYPE  FINS_LEDGER
*"     VALUE(IV_BUKRS) TYPE  BUKRS
*"     VALUE(IV_GJAHR) TYPE  GJAHR
*"     VALUE(IV_LOGID) TYPE  BALNREXT
*"----------------------------------------------------------------------

  DATA(_logger) = yclean_bc_logger_toolkit=>get_instance(
    EXPORTING
      iv_log_object = mc_logger-_object
      iv_log_subobject = mc_logger-_subobject
      iv_extnumber = iv_logid ).

  _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '002' iv_msgv1 = TEXT-t03 iv_msgv2 = |{ TEXT-p03 }:{ iv_rldnr }| iv_msgv3 = |{ TEXT-p01 }:{ iv_bukrs }| iv_msgv4 = |{ TEXT-p05 }:{ iv_gjahr }| ).
  _logger->post( iv_refresh = abap_true iv_commit = abap_true ).
  TRY.
      yclean_cl01=>_rb3_rundat(
        EXPORTING
          iv_rldnr  = iv_rldnr
          iv_bukrs  = iv_bukrs
          iv_gjahr  = iv_gjahr
        IMPORTING
          ev_subrc  = DATA(_subrc)
          ev_result = DATA(_result) ).
      IF _subrc IS INITIAL.
        _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '003' iv_msgv1 = TEXT-t03 ).
      ELSE.
        _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '004' iv_msgv1 = TEXT-t03 iv_msgv2 = _result ).
      ENDIF.
    CATCH cx_amdp_error INTO DATA(lv_exp).
      _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t03 iv_msgv2 = lv_exp->get_text( ) ).
  ENDTRY.
  _logger->post( iv_refresh = abap_true ).

ENDFUNCTION.

FUNCTION yclean_fm01.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BUKRS) TYPE  BUKRS
*"     VALUE(IV_BSTAT) TYPE  BSTAT_D
*"     VALUE(IV_BELNR) TYPE  STRING OPTIONAL
*"     VALUE(IV_LOGID) TYPE  BALNREXT
*"----------------------------------------------------------------------

  DATA(_logger) = yclean_bc_logger_toolkit=>get_instance(
    EXPORTING
      iv_log_object = mc_logger-_object
      iv_log_subobject = mc_logger-_subobject
      iv_extnumber = iv_logid ).

  _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '002' iv_msgv1 = TEXT-t01 iv_msgv2 = |{ TEXT-p01 }:{ iv_bukrs }| iv_msgv3 = |{ TEXT-p02 }:{ iv_bstat }| ).
  _logger->post( iv_refresh = abap_true iv_commit = abap_true ).
  TRY.
      yclean_cl01=>_rb1_rundat(
        EXPORTING
          iv_bukrs  = iv_bukrs
          iv_bstat  = iv_bstat
          iv_belnr  = iv_belnr
        IMPORTING
          ev_subrc  = DATA(_subrc)
          ev_result = DATA(_result) ).
      IF _subrc IS INITIAL.
        _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '003' iv_msgv1 = TEXT-t01 ).
      ELSE.
        _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '004' iv_msgv1 = TEXT-t01 iv_msgv2 = _result ).
      ENDIF.
    CATCH cx_amdp_error INTO DATA(lv_exp).
      _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t01 iv_msgv2 = lv_exp->get_text( ) ).
  ENDTRY.
  _logger->post( iv_refresh = abap_true ).

ENDFUNCTION.

FUNCTION yclean_fm02.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_KIND) TYPE  YCLEAN_DE01
*"     VALUE(IV_RLDNR) TYPE  FINS_LEDGER
*"     VALUE(IV_BUKRS) TYPE  BUKRS
*"     VALUE(IV_GJAHR) TYPE  GJAHR
*"     VALUE(IV_BSTAT) TYPE  BSTAT_D OPTIONAL
*"     VALUE(IV_BUDAT) TYPE  YCLEAN_TT01 OPTIONAL
*"     VALUE(IV_SPMON) TYPE  JAHRPER OPTIONAL
*"     VALUE(IV_BELNR) TYPE  STRING OPTIONAL
*"     VALUE(IV_LOGID) TYPE  BALNREXT
*"  EXCEPTIONS
*"      KIND_INVALID
*"----------------------------------------------------------------------

  DATA(_logger) = yclean_bc_logger_toolkit=>get_instance(
    EXPORTING
      iv_log_object = mc_logger-_object
      iv_log_subobject = mc_logger-_subobject
      iv_extnumber = iv_logid ).

  CASE iv_kind.
    WHEN 'RB1'.
      _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '002' iv_msgv1 = TEXT-t02 iv_msgv2 = |{ TEXT-p03 }:{ iv_rldnr }| iv_msgv3 = |{ TEXT-p01 }:{ iv_bukrs }| iv_msgv4 = |{ TEXT-p04 }:{ iv_spmon+4(3) }.{ iv_spmon(4) }| ).
      _logger->post( iv_refresh = abap_true iv_commit = abap_true ).
      TRY.
          yclean_cl01=>_rb2a_rundat(
            EXPORTING
              iv_rldnr  = iv_rldnr
              iv_bukrs  = iv_bukrs
              iv_gjahr  = iv_gjahr
              iv_budat  = iv_budat
              iv_belnr  = iv_belnr
            IMPORTING
              ev_subrc  = DATA(_subrc)
              ev_rowcnt = DATA(_rowcnt)
              ev_result = DATA(_result) ).
          IF _subrc IS INITIAL.
            _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '003' iv_msgv1 = TEXT-t02 iv_msgv2 = |{ _rowcnt NUMBER = USER }| ).
          ELSE.
            _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '004' iv_msgv1 = TEXT-t02 iv_msgv2 = _result ).
          ENDIF.
        CATCH cx_amdp_error INTO DATA(lv_exp).
          _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t02 iv_msgv2 = lv_exp->get_text( ) ).
      ENDTRY.
      _logger->post( iv_refresh = abap_true ).

    WHEN 'RB2'.
      _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '002' iv_msgv1 = TEXT-t02 iv_msgv2 = |{ TEXT-p03 }:{ iv_rldnr }| iv_msgv3 = |{ TEXT-p01 }:{ iv_bukrs }| iv_msgv4 = |{ TEXT-p04 }:{ iv_spmon+4(3) }.{ iv_spmon(4) }| ).
      _logger->post( iv_refresh = abap_true iv_commit = abap_true ).
      TRY.
          yclean_cl01=>_rb2b_rundat(
            EXPORTING
              iv_rldnr  = iv_rldnr
              iv_bukrs  = iv_bukrs
              iv_gjahr  = iv_gjahr
              iv_bstat  = iv_bstat
              iv_spmon  = iv_spmon
              iv_belnr  = iv_belnr
            IMPORTING
              ev_subrc  = _subrc
              ev_rowcnt = _rowcnt
              ev_result = _result ).
          IF _subrc IS INITIAL.
            _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '003' iv_msgv1 = TEXT-t02 iv_msgv2 = |{ _rowcnt NUMBER = USER }| ).
          ELSE.
            _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '004' iv_msgv1 = TEXT-t02 iv_msgv2 = _result ).
          ENDIF.
        CATCH cx_amdp_error INTO lv_exp.
          _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t02 iv_msgv2 = lv_exp->get_text( ) ).
      ENDTRY.
      _logger->post( iv_refresh = abap_true ).
    WHEN OTHERS.
      MESSAGE e006(yclean) RAISING kind_invalid.
  ENDCASE.

ENDFUNCTION.

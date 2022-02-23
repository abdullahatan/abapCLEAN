FUNCTION yclean_fm08.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_RKIND) TYPE  YCLEAN_DE02
*"     VALUE(IV_FKART) TYPE  FKART OPTIONAL
*"     VALUE(IT_FKDAT) TYPE  YCLEAN_TT02 OPTIONAL
*"     VALUE(IV_WHERE) TYPE  STRING OPTIONAL
*"     VALUE(IV_KAPPL) TYPE  KAPPL OPTIONAL
*"     VALUE(IV_KWERT) TYPE  VFPRC_ELEMENT_VALUE OPTIONAL
*"     VALUE(IT_KSCHL) TYPE  YCLEAN_TT03 OPTIONAL
*"     VALUE(IV_SPMON) TYPE  SPMON OPTIONAL
*"     VALUE(IV_BCKUP) TYPE  ABAP_BOOLEAN OPTIONAL
*"     VALUE(IV_LOGID) TYPE  BALNREXT
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      KIND_INVALID
*"----------------------------------------------------------------------

  DATA: _msgdat TYPE c LENGTH 150.

  DATA(_logger) = yclean_bc_logger_toolkit=>get_instance(
    EXPORTING
      iv_log_object = mc_logger-_object
      iv_log_subobject = mc_logger-_subobject
      iv_extnumber = iv_logid ).

  CASE iv_rkind.
*&---------------------------------------------------------------------*
*&  PRCD_ELMENTS Save Dat->
*&---------------------------------------------------------------------*
    WHEN yclean_cl02=>mc_cons-save.
      _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '009' iv_msgv1 = TEXT-t09 iv_msgv2 = |{ TEXT-p06 }:{ iv_fkart }| iv_msgv3 = |{ TEXT-p04 }:{ iv_spmon+4(2) }.{ iv_spmon(4) }| ).
      _logger->post( iv_refresh = abap_true ).
      TRY.
          yclean_cl02=>_rb1_rundat(
            EXPORTING
                iv_fkart = iv_fkart
                it_fkdat = it_fkdat
                iv_where = iv_where
                iv_kappl = iv_kappl
                iv_kwert = iv_kwert
                it_kschl = it_kschl
              IMPORTING
                ev_subrc  = DATA(_subrc)
                ev_rowcnt = DATA(_rowcnt)
                ev_result = DATA(_result) ).
          IF _subrc IS INITIAL.
            _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '010' iv_msgv1 = TEXT-t09 iv_msgv2 = |{ _rowcnt NUMBER = USER }| ).
          ELSE.
            _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '011' iv_msgv1 = TEXT-t09 iv_msgv2 = _result ).
          ENDIF.
        CATCH cx_amdp_error INTO DATA(lv_exp).
          CLEAR: _msgdat. _msgdat = lv_exp->get_text( ).
          _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t09 iv_msgv2 = _msgdat(50) iv_msgv3 = _msgdat+50(50) iv_msgv4 = _msgdat+100(50) ).
      ENDTRY.
      _logger->post( iv_refresh = abap_true ).

*&---------------------------------------------------------------------*
*&  PRCD_ELMENTS Delete Dat->
*&---------------------------------------------------------------------*
    WHEN yclean_cl02=>mc_cons-delete.
      _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '002' iv_msgv1 = TEXT-t10 iv_msgv2 = |{ TEXT-p06 }:{ iv_fkart }| iv_msgv3 = |{ TEXT-p04 }:{ iv_spmon+4(2) }.{ iv_spmon(4) }| ).
      _logger->post( iv_refresh = abap_true ).
      TRY.
          CLEAR: _subrc, _rowcnt, _result.
          yclean_cl02=>_rb2_rundat(
            EXPORTING
                iv_fkart = iv_fkart
                it_fkdat = it_fkdat
                iv_where = iv_where
                iv_kappl = iv_kappl
                iv_kwert = iv_kwert
                it_kschl = it_kschl
              IMPORTING
                ev_subrc  = _subrc
                ev_rowcnt = _rowcnt
                ev_result = _result ).
          IF _subrc IS INITIAL.
            _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '003' iv_msgv1 = TEXT-t10 iv_msgv2 = |{ _rowcnt NUMBER = USER }| ).
          ELSE.
            _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '004' iv_msgv1 = TEXT-t10 iv_msgv2 = _result ).
          ENDIF.
        CATCH cx_amdp_error INTO lv_exp.
          CLEAR: _msgdat. _msgdat = lv_exp->get_text( ).
          _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t10 iv_msgv2 = _msgdat(50) iv_msgv3 = _msgdat+50(50) iv_msgv4 = _msgdat+100(50) ).
      ENDTRY.
      _logger->post( iv_refresh = abap_true ).

*&---------------------------------------------------------------------*
*&  PRCD_ELMENTS Restored Dat->
*&---------------------------------------------------------------------*
    WHEN yclean_cl02=>mc_cons-restore.
      _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '014' iv_msgv1 = TEXT-t11 iv_msgv2 = |{ TEXT-p06 }:{ iv_fkart }| iv_msgv3 = |{ TEXT-p04 }:{ iv_spmon+4(2) }.{ iv_spmon(4) }| ).
      _logger->post( iv_refresh = abap_true ).
      TRY.
          CLEAR: _subrc, _rowcnt, _result.
          yclean_cl02=>_rb3_rundat(
            EXPORTING
                iv_fkart = iv_fkart
                it_fkdat = it_fkdat
                iv_where = iv_where
              IMPORTING
                ev_subrc  = _subrc
                ev_rowcnt = _rowcnt
                ev_result = _result ).
          IF _subrc IS INITIAL.
            _logger->add_success( iv_msgid = mc_msg-id iv_msgno = '015' iv_msgv1 = TEXT-t11 iv_msgv2 = |{ _rowcnt NUMBER = USER }| ).
          ELSE.
            _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '016' iv_msgv1 = TEXT-t11 iv_msgv2 = _result ).
          ENDIF.
        CATCH cx_amdp_error INTO lv_exp.
          CLEAR: _msgdat. _msgdat = lv_exp->get_text( ).
          _logger->add_error( iv_msgid = mc_msg-id iv_msgno = '005' iv_msgv1 = TEXT-t11 iv_msgv2 = _msgdat(50) iv_msgv3 = _msgdat+50(50) iv_msgv4 = _msgdat+100(50) ).
      ENDTRY.
      _logger->post( iv_refresh = abap_true ).

    WHEN OTHERS.
      MESSAGE e006(yclean) RAISING kind_invalid.
  ENDCASE.

ENDFUNCTION.

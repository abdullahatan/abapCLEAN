*&---------------------------------------------------------------------*
*& Include          YCLEAR_P001_CLSDAT
*&---------------------------------------------------------------------*
TYPE-POOLS: vrm.
TABLES: sscrfields, acdoca, bkpf, t093.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS: p_rb1 RADIOBUTTON GROUP gr1 DEFAULT 'X' USER-COMMAND cmd1 MODIF ID rd1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: p_rb1a RADIOBUTTON GROUP gr2 DEFAULT 'X' USER-COMMAND cmd2 MODIF ID rd2.
    SELECTION-SCREEN COMMENT (50) TEXT-p01 FOR FIELD p_rb1a MODIF ID rd2.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: p_rb1b RADIOBUTTON GROUP gr2 MODIF ID rd2.
    SELECTION-SCREEN COMMENT (50) TEXT-p02 FOR FIELD p_rb1b MODIF ID rd2.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: p_rb2 RADIOBUTTON GROUP gr1 MODIF ID rd1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: p_rb2a RADIOBUTTON GROUP gr3 DEFAULT 'X' USER-COMMAND cmd2 MODIF ID rd3.
    SELECTION-SCREEN COMMENT (30) TEXT-p03 FOR FIELD p_rb2a MODIF ID rd3.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: p_rb2b RADIOBUTTON GROUP gr3 MODIF ID rd3.
    SELECTION-SCREEN COMMENT (30) TEXT-p04 FOR FIELD p_rb2b MODIF ID rd3.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS:
    p_rb3 RADIOBUTTON GROUP gr1 MODIF ID rd1,
    p_rb4 RADIOBUTTON GROUP gr1 MODIF ID rd1,
    p_rb5 RADIOBUTTON GROUP gr1 MODIF ID rd1,
    p_rb6 RADIOBUTTON GROUP gr1 MODIF ID rd1,
    p_rb7 RADIOBUTTON GROUP gr1 MODIF ID rd1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  PARAMETERS:
    p_rldnr TYPE rldnr AS LISTBOX VISIBLE LENGTH 6 DEFAULT 'ML' OBLIGATORY MODIF ID m01,
    p_bukrs TYPE bukrs DEFAULT '1001' MODIF ID m02,
    p_gjahr TYPE gjahr DEFAULT sy-datum(4) MODIF ID m03,
    p_monat TYPE monat DEFAULT '01' MODIF ID m04,
    p_bstat TYPE bstat_d DEFAULT 'L' MODIF ID m05.
  SELECT-OPTIONS:
   s_afabe FOR t093-afaber NO INTERVALS MODIF ID m06,
   s_belnr FOR acdoca-belnr NO INTERVALS MODIF ID m07,
   s_ldgrp FOR bkpf-ldgrp NO INTERVALS MODIF ID m08.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s03.
  PARAMETERS: p_prll TYPE int1 AS LISTBOX VISIBLE LENGTH 6 DEFAULT 1 OBLIGATORY MODIF ID m09,
              p_rz12 TYPE rzlli_apcl DEFAULT '' MODIF ID m10.
SELECTION-SCREEN END OF BLOCK b3.

CLASS application DEFINITION DEFERRED.
DATA: app_session TYPE REF TO application.

*----------------------------------------------------------------------*
*       CLASS application DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS application DEFINITION .

  PUBLIC SECTION.
    CLASS-DATA:
      app TYPE REF TO application.

    CONSTANTS: BEGIN OF mc_cons,
                 rz12 TYPE fieldname VALUE 'P_RZ12',
               END OF mc_cons.

    DATA: mv_tabname  TYPE char40,
          mv_taskname TYPE char20,
          mv_logid    TYPE balnrext.

    CLASS-METHODS:
      initialization,
      at_selection_screen,
      at_selection_screen_output,
      at_selection_screen_request
        IMPORTING
          !iv_fieldname TYPE clike,
      start_of_selection,
      end_of_selection,
      app_instance
        RETURNING
          VALUE(mo_app) TYPE REF TO application.

    METHODS:
      listbox_build,
      f4_finanscal_perio
        CHANGING
          cv_spmon TYPE s031-spmon,
      f4_server_group
        CHANGING
          cv_rz12 TYPE rzlli_apcl,
      rundat_control
        EXCEPTIONS
          contains_error,
      retrieve_dat
        EXCEPTIONS
          contains_error,
      generate_guid
        RETURNING
          VALUE(rv_guid) TYPE guid_16,
      popup_confirm
        IMPORTING
          !im_titlebar     TYPE clike
          !im_question     TYPE clike
        RETURNING
          VALUE(rv_answer) TYPE char1,
      show_message_tool
        IMPORTING
          !iv_msgdat TYPE bapiret2_tab.

  PRIVATE SECTION.
    DATA: mt_msgdat TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY.


ENDCLASS .                    "application DEFINITION

*----------------------------------------------------------------------*
*       CLASS application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS application IMPLEMENTATION .

  METHOD initialization.
    app->listbox_build( ).
  ENDMETHOD.                    "initialization
  METHOD at_selection_screen.

    CASE sy-ucomm.
      WHEN 'ONLI'.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "at_selection_screen
  METHOD at_selection_screen_output.

    LOOP AT SCREEN.
      CLEAR: app_session->mv_tabname.
      IF screen-group1 EQ 'M02'.
        screen-input = 0.
      ENDIF.
      CASE abap_true.
        WHEN p_rb1.
          IF screen-group1 EQ 'RD2'.
            screen-active = 1.
          ELSEIF screen-group1 EQ 'RD3' OR screen-group1 EQ 'M01' OR screen-group1 EQ 'M04' OR screen-group1 EQ 'M06' OR screen-group1 EQ 'M08' OR screen-group1 EQ 'M09' OR screen-group1 EQ 'M10'.
            screen-active = 0.
          ENDIF.
          IF screen-group1 EQ 'M05' OR screen-group1 EQ 'M08'.
            screen-input = 0.
          ENDIF.
          CASE abap_true.
            WHEN p_rb1a.
              p_bstat = 'L'.
              IF screen-group1 EQ 'M02' OR screen-group1 EQ 'M03' OR screen-group1 EQ 'M05' OR screen-group1 EQ 'M07'.
                screen-active = 1.
              ENDIF.
            WHEN p_rb1b.
              p_bstat = 'U'.
              IF screen-group1 EQ 'M02' OR screen-group1 EQ 'M03' OR screen-group1 EQ 'M05' OR screen-group1 EQ 'M08'.
                screen-active = 1.
              ELSEIF screen-group1 EQ 'M07'.
                screen-active = 0.
              ENDIF.
          ENDCASE.
          FREE: s_ldgrp[].
          s_ldgrp[] = VALUE #( sign = 'I' option = 'EQ' ( low = 'IF' ) ( low = 'ML' )  ).
          app_session->mv_tabname = TEXT-t01.
        WHEN p_rb2.
          IF screen-group1 EQ 'M01' OR screen-group1 EQ 'M02' OR screen-group1 EQ 'M03' OR screen-group1 EQ 'M04' OR screen-group1 EQ 'M07' OR screen-group1 EQ 'M09' OR screen-group1 EQ 'M10'.
            screen-active = 1.
          ELSEIF screen-group1 EQ 'RD2' OR screen-group1 EQ 'M06' OR screen-group1 EQ 'M05' OR screen-group1 EQ 'M08'.
            screen-active = 0.
          ENDIF.
          CASE abap_true.
            WHEN p_rb2a.
              IF screen-group1 EQ 'M04' .
                screen-active = 1.
              ENDIF.
            WHEN p_rb2b.
              IF screen-group1 EQ 'M04' OR screen-group1 EQ 'M09' OR screen-group1 EQ 'M10'.
                screen-active = 0.
              ENDIF.
          ENDCASE.
          app_session->mv_tabname = TEXT-t02.
        WHEN p_rb3.
          IF screen-group1 EQ 'M01' OR screen-group1 EQ 'M02' OR screen-group1 EQ 'M03'.
            screen-active = 1.
          ELSEIF screen-group1 EQ 'RD2' OR screen-group1 EQ 'RD3' OR screen-group1 EQ 'M04' OR screen-group1 EQ 'M05' OR screen-group1 EQ 'M06' OR screen-group1 EQ 'M07' OR
                 screen-group1 EQ 'M08' OR screen-group1 EQ 'M09' OR screen-group1 EQ 'M10'.
            screen-active = 0.
          ENDIF.
          app_session->mv_tabname = TEXT-t03.
        WHEN p_rb4.
          IF screen-group1 EQ 'M02' OR screen-group1 EQ 'M06'.
            screen-active = 1.
            screen-input = 0.
          ELSEIF screen-group1 EQ 'RD2' OR screen-group1 EQ 'RD3' OR screen-group1 EQ 'M01' OR screen-group1 EQ 'M03' OR screen-group1 EQ 'M04' OR screen-group1 EQ 'M05' OR
                 screen-group1 EQ 'M07' OR screen-group1 EQ 'M08' OR screen-group1 EQ 'M09' OR screen-group1 EQ 'M10'.
            screen-active = 0.
          ENDIF.
          FREE: s_afabe[].
          s_afabe[] = VALUE #( sign = 'I' option = 'EQ' ( low = '20' ) ( low = '30' ) ).
          app_session->mv_tabname = TEXT-t04.
        WHEN p_rb5.
          IF screen-group1 EQ 'M02' OR screen-group1 EQ 'M06'.
            screen-active = 1.
            screen-input = 0.
          ELSEIF screen-group1 EQ 'RD2' OR screen-group1 EQ 'RD3' OR screen-group1 EQ 'M01' OR screen-group1 EQ 'M03' OR screen-group1 EQ 'M04' OR screen-group1 EQ 'M05' OR
                 screen-group1 EQ 'M07' OR screen-group1 EQ 'M08' OR screen-group1 EQ 'M09' OR screen-group1 EQ 'M10'.
            screen-active = 0.
          ENDIF.
          FREE: s_afabe[].
          s_afabe[] = VALUE #( sign = 'I' option = 'EQ' ( low = '20' ) ( low = '21' ) ( low = '22' ) ( low = '30' ) ( low = '31' ) ( low = '32' ) ).
          app_session->mv_tabname = TEXT-t05.
        WHEN p_rb6.
          IF screen-group1 EQ 'M02' OR screen-group1 EQ 'M06'.
            screen-active = 1.
            screen-input = 0.
          ELSEIF screen-group1 EQ 'RD2' OR screen-group1 EQ 'RD3' OR screen-group1 EQ 'M01' OR screen-group1 EQ 'M03' OR screen-group1 EQ 'M04' OR screen-group1 EQ 'M05' OR
                 screen-group1 EQ 'M07' OR screen-group1 EQ 'M08' OR screen-group1 EQ 'M09' OR screen-group1 EQ 'M10'.
            screen-active = 0.
          ENDIF.
          FREE: s_afabe[].
          s_afabe[] = VALUE #( sign = 'I' option = 'EQ' ( low = '20' ) ( low = '30' ) ).
          app_session->mv_tabname = TEXT-t06.
        WHEN p_rb7.
          IF screen-group1 EQ 'M02' OR screen-group1 EQ 'M06'.
            screen-active = 1.
            screen-input = 0.
          ELSEIF screen-group1 EQ 'RD2' OR screen-group1 EQ 'RD3' OR screen-group1 EQ 'M01' OR screen-group1 EQ 'M03' OR screen-group1 EQ 'M04' OR screen-group1 EQ 'M05' OR
                 screen-group1 EQ 'M07' OR screen-group1 EQ 'M08' OR screen-group1 EQ 'M09' OR screen-group1 EQ 'M10'.
            screen-active = 0.
          ENDIF.
          FREE: s_afabe[].
          s_afabe[] = VALUE #( sign = 'I' option = 'EQ' ( low = '20' ) ( low = '21' ) ( low = '22' ) ( low = '30' ) ( low = '31' ) ( low = '32' ) ).
          app_session->mv_tabname = TEXT-t07.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.                    "at_selection_screen_output
  METHOD at_selection_screen_request.

    CASE iv_fieldname.
      WHEN 'P_RZ12'.
        app->f4_server_group(
          CHANGING
            cv_rz12 = p_rz12 ).
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "at_selection_screen_request
  METHOD start_of_selection.

    app->retrieve_dat(
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "start_of_selection
  METHOD end_of_selection.


  ENDMETHOD.                    "end_of_selection
  METHOD app_instance.

    FREE: mo_app, app.
    IF application=>app IS NOT BOUND.
      CREATE OBJECT application=>app.
    ENDIF.
    mo_app = app.

  ENDMETHOD.                    "app_instance
  METHOD listbox_build.

    DATA: _values TYPE vrm_values,
          _id     TYPE vrm_id.

    FREE: _values, _id.
    _id = 'P_PRLL'.
    _values = VALUE #( BASE _values ( key = '01' ) ( key = '02' ) ( key = '04' ) ( key = '06' ) ( key = '08' ) ( key = '10' ) ( key = '15' ) ).
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = _id
        values = _values.

    FREE: _values, _id.
    _id = 'P_RLDNR'.
    _values = VALUE #( BASE _values ( key = 'ML' ) ( key = 'IF' ) ).
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = _id
        values = _values.

  ENDMETHOD.                    "listbox_build
  METHOD f4_finanscal_perio.

    DATA: l_returncode1 TYPE sy-subrc,
          l_monat1      TYPE isellist-month,
          l_hlp_repid1  TYPE sy-repid.

    l_monat1 = sy-datum+0(6).
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        actual_month               = l_monat1
        factory_calendar           = ' '
        holiday_calendar           = ' '
        language                   = sy-langu
        start_column               = 8
        start_row                  = 5
      IMPORTING
        selected_month             = l_monat1
        return_code                = l_returncode1
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        month_not_found            = 3
        OTHERS                     = 4.
    IF sy-subrc = 0 AND l_returncode1 = 0.
      cv_spmon = l_monat1.
    ENDIF.

  ENDMETHOD.                    "f4_finanscal_perio
  METHOD f4_server_group.

    DATA: return_tab TYPE STANDARD TABLE OF ddshretval.
    SELECT classname, applserver, grouptype FROM rzllitab INTO TABLE @DATA(t_rzllitab) WHERE grouptype EQ 'S'.
    IF sy-subrc IS INITIAL.
      FREE: return_tab.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield    = 'CLASSNAME'
          dynpprog    = sy-repid
          dynpnr      = sy-dynnr
          dynprofield = 'P_RZ12'
          value_org   = 'S'
        TABLES
          value_tab   = t_rzllitab
          return_tab  = return_tab.
      READ TABLE return_tab REFERENCE INTO DATA(_return) INDEX 1.
      IF sy-subrc IS INITIAL.
        cv_rz12 = _return->fieldval.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "f4_server_group
  METHOD rundat_control.

    CASE abap_true.
      WHEN p_rb2.
        SELECT bkpf~* FROM bkpf
          INNER JOIN acdoca ON acdoca~rbukrs = bkpf~bukrs AND acdoca~belnr  = bkpf~belnr AND acdoca~gjahr  = bkpf~gjahr
            INTO TABLE @DATA(_bkpf) UP TO 1 ROWS
              WHERE acdoca~rbukrs = @p_bukrs
                AND acdoca~bstat  = @p_bstat.

        SELECT bseg_add~* FROM bseg_add
          INNER JOIN acdoca ON acdoca~rbukrs = bseg_add~bukrs AND acdoca~belnr  = bseg_add~belnr AND acdoca~gjahr  = bseg_add~gjahr
            INTO TABLE @DATA(_bseg_add) UP TO 1 ROWS
              WHERE acdoca~rbukrs = @p_bukrs
                AND acdoca~bstat  = @p_bstat.

        IF _bkpf[] IS NOT INITIAL OR _bseg_add[] IS NOT INITIAL.
          MESSAGE e007(yclean) RAISING contains_error.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "rundat_control
  METHOD retrieve_dat.

    DATA: _where  TYPE string,
          _result TYPE string.

*--------------------------------------------------------------------*
*-&Run controls->
*--------------------------------------------------------------------*
    app->rundat_control(
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'. RETURN.
    ENDIF.

    CHECK popup_confirm( im_titlebar = TEXT-h01 im_question = |{ mv_tabname } { TEXT-q01 }| ) EQ '1'.
    CASE abap_true.
*--------------------------------------------------------------------*
*-&Step-1: Delete From BKPF & BSEG_ADD ->
*--------------------------------------------------------------------*
      WHEN p_rb1.

        CLEAR: _where, mv_logid, mv_taskname.
        CASE abap_true.
          WHEN p_rb1a.
            _where = cl_shdb_seltab=>combine_seltabs( it_named_seltabs = VALUE #( ( name = 'BELNR' dref = REF #( s_belnr[] ) ) ) ).
            mv_taskname = |BKPF_BSEG_ADD|.
            mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.
            CALL FUNCTION 'YCLEAN_FM01' STARTING NEW TASK mv_taskname DESTINATION 'NONE'
              EXPORTING
                iv_kind         = 'RB1'
                iv_bukrs        = p_bukrs
                iv_gjahr        = p_gjahr
                iv_bstat        = p_bstat
                iv_belnr        = _where
                iv_logid        = mv_logid
              EXCEPTIONS
                parameter_error = 1
                kind_invalid    = 2
                OTHERS          = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error. RETURN.
            ENDIF.

          WHEN p_rb1b.
            _where = cl_shdb_seltab=>combine_seltabs( it_named_seltabs = VALUE #( ( name = 'LDGRP' dref = REF #( s_ldgrp[] ) ) ) ).
            mv_taskname = |BKPF|.
            mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.
            CALL FUNCTION 'YCLEAN_FM01' STARTING NEW TASK mv_taskname DESTINATION 'NONE'
              EXPORTING
                iv_kind         = 'RB2'
                iv_bukrs        = p_bukrs
                iv_gjahr        = p_gjahr
                iv_bstat        = p_bstat
                iv_ldgrp        = _where
                iv_logid        = mv_logid
              EXCEPTIONS
                parameter_error = 1
                kind_invalid    = 2
                OTHERS          = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error. RETURN.
            ENDIF.
        ENDCASE.
        cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t01 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| )->write_text( |{ mv_logid ALPHA = OUT }| )->display( ).

*--------------------------------------------------------------------*
*-&Step-2: Delete From ACDOCA ->
*--------------------------------------------------------------------*
      WHEN p_rb2.
        CLEAR: _where.
        _where = cl_shdb_seltab=>combine_seltabs( it_named_seltabs = VALUE #( ( name = 'BELNR' dref = REF #( s_belnr[] ) ) ) ).

        DATA(_output) = cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t02 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| ).
        CASE abap_true.
          WHEN p_rb2a.
            DATA(t_splitdat) = NEW yclean_cl01( )->_period_splitdat(
              EXPORTING
                  im_spmon = CONV #( |{ p_gjahr }{ p_monat }| )
                  im_split = p_prll ).

            LOOP AT t_splitdat REFERENCE INTO DATA(_splitdat).
              CLEAR: mv_logid, mv_taskname.
              mv_taskname = |ACDOCA_{ p_gjahr }{ p_monat }_{ p_rldnr }/T{ _splitdat->line }|.
              mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

              CALL FUNCTION 'YCLEAN_FM02' STARTING NEW TASK mv_taskname DESTINATION IN GROUP p_rz12
                EXPORTING
                  iv_kind      = 'RB1'
                  iv_rldnr     = p_rldnr
                  iv_bukrs     = p_bukrs
                  iv_gjahr     = p_gjahr
                  iv_spmon     = CONV jahrper( |{ p_gjahr }0{ p_monat }| )
                  iv_budat     = _splitdat->budat
                  iv_belnr     = _where
                  iv_logid     = mv_logid
                EXCEPTIONS
                  kind_invalid = 1
                  OTHERS       = 2.

              _output->write_text( |{ mv_logid ALPHA = OUT }| ).
            ENDLOOP.

          WHEN p_rb2b.
            CLEAR: mv_logid, mv_taskname.
            mv_taskname = |ADCDOCA_{ p_gjahr }000_{ p_rldnr }|.
            mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

            CALL FUNCTION 'YCLEAN_FM02' STARTING NEW TASK mv_taskname DESTINATION 'NONE'
              EXPORTING
                iv_kind      = 'RB2'
                iv_rldnr     = p_rldnr
                iv_bukrs     = p_bukrs
                iv_gjahr     = p_gjahr
                iv_spmon     = CONV jahrper( |{ p_gjahr }000| )
                iv_belnr     = _where
                iv_logid     = mv_logid
              EXCEPTIONS
                kind_invalid = 1
                OTHERS       = 2.
            _output->begin_section( |Günlük Bilgileri({ TEXT-t02 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| )->write_text( |{ mv_logid ALPHA = OUT }| )->display( ).

        ENDCASE.
        _output->display( ).

*--------------------------------------------------------------------*
*-&Step-3: Delete From ACDOCD ->
*--------------------------------------------------------------------*
      WHEN p_rb3.
        CLEAR: mv_logid, mv_taskname.
        mv_taskname = |ACDOCD_{ p_gjahr }_{ p_rldnr }|.
        mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

        CALL FUNCTION 'YCLEAN_FM03' STARTING NEW TASK mv_taskname DESTINATION 'NONE'
          EXPORTING
            iv_rldnr = p_rldnr
            iv_bukrs = p_bukrs
            iv_gjahr = p_gjahr
            iv_logid = mv_logid.

        cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t03 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| )->write_text( |{ mv_logid ALPHA = OUT }| )->display( ).

*--------------------------------------------------------------------*
*-&Step-4: Delete From FAAT_DOC_IT ->
*--------------------------------------------------------------------*
      WHEN p_rb4.
        CLEAR: _where.
        _where = cl_shdb_seltab=>combine_seltabs( it_named_seltabs = VALUE #( ( name = 'AFABE' dref = REF #( s_afabe[] ) ) ) ).

        CLEAR: mv_logid, mv_taskname.
        mv_taskname = |FAAT_DOC_IT|.
        mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

        CALL FUNCTION 'YCLEAN_FM04' STARTING NEW TASK mv_taskname DESTINATION 'NONE'
          EXPORTING
            iv_bukrs = p_bukrs
            iv_afabe = _where
            iv_logid = mv_logid.

        cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t04 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| )->write_text( |{ mv_logid ALPHA = OUT }| )->display( ).

*--------------------------------------------------------------------*
*-&Step-5: Delete From FAAT_YDDA ->
*--------------------------------------------------------------------*
      WHEN p_rb5.
        CLEAR: _where.
        _where = cl_shdb_seltab=>combine_seltabs( it_named_seltabs = VALUE #( ( name = 'AFABE' dref = REF #( s_afabe[] ) ) ) ).

        CLEAR: mv_logid, mv_taskname.
        mv_taskname = |FAAT_YDDA|.
        mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

        CALL FUNCTION 'YCLEAN_FM05' STARTING NEW TASK mv_taskname DESTINATION 'NONE'
          EXPORTING
            iv_bukrs = p_bukrs
            iv_afabe = _where
            iv_logid = mv_logid.

        cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t05 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| )->write_text( |{ mv_logid ALPHA = OUT }| )->display( ).

*--------------------------------------------------------------------*
*-&Step-6: Delete From FAAT_PLAN_VALUES ->
*--------------------------------------------------------------------*
      WHEN p_rb6.
        CLEAR: _where.
        _where = cl_shdb_seltab=>combine_seltabs( it_named_seltabs = VALUE #( ( name = 'AFABE' dref = REF #( s_afabe[] ) ) ) ).

        CLEAR: mv_logid, mv_taskname.
        mv_taskname = |FAAT_PLAN_VALUES|.
        mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

        CALL FUNCTION 'YCLEAN_FM06' STARTING NEW TASK mv_taskname DESTINATION 'NONE'
          EXPORTING
            iv_bukrs = p_bukrs
            iv_afabe = _where
            iv_logid = mv_logid.

        cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t06 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| )->write_text( |{ mv_logid ALPHA = OUT }| )->display( ).
*--------------------------------------------------------------------*
*-&Step-7: Delete From ANLB ->
*--------------------------------------------------------------------*
      WHEN p_rb7.
        CLEAR: _where.
        _where = cl_shdb_seltab=>combine_seltabs( it_named_seltabs = VALUE #( ( name = 'AFABE' dref = REF #( s_afabe[] ) ) ) ).

        CLEAR: mv_logid, mv_taskname.
        mv_taskname = |ANLB|.
        mv_logid = |{ mv_taskname }-{ generate_guid( ) }|.

        CALL FUNCTION 'YCLEAN_FM07' STARTING NEW TASK mv_taskname DESTINATION 'NONE'
          EXPORTING
            iv_bukrs = p_bukrs
            iv_afabe = _where
            iv_logid = mv_logid.

        cl_demo_output=>new( )->begin_section( |Günlük Bilgileri({ TEXT-t07 })| )->begin_section( |Nesne:| )->write_text( |YCLEAN| )->next_section( |Harici tanıtıcı:| )->write_text( |{ mv_logid ALPHA = OUT }| )->display( ).

    ENDCASE.

  ENDMETHOD .                    "retrieve_dat
  METHOD generate_guid.
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = rv_guid.
  ENDMETHOD .                    "generate_guid
  METHOD popup_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = im_titlebar
        text_question         = im_question
        text_button_1         = TEXT-d01
        text_button_2         = TEXT-d02
        popup_type            = 'ICON_MESSAGE_CRITICAL'
        default_button        = '2'
        display_cancel_button = abap_true
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

  ENDMETHOD.                    "popup_confirm
  METHOD show_message_tool.

    CHECK iv_msgdat[] IS NOT INITIAL.

    DATA: lt_message_tab TYPE esp1_message_tab_type,
          lr_message_tab TYPE REF TO esp1_message_wa_type,
          lr_messtab     TYPE REF TO bapiret2.


    LOOP AT iv_msgdat REFERENCE INTO lr_messtab.
      APPEND INITIAL LINE TO lt_message_tab
         REFERENCE INTO lr_message_tab.
      lr_message_tab->msgty = lr_messtab->type       .
      lr_message_tab->msgid = lr_messtab->id         .
      lr_message_tab->msgno = lr_messtab->number     .
      lr_message_tab->msgv1 = lr_messtab->message_v1 .
      lr_message_tab->msgv2 = lr_messtab->message_v2 .
      lr_message_tab->msgv3 = lr_messtab->message_v3 .
      lr_message_tab->msgv4 = lr_messtab->message_v4 .
    ENDLOOP.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_message_tab[].

  ENDMETHOD.                    "show_message_tool
ENDCLASS .                    "application IMPLEMENTATION

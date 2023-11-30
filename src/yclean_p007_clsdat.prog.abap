*&---------------------------------------------------------------------*
*& Include          YCLEAN_P007_CLSDAT
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN COMMENT /1(40) blok_1.
  PARAMETERS:
    p_objct TYPE balhdr-object OBLIGATORY DEFAULT 'YCLEAN',
    p_subob TYPE balhdr-subobject OBLIGATORY DEFAULT '*',
    p_exnum TYPE balhdr-extnumber OBLIGATORY DEFAULT '*'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN COMMENT /1(40) blok_2.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(33) TEXT-s01 FOR FIELD p_bdate.
    PARAMETERS:
      p_bdate TYPE balhdr-alchdate DEFAULT sy-datum OBLIGATORY,
      p_bhour TYPE balhdr-alchtime DEFAULT '000000' OBLIGATORY.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(33) TEXT-s02 FOR FIELD p_edate.
    PARAMETERS:
      p_edate TYPE balhdr-aldate DEFAULT sy-datum OBLIGATORY,
      p_ehour TYPE balhdr-altime DEFAULT '235959' OBLIGATORY.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_detail AS CHECKBOX DEFAULT ''.

CLASS lcl_times DEFINITION DEFERRED.
DATA: app_session TYPE REF TO lcl_times.

*----------------------------------------------------------------------*
*       CLASS lcl_times DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_times DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA:
      app TYPE REF TO lcl_times.

    TYPES:
      BEGIN OF mty_logdat,
        lognumber TYPE balognr,
        extnumber	TYPE balnrext,
        b_date    TYPE datum,
        b_hour    TYPE uzeit,
        e_date    TYPE datum,
        e_hour    TYPE uzeit,
      END OF mty_logdat,
      mtt_logdat TYPE SORTED TABLE OF mty_logdat WITH UNIQUE KEY primary_key COMPONENTS lognumber.

    DATA:
      mt_logdat TYPE mtt_logdat.

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
          VALUE(mo_app) TYPE REF TO lcl_times.

    METHODS:
      display_output_log
        EXCEPTIONS
          handle_error.

  PRIVATE SECTION.

    METHODS:
      read_application_log
        IMPORTING
          im_objct  TYPE balhdr-object
          im_subob  TYPE balhdr-subobject
          im_exnum  TYPE balhdr-extnumber
          im_bdate  TYPE balhdr-alchdate
          im_bhour  TYPE balhdr-alchtime
          im_edate  TYPE balhdr-aldate
          im_ehour  TYPE balhdr-altime
        EXPORTING
          et_logdat TYPE mtt_logdat
        EXCEPTIONS
          handle_error,
      two_times_subtract
        IMPORTING
          im_strdate       TYPE datum
          im_strtime       TYPE uzeit
          im_enddate       TYPE datum
          im_endtime       TYPE uzeit
        RETURNING
          VALUE(rv_minute) TYPE int8.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_times IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_times IMPLEMENTATION.

  METHOD initialization.
    blok_1 = TEXT-b01.
    blok_2 = TEXT-b02.
  ENDMETHOD.                    "initialization
  METHOD at_selection_screen.

    CASE sy-ucomm.
      WHEN 'ONLI'.
        IF p_bdate GT p_edate.
          MESSAGE e260(bl) DISPLAY LIKE 'S'.
        ENDIF.
        IF p_bhour GT p_ehour.
          MESSAGE e261(bl) DISPLAY LIKE 'S'.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "at_selection_screen
  METHOD at_selection_screen_output.

  ENDMETHOD.                    "at_selection_screen_output
  METHOD at_selection_screen_request.

  ENDMETHOD.                    "at_selection_screen_request
  METHOD start_of_selection.

    app_session->read_application_log(
      EXPORTING
        im_objct     = p_objct
        im_subob     = p_subob
        im_exnum     = p_exnum
        im_bdate     = p_bdate
        im_bhour     = p_bhour
        im_edate     = p_edate
        im_ehour     = p_ehour
      IMPORTING
        et_logdat    = app_session->mt_logdat
      EXCEPTIONS
        handle_error = 1
        OTHERS       = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "start_of_selection
  METHOD end_of_selection.

    app_session->display_output_log(
      EXCEPTIONS
        handle_error = 1
        OTHERS       = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "end_of_selection
  METHOD app_instance.

    FREE: mo_app, app.
    IF app IS NOT BOUND.
      CREATE OBJECT app.
    ENDIF.
    mo_app = app.

  ENDMETHOD.                    "app_instance
  METHOD display_output_log.

    TYPES:
      BEGIN OF ty_alvdat,
        lognumber TYPE balognr,
        extnumber	TYPE balnrext,
        b_date    TYPE datum,
        b_hour    TYPE uzeit,
        e_date    TYPE datum,
        e_hour    TYPE uzeit,
        minute    TYPE int8,
        maximu    TYPE int8,
        averag    TYPE dec23_2,
      END OF ty_alvdat,
      tt_alvdat TYPE STANDARD TABLE OF ty_alvdat.

    TYPES:
      BEGIN OF ty_maximu,
        maximu TYPE int8,
      END OF ty_maximu,
      tt_maximu TYPE STANDARD TABLE OF ty_maximu.

    TYPES:
      BEGIN OF ty_extdata,
        keyvalue TYPE char50,
      END OF ty_extdata,
      tt_extdata TYPE SORTED TABLE OF ty_extdata WITH UNIQUE KEY primary_key COMPONENTS keyvalue.

    DATA: gr_table     TYPE REF TO cl_salv_table,
          gr_functions TYPE REF TO cl_salv_functions,
          gr_display   TYPE REF TO cl_salv_display_settings,
          gr_settings  TYPE REF TO cl_salv_display_settings,
          gr_columns   TYPE REF TO cl_salv_columns_table,
          t_alvdat     TYPE tt_alvdat,
          t_maximu     TYPE tt_maximu,
          t_fcatdat    TYPE lvc_t_fcat,
          t_extdata    TYPE tt_extdata,
          lv_dtext     TYPE string,
          lv_right     TYPE string,
          lv_left      TYPE string.

    TRY.
        CHECK NOT mt_logdat IS INITIAL.
        CASE p_detail.
          WHEN abap_true.
            t_alvdat = VALUE #( FOR <wa> IN mt_logdat (
                                  VALUE #( BASE CORRESPONDING #( <wa> )
                                    minute = two_times_subtract( EXPORTING im_strdate = <wa>-b_date
                                                                           im_strtime = <wa>-b_hour
                                                                           im_enddate = <wa>-e_date
                                                                           im_endtime = <wa>-e_hour ) ) ) ).
          WHEN abap_false.
            FREE: t_extdata.
            LOOP AT mt_logdat ASSIGNING FIELD-SYMBOL(<logdat>).
              SPLIT <logdat>-extnumber AT '/' INTO lv_right lv_left.
              IF sy-subrc IS INITIAL.
                INSERT VALUE #( keyvalue = lv_right ) INTO TABLE t_extdata.
              ENDIF.
            ENDLOOP.

            LOOP AT t_extdata ASSIGNING FIELD-SYMBOL(<extdata>).
              FREE: t_maximu.
              LOOP AT mt_logdat INTO DATA(_logdat)
                                  WHERE ( extnumber CP |{ <extdata>-keyvalue }*| )
                                    GROUP BY ( extnumber = _logdat-extnumber  )
                                    ASSIGNING FIELD-SYMBOL(<router_tab>).

                t_maximu = VALUE #( BASE t_maximu FOR <members> IN GROUP <router_tab> ( maximu = two_times_subtract( EXPORTING im_strdate = <members>-b_date
                                                                                                                               im_strtime = <members>-b_hour
                                                                                                                               im_enddate = <members>-e_date
                                                                                                                               im_endtime = <members>-e_hour ) ) ).
              ENDLOOP.
              DELETE t_maximu WHERE maximu IS INITIAL.
              SORT t_maximu BY maximu DESCENDING.
              APPEND VALUE #( extnumber = <extdata>-keyvalue
                              maximu = VALUE #( t_maximu[ 1 ]-maximu OPTIONAL )
                              averag = REDUCE #( INIT i TYPE int8 FOR <mx> IN t_maximu NEXT i = i + <mx>-maximu ) /
                                       ( COND #( WHEN t_maximu[] IS INITIAL THEN 0 ELSE lines( t_maximu ) ) ) ) TO t_alvdat.
            ENDLOOP.
        ENDCASE.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table  = gr_table
          CHANGING
            t_table       = t_alvdat ).

        gr_settings = gr_table->get_display_settings( ).
        gr_settings->set_striped_pattern( cl_salv_display_settings=>true ).

        gr_columns = gr_table->get_columns( ).
        gr_columns->set_optimize( abap_true ).

        gr_functions = gr_table->get_functions( ).
        gr_functions->set_all( abap_true ).
        gr_table->set_screen_status(
          EXPORTING
            pfstatus      = 'SALV_STANDARD'
            report        = sy-repid
            set_functions = gr_table->c_functions_all ).

        CLEAR: t_fcatdat.
        t_fcatdat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
          EXPORTING
            r_columns      = gr_table->get_columns( )
            r_aggregations = gr_table->get_aggregations( ) ).

        LOOP AT t_fcatdat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
          CLEAR: lv_dtext.
          CASE <fs_fcat>-fieldname.
            WHEN 'B_DATE'.
              lv_dtext = 'Bşl.Tarihi'.
              <fs_fcat>-tech = COND #( WHEN p_detail EQ abap_false THEN abap_true ).
            WHEN 'B_HOUR'.
              lv_dtext = 'Bşl.Saati'.
              <fs_fcat>-tech = COND #( WHEN p_detail EQ abap_false THEN abap_true ).
            WHEN 'E_DATE'.
              lv_dtext = 'Bitiş Tarihi'.
              <fs_fcat>-tech = COND #( WHEN p_detail EQ abap_false THEN abap_true ).
            WHEN 'E_HOUR'.
              lv_dtext = 'Bitiş Saati'.
              <fs_fcat>-tech = COND #( WHEN p_detail EQ abap_false THEN abap_true ).
            WHEN 'MINUTE'.
              lv_dtext = 'Süre(DK)'.
              <fs_fcat>-tech = COND #( WHEN p_detail EQ abap_false THEN abap_true ).
            WHEN 'MAXIMU'.
              lv_dtext = 'Maks.(DK)'.
              <fs_fcat>-tech = COND #( WHEN p_detail EQ abap_true THEN abap_true ELSE abap_false ).
            WHEN 'LOGNUMBER'.
              <fs_fcat>-no_out = abap_true.
              <fs_fcat>-tech = COND #( WHEN p_detail EQ abap_false THEN abap_true ).
            WHEN OTHERS.
          ENDCASE.
          IF NOT lv_dtext IS INITIAL.
            <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = lv_dtext.
          ENDIF.
        ENDLOOP.
        cl_salv_controller_metadata=>set_lvc_fieldcatalog(
          EXPORTING
            t_fieldcatalog  = t_fcatdat
            r_columns       = gr_table->get_columns( )
            r_aggregations  = gr_table->get_aggregations( ) ).
      CATCH cx_root.
        MESSAGE 'Görüntüleme sırasında teknik hatalar oluştu!' TYPE 'S' RAISING handle_error.
    ENDTRY.
    gr_table->display( ).

  ENDMETHOD.                    "display_output_log
  METHOD read_application_log.

    DATA: t_header  TYPE STANDARD TABLE OF balhdr,
          t_message TYPE STANDARD TABLE OF balm.

    DATA: b_date TYPE datum,
          e_date TYPE datum,
          b_hour TYPE uzeit,
          e_hour TYPE uzeit.

    FREE: t_header, t_message.
    CALL FUNCTION 'APPL_LOG_READ_DB'
      EXPORTING
        object      = im_objct
        subobject   = im_subob
        date_from   = im_bdate
        date_to     = im_edate
        time_from   = im_bhour
        time_to     = im_ehour
      TABLES
        header_data = t_header
        messages    = t_message.

    LOOP AT t_header ASSIGNING FIELD-SYMBOL(<header>).
      CLEAR: b_date, e_date, b_hour, e_hour.
      LOOP AT t_message ASSIGNING FIELD-SYMBOL(<message>)
                          WHERE lognumber = <header>-lognumber.
        AT NEW lognumber.
          CONVERT TIME STAMP <message>-time_stmp TIME ZONE sy-zonlo INTO DATE b_date TIME b_hour.
        ENDAT.

        AT END OF lognumber.
          CONVERT TIME STAMP <message>-time_stmp TIME ZONE sy-zonlo INTO DATE e_date TIME e_hour.
        ENDAT.
      ENDLOOP.
      IF sy-subrc IS INITIAL.
        INSERT VALUE #(
          lognumber = <header>-lognumber
          extnumber = <header>-extnumber
          b_date    = b_date
          b_hour    = b_hour
          e_date    = e_date
          e_hour    = e_hour ) INTO TABLE et_logdat.
      ENDIF.
    ENDLOOP.
    IF et_logdat IS INITIAL.
      MESSAGE s208(bl) RAISING handle_error.
    ENDIF.

  ENDMETHOD.                    "read_application_log
  METHOD two_times_subtract.

    cl_abap_tstmp=>systemtstmp_syst2utc(
       EXPORTING
         syst_date = im_strdate
         syst_time = im_strtime
       IMPORTING
         utc_tstmp = DATA(start_stamp) ).

    cl_abap_tstmp=>systemtstmp_syst2utc(
      EXPORTING
        syst_date = im_enddate
        syst_time = im_endtime
      IMPORTING
        utc_tstmp = DATA(end_stamp) ).

    rv_minute = cl_abap_tstmp=>subtract( tstmp1 = end_stamp tstmp2 = start_stamp ) / 60.

  ENDMETHOD.                    "two_times_subtract

ENDCLASS.

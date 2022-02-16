CLASS yclean_cl01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb.

    TYPES: BEGIN OF ty_budatdat,
             budat TYPE budat,
           END OF ty_budatdat,
           tt_budatdat TYPE STANDARD TABLE OF ty_budatdat WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_splitdat,
             line  TYPE sy-tabix,
             budat TYPE tt_budatdat,
           END OF ty_splitdat,
           tt_splitdat TYPE STANDARD TABLE OF ty_splitdat WITH DEFAULT KEY.

    CLASS-METHODS:
      _rb1a_rundat
        IMPORTING
          VALUE(iv_bukrs)  TYPE bukrs
          VALUE(iv_gjahr)  TYPE gjahr
          VALUE(iv_bstat)  TYPE bstat_d
          VALUE(iv_belnr)  TYPE string
        EXPORTING
          VALUE(ev_subrc)  TYPE syst_subrc
          VALUE(ev_rowcnt) TYPE syst_tabix
          VALUE(ev_result) TYPE string
        RAISING
          cx_amdp_error,
      _rb1b_rundat
        IMPORTING
          VALUE(iv_bukrs)  TYPE bukrs
          VALUE(iv_gjahr)  TYPE gjahr
          VALUE(iv_bstat)  TYPE bstat_d
          VALUE(iv_ldgrp)  TYPE string
        EXPORTING
          VALUE(ev_subrc)  TYPE syst_subrc
          VALUE(ev_rowcnt) TYPE syst_tabix
          VALUE(ev_result) TYPE string
        RAISING
          cx_amdp_error,
      _rb2a_rundat
        IMPORTING
          VALUE(iv_rldnr)  TYPE fins_ledger
          VALUE(iv_bukrs)  TYPE bukrs
          VALUE(iv_gjahr)  TYPE gjahr
          VALUE(iv_budat)  TYPE tt_budatdat
          VALUE(iv_belnr)  TYPE string
        EXPORTING
          VALUE(ev_subrc)  TYPE syst_subrc
          VALUE(ev_rowcnt) TYPE syst_tabix
          VALUE(ev_result) TYPE string
        RAISING
          cx_amdp_error,
      _rb2b_rundat
        IMPORTING
          VALUE(iv_rldnr)  TYPE fins_ledger
          VALUE(iv_bukrs)  TYPE bukrs
          VALUE(iv_gjahr)  TYPE gjahr
          VALUE(iv_spmon)  TYPE jahrper
          VALUE(iv_belnr)  TYPE string
        EXPORTING
          VALUE(ev_subrc)  TYPE syst_subrc
          VALUE(ev_rowcnt) TYPE syst_tabix
          VALUE(ev_result) TYPE string
        RAISING
          cx_amdp_error,
      _rb3_rundat
        IMPORTING
          VALUE(iv_rldnr)  TYPE fins_ledger
          VALUE(iv_bukrs)  TYPE bukrs
          VALUE(iv_gjahr)  TYPE gjahr
        EXPORTING
          VALUE(ev_subrc)  TYPE syst_subrc
          VALUE(ev_rowcnt) TYPE syst_tabix
          VALUE(ev_result) TYPE string
        RAISING
          cx_amdp_error,
      _rb4_rundat
        IMPORTING
          VALUE(iv_bukrs)  TYPE bukrs
          VALUE(iv_afabe)  TYPE string
        EXPORTING
          VALUE(ev_subrc)  TYPE syst_subrc
          VALUE(ev_rowcnt) TYPE syst_tabix
          VALUE(ev_result) TYPE string
        RAISING
          cx_amdp_error,
      _rb5_rundat
        IMPORTING
          VALUE(iv_bukrs)  TYPE bukrs
          VALUE(iv_afabe)  TYPE string
        EXPORTING
          VALUE(ev_subrc)  TYPE syst_subrc
          VALUE(ev_rowcnt) TYPE syst_tabix
          VALUE(ev_result) TYPE string
        RAISING
          cx_amdp_error,
      _rb6_rundat
        IMPORTING
          VALUE(iv_bukrs)  TYPE bukrs
          VALUE(iv_afabe)  TYPE string
        EXPORTING
          VALUE(ev_subrc)  TYPE syst_subrc
          VALUE(ev_rowcnt) TYPE syst_tabix
          VALUE(ev_result) TYPE string
        RAISING
          cx_amdp_error,
      _rb7_rundat
        IMPORTING
          VALUE(iv_bukrs)  TYPE bukrs
          VALUE(iv_afabe)  TYPE string
        EXPORTING
          VALUE(ev_subrc)  TYPE syst_subrc
          VALUE(ev_rowcnt) TYPE syst_tabix
          VALUE(ev_result) TYPE string
        RAISING
          cx_amdp_error,
      _period_splitdat
        IMPORTING
          im_spmon           TYPE spmon
          im_split           TYPE int1
        RETURNING
          VALUE(rt_splitdat) TYPE tt_splitdat.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS yclean_cl01 IMPLEMENTATION.
  METHOD _rb1a_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING acdoca bkpf bseg_add.

    DECLARE numberofentities NUMBER(10);

    t_acdoca = SELECT DISTINCT rbukrs, gjahr, belnr
                 FROM acdoca
                   WHERE rclnt  = Session_context('CLIENT')
                     AND rbukrs = :iv_bukrs
                     and gjahr  = :iv_gjahr
                     AND bstat  = :iv_bstat;
*-> Apply filter->
    t_result = APPLY_FILTER ( :t_acdoca, :iv_belnr);

    SELECT COUNT (*) INTO numberofentities FROM :t_result;
    IF numberofentities <> 0
    THEN
        ev_rowcnt = :numberofentities;
        DELETE FROM bkpf as db WHERE( db.mandt, db.bukrs, db.belnr, db.gjahr )
                                  IN ( SELECT Session_context( 'CLIENT' ), rbukrs, belnr, gjahr FROM :t_result );

        DELETE FROM bseg_add as db WHERE( db.mandt, db.bukrs, db.belnr, db.gjahr )
                                      IN ( SELECT Session_context( 'CLIENT' ), rbukrs, belnr, gjahr FROM :t_result );

        ev_subrc = 0;
        ev_result = 'Entity has been deleted successfully.';
    ELSE
    ev_subrc = 4;
    ev_result = 'Your entried value is not valid.';
    END IF;

  ENDMETHOD.
  METHOD _rb1b_rundat BY DATABASE PROCEDURE
                      FOR HDB
                      LANGUAGE SQLSCRIPT
                      USING bkpf.

    DECLARE numberofentities NUMBER(10);

    t_bkpf = SELECT DISTINCT bukrs, belnr, gjahr, ldgrp
                 FROM bkpf
                   WHERE mandt = Session_context('CLIENT')
                     AND bukrs = :iv_bukrs
                     and gjahr = :iv_gjahr
                     AND bstat = :iv_bstat;
*-> Apply filter->
    t_result = APPLY_FILTER ( :t_bkpf, :iv_ldgrp);

    SELECT COUNT (*) INTO numberofentities FROM :t_result;
    IF numberofentities <> 0
    THEN
        ev_rowcnt = :numberofentities;
        DELETE FROM bkpf as db WHERE( db.mandt, db.bukrs, db.belnr, db.gjahr )
                                  IN ( SELECT Session_context( 'CLIENT' ), bukrs, belnr, gjahr FROM :t_result );

        SELECT COUNT (*) INTO numberofentities FROM bkpf WHERE( mandt, bukrs, belnr, gjahr )
                                                            IN ( SELECT Session_context( 'CLIENT' ), bukrs, belnr, gjahr FROM :t_result );
        IF numberofentities = 0
        THEN
            ev_subrc = 0;
            ev_result = 'Entity has been deleted successfully.';
        ELSE
            ev_subrc = 4;
            ev_result = 'Entity has not been deleted.';
        END IF;
    ELSE
    ev_subrc = 4;
    ev_result = 'Your entried value is not valid.';
    END IF;

  ENDMETHOD.
  METHOD _rb2a_rundat BY DATABASE PROCEDURE
                      FOR HDB
                      LANGUAGE SQLSCRIPT
                      USING acdoca.

    DECLARE numberofentities NUMBER(10);

    t_acdoca = SELECT DISTINCT rldnr, rbukrs, gjahr, belnr
                 FROM acdoca
                   WHERE rclnt  = Session_context('CLIENT')
                     AND rldnr  = :iv_rldnr
                     AND rbukrs = :iv_bukrs
                     AND gjahr  = :iv_gjahr
                     AND budat  IN( SELECT DISTINCT budat FROM :iv_budat );
*-> Apply filter->
    t_result = APPLY_FILTER ( :t_acdoca, :iv_belnr);

    SELECT COUNT (*) INTO numberofentities FROM :t_result;
    IF numberofentities <> 0
    THEN
        ev_rowcnt = :numberofentities;
        DELETE FROM acdoca as db WHERE( db.rclnt, db.rldnr, db.rbukrs, db.gjahr, db.belnr )
                                    IN ( SELECT Session_context( 'CLIENT' ), rldnr, rbukrs, gjahr, belnr FROM :t_result );

        SELECT COUNT (*) INTO numberofentities FROM acdoca WHERE( rclnt, rldnr, rbukrs, gjahr, belnr )
                                                              IN ( SELECT Session_context( 'CLIENT' ), rldnr, rbukrs, gjahr, belnr FROM :t_result );
        IF numberofentities = 0
        THEN
            ev_subrc = 0;
            ev_result = 'Entity has been deleted successfully.';
        ELSE
            ev_subrc = 4;
            ev_result = 'Entity has not been deleted.';
        END IF;
    ELSE
    ev_subrc = 4;
    ev_result = 'Your entried value is not valid.';
    END IF;


  ENDMETHOD.
  METHOD _rb2b_rundat BY DATABASE PROCEDURE
                      FOR HDB
                      LANGUAGE SQLSCRIPT
                      USING acdoca.

    DECLARE numberofentities NUMBER(10);

    t_acdoca = SELECT DISTINCT rldnr, rbukrs, gjahr, belnr
                 FROM acdoca
                   WHERE rclnt  = Session_context('CLIENT')
                     AND rldnr  = :iv_rldnr
                     AND rbukrs = :iv_bukrs
                     AND gjahr  = :iv_gjahr
                     AND fiscyearper = :iv_spmon;
*-> Apply filter->
    t_result = APPLY_FILTER ( :t_acdoca, :iv_belnr);

    SELECT COUNT (*) INTO numberofentities FROM :t_result;
    IF numberofentities <> 0
    THEN
        ev_rowcnt = :numberofentities;
        DELETE FROM acdoca as db WHERE( db.rclnt, db.rldnr, db.rbukrs, db.gjahr, db.belnr )
                                    IN ( SELECT Session_context( 'CLIENT' ), rldnr, rbukrs, gjahr, belnr FROM :t_result );

        SELECT COUNT (*) INTO numberofentities FROM acdoca WHERE( rclnt, rldnr, rbukrs, gjahr, belnr )
                                                              IN ( SELECT Session_context( 'CLIENT' ), rldnr, rbukrs, gjahr, belnr FROM :t_result );
        IF numberofentities = 0
        THEN
            ev_subrc = 0;
            ev_result = 'Entity has been deleted successfully.';
        ELSE
            ev_subrc = 4;
            ev_result = 'Entity has not been deleted.';
        END IF;
    ELSE
    ev_subrc = 4;
    ev_result = 'Your entried value is not valid.';
    END IF;


  ENDMETHOD.
  METHOD _rb3_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING acdocd.

    DECLARE numberofentities NUMBER(10);

    t_result = SELECT DISTINCT rldnr, rbukrs, gjahr, belnr
                 FROM acdocd
                   WHERE rclnt  = Session_context('CLIENT')
                     AND rldnr  = :iv_rldnr
                     AND rbukrs = :iv_bukrs
                     AND gjahr  = :iv_gjahr;

    SELECT COUNT (*) INTO numberofentities FROM :t_result;
    IF numberofentities <> 0
    THEN
        ev_rowcnt = :numberofentities;
        DELETE FROM acdocd as db WHERE( db.rclnt, db.rldnr, db.rbukrs, db.gjahr, db.belnr )
                                    IN ( SELECT Session_context( 'CLIENT' ), rldnr, rbukrs, gjahr, belnr FROM :t_result );

        SELECT COUNT (*) INTO numberofentities FROM acdocd WHERE( rclnt, rldnr, rbukrs, gjahr, belnr )
                                                              IN ( SELECT Session_context( 'CLIENT' ), rldnr, rbukrs, gjahr, belnr FROM :t_result );
        IF numberofentities = 0
        THEN
            ev_subrc = 0;
            ev_result = 'Entity has been deleted successfully.';
        ELSE
            ev_subrc = 4;
            ev_result = 'Entity has not been deleted.';
        END IF;
    ELSE
    ev_subrc = 4;
    ev_result = 'Your entried value is not valid.';
    END IF;


  ENDMETHOD.
  METHOD _rb4_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING faat_doc_it.

    DECLARE numberofentities NUMBER(10);

    t_faat_doc_it = SELECT DISTINCT * FROM faat_doc_it
                        WHERE mandt = Session_context('CLIENT')
                          AND bukrs = :iv_bukrs;
*-> Apply filter->
    t_result = APPLY_FILTER ( :t_faat_doc_it, :iv_afabe);

    SELECT COUNT (*) INTO numberofentities FROM :t_result;
    IF numberofentities <> 0
    THEN
        ev_rowcnt = :numberofentities;
        DELETE FROM faat_doc_it as db WHERE( db.mandt, db.bukrs, db.anln1, db.anln2, db.gjahr, db.awtyp, db.awref, db.aworg, db.awsys, db.subta, db.afabe, db.slalittype, db.drcrk )
                                         IN ( SELECT Session_context( 'CLIENT' ), bukrs, anln1, anln2, gjahr, awtyp, awref, aworg, awsys, subta, afabe, slalittype, drcrk FROM :t_result );

        SELECT COUNT (*) INTO numberofentities FROM faat_doc_it WHERE( mandt, bukrs, anln1, anln2, gjahr, awtyp, awref, aworg, awsys, subta, afabe, slalittype, drcrk )
                                                                   IN ( SELECT Session_context( 'CLIENT' ), bukrs, anln1, anln2, gjahr, awtyp, awref, aworg, awsys, subta, afabe, slalittype, drcrk FROM :t_result );
        IF numberofentities = 0
        THEN
            ev_subrc = 0;
            ev_result = 'Entity has been deleted successfully.';
        ELSE
            ev_subrc = 4;
            ev_result = 'Entity has not been deleted.';
        END IF;
    ELSE
        ev_subrc = 4;
        ev_result = 'Entity has not been deleted.';
    END IF;


  ENDMETHOD.
  METHOD _rb5_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING faat_ydda.

    DECLARE numberofentities NUMBER(10);

    t_faat_ydda = SELECT DISTINCT * FROM faat_ydda
                    WHERE mandt = Session_context('CLIENT')
                      AND bukrs = :iv_bukrs;
*-> Apply filter->
     t_result = APPLY_FILTER ( :t_faat_ydda, :iv_afabe);

    SELECT COUNT (*) INTO numberofentities FROM :t_result;
    IF numberofentities <> 0
    THEN
        ev_rowcnt = :numberofentities;
        DELETE FROM faat_ydda as db WHERE( db.mandt, db.bukrs, db.anln1, db.anln2, db.gjahr, db.afabe )
                                       IN ( SELECT Session_context( 'CLIENT' ), bukrs, anln1, anln2, gjahr, afabe FROM :t_result );

        SELECT COUNT (*) INTO numberofentities FROM faat_ydda WHERE( mandt, bukrs, anln1, anln2, gjahr, afabe )
                                                                 IN ( SELECT Session_context( 'CLIENT' ), bukrs, anln1, anln2, gjahr, afabe FROM :t_result );
        IF numberofentities = 0
        THEN
            ev_subrc = 0;
            ev_result = 'Entity has been deleted successfully.';
        ELSE
            ev_subrc = 4;
            ev_result = 'Entity has not been deleted.';
        END IF;
    ELSE
        ev_subrc = 4;
        ev_result = 'Entity has not been deleted.';
    END IF;


  ENDMETHOD.
  METHOD _rb6_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING faat_plan_values.

    DECLARE numberofentities NUMBER(10);

    t_faat_plan_values = SELECT DISTINCT * FROM faat_plan_values
                           WHERE mandt = Session_context('CLIENT')
                             AND bukrs = :iv_bukrs;
*-> Apply filter->
    t_result = APPLY_FILTER ( :t_faat_plan_values, :iv_afabe);

    SELECT COUNT (*) INTO numberofentities FROM :t_result;
    IF numberofentities <> 0
    THEN
        ev_rowcnt = :numberofentities;
        DELETE FROM faat_plan_values as db WHERE( db.mandt, db.bukrs, db.anln1, db.anln2, db.gjahr, db.afabe, db.poper, db.slalittype )
                                              IN ( SELECT Session_context( 'CLIENT' ), bukrs, anln1, anln2, gjahr, afabe, poper, slalittype FROM :t_result );

        SELECT COUNT (*)  INTO numberofentities FROM faat_plan_values WHERE( mandt, bukrs, anln1, anln2, gjahr, afabe, poper, slalittype )
                                                                         IN ( SELECT Session_context( 'CLIENT' ), bukrs, anln1, anln2, gjahr, afabe, poper, slalittype FROM :t_result );
        IF numberofentities = 0
        THEN
            ev_subrc = 0;
            ev_result = 'Entity has been deleted successfully.';
        ELSE
            ev_subrc = 4;
            ev_result = 'Entity has not been deleted.';
        END IF;
    ELSE
        ev_subrc = 4;
        ev_result = 'Entity has not been deleted.';
    END IF;


  ENDMETHOD.
  METHOD _rb7_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING anlb.


    DECLARE numberofentities NUMBER(10);

    t_anlb = SELECT DISTINCT * FROM anlb
                WHERE mandt = Session_context('CLIENT')
                  AND bukrs = :iv_bukrs;

*-> Apply filter->
    t_result = APPLY_FILTER ( :t_anlb, :iv_afabe);

    SELECT COUNT (*) INTO numberofentities FROM :t_result;
    IF numberofentities <> 0
    THEN
        ev_rowcnt = :numberofentities;
        DELETE FROM anlb as db WHERE( db.mandt, db.bukrs, db.anln1, db.anln2, db.afabe, db.bdatu )
                                  IN ( SELECT Session_context( 'CLIENT' ), bukrs, anln1, anln2, afabe, bdatu FROM :t_result );

        SELECT COUNT (*) INTO numberofentities FROM anlb WHERE( mandt, bukrs, anln1, anln2, afabe, bdatu )
                                                            IN ( SELECT Session_context( 'CLIENT' ), bukrs, anln1, anln2, afabe, bdatu FROM :t_result );
        IF numberofentities = 0
        THEN
            ev_subrc = 0;
            ev_result = 'Entity has been deleted successfully.';
        ELSE
            ev_subrc = 4;
            ev_result = 'Entity has not been deleted.';
        END IF;
    ELSE
        ev_subrc = 4;
        ev_result = 'Entity has not been deleted.';
    END IF;


  ENDMETHOD.
  METHOD _period_splitdat.

    TYPES: BEGIN OF ty_keydat,
             date TYPE datum,
           END OF ty_keydat,
           tt_keydat TYPE STANDARD TABLE OF ty_keydat WITH EMPTY KEY.

    DATA: _splitdat TYPE REF TO ty_splitdat,
          _begdat   TYPE datum,
          _enddat   TYPE datum,
          _keydat   TYPE tt_keydat,
          _tabix    TYPE sy-tabix,
          _mod      TYPE n LENGTH 2.


    FREE: _begdat, _begdat, _keydat.
    _begdat = |{ im_spmon }01|.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = _begdat
      IMPORTING
        last_day_of_month = _enddat
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    APPEND VALUE #( date = _begdat ) TO _keydat.
    DO.
      ADD 1 TO _begdat.
      APPEND VALUE #( date = _begdat ) TO _keydat.
      IF _begdat EQ _enddat.
        EXIT.
      ENDIF.
    ENDDO.

    FREE: _mod.
    _mod = lines( _keydat ) / im_split.

    FREE: rt_splitdat, _tabix.
    DO im_split TIMES.
      ADD 1 TO _tabix.
      CREATE DATA _splitdat LIKE LINE OF rt_splitdat.
      _splitdat->line = _tabix.
      LOOP AT _keydat ASSIGNING FIELD-SYMBOL(<fs_keydat>) FROM 1 TO _mod.
        APPEND VALUE #( budat = <fs_keydat>-date ) TO _splitdat->budat.
      ENDLOOP.
      IF sy-subrc IS INITIAL.
        APPEND _splitdat->* TO rt_splitdat.
      ENDIF.
      DELETE _keydat FROM 1 TO _mod.
    ENDDO.
    IF NOT _keydat[] IS INITIAL.
      READ TABLE rt_splitdat REFERENCE INTO _splitdat INDEX im_split.
      IF sy-subrc IS INITIAL.
        LOOP AT _keydat ASSIGNING <fs_keydat>.
          APPEND VALUE #( budat = <fs_keydat>-date ) TO _splitdat->budat.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

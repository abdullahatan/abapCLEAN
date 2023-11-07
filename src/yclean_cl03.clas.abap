CLASS yclean_cl03 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb .

    TYPES:
      BEGIN OF ty_monatdat,
        poper TYPE poper,
      END OF ty_monatdat .
    TYPES:
      tt_monatdat TYPE STANDARD TABLE OF ty_monatdat WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_splitdat,
        line   TYPE sy-tabix,
        monats TYPE tt_monatdat,
      END OF ty_splitdat .
    TYPES:
      tt_splitdat TYPE STANDARD TABLE OF ty_splitdat WITH DEFAULT KEY .

    TYPES:
      BEGIN OF ty_jahrper,
        jahrper TYPE jahrper,
      END OF ty_jahrper.
    TYPES:
      tt_jahrpers TYPE STANDARD TABLE OF ty_jahrper WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_jahrperdat,
        line     TYPE sy-tabix,
        jahrpers TYPE tt_jahrpers,
      END OF ty_jahrperdat.
    TYPES:
      tt_jahrperdat TYPE STANDARD TABLE OF ty_jahrperdat WITH DEFAULT KEY .

    CLASS-METHODS _rb1_rundat
      IMPORTING
        VALUE(iv_bdatj)  TYPE bdatj
        VALUE(iv_poper)  TYPE yclean_tt04
        VALUE(iv_curtp)  TYPE fins_curtype
        VALUE(iv_kalnr)  TYPE yclean_tt06
      EXPORTING
        VALUE(ev_subrc)  TYPE syst_subrc
        VALUE(ev_result) TYPE string
      RAISING
        cx_amdp_error .
    CLASS-METHODS _rb2_rundat
      IMPORTING
        VALUE(iv_jahrper) TYPE yclean_tt05
        VALUE(iv_curtp)   TYPE fins_curtype
        VALUE(iv_docref)  TYPE yclean_tt07
      EXPORTING
        VALUE(ev_subrc)   TYPE syst_subrc
        VALUE(ev_result)  TYPE string
      RAISING
        cx_amdp_error .
    CLASS-METHODS _rb3_rundat
      IMPORTING
        VALUE(iv_jahrper) TYPE yclean_tt05
        VALUE(iv_curtp)   TYPE fins_curtype
        VALUE(iv_docref)  TYPE yclean_tt07
      EXPORTING
        VALUE(ev_subrc)   TYPE syst_subrc
        VALUE(ev_result)  TYPE string
      RAISING
        cx_amdp_error .
    CLASS-METHODS _monat_splitdat
      IMPORTING
        !im_split          TYPE int1
      RETURNING
        VALUE(rt_splitdat) TYPE tt_splitdat
      EXCEPTIONS
        contains_error .
    CLASS-METHODS _jahrper_splitdat
      IMPORTING
        !im_gjahr          TYPE gjahr
        !im_split          TYPE int1
      RETURNING
        VALUE(rt_splitdat) TYPE tt_jahrperdat
      EXCEPTIONS
        contains_error .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCLEAN_CL03 IMPLEMENTATION.


  METHOD _jahrper_splitdat.

    DATA: _mod       TYPE n LENGTH 2,
          _poper     TYPE poper,
          _tabix     TYPE sy-tabix,
          t_jahrpers TYPE tt_jahrpers.

    IF NOT im_split BETWEEN 1 AND 12.
      MESSAGE e018(yclean) RAISING contains_error.
    ENDIF.

    _mod = floor( 12 / im_split ).

    DO im_split TIMES.
      ADD 1 TO _tabix.
      IF _tabix EQ im_split.
        DO.
          ADD 1 TO _poper.
          APPEND VALUE #( jahrper = |{ im_gjahr }{  _poper }| ) TO t_jahrpers.
          IF _poper EQ 12.
            EXIT.
          ENDIF.
        ENDDO.
      ELSE.
        DO _mod TIMES.
          ADD 1 TO _poper.
          APPEND VALUE #( jahrper = |{ im_gjahr }{  _poper }| ) TO t_jahrpers.
        ENDDO.
      ENDIF.
      APPEND VALUE #( line = _tabix jahrpers = t_jahrpers[] ) TO rt_splitdat.
      FREE: t_jahrpers.
    ENDDO.


  ENDMETHOD.


  METHOD _monat_splitdat.

    DATA: _mod     TYPE n LENGTH 2,
          _poper   TYPE poper,
          _tabix   TYPE sy-tabix,
          t_monats TYPE tt_monatdat.

    IF NOT im_split BETWEEN 1 AND 12.
      MESSAGE e018(yclean) RAISING contains_error.
    ENDIF.

    _mod = floor( 12 / im_split ).

    DO im_split TIMES.
      ADD 1 TO _tabix.
      IF _tabix EQ im_split.
        DO.
          ADD 1 TO _poper.
          APPEND VALUE #( poper = _poper ) TO t_monats.
          IF _poper EQ 12.
            EXIT.
          ENDIF.
        ENDDO.
      ELSE.
        DO _mod TIMES.
          ADD 1 TO _poper.
          APPEND VALUE #( poper = _poper ) TO t_monats.
        ENDDO.
      ENDIF.
      APPEND VALUE #( line = _tabix monats = t_monats[] ) TO rt_splitdat.
      FREE: t_monats.
    ENDDO.


  ENDMETHOD.


  METHOD _rb1_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING ckmlcr.

    DECLARE lv_count NUMBER(10);

    SELECT COUNT (*) INTO lv_count FROM :iv_kalnr;
    IF :lv_count = 0 THEN
        DELETE FROM ckmlcr as db WHERE( db.mandt, db.kalnr, db.bdatj, db.poper, db.untper, db.curtp )
                                    IN ( SELECT  DISTINCT mandt, kalnr, bdatj, poper, untper, curtp
                                            FROM ckmlcr
                                            WHERE mandt  = Session_context('CLIENT')
                                              AND bdatj  = :iv_bdatj
                                              AND poper IN( SELECT DISTINCT poper FROM :iv_poper )
                                              AND curtp = :iv_curtp );
    ELSE
        DELETE FROM ckmlcr as db WHERE( db.mandt, db.kalnr, db.bdatj, db.poper, db.untper, db.curtp )
                                    IN ( SELECT  DISTINCT mandt, kalnr, bdatj, poper, untper, curtp
                                            FROM ckmlcr
                                            WHERE mandt  = Session_context('CLIENT')
                                              AND bdatj  = :iv_bdatj
                                              AND poper IN( SELECT DISTINCT poper FROM :iv_poper )
                                              AND curtp = :iv_curtp
                                              AND kalnr IN( SELECT DISTINCT kalnr FROM :iv_kalnr) );
    END IF ;
    ev_subrc = 0;
    ev_result = 'Entity has been deleted successfully.';


  ENDMETHOD.


  METHOD _rb2_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING mldoc.


    DECLARE lv_count NUMBER(10);

    SELECT COUNT (*) INTO lv_count FROM :iv_docref;
    IF :lv_count = 0 THEN
        DELETE FROM mldoc as db WHERE( db.mandt, db.docref, db.curtp )
                                   IN ( SELECT DISTINCT mandt, docref, curtp
                                            FROM mldoc
                                            WHERE mandt  = Session_context('CLIENT')
                                              AND jahrper IN( SELECT DISTINCT jahrper FROM :iv_jahrper)
                                              AND curtp = :iv_curtp );
    ELSE
        DELETE FROM mldoc as db WHERE( db.mandt, db.docref, db.curtp )
                                   IN ( SELECT DISTINCT mandt, docref, curtp
                                            FROM mldoc
                                            WHERE mandt  = Session_context('CLIENT')
                                              AND jahrper IN( SELECT DISTINCT jahrper FROM :iv_jahrper)
                                              AND curtp = :iv_curtp
                                              AND docref IN( SELECT DISTINCT docref FROM :iv_docref) );
    END IF ;
    ev_subrc = 0;
    ev_result = 'Entity has been deleted successfully.';

  ENDMETHOD.


  METHOD _rb3_rundat BY DATABASE PROCEDURE
                     FOR HDB
                     LANGUAGE SQLSCRIPT
                     USING mldoc_extract.

    DECLARE lv_count NUMBER(10);

    SELECT COUNT (*) INTO lv_count FROM :iv_docref;
    IF :lv_count = 0 THEN
        DELETE FROM mldoc_extract as db WHERE( db.mandt, db.kalnr, db.jahrper, db.docref, db.curtp )
                                           IN ( SELECT DISTINCT mandt, kalnr, jahrper, docref, curtp
                                                    FROM mldoc_extract
                                                    WHERE mandt  = Session_context('CLIENT')
                                                      AND jahrper IN( SELECT DISTINCT jahrper FROM :iv_jahrper)
                                                      AND curtp = :iv_curtp );
    ELSE
        DELETE FROM mldoc_extract as db WHERE( db.mandt, db.kalnr, db.jahrper, db.docref, db.curtp )
                                           IN ( SELECT DISTINCT mandt, kalnr, jahrper, docref, curtp
                                                    FROM mldoc_extract
                                                    WHERE mandt  = Session_context('CLIENT')
                                                      AND jahrper IN( SELECT DISTINCT jahrper FROM :iv_jahrper)
                                                      AND curtp = :iv_curtp
                                                      AND docref IN( SELECT DISTINCT docref FROM :iv_docref) );
    END IF ;
    ev_subrc = 0;
    ev_result = 'Entity has been deleted successfully.';

  ENDMETHOD.
ENDCLASS.

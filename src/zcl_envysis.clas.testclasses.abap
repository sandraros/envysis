*"* use this source file for your ABAP unit test classes

CLASS ltc_get_package_used_objects DEFINITION DEFERRED.
CLASS zcl_envysis DEFINITION LOCAL FRIENDS ltc_get_package_used_objects.

CLASS ltch_doc DEFINITION
    FOR TESTING.

  PUBLIC SECTION.
    INTERFACES lif_doc.
    TYPES:
      BEGIN OF ty_tadir,
        pgmid    TYPE tadir-pgmid,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir,
      BEGIN OF ty_para,
        rollname TYPE rollname,
        paramid  TYPE memoryid,
      END OF ty_para,
      BEGIN OF ty_typeinfo,
        typename TYPE tabname,
        typekind TYPE ddtypekind,
        gotstate TYPE ddgotstate,
      END OF ty_typeinfo,
      BEGIN OF ty_function_include,
        funcname TYPE rs38l_fnam,
        include  TYPE progname,
      END OF ty_function_include,
      BEGIN OF ty_tdevc,
        devclass      TYPE tdevc-devclass,
        dlvunit       TYPE tdevc-dlvunit,
        super_package TYPE devclass,
        sub_packages  TYPE STANDARD TABLE OF devclass WITH EMPTY KEY,
        interfaces    TYPE STANDARD TABLE OF scomifnam WITH EMPTY KEY,
      END OF ty_tdevc,
      BEGIN OF ty_e071_to_tadir,
        e071  TYPE e071,
        tadir TYPE tadir,
      END OF ty_e071_to_tadir.
    DATA:
      tadir_lines TYPE TABLE OF ty_tadir,
      BEGIN OF doma,
        dd01v_lines TYPE TABLE OF dd01v,
      END OF doma,
      BEGIN OF dtel,
        dd04v_lines TYPE TABLE OF dd04v,
        tpara_lines TYPE TABLE OF ty_para,
      END OF dtel,
      BEGIN OF enqu,
        dd25v_lines TYPE TABLE OF dd25v,
      END OF enqu,
      BEGIN OF shlp,
        dd30v_lines TYPE TABLE OF dd30v,
      END OF shlp,
      BEGIN OF tabl,
        dd03p_lines TYPE lif_doc=>ty_lt_dd03p,
        dd08v_lines TYPE lif_doc=>ty_lt_dd08v,
        dd35v_lines TYPE lif_doc=>ty_lt_dd35v,
      END OF tabl,
      BEGIN OF ttyp,
        dd40v_lines TYPE TABLE OF dd40v,
      END OF ttyp,
      typeinfo_lines TYPE TABLE OF ty_typeinfo,
      BEGIN OF view,
        dd26v_lines TYPE lif_doc=>ty_lt_dd26v,
      END OF view,
      function_includes   TYPE TABLE OF ty_function_include,
      program_includes    TYPE lif_doc=>ty_lt_incl,
      tdevc_lines         TYPE TABLE OF ty_tdevc,
      e071_to_tadir_lines TYPE TABLE OF ty_e071_to_tadir,
      dd03l_lines         TYPE TABLE OF lif_doc=>ty_dd03l,
      wbcrossgt_lines     TYPE TABLE OF wbcrossgt,
      tcodes              TYPE TABLE OF lif_doc=>ty_tcode_info,
      cross_lines         TYPE TABLE OF cross,
      trdir_lines         TYPE TABLE OF trdir.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltch_doc IMPLEMENTATION.

  METHOD lif_doc~ddif_doma_get.

    dd01v = doma-dd01v_lines[ domname = domname ].

  ENDMETHOD.

  METHOD lif_doc~ddif_dtel_get.

    dd04v = dtel-dd04v_lines[ rollname = rollname ].
    tpara = dtel-tpara_lines[ rollname = rollname ].

  ENDMETHOD.

  METHOD lif_doc~ddif_enqu_get.

    dd25v = enqu-dd25v_lines[ viewname = lockobject ].

  ENDMETHOD.

  METHOD lif_doc~ddif_shlp_get.

    dd30v = shlp-dd30v_lines[ shlpname = shlpname ].

  ENDMETHOD.

  METHOD lif_doc~ddif_tabl_get.

    et_dd03p = VALUE #(
        FOR <dd03p> IN tabl-dd03p_lines WHERE ( tabname = name )
        ( <dd03p> ) ).
    et_dd08v = VALUE #(
        FOR <dd08v> IN tabl-dd08v_lines WHERE ( tabname = name )
        ( <dd08v> ) ).
    et_dd35v = VALUE #(
        FOR <dd35v> IN tabl-dd35v_lines WHERE ( tabname = name )
        ( <dd35v> ) ).

  ENDMETHOD.

  METHOD lif_doc~ddif_ttyp_get.

    rs_dd40v = ttyp-dd40v_lines[ typename = ttypname ].

  ENDMETHOD.

  METHOD lif_doc~ddif_typeinfo_get.

    DATA(x) = REF #( typeinfo_lines[ typename = typename ] ).
    typekind = x->typekind.
    gotstate = x->gotstate.

  ENDMETHOD.

  METHOD lif_doc~ddif_view_get.

    rt_dd26v = VALUE #(
        FOR <dd26v> IN view-dd26v_lines WHERE ( viewname = viewname )
        ( <dd26v> ) ).

  ENDMETHOD.

  METHOD lif_doc~function_exists.

    include = function_includes[ funcname = funcname ].

  ENDMETHOD.

  METHOD lif_doc~rs_get_all_includes.

    includes = VALUE #(
        FOR <program_include> IN program_includes WHERE ( master = progname )
        ( <program_include> ) ).

  ENDMETHOD.

  METHOD lif_doc~rs_progname_concatenate.

    cl_abap_unit_assert=>fail( msg = 'TODO no need to mock RS_PROGNAME_CONCATENATE, real objects will be used' ).

  ENDMETHOD.

  METHOD lif_doc~rs_progname_split.

    cl_abap_unit_assert=>fail( msg = 'TODO no need to mock RS_PROGNAME_SPLIT, real objects will be used' ).

  ENDMETHOD.

  METHOD lif_doc~tr_check_type.

    we_tadir = VALUE #( e071_to_tadir_lines[ e071-pgmid    = wi_e071-pgmid
                                             e071-object   = wi_e071-object
                                             e071-obj_name = wi_e071-obj_name
               ]-tadir
               DEFAULT VALUE #( pgmid    = wi_e071-pgmid
                                object   = wi_e071-object
                                obj_name = wi_e071-obj_name ) ).

  ENDMETHOD.

  METHOD lif_doc~select_tdevc.

    tdevc_lines = VALUE #(
        FOR <devclass> IN devclasses
        LET <tdevc> = me->tdevc_lines[ devclass = <devclass> ]
        IN ( devclass = <tdevc>-devclass
             dlvunit  = <tdevc>-dlvunit
             parentcl = <tdevc>-super_package ) ).

  ENDMETHOD.

  METHOD lif_doc~select_tadir_2.

    DATA(initial_tadir) = NEW zcl_envysis=>ty_tadir( ).

    tadir_lines = VALUE #(
        FOR <e071> IN it_e071_key_new
        LET tadir = REF #( tadir_lines[ pgmid    = <e071>-pgmid
                                               object   = <e071>-object
                                               obj_name = <e071>-obj_name ]
                        DEFAULT initial_tadir )
        IN ( LINES OF COND #( WHEN tadir IS BOUND THEN VALUE #( ( tadir->* ) ) ) ) ).

  ENDMETHOD.

  METHOD lif_doc~select_tadir_3.

    devclass = tadir_lines[ pgmid    = e071_objkey-pgmid
                            object   = e071_objkey-object
                            obj_name = e071_objkey-obj_name
                ]-devclass.

  ENDMETHOD.

  METHOD lif_doc~get_oo_info_of_include.

    cl_abap_unit_assert=>fail( msg = 'TODO no need to mock GET_OO_INFO_OF_INCLUDE, real objects will be used' ).

  ENDMETHOD.

  METHOD lif_doc~select_tadir_1.

    rt_e071_key = VALUE #(
      FOR <tadir> IN tadir_lines WHERE ( devclass IN packages )
      ( pgmid    = <tadir>-pgmid
        object   = <tadir>-object
        obj_name = <tadir>-obj_name ) ).

  ENDMETHOD.

  METHOD lif_doc~select_dd03l.

    dd03l = dd03l_lines[ tabname = tabname fieldname = fieldname ].

  ENDMETHOD.

  METHOD lif_doc~select_wbcrossgt.

    wbcrossgt_lines = VALUE #(
        FOR <include> IN includes
        FOR <wbcrossgt> IN me->wbcrossgt_lines WHERE ( include = <include>-master AND otype = 'TY' )
        ( <wbcrossgt> ) ).

  ENDMETHOD.

  METHOD lif_doc~get_objects_needed_by_package.

    result = VALUE #(
        LET <tdevc> = tdevc_lines[ devclass = i_package_name ]
        IN super_package = <tdevc>-super_package
           sub_packages  = <tdevc>-sub_packages
           interfaces    = <tdevc>-interfaces ).

  ENDMETHOD.

  METHOD lif_doc~get_tcode_required_objects.

    result = tcodes[ tcode = i_tcode ].

  ENDMETHOD.

  METHOD lif_doc~select_cross.

    cross_lines = VALUE #(
        FOR <include> IN includes
        FOR <cross> IN me->cross_lines WHERE ( include = <include>-master )
        ( <cross> ) ).

  ENDMETHOD.

  METHOD lif_doc~select_trdir.

    r_subc = trdir_lines[ name = progname ]-subc.

  ENDMETHOD.

  METHOD lif_doc~convert_e071_to_tadir_objkeys.

  ENDMETHOD.

ENDCLASS.

CLASS ltc_get_package_used_objects DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS package_contains_nothing FOR TESTING.
    METHODS one_program_containing_nothing FOR TESTING.
    METHODS exclude_standard FOR TESTING.

    DATA: cut TYPE REF TO zcl_envysis,
          doc TYPE REF TO ltch_doc.

*    class-methods class_setup.
*    class-methods class_teardown.
    methods setup.
*    methods teardown.

ENDCLASS.

CLASS ltc_get_package_used_objects IMPLEMENTATION.

  METHOD setup.

    doc = NEW ltch_doc( ).
    cut = NEW zcl_envysis( ).
    cut->doc = doc.

  ENDMETHOD.

  METHOD one_program_containing_nothing.

    doc->tadir_lines = VALUE #(
        ( pgmid = 'R3TR' object = 'PROG' obj_name = 'ZPROG' devclass = 'ZDEVC' ) ).
*    doc->tdevc_lines = VALUE #(
*        ( devclass = 'ZDEVC' ) ).


    cut->get_package_used_objects(
      EXPORTING
        packages      = VALUE #( ( sign = 'I' option = 'EQ' low = 'ZDEVC' ) )
*        " Do not get the used objects of standard objects (= exclude all except HOME/LOCAL)
*        excluding     = VALUE #( software_components =
*            VALUE #( sign = 'E' option = 'EQ' ( low = 'HOME' ) ( low = 'LOCAL' ) ) )
      IMPORTING
        all           = DATA(all)
        tadir_lines_2 = DATA(tadir_lines_2) ).

    cl_abap_unit_assert=>assert_equals(
        act = all
        exp = VALUE zcl_envysis=>tt_e071_rel2(
        ( object       = VALUE #( pgmid = 'R3TR' object = 'PROG' obj_name = 'ZPROG' )
          soft_or_hard = 'S'
          subobject    = VALUE #( pgmid = 'R3TR' object = 'DEVC' obj_name = 'ZDEVC' ) ) ) ).

  ENDMETHOD.

  METHOD package_contains_nothing.

    cut->get_package_used_objects(
      EXPORTING
        packages      = VALUE #( ( sign = 'I' option = 'EQ' low = 'ZDEVC' ) )
      IMPORTING
        all           = DATA(all)
        tadir_lines_2 = DATA(tadir_lines_2) ).

    cl_abap_unit_assert=>assert_equals(
        act = all
        exp = VALUE zcl_envysis=>tt_e071_rel2( ) ).

  ENDMETHOD.

  METHOD exclude_standard.

    doc->tadir_lines = VALUE #(
        ( pgmid = 'R3TR' object = 'PROG' obj_name = 'ZPROG' devclass = 'ZDEVC' ) ).
    doc->cross_lines = VALUE #(
        ( INCLUDE = 'ZPROG' NAME = 'GUI_UPLOAD' PROG = 'PROG' TYPE = 'F' ) ).
*    doc->tdevc_lines = VALUE #(
*        ( devclass = 'ZDEVC' ) ).


    cut->get_package_used_objects(
      EXPORTING
        packages      = VALUE #( ( sign = 'I' option = 'EQ' low = 'ZDEVC' ) )
        " Do not analyze what the standard objects use (= exclude all except HOME/LOCAL)
        excluding     = VALUE #( software_components =
            VALUE #( sign = 'E' option = 'EQ' ( low = 'HOME' ) ( low = 'LOCAL' ) ) )
      IMPORTING
        all           = DATA(all)
        tadir_lines_2 = DATA(tadir_lines_2) ).

    cl_abap_unit_assert=>assert_equals(
        act = all
        exp = VALUE zcl_envysis=>tt_e071_rel2(
        ( object       = VALUE #( pgmid = 'R3TR' object = 'PROG' obj_name = 'ZPROG' )
          soft_or_hard = 'S'
          subobject    = VALUE #( pgmid = 'R3TR' object = 'DEVC' obj_name = 'ZDEVC' ) ) ) ).

  ENDMETHOD.

ENDCLASS.

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_doc DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_doc.
ENDCLASS.

CLASS lcl_doc IMPLEMENTATION.

  METHOD lif_doc~rs_progname_concatenate.

    " Copy of function module RS_PROGNAME_CONCATENATE and
    " + adaptation of OO part to avoid short dump if object type doesn't exist

    DATA: bool TYPE c LENGTH 1.

    CONSTANTS:
*      false LIKE bool VALUE 'F',
*      true  LIKE bool VALUE 'T',
*      yes   LIKE bool VALUE 'X',
          no    LIKE bool VALUE ' '.

    CONSTANTS:

      c_menu_progname_praefix_len      "Bereichsmenü
        TYPE i   VALUE 4,
      c_menu_progname_praefix(c_menu_progname_praefix_len)
        TYPE c   VALUE 'MENU',

      c_type_progname_praefix_len      "Type-Pools
        TYPE i   VALUE 3,
      c_type_progname_praefix(c_type_progname_praefix_len)
        TYPE c   VALUE '%_C',

      c_cntx_progname_praefix_len      "Contexte
        TYPE i   VALUE 10,
      c_cntx_progname_praefix(c_cntx_progname_praefix_len)
        TYPE c   VALUE 'CONTEXT_S_',

      c_mst_progname_praefix_len       "alte Tab-pflege
        TYPE i   VALUE 3,
      c_mst_progname_praefix(c_mst_progname_praefix_len)
        TYPE c   VALUE 'MST'.

    DATA: l_program   TYPE rs38l-include,
          l_namespace TYPE rs38l-namespace,
          l_area      TYPE rs38l-area,
          l_name      TYPE trdir-name.

    l_program   = space.
    l_namespace = space.

* Programmnamen für Bestandteile von Funktionsgruppenobjekten
    CLEAR fugr_progname_group.
    CLEAR fugr_progname_include.
    IF NOT ( fugr_group IS INITIAL ).
      l_area = fugr_group.
      CALL FUNCTION 'FUNCTION_INCLUDE_CONCATENATE'
        EXPORTING
          include_number           = fugr_include_number
        IMPORTING
          include                  = fugr_progname_include
          uxx                      = fugr_progname_uxx
          top                      = fugr_progname_top
        CHANGING
          program                  = fugr_progname_group
          complete_area            = l_area
        EXCEPTIONS
          delimiter_wrong_position = 1
          OTHERS                   = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_envysis.
      ENDIF.
    ENDIF.


* Programmnamen für Bestandteile von log. Datenbanken
    CLEAR sldb_progname_db.
    CLEAR sldb_progname_sel.
    IF NOT ( sldb_name IS INITIAL ).
      CALL FUNCTION 'LDB_CONVERT_LDBNAME_2_DBPROG'
        EXPORTING
          ldb_name                  = sldb_name
          flag_existence_check      = no
        IMPORTING
          db_name                   = sldb_progname_db
          sel_name                  = sldb_progname_sel
        EXCEPTIONS
          wrong_position_of_slashes = 1
          OTHERS                    = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_envysis.
      ENDIF.
    ENDIF.

* Programmnamen für Bereichsmenü
    IF NOT ( menu_name IS INITIAL ).
      l_name = menu_name.
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = l_name
        IMPORTING
          namespace              = l_namespace
          name_without_namespace = l_name
        EXCEPTIONS
          delimiter_error        = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_envysis.
      ENDIF.
      CONCATENATE l_namespace
                  c_menu_progname_praefix
                  l_name
             INTO menu_progname.
    ELSE.
      CLEAR menu_progname.
    ENDIF.

* Programmnamen für Type-Pools
    IF NOT ( type_name IS INITIAL ).
      l_name = type_name.
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = l_name
        IMPORTING
          namespace              = l_namespace
          name_without_namespace = l_name
        EXCEPTIONS
          delimiter_error        = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_envysis.
      ENDIF.
      CONCATENATE l_namespace
                  c_type_progname_praefix
                  l_name
             INTO type_progname.
    ELSE.
      CLEAR type_progname.
    ENDIF.

* Programmnamen für Contexte
    IF NOT ( cntx_name IS INITIAL ).
      l_name = cntx_name.
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = l_name
        IMPORTING
          namespace              = l_namespace
          name_without_namespace = l_name
        EXCEPTIONS
          delimiter_error        = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_envysis.
      ENDIF.
      CONCATENATE l_namespace
                  c_cntx_progname_praefix
                  l_name
             INTO cntx_progname.
    ELSE.
      CLEAR cntx_progname.
    ENDIF.

* Programmnamen für (veraltete) Tabellenpflege
    IF NOT ( mst_name IS INITIAL ).
      l_name = mst_name.
      CALL FUNCTION 'RS_NAME_SPLIT_NAMESPACE'
        EXPORTING
          name_with_namespace    = l_name
        IMPORTING
          namespace              = l_namespace
          name_without_namespace = l_name
        EXCEPTIONS
          delimiter_error        = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_envysis.
      ENDIF.
      CONCATENATE l_namespace
                  c_mst_progname_praefix
                  l_name
             INTO mst_progname.
    ELSE.
      CLEAR mst_progname.
    ENDIF.

* oo
* class
    CLEAR clas_progname.
    IF NOT clas_name IS INITIAL.
      " STANDARD CODE RAISES NO_OBJECTTYPE IF CLASS POOL DOESN'T EXIST
*    l_ooref = cl_oo_include_naming=>get_instance_by_name( clas_name ).
*    clas_progname = l_ooref->pool.
      TYPES: BEGIN OF ty_progname,
               main_part TYPE c LENGTH 30,
               extension TYPE c LENGTH 10,
             END OF ty_progname.
      clas_progname = translate(
            val  = CONV string( VALUE ty_progname( main_part = clas_name extension = 'CP' ) )
            from = ` `
            to   = '=' ).
    ENDIF.

* interface
    CLEAR intf_progname.
    IF NOT intf_name IS INITIAL.
      " STANDARD CODE RAISES NO_OBJECTTYPE IF INTERFACE POOL DOESN'T EXIST
*    l_ooref = cl_oo_include_naming=>get_instance_by_name( intf_name ).
*    intf_progname = l_ooref->pool.
      intf_progname = translate(
            val  = CONV string( VALUE ty_progname( main_part = intf_name extension = 'IP' ) )
            from = ` `
            to   = '=' ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_doc~rs_progname_split.

    CALL FUNCTION 'RS_PROGNAME_SPLIT'
      EXPORTING
        progname_with_namespace = include
      IMPORTING
        fugr_group              = result-fugr_group
        class_is_name           = result-class_is_name
        class_name              = result-class_name
        class_is_method_name    = result-class_is_method_name
        class_method_name       = result-class_method_name
      EXCEPTIONS
        delimiter_error         = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      " todo RAISE EXCEPTION TYPE lcx_...
    ENDIF.

  ENDMETHOD.

  METHOD lif_doc~tr_check_type.

    DATA ls_e071 TYPE e071.

    we_tadir = VALUE #( ).

    " TR_CHECK_TYPE returns empty when object = VARX !
    " With VARI, it returns program name
    ls_e071 = wi_e071.
    IF ls_e071-object = 'VARX'.
      ls_e071-object = 'VARI'.
    ENDIF.
    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071  = ls_e071
      IMPORTING
        we_tadir = we_tadir.
*        we_lock_key = we_lock_key.

  ENDMETHOD.

  METHOD lif_doc~ddif_typeinfo_get.

    DATA: l_typename TYPE typename,
          l_typekind TYPE ddtypekind.

    CLEAR typekind.

    l_typename = typename.
    CALL FUNCTION 'DDIF_TYPEINFO_GET'
      EXPORTING
        typename = l_typename
      IMPORTING
        typekind = l_typekind.

    IF l_typekind IS INITIAL.
      " TY:ObjectType
      " If it's not a DDIC type, then it's a class or interface
      DATA l_clstype TYPE seoclass-clstype.
      SELECT SINGLE clstype FROM seoclass INTO l_clstype
            WHERE clsname = typename.
      IF sy-subrc = 0.
        IF l_clstype = 0.
          l_typekind = 'CLAS'.
        ELSE.
          l_typekind = 'INTF'.
        ENDIF.
      ENDIF.
    ENDIF.

    typekind = l_typekind.

  ENDMETHOD.

  METHOD lif_doc~rs_get_all_includes.

    " Get list of INCLUDE statements.
    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = progname
      TABLES
        includetab   = includes
      EXCEPTIONS
        not_existent = 0
        no_program   = 0
        OTHERS       = 3.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
* 2010-11-22
    " D010INC table may sometimes contain an erroneous entry:
    "   for classname===...===CP, RS_GET_ALL_INCLUDES returns
    "   packagename===...===P that doesn't exist
    DELETE includes WHERE master+30 = 'P'.

  ENDMETHOD.

  METHOD lif_doc~function_exists.

    " FUNCTION_EXISTS
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = funcname
      IMPORTING
        include            = include
      EXCEPTIONS
        function_not_exist = 1.

  ENDMETHOD.

  METHOD lif_doc~ddif_tabl_get.

    " DDIF_TABL_GET
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = name
      TABLES
        dd03p_tab     = et_dd03p
        dd08v_tab     = et_dd08v
        dd35v_tab     = et_dd35v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD lif_doc~ddif_view_get.

    DATA ls_dd25v TYPE dd25v.

    " DDIF_VIEW_GET
    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = viewname
      IMPORTING
        dd25v_wa      = ls_dd25v
      TABLES
        dd26v_tab     = rt_dd26v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD lif_doc~ddif_ttyp_get.

    " DDIF_TTYP_GET
    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name          = ttypname
      IMPORTING
        dd40v_wa      = rs_dd40v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD lif_doc~ddif_dtel_get.

    " DDIF_DTEL_GET
    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = rollname
      IMPORTING
        dd04v_wa      = dd04v
        tpara_wa      = tpara
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD lif_doc~ddif_doma_get.

    " DDIF_DOMA_GET
    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = domname
      IMPORTING
        dd01v_wa      = dd01v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD lif_doc~ddif_shlp_get.

    " DDIF_SHLP_GET
    CALL FUNCTION 'DDIF_SHLP_GET'
      EXPORTING
        name          = shlpname
      IMPORTING
        dd30v_wa      = dd30v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD lif_doc~ddif_enqu_get.

    " DDIF_ENQU_GET
    CALL FUNCTION 'DDIF_ENQU_GET'
      EXPORTING
        name          = lockobject
      IMPORTING
        dd25v_wa      = dd25v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD lif_doc~select_tdevc.

    IF devclasses IS NOT INITIAL.

      SELECT devclass, dlvunit, parentcl
          FROM tdevc
          FOR ALL ENTRIES IN @devclasses
          WHERE devclass = @devclasses-table_line
          INTO TABLE @tdevc_lines.

    ENDIF.

  ENDMETHOD.

  METHOD lif_doc~get_oo_info_of_include.

    CALL METHOD cl_oo_include_naming=>get_instance_by_include
      EXPORTING
        progname      = include-master
      RECEIVING
        cifref        = clif
      EXCEPTIONS
        no_objecttype = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD lif_doc~select_tadir_2.

    IF it_e071_key_new IS NOT INITIAL.

      SELECT tadir~pgmid, tadir~object, tadir~obj_name, tadir~devclass, tdevc~dlvunit AS softcomp, df14l~ps_posid AS applcomp
          FROM tadir
          LEFT OUTER JOIN tdevc
            ON tdevc~devclass = tadir~devclass
          LEFT OUTER JOIN df14l
            ON df14l~fctr_id = tdevc~component
          FOR ALL ENTRIES IN @it_e071_key_new
          WHERE tadir~pgmid    = @it_e071_key_new-pgmid
            AND tadir~object   = @it_e071_key_new-object
            AND tadir~obj_name = @it_e071_key_new-obj_name(40)
          INTO TABLE @tadir_lines.

    ENDIF.

  ENDMETHOD.

  METHOD lif_doc~select_tadir_1.

    SELECT pgmid, object, obj_name
        FROM tadir
        WHERE devclass IN @packages
        INTO TABLE @rt_e071_key.

  ENDMETHOD.

  METHOD lif_doc~select_tadir_3.

    SELECT SINGLE devclass FROM tadir INTO devclass
          WHERE pgmid     = e071_objkey-pgmid
            AND object    = e071_objkey-object
            AND obj_name  = e071_objkey-obj_name.

  ENDMETHOD.

  METHOD lif_doc~select_wbcrossgt.

    IF includes IS NOT INITIAL.

      SELECT * FROM wbcrossgt INTO TABLE wbcrossgt_lines
            FOR ALL ENTRIES IN includes
            WHERE include = includes-master
              AND otype = 'TY'.

    ENDIF.

  ENDMETHOD.


  METHOD lif_doc~select_dd03l.

    SELECT SINGLE FROM dd03l
        FIELDS tabname, fieldname, precfield, comptype, rollname
        WHERE tabname   = @tabname
          AND fieldname = @fieldname
          AND as4local  = 'A'
          AND as4vers   = 0
        INTO @dd03l.

  ENDMETHOD.


  METHOD lif_doc~get_objects_needed_by_package.

    DATA: lo_package       TYPE REF TO if_package,
          lto_package_sub  TYPE scompaklis,
          lo_package_sub   TYPE REF TO if_package,
          lo_package_super TYPE REF TO if_package,
          lo_interface     TYPE REF TO if_package_interface,
          lto_interface    TYPE tpak_package_interface_list.

    CALL METHOD cl_package=>load_package
      EXPORTING
        i_package_name             = i_package_name
      IMPORTING
        e_package                  = lo_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        object_locked_and_modified = 4
        OTHERS                     = 5.

    IF sy-subrc <> 0.
      " TODO
    ENDIF.

    IF lo_package IS BOUND.

      " SUB-PACKAGES
      CALL METHOD lo_package->get_sub_packages
        IMPORTING
          e_sub_packages   = lto_package_sub
        EXCEPTIONS
          object_invalid   = 1
          leaf_package     = 2
          unexpected_error = 3
          OTHERS           = 4.

      IF sy-subrc <> 0.
        " TODO
      ENDIF.

      LOOP AT lto_package_sub INTO lo_package_sub.
        APPEND lo_package_sub->package_name TO result-sub_packages.
      ENDLOOP.

      " SUPER PACKAGE
      CALL METHOD lo_package->get_super_package
        IMPORTING
          e_super_package = lo_package_super
        EXCEPTIONS
          root_package    = 1
          OTHERS          = 2.

      IF sy-subrc = 0.

        result-super_package = lo_package_super->package_name.

        CALL METHOD lo_package->get_interfaces
          IMPORTING
            e_package_interfaces = lto_interface.

        LOOP AT lto_interface INTO lo_interface.
          APPEND lo_interface->interface_name TO result-interfaces.
        ENDLOOP.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD lif_doc~select_cross.

    SELECT * FROM cross
        INTO TABLE cross_lines
        FOR ALL ENTRIES IN includes
        WHERE include = includes-master
          AND name    NE '?'.

  ENDMETHOD.


  METHOD lif_doc~select_trdir.

    SELECT SINGLE subc FROM trdir INTO r_subc WHERE name = progname.

  ENDMETHOD.


  METHOD lif_doc~get_tcode_required_objects.

    DATA(eo_transaction) = zcl_tcode=>load( i_tcode ).

    result = VALUE lif_doc=>ty_tcode_info(
        tcode       = i_tcode
        type        = eo_transaction->type
        s_report    = eo_transaction->s_report
        s_dialog    = eo_transaction->s_dialog
        s_object    = eo_transaction->s_object
        s_parameter = eo_transaction->s_parameter
        s_variant   = eo_transaction->s_variant ).

  ENDMETHOD.


  METHOD lif_doc~convert_e071_to_tadir_objkeys.

    LOOP AT e071_objkeys REFERENCE INTO DATA(e071_objkey).



    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

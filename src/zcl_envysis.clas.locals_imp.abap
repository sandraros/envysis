*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_tcode DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES : BEGIN OF ty_cobj,
              field TYPE tstca-field,
              value TYPE tstca-value,
              olen  TYPE dfies-outputlen,
            END OF ty_cobj,
            tt_cobj TYPE TABLE OF ty_cobj,
            "! report transaction
            BEGIN OF ty_report,
              program_name    TYPE tstc-pgmna,
              screen_number   TYPE tstc-dypno,
              program_variant TYPE rsstcd-repo_vari,
              auth_object     TYPE tstca-objct,
              t_cobj          TYPE TABLE OF ty_cobj WITH DEFAULT KEY,
            END OF ty_report,
            "! dialog transaction
            BEGIN OF ty_dialog,
              program_name              TYPE tstc-pgmna,
              screen_number             TYPE tstc-dypno,
              allow_std_transac_variant TYPE flag,
              auth_object               TYPE tstca-objct,
              t_cobj                    TYPE TABLE OF ty_cobj WITH DEFAULT KEY,
            END OF ty_dialog,
            "! parameter transaction
            BEGIN OF ty_parameter,
              called_tcode     TYPE tstc-tcode,
              skip_init_screen TYPE flag,
              inherit_gui_attr TYPE flag,
              program_name     TYPE tstc-pgmna,
              screen_number    TYPE tstc-dypno,
              t_param          TYPE s_param,
            END OF ty_parameter,
            "! variant transaction
            BEGIN OF ty_variant,
              called_tcode     TYPE tstc-tcode,
              transac_variant  TYPE rsstcd-variant,
              cross_client     TYPE flag,
              inherit_gui_attr TYPE flag,
            END OF ty_variant,
            "! object transaction
            BEGIN OF ty_object,
              transaction_model TYPE flag,
              local_class       TYPE flag,
              global_class_name TYPE seoclsname,
              local_class_name  TYPE seoclsname,
              method_name       TYPE seocpdname,
              program_name      TYPE tstc-pgmna,
              update_mode       TYPE char01, "only for transaction model
              auth_object       TYPE tstca-objct,
              t_cobj            TYPE TABLE OF ty_cobj WITH DEFAULT KEY,
            END OF ty_object,
            ty_transaction_type TYPE char01.

    CONSTANTS: BEGIN OF c_type,
                 report    TYPE ty_transaction_type VALUE 'R',
                 dialog    TYPE ty_transaction_type VALUE 'D',
                 object    TYPE ty_transaction_type VALUE 'O',
                 menu_area TYPE ty_transaction_type VALUE 'M',
                 variant   TYPE ty_transaction_type VALUE 'V',
                 parameter TYPE ty_transaction_type VALUE 'P',
               END OF c_type.

    DATA : tcode           TYPE tstc-tcode READ-ONLY,
           type            TYPE ty_transaction_type READ-ONLY,
           s_report        TYPE ty_report READ-ONLY,
           s_dialog        TYPE ty_dialog READ-ONLY,
           s_object        TYPE ty_object READ-ONLY,
           s_parameter     TYPE ty_parameter READ-ONLY,
           s_variant       TYPE ty_variant READ-ONLY,
           locked_via_sm01 TYPE flag READ-ONLY,
           professional    TYPE flag READ-ONLY,
           easy_web        TYPE flag READ-ONLY,
           ew_service      TYPE tstcc-s_service READ-ONLY, "easy web
           ew_pervasive    TYPE tstcc-s_pervas READ-ONLY, "easy web
           gui_html        TYPE tstcc-s_webgui READ-ONLY,
           gui_win32       TYPE tstcc-s_win32 READ-ONLY,
           gui_java        TYPE tstcc-s_platin READ-ONLY.

    CLASS-METHODS load
      IMPORTING
        tcode         TYPE tcode
      RETURNING
        VALUE(result) TYPE REF TO lcl_tcode.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS _load
      IMPORTING
        tcode TYPE tcode.

ENDCLASS.

CLASS lcl_doc DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_doc.
ENDCLASS.




CLASS LCL_TCODE IMPLEMENTATION.

  METHOD load.

    result = NEW lcl_tcode( ).
    result->_load( tcode ).

  ENDMETHOD.


  METHOD _load.

    FIELD-SYMBOLS:
      <ls_tstc>                     TYPE tstc,
      <ls_tstcc>                    TYPE tstcc,
      <lt_cobj>                     TYPE tt_cobj,
      <ls_rsstcd>                   TYPE rsstcd,
      <lt_param>                    TYPE s_param,
      <l_easy_web>                  TYPE flag,
      <l_professional>              TYPE flag,
      <l_transaction_variant_flag>  TYPE flag,
      <l_transaction_type>          TYPE tstc-cinfo,
      <l_authorization_object_flag> TYPE syhex01,
      <l_locked_via_sm01>           TYPE syhex01,
      <l_gui_inherit>               TYPE flag.
    DATA l_auth_object TYPE tstca-objct.

    PERFORM select_tstc_tables_new IN PROGRAM saplseuk
          USING tcode sy-langu sy-langu.

    ASSIGN ('(SAPLSEUK)TSTC') TO <ls_tstc>.
    ASSIGN ('(SAPLSEUK)TSTCC') TO <ls_tstcc>.
    ASSIGN ('(SAPLSEUK)COBJ[]') TO <lt_cobj>.
    ASSIGN ('(SAPLSEUK)RSSTCD') TO <ls_rsstcd>.
    ASSIGN ('(SAPLSEUK)PARAM[]') TO <lt_param>.
    ASSIGN ('(SAPLSEUK)G_IAC_EWT') TO <l_easy_web>.
    ASSIGN ('(SAPLSEUK)G_PROFI_TRAN') TO <l_professional>.
    ASSIGN ('(SAPLSEUK)PARAM_VARI') TO <l_transaction_variant_flag>.
    ASSIGN ('(SAPLSEUK)TC_TYP') TO <l_transaction_type>.
    ASSIGN ('(SAPLSEUK)TC_CHK') TO <l_authorization_object_flag>.
    ASSIGN ('(SAPLSEUK)TC_ENQ') TO <l_locked_via_sm01>.
    ASSIGN ('(SAPLSEUK)G_GUI_INHE') TO <l_gui_inherit>.

    me->tcode = tcode.

    IF NOT <l_authorization_object_flag> IS INITIAL.
      SELECT SINGLE objct FROM tstca
            INTO l_auth_object
            WHERE tcode = tcode.
    ENDIF.

    CASE <l_transaction_type>.
      WHEN '80'.
        " Report transaction
        type = c_type-report.
        s_report-program_name = <ls_tstc>-pgmna.
        s_report-screen_number = <ls_tstc>-dypno.
        s_report-program_variant = <ls_rsstcd>-repo_vari.
        s_report-auth_object = l_auth_object.
        s_report-t_cobj = <lt_cobj>.
      WHEN '00'.
        type = c_type-dialog.
        s_dialog-program_name  = <ls_tstc>-pgmna.
        s_dialog-screen_number = <ls_tstc>-dypno.
        s_dialog-allow_std_transac_variant = <ls_rsstcd>-trans_var.
        s_dialog-auth_object   = l_auth_object.
        s_dialog-t_cobj        = <lt_cobj>.
      WHEN '01'.
        type = c_type-menu_area. "menu area (obsolete transaction type)
      WHEN '08'.
        type = c_type-object.
        IF <ls_rsstcd>-call_tcode = 'OS_APPLICATION'.
          s_object-transaction_model = 'X'.
          s_object-global_class_name = <ls_rsstcd>-classname.
* Update mode is stored in TSTCP-PARM like %UPDATE_MODE=?%
          IF <ls_rsstcd>-s_upddir = 'X'.
            s_object-update_mode = 'S'.
          ELSEIF <ls_rsstcd>-s_updtask = 'X'.
            s_object-update_mode = 'A'.
          ELSEIF <ls_rsstcd>-s_updlok = 'X'.
            s_object-update_mode = 'L'.
          ENDIF.
        ELSE.
          IF NOT <ls_tstc>-pgmna IS INITIAL.
            s_object-local_class       = 'X'.
            s_object-program_name      = <ls_tstc>-pgmna.
            s_object-local_class_name  = <ls_rsstcd>-classname.
          ELSE.
            s_object-global_class_name = <ls_rsstcd>-classname.
          ENDIF.
        ENDIF.
        s_object-method_name   = <ls_rsstcd>-method.
        s_object-auth_object   = l_auth_object.
        s_object-t_cobj        = <lt_cobj>.
      WHEN '02'.
        IF <l_transaction_variant_flag> = 'X'.
          " variant transaction
          type = c_type-variant.
          s_variant-called_tcode      = <ls_rsstcd>-call_tcode.
          s_variant-transac_variant   = <ls_rsstcd>-variant.
          s_variant-cross_client      = <ls_rsstcd>-s_ind_vari.
          s_variant-inherit_gui_attr  = <l_gui_inherit>.
        ELSE.
          " parameter transaction
          type = c_type-parameter.
          s_parameter-called_tcode      = <ls_rsstcd>-call_tcode.
          s_parameter-skip_init_screen  = <ls_rsstcd>-st_skip_1.
          s_parameter-inherit_gui_attr  = <l_gui_inherit>.
          s_parameter-program_name      = <ls_tstc>-pgmna.
          s_parameter-screen_number     = <ls_tstc>-dypno.
          s_parameter-t_param           = <lt_param>.
        ENDIF.
    ENDCASE.

    IF NOT <l_locked_via_sm01> IS INITIAL.
      locked_via_sm01 = 'X'.
    ENDIF.

    professional = <l_professional>.
    easy_web = <l_easy_web>.
    ew_service   = <ls_tstcc>-s_service.
    ew_pervasive = <ls_tstcc>-s_pervas.
    gui_html     = <ls_tstcc>-s_webgui.
    gui_win32    = <ls_tstcc>-s_win32.
    gui_java     = <ls_tstcc>-s_platin.

  ENDMETHOD.
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

    DATA(eo_transaction) = lcl_tcode=>load( i_tcode ).

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

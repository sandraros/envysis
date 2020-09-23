*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
INTERFACE lif_doc.

  TYPES:
    BEGIN OF ty_progname_split_result,
      fugr_group           TYPE rs38l_area,
      class_is_name        TYPE abap_bool,
      class_name           TYPE seoclsname,
      class_is_method_name TYPE abap_bool,
      class_method_name    TYPE seocpdname,
    END OF ty_progname_split_result,
    BEGIN OF ty_tadir_objkey,
    pgmid type tadir-pgmid,
    object type tadir-object,
    obj_name type tadir-obj_name,
    end OF ty_tadir_objkey,
    BEGIN OF ty_e071_to_tadir_objkey,
    e071_objkey type zcl_envysis=>ty_e071_objkey,
    tadir_objkey type ty_tadir_objkey,
    END OF ty_e071_to_tadir_objkey,
    tt_e071_to_tadir_objkey type STANDARD TABLE OF ty_e071_to_tadir_objkey with empty key,
    BEGIN OF ty_devc_required_objects,
      super_package TYPE devclass,
      sub_packages  TYPE STANDARD TABLE OF devclass WITH EMPTY KEY,
      interfaces    TYPE STANDARD TABLE OF scomifnam WITH EMPTY KEY,
    END OF ty_devc_required_objects,
    ty_lt_incl    TYPE STANDARD TABLE OF rseuinc WITH DEFAULT KEY,
    ty_lt_cross   TYPE STANDARD TABLE OF cross WITH DEFAULT KEY,
    ty_lt_dd03p   TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY,
    ty_lt_dd08v   TYPE STANDARD TABLE OF dd08v WITH DEFAULT KEY,
    ty_lt_dd35v   TYPE STANDARD TABLE OF dd35v WITH DEFAULT KEY,
    ty_lt_dd26v   TYPE STANDARD TABLE OF dd26v WITH DEFAULT KEY,
    ty_devclasses TYPE STANDARD TABLE OF tadir-devclass WITH DEFAULT KEY,
    BEGIN OF ty_tdevc,
      devclass TYPE tdevc-devclass,
      dlvunit  TYPE tdevc-dlvunit,
      parentcl TYPE tdevc-parentcl,
    END OF ty_tdevc,
    ty_tdevc_lines  TYPE HASHED TABLE OF ty_tdevc WITH UNIQUE KEY devclass,
    tt_e071_key     TYPE HASHED TABLE OF zcl_envysis=>ty_e071_objkey WITH UNIQUE KEY pgmid object obj_name,
    tt_e071_key_new TYPE STANDARD TABLE OF zcl_envysis=>ty_e071_objkey WITH DEFAULT KEY,
    ty_lt_wbcrossgt TYPE STANDARD TABLE OF wbcrossgt WITH DEFAULT KEY,
    BEGIN OF ty_dd03l,
      tabname   TYPE dd03l-tabname,
      fieldname TYPE dd03l-fieldname,
      precfield TYPE dd03l-precfield,
      reftype   TYPE dd03l-reftype,
      comptype  TYPE dd03l-comptype,
      rollname  TYPE dd03l-rollname,
    END OF ty_dd03l,
    BEGIN OF ty_tcode_info,
      tcode       TYPE sytcode,
      type        TYPE zcl_tcode=>ty_transaction_type,
      s_report    TYPE zcl_tcode=>ty_report,
      s_dialog    TYPE zcl_tcode=>ty_dialog,
      s_object    TYPE zcl_tcode=>ty_object,
      s_parameter TYPE zcl_tcode=>ty_parameter,
      s_variant   TYPE zcl_tcode=>ty_variant,
    END OF ty_tcode_info.

  METHODS ddif_tabl_get
    IMPORTING
      name     TYPE ddobjname
    EXPORTING
      et_dd03p TYPE ty_lt_dd03p
      et_dd08v TYPE ty_lt_dd08v
      et_dd35v TYPE ty_lt_dd35v.

  METHODS ddif_view_get
    IMPORTING
      viewname        TYPE ddobjname
    RETURNING
      VALUE(rt_dd26v) TYPE ty_lt_dd26v.

  METHODS ddif_ttyp_get
    IMPORTING
      ttypname        TYPE ddobjname
    RETURNING
      VALUE(rs_dd40v) TYPE dd40v.

  METHODS ddif_dtel_get
    IMPORTING
      rollname TYPE ddobjname
    EXPORTING
      dd04v    TYPE dd04v
      tpara    TYPE tpara.

  METHODS ddif_doma_get
    IMPORTING
      domname      TYPE ddobjname
    RETURNING
      VALUE(dd01v) TYPE dd01v.

  METHODS function_exists
    IMPORTING
      funcname       TYPE rs38l-name
    RETURNING
      VALUE(include) TYPE trdir-name.

  METHODS rs_get_all_includes
    IMPORTING
      progname        TYPE syrepid
    RETURNING
      VALUE(includes) TYPE ty_lt_incl.

  METHODS ddif_typeinfo_get
    IMPORTING
      typename TYPE tabname
    EXPORTING
      typekind TYPE ddtypekind
      gotstate TYPE ddgotstate.

  METHODS ddif_shlp_get
    IMPORTING
      shlpname     TYPE ddobjname
    RETURNING
      VALUE(dd30v) TYPE dd30v.

  METHODS ddif_enqu_get
    IMPORTING
      lockobject   TYPE ddobjname
    RETURNING
      VALUE(dd25v) TYPE dd25v.

  METHODS rs_progname_concatenate
    IMPORTING
      VALUE(fugr_group)            TYPE rs38l-area OPTIONAL
      VALUE(fugr_include_number)   TYPE tfdir-include OPTIONAL
      VALUE(sldb_name)             TYPE ldbd-ldbname OPTIONAL
      VALUE(menu_name)             TYPE tstc-tcode OPTIONAL
      VALUE(type_name)             TYPE trdir-name OPTIONAL
      VALUE(mst_name)              TYPE dd02l-tabname OPTIONAL
      VALUE(cntx_name)             TYPE trdir-name OPTIONAL
      VALUE(clas_name)             TYPE seoclass-clsname OPTIONAL
      VALUE(intf_name)             TYPE seoclass-clsname OPTIONAL
    EXPORTING
      VALUE(fugr_progname_group)   TYPE trdir-name
      VALUE(fugr_progname_include) TYPE trdir-name
      VALUE(fugr_progname_top)     TYPE trdir-name
      VALUE(fugr_progname_uxx)     TYPE trdir-name
      VALUE(sldb_progname_db)      TYPE trdir-name
      VALUE(sldb_progname_sel)     TYPE trdir-name
      VALUE(menu_progname)         TYPE trdir-name
      VALUE(type_progname)         TYPE trdir-name
      VALUE(mst_progname)          TYPE trdir-name
      VALUE(cntx_progname)         TYPE trdir-name
      VALUE(intf_progname)         TYPE trdir-name
      VALUE(clas_progname)         TYPE trdir-name
    RAISING
      zcx_envysis.

  "! Converts E071 into TADIR object WI_E071 = { LIMU FUNC GUI_UPLOAD } -> WE_TADIR = { R3TR FUGR SFES }
  "! @parameter wi_e071 | Example: LIMU FUNC GUI_UPLOAD
  "! @parameter we_tadir | Example: R3TR FUGR SFES
  METHODS tr_check_type
    IMPORTING
      wi_e071  TYPE e071
    EXPORTING
      we_tadir TYPE tadir.

  METHODS convert_e071_to_tadir_objkeys
    IMPORTING
      e071_objkeys  TYPE tt_e071_key
    returning
    value(e071_to_tadir_objkeys) TYPE tt_e071_to_tadir_objkey.

  METHODS rs_progname_split
    IMPORTING
      include       TYPE progname
    RETURNING
      VALUE(result) TYPE ty_progname_split_result.

  METHODS select_tdevc
    IMPORTING
      devclasses         TYPE ty_devclasses
    RETURNING
      VALUE(tdevc_lines) TYPE ty_tdevc_lines.

  METHODS select_tadir_1
    IMPORTING
      packages           TYPE zcl_envysis=>ty_packages
    RETURNING
      VALUE(rt_e071_key) TYPE tt_e071_key.

  METHODS select_tadir_2
    IMPORTING
      it_e071_key_new    TYPE tt_e071_key_new
    RETURNING
      VALUE(tadir_lines) TYPE zcl_envysis=>ty_tadir_lines.

  METHODS select_tadir_3
    IMPORTING
      e071_objkey     TYPE zcl_envysis=>ty_e071_objkey
    RETURNING
      VALUE(devclass) TYPE tadir-devclass.

  METHODS get_oo_info_of_include
    IMPORTING
      include     TYPE rseuinc
    RETURNING
      VALUE(clif) TYPE REF TO if_oo_clif_incl_naming.

  METHODS select_wbcrossgt
    IMPORTING
      includes               TYPE ty_lt_incl
    RETURNING
      VALUE(wbcrossgt_lines) TYPE ty_lt_wbcrossgt.

  METHODS select_dd03l
    IMPORTING
      tabname      TYPE wbcrossgt-name
      fieldname    TYPE wbcrossgt-name
    RETURNING
      VALUE(dd03l) TYPE ty_dd03l.

  METHODS get_tcode_required_objects
    IMPORTING
      i_tcode       TYPE tcode
    RETURNING
      VALUE(result) TYPE ty_tcode_info.

  METHODS get_objects_needed_by_package
    IMPORTING
      i_package_name TYPE devclass
    RETURNING
      VALUE(result)  TYPE ty_devc_required_objects.

  METHODS select_cross
    IMPORTING
      includes           TYPE ty_lt_incl
    RETURNING
      VALUE(cross_lines) TYPE ty_lt_cross.

  METHODS select_trdir
    IMPORTING
      progname      TYPE syrepid
    RETURNING
      VALUE(r_subc) TYPE trdir-subc.

ENDINTERFACE.

# envysis
Environment Analysis for ABAP Workbench Objects (what objects are used by a given object)

Example:
```
" Build list of objects that are used by objects of packages ZPROJECT1 and ZPROJECT2.
" Do not analyze the used objects of standard objects (= exclude all except HOME/LOCAL)
DATA(analyzer) = NEW zcl_envysis( ).
analyzer->get_package_used_objects(
  EXPORTING
    packages      = VALUE zcl_envysis=>ty_packages( sign = 'I' option = 'CP' 
                      ( low = 'ZPROJECT1*' )
                      ( low = 'ZPROJECT2*' ) )
    excluding     = VALUE #( software_components =
                      VALUE #( sign = 'E' option = 'EQ' ( low = 'HOME' ) ( low = 'LOCAL' ) ) )
  IMPORTING
    all           = DATA(all)
    tadir_lines_2 = DATA(tadir_lines_2) ).
```

# Installation
requires https://github.com/sandraros/tcode

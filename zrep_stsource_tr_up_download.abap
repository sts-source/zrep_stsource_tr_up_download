
REPORT zrep_stsource_tr_up_download.



CLASS lcx_exception DEFINITION INHERITING FROM cx_dynamic_check.

  PUBLIC SECTION.
    METHODS constructor IMPORTING !message  TYPE string         OPTIONAL
                                  !previous TYPE REF TO cx_root OPTIONAL.

    METHODS if_message~get_longtext REDEFINITION.
    METHODS if_message~get_text     REDEFINITION.

  PROTECTED SECTION.
    DATA my_message TYPE string.
ENDCLASS.


CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.

    super->constructor( previous = previous ).
    my_message = message.

  ENDMETHOD.

  METHOD if_message~get_longtext.
    IF my_message IS NOT INITIAL.
      result = my_message.
      RETURN.
    ENDIF.
    result = super->if_message~get_longtext( preserve_newlines = preserve_newlines ).
  ENDMETHOD.

  METHOD if_message~get_text.
    IF my_message IS NOT INITIAL.
      result = my_message.
      RETURN.
    ENDIF.
    result = super->if_message~get_text( ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_file_system DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS path_combine
      IMPORTING parts         TYPE string_table
      RETURNING VALUE(result) TYPE string
      RAISING   lcx_exception.

    METHODS file_read ABSTRACT
      IMPORTING filepath      TYPE string
      RETURNING VALUE(result) TYPE xstring
      RAISING   lcx_exception.

    METHODS file_write ABSTRACT
      IMPORTING filepath TYPE string
                content  TYPE xstring
      RAISING   lcx_exception.

    METHODS replace_invalide_chars
      IMPORTING path_or_filename TYPE string
                replace_with     TYPE char1     DEFAULT '_'
                is_filename      TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(result)    TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS class_constructor.

    CLASS-DATA server TYPE REF TO lcl_file_system READ-ONLY.
    CLASS-DATA client TYPE REF TO lcl_file_system READ-ONLY.

  PROTECTED SECTION.
    METHODS get_path_kind ABSTRACT RETURNING VALUE(result) TYPE cl_fs_path=>path_kind_t.
ENDCLASS.


CLASS lcl_file_system_server DEFINITION INHERITING FROM lcl_file_system.
  PUBLIC SECTION.
    METHODS file_read  REDEFINITION.
    METHODS file_write REDEFINITION.

  PROTECTED SECTION.
    METHODS get_path_kind REDEFINITION.
ENDCLASS.


CLASS lcl_file_system_server IMPLEMENTATION.

  METHOD file_read.
    TRY.
        OPEN DATASET filepath FOR INPUT IN BINARY MODE.
        IF sy-subrc = 0.
          READ DATASET filepath INTO result.
          CLOSE DATASET filepath.
        ENDIF.
      CATCH cx_root INTO DATA(exception) ##CATCH_ALL.
        TRY.
            CLOSE DATASET filepath.
          CATCH cx_root ##CATCH_ALL ##NO_HANDLER.
        ENDTRY.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING previous = exception.
    ENDTRY.
  ENDMETHOD.

  METHOD file_write.
    TRY.
        OPEN DATASET filepath FOR OUTPUT IN BINARY MODE.
        TRANSFER content TO filepath.
        CLOSE DATASET filepath.
      CATCH cx_root INTO DATA(exception) ##CATCH_ALL.
        TRY.
            CLOSE DATASET filepath.
          CATCH cx_root ##CATCH_ALL ##NO_HANDLER.
        ENDTRY.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING previous = exception.
    ENDTRY.
  ENDMETHOD.

  METHOD get_path_kind.
    result = cl_fs_path=>path_kind_from_opsys( opsys = sy-opsys ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_file_system_client DEFINITION INHERITING FROM lcl_file_system.
  PUBLIC SECTION.
    METHODS file_read  REDEFINITION.
    METHODS file_write REDEFINITION.

  PROTECTED SECTION.
    DATA my_path_kind LIKE cl_fs_path=>path_kind_windows.

    METHODS get_path_kind REDEFINITION.

ENDCLASS.


CLASS lcl_file_system_client IMPLEMENTATION.

  METHOD file_read.
    DATA(bin_tab) = VALUE solix_tab( ).
    DATA(length) = 0.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = filepath            " Name der Datei
                                                     filetype   = 'BIN'            " Dateityp (Ascii, Binär)
                                          IMPORTING  filelength = length                 " Dateilänge
                                          CHANGING   data_tab   = bin_tab
                                          EXCEPTIONS OTHERS     = 99 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING message = |Can´t upload file. [{ filepath }]|.
    ENDIF.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING  input_length = length
      IMPORTING  buffer       = result
      TABLES     binary_tab   = bin_tab
      EXCEPTIONS failed       = 1
                 OTHERS       = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING message = |Can´t upload file. [{ filepath }]|.
    ENDIF.
  ENDMETHOD.

  METHOD file_write.
    DATA(bin_tab) = VALUE solix_tab( ).

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING buffer     = content
      TABLES    binary_tab = bin_tab.

    cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize = xstrlen( content )                      " Dateilänge bei Binärdateien
                                                       filename     = filepath                     " Name der Datei
                                                       filetype     = 'BIN'                " Dateityp (Ascii, Binär, ...)
                                            CHANGING   data_tab     = bin_tab                     " Übergabetabelle
                                            EXCEPTIONS OTHERS       = 99 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING message = |Can´t download file. [{ filepath }]|.
    ENDIF.

  ENDMETHOD.

  METHOD get_path_kind.
    IF my_path_kind IS INITIAL.
      "  Client OS ******************************************
      cl_gui_frontend_services=>get_platform( RECEIVING  platform             = DATA(client_platform)    " Gibt die Plattform zurück
                                              EXCEPTIONS error_no_gui         = 1
                                                         cntl_error           = 2
                                                         not_supported_by_gui = 3
                                                         OTHERS               = 4 ).
      IF sy-subrc = 0.
        " Logic From FUMO: FILE_GET_NAME
        IF 1 = 2.
          CALL FUNCTION 'FILE_GET_NAME'
            EXPORTING logical_filename = ' '.
        ENDIF.

        CASE client_platform.
          WHEN 1 OR 2 OR 3 OR 4 OR 5 OR 14 OR 15 OR 16 OR 17. " Windows all versions
            my_path_kind = cl_fs_path=>path_kind_windows.
          WHEN OTHERS.
            my_path_kind = cl_fs_path=>path_kind_unix.
*        WHEN 6 OR 13.                                         " MAC
*          path_kind = cl_fs_path=>path_kind_unix.
*        WHEN 7.                                               " OS/2
*
*        WHEN 8 OR 9 OR 10 OR 11 OR 12.                      " UNIX/LINUX
*          path_kind = cl_fs_path=>path_kind_unix.
        ENDCASE.

      ENDIF.
    ENDIF.
    result = my_path_kind.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_file_system IMPLEMENTATION.

  METHOD class_constructor.
    server = NEW lcl_file_system_server( ).
    client = NEW lcl_file_system_client( ).
  ENDMETHOD.

  METHOD replace_invalide_chars.

    TRY.

        FIELD-SYMBOLS <lc> TYPE xstring.

        result = path_or_filename.

        IF is_filename = abap_false.
          " Source: Microsoft .NET C# ... System.IO.Path.GetInvalidPathChars()
          CONSTANTS c_chars_p TYPE xstring VALUE '0102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F223C3E7C'.

          ASSIGN c_chars_p TO <lc>.
        ELSE.
          " Source: Microsoft .NET C# ... System.IO.Path.GetInvalidFileNameChars()
          CONSTANTS c_chars_f TYPE xstring
                              VALUE '0102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F222A2F3A3C3E3F5C7C'.

          ASSIGN c_chars_f TO <lc>.
        ENDIF.

        IF result IS NOT INITIAL.
          DO xstrlen( <lc> ) TIMES.
            DATA lx4    TYPE x LENGTH 4.
            DATA l_idx  TYPE i.
            DATA l_i    TYPE i.
            DATA l_char TYPE string.

            lx4+3(1) = <lc>+l_idx(1).
            l_i = lx4.
            l_char = cl_abap_conv_in_ce=>uccpi( l_i ).
            REPLACE ALL OCCURRENCES OF l_char IN result WITH replace_with.
            l_idx = l_idx + 1.
          ENDDO.
        ENDIF.

      CATCH cx_root INTO DATA(exception) ##CATCH_ALL.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING previous = exception.
    ENDTRY.
  ENDMETHOD.

  METHOD path_combine.

    LOOP AT parts INTO DATA(part).
      IF sy-tabix = 1.
        DATA(path_api) = cl_fs_path=>create( name           = part
                                             force_absolute = abap_false
                                             path_kind      = get_path_kind( ) ).
        CONTINUE.
      ENDIF.
      path_api->append_path_name( name = part ).
    ENDLOOP.
    IF path_api IS BOUND.
      result = path_api->get_path_name( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_dev_tr DEFINITION.

  PUBLIC SECTION.
    TYPES ty_req_type TYPE trreqtype.

    CLASS-METHODS s_tr_append_to_queue
      IMPORTING trkorr           TYPE trkorr
                popup_to_confirm TYPE abap_bool DEFAULT abap_true
      RAISING   lcx_exception.

    CLASS-METHODS s_tr_as_zip_export
      IMPORTING trkorr        TYPE trkorr
      RETURNING VALUE(result) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS s_tr_as_zip_import
      IMPORTING zip_content             TYPE xstring
                popup_to_confirm_append TYPE abap_bool DEFAULT abap_true
      RAISING   lcx_exception.

    CLASS-METHODS s_tr_as_zip_popup_download
      IMPORTING VALUE(trkorr) TYPE trkorr OPTIONAL
      RAISING   lcx_exception.

    CLASS-METHODS s_tr_as_zip_popup_upload
      RAISING lcx_exception.

    CLASS-METHODS s_tr_choice_dialog
      RETURNING VALUE(result) TYPE trkorr
      RAISING   lcx_exception.

    CLASS-METHODS s_tr_download
      IMPORTING trkorr   TYPE trkorr
                dir_path TYPE csequence
      RAISING   lcx_exception.

    CLASS-METHODS s_tr_get_description
      IMPORTING trkorr        TYPE trkorr
      RETURNING VALUE(result) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS s_tr_path
      RETURNING VALUE(result) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS s_tr_path_cofiles
      RETURNING VALUE(result) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS s_tr_path_data
      RETURNING VALUE(result) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS s_tr_upload
      IMPORTING trkorr   TYPE trkorr
                dir_path TYPE csequence
      RAISING   lcx_exception.
ENDCLASS.


CLASS lcl_dev_tr IMPLEMENTATION.

  METHOD s_tr_append_to_queue.

    " Created: ©2019 - Stefan Schwab

    IF popup_to_confirm = abap_true.
      DATA(answer) = VALUE char1( ).
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING  text_question  = |Do you want to append the { trkorr } order to the queue?|
        IMPORTING  answer         = answer
        EXCEPTIONS text_not_found = 1
                   OTHERS         = 2.
      IF answer <> '1'.
        RETURN.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'TMS_MGR_FORWARD_TR_REQUEST'
      EXPORTING  iv_request                 = trkorr    " transport request
                 iv_target                  = CONV tmscsys-sysnam( sy-sysid )   " target system
      EXCEPTIONS read_config_failed         = 1
                 table_of_requests_is_empty = 2
                 OTHERS                     = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING message = |TMS_MGR_FORWARD_TR_REQUEST Call fail|.
    ENDIF.

  ENDMETHOD.

  METHOD s_tr_as_zip_export.

    " Created: ©2019 - Stefan Schwab

    DATA rfile TYPE string.
    DATA kfile TYPE string.

    " --------------------------------[ B O D Y ]---------------------------------------

    IF trkorr+3(1) = 'K'.
      DATA sid  TYPE char3.
      DATA tonr TYPE string.

      sid = trkorr(3).
      tonr = trkorr+4.
      " R-FilePath (DATA)
      rfile = |R{ tonr }.{ sid }|.
      " K-FilePath (COFILES)
      kfile = |K{ tonr }.{ sid }|.
    ELSE.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING message = |Wrong trkorr!|.
    ENDIF.

    TRY.

        DATA src_data_filename TYPE string.
        DATA src_co_filename   TYPE string.
        DATA lo_zip            TYPE REF TO cl_abap_zip.

        " ------------------------------ Create FilePath´s
        " System R-FilePath (DATA)
        src_data_filename = lcl_file_system=>server->path_combine( VALUE #( ( s_tr_path_data( ) )
                                                                            ( rfile ) ) ).
        " System K-FilePath (COFILES)
        src_co_filename = lcl_file_system=>server->path_combine( VALUE #( ( s_tr_path_cofiles( ) )
                                                                          ( kfile ) ) ).

        " ------------------------------ ZIP Files
        lo_zip = NEW #( ).
        lo_zip->add( name    = rfile
                     content = lcl_file_system=>server->file_read( src_data_filename ) ).

        lo_zip->add( name    = kfile
                     content = lcl_file_system=>server->file_read( src_co_filename ) ).

        result = lo_zip->save( ).

      CATCH cx_root INTO DATA(exception).
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING previous = exception.
    ENDTRY.

  ENDMETHOD.

  METHOD s_tr_as_zip_import.

    " Created: ©2019 - Stefan Schwab

    TYPES: BEGIN OF lty_s_trkorr_file,
             trkorr      TYPE trkorr,
             r_file      TYPE xstring,
             r_file_name TYPE string,
             k_file      TYPE xstring,
             k_file_name TYPE string,
           END OF lty_s_trkorr_file.

    " --------------------------------[ B O D Y ]---------------------------------------

    TRY.

        DATA zip_archive    TYPE REF TO cl_abap_zip.
        DATA zip_file_entry TYPE LINE OF cl_abap_zip=>t_files.
        DATA trkorr         TYPE trkorr.
        DATA trkorr_file    TYPE REF TO lty_s_trkorr_file.
        DATA trkorr_files   TYPE HASHED TABLE OF lty_s_trkorr_file WITH UNIQUE KEY trkorr.

        zip_archive = NEW cl_abap_zip( ).
        zip_archive->load( EXPORTING  zip             = zip_content
                           EXCEPTIONS zip_parse_error = 1
                                      OTHERS          = 2 ).
        IF sy-subrc <> 0.

        ENDIF.

        LOOP AT zip_archive->files INTO zip_file_entry.

          DATA file TYPE string.
          DATA len  TYPE i.

          file = zip_file_entry-name.
          len = strlen( file ).
          IF len > 11.
            len = len - 12.
            IF file+len(1) CO '/\'.
              len = len + 1.
              file = file+len.
            ENDIF.
          ENDIF.

          file = to_upper( file ).

          IF NOT (     strlen( file )  = 11
                   AND file+1(6)      CO '0123456789'
                   AND file+7(1)       = '.' ).
            CONTINUE.
          ENDIF.

          trkorr(3)   = file+8.
          trkorr+3(1) = 'K'.
          trkorr+4(6) = file+1(6).

          trkorr_file = REF #( trkorr_files[ trkorr = trkorr ] OPTIONAL ).
          IF trkorr_file IS NOT BOUND.
            INSERT VALUE #( trkorr = trkorr ) INTO TABLE trkorr_files REFERENCE INTO trkorr_file.
          ENDIF.

          IF file(1) = `K`.

            trkorr_file->k_file_name = file.

            zip_archive->get( EXPORTING  name    = zip_file_entry-name
                              IMPORTING  content = trkorr_file->k_file
                              EXCEPTIONS OTHERS  = 99 ).
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE lcx_exception
                EXPORTING message = |can´t find { zip_file_entry-name } in ZipFile|.
            ENDIF.

          ELSEIF file(1) = `R`.

            trkorr_file->r_file_name = file.

            zip_archive->get( EXPORTING  name    = zip_file_entry-name
                              IMPORTING  content = trkorr_file->r_file
                              EXCEPTIONS OTHERS  = 99 ).
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE lcx_exception
                EXPORTING message = |can´t find { zip_file_entry-name } in ZipFile|.
            ENDIF.

          ENDIF.
        ENDLOOP.

        CLEAR zip_archive.

        LOOP AT trkorr_files REFERENCE INTO trkorr_file.
          IF    trkorr_file->k_file_name IS INITIAL
             OR trkorr_file->k_file      IS INITIAL
             OR trkorr_file->r_file_name IS INITIAL
             OR trkorr_file->r_file      IS INITIAL.
            DELETE TABLE trkorr_files WITH TABLE KEY trkorr = trkorr_file->trkorr.
          ENDIF.
        ENDLOOP.

        IF trkorr_files[] IS INITIAL.
          RAISE EXCEPTION TYPE lcx_exception
            EXPORTING message = |Wrong Zip File! Can´t find K or R File|.
        ENDIF.

        LOOP AT trkorr_files REFERENCE INTO trkorr_file.

          " ------------------------------ Create FilePath´s
          "   System R-FilePath (DATA)
          DATA(r_filepath) = lcl_file_system=>server->path_combine( VALUE #( (  s_tr_path_data( ) )
                                                                             (  trkorr_file->r_file_name ) ) ).
          "   System K-FilePath (COFILES)
          DATA(k_filepath) = lcl_file_system=>server->path_combine( VALUE #( ( s_tr_path_cofiles( ) )
                                                                             ( trkorr_file->k_file_name ) ) ).

          " ------------------------------ write Files
          lcl_file_system=>server->file_write( filepath = r_filepath
                                               content  = trkorr_file->r_file ).

          lcl_file_system=>server->file_write( filepath = k_filepath
                                               content  = trkorr_file->k_file ).

          " ------------------------------ Append to Queue
          s_tr_append_to_queue( trkorr           = trkorr_file->trkorr
                                popup_to_confirm = popup_to_confirm_append ).

        ENDLOOP.

      CATCH cx_root INTO DATA(exception).
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING previous = exception.
    ENDTRY.

  ENDMETHOD.

  METHOD s_tr_as_zip_popup_download.

    " Created: ©2019 - Stefan Schwab

    DATA path        TYPE string.
    DATA filename    TYPE string.
    DATA fullpath    TYPE string.
    DATA user_action LIKE cl_gui_frontend_services=>action_ok.

    " --------------------------------[ B O D Y ]---------------------------------------

    IF trkorr IS INITIAL.
      trkorr = s_tr_choice_dialog( ).
    ENDIF.

    IF trkorr IS INITIAL.
      RETURN.
    ENDIF.

    path = trkorr.
    filename = s_tr_get_description( trkorr = trkorr ).
    REPLACE ALL OCCURRENCES OF '/' IN filename WITH '#'.
    filename = lcl_file_system=>client->replace_invalide_chars( path_or_filename = filename
                                                                is_filename      = abap_true ).
    filename = |{ path } { filename }.zip|.

    CLEAR path.

    cl_gui_frontend_services=>file_save_dialog( EXPORTING  default_file_name = filename    " Vorschlagsdateiname
                                                           file_filter       = 'Zip Files (*.zip)|*.zip|'    " Filter Tabelle für Dateityp
                                                CHANGING   filename          = filename    " Dateiname für Sichern
                                                           path              = path    " Pfad zu Datei
                                                           fullpath          = fullpath    " Pfad + Dateiname
                                                           user_action       = user_action    " Benutzeraktion ( K.Konst. ACTION_OK, ACTION_OVERWRITE usw.)
                                                EXCEPTIONS OTHERS            = 99 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING message = |Error on cl_gui_frontend_services=>file_save_dialog|.
    ENDIF.

    IF user_action = cl_gui_frontend_services=>action_ok.
      lcl_file_system=>client->file_write( filepath = fullpath
                                           content  = s_tr_as_zip_export( trkorr ) ).
    ENDIF.

  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_DEV_TR=>S_TR_AS_ZIP_POPUP_UPLOAD
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] lcx_exception
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD s_tr_as_zip_popup_upload.

    " Created: ©2019 - Stefan Schwab

    DATA lt_file_table TYPE filetable.
    DATA rc            TYPE i.
    DATA user_action   TYPE i.

    " --------------------------------[ B O D Y ]---------------------------------------

    cl_gui_frontend_services=>file_open_dialog( EXPORTING  file_filter    = `Zip Files (*.zip)|*.zip|`    " Filterstring für Dateierweiterung
                                                           multiselection = abap_false    " Mehrfachselektion möglich
                                                CHANGING   file_table     = lt_file_table    " Tabelle, die selektierte Dateien enthält
                                                           rc             = rc    " Rückgabewert: Anzahl Dateien oder -1 falls Fehler auftritt
                                                           user_action    = user_action    " Benutzeraktion( s. Kl.konstanten ACTION_OK, ACTION_CANCEL)
                                                EXCEPTIONS OTHERS         = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING message = |Error on cl_gui_frontend_services=>file_open_dialog|.
    ENDIF.

    IF user_action = cl_gui_frontend_services=>action_ok AND rc = 1.
      DATA ls_file TYPE LINE OF filetable.

      READ TABLE lt_file_table INTO ls_file INDEX 1.
      s_tr_as_zip_import( lcl_file_system=>client->file_read( CONV #( ls_file-filename ) ) ).
    ENDIF.

  ENDMETHOD.

  METHOD s_tr_choice_dialog.

    " Created: ©2019 - Stefan Schwab

    CALL FUNCTION 'TR_POPUP_INPUT_REQUEST'
      EXPORTING  iv_trfunctions         = 'KWT'
                 iv_trstatus            = 'R'
      IMPORTING  ev_trkorr              = result
      EXCEPTIONS action_aborted_by_user = 1
                 OTHERS                 = 2.

    IF result CA '*+'.
      CLEAR result.
    ENDIF.

* Infos

* FBs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*TR_POPUP_INPUT_REQUEST
*TR_PRESENT_REQUEST
*TRINT_SELECT_REQUESTS
*TR_SEARCH_AND_DISPLAY_REQUESTS
*TR_F4_REQUESTS

*********************************************

* Status aus Domäne TRSTATUS
*D  Änderbar
*L  Änderbar, geschützt
*O  Freigabe gestartet
*R  Freigegeben
*N  Freigegeben (Importschutz für reparierte Objekte aktiv)
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Domäne TRFUNCTION
*K  Workbench-Auftrag
*W  Customizing-Auftrag
*C  Umzug von Objekten ohne Paketwechsel
*O  Umzug von Objekten mit Paketwechsel
*E  Umzug eines kompletten Pakets
*T  Transport von Kopien
*S  Entwicklung/Korrektur
*R  Reparatur
*X  Unklassifizierte Aufgabe
*Q  Customizing-Aufgabe
*G  Stückliste für CTS-Projekt
*M  Auftrag für Mandantentransport
*P  Stückliste für Upgrade
*D  Stückliste für Patch
*F  Stückliste
*L  Löschtransport
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ENDMETHOD.

  METHOD s_tr_download.

    " Created: ©2019 - Stefan Schwab

    DATA rfile             TYPE string.
    DATA kfile             TYPE string.
    DATA dst_data_filename TYPE string.
    DATA dst_co_filename   TYPE string.
    DATA src_data_filename TYPE string.
    DATA src_co_filename   TYPE string.

    " --------------------------------[ B O D Y ]---------------------------------------

    IF trkorr+3(1) = 'K'.
      DATA sid  TYPE char3.
      DATA tonr TYPE string.

      sid = trkorr(3).
      tonr = trkorr+4.
      " R-FilePath (DATA)
      rfile = |R{ tonr }.{ sid }|.
      " K-FilePath (COFILES)
      kfile = |K{ tonr }.{ sid }|.
    ELSE.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING message = |Wrong trkorr!|.
    ENDIF.

    " ------------------------------ Create FilePath´s

    " Local R-FilePath (DATA)
    dst_data_filename = lcl_file_system=>client->path_combine( VALUE #( ( dir_path )
                                                                        ( rfile ) ) ).
    " Local K-FilePath (COFILES)
    dst_co_filename = lcl_file_system=>client->path_combine( VALUE #( ( dir_path )
                                                                      ( kfile ) ) ).

    " System R-FilePath (DATA)
    src_data_filename = lcl_file_system=>server->path_combine( VALUE #( ( s_tr_path_data( ) )
                                                                        ( rfile ) ) ).
    " System K-FilePath (COFILES)
    src_co_filename = lcl_file_system=>server->path_combine( VALUE #( ( s_tr_path_cofiles( ) )
                                                                      ( kfile ) ) ).

    " ------------------------------ Files Download

    lcl_file_system=>client->file_write( filepath = dst_data_filename
                                         content  = lcl_file_system=>server->file_read( src_data_filename ) ).

    lcl_file_system=>client->file_write( filepath = dst_co_filename
                                         content  = lcl_file_system=>server->file_read( src_co_filename ) ).

  ENDMETHOD.

  METHOD s_tr_get_description.

    " Created: ©2019 - Stefan Schwab

    DATA cs_request TYPE trwbo_request_header.

    cs_request-trkorr = trkorr.

    CALL FUNCTION 'TRINT_READ_REQUEST_HEADER'
      EXPORTING  iv_read_e07t   = 'X'
      CHANGING   cs_request     = cs_request
      EXCEPTIONS empty_trkorr   = 1
                 not_exist_e070 = 2
                 OTHERS         = 3.
    IF sy-subrc = 0.
      result = cs_request-as4text.
    ENDIF.

  ENDMETHOD.

  METHOD s_tr_path.

    " Created: ©2019 - Stefan Schwab

    STATICS dir TYPE REF TO string.

    IF dir IS NOT BOUND.
      DATA(value) = VALUE spfl_parameter_value( ).

      DATA(rc) = cl_spfl_profile_parameter=>get_value( EXPORTING name  = CONV #( 'DIR_TRANS' )
                                                       IMPORTING value = value ).
      IF rc <> 0.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING message = |can´t read DIR_TRANS|.
      ENDIF.

      dir = NEW #( value ).
    ENDIF.

    result = dir->*.

  ENDMETHOD.

  METHOD s_tr_path_cofiles.

    " Created: ©2019 - Stefan Schwab

    result = lcl_file_system=>server->path_combine( VALUE #( ( s_tr_path( ) )
                                                             ( `cofiles` ) ) ).

  ENDMETHOD.

  METHOD s_tr_path_data.

    " Created: ©2019 - Stefan Schwab
    result = lcl_file_system=>server->path_combine( VALUE #( ( s_tr_path( ) )
                                                             ( `data` ) ) ).

  ENDMETHOD.

  METHOD s_tr_upload.

    " Created: ©2019 - Stefan Schwab

    DATA rfile             TYPE string.
    DATA kfile             TYPE string.

    DATA src_data_filename TYPE string.
    DATA src_co_filename   TYPE string.

    DATA dst_data_filename TYPE string.
    DATA dst_co_filename   TYPE string.

    " --------------------------------[ B O D Y ]---------------------------------------

    IF trkorr+3(1) = 'K'.
      DATA sid  TYPE char3.
      DATA tonr TYPE string.

      sid = trkorr(3).
      tonr = trkorr+4.
      " R-FilePath (DATA)
      rfile = |R{ tonr }.{ sid }|.
      " K-FilePath (COFILES)
      kfile = |K{ tonr }.{ sid }|.
    ELSE.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING message = |Wrong trkorr!|.
    ENDIF.

    " ------------------------------ Create FilePath´s
    " local R-FilePath (DATA)
    src_data_filename = lcl_file_system=>client->path_combine( VALUE #( ( dir_path )
                                                                        ( rfile ) ) ).
    " local K-FilePath (COFILES)
    src_co_filename = lcl_file_system=>client->path_combine( VALUE #( ( dir_path )
                                                                      ( kfile ) ) ).

    " System R-FilePath (DATA)
    dst_data_filename = lcl_file_system=>server->path_combine( VALUE #( ( s_tr_path_data( ) )
                                                                        ( rfile ) ) ).
    " System K-FilePath (COFILES)
    dst_co_filename = lcl_file_system=>server->path_combine( VALUE #( ( s_tr_path_cofiles( ) )
                                                                      ( kfile ) ) ).

    " ------------------------------ Files Upload

    lcl_file_system=>server->file_write( filepath = dst_data_filename
                                         content  = lcl_file_system=>client->file_read( src_data_filename ) ).

    lcl_file_system=>server->file_write( filepath = dst_co_filename
                                         content  = lcl_file_system=>client->file_read( src_co_filename ) ).

    " ------------------------------ Append to Queue

    s_tr_append_to_queue( trkorr ).

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN SKIP 4.
SELECTION-SCREEN PUSHBUTTON 20(30) push_d USER-COMMAND b_d.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN PUSHBUTTON 20(30) push_u USER-COMMAND b_u.

************************************************************
******************* INITIALIZATION *************************
************************************************************
INITIALIZATION.

  push_d = |{ icon_system_save } TR Zip Download|. " icon_last_page
  push_u = |{ icon_transport   } TR Zip Upload|.   " icon_first_page

  DATA it_ucomm TYPE STANDARD TABLE OF sy-ucomm WITH EMPTY KEY.
  " Remove the standard Execute button (F8) in the toolbar
  APPEND 'ONLI' TO it_ucomm.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING p_status  = sy-pfkey
    TABLES    p_exclude = it_ucomm.

************************************************************
****************** AT SELECTION-SCREEN *********************
************************************************************
AT SELECTION-SCREEN.

  TRY.

      CASE sy-ucomm.
        WHEN 'B_D'.
          lcl_dev_tr=>s_tr_as_zip_popup_download( ).
        WHEN 'B_U'.
          lcl_dev_tr=>s_tr_as_zip_popup_upload( ).
        WHEN OTHERS.
      ENDCASE.

    CATCH cx_root INTO DATA(exception).
      MESSAGE exception->if_message~get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

CLASS zcl_btocs_ollama_connector DEFINITION
  PUBLIC
  INHERITING FROM zcl_btocs_rws_connector
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_btocs_ollama_connector .

    CLASS-METHODS create
      RETURNING
        VALUE(rr_instance) TYPE REF TO zif_btocs_ollama_connector .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BTOCS_OLLAMA_CONNECTOR IMPLEMENTATION.


  METHOD create.
    rr_instance ?= zcl_btocs_factory=>create_instance( 'ZIF_BTOCS_OLLAMA_CONNECTOR' ).
  ENDMETHOD.


  METHOD zif_btocs_ollama_connector~api_generate.

* ========== init
    DATA(ls_params) = is_params.
    ro_response     = zcl_btocs_factory=>create_web_service_response( ).
    ro_response->set_logger( get_logger( ) ).


* =========== checks and preparations
    IF zif_btocs_ollama_connector~is_initialized( ) EQ abap_false.
      ro_response->set_reason( |connector is not initialized| ).
      RETURN.
    ENDIF.

    IF ls_params-prompt IS INITIAL.
      ro_response->set_reason( |prompt is missing| ).
      RETURN.
    ENDIF.

    IF ls_params-model IS INITIAL.
      ro_response->set_reason( |target language is missing| ).
      RETURN.
    ENDIF.

* ----------- transformation line breaks
    zcl_btocs_factory=>create_text_util( )->replace_eol_with_esc_n( CHANGING cv_text = ls_params-prompt ).
    zcl_btocs_factory=>create_text_util( )->replace_tab_with_esc_t( CHANGING cv_text = ls_params-prompt ).

    zcl_btocs_factory=>create_text_util( )->replace_eol_with_esc_n( CHANGING cv_text = ls_params-sys_prompt ).
    zcl_btocs_factory=>create_text_util( )->replace_tab_with_esc_t( CHANGING cv_text = ls_params-sys_prompt ).


* =========== fill form based params
    DATA(lo_request) = zif_btocs_ollama_connector~new_request( ). " from current client

    DATA(lo_json) = lo_request->new_json_object( ).
    DATA(lo_mgr)  = lo_json->get_manager( ).

*   hidden params
    lo_json->set(
        iv_name      = zif_btocs_ollama_connector=>c_json_key-stream
        io_value     = lo_mgr->new_boolean( abap_false )
    ).

*   main params
    lo_json->set(
        iv_name      = zif_btocs_ollama_connector=>c_json_key-model
        io_value     = lo_mgr->new_string( ls_params-model )
    ).

    lo_json->set(
        iv_name      = zif_btocs_ollama_connector=>c_json_key-role
        io_value     = lo_mgr->new_string( ls_params-role )
    ).

    lo_json->set(
        iv_name      = zif_btocs_ollama_connector=>c_json_key-prompt
        io_value     = lo_mgr->new_string( ls_params-prompt )
    ).

* optional params
    IF ls_params-template IS NOT INITIAL.
      lo_json->set(
          iv_name      = zif_btocs_ollama_connector=>c_json_key-template
          io_value     = lo_mgr->new_string( ls_params-template )
      ).
    ENDIF.

    IF ls_params-sys_prompt IS NOT INITIAL.
      lo_json->set(
          iv_name      = zif_btocs_ollama_connector=>c_json_key-sys_prompt
          io_value     = lo_mgr->new_string( ls_params-sys_prompt )
      ).
    ENDIF.

    IF ls_params-context IS NOT INITIAL.
      lo_json->set(
          iv_name      = zif_btocs_ollama_connector=>c_json_key-context
          io_value     = lo_mgr->new_string( ls_params-context )
      ).
    ENDIF.

* image
    IF ls_params-image IS NOT INITIAL.
      DATA(lo_convert) = zcl_btocs_factory=>create_convert_util( ).
      DATA(lv_base64) = lo_convert->encode_xstring_to_base64( ls_params-image ).
      IF lv_base64 IS INITIAL.
        get_logger( )->error( |convert image data to base64 failed| ).
      ELSE.
        DATA(lo_array) = lo_mgr->new_json_array( ).
        lo_array->add( lo_mgr->new_string( lv_base64 ) ).
        lo_json->set(
            iv_name      = zif_btocs_ollama_connector=>c_json_key-images
            io_value     = lo_array
        ).
      ENDIF.
    ENDIF.



* ============ execute via api path
    DATA(lo_response) = zif_btocs_ollama_connector~new_response( ).
    ro_response ?= zif_btocs_ollama_connector~execute(
     iv_api_path = zif_btocs_ollama_c=>api_path-generate
     io_response = lo_response
    ).

* ----- parse?
    IF iv_parse EQ abap_true.
      es_result = zif_btocs_ollama_connector~parse_response_generate( ro_response ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_btocs_ollama_connector~parse_response_generate.

* ------ check
    IF io_response IS INITIAL
      OR io_response->is_json_object( ) EQ abap_false.
      get_logger( )->error( |invalid generate response| ).
      RETURN.
    ENDIF.

* ------- transform
    TRY.
        DATA(lo_parsed) = io_response->get_values_from_parsed_json( ).
        DATA(lo_answer)   = lo_parsed->get_structure_value( ).

        IF lo_answer IS INITIAL.
          get_logger( )->error( |no generate answer structure found| ).
        ELSE.
          rs_result-response = lo_answer->get_string( zif_btocs_ollama_connector~c_json_key-response ).
          rs_result-model    = lo_answer->get_string( zif_btocs_ollama_connector~c_json_key-model ).
          rs_result-context  = lo_answer->get_string( zif_btocs_ollama_connector~c_json_key-context ).
        ENDIF.
      CATCH cx_root INTO DATA(lx_exc).
        DATA(lv_error) = lx_exc->get_text( ).
        get_logger( )->error( lv_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_btocs_ollama_connector~api_tags.
* ========== init
    ro_response     = zcl_btocs_factory=>create_web_service_response( ).
    ro_response->set_logger( get_logger( ) ).

* =========== checks and preparations
    IF zif_btocs_ollama_connector~is_initialized( ) EQ abap_false.
      ro_response->set_reason( |connector is not initialized| ).
      RETURN.
    ENDIF.

* ============ execute via api path
    DATA(lo_response) = zif_btocs_ollama_connector~new_response( ).
    ro_response ?= zif_btocs_ollama_connector~execute(
     iv_api_path = zif_btocs_ollama_c=>api_path-tags
     io_response = lo_response
     iv_method   = 'GET'
    ).

* ============ parse
    IF iv_parse = abap_true.
      et_result = zif_btocs_ollama_connector~parse_response_tags( ro_response ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_btocs_ollama_connector~api_show.

* ========== init
    ro_response     = zcl_btocs_factory=>create_web_service_response( ).
    ro_response->set_logger( get_logger( ) ).


* =========== checks and preparations
    IF zif_btocs_ollama_connector~is_initialized( ) EQ abap_false.
      ro_response->set_reason( |connector is not initialized| ).
      RETURN.
    ENDIF.

    IF iv_model IS INITIAL.
      ro_response->set_reason( |model is missing| ).
      RETURN.
    ENDIF.


* =========== fill form based params
    DATA(lo_request) = zif_btocs_ollama_connector~new_request( ). " from current client

    DATA(lo_json) = lo_request->new_json_object( ).
    DATA(lo_mgr)  = lo_json->get_manager( ).

    lo_json->set(
        iv_name      = zif_btocs_ollama_connector=>c_json_key-model
        io_value     = lo_mgr->new_string( iv_model )
    ).

* ============ execute via api path
    DATA(lo_response) = zif_btocs_ollama_connector~new_response( ).
    ro_response ?= zif_btocs_ollama_connector~execute(
     iv_api_path = zif_btocs_ollama_c=>api_path-show
     io_response = lo_response
    ).

* ----- parse?
    IF iv_parse EQ abap_true.
      es_result = zif_btocs_ollama_connector~parse_response_show( ro_response ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_btocs_ollama_connector~api_embeddings.
* ========== init
    DATA(ls_params) = is_params.
    ro_response     = zcl_btocs_factory=>create_web_service_response( ).
    ro_response->set_logger( get_logger( ) ).


* =========== checks and preparations
    IF zif_btocs_ollama_connector~is_initialized( ) EQ abap_false.
      ro_response->set_reason( |connector is not initialized| ).
      RETURN.
    ENDIF.

    IF ls_params-prompt IS INITIAL.
      ro_response->set_reason( |prompt is missing| ).
      RETURN.
    ENDIF.

    IF ls_params-model IS INITIAL.
      ro_response->set_reason( |target language is missing| ).
      RETURN.
    ENDIF.

* ----------- transformation line breaks
    zcl_btocs_factory=>create_text_util( )->replace_eol_with_esc_n( CHANGING cv_text = ls_params-prompt ).
    zcl_btocs_factory=>create_text_util( )->replace_tab_with_esc_t( CHANGING cv_text = ls_params-prompt ).



* =========== fill form based params
    DATA(lo_request) = zif_btocs_ollama_connector~new_request( ). " from current client

    DATA(lo_json) = lo_request->new_json_object( ).
    DATA(lo_mgr)  = lo_json->get_manager( ).

*   main params
    lo_json->set(
        iv_name      = zif_btocs_ollama_connector=>c_json_key-model
        io_value     = lo_mgr->new_string( ls_params-model )
    ).

    lo_json->set(
        iv_name      = zif_btocs_ollama_connector=>c_json_key-prompt
        io_value     = lo_mgr->new_string( ls_params-prompt )
    ).


* ============ execute via api path
    DATA(lo_response) = zif_btocs_ollama_connector~new_response( ).
    ro_response ?= zif_btocs_ollama_connector~execute(
     iv_api_path = zif_btocs_ollama_c=>api_path-embeddings
     io_response = lo_response
    ).

* ----- parse?
    IF iv_parse EQ abap_true.
      et_embedding = zif_btocs_ollama_connector~parse_response_embedding(
      EXPORTING
        io_response = ro_response
      IMPORTING
        ev_array    = ev_embedding
      ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_btocs_ollama_connector~parse_response_embedding.
* ------ check
    IF io_response IS INITIAL
      OR io_response->is_json_object( ) EQ abap_false.
      get_logger( )->error( |invalid tags response| ).
      RETURN.
    ENDIF.

* ------- transform
    TRY.
        DATA(lo_parsed) = io_response->get_values_from_parsed_json( ).

        DATA(lo_answer)   = lo_parsed->get_structure_value( ).
        IF lo_answer IS INITIAL.
          get_logger( )->error( |no embeddings answer structure found| ).
          RETURN.
        ENDIF.

        DATA(lo_embeddings) = lo_answer->get( zif_btocs_ollama_connector~c_json_key-embedding ).
        IF lo_embeddings IS INITIAL.
          get_logger( )->error( |no embeddings answer structure found| ).
          RETURN.
        ENDIF.

        DATA(lo_array) = lo_embeddings->get_array_value( ).
        DATA(lv_lin)   = lo_array->count( ).

* ----- loop model list
        DO lv_lin TIMES.
          DATA(lo_entry) = lo_array->get( sy-index ).
          DATA(lv_value) = lo_entry->get_string( ).
          APPEND lv_value TO rt_result.
        ENDDO.

* ---- get array string
        DATA(lv_body) = io_response->get_content( ).
        IF lv_body CS '['.
          DATA(lv_from) = sy-fdpos.
          IF lv_body CS ']'.
            DATA(lv_to) = sy-fdpos.
            DATA(lv_len) = lv_to - lv_from + 1.
            IF lv_len > 0.
              ev_array = lv_body+lv_from(lv_len).
            ENDIF.
          ENDIF.
        ENDIF.


* --------- catch errors
      CATCH cx_root INTO DATA(lx_exc).
        DATA(lv_error) = lx_exc->get_text( ).
        get_logger( )->error( lv_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_btocs_ollama_connector~parse_response_show.
* ------ check
    IF io_response IS INITIAL
      OR io_response->is_json_object( ) EQ abap_false.
      get_logger( )->error( |invalid show response| ).
      RETURN.
    ENDIF.

* ------- transform
    TRY.
        DATA(lo_parsed) = io_response->get_values_from_parsed_json( ).

        DATA(lo_answer)   = lo_parsed->get_structure_value( ).
        IF lo_answer IS INITIAL.
          get_logger( )->error( |no tags answer structure found| ).
          RETURN.
        ENDIF.

        rs_result-license = lo_answer->get_string( zif_btocs_ollama_connector~c_json_key-license ).
        rs_result-modelfile = lo_answer->get_string( zif_btocs_ollama_connector~c_json_key-modelfile ).
        rs_result-parameters = lo_answer->get_string( zif_btocs_ollama_connector~c_json_key-parameters ).
        rs_result-template = lo_answer->get_string( zif_btocs_ollama_connector~c_json_key-template ).

*         add details
        DATA(lo_detail_obj) = lo_answer->get( zif_btocs_ollama_connector~c_json_key-details ).
        IF lo_detail_obj IS NOT INITIAL.
          DATA(lo_details) = lo_detail_obj->get_structure_value( ).
          rs_result-details_parent_model = lo_details->get_string( zif_btocs_ollama_connector~c_json_key-parent_model ).
          rs_result-details_format = lo_details->get_string( zif_btocs_ollama_connector~c_json_key-format ).
          rs_result-details_familiy = lo_details->get_string( zif_btocs_ollama_connector~c_json_key-family ).
          rs_result-details_parameter_size = lo_details->get_string( zif_btocs_ollama_connector~c_json_key-parameter_size ).
          rs_result-details_quantization_level = lo_details->get_string( zif_btocs_ollama_connector~c_json_key-quantization_level ).

*         build families array as comma separated string
          DATA(lo_families_obj) = lo_details->get( zif_btocs_ollama_connector~c_json_key-families ).
          IF lo_families_obj IS NOT INITIAL.
            DATA(lo_families) = lo_families_obj->get_array_value( ).
            DATA(lv_lin_fam)  = lo_families->count( ).
            DO lv_lin_fam TIMES.
              DATA(lo_fam_entry) = lo_families->get( sy-index ).
              DATA(lv_fam_value) = lo_fam_entry->get_string( ).
              IF lv_fam_value IS NOT INITIAL.
                IF rs_result-details_families IS INITIAL.
                  rs_result-details_families = lv_fam_value.
                ELSE.
                  rs_result-details_families = |{ rs_result-details_families }, { lv_fam_value }|.
                ENDIF.
              ENDIF.
            ENDDO.
          ENDIF.
        ENDIF.

* --------- catch errors
      CATCH cx_root INTO DATA(lx_exc).
        DATA(lv_error) = lx_exc->get_text( ).
        get_logger( )->error( lv_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_btocs_ollama_connector~parse_response_tags.
* ------ check
    IF io_response IS INITIAL
      OR io_response->is_json_object( ) EQ abap_false.
      get_logger( )->error( |invalid tags response| ).
      RETURN.
    ENDIF.

* ------- transform
    TRY.
        DATA(lo_parsed) = io_response->get_values_from_parsed_json( ).

        DATA(lo_answer)   = lo_parsed->get_structure_value( ).
        IF lo_answer IS INITIAL.
          get_logger( )->error( |no tags answer structure found| ).
          RETURN.
        ENDIF.

        DATA(lo_models) = lo_answer->get( zif_btocs_ollama_connector~c_json_key-models ).
        IF lo_models IS INITIAL.
          get_logger( )->error( |no tags answer structure found| ).
          RETURN.
        ENDIF.

        DATA(lo_array) = lo_models->get_array_value( ).
        DATA(lv_lin)   = lo_array->count( ).

* ----- loop model list
        DO lv_lin TIMES.
          DATA(lo_entry) = lo_array->get( sy-index ).
          DATA(lo_model) = lo_entry->get_structure_value( ).
          APPEND INITIAL LINE TO rt_result ASSIGNING FIELD-SYMBOL(<ls_rec>).

          <ls_rec>-model = lo_model->get_string( zif_btocs_ollama_connector~c_json_key-model ).
          <ls_rec>-name = lo_model->get_string( zif_btocs_ollama_connector~c_json_key-name ).
          <ls_rec>-modified_at = lo_model->get_string( zif_btocs_ollama_connector~c_json_key-modified_at ).
          <ls_rec>-size = lo_model->get_string( zif_btocs_ollama_connector~c_json_key-size ).
          <ls_rec>-digest = lo_model->get_string( zif_btocs_ollama_connector~c_json_key-digest ).

*         add details
          DATA(lo_detail_obj) = lo_model->get( zif_btocs_ollama_connector~c_json_key-details ).
          IF lo_detail_obj IS NOT INITIAL.
            DATA(lo_details) = lo_detail_obj->get_structure_value( ).
            <ls_rec>-details_parent_model = lo_details->get_string( zif_btocs_ollama_connector~c_json_key-parent_model ).
            <ls_rec>-details_format = lo_details->get_string( zif_btocs_ollama_connector~c_json_key-format ).
            <ls_rec>-details_familiy = lo_details->get_string( zif_btocs_ollama_connector~c_json_key-family ).
            <ls_rec>-details_parameter_size = lo_details->get_string( zif_btocs_ollama_connector~c_json_key-parameter_size ).
            <ls_rec>-details_quantization_level = lo_details->get_string( zif_btocs_ollama_connector~c_json_key-quantization_level ).

*         build families array as comma separated string
            DATA(lo_families_obj) = lo_details->get( zif_btocs_ollama_connector~c_json_key-families ).
            IF lo_families_obj IS NOT INITIAL.
              DATA(lo_families) = lo_families_obj->get_array_value( ).
              DATA(lv_lin_fam)  = lo_families->count( ).
              DO lv_lin_fam TIMES.
                DATA(lo_fam_entry) = lo_families->get( sy-index ).
                DATA(lv_fam_value) = lo_fam_entry->get_string( ).
                IF lv_fam_value IS NOT INITIAL.
                  IF <ls_rec>-details_families IS INITIAL.
                    <ls_rec>-details_families = lv_fam_value.
                  ELSE.
                    <ls_rec>-details_families = |{ <ls_rec>-details_families }, { lv_fam_value }|.
                  ENDIF.
                ENDIF.
              ENDDO.
            ENDIF.
          ENDIF.
        ENDDO.

* --------- catch errors
      CATCH cx_root INTO DATA(lx_exc).
        DATA(lv_error) = lx_exc->get_text( ).
        get_logger( )->error( lv_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

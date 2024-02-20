class ZCL_BTOCS_OLLAMA_CONNECTOR definition
  public
  inheriting from ZCL_BTOCS_RWS_CONNECTOR
  create public .

public section.

  interfaces ZIF_BTOCS_OLLAMA_CONNECTOR .

  class-methods CREATE
    returning
      value(RR_INSTANCE) type ref to ZIF_BTOCS_OLLAMA_CONNECTOR .
protected section.
private section.
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
  ENDMETHOD.
ENDCLASS.

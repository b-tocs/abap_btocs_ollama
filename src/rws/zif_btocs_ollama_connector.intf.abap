INTERFACE zif_btocs_ollama_connector
  PUBLIC .


  INTERFACES zif_btocs_rws_connector .
  INTERFACES zif_btocs_util_base .

  ALIASES destroy
    FOR zif_btocs_rws_connector~destroy .
  ALIASES execute
    FOR zif_btocs_rws_connector~execute .
  ALIASES get_client
    FOR zif_btocs_rws_connector~get_client .
  ALIASES get_logger
    FOR zif_btocs_rws_connector~get_logger .
  ALIASES is_initialized
    FOR zif_btocs_rws_connector~is_initialized .
  ALIASES is_logger_external
    FOR zif_btocs_rws_connector~is_logger_external .
  ALIASES new_request
    FOR zif_btocs_rws_connector~new_request .
  ALIASES new_response
    FOR zif_btocs_rws_connector~new_response .
  ALIASES set_endpoint
    FOR zif_btocs_rws_connector~set_endpoint .
  ALIASES set_logger
    FOR zif_btocs_rws_connector~set_logger .

  CONSTANTS:
    " see https://github.com/jmorganca/ollama/blob/main/docs/api.md
    BEGIN OF c_json_key,
      role       TYPE string VALUE 'role',
      model      TYPE string VALUE 'model',
      prompt     TYPE string VALUE 'prompt',
      stream     TYPE string VALUE 'stream',
      response   TYPE string VALUE 'response',
      context    TYPE string VALUE 'context',
      sys_prompt TYPE string VALUE 'system',
      template   TYPE string VALUE 'template',
    END OF c_json_key .

  METHODS api_generate
    IMPORTING
      !is_params         TYPE zbtocs_ollama_s_generate_par
      !iv_parse          TYPE abap_bool
    EXPORTING
      !ev_answer_text    TYPE string
      !es_result         TYPE zbtocs_ollama_s_generate_res
    RETURNING
      VALUE(ro_response) TYPE REF TO zif_btocs_rws_response .

  METHODS api_tags
    RETURNING
      VALUE(ro_response) TYPE REF TO zif_btocs_rws_response .

  METHODS api_show
    IMPORTING
      !iv_model          TYPE data
    RETURNING
      VALUE(ro_response) TYPE REF TO zif_btocs_rws_response .


  METHODS parse_response_generate
    IMPORTING
      !io_response     TYPE REF TO zif_btocs_rws_response
    RETURNING
      VALUE(rs_result) TYPE zbtocs_ollama_s_generate_res .
ENDINTERFACE.

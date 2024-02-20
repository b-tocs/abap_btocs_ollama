interface ZIF_BTOCS_OLLAMA_CONNECTOR
  public .


  interfaces ZIF_BTOCS_RWS_CONNECTOR .
  interfaces ZIF_BTOCS_UTIL_BASE .

  aliases DESTROY
    for ZIF_BTOCS_RWS_CONNECTOR~DESTROY .
  aliases EXECUTE
    for ZIF_BTOCS_RWS_CONNECTOR~EXECUTE .
  aliases GET_CLIENT
    for ZIF_BTOCS_RWS_CONNECTOR~GET_CLIENT .
  aliases GET_LOGGER
    for ZIF_BTOCS_RWS_CONNECTOR~GET_LOGGER .
  aliases IS_INITIALIZED
    for ZIF_BTOCS_RWS_CONNECTOR~IS_INITIALIZED .
  aliases IS_LOGGER_EXTERNAL
    for ZIF_BTOCS_RWS_CONNECTOR~IS_LOGGER_EXTERNAL .
  aliases NEW_REQUEST
    for ZIF_BTOCS_RWS_CONNECTOR~NEW_REQUEST .
  aliases NEW_RESPONSE
    for ZIF_BTOCS_RWS_CONNECTOR~NEW_RESPONSE .
  aliases SET_ENDPOINT
    for ZIF_BTOCS_RWS_CONNECTOR~SET_ENDPOINT .
  aliases SET_LOGGER
    for ZIF_BTOCS_RWS_CONNECTOR~SET_LOGGER .

  constants:
    " see https://github.com/jmorganca/ollama/blob/main/docs/api.md
    BEGIN OF c_json_key,
      role               TYPE string VALUE 'role',
      model              TYPE string VALUE 'model',
      models             TYPE string VALUE 'models',
      modelfile          TYPE string VALUE 'modelfile',
      prompt             TYPE string VALUE 'prompt',
      stream             TYPE string VALUE 'stream',
      response           TYPE string VALUE 'response',
      context            TYPE string VALUE 'context',
      sys_prompt         TYPE string VALUE 'system',
      template           TYPE string VALUE 'template',
      license            TYPE string VALUE 'license',
      name               TYPE string VALUE 'name',
      modified_at        TYPE string VALUE 'modified_at',
      size               TYPE string VALUE 'size',
      digest             TYPE string VALUE 'digest',
      details            TYPE string VALUE 'details',
      parent_model       TYPE string VALUE 'parent_model',
      format             TYPE string VALUE 'format',
      family             TYPE string VALUE 'family',
      families           TYPE string VALUE 'families',
      parameter_size     TYPE string VALUE 'parameter_size',
      parameters         TYPE string VALUE 'parameters',
      quantization_level TYPE string VALUE 'quantization_level',
      embedding          TYPE String VALUE 'embedding',
    END OF c_json_key .

  methods API_GENERATE
    importing
      !IS_PARAMS type ZBTOCS_OLLAMA_S_GENERATE_PAR
      !IV_PARSE type ABAP_BOOL
    exporting
      !EV_ANSWER_TEXT type STRING
      !ES_RESULT type ZBTOCS_OLLAMA_S_GENERATE_RES
    returning
      value(RO_RESPONSE) type ref to ZIF_BTOCS_RWS_RESPONSE .
  methods API_TAGS
    importing
      !IV_PARSE type ABAP_BOOL default ABAP_FALSE
    exporting
      !ET_RESULT type ZBTOCS_OLLAMA_T_TAGS_RES
    returning
      value(RO_RESPONSE) type ref to ZIF_BTOCS_RWS_RESPONSE .
  methods API_SHOW
    importing
      !IV_MODEL type DATA
      !IV_PARSE type ABAP_BOOL default ABAP_FALSE
    exporting
      !ES_RESULT type ZBTOCS_OLLAMA_S_SHOW_RES
    returning
      value(RO_RESPONSE) type ref to ZIF_BTOCS_RWS_RESPONSE .
  methods API_EMBEDDINGS
    importing
      !IS_PARAMS type ZBTOCS_OLLAMA_S_EMBEDDINGS_PAR
      !IV_PARSE type ABAP_BOOL
    exporting
      !ET_EMBEDDING type STRING_TABLE
      !EV_EMBEDDING type STRING
    returning
      value(RO_RESPONSE) type ref to ZIF_BTOCS_RWS_RESPONSE .
  methods PARSE_RESPONSE_GENERATE
    importing
      !IO_RESPONSE type ref to ZIF_BTOCS_RWS_RESPONSE
    returning
      value(RS_RESULT) type ZBTOCS_OLLAMA_S_GENERATE_RES .
  methods PARSE_RESPONSE_TAGS
    importing
      !IO_RESPONSE type ref to ZIF_BTOCS_RWS_RESPONSE
    returning
      value(RT_RESULT) type ZBTOCS_OLLAMA_T_TAGS_RES .
  methods PARSE_RESPONSE_SHOW
    importing
      !IO_RESPONSE type ref to ZIF_BTOCS_RWS_RESPONSE
    returning
      value(RS_RESULT) type ZBTOCS_OLLAMA_S_SHOW_RES .
  methods PARSE_RESPONSE_EMBEDDING
    importing
      !IO_RESPONSE type ref to ZIF_BTOCS_RWS_RESPONSE
    exporting
      !EV_ARRAY type STRING
    returning
      value(RT_RESULT) type STRING_TABLE .
endinterface.

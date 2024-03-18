interface ZIF_BTOCS_OLLAMA_C
  public .


  constants VERSION type STRING value 'V20240318' ##NO_TEXT.
  constants RELEASE type STRING value '0.2.1' ##NO_TEXT.
  constants HOMEPAGE type STRING value 'https://b-tocs.org' ##NO_TEXT.
  constants REPOSITORY type STRING value 'https://github.com/b-tocs/abap_btocs_ollamal' ##NO_TEXT.
  constants AUTHOR type STRING value 'mdjoerg@b-tocs.org' ##NO_TEXT.
  constants DEPENDING type STRING value 'https://github.com/b-tocs/abap_btocs_core:0.3.1' ##NO_TEXT.
  constants:
  " reference https://github.com/ollama/ollama/blob/main/docs/api.md
    BEGIN OF api_path,
      generate   TYPE string VALUE '/api/generate',
      embeddings TYPE string VALUE '/api/embeddings',
      tags       TYPE string VALUE '/api/tags',
      show       TYPE string VALUE '/api/show',
    END OF api_path .
endinterface.

%% define amf0
-define(VERSION_1, 1).
%% not used anymore but just there
-define(VERSION_2, 2).
%% define amf3
-define(VERSION_3, 3).

-define(STATUS_METHOD, "/onStatus").
-define(RESULT_METHOD, "/onResult").

-define(AMF_CONTENT_TYPE, "application/x-amf").
-define(XML_CONTENT_TYPE, "application/xml").

-define(OBJECT_REF_TABLE_AMF0, objectRefTableAmf0).
-define(OBJECT_REF_TABLE, objectRefTable).
-define(TRAIT_REF_TABLE, traitRefTable).
-define(STRING_REF_TABLE, stringRefTable).

-record(header, {headerName = "", mustUnderstand = false, data = <<>>}).
-record(body, {targetUri = "", responseUri = "", data = <<>> }).

-record(action_message, {
	  version = ?VERSION_1, 
	  headers = [], %% list of 'header'
	  bodies = [] %% list of 'body'
	 }).

-record(trait, {
	  className = "",
	  externalizable = false,
	  dynamic = false,
	  properties = []
	 }).


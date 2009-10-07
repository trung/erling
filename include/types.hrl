%% AMF0 custom types
%% as it's hard to differentiate between normal list and string, we create record type string
-record(string, {data}).

-record(long_string, {data}).

-record(ecma_array, {data}).

-record(asobject, {array = []}).

-record(xml, {data}).

%% AMF3 custom types
-record(string_3, {data}).

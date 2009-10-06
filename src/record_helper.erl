%% For each header file, it scans thru all records and create helper functions
-module(record_helper).
-author("trung@mdkt.org").

-export([make/1, make/2]).

make(HeaderFiles) ->
    make([ atom_to_list(X) || X <- HeaderFiles ], ".").

%% .hrl file, relative to current dir
make(HeaderFiles, OutDir) ->
    ModuleName = "record_utils",
    HeaderComment = "%% This is auto generated file. Please don't edit it\n\n",
    ModuleDeclaration = "-module(" ++ ModuleName ++ ").\n" 
	++ "-author(\"trung@mdkt.org\").\n"
	++ "-compile(export_all).\n"
	++ [ "-include(\"" ++ X ++ "\").\n" || X <- HeaderFiles ]
	++ "\n",
    Src = format_src(lists:sort(lists:flatten([read(X) || X <- HeaderFiles]))),
    file:write_file(OutDir++"/" ++ ModuleName ++ ".erl", list_to_binary([HeaderComment, ModuleDeclaration, Src])).

read(HeaderFile) ->
    try epp:parse_file(HeaderFile,[],[]) of
	{ok, Tree} ->
	    parse(Tree)
    catch
	_:Error ->
	    {error, Error}
    end.

format_src([{_, _, _, Src}|T]) when length(T) == 0 ->
    Src ++ ".\n\n";
format_src([{Type, _, _, Src}|[{Type, A, B, NSrc}|T]]) ->
    Src ++ ";\n\n" ++ format_src([{Type, A, B, NSrc}|T]);
format_src([{_Type, _, _, Src}|[{Type1, A, B, NSrc}|T]]) ->
    Src ++ ".\n\n" ++ format_src([{Type1, A, B, NSrc}|T]);
format_src([{_, _, _, Src}|T]) when length(T) > 0 ->
    Src ++ ";\n\n" ++ format_src(T).

parse(Tree) ->
    [ parse_record(X) || X <- Tree ].

parse_record({attribute, _, record, RecordInfo}) ->
    {RecordName, RecordFields} = RecordInfo,
    if
	length(RecordFields) == 1 ->
	    lists:flatten([ generate_setter_function(RecordName, X) || X <- RecordFields ]
		  ++ [generate_type_function(RecordName)]);
	true ->
	    lists:flatten([generate_fields_function(RecordName, RecordFields)] 
			  ++ [generate_fields_atom_function(RecordName, RecordFields)]
		  ++ [ generate_setter_function(RecordName, X) || X <- RecordFields ]
		  ++ [generate_type_function(RecordName)])
    end;
parse_record(_) -> [].

parse_field_name({record_field, _, {atom, _, FieldName}}) ->
    "\"" ++ atom_to_list(FieldName) ++ "\"";
parse_field_name({record_field, _, {atom, _, FieldName}, _}) ->
    "\"" ++ atom_to_list(FieldName) ++ "\"".

parse_field_name_atom({record_field, _, {atom, _, FieldName}}) ->
    atom_to_list(FieldName);
parse_field_name_atom({record_field, _, {atom, _, FieldName}, _}) ->
    atom_to_list(FieldName).

parse_field([F|T]) when length(T) == 0 -> parse_field_name(F);
parse_field([F|T]) ->
    parse_field_name(F) ++ ", " ++ parse_field(T).

parse_field_atom([F|T]) when length(T) == 0 -> parse_field_name_atom(F);
parse_field_atom([F|T]) ->
    parse_field_name_atom(F) ++ ", " ++ parse_field_atom(T).

generate_type_function(RecordName) ->
    {type, RecordName, 0, "type(Obj) when is_record(Obj, " ++ atom_to_list(RecordName) ++ ") -> " ++ atom_to_list(RecordName)}.

generate_fields_function(RecordName, RecordFields) ->
    Fields = parse_field(RecordFields),
    {field, RecordName, 1, "fields(" ++ atom_to_list(RecordName) ++ ") -> \n\t[" ++ Fields ++ "]"}.

generate_fields_atom_function(RecordName, RecordFields) ->
    Fields = parse_field_atom(RecordFields),
    {field_atom, RecordName, 1, "fields_atom(" ++ atom_to_list(RecordName) ++ ") -> \n\t[" ++ Fields ++ "]"}.

generate_setter_function(RecordName, {record_field, _, {atom, _, FieldName}, {record, _, ParentRecordName, _}}) ->
    to_setter_function(atom_to_list(RecordName), atom_to_list(FieldName), atom_to_list(ParentRecordName));
generate_setter_function(RecordName, {record_field, _, {atom, _, array}, _}) ->
    to_setter_function(atom_to_list(RecordName));
generate_setter_function(RecordName, {record_field, _, {atom, _, FieldName}, _}) ->
    to_setter_function(atom_to_list(RecordName), atom_to_list(FieldName));
generate_setter_function(RecordName, {record_field, _, {atom, _, FieldName}}) ->
    to_setter_function(atom_to_list(RecordName), atom_to_list(FieldName)).

%% when field name is array of object
to_setter_function(RecordName) ->
    {setter, RecordName, 1, "set(Obj, PropertyName, Value) when is_record(Obj, " ++ RecordName ++ ") -> \n"
	++ "\tNewObj = #" ++ RecordName ++ "{array = Obj#" ++ RecordName ++ ".array ++ [{PropertyName, Value}]},\n" 
	++ "\t{ok, NewObj, {array, Value}}"}.    

to_setter_function(RecordName, FieldName) ->
    %% setters
    {setter, RecordName, 1, "set(Obj, " ++ FieldName ++ ", Value) when is_record(Obj, " ++ RecordName ++ ") -> \n"
	++ "\tNewObj = Obj#" ++ RecordName ++ "{" ++ FieldName ++ " = Value},\n" 
	++ "\t{ok, NewObj, {" ++ FieldName ++ ", Value}}"}.

to_setter_function(RecordName, FieldName, ParentRecordName) ->
    {setter, RecordName, 2, "set(Obj, " ++ FieldName ++ ", Value) when is_record(Obj, " ++ RecordName ++ ") and is_record(Value, " ++ ParentRecordName ++ ") -> \n"
     ++ "\tNewObj = Obj#" ++ RecordName ++ "{" ++ FieldName ++ " = Value},\n" 
     ++ "\t{ok, NewObj, {" ++ FieldName ++ ", Value}};\n\n" 
     ++ "set(Obj, ParentProperty, Value) when is_record(Obj, " ++ RecordName ++ ") and is_atom(ParentProperty) -> \n"
     ++ "\t{ok, NewParentObject, _} = set(Obj#" ++ RecordName ++ ".parent, ParentProperty, Value),\n" 
     ++ "\tset(Obj, parent, NewParentObject)"}.

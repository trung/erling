-module(registry).
-author("trung@mdkt.org").
-include("../include/messages.hrl").
-include("../include/flex_classes.hrl").
-compile(export_all).

fc_to_record(?FC_REMOTINGMESSAGE) -> {ok, #remoting_message{}};
fc_to_record(?FC_COMMANDMESSAGE)  -> {ok, #command_message{}};
fc_to_record(_) -> {ok, undefined}.

record_to_fc(remoting_message) -> {ok, ?FC_REMOTINGMESSAGE};
record_to_fc(command_message) -> {ok, ?FC_COMMANDMESSAGE};
record_to_fc(_) -> {ok, undefined}.

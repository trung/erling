%% operations for command message
-define(SUBSCRIBE_OPERATION, 0).
-define(UNSUBSCRIBE_OPERATION, 1).
-define(POLL_OPERATION, 2).
-define(CLIENT_SYNC_OPERATION, 4).
-define(CLIENT_PING_OPERATION, 5).
-define(CLUSTER_REQUEST_OPERATION, 7).
-define(LOGIN_OPERATION, 8).
-define(LOGOUT_OPERATION, 9).
-define(SUBSCRIPTION_INVALIDATE_OPERATION, 10).
-define(MULTI_SUBSCRIBE_OPERATION, 11).
-define(DISCONNECT_OPERATION, 12).
-define(UNKNOWN_OPERATION, 1000).

-record(abstract_message, {
	  clientId,
	  destination = "",
	  messageId = "",
	  timestamp = 0,
	  timeToLive = 0,
	  headers = [],
	  body
	 }).

-record(rpc_message, {
	  parent = #abstract_message{},
	  remoteUsername = "",
	  remotePassword = ""
	 }).

-record(remoting_message, {
	  parent = #rpc_message{},
	  source = "",
	  operation = "",
	  parameters = []
	 }).

-record(async_message, {
	  parent = #abstract_message{},
	  correlationId = "",
	  correlationIdBytes = []
	 }).

-record(command_message, {
	  parent = #async_message{},
	  operation = ?UNKNOWN_OPERATION
	 }).

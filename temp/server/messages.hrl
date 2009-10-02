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

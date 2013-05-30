-module(skyraid_webmachine_file_resource).

-include_lib("skyraid/include/skyraid.hrl").

-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2, is_authorized/2, to_text/2, from_text/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% ====================================================================
%% API
%% ====================================================================

init([]) -> {ok, []}.

allowed_methods(ReqData, Context) ->
    {['GET', 'PUT'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
	{[{"text/plain", to_text}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
	{[{"text/plain", from_text}], ReqData, Context}.

is_authorized(ReqData, Context) ->
	case wrq:get_req_header("Authorization", ReqData)of
		undefined -> 
			{true, ReqData, Context};
		Token -> 
			Session = binary_to_term(base64:decode(Token)),
			{true, ReqData, Context}
	end.

to_text(ReqData, Context) ->
	{<<"here comes the file">>, ReqData, Context}.

from_text(ReqData, Context) ->
	%% Just echo the content for now
	Bin = wrq:req_body(ReqData),
	{true, wrq:append_to_response_body(Bin, ReqData), Context}.

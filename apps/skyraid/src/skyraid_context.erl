-module(skyraid_context).

-export([init/0, close/0, get_account_provider/1, get_account_providers/0, register_account_provider/2, get_file_provider/1, register_file_provider/2]).

-define(ACCOUNT_PROVIDER_TABLE, skyraid_context_account_provider).
-define(FILE_PROVIDER_TABLE, skyraid_context_file_provider).

init() ->
	?ACCOUNT_PROVIDER_TABLE = ets:new(?ACCOUNT_PROVIDER_TABLE, [set, named_table, public]),
	?FILE_PROVIDER_TABLE = ets:new(?FILE_PROVIDER_TABLE, [set, named_table, public]),
	ok.

close() ->
	true = ets:delete(?ACCOUNT_PROVIDER_TABLE),
	true = ets:delete(?FILE_PROVIDER_TABLE).

get_account_provider(Provider) when is_atom(Provider) ->
	case ets:lookup(?ACCOUNT_PROVIDER_TABLE, Provider) of
		[] -> {error, not_found};
		[{_Provider, Module}] -> {ok, Module}
	end.

get_account_providers() ->
	case ets:tab2list(?ACCOUNT_PROVIDER_TABLE) of
		Providers -> {ok, Providers}
	end.

register_account_provider(Provider, Module) when is_atom(Provider) and is_atom(Module) ->
	case ets:insert_new(?ACCOUNT_PROVIDER_TABLE, {Provider, Module}) of
		true -> ok;
		false -> {error, already_exist}
	end.

get_file_provider(Provider) when is_atom(Provider) ->
	case ets:lookup(?FILE_PROVIDER_TABLE, Provider) of
		[] -> {error, not_found};
		[{_Provider, Module}] -> {ok, Module}
	end.

register_file_provider(Provider, Module) when is_atom(Provider) and is_atom(Module) ->
	case ets:insert_new(?FILE_PROVIDER_TABLE, {Provider, Module}) of
		true -> ok;
		false -> {error, already_exist}
	end.

%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

skyraid_context_test_() ->
	{foreach, fun setup/0, fun teardown/1,
		[
			{"register account provider", fun register_account_provider_tc/0},
			{"register duplicate account provider", fun register_account_provider_duplicate_tc/0},
			{"register file provider", fun register_file_provider_tc/0},
			{"register duplicate file provider", fun register_file_provider_duplicate_tc/0}
		]
	}.


setup() -> skyraid_context:init().

teardown(_) -> skyraid_context:close().

register_account_provider_tc() ->
	ok = skyraid_context:register_account_provider(local, ?MODULE),
	{ok, ?MODULE} = skyraid_context:get_account_provider(local),
	{ok, [{local, ?MODULE}]} = skyraid_context:get_account_providers().

register_account_provider_duplicate_tc() ->
	ok = skyraid_context:register_account_provider(duplicate, ?MODULE),
	{error, already_exist} = skyraid_context:register_account_provider(duplicate, ?MODULE),
	{ok, [{duplicate, ?MODULE}]} = skyraid_context:get_account_providers().

register_file_provider_tc() ->
	ok = skyraid_context:register_file_provider(local, ?MODULE),
	{ok, ?MODULE} = skyraid_context:get_file_provider(local).

register_file_provider_duplicate_tc() ->
	ok = skyraid_context:register_file_provider(local, ?MODULE),
	{error, already_exist} = skyraid_context:register_file_provider(local, ?MODULE).

-endif.

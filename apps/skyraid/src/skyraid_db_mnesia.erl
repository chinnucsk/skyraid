-module(skyraid_db_mnesia).

-include("skyraid.hrl").

-export([init/0, create_user/1, create_storage/1, get_users/0, get_user/1, get_user_by_id/1]).
-export([create_account/1, get_account/1, get_accounts/0]).

-define(await_timeout, 20000).

init() ->
	case mnesia:create_schema([node()]) of
		{error, {_Node, {already_exists, _Node}}} ->
			mnesia:start(),
			await_tables(),
			ok;
		ok ->
			mnesia:start(),
			create_tables(),
			await_tables(),
			insert_test_data(),
			ok
	end.

get_users() ->
	case get_table(skr_user) of
		Users -> {ok, Users}
	end.

get_user(Username)->
	case read(skr_user, Username, #skr_user.username) of
		{atomic, [User]} -> {ok, User};
		{atomic, []} -> not_found
	end.

get_user_by_id(ID) ->
	case read(skr_user, ID, #skr_user.id) of
	 	{atomic, [User]} -> {ok, User};
		{atomic, []} -> not_found
	end.

create_user(#skr_user{} = User) ->
	case write(User) of 
		{atomic, ok}-> {ok, User}; 
		Any-> {error, Any}
	end.

get_accounts() ->
	ets:tab2list(skr_account).

get_account(AccountID) ->
	case read(skr_account, AccountID) of
		{atomic, [Account]} -> {ok, Account};
		[] -> not_found
	end.

create_account(#skr_account{} = Account) ->
	case write(Account) of
		{atomic, ok} -> {ok, Account}; 
		Any-> {error, Any}
	end.

create_storage(#skr_storage{} = Storage) ->
	ets:insert(storages, Storage).

%% ====================================================================
%% Internal functions
%% ====================================================================
create_tables() ->
	{atomic, ok} = mnesia:create_table(skr_user, 
						[{disc_copies,[node()]},
						 {attributes, 
						  record_info(fields, skr_user)}]),
	{atomic, ok} = mnesia:add_table_index(skr_user, username),

	{atomic, ok} = mnesia:create_table(skr_account, 
						[{disc_copies,[node()]},
						 {attributes, 
						  record_info(fields, skr_account)}]).

await_tables() ->
	mnesia:wait_for_tables([skr_user, skr_account], ?await_timeout).

%do(Q) ->
% 	F = fun() -> qlc:e(Q) end,
%   	mnesia:transaction(F).

write(Record) ->
	Fun = fun()-> 
				  mnesia:write(Record)
		  end,
	mnesia:transaction(Fun).

read(Tab, Key, Pos) ->
	Fun = fun() ->
				mnesia:index_read(Tab, Key, Pos)
		  end,
	mnesia:transaction(Fun).

read(Tab, Key) ->
	Fun = fun() ->
				  mnesia:read({Tab, Key})
		  end,
	mnesia:transaction(Fun).

%delete(Tab, Key) ->
%		Fun = fun() ->
%				  mnesia:delete({Tab, Key})
%		  end,
%	mnesia:transaction(Fun).

get_table(Table) when is_atom(table) ->
	ets:tab2list(Table).

insert_test_data() ->
	Adam = #skr_user{username = <<"Adam">>, 
			  password = <<"test">>, 
			  display_name = <<"AdamDisplay">>, 
			  email = <<"adam@gmail.com">>},

	Eva = #skr_user{username = <<"Eva">>, 
	  password = <<"test">>, 
	  display_name = <<"EvaDisplay">>, 
	  email = <<"adam@gmail.com">>},

	create_user(Adam),
	create_user(Eva).



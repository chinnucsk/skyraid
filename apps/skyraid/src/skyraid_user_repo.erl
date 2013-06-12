-module(skyraid_user_repo).

-include_lib("skyraid.hrl").

-export([new/1, get_all/0, get_user/1, get_user_by_id/1]).

-spec new(skr_user()) -> {ok, skr_user()} | {error, username_exist}.
new(#skr_user{username=Username}=User) ->
    case skyraid_db:get_user(Username) of
	not_found -> skyraid_db:create_user(User);
	_ -> {error, username_exist}
    end.

-spec get_all() -> {ok, [skr_user()]} | {error, term()}.
get_all() ->
    skyraid_db:get_users().

get_user(Username) ->
    skyraid_db:get_user(Username).

get_user_by_id(ID) ->
    skyraid_db:get_user_by_id(ID).

-module(skyraid_user_repo).

-include_lib("skyraid.hrl").

-export([new/1, get_all/0, get_user/1, get_user_by_id/1]).

new(#skr_user{}=User) ->
	skyraid_db:create_user(User).

get_all() ->
	skyraid_db:get_users().	

get_user(Username) ->
	skyraid_db:get_user(Username).

get_user_by_id(ID) ->
	skyraid_db:get_user_by_id(ID).
-module(skyraid_user_repo).

-include_lib("skyraid.hrl").

-export([new/1, get_all/0]).

new(#skr_user{}=User) ->
	skyraid_db:create_user(User).

get_all() ->
	skyraid_db:get_users().	

-module(skyraid_storage_ftp).

-include("skyraid.hrl").

-export([list_files/1]).

list_files(#skr_auth_basic{url=_URL,
			   username=_Username,
			   password=_Password}) ->
    {ok, "here comes the files later"}.

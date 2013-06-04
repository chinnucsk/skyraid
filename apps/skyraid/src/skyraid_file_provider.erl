-module(skyraid_file_provider).

-include("skyraid.hrl").

-type session() :: skr_auth_acctoken().

-callback list_files(session()) -> {ok, skr_file_info()} | {error, term()}.
-callback write_file(session(), string(), binary()) -> ok | {error, term()}.
-callback read_file(session(), string()) -> {ok, binary()} | {error, term()}.
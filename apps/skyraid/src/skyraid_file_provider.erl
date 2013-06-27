-module(skyraid_file_provider).

-include("skyraid.hrl").

-type session() :: skr_auth_acctoken().

-callback open(session(), string(), term()) ->{ok, skr_file_ref()} | {error, term()}.
-callback close(session(), skr_file_ref()) -> ok | {error, term()}.
-callback write(session(), skr_file_ref(), binary()) -> ok | {error, term()}.
-callback read(session(), skr_file_ref()) -> ok | {error, term()}.

-callback write_file(session(), string(), binary(), list()) -> ok | {error, term()}.
-callback read_file(session(), string(), list()) -> {ok, binary()} | {error, term()}.
-callback list_files(session()) -> {ok, skr_file_info()} | {error, term()}.
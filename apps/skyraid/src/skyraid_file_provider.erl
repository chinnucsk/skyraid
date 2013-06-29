-module(skyraid_file_provider).

-include("skyraid.hrl").

-type session() :: skr_auth_acctoken().

-callback open(session(), string(), term()) ->{ok, skr_file_ref()} | {error, term()}.
-callback close(session(), skr_file_ref()) -> ok | {error, term()}.
-callback write(session(), skr_file_ref(), binary()) -> ok | {error, term()}.
-callback read(session(), skr_file_ref()) -> ok | {error, term()}.

-callback put(session(), string(), binary(), list()) -> ok | {error, term()}.
-callback get(session(), string(), list()) -> {ok, binary()} | {error, term()}.
-callback list(session()) -> {ok, skr_file_info()} | {error, term()}.
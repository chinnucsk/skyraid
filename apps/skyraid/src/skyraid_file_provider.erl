-module(skyraid_file_provider).

-include("skyraid.hrl").

-type session() :: skr_auth_acctoken().
-type path() :: string().
-type opts() :: list().

-callback open(session(), path(), opts()) ->{ok, skr_file_ref()} | {error, term()}.
-callback close(session(), skr_file_ref()) -> ok | {error, term()}.
-callback write(session(), skr_file_ref(), binary()) -> ok | {error, term()}.
-callback read(session(), skr_file_ref()) -> {ok, binary()} | {error, term()}.

-callback put(session(), path(), binary(), opts()) -> ok | {error, term()}.
-callback get(session(), path(), opts()) -> {ok, binary()} | {error, term()}.
-callback list(session(), path()) -> {ok, [skr_file_info()]} | {error, term()}.

-callback mkdir(session(), path()) -> {ok, skr_file_info()} | {error, term}.
-callback copy(session(), path(), path()) -> {ok, skr_file_info()} | {error, term()}.
-callback move(session(), path(), path()) -> {ok, skr_file_info()} | {error, term()}.
-callback delete(session(), path()) -> {ok, skr_file_info()} | {error, term()}.
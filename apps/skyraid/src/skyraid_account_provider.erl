-module(skyraid_account_provider).

-include("skyraid.hrl").

-type basic() :: skr_auth_basic().
-type token() :: skr_auth_reqtoken().
-type session() :: skr_auth_acctoken().
 

-callback init() -> {ok, term()} | {error, term()}.
-callback create_token() -> {ok, token()} | {error, term()}.
-callback authenticate(basic() | token()) -> {ok, session()}| {error, term()}.
-callback logout(session()) -> ok | {error, term()}.
-callback account_info(session()) -> {ok, skr_account()} | {error, term()}.
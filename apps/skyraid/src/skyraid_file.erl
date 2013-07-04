-module(skyraid_file).

-include_lib("skyraid.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([open/4, close/2, write/3, read/2, put/5, get/4, list/3]).

open(Session, AccountID, FileName, Opts) ->
    {ok, #skr_account{provider=Provider, authentication=Auth}} = skyraid_user_session:get_account(Session, AccountID),
    {ok, ProviderModule} = skyraid_context:get_file_provider(Provider),
    case ProviderModule:open(Auth, FileName, Opts) of
        {ok, FileRef} -> {ok, #skr_file_ref{account_id=AccountID, ref=FileRef}};
        {error, Reason} -> {error, Reason}
    end.

close(Session, #skr_file_ref{account_id=AccountID} = FR) ->
    {ok, #skr_account{provider=Provider, authentication=Auth}} = skyraid_user_session:get_account(Session, AccountID),
    {ok, ProviderModule} = skyraid_context:get_file_provider(Provider),
    ProviderModule:close(Auth, FR).

write(Session, #skr_file_ref{account_id=AccountID} = FR, Content) ->
    {ok, #skr_account{provider=Provider, authentication=Auth}} = skyraid_user_session:get_account(Session, AccountID),
    {ok, ProviderModule} = skyraid_context:get_file_provider(Provider),
    ProviderModule:write(Auth, FR, Content).

read(Session, #skr_file_ref{account_id=AccountID} = FR) ->
    {ok, #skr_account{provider=Provider, authentication=Auth}} = skyraid_user_session:get_account(Session, AccountID),
    {ok, ProviderModule} = skyraid_context:get_file_provider(Provider),
    ProviderModule:read(Auth, FR).

put(Session, AccountID, FileName, Content, Opts) ->
    {ok, #skr_account{provider=Provider, authentication=Auth}} = skyraid_user_session:get_account(Session, AccountID),
    {ok, ProviderModule} = skyraid_context:get_file_provider(Provider),
    ProviderModule:put(Auth, FileName, Content, Opts).

get(Session, AccountID, FileName, Opts) ->
    {ok, #skr_account{provider=Provider, authentication=Auth}} = skyraid_user_session:get_account(Session, AccountID),
    {ok, ProviderModule} = skyraid_context:get_file_provider(Provider),
    ProviderModule:get(Auth, FileName, Opts).

list(Session, AccountID, Path) ->
    {ok, #skr_account{provider=Provider, authentication=Auth}} = skyraid_user_session:get_account(Session, AccountID),
    {ok, ProviderModule} = skyraid_context:get_file_provider(Provider),
    ProviderModule:list(Auth, Path).

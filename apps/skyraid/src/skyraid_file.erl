-module(skyraid_file).

-include_lib("skyraid.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([open/4, close/2, write/3, read/2, write_file/5, read_file/4, list_files/2]).

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

write_file(Session, AccountID, FileName, Content, Opts) ->
    {ok, #skr_account{provider=Provider, authentication=Auth}} = skyraid_user_session:get_account(Session, AccountID),
    {ok, ProviderModule} = skyraid_context:get_file_provider(Provider),
    ProviderModule:write_file(Auth, FileName, Content, Opts).

read_file(Session, AccountID, FileName, Opts) ->
    {ok, #skr_account{provider=Provider, authentication=Auth}} = skyraid_user_session:get_account(Session, AccountID),
    {ok, ProviderModule} = skyraid_context:get_file_provider(Provider),
    ProviderModule:read_file(Auth, FileName, Opts).

list_files(Session, AccountID) ->
    {ok, #skr_account{provider=Provider, authentication=Auth}} = skyraid_user_session:get_account(Session, AccountID),
    {ok, ProviderModule} = skyraid_context:get_file_provider(Provider),
    ProviderModule:list_files(Auth).

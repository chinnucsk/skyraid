-module(skyraid_ranch_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = ensure_started(skyraid),
	ok = ensure_started(ranch),

	{ok, _Ip} = application:get_env(skyraid_ranch, ftp_ip),
    {ok, CommandPort} = application:get_env(skyraid_ranch, ftp_command_port),
    {ok, DataPort} = application:get_env(skyraid_ranch, ftp_data_port),

    erlang:display({"Starting ranch", CommandPort, DataPort}),
	{ok, _CommandPid} = ranch:start_listener(skyraid_ftp_command, 10, ranch_tcp, [{port, CommandPort}], skyraid_ranch_ftp_command, []),

    skyraid_ranch_sup:start_link().

stop(_State) ->
    ok.

ensure_started(App) ->
    case application:start(App) of
        ok ->ok;
        {error, {already_started, App}} ->ok
    end.

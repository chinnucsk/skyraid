-module(skyraid_webmachine).

-export([start/0, stop/0]).

start() ->
	ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,webmachine_logger),
    ensure_started(webmachine),
    ensure_started(skyraid),
    application:start(skyraid_webmachine).

stop() ->
    application:stop(inets),
    application:stop(crypto),
    application:stop(mochiweb),
    application:stop(webmachine),
    application:stop(skyraid),
    application:stop(skyraid_webmachine).

ensure_started(App) ->
    case application:start(App) of
        ok ->ok;
        {error, {already_started, App}} ->ok
    end.
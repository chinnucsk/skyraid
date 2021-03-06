-module(skyraid_webmachine_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    {ok, Ip} = application:get_env(skyraid_webmachine, ip),
    {ok, Port} = application:get_env(skyraid_webmachine, port),
	
    {ok, FileDispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                         "..", "priv", "dispatch.conf"])),
	
	DocRoot = filename:join([filename:dirname(code:which(?MODULE)),"..", "priv", "www"]),
	%Dispatch = [{['*'], tracktrain_root_resource, [DocRoot]}],
	Dispatch =  FileDispatch ++ [{['*'], skyraid_webmachine_root_resource, [DocRoot]}],

  WebConfig = [{ip, Ip},
               {port, Port},
               {log_dir, "priv/log"},
               {dispatch, Dispatch}],
               
  lager:info("Starting skyraid_webmachine with the following WebConfig( ~s:~w )", [Ip, Port]),

  Web = {webmachine_mochiweb, {webmachine_mochiweb, start, [WebConfig]}, permanent, 5000, worker, dynamic},
  Processes = [Web],
  {ok, { {one_for_one, 10, 10}, Processes} }.

-module(skyraid_ranch_ftp_command).

-export([start_link/4, init/4]).

-export([user/2, pass/2, quit/1, pasv/1, opts/3, type/2, list/1, pwd/1, cwd/2, stor/2, retr/2]).

-include("../../skyraid/include/skyraid.hrl").
 
-record(state, 
{
	ip, 			%% The ip of the running server 
	port, 			%% The command port number of the running server

	socket,			%% The socket for receiving commands, (used by ranch)
	transport,		%% The transport used, (used by ranch). 

	username,		%% The username of the connected user
	session, 		%% The session for the logged in user

	data_state		%% The state of the data module.
}).

start_link(ListenerPid, Socket, Transport, Opts) ->
	IP = proplists:get_value(ftp_ip, Opts),
	Port = proplists:get_value(ftp_command_port, Opts),

	?DEBUG({"Starting link", IP, Port}),
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, #state{ip=IP, port=Port, socket=Socket, transport=Transport}]),
    {ok, Pid}.
 
init(ListenerPid, Socket, Transport, State) ->
    ok = ranch:accept_ack(ListenerPid),
    response(Socket, Transport, 200, "Skyraid FTP server welcomes you!!"),
    loop(Socket, Transport, State).

loop(Socket, Transport, State) ->
    case Transport:recv(Socket, 0, 30000) of
        {ok, Data} ->
        	?DEBUG({"Received", Data, unicode:characters_to_list(Data)}),
            NewState = handle(State, Data),
			loop(Socket, Transport, NewState);
        {error, _} ->
            ?DEBUG("The client disconnected~n")
    end.
 

handle(State, Data) ->
    ?DEBUG({"Command received", Data}),
    Commands = parse_commands(Data),
    lists:foldl(fun process_command/2, State, Commands).

process_command({CommandStr, Args}, #state{socket=Socket, transport=Transport} = S) ->
	CommandAtom = erlang:list_to_atom(string:to_lower(CommandStr)),
	case erlang:apply(?MODULE, CommandAtom, Args ++ [S]) of
		{Code, Msg, NewState} ->
			response(Socket, Transport, Code, Msg),
			NewState;
		{Code, NewState} when is_number(Code) ->
			response(Socket, Transport, Code),
			NewState;
		{done, NewState} ->
			NewState
	end.

parse_commands(Data) ->
	Str = unicode:characters_to_list(Data),
    CmdTokens = string:tokens(Str, "\r\n"),
    CmdList = [string:tokens(X, " ") ||  X <- CmdTokens],
    [ {Cmd, [unicode:characters_to_binary(B) || B <- Args] } || [Cmd|Args] <- CmdList].

%% ====================================================================
%% FTP command functions 
%% ====================================================================
user(Username, State) ->
	?DEBUG({user, Username}),
	NewState = State#state{username=Username},
	{331, NewState}.

pass(Password, #state{username=Username}=S) ->
	?DEBUG({pass, Password}),
	case skyraid:login(Username, Password) of
			{ok, SessionRef} ->
				{230, S#state{session=SessionRef}};
			{error, _} ->
				{530, S}
	end.

quit(#state{session=Session}=S) ->
	?DEBUG({quit, S}),
	case skyraid:logout(Session) of
		ok -> {231, S#state{session=undefined}};
		{error, _Error} -> {400, S}
	end.

pasv(#state{socket=_Socket, transport=_Transport} = S) ->
	?DEBUG({pasv, S}),
	{ok, [S1,S2,S3,S4,P1,P2], DataState} = skyraid_ranch_ftp_data:start_pasv(),
	Msg = lists:flatten(io_lib:format("Entering Passive Mode(~p,~p,~p,~p,~p,~p)", [S1,S2,S3,S4,P1,P2])),
	%% response(Socket, Transport, 227, Msg),
	%%{ok, NewDataState} = skyraid_ranch_ftp_data:connect(DataState),
	{227, Msg, S#state{data_state=DataState}}.

opts(<<"utf8">>, Value, State) ->
	?DEBUG({opts, <<"utf8">>, Value}),
	{200, State};

opts(Parameter, Value, State) ->
	?DEBUG({opts, Parameter, Value}),
	{500, State}.

type(<<"A">>, State) ->
	?DEBUG({type, <<"A">>}),
	{200, "Type set to A", State}.


pwd(State) ->
	{257, "\"myftpserver/dir1\" is current directory", State}.

cwd(Dir, State) ->
	?DEBUG({cwd, Dir}),
	{250, State}.

list(#state{session=Session, data_state=DataState} = State) ->
	?DEBUG({list, State}),
	skyraid_ranch_ftp_data:list_files(Session, DataState),
	{200, State}.

stor(FileName, #state{socket=Socket, transport=Transport, data_state=DataState, session=Session} = State) ->
	File =  unicode:characters_to_list(FileName),
	?DEBUG({stor, File, State}),
	response(Socket, Transport, 150),
	case skyraid_ranch_ftp_data:put_file(Session, FileName, DataState) of
		{ok, NewState} -> {200, NewState};
		{error, _Error} -> {500, State}
	end.

retr(FileName, #state{socket=Socket, transport=Transport, data_state=DataState, session=Session} = State) ->
	File = unicode:characters_to_list(FileName),
	?DEBUG({retr, File, State}),
	response(Socket, Transport, 150),
	case skyraid_ranch_ftp_data:get_file(Session, FileName, DataState) of
		{ok, NewState} -> {226, NewState};
		{error, _Error} -> {500, State}
	end.


%% ====================================================================
%% Private functions 
%% ====================================================================
response(Socket, Transport, Code) ->
	Resp = response_string(Code),
	?DEBUG(Resp),
	Transport:send(Socket, Resp).

response(Socket, Transport, Code, Msg) ->
	Resp = response_string(Code, Msg),
	?DEBUG(Resp),
	Transport:send(Socket, Resp).

response_string(Code) ->
	integer_to_list(Code) ++ " " ++ response_code_string(Code) ++ "\r\n".

response_string(Code, Msg) ->
	integer_to_list(Code) ++ " " ++ Msg ++ "\r\n".


% Adapted from jungerl/ftpd.erl
response_code_string(110) -> "MARK yyyy = mmmm";
response_code_string(120) -> "Service ready in nnn minutes.";
response_code_string(125) -> "Data connection alredy open; transfere starting.";
response_code_string(150) -> "File status okay; about to open data connection.";
response_code_string(200) -> "Command okay.";
response_code_string(202) -> "Command not implemented, superfluous at this site.";
response_code_string(211) -> "System status, or system help reply.";
response_code_string(212) -> "Directory status.";
response_code_string(213) -> "File status.";
response_code_string(214) -> "Help message.";
response_code_string(215) -> "UNIX system type";
response_code_string(220) -> "Service ready for user.";
response_code_string(221) -> "Service closing control connection.";
response_code_string(225) -> "Data connection open; no transfere in progress";
response_code_string(226) -> "Closing data connection.";
response_code_string(227) -> "Entering Passive Mode (h1,h2,h3,h4,p1,p2).";
response_code_string(230) -> "User logged in, proceed.";
response_code_string(250) -> "Requested file action okay, completed.";
response_code_string(257) -> "PATHNAME created.";
response_code_string(331) -> "User name okay, need password.";
response_code_string(332) -> "Need account for login.";
response_code_string(350) -> "Requested file action pending further information.";
response_code_string(421) -> "Service not available, closing control connection.";
response_code_string(425) -> "Can't open data connection.";
response_code_string(426) -> "Connection closed; transfere aborted.";
response_code_string(450) -> "Requested file action not taken.";
response_code_string(451) -> "Requested action not taken: local error in processing.";
response_code_string(452) -> "Requested action not taken.";
response_code_string(500) -> "Syntax error, command unrecognized.";
response_code_string(501) -> "Syntax error in parameters or arguments.";
response_code_string(502) -> "Command not implemented.";
response_code_string(503) -> "Bad sequence of commands.";
response_code_string(504) -> "Command not implemented for that parameter.";
response_code_string(530) -> "Not logged in.";
response_code_string(532) -> "Need account for storing files.";
response_code_string(550) -> "Requested action not taken.";
response_code_string(551) -> "Requested action aborted: page type unkown.";
response_code_string(552) -> "Requested file action aborted.";
response_code_string(553) -> "Requested action not taken.";
response_code_string(_) -> "N/A".


%% ====================================================================
%% Tests 
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(INTEGRATION_TEST, true).

-define(IP, "127.0.0.1").
-define(PORT_CMD, 22).
-define(PORT_DATA, 21).

-define(T(TestName), {atom_to_list(TestName), fun TestName/0}).

-ifdef(INTEGRATION_TEST).
skyraid_ranch_ftp_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			?T(connect_tc),
			?T(login_normal_tc),
			?T(login_invalid_user_tc),
			?T(login_invalid_password_tc),
			?T(logout_tc),
			?T(pwd_tc),
			?T(put_normal_tc),
			?T(put_chunked_normal_tc),
			?T(get_normal_tc)
		] 
	}.

setup() ->
	application:set_env(skyraid_ranch, ftp_ip, ?IP),
	application:set_env(skyraid_ranch, ftp_command_port, ?PORT_CMD),
	application:set_env(skyraid_ranch, ftp_data_port, ?PORT_DATA),
	inets:start(),
	ok = application:start(skyraid_ranch).

teardown(_Any) ->
	application:stop(skyraid_ranch).

connect_tc() ->
	{ok, _Pid} = inets:start(ftpc, [{host, ?IP}, {port, ?PORT_CMD}]).

login_normal_tc() ->
	{ok, Pid} = inets:start(ftpc, [{host, ?IP}, {port, ?PORT_CMD}]),
	?assertEqual(ok, ftp:user(Pid, <<"Adam">>, <<"test">>)).

login_invalid_user_tc() ->
	{ok, Pid} = inets:start(ftpc, [{host, ?IP}, {port, ?PORT_CMD}]),
	?assertEqual({error, euser}, ftp:user(Pid, <<"NotFoundUser">>, <<"test">>)).

login_invalid_password_tc() ->
	{ok, Pid} = inets:start(ftpc, [{host, ?IP}, {port, ?PORT_CMD}]),
	?assertEqual({error, euser}, ftp:user(Pid, <<"Adam">>, <<"blasdsad">>)).

logout_tc() ->
	{ok, Pid} = inets:start(ftpc, [{host, ?IP}, {port, ?PORT_CMD}]),
	ok = ftp:user(Pid, <<"Adam">>, <<"test">>),
	?assertEqual(ok, ftp:close(Pid)).

%%ls_tc() ->
%%	{ok, Pid} = inets:start(ftpc, [{host, ?IP}, {port, ?PORT_CMD}]),
%%	ok = ftp:user(Pid, <<"Adam">>, <<"test">>),
%%	{ok, _Listing} = ftp:ls(Pid).

pwd_tc() ->
	{ok, Pid} = inets:start(ftpc, [{host, ?IP}, {port, ?PORT_CMD}]),
	ok = ftp:user(Pid, <<"Adam">>, <<"test">>),
	{ok, _Listing} = ftp:pwd(Pid).

put_normal_tc() ->
	{ok, Pid} = inets:start(ftpc, [{host, ?IP}, {port, ?PORT_CMD}]),
	ok = ftp:user(Pid, <<"Adam">>, <<"test">>),
	ok = ftp:send(Pid, "../test/data/file1.txt").

put_chunked_normal_tc() ->
	{ok, Pid} = inets:start(ftpc, [{host, ?IP}, {port, ?PORT_CMD}]),
	ok = ftp:user(Pid, <<"Adam">>, <<"test">>),
	ok = ftp:send_chunk_start(Pid, "chunky.txt"),
	ok = ftp:send_chunk(Pid, <<"Rad1\n">>),
	ok = ftp:send_chunk(Pid, <<"Rad2\n">>),
	ok = ftp:send_chunk(Pid, <<"Rad4\n">>),
	ok = ftp:send_chunk_end(Pid).

get_normal_tc() ->
	{ok, _} = file:copy("../test/data/chunked.txt", "data/test/User/get.txt"),
	{ok, Pid} = inets:start(ftpc, [{host, ?IP}, {port, ?PORT_CMD}]),
	ok = ftp:user(Pid, <<"Adam">>, <<"test">>),
	{ok, <<"1\r\n2\r\n3\r\n4\r\n5\r\n6\r\n7\r\n8\r\n9\r\n10">>} = ftp:recv_bin(Pid, <<"get.txt">>).

-endif.

response_string_test() ->
	Msg = "MyMessage 3434",
	Expected = "230 " ++ Msg ++ "\r\n",
	Expected = response_string(230, Msg).


cmd_parse_test() ->
	[ {"USER", [<<"Adam">>]} ] = parse_commands(<<"USER Adam\r\n">>),
	[ {"USER", [<<"Adam">>]}, {"CMD", [<<"1">>, <<"2">>]} ] = parse_commands(<<"USER Adam\r\nCMD 1 2\r\n">>).

 -endif.
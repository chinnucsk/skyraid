-module(skyraid_ranch_ftp_data).
-export([start_pasv/0, stop_pasv/1, connect/1, put_file/3, get_file/3, list_files/2]).

-include("../../skyraid/include/skyraid.hrl").
 
-record(state, 
{
	mode, 			%% the mode this connection is in either active of pasv(passive)
	ip, 			%% the ip of the connection, if pasv will be the ip of the local machine, active the client machine
	port, 			%% the port, if pasv the port open on the local machine, if active the port to connect to on the client machine 
	listen_socket, 	%% The listener socket is passive mode
	data_socket 	%% The data socket used for the traffic.
}).

start_pasv() ->
	?DEBUG("Starting pasv data port"),
	case gen_tcp:listen(0,  [{active, false}, {packet, raw}, {exit_on_close, false}, binary]) of
        {ok, ListenSock} ->
            {ok, {Address, Port}} = inet:sockname(ListenSock),
            FtpAddress = format_address(Address, Port),
    		{ok, FtpAddress, #state{mode=passive, port=Port, listen_socket=ListenSock}};
        {error,Reason} ->
            {error,Reason}
    end.

stop_pasv(#state{listen_socket=ListenSocket} = S) ->
	?DEBUG("Closing pasv data port"),
	ok = gen_tcp:close(ListenSocket),
	{ok, S#state{listen_socket=undefined}}.
 
connect(#state{listen_socket=ListenSocket} = S) ->
	?DEBUG("Start listening for connections on data port"),
	case gen_tcp:accept(ListenSocket) of
		{ok, DataSocket} ->
			?DEBUG({"Client connected on data port", DataSocket}),
			{ok, S#state{data_socket=DataSocket}};
    	{error, Error} ->
    		{error, Error}
    end.

put_file(Session, FileName, S) ->
	BaseName = filename:basename(FileName),
	?DEBUG(BaseName),
	case connect(S) of 
		{ok, #state{data_socket=DataSocket}} ->
			{ok , Bin} = do_recv(DataSocket, []),
			do_close(DataSocket),
			AccountID = {"0","0"},
			skyraid:file_write(Session, AccountID, BaseName, Bin, []),
			{ok, S};
		Any -> Any
	end.

get_file(Session, FileName, S) ->
	BaseName = filename:basename(FileName),
	?DEBUG(BaseName),
	case connect(S) of 
		{ok, #state{data_socket=DataSocket} = NewState} ->
			%% TODO fetch account id from file path
			AccountID = {"0","0"},
			case skyraid:file_read(Session, AccountID, BaseName, []) of
				{ok, Bin} -> 
					case do_send(DataSocket, Bin, NewState) of 
						ok ->
							do_close(DataSocket), 
							{ok, S};
						{error, Error} -> {error, Error}
					end;
				{error, Error} -> {error, Error}
			end;
		{error, Error} -> {error, Error}
	end.

list_files(_Session, S) ->
	case connect(S) of 
		{ok, #state{data_socket=DataSocket} = NewState} ->
			do_send(DataSocket, <<"-rwxrwxrwx 1 stefan stefa 2200 Jan  1  1970 serverflags.txt">>, NewState),
			do_close(DataSocket),
			{ok, S};
		{error, Error} -> {error, Error}
	end.


do_recv(Socket, Bs) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, B} ->
            do_recv(Socket, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.

do_send(Socket, Bin, _S) ->
	?DEBUG({do_send, Bin}),
	gen_tcp:send(Socket, Bin).

do_close(Socket) ->
	gen_tcp:close(Socket).
    
format_address(_Address, Port) ->
	{S1,S2,S3,S4} = {127,0,0,1},
	P1 = Port div 256,
	P2 = Port rem 256,
	[S1,S2,S3,S4,P1,P2].

%% ====================================================================
%% Unit Tests 
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_pasv_test() ->
	{ok, [_S1,_S2,_S3,_S4,_P1,_P2], State} = start_pasv(),
	{ok, #state{listen_socket=undefined}} = stop_pasv(State).

connect_pasv_test() ->
	{ok, [S1,S2,S3,S4,P1,P2], #state{data_socket=undefined}=S} = start_pasv(),
	Ip = {S1,S2,S3,S4},
	Port = P1*256 + P2,
	spawn_link(fun()-> gen_tcp:connect(Ip, Port, []) end),
	{ok, #state{data_socket=DataSocket}} = connect(S),
	?assert(DataSocket =/= undefined).

-endif.
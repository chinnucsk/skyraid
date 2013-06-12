%%% ----------------------------------------------------------------------------
%%% guards.hrl
%%% ----------------------------------------------------------------------------
%%

-ifndef(__GUARDS_HRL_INCLUDED).
-define(__GUARDS_HRL_INCLUDED).

%% Useful stuff from Klarna's tulib.


-define(is_ip_address(IP),
	(is_tuple(IP)
	 andalso (size(IP) =:= 4)
         andalso (0 =< element(1, IP) andalso element(1, IP) =< 255)
         andalso (0 =< element(2, IP) andalso element(2, IP) =< 255)
         andalso (0 =< element(3, IP) andalso element(3, IP) =< 255)
         andalso (0 =< element(4, IP) andalso element(4, IP) =< 255))).

-define(is_ip_port(Port),
        (is_integer(Port) andalso (0 =< Port andalso Port =< 65535))).

-define(is_string(Str),
        (Str =:= "" orelse (is_list(Str) andalso is_integer(hd(Str))))).




-endif. % __GUARDS_HRL_INCLUDED

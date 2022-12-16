%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(nodelog). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(SERVER,nodelog_server).


%% External exports
-export([
	 read/1,
	 log/4,
	 create/1,
	 ping/0
	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Application handling
%% ====================================================================

%% ====================================================================
%% Support functions
%% ====================================================================
%%---------------------------------------------------------------
%% Function:all_specs()
%% @doc: all service specs infromation       
%% @param: non 
%% @returns:State#state.service_specs_info
%%
%%---------------------------------------------------------------
read(LogLevel)->
    gen_server:call(?SERVER, {read,LogLevel},infinity).


create(LogFile)->
    gen_server:call(?SERVER, {create,LogFile},infinity).


log(Level,ModuleString,Line,Msg)-> 
    gen_server:cast(?SERVER, {log,Level,ModuleString,Line,Msg}).

%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(lib_logger).   
 
-export([
	 parse/1,
	 log/4,
	 create_logger/1
	]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
% {erlang:system_time(microsecond),ModuleString,Line,MsgAsString}
parse(ListRaw)->
    [parse_item(Item)||Item<-ListRaw].

parse_item({SysTime,ModuleStr,Line,MsgAsString})->
    TimeUnit=microsecond,
    {{Y,M,D},{H,Mi,S}}=calendar:system_time_to_local_time(SysTime, TimeUnit),
    Year=integer_to_list(Y),
    Month=integer_to_list(M),
    Day=integer_to_list(D),
    Hour=integer_to_list(H),
    Min=integer_to_list(Mi),
    Sec=integer_to_list(S),
    LineStr=integer_to_list(Line),
    DateTime=Year++"-"++Month++"-"++Day++" | "++Hour++":"++Min++":"++Sec++" | ",
    Text=DateTime++ModuleStr++"/ "++LineStr++" | "++MsgAsString,
    Text.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
log(notice,ModuleString,Line,Msg)->
    logger:notice(Msg,#{file=>ModuleString,line=>Line});
log(warning,ModuleString,Line,Msg)->
    logger:warning(Msg,#{file=>ModuleString,line=>Line});
log(alert,ModuleString,Line,Msg)->
    logger:alert(Msg,#{file=>ModuleString,line=>Line}).
    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
%create_logger(LogDir)->
create_logger(LogFile)->
    Result=case logger:add_handler(my_standar_disk_h, logger_std_h,
			  #{formatter => {logger_formatter,
					  #{ template => [time," | ", file," | ",line," | ",level," | ",msg,"\n"]}}}) of
	       {error,Reason}->
		   {error,["Error when creating LogFile :",Reason,?MODULE,?LINE]};
	       ok->
		   case logger:add_handler(my_disk_log_h, logger_disk_log_h,
			  #{
			    config => #{file => LogFile,
					type => wrap,
					max_no_files => 4,
					max_no_bytes =>1000*100,
					filesync_repeat_interval => 1000},
			    formatter => {logger_formatter,
					  #{ template => [time," | ", file," | ",line," | ",level," | ",msg,"\n"]}}}) of
		       {error,Reason}->
			   {error,["Error when creating LogFile :",Reason,?MODULE,?LINE]};
		       ok-> 
			   ok
		   end
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

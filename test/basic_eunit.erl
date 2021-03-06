%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface §
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(basic_eunit).   
 
-export([start/0
	]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    ok=t1_test(),

%    init:stop(),
    ok.


setup_test()->
    ok.

t1_test()->
    os:cmd("rm -rf cluster1"),
    ok=file:make_dir("cluster1"),
    ok=file:make_dir("cluster1/logs"),
    
    LogFile1=filename:join(["cluster1","logs","test1.logs"]),
 %   LogDir="logs",
    Node=node(),
    ok=rpc:call(node(),application,start,[nodelog],5000),
    ok=rpc:call(node(),nodelog_server,create,[LogFile1],5000),
    true=rpc:cast(node(),nodelog_server,log,[notice,?MODULE_STRING,?LINE,"notice1"]),
    true=rpc:cast(node(),nodelog_server,log,[warning,?MODULE_STRING,?LINE,"warning1"]),
    true=rpc:cast(node(),nodelog_server,log,[alert,?MODULE_STRING,?LINE,"alert1"]),

    Term={error,[eexists,{?MODULE,?LINE},time()]},
    R= io_lib:format("~p",[Term]),
    TermAsStering=lists:flatten(R),
  
    true=rpc:cast(node(),nodelog_server,log,[alert,?MODULE_STRING,?LINE,TermAsStering]),

    
    Arg="-pa ebin  -setcookie test_cookie", 
    NewVm="96X23",
     {_VmId,HostId}=misc_node:vmid_hostid(Node),    
    {ok,N1}=slave:start(HostId,NewVm,Arg),
    pong=net_adm:ping(N1),

    LogFile2=filename:join(["cluster1","logs","test2.logs"]),
    
    ok=rpc:call(N1,application,start,[nodelog],5000),
    ok=rpc:call(N1,nodelog_server,create,[LogFile2],5000),
    true=rpc:cast(N1,nodelog_server,log,[notice,?MODULE_STRING,?LINE,"notice2"]),
    true=rpc:cast(N1,nodelog_server,log,[warning,?MODULE_STRING,?LINE,"warning2"]),
    true=rpc:cast(N1,nodelog_server,log,[alert,?MODULE_STRING,?LINE,"alert2"]),
    
    ok.

stop_test()->
   % init:stop(),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

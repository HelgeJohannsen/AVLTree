%%%-------------------------------------------------------------------
%%% @author Helge
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Dez 2017 11:51
%%%-------------------------------------------------------------------
-module(runtime).
-author("Helge").

%% API
-export([main/0]).


main() -> util:setglobalvar(left,1),
util:getglobalvar(left).
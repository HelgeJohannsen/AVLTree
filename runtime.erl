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


main() -> testInsertAndPrint(100).

testInsertAndPrint(Counts) ->
  List = util:randomliste(Counts),
  io:format("Liste: ~p~n",[List]),
  BT = avltree:initBT(),
  BTree = insert(BT,util:randomliste(Counts)),
  avltree:printBT(BTree, test1).


insert(Btree,[]) -> Btree;
insert(BTree,[H|Tail]) -> insert(avltree:insertBT(BTree,H),Tail).
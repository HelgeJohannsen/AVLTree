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


main() ->
  ListToDelete = [8,4,13,7,10,19,5,1,16],
  ListToDelete2 = [8,4,13,7,10,19,5,1,16],
  testDelete(10000), printRotations().



testInsertPrintDeletePrint(Number) ->
  List = util:randomliste(Number),
  List2 = util:randomlisteD(floor(Number*0.58),0,Number),
  BT = avltree:initBT(),
  BTN = insertList(BT,List),
  avltree:printBT(BTN, btN),
  BTD = deleteList(BTN,List2),
  avltree:printBT(BTD, btD),
  avltree:isBT(BTD).

printRotations() ->   io:format("Linksrotation: ~p",[util:getglobalvar(left)]),
                      io:format(" Rechtsrotation: ~p",[util:getglobalvar(right)]),
                      io:format(" Doppellinksrotation: ~p",[util:getglobalvar(dleft)]),
                      io:format(" Doppelrechtsrotation: ~p",[util:getglobalvar(dright)]).

testInsert(Number) -> List = util:randomliste(Number), testInsertTimeAVLTree(List),testInsertTimeBTree(List).

testInsertTimeBTree(List) ->
  StartTime = erlang:timestamp(),
  insertListBTree(btree:initBT(),List),
  StopTime = erlang:timestamp(),
  Duration = round(timer:now_diff(StopTime,StartTime)/1000),
  io:format("Dauer BT in ms: ~p~n",[Duration]).

testInsertTimeAVLTree(List) ->
  StartTime = erlang:timestamp(),
  insertList(avltree:initBT(),List),
  StopTime = erlang:timestamp(),
  Duration = round(timer:now_diff(StopTime,StartTime)/1000),
  io:format("Dauer AVL in ms: ~p~n",[Duration]).

insertListavl(Btree,[]) -> Btree;
insertListavl(BTree,[H|Tail]) -> insertListavl(avl:insertBT(BTree,H),Tail).


testDelete(Number) ->
  List = util:randomliste(Number),
  List2 = util:randomliste(Number),
  BTree= btree:initBT(),
  BTreeF = insertListBTree(BTree,List),
  AVLTree = avltree:initBT(),
  AVLTreeF = insertList(AVLTree,List),
  testDeleteTime(BTreeF,List2),
  testDeleteTime(AVLTreeF,List2).


testDeleteTime(BT, List) ->
  StartTime = erlang:timestamp(),
  BTN = deleteList(BT,List),
  StopTime = erlang:timestamp(),
  Duration = round(timer:now_diff(StopTime,StartTime)/1000),
  io:format("Dauer in ms: ~p~n",[Duration]),BTN.

insertListBTree(Btree,[]) -> Btree;
insertListBTree(BTree,[H|Tail]) -> insertListBTree(btree:insertBT(BTree,H),Tail).

insertList(Btree,[]) -> Btree;
insertList(BTree,[H|Tail]) -> insertList(avltree:insertBT(BTree,H),Tail).

deleteList(BTree,[]) -> BTree;
deleteList(BTree,[H|Tail]) -> deleteList(avltree:deleteBT(BTree,H),Tail).

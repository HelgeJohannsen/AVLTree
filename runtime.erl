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


main() ->runCompareInsert().

runCompareInsert() -> runCompareInsert(1024).
runCompareInsert(ListSize) when ListSize =< 16777216 -> % 2^14
  Min = 10,
  Max = 9999999999,
  List = util:randomlisteD(ListSize,Min,Max),
  testInsertTimeAVLTree(List),testInsertTimeAVL2(List), printRotations(),printRotationsJan(),
  io:format("~nDone with ListSize ~p.~n~n", [ListSize]),
  runCompareInsert(ListSize * 2);
  runCompareInsert(_) -> io:format("~nDone.~n").

% Ausgabe der Rotationen
printRotations() ->   io:format("Linksrotation: ~p",[util:getglobalvar(left)]),
                      io:format(" Rechtsrotation: ~p",[util:getglobalvar(right)]),
                      io:format(" Doppellinksrotation: ~p",[util:getglobalvar(dleft)]),
                      io:format(" Doppelrechtsrotation: ~p~n",[util:getglobalvar(dright)]).

printRotationsJan() ->io:format("Linksrotation: ~p",[util:getglobalvar(leftrotate)]),
                      io:format(" Rechtsrotation: ~p",[util:getglobalvar(rightrotate)]),
                      io:format(" Doppellinksrotation: ~p",[util:getglobalvar(ddleftrotate)]),
                      io:format(" Doppelrechtsrotation: ~p",[util:getglobalvar(ddrightrotate)]).

testInsertTimeAVLTree(List) ->
  StartTime = erlang:timestamp(),
  insertList(avltree:initBT(),List),
  StopTime = erlang:timestamp(),
  Duration = round(timer:now_diff(StopTime,StartTime)/1000),
  io:format("Dauer AVL in ms: ~p~n",[Duration]).

insertList(Btree,[]) -> Btree;
insertList(BTree,[H|Tail]) -> insertList(avltree:insertBT(BTree,H),Tail).

testInsertTimeAVL2(List) ->
  avl2:initCounter(),
  StartTime = erlang:timestamp(),
  insertList2(avl2:initBT(),List),
  StopTime = erlang:timestamp(),
  Duration = round(timer:now_diff(StopTime,StartTime)/1000),
  io:format("Dauer AVL Vergleichsalgorithmus in ms: ~p~n",[Duration]).

insertList2(Btree,[]) -> Btree;
insertList2(BTree,[H|Tail]) -> insertList2(avl2:insertBT(BTree,H),Tail).

% Erzeugt eine Liste mit x grösse und eine zweite die die gleichen Elemente
% enthält aber nur 42% der Elemente enthält diese werden aus dem
% Baum gelöscht
testInsertPrintDeletePrint(Number) ->
  List = util:randomliste(Number),
  List2 = util:randomlisteD(floor(Number*0.58),0,Number),
  BT = avltree:initBT(),
  BTN = insertList(BT,List),
  avltree:printBT(BTN, btN),
  BTD = deleteList(BTN,List2),
  avltree:printBT(BTD, btD),
  avltree:isBT(BTD).

testInsertTimeBTree(List) ->
  StartTime = erlang:timestamp(),
  insertListBTree(btree:initBT(),List),
  StopTime = erlang:timestamp(),
  Duration = round(timer:now_diff(StopTime,StartTime)/1000),
  io:format("Dauer BT in ms: ~p~n",[Duration]).

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

deleteList(BTree,[]) -> BTree;
deleteList(BTree,[H|Tail]) -> deleteList(avltree:deleteBT(BTree,H),Tail).

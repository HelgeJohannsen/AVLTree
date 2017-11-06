%%%-------------------------------------------------------------------
%%% @author Helge
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Nov 2017 03:31
%%%-------------------------------------------------------------------
-module(avltree).
-author("Helge").

%% API
-export([main/0]).

main() ->	BT = initBT(),
  BT2 = insertBT(BT, 5),
  BT3 = insertBT(BT2, 9),
  BT4 = insertBT(BT3, 3),
  BT5 = insertBT(BT4, 6),
  BT6 = insertBT(BT5, 1),
  BT7 = insertBT(BT6, 4),
  BT8 = insertBT(BT7, 7),
  printBT(BT8, "Binary.dot"),
  isBalanced(BT8).




  printBT(Btree, FileName) ->
    util:logging(FileName, "digraph G {\n\t"),
    printLeaf(Btree, FileName),
    util:logging(FileName, "}").

printLeaf({btnode,_,_,btempty,btempty}, FileName) -> true;
printLeaf({btnode,Elem,_,L,btempty}, FileName) ->
  {btnode, LE, _,_,_} = L,
  util:logging(FileName, integer_to_list(Elem)),
  util:logging(FileName, " -> "),
  util:logging(FileName, integer_to_list(LE)),
  util:logging(FileName, ";\n\t"),
  printLeaf(L, FileName);
printLeaf({btnode,Elem,_,btempty, R}, FileName) ->
  {btnode, RE, _,_,_} = R,
  util:logging(FileName, integer_to_list(Elem)),
  util:logging(FileName, " -> "),
  util:logging(FileName, integer_to_list(RE)),
  util:logging(FileName, ";\n\t"),
  printLeaf(R, FileName);
printLeaf({btnode, Elem, _,L,R}, FileName) ->
    {btnode, LE, _,_,_} = L,
    {btnode, RE, _,_,_} = R,
    util:logging(FileName, integer_to_list(Elem)),
    util:logging(FileName, " -> "),
    util:logging(FileName, integer_to_list(LE)),
    util:logging(FileName, ";\n\t"),
    util:logging(FileName, integer_to_list(Elem)),
    util:logging(FileName, " -> "),
    util:logging(FileName, integer_to_list(RE)),
    util:logging(FileName, ";\n\t"),
    printLeaf(L, FileName),
  printLeaf(R, FileName).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Algorithmen und Datenstrukturen                       %%
%% Praktikumsaufgabe 1 : Die Kunst der Abstraktion       %%
%%                                                       %%
%% bearbeitet von Helge Johannsen und Christian Stüber   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementierung des ADT BTree                                              %%
%%                                                                            %%
%% BTreeform : {btnode, VAL, HEIGHT, LEFT, RIGHT}                             %%
%%             btempty , bei leerem Tree                                      %%
%%                                                                            %%
%% btnode = atomares Tag zur Identifizierung des BTree                        %%
%%                                                                            %%
%% VAL = Wert des Nodes, Zahl                                                 %%
%% HEIGHT = Hoehe des Baumes an der aktuellen Node                            %%
%%                                                                            %%
%% LEFT = rekursiv naechster Node, Nodes auf der linken Seite haben einen     %%
%%        niedrigeren Wert als der Parent-node                                %%
%% RIGHT = rekursiv naechster Node, Nodes auf der rechten Seite haben einen   %%
%%        hoeheren Wert als der Parent-node                                   %%
%%                                                                            %%
%% LEFT, RIGHT = btempty wenn es kein Children-node gibt                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Erzeugt einen leeren BTree.
%%
%% Signatur | initBT: (/) → btree
%%
initBT() -> btempty.

%%
%% Ueberprueft ob der BTree von korrekter syntaktischer Struktur ist und ob die Semantik (Sortierung und Hoehe) korrekt ist.
%%
%% Signatur | isBT: btree → bool
%%
isBT(btempty) 					-> true;
isBT(BT = {btnode, _, _, _, _}) -> isBT_helper(BT, {undef, undef});
isBT(_) -> false.
%%
isBT_helper(btempty, _) -> true;
isBT_helper({btnode, V, H, L, R} , LIMITS = {LOW, HIGH}) when is_number(V), is_number(H) -> %% Fehler behoben, merkt sich jetzt die Limits
  (case LIMITS of
     {undef, undef} 	-> true;
     {undef, HIGH} 	-> V < HIGH;
     {LOW, 	undef} 	-> LOW < V;
     {LOW, 	HIGH} 	-> (LOW < V) and (V < HIGH)
   end)
    andalso isBT_helper(L, {LOW, V})
    andalso isBT_helper(R, {V, HIGH})
    andalso (H == max(hoeheBT(L), hoeheBT(R)) + 1); %% Short-circuit-evaluating andalso to prevent max from err'ing if L or R are no BT's
isBT_helper(_, _) -> false.

isBalanced(btempty) -> true;
isBalanced({btnode,_,_,btempty,R}) ->
  {btnode, _, HR,_,_} = R,
  if(HR >1) ->
    false
  end;
isBalanced({btnode,_,_,L,btempty}) ->
  {btnode, _, HR,_,_} = L,
  if(HR >1) ->
    false
  end;
isBalanced({btnode,_,_,L,R}) ->
  {btnode, _, HL,_,_} = L,
  {btnode, _, HR,_,_} = R,
  Diff = HL - HR,
  if(Diff > 1) or (Diff < -1) ->
      false;
  true ->
         isBalanced(L),isBalanced(R)
    end.
%%
%% Fuegt das Element in den BTree ein,
%% es sind nur Zahlen als Werte erlaubt.
%%
%% Signatur | insertBT: btree x elem → btree
%%
insertBT(btempty, Elem) 					when is_number(Elem) -> {btnode, Elem, 1, btempty, btempty};
insertBT(N = {btnode, Elem, _, _, _}, Elem) when is_number(Elem) -> N;
insertBT({btnode, V, _, L, R}, Elem) 		when is_number(Elem) ->
  LN = case Elem < V of
         true  -> insertBT(L, Elem);
         false -> L
       end,
  RN = case Elem > V of
         true  -> insertBT(R, Elem);
         false -> R
       end,
  HN = max(hoeheBT(LN), hoeheBT(RN)) + 1,
  {btnode, V, HN, LN, RN};
insertBT(_, _) -> ok.

%%
%% Prueft ob ein BTree leer ist.
%%
%% Signatur | isEmptyBT: btree → bool
%%
isEmptyBT(btempty) 				-> true;
isEmptyBT({btnode, _, _, _, _}) -> false;
isEmptyBT(_) 					-> ok.

%%
%% Prueft ob zwei BTrees strukturell gleich sind.
%%
%% Signatur | equalBT: btree x btree → bool
%%
equalBT(btempty, btempty) 							-> true;
equalBT({btnode, _, _, _, _}, btempty) 				-> false;
equalBT(btempty, {btnode, _, _, _, _}) 				-> false;
equalBT({btnode, A, B, C, D}, {btnode, A, B, C, D}) -> true;
equalBT({btnode, _, _, _, _}, {btnode, _, _, _, _}) -> false;
equalBT(_, _) 										-> ok.

%%
%% Gibt die Hoehe des BTrees zurueck.
%%
%% Signatur | hoeheBT: btree → int
%%
hoeheBT(btempty) 				-> 0;
hoeheBT({btnode, _, H, _, _}) 	-> H;
hoeheBT(_) 						-> ok.

%%%%%%%%%%%%%%%%
%% Testfaelle %%
%%%%%%%%%%%%%%%%
test_initBT() ->
  BT = initBT(),
  RESULT = isEmptyBT(BT),
  io:format("test_initBT: ~p~n", [RESULT]).

test_isBT() ->
  BT = initBT(),
  BT2 = insertBT(BT, 42),
  BT3 = insertBT(BT2, 21),
  BT4 = insertBT(BT3, 80),
  BT5 = insertBT(BT4, 66),
  RESULT = isBT(BT)
    and isBT(BT2)
    and isBT(BT3)
    and isBT(BT4)
    and isBT(BT5)
    and not isBT(1)
    and not isBT(test)
    and not isBT({1, 2})
    and not isBT({btnode, notanumber, 1, btempty, btempty})
    and not isBT({btnode, 42, 2, {btnode, 99, 1, btempty, btempty}, btempty})
    and not isBT({btnode, 42, 2, {btnode, 99, 1, btempty, btempty}, 33})
    and not isBT({btnode, {btnode, 99, 1, btempty, btempty}, {btnode, 99, 1, btempty, btempty}, {btnode, 99, 1, btempty, btempty}, 33})
    and not isBT({btnode, 42, 2, btempty, {btnode, 21, 1, btempty, btempty}})
    and not isBT({btnode, 42, 1, {btnode, 21, 1, btempty, btempty}, btempty})
    and not isBT({btnode, 42, 0, btempty, btempty})
    and not isBT({btnode, 42, 5, {btnode, 21, 1, btempty, btempty}, {btnode, 80, 2, {btnode, 66, 1, btempty, btempty}, btempty}})
    and not isBT({btnode, 42, 3,
      {btnode, 21, 2, btempty, {btnode, 42, 1, btempty, btempty}}, %% 42 is wrong
      {btnode, 80, 2, {btnode, 66, 1, btempty, btempty}, btempty}})
    and not isBT({btnode, 42, 3,
      {btnode, 21, 1, btempty, btempty},
      {btnode, 80, 3, {btnode, 66, 2, {btnode, 21, 1, btempty, btempty}, btempty}, btempty}}) %% 21 is wrong
  ,
  io:format("test_isBT: ~p~n", [RESULT]).

test_insertBT() ->
  BT = initBT(),
  BT2 = insertBT(BT, 42),
  BT3 = insertBT(BT2, 21),
  BT4 = insertBT(BT3, 80),
  BT5 = insertBT(BT4, 66),
  RESULT = (BT == btempty)
    and (BT2 == {btnode, 42, 1, btempty, btempty})
    and (BT3 == {btnode, 42, 2, {btnode, 21, 1, btempty, btempty}, btempty})
    and (BT4 == {btnode, 42, 2, {btnode, 21, 1, btempty, btempty}, {btnode, 80, 1, btempty, btempty}})
    and (BT5 == {btnode, 42, 3, {btnode, 21, 1, btempty, btempty}, {btnode, 80, 2, {btnode, 66, 1, btempty, btempty}, btempty}}),
  io:format("test_insertBT: ~p~n", [RESULT]).

test_isEmptyBT() ->
  BT = initBT(),
  RESULT = isEmptyBT(BT)
    and (isEmptyBT(insertBT(BT, 42)) == false),
  io:format("test_isEmptyBT: ~p~n", [RESULT]).

test_equalBT() ->
  BT = initBT(),
  BT2 = insertBT(BT, 42),
  RESULT = (equalBT(BT, BT) == true)
    and (equalBT(BT2, BT2) == true)
    and (equalBT(BT, BT2) == false),
  io:format("test_equalBT: ~p~n", [RESULT]).

test_hoeheBT() ->
  BT = initBT(),
  BT2 = insertBT(BT, 42),
  BT3 = insertBT(BT2, 21),
  BT4 = insertBT(BT3, 80),
  BT5 = insertBT(BT4, 66),
  RESULT = (hoeheBT(BT) == 0)
    and (hoeheBT(BT2) == 1)
    and (hoeheBT(BT3) == 2)
    and (hoeheBT(BT4) == 2)
    and (hoeheBT(BT5) == 3),
  io:format("test_hoeheBT: ~p~n", [RESULT]).

%%%%%%%%%%%%%%%%%
%%	Entrypoint %%
%%%%%%%%%%%%%%%%%
run_tests() ->
  %% ADT BTree Testcases
  test_initBT(),
  test_isBT(),
  test_insertBT(),
  test_isEmptyBT(),
  test_equalBT(),
  test_hoeheBT().

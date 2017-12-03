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
-export([main/0, run_tests/0]).

main() ->	BT = initBT(),
    BT1 = insertBT(BT,5),
    BT2 = insertBT(BT1, 10),
    BT3 = insertBT(BT2, 15),
  BT4 = insertBT(BT3, 12),
BT5 = insertBT(BT4, 14),
BT6 = insertBT(BT5, 35),
BT7 = insertBT(BT6, 11),
BT8 = insertBT(BT7, 19),
BT9 = insertBT(BT8, 13),
BT10 = insertBT(BT9, 37),
BT11 = insertBT(BT10, 36),
BT12 = insertBT(BT11, 22),
BT13 = insertBT(BT12, 27).

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
  Diff = hoeheBT(L) - hoeheBT(R),
  (case LIMITS of
     {undef, undef} 	-> true;
     {undef, HIGH} 	-> V < HIGH;
     {LOW, 	undef} 	-> LOW < V;
     {LOW, 	HIGH} 	-> (LOW < V) and (V < HIGH)
   end)
    andalso isBT_helper(L, {LOW, V})
    andalso isBT_helper(R, {V, HIGH})
    andalso (H == max(hoeheBT(L), hoeheBT(R)) + 1);
isBT_helper(_, _) -> false.




isBalanced(btempty) -> true;
isBalanced({btnode,_,_,btempty,btempty}) -> true;
isBalanced({btnode,_,_,btempty,R}) ->
  {btnode, _, HR,_,_} = R,
  if(HR >1) ->
    false;
    true -> true
  end;
isBalanced({btnode,_,_,L,btempty}) ->
  {btnode, _, HR,_,_} = L,
  if(HR >1) ->
    false;
  true -> true
  end;
isBalanced({btnode,_,_,L,R}) ->
  Diff = hoeheBT(L) - hoeheBT(R),
  if(Diff > 1) or (Diff < -1) ->
      false;
  true ->
         isBalanced(L),isBalanced(R)
    end.

left_rotate(({btnode, VX, HX, A,{btnode, VY,HY ,B,C}})) ->
  io:format("rotation nach links: ~p~n",[VY]),
  {btnode, VY, HX, {btnode, VX, HY, A, B }, C}.
left_rotate({btnode, VX, HX, A,{btnode, VY,HY ,B,C}},E) ->
  io:format("rotation nach links: ~p~n",[VY]),
  {btnode, VY, HX, {btnode, VX, HY, A, B }, insertBT(C,E)}.


right_rotate({btnode, VY, HY,{btnode, VX, HX, A,B},C }) ->
  io:format("rotation nach rechts: ~p~n",[VY]),
  {btnode, VX, HY, A, {btnode, VY, HX, B,C}}.
right_rotate({btnode, VY, HY,{btnode, VX, HX, A,B},C },E) ->
  io:format("rotation nach rechts: ~p~n",[VY]),
  {btnode, VX, HY, insertBT(A,E), {btnode, VY, HX, B,C}}.

%% Fuegt das Element in den BTree ein,
%% es sind nur Zahlen als Werte erlaubt.
%%
%% Signatur | insertBT: btree x elem → btree
%%
%% dieser Fall stellt eine Links rotation dar
%%insertBT(W = {btnode,VW,HW,L,{btnode,VY, HY, _,_}}, Elem) when Elem > VY ->
%%  insertBT(left_rotate(X), Elem);


%% Linksrotation
%% dieser Fall stellt eine Doppelrotation Rechts dar, der Baum ist Linksgewichtig und wir möchten erneut links einfgueen
insertBT({btnode,VW,_,L ={btnode,VL, _, _,_}, btempty}, Elem) when ((Elem < VW) and (Elem > VL)) ->
  io:format("Doppelrotation nach rechts ab: ~p~n",[VW]),
  {btnode, Elem, 1, {btnode, VL, 0, btempty, btempty}, {btnode, VW, 0, btempty, btempty}};

%% dieser Fall stellt eine DoppelRotation nach links dar
insertBT({btnode,VW,_,btempty, {btnode,VR, _, _,_}}, Elem) when ((Elem > VW) and (Elem < VR)) ->
  io:format("Doppelrotation nach Links: ~p~n",[VW]),
  {btnode, Elem, 1, {btnode, VW, 0, btempty, btempty}, {btnode, VR, 0, btempty, btempty}};

insertBT(btempty, Elem)                    when is_number(Elem) -> {btnode, Elem, 0, btempty, btempty};
insertBT({btnode, V, H, L,btempty},Elem)  when (Elem>V) -> {btnode, V, H, L,{btnode, Elem, 0, btempty, btempty}};
insertBT({btnode, V, H, btempty,R},Elem)  when (Elem<V) -> {btnode, V, H, {btnode, Elem, 0, btempty, btempty},R};
insertBT({btnode, V, H, L,R},Elem)  when (Elem>V) ->
  {btnode, VR,HR ,LR,RR} = R,
  Diff = hoeheBT(L) - hoeheBT(R),
  HN = max(hoeheBT(L), hoeheBT(R)) + 1,
  if (Diff<0)
      -> left_rotate(insertBT(RR, Elem));
    true ->{btnode, V, HN, L, insertBT(R, Elem)}
  end;
insertBT({btnode, V, H, L,R},Elem)  when (Elem<V) ->
  {btnode, VL,HL ,LL,RL} = L,
  Diff = hoeheBT(L) - hoeheBT(R),
  HN = max(hoeheBT(L), hoeheBT(R)) + 1,
  if (Diff<0)
    -> {btnode, VL, HN, insertBT(RL, Elem),{btnode, V,H,R,LL}};
    true ->{btnode, V, HN, insertBT(L, Elem), R}
  end.







valueNode(btempty) -> 0;
valueNode({btnode, V, _, _, _}) -> V.



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
hoeheBT(btempty) 				-> -1;
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
    and not isBT({btnode, 42, 1, {btnode, 99, 0, btempty, btempty}, btempty})
    and not isBT({btnode, 42, 1, {btnode, 99, 0, btempty, btempty}, 33})
    and not isBT({btnode, {btnode, 99, 1, btempty, btempty}, {btnode, 99, 1, btempty, btempty}, {btnode, 99, 1, btempty, btempty}, 33})
    and not isBT({btnode, 42, 1, btempty, {btnode, 21, 0, btempty, btempty}})
    and not isBT({btnode, 42, 1, {btnode, 21, 1, btempty, btempty}, btempty})
    and not isBT({btnode, 42, -1, btempty, btempty})
    and not isBT({btnode, 42, 5, {btnode, 21, 1, btempty, btempty}, {btnode, 80, 2, {btnode, 66, 1, btempty, btempty}, btempty}})
    and not isBT({btnode, 42, 3,
      {btnode, 21, 2, btempty, {btnode, 42, 1, btempty, btempty}}, %% 42 is wrong
      {btnode, 80, 2, {btnode, 66, 1, btempty, btempty}, btempty}})
    and not isBT({btnode, 42, 3,
      {btnode, 21, 1, btempty, btempty},
      {btnode, 80, 2, {btnode, 66, 1, {btnode, 21, 0, btempty, btempty}, btempty}, btempty}}) %% 21 is wrong
  ,
  io:format("test_isBT: ~p~n", [RESULT]).

test_left_rotate() ->
  BT = initBT(),
  BT2 = insertBT(BT, 4),
  BT3 = insertBT(BT2, 7),
  BT4 = insertBT(BT3, 5),
  BT5 = insertBT(BT4, 8),
  BT6 = insertBT(BT5, 2),
  BT2A = insertBT(BT, 7),
  BT3A = insertBT(BT2A, 8),
  BT4A = insertBT(BT3A, 4),
  BT5A = insertBT(BT4A, 5),
  BT6A = insertBT(BT5A, 2),
  Result = equalBT(left_rotate(BT6), BT6A),
  io:format("test_left_rotate: ~p~n", [Result]).

test_right_rotate() ->
  BT = initBT(),
  BT2 = insertBT(BT, 4),
  BT3 = insertBT(BT2, 7),
  BT4 = insertBT(BT3, 5),
  BT5 = insertBT(BT4, 8),
  BT6 = insertBT(BT5, 2),
  BT2A = insertBT(BT, 7),
  BT3A = insertBT(BT2A, 8),
  BT4A = insertBT(BT3A, 4),
  BT5A = insertBT(BT4A, 5),
  BT6A = insertBT(BT5A, 2),
  Result = equalBT(right_rotate(BT6A), BT6),
  io:format("test_right_rotate: ~p~n", [Result]).

test_isBalanced() ->
  BT = initBT(),
  BT1 = insertBT(BT,5 ),
  BT2 = insertBT(BT1,8 ),
  BT3 = insertBT(BT2,9 ),
  BT4 = insertBT(BT3,11 ).

test_insertBT() ->
  BT = initBT(),
  BT2 = insertBT(BT, 42),
  BT3 = insertBT(BT2, 21),
  BT4 = insertBT(BT3, 80),
  BT5 = insertBT(BT4, 66),
  RESULT = (BT == btempty)
    and (BT2 == {btnode, 42, 0, btempty, btempty})
    and (BT3 == {btnode, 42, 1, {btnode, 21, 0, btempty, btempty}, btempty})
    and (BT4 == {btnode, 42, 1, {btnode, 21, 0, btempty, btempty}, {btnode, 80, 0, btempty, btempty}})
    and (BT5 == {btnode, 42, 2, {btnode, 21, 0, btempty, btempty}, {btnode, 80, 1, {btnode, 66, 0, btempty, btempty}, btempty}}),
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
  RESULT = (hoeheBT(BT) == -1)
    and (hoeheBT(BT2) == 0)
    and (hoeheBT(BT3) == 1)
    and (hoeheBT(BT4) == 1)
    and (hoeheBT(BT5) == 2),
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
  test_hoeheBT(),
  test_left_rotate(),
  test_right_rotate().

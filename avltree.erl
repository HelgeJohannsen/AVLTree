%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Algorithmen und Datenstrukturen                       %%
%% Praktikumsaufgabe 3 : Rekursion auf Bäumen            %%
%%                                                       %%
%% bearbeitet von Helge Johannsen                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementierung des ADT AVLTree                                            %%
%%                                                                            %%
%% AVLTree : {btnode, VAL, HEIGHT, LEFT, RIGHT}                               %%
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

-module(avltree).
-author("Helge").

%% API
-export([initBT/0, isBT/1,insertBT/2,deleteBT/2,printBT/2, run_tests/0]).

%% Erzeugt einen leeren BTree.
%% Initialisiert glabale Variablen zum zählen der Rotationen
initBT() ->
  util:setglobalvar(left,0),
  util:setglobalvar(right,0),
  util:setglobalvar(dleft,0),
  util:setglobalvar(dright,0),
  btempty.

%% Prueft ob ein BTree leer ist.
%% Signatur | isEmptyBT: btree → bool
isEmptyBT(btempty)        -> true;
isEmptyBT({btnode, _, _, _, _}) -> false;
isEmptyBT(_)          -> ok.

%% Prueft ob zwei BTrees strukturell gleich sind.
%% Signatur | equalBT: btree x btree → bool
equalBT(btempty, btempty)               -> true;
equalBT({btnode, _, _, _, _}, btempty)        -> false;
equalBT(btempty, {btnode, _, _, _, _})        -> false;
equalBT({btnode, A, B, C, D}, {btnode, A, B, C, D}) -> true;
equalBT({btnode, _, _, _, _}, {btnode, _, _, _, _}) -> false;
equalBT(_, _)                     -> ok.


%% Ueberprueft ob der BTree von korrekter syntaktischer Struktur ist,
%% ob die Semantik (Sortierung und Hoehe) korrekt ist und
%% ob der übergebene Baum balanciert ist.
isBT(btempty)     -> true;
isBT(BT = {btnode, _, _, _, _}) -> isBT_helper(BT, {undef, undef});
isBT(_) -> false.
%%
isBT_helper(btempty, _) -> true;
isBT_helper(W = {btnode, V, H, L, R} , LIMITS = {LOW, HIGH}) when is_number(V), is_number(H) ->
%% Hilfsfunktion die den Balance Faktor des Baumes ermittelt.
  Balance = directionBT(W),
  (case LIMITS of
     {undef, undef}   -> true;
     {undef, HIGH}  -> V < HIGH;
     {LOW,  undef}  -> LOW < V;
     {LOW,  HIGH}   -> (LOW < V) and (V < HIGH)
   end)
% Diese Abfrage testet ob das AVL Kriterium der balancierten Bäume eingehalten wird
    andalso ((Balance == links) or (Balance == rechts) or (Balance == balanced))
    andalso isBT_helper(L, {LOW, V})
    andalso isBT_helper(R, {V, HIGH})
  %% Short-circuit-evaluating andalso to prevent max from err'ing if L or R are no BT's
    andalso (H == max(hoeheBT(L), hoeheBT(R)) + 1);
isBT_helper(_, _) -> false.

%% Rekursionsabbruch sobald wir das Element auf ein leeres Blatt packen können.
%% Die Höhe wird mit 0 initialisiert 
insertBT(btempty, Elem) when is_number(Elem) -> {btnode, Elem, 0, btempty, btempty};
insertBT({btnode, V, _H, L,R},Elem)->
  LN = case Elem < V of
         true  -> insertBT(L, Elem);
         false -> L
       end,
  RN = case Elem > V of
         true  -> insertBT(R, Elem);
         false -> R
       end,
  HN = max(hoeheBT(LN), hoeheBT(RN)) + 1,
  Z = {btnode, V, HN, LN, RN},
  balance(Z).

balance(btempty) -> btempty;
balance(W = {btnode, V, H, L,R}) ->
  WW = directionBT(W),
  WR = directionBT(R),
  WL = directionBT(L),
  case {WW,WR,WL} of
    {linkslinks ,_,rechts} ->   CDR = util:getglobalvar(dright),util:setglobalvar(dright, CDR +1),right_rotate({btnode, V,H,left_rotate(L),R});
    {rechtsrechts,links,_} -> CDL = util:getglobalvar(dleft),util:setglobalvar(dleft, CDL +1),left_rotate({btnode, V, H, L, right_rotate(R)});
    {rechtsrechts,_,_} -> left_rotate(W);
    {linkslinks,_,_} -> right_rotate(W);
    {_,_,_} -> W
  end.

% besitzt das zu löschende Element keine Kinder wird der btempty token ausgegeben
deleteBT({btnode, V,_,btempty,btempty},V) -> btempty;
% besitzt das zu löschende Element nur ein Kind kann der momentane Knoten durch das noch vorhandene Kind ersetzt werden
% die hoehe muss nicht angepasst werden.
deleteBT({btnode, V,_,L,btempty},V) -> L;
deleteBT({btnode, V,_,btempty,R},V) -> R;
% besitzt das zu loeschende Element zwei Kinder tritt dieser Fall ein
deleteBT({btnode, V,_,L,R},V) ->
  Diff = hoeheBT(L) - hoeheBT(R),
  case (Diff<0) of
    true-> VN = lowestValueBT(R),LN = L, RN = deleteBT(R,lowestValueBT(R));
    false-> VN = highestValueBT(L),LN = deleteBT(L,highestValueBT(L)), RN = R
  end,
  HN = max(hoeheBT(LN), hoeheBT(RN)) + 1,
  {btnode, VN, HN, LN, RN};

% Aufbau des Baumes sollte das Element noch nicht gefunden sein.
deleteBT(btempty,_) -> btempty;
deleteBT({btnode, V,_,L,R},Elem) ->
  LN = case Elem < V of
         true  -> deleteBT(L, Elem);
         false -> L
       end,
  RN = case Elem > V of
         true  -> deleteBT(R, Elem);
         false -> R
       end,
  HN =  max(hoeheBT(LN), hoeheBT(RN)) + 1,
  balance({btnode, V, HN, LN, RN}).

% Hilfsmethode fuer deleteBT.
highestValueBT({btnode, V,_,_,btempty}) -> V;
highestValueBT({btnode, _,_,_,R}) -> highestValueBT(R).
lowestValueBT({btnode, V,_,btempty,_}) -> V;
lowestValueBT({btnode, _,_,L,_}) -> lowestValueBT(L).

% eine Linksrotation, alle höhen werden aktualisiert und der left zähler hochgezählt.
left_rotate(({btnode, VX, _HX, A,{btnode, VY,_HY ,B,C}})) ->
  CLR = util:getglobalvar(left),util:setglobalvar(left, CLR +1),
  NHX = (max(hoeheBT(A),hoeheBT(B))+1),
  NHY = max(NHX,hoeheBT(C))+1,
  {btnode, VY, NHY, {btnode, VX, NHX, A, B }, C}.
% eine Rechtsrotation, alle höhen werden aktualisiert und der right zähler hochgezählt.
right_rotate({btnode, VY, _HY,{btnode, VX, _HX, A,B},C }) ->
  CRR = util:getglobalvar(right),util:setglobalvar(right, CRR +1),
  NHY = (max(hoeheBT(B),hoeheBT(C))+1),
  NHX = (max(NHY,hoeheBT(A))+1),
  {btnode, VX, NHX, A, {btnode, VY, NHY, B,C}}.

%% diese Methode gibt mit Hilfe des Balance Faktors an, ob und wenn ja wie balanciert werden muss.
%% Sollte das linke oder rechte Kind des Knoten kein BST sein gibt hoeheBT ein falls zurück,
%% das abgefangen und an die übergeordneten Funktionen übergeben wird.
directionBT({btnode, _,_,L,R}) ->
  LN = hoeheBT(L), RN = hoeheBT(R),
  if(is_number(LN) and is_number(RN))
     -> directionBT(LN-RN);
    true -> false
  end;

%% Die Funktionen wandeln den Balance Faktor in token um,
%% Die Funktionen die darauf zugreifen brauchen nur wissen ob der BST leicht oder stark 
%% gewichtig ist linkslinks und rechtstechts stehe für ein Ungleichgewicht das
%% die AVL Kriterien stört.    
directionBT(BalanceFactor) when BalanceFactor > 1 -> linkslinks;
directionBT(BalanceFactor) when BalanceFactor < -1 -> rechtsrechts;
directionBT(BalanceFactor) when BalanceFactor =:= 1 -> links;
directionBT(BalanceFactor) when BalanceFactor =:= -1 -> rechts;
directionBT(_)  -> balanced.


%% Gibt die Hoehe des BTrees zurueck.
%% Signatur | hoeheBT: btree → int
%% im Fehlerfall false
hoeheBT(btempty)        -> -1;
hoeheBT({btnode, _, H, _, _})   -> H;
hoeheBT(_)            -> false.

%% speichert einen Binärbaum in einer Datei namens des Parameters FileName
%% ist die Datei bereits vorhanden wird in die vorhandene Datei geschrieben
printBT(Btree, FileName) ->
  util:logging(FileName, "digraph G {\n\t"),
  printLeaf(Btree, FileName),
  util:logging(FileName, "}").
%% Rekursionsabbruch
printLeaf({btnode,_,_,btempty,btempty}, _FileName) -> true;
%% speichert das linke Blatt und ruft die Funktion rekursiv auf
printLeaf({btnode,Elem,_,L,btempty}, FileName) ->
  {btnode, LE, _,_,_} = L,
  util:logging(FileName, integer_to_list(Elem)),
  util:logging(FileName, " -> "),
  util:logging(FileName, integer_to_list(LE)),
  util:logging(FileName, ";\n\t"),
  printLeaf(L, FileName);
%% speichert das rechte Blatt und ruft die Funktion rekursiv auf
printLeaf({btnode,Elem,_,btempty, R}, FileName) ->
  {btnode, RE, _,_,_} = R,
  util:logging(FileName, integer_to_list(Elem)),
  util:logging(FileName, " -> "),
  util:logging(FileName, integer_to_list(RE)),
  util:logging(FileName, ";\n\t"),
  printLeaf(R, FileName);
%% speichert beide Blätter und ruft die Funktion erneut mit dem linken und dem rechten Blatt auf
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
%%  Entrypoint %%
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

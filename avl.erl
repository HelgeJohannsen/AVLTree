%%%-------------------------------------------------------------------
%%% @author Helge
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2017 16:03
%%%-------------------------------------------------------------------
-module(avl).
-author("Helge").

%% API
-export([main/0]).

main() ->	BT = initBT(),
  BT2 = insertBT(BT, 4),
  BT3 = insertBT(BT2, 7),
  BT4 = insertBT(BT3, 5),
  BT5 = insertBT(BT4, 8),
  BT6 = insertBT(BT5, 2),
  BT7 = insertBT(BT6, 20),
  BT8 = insertBT(BT7, 7),
  insertBT(BT7, 6).


initBT() -> btempty.
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


insertBT(btempty, Elem) 					when is_number(Elem) -> {btnode, Elem, 1, btempty, btempty};
insertBT({btnode, V, H, L, R}, Elem) ->
  case Elem > V of
    true  -> {btnode, V, max(hoeheBT(L),hoeheBT(R)) + 1, L, insertBT(R, Elem)};
    false -> {btnode, V, max(hoeheBT(L),hoeheBT(R)) + 1, insertBT(L, Elem), R}
  end.



hoeheBT(btempty) 				-> 0;
hoeheBT({btnode, _, H, _, _}) 	-> H;
hoeheBT(_) 						-> ok.

%% dieser Fall stellt eine Links rotation dar
insertBT({btnode,V,1,btempty,{btnode,VY, HY, _,_}}, Elem) when Elem > V ->
  case Elem < VY of
    true  -> {btnode,VY,1,{btnode, Elem, 0,btempty, btempty }, {btnode, V, 0, btempty, btempty}};
    false -> {btnode,VY,1,{btnode, V, 0,btempty, btempty }, {btnode, Elem, 0, btempty, btempty}}
  end;
insertBT(btnode, V, H, L, R) when (hoeheBT(L) - hoeheBT(R)) ->

%% dieser Fall stellt eine rechts Rotation dar
  insertBT({btnode,V,1,{btnode,VX, HY, _,_},btempty}, Elem) when Elem < V ->
case Elem < VX of
true  -> {btnode,VX,1,{btnode, Elem, 0,btempty, btempty }, {btnode, V, 0, btempty, btempty}};
false -> {btnode,VX,1,{btnode, V, 0,btempty, btempty }, {btnode, Elem, 0, btempty, btempty}}
end;


insertBT(btempty, Elem)                     when is_number(Elem) -> {btnode, Elem, 0, btempty, btempty};
insertBT(Node = {btnode, V, H, L, R}, Elem)        when (Elem > V) ->
BF = hoeheBT(L) - hoeheBT(R),
case BF of
-1 -> RNode = left_rotate(Node), insertBT(RNode, Elem)
end;
insertBT(Node = {btnode, V, H, L, R}, Elem)        when (Elem < V) ->
BF = hoeheBT(L) - hoeheBT(R),
case BF of
1 -> RNode = left_rotate(Node), insertBT(RNode, Elem)
end;
insertBT({btnode, V, _, L, R}, Elem)        when is_number(Elem) ->
case Elem < V of
true  -> LN = insertBT(L, Elem), RN = R;
false -> RN = insertBT(R, Elem), LN = L
end,

%%insertBT(X = {btnode,V,H,{btnode, VL, HL, _,_},{btnode,VY, HR, _,_}}, Elem) when HL > HR ->
%%  insertBT(left_rotate(X), Elem);
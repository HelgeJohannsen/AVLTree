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
-export([main/0, setGlobals/0]).

main() ->
  setGlobals(),
  BT = initBT(),
  BT1 = insertBT(BT,10),
  BT2 = insertBT(BT1, 5),
  BT3 = insertBT(BT2, 15),
  BT4 = insertBT(BT3, 4),
  BT5 = insertBT(BT4, 8),
  BT6 = insertBT(BT5, 12),
  BT7 = insertBT(BT6, 20),
  BT8 = insertBT(BT7, 6),
  BT9 = insertBT(BT8, 9),
  BT10 = insertBT(BT9, 37),
  BT11 = insertBT(BT10, 36),
  BT12 = insertBT(BT11, 22),
  BT13 = insertBT(BT12, 27),
  BT14 = insertBT(BT13, 29),
  BT15 = insertBT(BT14, 30),
  BT16 = insertBT(BT15, 31),
  BT17 = insertBT(BT16, 32),
  BT18 = insertBT(BT17, 33),
  BT19 = insertBT(BT18, 34),
  BT20 = insertBT(BT19, 40),
  BT21 = insertBT(BT20, 41),
  BT22 = insertBT(BT21, 42),
  isBT(BT22).


initBT() -> btempty.
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
    andalso ((Diff < 2) and (Diff > -2))
    andalso isBT_helper(L, {LOW, V})
    andalso isBT_helper(R, {V, HIGH})
    andalso (H == max(hoeheBT(L), hoeheBT(R)) + 1); %% Short-circuit-evaluating andalso to prevent max from err'ing if L or R are no BT's
isBT_helper(_, _) -> false.


hoeheBT(btempty) 				-> 0;
hoeheBT({btnode, _, H, _, _}) 	-> H;
hoeheBT(_) 						-> ok.

valueBT(btempty) 				-> 0;
valueBT({btnode, V, _, _, _}) 	-> V.

highestValueBT({btnode, V,_,_,btempty}) -> V;
highestValueBT({btnode, _,_,_,R}) -> highestValueBT(R).
lowestValueBT({btnode, V,_,btempty,_}) -> V;
lowestValueBT({btnode, _,_,L,_}) -> lowestValueBT(L).

deleteBT({btnode, V,H,L,btempty},V) -> L;
deleteBT({btnode, V,H,btempty,R},V) -> R;
deleteBT({btnode, V,H,btempty,btempty},V) -> btempty;
deleteBT({btnode, V,H,L,R},V) ->
  {btnode, VL,HL,LL,RL} = L,
  Diff = hoeheBT(L) - hoeheBT(R),
  case (Diff<0) of
      true-> VN = lowestValueBT(R),LN = L, RN = deleteBT(R,lowestValueBT(R));
      false-> VN = highestValueBT(L),LN = deleteBT(L,highestValueBT(L)), RN = R
  end,
  HN = max(hoeheBT(LN), hoeheBT(RN)) + 1,
  {btnode, VN, HN, LN, RN};


deleteBT(btempty,V) -> btempty;
deleteBT(BTree = {btnode, V,H,L,R},Elem) ->
  LN = case Elem < V of
         true  -> deleteBT(L, Elem);
         false -> L
       end,
  RN = case Elem > V of
         true  -> deleteBT(R, Elem);
         false -> R
       end,
  HN =  max(hoeheBT(LN), hoeheBT(RN)) + 1,
  Z = {btnode, V, HN, LN, RN}
,balance(Z).


left_rotate(({btnode, VX, HX, A,{btnode, VY,HY ,B,C}})) ->
io:format("rotation nach links: ~p~n",[VY]),
  NHX = (max(hoeheBT(A),hoeheBT(B))+1),
  NHY = max(NHX,hoeheBT(C))+1,
{btnode, VY, NHY, {btnode, VX, NHX, A, B }, C}.
left_rotate({btnode, VX, HX, A,{btnode, VY,HY ,B,C}},E) ->
  io:format("rotation nach links: ~p~n",[VY]),
  {btnode, VY, HX, {btnode, VX, HY, A, B }, insertBT(C,E)}.


right_rotate({btnode, VY, HY,{btnode, VX, HX, A,B},C }) ->
  io:format("rotation nach rechts: ~p~n",[VY]),
  NHY = (max(hoeheBT(B),hoeheBT(C))+1),
  NHX = (max(NHY,hoeheBT(A))+1),
  {btnode, VX, NHX, A, {btnode, VY, NHY, B,C}}.
right_rotate({btnode, VY, HY,{btnode, VX, HX, A,B},C },E) ->
  io:format("rotation nach rechts: ~p~n",[VY]),
  {btnode, VX, HY, insertBT(A,E), {btnode, VY, HX, B,C}}.


insertBT(btempty, Elem)                    when is_number(Elem) -> {btnode, Elem, 1, btempty, btempty};
%nsertBT({btnode, V, H, L,btempty},Elem)  when (Elem>V) -> {btnode, V, H, L,{btnode, Elem, 0, btempty, btempty}};
%insertBT({btnode, V, H, btempty,R},Elem)  when (Elem<V) -> {btnode, V, H, {btnode, Elem, 0, btempty, btempty},R};

insertBT({btnode, V, H, L,R},Elem)->
  LN = case Elem < V of
         true  -> insertBT(L, Elem);
         false -> L
       end,
  RN = case Elem > V of
         true  -> insertBT(R, Elem);
         false -> R
       end,
  HN = max(hoeheBT(LN), hoeheBT(RN)) + 1,
  io:format("Wert: ~p~n",[V]),
  io:format("Hoehe: ~p~n",[HN]),
  io:format("All: ~p~n",[RN]),
  Z = {btnode, V, HN, LN, RN},
  balance(Z).

balance(btempty) -> btempty;
balance(W = {btnode, V, H, L,R}) ->
  WW = dir(W),
  WR = dir(R),
  WL = dir(L),
  io:format("WeightWurzel: ~p~n",[WW]),
  io:format("WeightRechts ~p~n",[WR]),
  io:format("WeightLinks ~p~n",[WL]),
  case {WW,WR,WL} of
    {balanced,_,_} -> W;
    {rechtsrechts,rechts,_} -> CLR = util:getglobalvar(left),util:setglobalvar(left, CLR +1),left_rotate(W);
    {linkslinks,_,links} -> CRR = util:getglobalvar(right),util:setglobalvar(right, CRR +1),left_rotate(W);
    {linkslinks ,_,rechts} -> right_rotate({btnode, V,H,left_rotate(L),R});
    {rechtsrechts,links,_} -> left_rotate({btnode, V, H, L, right_rotate(R)});
    {links,_,_} -> W;
    {rechts,_,_} -> W
  end.

dir(btempty) -> balanced;
dir({btnode, _,_,L,R}) -> case(hoeheBT(L) - hoeheBT(R)) of
                            1 -> links;
                            2 -> linkslinks;
                            -1 -> rechts;
                            -2 -> rechtsrechts;
                            0 -> balanced
                          end
.
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

setGlobals() ->
  util:setglobalvar(left,0),
  util:setglobalvar(right,0),
  util:setglobalvar(dleft,0),
  util:setglobalvar(dright,0)
.
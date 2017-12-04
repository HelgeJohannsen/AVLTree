%%%-------------------------------------------------------------------
%%% @author Helge
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Nov 2017 15:32
%%%-------------------------------------------------------------------
-module('Garbage').
-author("Helge").

%% API
-export([]).

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

%% dieser Fall stellt eine Doppelrotation Rechts dar, der Baum ist Linksgewichtig und wir möchten erneut links einfgueen
insertBT({btnode,VW,_,L ={btnode,VL, _, _,_}, btempty}, Elem) when ((Elem < VW) and (Elem > VL)) ->
  io:format("Doppelrotation nach rechts ab: ~p~n",[VW]),
  {btnode, Elem, 1, {btnode, VL, 0, btempty, btempty}, {btnode, VW, 0, btempty, btempty}};

%% dieser Fall stellt eine rechts Rotation dar
insertBT({btnode,VW,_,btempty, {btnode,VR, _, _,_}}, Elem) when ((Elem > VW) and (Elem < VR)) ->
  io:format("Doppelrotation nach Links: ~p~n",[VW]),
  {btnode, Elem, 1, {btnode, VW, 0, btempty, btempty}, {btnode, VR, 0, btempty, btempty}};
insertBT(btempty, Elem)                     when is_number(Elem) -> {btnode, Elem, 0, btempty, btempty};


insertBT(W ={btnode, V, H, L, R} , Elem)        when is_number(Elem) ->
  case {(Elem < V), (V < Elem)}of
    {true, false} -> Richtung = links;
    {false, true} -> Richtung = rechts
  end,
  Diff = hoeheBT(L) - hoeheBT(R),
  HN = max(hoeheBT(L), hoeheBT(R)) + 1,
  io:format("Diff: ~p~n", [Diff]),
  RRechts = ((Elem < V) and (Diff > 0)),
  RLinks  = ((Elem > V) and (Diff < 0)),
  case {Richtung,RRechts,RLinks} of
    {rechts, false, false}  -> {btnode, V, HN, L, insertBT(R, Elem)};
    {links, false, false} -> {btnode, V, HN, insertBT(L, Elem), R};
    {rechts, false, true} -> insertBT(left_rotate(W), Elem);
    {links, true, false} -> insertBT(right_rotate(W), Elem)
  end.



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
isBalanced(BT22).



deleteBT({btnode, V,H,L,R},V) -> {btnode,valueBT(L),H,R };
deleteBT({btnode, V,H,L,btempty},V) -> L;
deleteBT({btnode, V,H,btempty,R},V) -> R;
deleteBT({btnode, V,H,btempty,btempty},V) -> btempty;
deleteBT(BTree = {btnode, V,H,L,R},Elem) ->
  LN = case Elem < V of
         true  -> deleteBT(L, Elem);
         false -> L
       end,
  RN = case Elem > V of
         true  -> deleteBT(R, Elem);
         false -> R
       end,
%  HN = max(hoeheBT(LN), hoeheBT(RN)) + 1,
  io:format("Wert: ~p~n",[V]),
  io:format("Hoehe: ~p~n",[HN]),
  io:format("All: ~p~n",[RN]),
  Z = {btnode, V, HN, LN, RN}.


main() ->
  setGlobals(),
  BT = initBT(),
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
  BT13 = insertBT(BT12, 27),
  BT14 = insertBT(BT13, 29),
  BT15 = insertBT(BT14, 30),
  BT16 = insertBT(BT15, 31),
  BT17 = insertBT(BT16, 32),
  BT18 = insertBT(BT17, 33),
  BT19 = insertBT(BT18, 34),
  BT20 = insertBT(BT19, 40),
  BT21 = insertBT(BT20, 41),
  BT22 = insertBT(BT21, 42).


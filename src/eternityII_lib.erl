%%%-------------------------------------------------------------------
%%% @author  <firefly@scorpion>
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2015 by  <firefly@scorpion>
%%%-------------------------------------------------------------------
-module(eternityII_lib).

-include("../include/eternityII.hrl").

%% API
%%-export([]).
-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

make_tile({Id,Type,Pattern,Sources}) ->
    #tile{id = Id, type = Type, pattern = make_pattern(Pattern),
	  size = {1,1}, count = length(Sources),
	  sources = Sources, primary = []}.

make_pattern(Pattern) ->
    make_pattern_aux(sort_pattern(Pattern)).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% case a: matching a corner
%%--------------------------------------------------------------------
matches(#tile{id=IdL, type=corner, pattern=PatternL, size={XL, YL}} = _Corner,
	#tile{id=IdR, type=edge, pattern=PatternR, size={XR, YR}}  = _Edge)
  when YR == YL orelse YR == XL ->
    #pattern{left = undefined, up = undefined,
	     right = RightL, down = DownL} = PatternL,
    #pattern{left = LeftR, up = undefined,
	     right = RightR, down = DownR} = PatternR,
    Match0 = case is_same(RightL, LeftR) of
		 false -> false;
		 true  ->
		     #tile{pattern=#pattern{left=undefined, up=undefined,
					    right=RightR, down=DownR ++ DownL},
			   type=corner, size={XL + XR, YL}, count=1,
			   sources=[IdL, IdR]}
	     end,
    Match1 = case is_same(DownL, RightR) of
		 false -> false;
		 true  ->
		     #tile{pattern=#pattern{left=undefined, up=undefined,
					    right=RightL++DownR, down=LeftR},
			   type=corner, size={XL, YL + XR}, count=1,
			   sources=[IdL, IdR]}
	     end,
    case {Match0, Match1} of
	{false, false} -> false;
	{false, _}     -> {true, [Match1]};
	{_, false}     -> {true, [Match0]};
	_Else          -> {true, [Match0, Match1]}
    end;
matches(#tile{type=corner} = _Corner, #tile{type=edge}  = _Edge) ->
    false;

%%--------------------------------------------------------------------
%% case b: matching an edge
%%--------------------------------------------------------------------
matches(#tile{id=IdL, type=edge, pattern=PatternL, size={XL, Y}}  = _EdgeL,
	#tile{id=IdR, type=edge, pattern=PatternR, size={XR, Y}}  = _EdgeR) ->
    #pattern{left = LeftL, up = undefined,
	     right = RightL, down = DownL} = PatternL,
    #pattern{left = LeftR, up = undefined,
	     right = RightR, down = DownR} = PatternR,
    Match0 = case is_same(RightL, LeftR) of
		 false -> false;
		 true  ->
		     #tile{pattern=#pattern{left=LeftL, up=undefined,
					    right=RightR, down=DownR ++ DownL},
			   type=edge, size={XL + XR, Y}, count=1,
			   sources=[IdL, IdR]}
	     end,
    Match1 = case is_same(RightR, LeftL) of
		 false -> false;
		 true  ->
		     #tile{pattern=#pattern{left=LeftR, up=undefined,
					    right=RightL, down=DownL ++ DownR},
			   type=edge, size={XL + XR, Y}, count=1,
			   sources=[IdR, IdL]}
	     end,
    case {Match0, Match1} of
	{false, false} -> false;
	{false, _}     -> {true, [Match1]};
	{_, false}     -> {true, [Match0]};
	_Else          -> {true, [Match0, Match1]}
    end;
matches(#tile{type=edge} = _EdgeL, #tile{type=edge}  = _EdgeR) ->
    false;
matches(#tile{id=IdL, type=edge, pattern=PatternL, size={XL, Y}}  = _Edge,
	#tile{id=IdR, type=center, pattern=PatternR, size={XR, Y}}  = _Center) ->
    false; %% TODO

%%--------------------------------------------------------------------
%% case A: the two tiles are the same
%%--------------------------------------------------------------------
matches(#tile{type=center}=Tile, Tile) when Tile#tile.count < 2 ->
    false;
matches(#tile{type=center, pattern = Pattern, size = {X, Y}} = Tile, Tile) ->
    #pattern{left = Left, up = Up, right = Right, down = Down} = Pattern,
    Matches0 = [#tile{pattern=#pattern{up=Left,  right=Up++Down,
				       down=Left, left=Up++Down},
		      count = 1, size = {Y, 2*X},
		      sources=[{Tile#tile.id, Tile#tile.id}]},
		#tile{pattern=#pattern{up=Right,  right=Down++Up,
				       down=Right, left=Down++Up},
		      count = 1, size = {Y, 2*X},
		      sources=[{Tile#tile.id, Tile#tile.id}]}],
    Matches1 = if (Y == X) ->
		       [#tile{pattern=#pattern{up=Down,  right=Left++Right,
					       down=Down, left=Left++Right},
			      count = 1, size = {Y, 2*X},
			      sources=[{Tile#tile.id, Tile#tile.id}]},
			#tile{pattern=#pattern{up=Up,  right=Right++Left,
					       down=Up, left=Right++Left},
			      count = 1, size = {Y, 2*X},
			      sources=[{Tile#tile.id, Tile#tile.id}]}
			| Matches0];
		  true ->
		       Matches0
	       end,
    {true, lists:usort(Matches1)};
%%--------------------------------------------------------------------
%% case B: different tiles
%%--------------------------------------------------------------------
matches(#tile{type=center} = TileL, #tile{type=center} = TileR) ->
    MatchesLeft  = match_left(TileL, TileR),
    MatchesUp    = match_up(TileL, TileR),
    MatchesRight = match_right(TileL, TileR),
    MatchesDown  = match_down(TileL, TileR),
    Matches = MatchesLeft ++ MatchesUp ++ MatchesRight ++ MatchesDown,
    case Matches of
	[] -> false;
	_  -> {true, lists:usort(Matches)}
    end.

match_left(TileL, TileR) ->
    M0 = match_left_right(TileL, TileR),
    M1 = match_left_left(TileL, TileR),
    M2 = match_left_up(TileL, TileR),
    M3 = match_left_down(TileL, TileR),
    [M || M <- [M0, M1, M2, M3], M /= false].

match_right(TileL, TileR) ->
    M0 = match_right_right(TileL, TileR),
    M1 = match_right_left(TileL, TileR),
    M2 = match_right_up(TileL, TileR),
    M3 = match_right_down(TileL, TileR),
    [M || M <- [M0, M1, M2, M3], M /= false].

match_up(TileL, TileR) ->
    M0 = match_up_right(TileL, TileR),
    M1 = match_up_left(TileL, TileR),
    M2 = match_up_up(TileL, TileR),
    M3 = match_up_down(TileL, TileR),
    [M || M <- [M0, M1, M2, M3], M /= false].

match_down(TileL, TileR) ->
    M0 = match_down_right(TileL, TileR),
    M1 = match_down_left(TileL, TileR),
    M2 = match_down_up(TileL, TileR),
    M3 = match_down_down(TileL, TileR),
    [M || M <- [M0, M1, M2, M3], M /= false].

%% match left pattern on left tile
match_left_left(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {XL, Y}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {XR, Y}} = TileR) ->
    case is_same(LeftL, LeftR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = RightL,  right = DownL ++ UpR,
					  down = RightR, left = DownR ++ UpL},
		       size = {Y, XL + XR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_left_left(_, _) -> false.


match_left_right(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {XL, Y}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {XR, Y}} = TileR) ->
    case is_same(LeftL, RightR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = RightL, right = DownL ++ DownR,
					  down = LeftR, left = UpR ++ UpL},
		       size = {Y, XL + XR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_left_right(_, _) -> false.


match_left_up(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {X, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {X, YR}} = TileR) ->
    case is_same(LeftL, UpR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = RightL, right = DownL ++ RightR,
					  down = DownR, left = LeftR ++ UpL},
		       size = {YL, X + YR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_left_up(_, _) -> false.


match_left_down(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {X, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {X, YR}} = TileR) ->
    case is_same(LeftL, DownR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = RightL, right = DownL ++ LeftR,
					  down = UpR,  left = RightR ++ UpL},
		       size = {YL, X + YR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_left_down(_, _) -> false.


%% match right pattern on left tile
match_right_left(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {XL, Y}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {XR, Y}} = TileR) ->
    case is_same(RightL, LeftR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = LeftL, right = UpL ++ UpR,
					  down = RightR, left = DownR ++ DownL},
		       size = {Y, XL + XR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_right_left(_, _) -> false.


match_right_right(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {XL, Y}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {XR, Y}} = TileR) ->
    case is_same(RightL, RightR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = LeftL, right = UpL ++ DownR,
					  down = LeftR, left = UpR ++ DownL},
		       size = {Y, XL + XR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_right_right(_, _) -> false.


match_right_up(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {XL, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {YL, YR}} = TileR) ->
    case is_same(RightL, UpR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = LeftL, right = UpL ++ RightR,
					  down = DownR, left = LeftR ++ DownL},
		       size = {YL, XL + YR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_right_up(_, _) -> false.


match_right_down(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {XL, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {YL, YR}} = TileR) ->
    case is_same(RightL, DownR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = LeftL, right = UpL ++ LeftR,
					  down = UpR,  left = RightR ++ DownL},
		       size = {YL, XL + YR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_right_down(_, _) -> false.


%% match top pattern on left tile
match_up_left(#tile{pattern = #pattern{left = LeftL, up = UpL,
				       right = RightL, down = DownL},
		      size = {XL, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {XR, XL}} = TileR) ->
    case is_same(UpL, LeftR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = DownL, right = LeftL ++ UpR,
					  down = RightR, left = DownR ++ RightL},
		       size = {XL, YL + XR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_up_left(_, _) -> false.


match_up_right(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {XL, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {XR, XL}} = TileR) ->
    case is_same(UpL, RightR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = DownL, right = LeftL ++ DownR,
					  down = LeftR, left = UpR ++ RightL},
		       size = {XL, YL + XR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_up_right(_, _) -> false.


match_up_up(#tile{pattern = #pattern{left = LeftL, up = UpL,
				     right = RightL, down = DownL},
		      size = {X, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {X, YR}} = TileR) ->
    case is_same(UpL, UpR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = DownL, right = LeftL ++ RightR,
					  down = DownR, left = LeftR ++ RightL},
		       size = {X, YL + YR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_up_up(_, _) -> false.


match_up_down(#tile{pattern = #pattern{left = LeftL, up = UpL,
				       right = RightL, down = DownL},
		      size = {X, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {X, YR}} = TileR) ->
    case is_same(UpL, DownR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = DownL, right = LeftL ++ LeftR,
					  down = UpR,  left = RightR ++ RightL},
		       size = {X, YL + YR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_up_down(_, _) -> false.


%% match bottom pattern on left tile
match_down_left(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {XL, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {XR, XL}} = TileR) ->
    case is_same(DownL, LeftR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = UpL, right = RightL ++ UpR,
					  down = RightR, left = DownR ++ LeftL},
		       size = {XL, YL + XR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_down_left(_, _) -> false.


match_down_right(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {XL, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {XR, XL}} = TileR) ->
    case is_same(DownL, RightR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = UpL, right = RightL ++ DownR,
					  down = LeftR, left = UpR ++ LeftL},
		       size = {XL, YL + XR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_down_right(_, _) -> false.


match_down_up(#tile{pattern = #pattern{left = LeftL, up = UpL,
				       right = RightL, down = DownL},
		      size = {X, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {X, YR}} = TileR) ->
    case is_same(DownL, UpR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = UpL, right = RightL ++ RightR,
					  down = DownR, left = LeftR ++ LeftL},
		       size = {X, YL + YR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_down_up(_, _) -> false.


match_down_down(#tile{pattern = #pattern{left = LeftL, up = UpL,
					 right = RightL, down = DownL},
		      size = {X, YL}} = TileL,
		#tile{pattern = #pattern{left = LeftR, up = UpR,
					 right = RightR, down = DownR},
		      size = {X, YR}} = TileR) ->
    case is_same(DownL, DownR) of
	false -> false;
	true  -> #tile{pattern = #pattern{up = UpL, right = RightL ++ LeftR,
					  down = UpR,  left = RightR ++ LeftL},
		       size = {X, YL + YR}, count = 1,
		       sources = {TileL#tile.id, TileR#tile.id}}
    end;
match_down_down(_, _) -> false.



is_same(P1, P2) when length(P1) /= length(P2) -> false;
is_same(P1, P2) -> P1 == lists:reverse(P2).

make_pattern_aux({Left, Down, Right, Up}) ->
    #pattern{left = [Left], up = [Up], right = [Right], down = [Down]};
make_pattern_aux({Left, Down, Right}) ->
    #pattern{left = [Left], right = [Right], down = [Down]};
make_pattern_aux({Right, Down}) ->
    #pattern{right = [Right], down = [Down]}.

sort_pattern({Left, Down, Right, Up}=P0) ->
    P1 = if (Left < Right) orelse
	    ((Left == Right) andalso (Down =< Up)) ->
		 P0;
	    true ->
		 {Right, Up, Left, Down}
	 end,
    if length(Left) /= length(Down) ->
	    P1;
       Up < element(1, P1) andalso Up =< Down ->
	    {Up, Left, Down, Right};
       Down < element(1, P1) ->
	    {Down, Right, Up, Left};
       true -> P1
    end;
sort_pattern(P) -> P.


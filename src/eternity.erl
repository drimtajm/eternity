-module(eternity).
-compile(export_all).

-type pattern() :: atom() | [atom()].

-record(patterns, {up      :: pattern(),
		   down    :: pattern(),
		   left    :: pattern(),
		   right   :: pattern()}).

-record(brick, {id             :: pos_integer(),
		type           :: corner | edge | center,
		valid_patterns :: [#patterns{}],
		numbers        :: [pos_integer()]}).

-define(VERBOSE, false).
-define(DEBUG(Id, Pos, PPatterns, Brick, NPatterns),
	case ?VERBOSE of
	    false -> noop;
	    true  ->
		io:format("Trying brick ~p on position ~p~n"
			  "Patterns so far: ~p~nTile: ~p~n"
			  "New solution: ~p~n~n",
			  [Id, Pos, PPatterns, Brick, NPatterns])
	end).
-define(DEBUG(Id, Pos, PPatterns, Brick),
	case ?VERBOSE of
	    false -> noop;
	    true  ->
		io:format("Trying brick ~p on position ~p~n"
			  "Patterns so far: ~p~nTile: ~p~n"
			  "No match~n~n",
			  [Id, Pos, PPatterns, Brick])
	end).

-define(TILES, [{26, corner, {bg, bg}},
		{22, edge, {bg, bu, yb}},
		{11, edge, {yb, bu, bo}},
		{12, edge, {bo, by, bg}},
		{ 2, center, {bu, by, bu, by}},
		{35, center, {bu, by, by, by}},
		{10, edge, {bo, by, bo}},
		{24, center, {bu, bu, by, by}},
		{ 4, center, {yp, by, by, bu}}]).

go() ->
    Tiles = [{26, corner, {bg, bg}},
	     {22, edge, {bg, bu, yb}},
	     {11, edge, {yb, bu, bo}},
	     {12, edge, {bo, by, bg}},
	     { 2, center, {bu, by, bu, by}},
	     {35, center, {bu, by, by, by}},
	     {10, edge, {bo, by, bo}},
	     {24, center, {bu, bu, by, by}},
	     { 4, center, {yp, by, by, bu}},

	     {36, corner, {bg, bg}},
	     {34, corner, {yb, bp}},
	     {14, corner, {bp, bg}},
	     {16, edge, {bo, bu, yb}},
	     {25, edge, {yb, by, bg}},
	     {28, edge, {bg, by, bp}},
	     {13, edge, {bo, bu, bp}},
	     {17, edge, {bp, bu, bp}},
	     {18, edge, {yb, by, bo}},
	     {23, edge, {yb, by, bp}},
	     {15, edge, {bp, by, yb}},
	     {20, edge, {bp, by, bo}},
	     { 7, edge, {bg, by, yb}},
	     {21, edge, {bo, by, yb}},
	     {31, edge, {bp, bu, bo}},
	     { 1, center, {bu, bu, by, by}},
	     { 8, center, {bu, bu, by, by}},
	     {19, center, {bu, by, bu, by}},
	     {32, center, {bu, by, by, by}},
	     { 9, center, {by, bu, bu, bu}},
	     {27, center, {by, bu, bu, bu}},
	     { 3, center, {yp, bu, by, bu}},
	     { 5, center, {yp, bu, by, bu}},
	     {33, center, {yp, by, by, bu}},
	     { 6, center, {yp, by, bu, bu}},
	     {30, center, {yp, by, by, by}},
	     {29, center, {yp, yp, bu, bu}}],
    Bricks = lists:map(fun(Tile) ->
			       mk_brick(Tile)
		       end,
		       lists:zip(lists:seq(1, length(Tiles)), Tiles)),
    Corners = [Brick || #brick{type=Type}=Brick <- Bricks,
			Type =:= corner],
    Edges = [Brick || #brick{type=Type}=Brick <- Bricks,
		       Type =:= edge],
    CenterPieces = [Brick || #brick{type=Type}=Brick <- Bricks,
			      Type =:= center],
    MaxX = 3,
    MaxY = 3,
    get_all_solutions_aux(Corners, Edges, CenterPieces,
			  {MaxX, MaxY}, []).

get_all_solutions_aux([] = _Corners, _Edges, _CenterPieces, 
		      _Dimensions, Result) ->
    Result;
get_all_solutions_aux(Corners, Edges0, CenterPieces0,
		      {MaxX, MaxY} = Dimensions, Result0) ->
    NumberOfEdges = (MaxX-1)+(MaxY-1),
    NumberOfCenterPieces = (MaxX-1)*(MaxY-1),
    {Edges, RestEdges} = lists:split(NumberOfEdges, Edges0),
    {CenterPieces, RestCenterPieces} =
	lists:split(NumberOfCenterPieces, CenterPieces0),
    Result = [timer:tc(eternity, get_all_solutions,
		       [[hd(Corners)], Edges, CenterPieces, corner, {MaxX, MaxY}])
	      | Result0],
    get_all_solutions_aux(tl(Corners), RestEdges, RestCenterPieces,
			  Dimensions, Result).

get_all_solutions([Corner], Edges0, CenterPieces0, corner, Dimensions) ->
    {Edges, EdgeSeed} = combinations:get_first(Edges0),
    {CenterPieces, CenterPiecesSeed} = combinations:get_first(CenterPieces0),
    get_all_corner_solutions(Corner, Edges0, CenterPieces0,
			     EdgeSeed, CenterPiecesSeed,
			     Edges, CenterPieces, Dimensions,
			     []).

go_large() ->
    Tiles = [{3, corner, {bp, yb}},
	     {24, edge, {bo, puo, yb}},
	     {55, edge, {obr, pcb, obr}},
	     {54, edge, {bg, pcb, obr}},
	     {51, edge, {yb, bpu, obr}},
	     {11, edge, {yb, gy, bo}},
	     {30, edge, {obr, psb, bp}},
	     {126, center, {bpu, gy, yp, ob}},
	     {239, center, {ybr, pb, gy, puo}},
	     {134, center, {bpu, pcb, og, pb}},
	     {243, center, {gy, gy, psb, by}},
	     {248, center, {psb, gy, pcb, by}},
	     {252, center, {by, pcb, by, ob}},
	     {250, center, {puo, gy, ysp, bb}},
	     {117, center, {bb, pb, og, bpu}},
	     {98, center, {pb, yrp, ob, puo}}],
    Bricks = lists:map(fun(Tile) ->
			       mk_brick(Tile)
		       end,
		       lists:zip(lists:seq(1, length(Tiles)), Tiles)),
    Corners = [Brick || #brick{type=Type}=Brick <- Bricks,
			Type =:= corner],
    Edges0 = [Brick || #brick{type=Type}=Brick <- Bricks,
		       Type =:= edge],
    CenterPieces0 = [Brick || #brick{type=Type}=Brick <- Bricks,
			      Type =:= center],
    MaxX = 4,
    MaxY = 4,
    io:format("before permutations~n"),
    Edges = permutations(Edges0),
    CenterPieces = permutations(CenterPieces0),
    io:format("after permutations, length(Edges): ~p,"
	      " length(CenterPieces):~p, heap: ~p~n",
	      [length(Edges), length(CenterPieces),
	       process_info(self(), memory)]),
    timer:tc(eternity, get_all_solutions1,
	     [hd(Corners), Edges, Edges,
	      CenterPieces, corner, {MaxX, MaxY}, []]).

permutations([])   -> [[]];
permutations(List) ->
    [[Head|Tail] || Head <- List,
		    Tail <- permutations(List--[Head])].

get_all_solutions1(_Corner, _Edges0, [], [],
		   corner, _Dimensions, Result) ->
    Result;
get_all_solutions1(Corner, Edges0, [], CenterPieces,
		   corner, Dimensions, Result) ->
    erlang:garbage_collect(),
    if ((length(CenterPieces) rem 1000) == 0) ->
       io:format("heap size: ~p~n", [process_info(self(), memory)]);
       true -> ok
    end,
    get_all_solutions1(Corner, Edges0, Edges0, tl(CenterPieces),
		       corner, Dimensions, Result);
get_all_solutions1(Corner, Edges0, [Edges | RestEdges], CenterPieces,
		   corner, Dimensions, Result0) ->
    BrickList = mk_brick_list(corner, Corner, Edges, hd(CenterPieces),
			      Dimensions),
    Result = [timer:tc(eternity, get_matches_with_fixed_position,
		       [BrickList, Dimensions]) | Result0],
    get_all_solutions1(Corner, Edges0, RestEdges, CenterPieces,
		       corner, Dimensions, Result).

get_all_corner_solutions(Corner, Edges0, CenterPieces0,
			 EdgesSeed0, CenterPiecesSeed0,
			 Edges, CenterPieces, Dimensions,
			 Result0) ->
    BrickList = mk_brick_list(corner, Corner, Edges, CenterPieces, Dimensions),

    {Time, RetVal} = timer:tc(eternity, get_matches_with_fixed_position,
			      [BrickList, Dimensions]),
    SolutionCount = case RetVal of
			dead_end -> 0;
			_Else    -> length(RetVal)
		    end,
    Result = [{Time, SolutionCount} | Result0],
    
    case {EdgesSeed0, CenterPiecesSeed0} of
	{done, done} -> Result;
	{done, _}    ->
	    {Edges1, EdgesSeed} = combinations:get_first(Edges0),
	    {CenterPieces1, CenterPiecesSeed} =
		combinations:get_next(CenterPieces0, CenterPiecesSeed0),
	    get_all_corner_solutions(Corner, Edges0, CenterPieces0,
				     EdgesSeed, CenterPiecesSeed,
				     Edges1, CenterPieces1, Dimensions,
				     Result);
	_Otherwise   ->
	    {Edges1, EdgesSeed} = combinations:get_next(Edges0, EdgesSeed0),
	    get_all_corner_solutions(Corner, Edges0, CenterPieces0,
				     EdgesSeed, CenterPiecesSeed0,
				     Edges1, CenterPieces, Dimensions,
				     Result)
    end.
	    

mk_brick_list(corner, Corner, Edges, CenterPieces, {MaxX, MaxY}) ->
    mk_brick_list(corner, Corner, Edges, CenterPieces, {MaxX, MaxY}, MaxY, []).
mk_brick_list(corner, Corner, Edges, []=_CenterPieces, _Dimensions, 1, Result) ->
    Row = [Corner | Edges],
    lists:append(Row, Result);
mk_brick_list(corner, Corner, Edges, CenterPieces, {MaxX, MaxY}, Y, Result0) ->
    {CenterPiecesInRow, RestCenterPieces} = lists:split((MaxX-1), CenterPieces),
    Row = [hd(Edges) | CenterPiecesInRow],
    Result = lists:append(Row, Result0),
    mk_brick_list(corner, Corner, tl(Edges), RestCenterPieces, {MaxX, MaxY}, Y-1, Result).

go(Bricks) ->
    get_matches_with_fixed_position(Bricks, {3, 3}).

mk_brick({Id, {Number, Type, Patterns0}}) ->
    Patterns = get_patterns(Type, Patterns0),
    #brick{id=Id, type=Type, valid_patterns=Patterns, numbers=[Number]}.

get_patterns(corner, {Down, Right}) ->
    [#patterns{down=Down, right=Right}];
get_patterns(edge, {Left, Down, Right}) ->
    [#patterns{left=Left, down=Down, right=Right}];
get_patterns(center, {Left, Down, Right, Up}) ->
    expand_brick_patterns(#patterns{left=Left, down=Down, right=Right, up=Up}).

%% lists:foldl(fun, acc0, list), acc0={3, {1,1}, no_value}
%% fun (Brick, {size, pos, Tile}) -> {size, newpos, NewTile}
%% fun ({1, corner, {no_value=Left, bg=Down, bg=Right, no_value=Up}, [36, 26]},
%%      {3, {1, 1}, no_value} ->
%%      {3, {2, 1}, {corner, {nv=L, bg=D, bg=R, nv=Up}, [1]}}
%% fun ({4, edge, {bg=LN, bu=DN, yb=RN, nv=UN}, [22]},
%%      {3, {2, 1}, {corner, {nv=L, bg=D, bg=R, nv=U}, [1]}} -> (LN == R)
%%      {3, {3, 1}, {corner, {nv=L, {bg,bu}={D, DN}, yb=RN, nv=U}}, [1, 22]}}
get_matches_with_fixed_position(Bricks, {MaxX, MaxY} = Size0)
 when is_list(Bricks), is_integer(MaxX), is_integer(MaxY),
      length(Bricks) =:= MaxX*MaxY ->
    case get_matches_with_fixed_position_aux(Bricks, Size0) of
	dead_end                 -> dead_end;
	{_Size, _Pos, Solutions} ->
	    Numbers = [Id || #brick{id=Id} <- Bricks],
	    lists:map(fun({N, #brick{}=Brick}) ->
			      Brick#brick{id=N, numbers=Numbers}
		      end, lists:zip(lists:seq(1,length(Solutions)),
				     Solutions))
    end.

get_matches_with_fixed_position_aux(Bricks, Size0) ->
    lists:foldl(
      fun(_Brick, dead_end) -> dead_end;
	 (#brick{}=Brick, {Size, {_X, _Y}=Pos0, Solutions0}) ->
	      Pos = get_new_pos(Pos0, Size),
	      case get_matches(Brick, Solutions0, Pos0) of
		  []        -> dead_end;
		  Solutions -> {Size, Pos, Solutions}
	      end
      end, {Size0, {1, 1}, []}, Bricks).

get_matches(#brick{valid_patterns=ValidPatterns}=Brick, [], {1, 1}) ->
    lists:map(fun (Patterns0) ->
		      Patterns = listify(Patterns0),
		      Brick#brick{id=undefined,valid_patterns=[Patterns]}
	      end, ValidPatterns);
get_matches(#brick{valid_patterns=ValidPatterns0,
		   type=Type}, Solutions, {X, _Y}=Pos) ->
    ValidPatterns = case {X, Type} of
			{1, edge} -> rotate_left(ValidPatterns0);
			_Else     -> ValidPatterns0
		    end,
    lists:flatmap(fun (#brick{}=B) ->
			  get_matches(ValidPatterns, B, Pos)
		  end, Solutions);

get_matches(ValidPatterns, #brick{}=PreviousSolution, Pos) ->
    Matches = lists:map(fun (Tile) ->
				match(Tile, PreviousSolution, Pos)
			end, ValidPatterns),
    [B || B <- Matches, B =/= no_match].

match(#patterns{up=Up, down=Down, left=Left, right=Right},
      #brick{valid_patterns=[#patterns{down=PDown}=PPatterns]}
      =PSolution, {1, _Y}) ->
    case is_same(Up, hd(PDown)) of
	true ->
	    #patterns{left=PLeft, right=PRight} = PPatterns,
	    NDown = [Down | tl(PDown)],
	    NLeft = lists:append(PLeft, [Left]),
	    NRight = [Right | PRight],
	    NPatterns=PPatterns#patterns{down=NDown, left=NLeft, right=NRight},
%%	    ?DEBUG(Pos, PPatterns, B, NPatterns),
	    PSolution#brick{valid_patterns=[NPatterns]};
	false ->
%%	    ?DEBUG(Pos, PPatterns, B),
	    no_match
    end;
match(#patterns{up=Up, down=Down, left=Left, right=Right},
      #brick{valid_patterns=[#patterns{right=PRight}=PPatterns]}
      =PSolution, {_X, 1}) ->
    case is_same(Left, hd(PRight)) of
	true ->
	    #patterns{up=PUp, down=PDown} = PPatterns,
	    NDown = lists:append(PDown, [Down]),
	    NUp = [Up | PUp],
	    NRight = [Right],
	    NPatterns=PPatterns#patterns{down=NDown, up=NUp, right=NRight},
%%	    ?DEBUG(Id, Pos, PPatterns, B, NPatterns),
	    PSolution#brick{valid_patterns=[NPatterns]};
	false ->
%%	    ?DEBUG(Id, Pos, PPatterns, B),
	    no_match
    end;
%%%%%%%%%%%%%%%
%%% 3,2
%%%
%%%  a b c d e
%%%  f g x
match(#patterns{up=Up, down=Down, left=Left, right=Right},
      #brick{valid_patterns=[#patterns{right=PRight,down=PDown}=PPatterns]}
      =PSolution, {X, _Y}) ->
    case is_same(Left, hd(PRight)) of
	true ->
	    case is_same(Up, lists:nth(X, PDown)) of
		true ->
		    NDown = replace_nth(PDown, Down, X),
		    NRight = [Right | tl(PRight)],
		    NPatterns=PPatterns#patterns{down=NDown, right=NRight},
%%		    ?DEBUG(Id, Pos, PPatterns, B, NPatterns),
		    PSolution#brick{valid_patterns=[NPatterns]};
		false ->
%%		    ?DEBUG(Id, Pos, PPatterns, B),
		    no_match
	    end;
	false ->
%%	    ?DEBUG(Id, Pos, PPatterns, B),
	    no_match
    end.

replace_last([_E], Element) ->
    [Element];
replace_last([First | Rest], Element) ->
    [First | replace_last(Rest, Element)].

replace_nth(List, Element, Pos) ->
    replace_nth(List, Element, Pos, 1).
replace_nth([_First | Rest], Element, Pos, Pos) ->
    [Element | Rest];
replace_nth([First | Rest], Element, Pos, CurrentPos) ->
    [First | replace_nth(Rest, Element, Pos, CurrentPos+1)].

listify(#patterns{up=Up, down=Down, left=Left, right=Right}=Patterns)
when is_list(Up) andalso is_list(Down) andalso
     is_list(Left) andalso is_list(Right) ->
    Patterns;
listify(#patterns{up=Up, down=Down, left=Left, right=Right}) ->
    #patterns{up=[Up], down=[Down], left=[Left], right=[Right]}.

expand_brick_patterns(
  #brick{valid_patterns=[#patterns{up=U, down=D,
				   left=L, right=R}=Patterns0]}=Brick) ->
    ValidPatterns=
	lists:usort([Patterns0,
		     #patterns{up=L, right=U, down=R, left=D},
		     #patterns{up=R, left=U, down=L, right=D},
		     #patterns{up=D, down=U, left=R, right=L}]),
    Brick#brick{valid_patterns=ValidPatterns};
expand_brick_patterns(#patterns{up=U, down=D, left=L, right=R}=Patterns0) ->
	lists:usort([Patterns0,
		     #patterns{up=L, right=U, down=R, left=D},
		     #patterns{up=R, left=U, down=L, right=D},
		     #patterns{up=D, down=U, left=R, right=L}]);
expand_brick_patterns(Brick) ->
    Brick.

rotate_left([#patterns{left=Left, down=Down, right=Right}]) ->
    [#patterns{down=Left, right=Down, up=Right}].

get_new_pos({MaxX, Y}, {MaxX, _MaxY}) ->
    {1, Y+1};
get_new_pos({X, Y}, _Size) ->
    {X+1, Y}.

is_same(Pattern1, Pattern2) when is_atom(Pattern1),
				 is_atom(Pattern2) ->
    Pattern1 =:= Pattern2;
is_same(Pattern1, Pattern2) when is_list(Pattern1),
				 is_list(Pattern2) ->
    Pattern1 =:= lists:reverse(Pattern2).

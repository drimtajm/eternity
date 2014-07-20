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

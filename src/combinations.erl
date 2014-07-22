-module(combinations).
-compile(export_all).


first_seed(FirstCombination) ->
    lists:seq(1,length(FirstCombination)).

get_first(FirstCombination) ->
    get_next(FirstCombination, first_seed(FirstCombination)).

get_next(FirstCombination, Seed) ->
    Combination =
	lists:map(fun (SeedElement) ->
			  lists:nth(SeedElement, FirstCombination)
		  end, Seed),
    N = length(FirstCombination),
    case N of
	2 -> {Combination, next2(Seed)};
	3 -> {Combination, next3(Seed)};
	4 -> {Combination, next4(Seed)}
    end.

next2([1,2]) -> [2,1];
next2([2,1]) -> done.

next3([1,2,3]) -> [1,3,2];
next3([1,3,2]) -> [2,1,3];
next3([2,1,3]) -> [2,3,1];
next3([2,3,1]) -> [3,1,2];
next3([3,1,2]) -> [3,2,1];
next3([3,2,1]) -> done.

next4([1,2,3,4]) -> [1,2,4,3];
next4([1,2,4,3]) -> [1,3,2,4];
next4([1,3,2,4]) -> [1,3,4,2];
next4([1,3,4,2]) -> [1,4,2,3];
next4([1,4,2,3]) -> [1,4,3,2];
next4([1,4,3,2]) -> [2,1,3,4];
next4([2,1,3,4]) -> [2,1,4,3];
next4([2,1,4,3]) -> [2,3,1,4];
next4([2,3,1,4]) -> [2,3,4,1];
next4([2,3,4,1]) -> [2,4,1,3];
next4([2,4,1,3]) -> [2,4,3,1];
next4([2,4,3,1]) -> [3,1,2,4];
next4([3,1,2,4]) -> [3,1,4,2];
next4([3,1,4,2]) -> [3,2,1,4];
next4([3,2,1,4]) -> [3,2,4,1];
next4([3,2,4,1]) -> [3,4,1,2];
next4([3,4,1,2]) -> [3,4,2,1];
next4([3,4,2,1]) -> [4,1,2,3];
next4([4,1,2,3]) -> [4,1,3,2];
next4([4,1,3,2]) -> [4,2,1,3];
next4([4,2,1,3]) -> [4,2,3,1];
next4([4,2,3,1]) -> [4,3,1,2];
next4([4,3,1,2]) -> [4,3,2,1];
next4([4,3,2,1]) -> done.
	

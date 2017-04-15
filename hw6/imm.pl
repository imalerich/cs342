:- [airlines].

%%
%% Some utilities I'm going to need.
%%

merge([], [], []).		%% Base case: both empty.
merge([H], B, [H|B]) :- !.	%% Base case: Add a single head.

merge([H|T], B, [H|C]) :-
    T \= [],			%% Don't go to the base case.
    merge(T, B, C).		%% Recurse to merge the tail of our first list with B.

%% Finds the list of all visited cities in the order in which they are visited.
%% As an example:
%%	[[A,_,B], [B,_,C], [C,_,D]] -> [A,B,C,D].
%% Note the origin and the destination are always included,
%% 	but adjacent ports are included only once.
%% Thus:
%%	[[A,_,B], [B,_,A], [A,_,C]] -> [A,B,A,C].
%% Thus this predicate simply serves as a utity to be used by redundantPath below.
visited([], []) :- !.
visited([[A, _, B]], [A, B]) :- !.
visited([[A, _, _]|T], [A|C]) :- 
    T \= [], 
    visited(T, C).

%% True if the input list contains duplicate elements.
%% False if the input list has only unique members.
isSet([]).
isSet([_]).
isSet([H|T]) :-
    T \= [],
    (\+ once(member(H, T))),	%% Head must NOT be present in the Tail of the list.
    isSet(T).			%% The Tail of the list must also itself be a set.

%% Determines whether or not the input path is redundant,
%% 	this is to say the path visits a single city more than once.
%% Such paths should be excluded from results.
redundantPath(Path) :- 
    visited(Path, Visited),	%% Convert the path to a list of visited cities in order.
    (\+ isSet(Visited)).	%% Check if we visited any city more than once.

%% Create a single 'Flight Leg' from Origin to Dest.
leg(O, D, [O, Port, D]) :- flight(Port, O, D, _, _) ; flight(Port, D, O, _, _).

%% Recursively creates a 'Flight Path' from Origin to Dest.
%% Convenience predicate, excludes the utility visited list.
path(O, D, R) :- path(O, D, R, [O]).
%% Base case...
path(O, D, [Leg], _) :- leg(O, D, Leg).
%% Recursive case...
%% Make sure to pick an X we have not yet visited.
path(O, D, [H | T], Visited) :- 
    leg(O, X, H), 			%% Pick any leg from O -> X,
    \+ member(X, Visited),		%% provided we haven't picked X before,
    X \= D,				%% X shouldn't be our destination (base case),
    path(X, D, T, [X | Visited]).	%% then try to get from X to the destination.

%% Compute the cost for the given path.
cost([], Cost) :- Cost is 0.		%% No flight => No cost.
cost([[O, Port, D]], Cost) :-
    (flight(Port, O, D, C1, _) ;	%% C1 is the cost of the flight.
    flight(Port, D, O, C1, _)),		%% Flights go both directions, 
					%% 	so make sure we get the right one.
    airport(O, Tax, _),			%% Find the tax for the origin airport only.
    Cost is C1 + Tax.			%% Total cost is the sum of those two values.
cost([H|T], Cost) :-
    T \= [],				%% T should not be empty if we are going to recurse.
    cost([H], C1),			%% This should hit the base case right away.
    cost(T, C2),			%% Recurse to the tail of the list.
    Cost is C1 + C2.			%% Sum the results to get the total cost.

%% Compute the duration spent traveling for the given path (very similar to cost).
duration([], Dur) :- Dur is 0.
duration([[O, Port, D]], Dur) :-
    (flight(Port, O, D, _, T) ;		%% Get the length of the current flight.
    flight(Port, D, O, _, T)),		%% Flights go both directions, 
					%% 	so make sure we get the right one.
    airport(O, _, Delay),		%% Get the delay from the airport.
    Dur is T + Delay.			%% Duration is the length of the flight 
					%% 	plus airport delay.
duration([H|T], Dur) :-
    T \= [],				%% T should not be empty if we are going to recurse.
    duration([H], T1),			%% This should hit the base case right away.
    duration(T, T2),			%% Recurse to the tail of teh list.
    Dur is T1 + T2.			%% Sum the results to get the total duration.

%% Count the number of airlines in the flight path.
numairlines(Path, Count) :- numairlines(Path, Count, []).
%% Parameter 3 is a utility list containing the current airlines the path has flown on.
numairlines([], Count, _) :- Count is 0.
numairlines([[_, Port, _]], Count, Flown) :- 
    (once(member(Port, Flown)), Count is 0) ;	%% If we have already flown on Port, then 0,
    (once(\+ member(Port, Flown)), Count is 1).	%% else we need to add this port to our count.
numairlines([[A, Port, B]|T], Count, Flown) :-
    T \= [],					%% T should not be empty 
						%% 	if we are going to recurse.
    numairlines([[A, Port, B]], C1, Flown),	%% This should hit the base case right away.
    numairlines(T, C2, [Port|Flown]),
    Count is C1 + C2.				%% Sum the two results.

%%%%%%%%%%%%%%%%%%%
%% Predicate #1. %%
%%%%%%%%%%%%%%%%%%%

trip(Origin, Destination, [Price, Duration, NumAirlines, Path]) :-
    path(Origin, Destination, Path),
    cost(Path, Price),
    duration(Path, Duration),
    numairlines(Path, NumAirlines).

%%%%%%%%%%%%%%%%%%%
%% Predicate #2. %%
%%%%%%%%%%%%%%%%%%%

tripk(Origin, Destination, K, [Price, Duration, NumAirlines, Path]) :-
    trip(Origin, Destination, [Price, Duration, NumAirlines, Path]),
    (Duration < K).

%%%%%%%%%%%%%%%%%%%
%% Predicate #3. %%
%%%%%%%%%%%%%%%%%%%

multicitytrip(Origin, Destination, Intermediate, [Price, Duration, NumAirlines, Path]) :-
    trip(Origin, Intermediate, [_, _, _, P0]),		%% Origin -> Intermediate
    trip(Intermediate, Destination, [_, _, _, P1]),	%% Intermediate -> Destination
    merge(P0, P1, Path), 				%% Combine the paths.

    (\+ redundantPath(Path)),		%% Make sure the path is not redundant.
    cost(Path, Price),			%% Now all we have to do is find the cost
    duration(Path, Duration),		%% duration and numairlines for the
    numairlines(Path, NumAirlines).	%% combine paths that we found.

%%%%%%%%%%%%%%%%%%%%%%
%% Extra Utilities. %%
%%%%%%%%%%%%%%%%%%%%%%

%% Utility: Get the cheapest path by finding all possible paths.
cheapest(Origin, Destination, Cheapest) :-
    findall(Path, trip(Origin, Destination, Path), Paths),
    cheapest(Paths, Cheapest).

%% Base case: if we only get one path, the cheapest is that single path.
cheapest([Path], Path).
cheapest([[Price, A, B, C]|T], [Cost, Dur, NumA, Path]) :-
    T \= [],
    %% Find the cheapest path in the tail of the list.
    cheapest(T, [C0, _, _, _]),
    (((C0 < Price),
	%% Cheapest is in the tail, get it and store it in our "return value"...
	cheapest(T, [Cost, Dur, NumA, Path])
    ) ; ((C0 >= Price),
	%% else the current element is the cheapest, set it as the "return value".
	cheapest([[Price, A, B, C]], [Cost, Dur, NumA, Path])
    )).


%%%%%%%%%%%%%%%%%%%
%% Extra Credit. %%
%%%%%%%%%%%%%%%%%%%

%% Brute forced and will run out of memory very quickly,
%%	but easy as shit to implement,
%% 	and hopefully good for at least a couple free points.
:- dynamic airport/3.
findbesttrips(Origin, Route) :-
    retract(airport(X, A, B)),
    assert(airport(X, A, B)),
    X \= Origin,
    cheapest(Origin, X, Route).

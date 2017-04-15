:- [airlines].

%% Simple practice query, simply answers the question:
%% 	Is there a path between the cities X and Y?

reach(X, Y) :- flight(_, X, Y, _, _).
reach(X, Y) :- flight(_, X, Z, _, _), reach(Z, Y).

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

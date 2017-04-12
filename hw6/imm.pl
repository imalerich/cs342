:- [airlines].

%% Simple practice query, simply answers the question:
%% 	Is there a path between the cities X and Y?

reach(X, Y) :- flight(Line, X, Y, Cost, Time).
reach(X, Y) :- flight(Line, X, Z, Cost, Time), reach(Z, Y).

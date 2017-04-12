%% :-	implication
%% ,	conjunction
%% ;	disjunction

happy(vincent).
happy(kopaka).
listens2Music(butch).

%% Here the ',' means 'AND'.
playsAirGuitar(vincent) :-
    listens2Music(vincent),
    happy(vincent).

%% Two separate statements function as 'OR'.
playsAirGuitar(butch) :-
    happy(butch).
playsAirGuitar(butch) :-
    listens2Music(butch).

%% The ';' character is a shorthand for 'OR'.
shredsIce(kopaka) :-
    listens2Music(kopaka); happy(kopaka).

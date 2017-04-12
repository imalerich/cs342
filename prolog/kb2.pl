%% The following are facts, they are TRUE.
happy(yolanda).
listens2Music(mia).

%% The following are rules.
%% LHS is true if RHS is true.
listens2Music(yolanda) :- happy(yolanda).
playsAirGuitar(mia) :- listens2Music(mia).
playsAirGuitar(yolanda) :- listens2Music(yolanda).

%% This knowledege base contains 5 clauses (3 rules and 2 facts).

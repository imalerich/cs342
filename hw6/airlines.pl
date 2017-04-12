%% List of all available airports.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	CITY		TAX	DELAY %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

airport(newyork,	50, 	1).
airport(london,		75, 	2).
airport(madrid,		75, 	0.75).
airport(barcelona,	40, 	0.5).
airport(valencia,	40, 	0.33).
airport(malaga,		50, 	0.5).

%% you can add more airports for your testing

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	AIRLINE SRC DST			COST TIME %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% london <-> newyork
flight(aircanada, london, newyork,	500, 6).
flight(united, london, newyork,		650, 7).

%% newyork <-> madrid
flight(iberia, newyork, madrid, 	800, 8).
flight(united, madrid, newyork,		950, 9).
flight(aircanada, newyork, madrid,	900, 8).

%% madrid <-> barcelona
flight(aircanada, madrid, barcelona, 	100, 1).
flight(iberia, madrid, barcelona, 	120, 1.1).

%% london <-> barcelona
flight(iberia, london, barcelona, 	220, 4).

%% madrid <-> valencia
flight(iberia, valencia, madrid, 	40, 0.9).

%% valencia <-> barcelona
flight(iberia, valencia, barcelona, 	110, 1.25).

%% valencia <-> malaga
flight(iberia, valencia, malaga, 	80, 2).

%% madrid <-> malaga
flight(iberia, madrid, malaga, 		50, 0.5).

np_names(N) :- np(N, _ , _ ).
np_names_not_yosemite(N) :- np_names(N), \+ N = yosemite.
np_activities_yosemite(A) :- np(N, _ , A), N = yosemite.
np_states_yosemite(S) :- np(N, S, _ ), N = yosemite.
np_states_grandcanyon(S) :- np(N, S, _ ), N = grandcanyon.
np_states(N, S) :- np(N, S, _ ).
np_sorted_activities_yosemite(SA) :- np_activities_yosemite(A), sort(A, SA).
np_single_state(N) :- np_states(N, S), length(S, X), X = 1.
np_multi_state(N) :- np_states(N, S), length(S, X), X > 1.
np_pair_names([N1, N2]) :- np(N1, S, _), np(N2, S, _), N1 \= N2, N1 @< N2.
np_2_state_2_activities(N) :- np(N, S , A), length(S, X), X = 2, length(A, Z), Z = 2.
np_12_states_1or(N) :- np_single_state(N) ; np_states(N, S), length(S, X), X = 2.
np_12_states_2wo(N) :- np_single_state(N).
np_12_states_2wo(N) :- np_states(N, S), length(S, X), X = 2.
np_camping_hiking_1or(N) :- np(N, _, A), A = [camping, hiking] ; np(N, _, B), B = [hiking, camping].
np_camping_hiking_2wo(N) :- np(N, _, A), A = [camping, hiking].
np_camping_hiking_2wo(N) :- np(N, _, A), A = [hiking, camping].
np_camping_hiking_sort(N) :- np(N,_, A), sort(A, Z), Z = [camping, hiking].



insert(L,E,Z) :- append(L, [E], X), sort(X,Z).
butlast(L,Z) :- last(L,X), select(X, L, Z).
naaa([], [] , []).
naaa([H|T], NAL, AL) :- atom(H), naaa(T, NAL, AL1), append([H], AL1, AL).
naaa([H|T], NAL, AL) :- integer(H), naaa(T, NAL1, AL), append([H], NAL1, NAL).

splitlist([P|T], [], P, T).
splitlist([H|T], [H|T2], P, R) :- H \= P, splitlist(T, T2, P, R).

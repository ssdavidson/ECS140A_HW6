np_names(N) :- np(N, _ , _ ).
np_names_not_yosemite(N) :- np_names(N), \+ N = yosemite.
np_activities_yosemite(A) :- np(N, _ , A), N = yosemite.
np_states_yosemite(S) :- np(N, S, _ ), N = yosemite. 
np_states_grandcanyon(S) :- np(N, S, _ ), N = grandcanyon.
np_states(N, S) :- np(N, S, _ ).
np_sorted_activities_yosemite(SA) :- np_activities_yosemite(A), sort(A, SA).
np_single_state(N) :- np_states(N, S), length(S, X), X = 1. 
np_multi_state(N) :- np_states(N, S), length(S, X), X > 1.
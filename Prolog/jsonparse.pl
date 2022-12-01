%%%% -*- Mode: Prolog -*-

ws --> [W], { char_type(W, white) }, ws.
ws --> [].


json(O) --> element(O).


value(I) --> obj(I).
value(I) --> array(I).
value(I) --> string(I).
%value(I) --> json_number(I).
value(true) --> "true".
value(false) --> "false".
value(null) --> "null".


obj(Return) --> "{", ws, "}", !, {Return = jsonobj([])}.
obj(I) --> "{", ws, members_j(Vl), ws, "}", !, {I = jsonobj(Vl)}.

members_j(I) --> member(Q), ",", members_j(O), {append(Q, O, I)}.
members_j(I) --> member(O), !, {I = O}.

member(Q) --> ws, string(I), ws, ":" , ws, value(R), ws, !, {Q = [(I,R)]}.

array(O) --> "[", ws, "]", !, {O = jsonarray([])}.
array(O) --> "[", ws, elements(Vl), ws, "]", {O = jsonarray(Vl)}.

elements(O) --> element(I), ",", elements(Is), !, {append([I], Is, O)}.
elements(O) --> element(I), {O = [I]}.

element(O) --> ws, value(I), ws , {O = I}.

string(I) --> "\"", characters([], O) , "\"", !, {string_codes(I,O)},{print(I)},{nl}.

%json_number(_O) --> [].

%% ricordati di cambiarlo
characters(A, Out) -->
	["\\", Chr],
	characters([Chr, "\\" | A], Out).

characters(A, Out) -->
    [C], {C \= 34},
    characters([C | A], Out).

characters(A, Out) -->
    [],
    {reverse(A, Out)}, !.

json_parse(JSONString, Object) :-
    string(JSONString),
    string_codes(JSONString, L),
    phrase(json(Object), L),
    print(Object).

json_parse(JSONAtom, Object) :-
    atom(JSONAtom),
    atom_codes(JSONAtom, L),
    phrase(json(Object), L),
    print(Object).

%%% json_parse("{\"sasso\" : \"besugo\" ,\"besugo\" : {\"sasso1\": [\"s\",\"gabibbo\"]}}", O).

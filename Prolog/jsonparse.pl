%%%% -*- Mode: Prolog -*-

ws --> [W], { char_type(W, white) }, ws.
ws --> [].


json(O) --> element(O).


value(I) --> obj(I).
value(I) --> array(I).
value(I) --> string(I).
value(I) --> json_number(I).
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


json_number(O) --> json_integer(Ii), json_fraction(If), json_exponent(Ie),
		   {atom_concat(Ii, If, Is)}, {atom_concat(Is, Ie, I)},
		   {atom_number(I,O)}.

%json_number(O) --> json_integer(I), {atom_number(I,O)}.


json_integer(O) --> json_one_nine(I), json_digits(Is), {atom_concat(I, Is, O)}.
json_integer(O) --> "-", json_one_nine(I), json_digits(Is), {atom_concat(-, I, Ii)}, {atom_concat(Ii, Is, O)}.
json_integer(O) --> json_digit(O).
json_integer(O) --> "-", json_digit(I), {append(["-"], I, O)}.


json_digits(O) --> json_digit(I), {print("bela")}, json_digits(Is), !, {atom_concat(I, Is, O)}.
json_digits(O) --> json_digit(I), {O = I}.

json_digit('0') --> "0", !.
json_digit(O) --> json_one_nine(I), {O = I}.

json_one_nine(No) --> [N], {N > 48, N < 58}, {print("ciao")}, {No is N - 48},{print(No)} .


json_fraction(O) --> ".", json_digits(I), {atom_concat(.,I,O)}.
json_fraction('') --> [].

json_exponent(O) --> "E", json_sign(I), json_digits(Is),
		     {atom_concat('E',I,Ii)}, {atom_concat(Ii,Is,O)}.
json_exponent(O) --> "e", json_sign(I), json_digits(Is),
		     {atom_concat('e',I,Ii)}, {atom_concat(Ii,Is,O)}.
json_exponent('') --> [].


json_sign('+') --> "+".
json_sign('-') --> "-".
json_sign('') --> [].

ricordati di cambiarlo

characters(A, Out) -->
	["\\", "u"], !, hex(I1) ,hex(I2), hex(I3), hex(I4),
	characters([I4, I3, I2, I1, "u", "\\" | A], Out).

characters(A, Out) -->
	["\\", Chr],
	characters([Chr, "\\" | A], Out).

characters(A, Out) -->
    [C], {C \= 34},
    characters([C | A], Out).

characters(A, Out) -->
    [],
    {reverse(A, Out)}, !.


%%% controllo esplicito del hex anche se da alcune prove sembra che prolog faccia da solo con le strighe
hex(O) --> digit(I),{atom_string(I,O)}.
hex(H) --> [H], {H > 64, H < 71}.
hex(H) --> [H], {H > 96, H < 103}.

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
%%% json_parse('{\"sasso\" : 1.23 ,\"besugo\" : {\"sasso1\": [\"s\",\"gabibbo\u3A2f\"]}}', O).

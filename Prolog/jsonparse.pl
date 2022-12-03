%%%% -*- Mode: Prolog -*-

%% json_ws/0
% Skips whitespace (ws) as their defined in http://json.org
%
% Skips 20 space
% Skips 10 line feed (LF)
% Skips 13 carriage return (CR)
% Skips 9 horizontal tab (HT)

json_ws -->
    [W],
    { W == 20, W == 10, W == 13 , W == 9 },
    json_ws.

json_ws -->
    [].


%% json/1
% Defines json nonterminal
% as per definition found in the http://json.org

json(O) --> element(O).


%% json_value/1
% Defines value nonterminal
% as per definition found in the http://json.org
% a value is defined as being etheir
% an object an array a string a number or
% a true, false or null

json_value(O) --> json_obj(O).
json_value(O) --> array(O).
json_value(O) --> string(O).
json_value(O) --> json_number(O).
json_value(true) --> "true".
json_value(false) --> "false".
json_value(null) --> "null".


%% json_obj/1
% Defines json nonterminal
% as per definition found in the http://json.org
% an object is defined as being etheir a
% { ws } or
% { members }
% and parses by returning either the predicate
% jsonobj([]) or jsonobj([I])

json_obj(O) -->
    "{", json_ws, "}",
    {O = jsonobj([])}.

json_obj(O) -->
    "{",
    json_ws, json_members(I), json_ws,
    "}",
    {O = jsonobj(I)}.


%% json_members/1
% Defines members nonterminal
% as per definition found in the http://json.org
% members is defined as being:
% 1) member
% 2) member "," members
% and parses by appending the outputs of member
% and members if it's of the 2) form and
% returns directly the output of member if
% it's of the 1) form

json_members(O) --> json_member(I), ",", json_members(Is), {append(I, Is, O)}.
json_members(O) --> json_member(O).

json_member(Q) --> json_ws, string(I), json_ws, ":" , json_ws, json_value(R), json_ws, !, {Q = [(I,R)]}.

array(O) --> "[", json_ws, "]", !, {O = jsonarray([])}.
array(O) --> "[", json_ws, elements(Vl), json_ws, "]", {O = jsonarray(Vl)}.

elements(O) --> element(I), ",", elements(Is), !, {append([I], Is, O)}.
elements(O) --> element(I), {O = [I]}.

element(O) --> json_ws, json_value(I), json_ws , {O = I}.

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

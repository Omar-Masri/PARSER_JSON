%%%% -*- Mode: Prolog -*-
% Masri Omar 879237
% Piazza Lorenzo 886010
% Pirovano Diego 886009

%% json_ws/0
% Skips whitespace (ws) as their defined in http://json.org
%
% Skips 32 space
% Skips 10 line feed (LF)
% Skips 13 carriage return (CR)
% Skips 9 horizontal tab (HT)

json_ws -->
    [W],
    { W == 32; W == 10; W == 13; W == 9 },
    json_ws.

json_ws -->
    [].


%% json/1
% Defines json nonterminal
% as per definition found in the http://json.org

json(O) -->
    json_element(O).


%% json_value/1
% Defines value nonterminal
% as per definition found in the http://json.org
% a value is defined as being etheir
% an object an array a string a number or
% a true, false or null

json_value(O) --> json_obj(O).
json_value(O) --> json_array(O).
json_value(O) --> json_string(O).
json_value(O) --> json_number(O).
json_value(true) --> "true".
json_value(false) --> "false".
json_value(null) --> "null".


%% json_obj/1
% Defines json nonterminal
% as per definition found in the http://json.org
% an object is defined as being etheir a
% 1) '{' ws '}' or
% 2) '{' members '}'
% and parses by returning either the predicate
% jsonobj([]) or jsonobj([I])

json_obj(O) -->
    "{", json_ws, "}", !,
    { O = jsonobj([]) }.

json_obj(O) -->
    "{",
    json_ws, json_members(I), json_ws,
    "}",
    { O = jsonobj(I) }.


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

json_members(O) -->
    json_member(I),
    ",",
    json_members(Is),
    { append(I, Is, O) }.

json_members(O) -->
    json_member(O).


%% json_member/1
% Defines member nonterminal
% as per definition found in the http://json.org
% member is defined as being:
% 1) ws string ws ':' element
% and parses by creating an array with one tuple inside
% where the tuple has as the first item what is returned by string
% and as the second what is returned by value

json_member(O) -->
    json_ws,
    json_string(I),
    json_ws,
    ":",
    json_ws,
    json_value(Iv),
    json_ws, !,
    { O = [(I,Iv)] }.


%% json_array/1
% Defines array nonterminal
% as per definition found in the http://json.org
% array is defined as being:
% 1) '[' ws ']' or
% 2) '[' elements ']'
% and parses by returning either the predicate
% jsonarray([]) or jsonarray(I)

json_array(O) -->
    "[", json_ws, "]", !,
    { O = jsonarray([]) }.

json_array(O) -->
    "[",
    json_ws,
    json_elements(I),
    json_ws,
    "]",
    { O = jsonarray(I) }.


%% json_elements/1
% Defines elements nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) element or
% 2) element ',' elements
% and parses by appending the outputs of element
% and elements if it's of the 2) form and
% returns an array with the output of member inside of it
% if it's of the 1) form

json_elements(O) -->
    json_element(I),
    ",",
    json_elements(Is), !,
    { append([I], Is, O) }.

json_elements(O) -->
    json_element(I),
    { O = [I] }.


%% json_element/1
% Defines elements nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) ws value ws
% and parses by returning directly the output of value

json_element(O) -->
    json_ws,
    json_value(O),
    json_ws.


%% json_string/1
% Defines string nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) '"' characters '"'
% and parses by getting the output of charachter
% and transforming it to a string

json_string(O) -->
    "\"",
    characters([], I),
    "\"", !,
    { string_codes(O, I) },{print(O)},{nl}.


%% json_number/1
% Defines number nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) integer fraction exponent
% and parses by concatenating the output atoms from
% integer, fraction, and exponent and trasforming it to a number

json_number(O) -->
    json_integer(Ii), json_fraction(If), json_exponent(Ie),
    {atom_concat(Ii, If, Is)}, {atom_concat(Is, Ie, I)},
    {atom_number(I, O)}.


%% json_integer/1
% Defines integer nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) digit
% 2) onenine digit
% 3) '-' digit
% 4) '-' onenine digit
% and parses by concatenating the atoms retured from onenine
% and digits and eventually adding a minus at the start

json_integer(O) -->
    json_one_nine(I), json_digits(Is),
    {atom_concat(I, Is, O)}.

json_integer(O) -->
    "-", json_one_nine(I), json_digits(Is),
    {atom_concat(-, I, Ii)}, {atom_concat(Ii, Is, O)}.

json_integer(O) -->
    json_digit(O).

json_integer(O) -->
    "-", json_digit(I),
    { atom_concat(-, I, O) }.


%% json_digits/1
% Defines digits nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) digit
% 2) digit digits
% and parses by concatenating the atoms retured from onenine
% and digits and eventually adding a minus at the start

json_digits(O) -->
    json_digit(I), json_digits(Is), !,
    { atom_concat(I, Is, O) }.

json_digits(O) -->
    json_digit(O).


%% json_digit/1
% Defines digit nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) '0'
% 2) onenine
% and parses by returning the atom '0' if of the 1) form
% or returns directly the output of onenine if of 2) form

json_digit('0') -->
    "0", !.

json_digit(O) -->
    json_one_nine(O).


%% json_one_nine/1
% Defines onenine nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) '0' . '9'
% and parses by returning the number created by taking
% the ascii value of the charachter and subtracting 48 from it

json_one_nine(No) -->
    [N],
    { N > 48, N < 58 }, { print("ciao") },
    { No is N - 48 }, { print(No) }.


%% json_fraction/1
% Defines fraction nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) ""
% 2) '.' digits
% and parses by returning either and empty atom or the
% concatenation of '.' and the output of digits

json_fraction(O) -->
    ".", json_digits(I),
    { atom_concat(., I, O) }.

json_fraction('') --> [ ].


%% json_fraction/1
% Defines fraction nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) ""
% 2) 'E' sign digits
% 2) 'e' sign digits
% and parses by returning either and empty atom or the
% concatenation of 'E'/'e' , the output of sign and of digits

json_exponent(O) -->
    "E", json_sign(I), json_digits(Is),
    { atom_concat('E', I, Ii) }, { atom_concat(Ii, Is, O) }.

json_exponent(O) -->
    "e", json_sign(I), json_digits(Is),
    { atom_concat('e', I, Ii) }, { atom_concat(Ii, Is, O) }.

json_exponent('') --> [ ].


%% json_fraction/1
% Defines fraction nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) ""
% 2) '+'
% 2) '-'
% and parses by returning either and empty atom or
% the plus sign '+' or the minus sign '-'

json_sign('+') --> "+".
json_sign('-') --> "-".
json_sign('') --> [ ].


%% ricordati di cambiarlo

characters(A, Out) -->
    ["\\", "u"], !,
    hex(I1) ,hex(I2), hex(I3), hex(I4),
    characters([I4, I3, I2, I1, "u", "\\" | A], Out).

characters(A, Out) -->
    ["\\", Chr],
    characters([Chr, "\\" | A], Out).

characters(A, Out) -->
    [ C ], { C \= 34 },
    characters([C | A], Out).

characters(A, Out) -->
    [ ],
    { reverse(A, Out) }.


%%% controllo esplicito del hex anche se da alcune prove sembra che prolog faccia da solo con le strighe

%% hex/1
% Defines hex nonterminal
% as per definition found in the http://json.org
% elements is defined as being:
% 1) digit
% 2) 'A' . 'F'
% 2) 'a' . 'f'
% and parses by returning the string of the hex figure

hex(O) -->
    json_digit(I),
    { atom_string(I,O) }.

hex(H) -->
    [H],
    { H > 64, H < 71 }.

hex(H) -->
    [H],
    { H > 96, H < 103 }.


jsonparse(JSONString, Object) :-
    string(JSONString),
    string_codes(JSONString, L),
    phrase(json(Object), L),
    print(Object).

jsonparse(JSONAtom, Object) :-
    atom(JSONAtom),
    atom_codes(JSONAtom, L),
    phrase(json(Object), L),
    print(Object).

%%% jsonparse("{\"sasso\" : \"besugo\" ,\"besugo\" : {\"sasso1\": [\"s\",\"gabibbo\"]}}", O).
%%% jsonparse('{\"sasso\" : 1.23 ,\"besugo\" : {\"sasso1\": [\"s\",\"gabibbo\u3A2f\"]}}', O).


%%jsonaccess/3
%jsonaccess(jsonobj(), field, out)

jsonaccess(jsonobj(Members), [], jsonobj(Members)) :- !.
jsonaccess(jsonobj([(X, Out) | _]), X, Out).
jsonaccess(jsonobj([_ | Rest]), X, Out) :-
    string(X),
    jsonaccess(jsonobj(Rest), X, Out).

jsonaccess(jsonobj(Members), [X], Out) :-
    jsonaccess(jsonobj(Members), X, Out).

jsonaccess(jsonobj([(X, jsonarray([E | _])) | _]), [X, 0], E) :- !.

jsonaccess(jsonobj([(X, jsonarray([_ | Elements])) | _]), [X, N], Out) :-
    integer(N),
    N \= 0,
    N1 is N - 1,
    access_array(Elements, N1, Out).

jsonaccess(jsonobj([_ | Rest]), [X, N], Out) :-
    string(X),
    integer(N),
    jsonaccess(jsonobj(Rest), [X , N], Out).

jsonaccess(jsonobj([(_, jsonobj(Members)) | _ ]), X, Out) :-
    string(X),
    jsonaccess(jsonobj(Members), X, Out).

access_array([E | _], 0, E) :- !.
access_array([_ | Elements], N, Out) :-
    N1 is N - 1,
    access_array(Elements, N1, Out).

%%input e output
%%jsonread/2
%%jsonread(FileName, JSON)
jsonread(FileName, JSONObj) :-
    open(FileName, read, In),
    read_string(In, _, JSON),
    close(In),
    jsonparse(JSON, JSONObj).

%%jsonwrite/2
%%jsonwrite(JSONObj, FileName)
%jsonwrite(JSONObj, FileName) :-
%    phrase(json(Object), L).

%%jsonencode/2
jsonencode([], "") :- !.

jsonencode(jsonobj(I), Out) :-
    jsondecode(I, O1),
    string_concat("{/n", O1, O2),
    string_concat(O2, "/n}/n", Out), !.

jsonencode(jsonarray(I), Out) :-
    jsondecode(I, O1),
    string_concat(" [ ", O1, O2),
    string_concat(O2, " ] ", Out), !.

jsonencode([X | []], Out) :-
    jsondecode(X, Out), 
    !.

jsonencode([X | Xs], Out) :-
    jsondecode(X, O1),
    string_concat(O1, ",\n", O2), 
    jsondecode(Xs, O3),
    string_concat(O2, O3, Out), !.

jsonencode((X, Y), Out) :-
    %%potrei chiamare direttamente term_string/2
    %%dato che X è per forza una string
    jsondecode(X, XDecoded),
    string_concat(XDecoded, " : ", O1),
    jsondecode(Y, O2),
    string_concat(O1, O2, Out),
    !.
%%chiede se si può usare term_string/2
jsonencode(X, Out) :- term_string(X, Out).
    

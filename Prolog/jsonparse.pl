%%%% -*- Mode: Prolog -*-

%% Members:
% Masri Omar 879237
% Piazza Lorenzo 886010
% Pirovano Diego 886009


%% json_ws/0
% Skips whitespaces (ws) as their defined in http://json.org
%
% Skips ASCII code 32 - space
% Skips ASCII code 10 - line feed (LF)
% Skips ASCII code 13 - carriage return (CR)
% Skips ASCII code 9 - horizontal tab (HT)

json_ws -->
    [W],
    { W == 32; W == 10; W == 13; W == 9 },
    json_ws.

json_ws -->
    [].


%% json/1
% Defines json nonterminal
% as per definition found in http://json.org

json(O) -->
    json_element(O).


%% json_value/1
% Defines value nonterminal
% as per definition found in http://json.org
% a value is defined as being either:
% 1) object
% 2) array
% 3) string
% 4) number
% 5) true or false or null

json_value(O) --> json_obj(O).
json_value(O) --> json_array(O).
json_value(O) --> json_string(O).
json_value(O) --> json_number(O).
json_value(true) --> "true".
json_value(false) --> "false".
json_value(null) --> "null".


%% json_obj/1
% Defines object nonterminal
% as per definition found in http://json.org
% an object is defined as being etheir:
% 1) '{' ws '}' or
% 2) '{' members '}'
% and parses by returning either the predicate:
% jsonobj([]) if the string is of the form 1)
% jsonobj([I]) if the string is of the form 2)
% where "I" is the output of json_members/1

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
% as per definition found in http://json.org
% members is defined as being either:
% 1) member
% 2) member "," members
% and parses by returning a list created by appending the outputs
% of json_member/1 and json_members/1 if the string is of the form 2)
% or by returning directly the output of json_member/1 if
% the string is of the form 1)

json_members(O) -->
    json_member(I),
    ",",
    json_members(Is),
    { append(I, Is, O) }.

json_members(O) -->
    json_member(O).


%% json_member/1
% Defines member nonterminal
% as per definition found in http://json.org
% member is defined as being:
% 1) ws string ws ':' element
% and parses by returning a list created with one tuple inside, where
% the tuple has as the first item what is returned by json_string/1
% and as the second what is returned by json_value/1

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
% as per definition found in http://json.org
% array is defined as being either:
% 1) '[' ws ']' or
% 2) '[' elements ']'
% and parses by returning either the predicate:
% jsonarray([]) if the string is of the form 1)
% jsonarray(I) if the string is of the form 2)
% where "I" is the output of json_elements/1

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
% as per definition found in http://json.org
% elements is defined as being either:
% 1) element
% 2) element ',' elements
% and parses by returning a list created by appending the outputs of
% json_element/1 and json_elements/1 if the string is of the form 2)
% or by returning an array with the output of json_member/1 inside of it
% if it's of the form 1)

json_elements(O) -->
    json_element(I),
    ",",
    json_elements(Is), !,
    { append([I], Is, O) }.

json_elements(O) -->
    json_element(I),
    { O = [I] }.


%% json_element/1
% Defines element nonterminal
% as per definition found in http://json.org
% element is defined as being:
% 1) ws value ws
% and parses by returning directly the output of json_value/1

json_element(O) -->
    json_ws,
    json_value(O),
    json_ws.


%% json_string/1
% Defines string nonterminal
% as per definition found in http://json.org
% string is defined as being:
% 1) '"' characters '"'
% and parses by getting the output of json_charachter/1
% and converts it to a string

json_string(O) -->
    "\"",
    json_characters([], I),
    "\"", !,
    { string_codes(O, I) },{print(O)},{nl}.


%% json_number/1
% Defines number nonterminal
% as per definition found in http://json.org
% number is defined as being:
% 1) integer fraction exponent
% and parses by concatenating the output atoms of
% json_integer/1, json_fraction/1 and json_exponent/1
% and converting the atom, resulting from the concatenation,
% to a number and returning said number

json_number(O) -->
    json_integer(Ii), json_fraction(If), json_exponent(Ie),
    {atom_concat(Ii, If, Is)}, {atom_concat(Is, Ie, I)},
    {atom_number(I, O)}.


%% json_integer/1
% Defines integer nonterminal
% as per definition found in http://json.org
% integer is defined as being either:
% 1) digit
% 2) onenine digit
% 3) '-' digit
% 4) '-' onenine digit
% and parses by concatenating the atoms returned from json_onenine/1
% and json_digits/1 and eventually, if the string is of the
% form 1) or 2), adding a minus at the start

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
% as per definition found in http://json.org
% digits is defined as being either:
% 1) digit
% 2) digit digits
% and parses by concatenating the atoms returned from json_digit/1
% and json_digits/1 if the string is of the form 2)
% or by returing directly the output of json_digit/1 if
% the string is of the form 1)

json_digits(O) -->
    json_digit(I), json_digits(Is), !,
    { atom_concat(I, Is, O) }.

json_digits(O) -->
    json_digit(O).


%% json_digit/1
% Defines digit nonterminal
% as per definition found in http://json.org
% digit is defined as being either:
% 1) '0'
% 2) onenine
% and parses by returning the atom '0' if the string is of the form 1)
% or by returning directly the output of json_onenine/1 if of form 2)

json_digit('0') -->
    "0", !.

json_digit(O) -->
    json_one_nine(O).


%% json_one_nine/1
% Defines onenine nonterminal
% as per definition found in http://json.org
% onenine is defined as being:
% 1) '0' . '9'
% and parses by returning the number created by taking
% the ASCII value of the charachter and subtracting 48 from it

json_one_nine(No) -->
    [N],
    { N > 48, N < 58 }, { print("ciao") },
    { No is N - 48 }, { print(No) }.


%% json_fraction/1
% Defines fraction nonterminal
% as per definition found in the http://json.org
% fraction is defined as being either:
% 1) ""
% 2) '.' digits
% and parses by returning either an empty atom if the string is of the
% form 1) or the concatenation of '.' and the output of json_digits/1
% if the string is of the form 2)

json_fraction(O) -->
    ".", json_digits(I),
    { atom_concat(., I, O) }.

json_fraction('') --> [ ].


%% json_exponent/1
% Defines exponent nonterminal
% as per definition found in http://json.org
% exponent is defined as being either:
% 1) ""
% 2) 'E' sign digits
% 3) 'e' sign digits
% and parses by returning either an empty atom if the string is of the
% form 1) or the concatenation of 'E' or 'e' and the outputs of
% json_sign/1 and of json_digits/1 if the string is of the form 2) 3)

json_exponent(O) -->
    "E", json_sign(I), json_digits(Is),
    { atom_concat('E', I, Ii) }, { atom_concat(Ii, Is, O) }.

json_exponent(O) -->
    "e", json_sign(I), json_digits(Is),
    { atom_concat('e', I, Ii) }, { atom_concat(Ii, Is, O) }.

json_exponent('') --> [ ].


%% json_sign/1
% Defines sign nonterminal
% as per definition found in http://json.org
% sign is defined as being either:
% 1) ""
% 2) '+'
% 3) '-'
% and parses by returning either an empty atom,
% the atom '+' or the atom '-'

json_sign('+') --> "+".
json_sign('-') --> "-".
json_sign('') --> [ ].


%% ricordati di cambiarlo
%% json_characters/1
% accumulates chars and returns the reverse of the accumulated chars
% the predicate avoids the ASCII code 34 (") and has a special case
% for handling escape sequences

json_characters(A, Out) -->
    ["\\", "u"], !,
    hex(I1) ,hex(I2), hex(I3), hex(I4),
    json_characters([I4, I3, I2, I1, "u", "\\" | A], Out).

json_characters(A, Out) -->
    ["\\", Chr],
    json_characters([Chr, "\\" | A], Out).

json_characters(A, Out) -->
    [ C ], { C \= 34 },
    json_characters([C | A], Out).

json_characters(A, Out) -->
    [ ],
    { reverse(A, Out) }.


%%% controllo esplicito del hex anche se da alcune prove sembra che prolog faccia da solo con le strighe e atomi

%% hex/1
% Defines hex nonterminal
% as per definition found in http://json.org
% hex is defined as being:
% 1) digit
% 2) 'A' . 'F'
% 2) 'a' . 'f'
% and parses by returning the char of the hex digit

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
    jsonaccess(jsonobj(Rest), [X , N], Out).

jsonaccess(jsonobj([(X, jsonobj(Members)) | _ ]), [X, C], Out) :-
    string(X),
    jsonaccess(jsonobj(Members), C, Out).

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


%% jsonwrite/2
jsonwrite(JSONObj, FileName) :-
    jsonencode(JSONObj, Json, 0),
    open(FileName, write, Out),
    write(Out, Json),
    close(Out).
    

%%jsonencode/3
jsonencode([], "", _) :- !.

jsonencode(jsonobj(I), Out, Indent) :-
    integer(Indent),
    IncrementedIndent is Indent + 1,
    jsonencode(I, O1, IncrementedIndent),
    addtab(IncrementedIndent, Tab),
    addtab(Indent, Tab1),
    string_concat("{\n", Tab, Prev),
    string_concat(Prev, O1, O2),
    string_concat("\n", Tab1, Prev1),
    string_concat(Prev1, "}", Next),
    string_concat(O2, Next, Out), !.

jsonencode(jsonarray(I), Out, Indent) :-
    In is Indent + 1,
    jsonencode(I, O1, In),
    addtab(In, Tab),
    addtab(Indent, Tab1),
    string_concat("[\n", Tab, Prev),
    string_concat(Prev, O1, O2),
    string_concat("\n", Tab1, Mid),
    string_concat(Mid, "]", Next),
    print(Next),
    string_concat(O2, Next, Out), !.

jsonencode([X | []], Out, Indent) :-
    jsonencode(X, Out, Indent), 
    !.

jsonencode([X | Xs], Out, Indent) :-
    jsonencode(X, O1, Indent),
    addtab(Indent, Tab),
    string_concat(",\n", Tab, Mid),
    string_concat(O1, Mid, O2), 
    jsonencode(Xs, O3, Indent),
    string_concat(O2, O3, Out), !.


jsonencode((X, Y), Out, Indent) :-
    jsonencode(X, XEncoded, Indent),
    string_concat(XEncoded, " : ", O1),
    jsonencode(Y, O2, Indent),
    string_concat(O1, O2, Out),
    !.


jsonencode(X, Out, _) :- term_string(X, Out).

addtab(0, "") :- !.

addtab(Indent, Out1) :-
    integer(Indent),
    Indent > 0,
    NewIndent is Indent - 1,
    addtab(NewIndent, Out),
    string_concat(Out, "\t", Out1).

%%% jsonparse("{\"sasso\" : \"besugo\" ,\"besugo\" : {\"sasso1\": [\"s\",\"gabibbo\"]}}", O).
%%% jsonparse('{\"sasso\" : 1.23 ,\"besugo\" : {\"sasso1\": [\"s\",\"gabibbo\u3A2f\"]}}', O).

%%%% domande
%% chiedi arita delle dcg
%% chiedi per utilizzo degli or (;) in ws
%% chiedi per utilizzo indentazione su json encode
%% chiedi per atom_concat e eventualmente atomic_list_concat e string_concat
%% chiedi per hex e la gestione degli escape in json soprattutto per quanto riguarda hex


%%%% end of file -- jsonparse.pl --

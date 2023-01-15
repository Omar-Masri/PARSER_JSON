Team members:
Masri Omar: 879237
Piazza Lorenzo: 886010
Pirovano Diego: 886009

----------------------

Goal: create a json parser implementation using prolog.

Style:
we tried to write the code to be as close to the grammar laid down in json.org
as to make the code more readable

end user predicates:

jsonread/2: jsonread(Filename, JSON) opens the file "filename" and tries to create a parsed json 
by calling the jsonparse predicate and unifying it's result in the variable "JSON", 
with "parsed json" we mean an easy to manipulate prolog structure that keeps json properties,
this structure will be referred to as "JSONObject" from now on.

jsonparse/2: jsonparse(JSONString, Obj) true if the JSON structure in "JSONString" is valid
it is unifies the parsed result with "Obj"; 
We decided to implement it using DCGs and we achieved that via support predicates for each 
possible end value (string, number, any other composite term)(as per json.org specifications).
We also managed HEX characters.

jsonaccess/3: jsonaccess(JSONObject, Fields, Result) true if the trail (prolog list) of fields specified in "Fields" 
trails you back to a valid value starting from "JSONObject" and unifies the result in "Result", 
it's contemplated having in "Fields" a single string instead of a list of fields.
If there is more than one match it unifies with all of them.
E.g.) 

?- jsonparse("{\"sogliola\":1,\"sogliola\": 2 }", Out), jsonaccess(Out, ["sogliola"], Res).

Out = jsonobj([("sogliola", 1), ("sogliola", 2)]),
Res = 1 ;
Out = jsonobj([("sogliola", 1), ("sogliola", 2)]),
Res = 2 ;
false.


jsondump/2: jsondump(JSONObject, Filename) converts JSONObject back into a standard JSON 
and overwrites it into the "Filename" file or creates it if it does not exist, 
the conversion is achieved via the "jsonencode/3" predicate.

-------

Extra-Features: 
1) identification and handling of unicode escape \u hex hex hex hex
2) formatting in jsondump, the output file jsondump/2 will be formatted in a more readable form.

-------

Examples:

jsonparse("{
	\"besugo\":1,
	\"trota\":[
        {\"salmone\":[[0,12e2],[4]]},
		{\"merluzzo\":[[true],[null, 14.2e3]]}]
}", Out).

jsonparse("\"storione \\\t acciuga \"", Out).


jsonparse("{
	\"besugo\":1,
	\"trota\":[
        {\"salmone\":[[0,12e2],[4]]},
		{\"merluzzo\":[[true],[null, 14.2e3]]}]}", Out), jsonaccess(Out, ["trota",1,"merluzzo",1,1], Res).


jsonparse("{\"besugo\":1, \"trota\":[ {\"salmone\":[[0,12e2],[4]]},
{\"merluzzo\":[[true],[null, 14.2e3]]}]}", Out), jsondump(Out, "out.txt").

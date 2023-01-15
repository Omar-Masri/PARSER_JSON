Team members:
Masri Omar: 879237
Piazza Lorenzo: 886010
Pirovano Diego: 886009

----------------------

Goal: create a json parser implementation using lisp.

Style:
we tried to write the code to be as close to the grammar laid down in json.org
as to make the code more readable

end-user functions:

jsonread: jsonread(Filename) opens the file "filename" and tries to create a parsed json
by calling the jsonparse function that will either generate and error, 
if the structure does not respect JSON definitions, or return an easy to manipulate Lisp structure (a list) 
that keeps json properties, this structure will be referred to as "JSONObject" from now on.

jsonparse: jsonparse(JSONString) if the JSON structure in "JSONString" is valid it returns the
parsed result if it's not generates an error ("ERROR: syntax error"); 
Since in prolog we used DCGs we decided to use a similar approach here by creating 
a few functions that simulate their behaviour.

jsonaccess: jsonaccess(JSONObject, &rest Fields) tries to find a value to return that corresponds
to the trail of fields in the JSONObject variable, if it doesn't find any it returns an error.
Whilst in the prolog implematation it returns all the possible values reachable by the trail, in lisp
it returns only the first one, for example:
(jsonaccess (jsonparse "{\"ciao\":1, \"ciao\":2}") "ciao")
returns:
1.

jsondump: jsondump(Filename) converts JSONObject back into a standard JSON
and overwrites it into the "Filename" file or creates it if it does not exist, 
the conversion is achieved via the "jsonencode" predicate.
We didn't implement the json tabulation because of time constraints.

-------------

Extra-features:
1) formatting in jsondump, the output file jsondump will be formatted in a more readable form.

-------------

Examples:

(jsonparse "{
	\"besugo\":1,
	\"trota\":[
        {\"salmone\":[[0,12e2],[4]]},
		{\"merluzzo\":[[true],[null, 14.2e3]]}]
}")

(jsonparse "\"storione \\t acciuga \"")


(jsonaccess (jsonparse "{
	\"besugo\":1,
	\"trota\":[
        {\"salmone\":[[0,12e2],[4]]},
		{\"merluzzo\":[[true],[null, 14.2e3]]}]
}") "trota" 1 "merluzzo" 1 1)

(jsondump "out.txt" (jsonparse "{\"besugo\":1, \"trota\":[ {\"salmone\":[[0,12e2],[4]]},
{\"merluzzo\":[[true],[null, 14.2e3]]}]}"))


Team members:
Masri Omar: 879237
Piazza Lorenzo: 886010
Pirovano Diego: 886009

----------------------

Goal: create a json parser implementation using prolog.

end user functions:

jsonread/1: jsonread(Filename) opens the file "filename" and tries to create a parsed json 
by calling the jsonparse function that will either generate and error, 
if the structure does not respect JSON definitions, or return an easy to manipulate Lisp structure (a list) 
that keeps json properties, this structure will be referred to as "JSONObject" from now on.

jsonparse/1: jsonparse(JSONString) if the JSON structure in "JSONString" is valid it returns the 
parsed result if it's not generates an error ("ERROR: syntax error"); 
Since in prolog we used DCGs we decided to use a similar approach here by creating 
a few functions that simulate their behaviour.

jsonaccess/2: jsonaccess(JSONObject, &rest Fields) tries to find a value to return that corresponds 
to the trail of fields in the JSONObject variable, if it doesn't find any it returns an error.
Whilst in the prolog implematation it returns all the possible values reachable by the trail, in lisp
it returns only the first one, for example:
(jsonaccess (jsonparse "{\"ciao\":1, \"ciao\":2}") "ciao") 
returns:
1.

jsondump/1: jsondump(Filename) converts JSONObject back into a standard JSON 
and overwrites it into the "Filename" file or creates it if it does not exist, 
the conversion is achieved via the "jsonencode/2" predicate.
We didn't implement the json tabulation because of time constraints.
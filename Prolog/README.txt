Team members:
Masri Omar: 879237
Piazza Lorenzo: 886010
Pirovano Diego: 886009

----------------------

Goal: create a json parser implementation using prolog.

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

jsondump/2: jsondump(JSONObject, Filename) converts JSONObject back into a standard JSON 
and overwrites it into the "Filename" file or creates it if it does not exist, 
the conversion is achieved via the "jsonencode/3" predicate.
We also implemented the standard json tabulation when writing in the file.
# PARSER_JSON

Basic JSON parser implemented in Lisp (Purely Functional) and Prolog

## PROLOG
Parse:
```
?- jsonparse("{
	\"tuna\":1,
	\"trout\":[
        {\"salmon\":[[0,12e2],[4]]},
		{\"cod\":[[true],[null, 14.2e3]]}]}", O).

O = jsonobj([("tuna", 1), ("trout", jsonarray([jsonobj([("salmon", jsonarray(...))]), jsonobj([(..., ...)])]))]).
```

Read:
```
?- jsonread("json1.txt", O).

O = ...
```

Dump:
```
?- jsonparse("{
	\"tuna\":1,
	\"trout\":[
        {\"salmon\":[[0,12e2],[4]]},
		{\"cod\":[[true],[null, 14.2e3]]}]}", O), jsondump(O, "fishes.json").

O = jsonobj([("tuna", 1), ("trout", jsonarray([jsonobj([("salmon", jsonarray(...))]), jsonobj([(..., ...)])]))]).
```
Access:
```
?- jsonparse("{
	\"tuna\":1,
	\"trout\":[
        {\"salmon\":[[0,12e2],[4]]},
		{\"cod\":[[true],[null, 14.2e3]]}]}", O), jsonaccess(O, ["trout",1,"cod",1,1], V).

O = jsonobj([("tuna", 1), ("trout", jsonarray([jsonobj([("salmon", jsonarray(...))]), jsonobj([(..., ...)])]))]),
V = 14200.0 ;
false.
```

## LISP
Parse:
```
CL-USER> (jsonparse "{
	\"tuna\":1,
	\"trout\":[
        {\"salmon\":[[0,12e2],[4]]},
		{\"cod\":[[true],[null, 14.2e3]]}]
}")

(JSONOBJ ("tuna" 1)
 ("trout"
  (JSONARRAY
   (JSONOBJ ("salmon" (JSONARRAY (JSONARRAY 0 1200.0) (JSONARRAY 4))))
   (JSONOBJ ("cod" (JSONARRAY (JSONARRAY TRUE) (JSONARRAY NULL 14200.0)))))))
```
Read:
```
CL-USER> (jsonread "json.txt")

...
```
Dump:
```
CL-USER> (jsondump (jsonparse "{
	\"tuna\":1,
	\"trout\":[
        {\"salmon\":[[0,12e2],[4]]},
		{\"cod\":[[true],[null, 14.2e3]]}]
}") "fishes.json")

NIL
```
Access:
```
CL-USER> (jsonaccess (jsonparse "{
	\"tuna\":1,
	\"trout\":[
        {\"salmon\":[[0,12e2],[4]]},
		{\"cod\":[[true],[null, 14.2e3]]}]
}") "trout" 1 "cod" 1 1)

14200.0
```

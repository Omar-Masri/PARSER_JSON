# PARSER_JSON


# Modalità d'esame e regole
L'insegnamento di Linguaggi di Programmazione prevede il superamento di una prova teorica (uno scritto) e di un progetto di programmazione. A discrezione del docente potrà essere richiesta anche una prova orale. La prova scritta ed il progetto corrispondente al primo appello possono essere superati in due prove "in itinere"

Le prove in itinere sono due scritti offerti in Novembre (Programmazione Logica e Prolog) e a Gennaio/Febbraio (Programmazione Funzionale e Lisp; Programmazione Imperativa e C), e uno o due progetti in itinere, postati a fine Ottobre, da consegnare in forma elettronica a Gennaio/Febbraio (Common Lisp, Prolog e/o C). Le prove in itinere costituiscono il primo appello per il corso, e non sono recuperabili nei restanti appelli.

Dall'appello successivo (Febbraio/Marzo) lo scritto comprende tutte le parti del corso. Lo stesso vale per i progetti. Gli appelli seguenti saranno a Giugno, Luglio e Settembre (in data da precisarsi).

La parte teorica ed i progetti di laboratorio vanno superati entrambi nella stessa tornata; il voto complessivo è la media, ponderata a seconda dei casi, dei voti parziali.

I progetti posso essere svolti in gruppo (massimo 3 persone), ma sono da consegnare individualmente. Chi svolge il progetto da solo od in coppia potrà avere un bonus.

Nota finale: tutti gli esiti saranno caricati nel sistema: voto, ritirato, assente, insufficiente etc.

# Nota (studenti Anni Accademici precedenti al corrente)
Gli studenti degli anni precedenti che devono ancora passare l'esame di Linguaggi di Programmazione da 12 crediti (su tre moduli: Paradigmi di LP, Linguaggi e Computabilità e Laboratorio di LP) possono soddisfare i requisiti per Paradigmi di LP e Laboratorio di LP passando le prove del corso di Linguaggi di Programmazione corrente da 8 crediti. Lo stesso vale per gli studenti che devono ancora passare l'esame di Linguaggi da 6 crediti. Caveat: i contenuti delle prove rifletteranno i contenuti del corso LP attuale; non saranno predisposte prove speciali per gli studenti degli anni precedenti.

# Istruzioni per la stesura del codice.

Il codice C/C++, (Common) Lisp, Haskell, Javascript, Python, R, Julia ed il codice Prolog devono essere editati seguendo una serie minima di regole convenzionali al fine di renderli il più leggibili possibile. In particolare valgono le regole convenzionali seguenti.

Il testo non deve MAI superare le 80 colonne di ampiezza.
Il testo deve essere correttamente indentato.
Gli operatori in C/C++ e Prolog (e Python, R, Haskell, etc) vanno sempre delimitati correttamente: spazi attorno agli operatori eccezion fatta per le virgole, che hanno solo uno spazio dopo.
Qualunque codice presentato che non segue queste semplici regole verrà cassato.

Qualora abbiate delle rimostranze al riguardo, le potete fare; nel qual caso sarete interrogati su: (a) GNU Coding Standards, (b) Google Coding Standards e (c) Node/NPM/Javascript Coding standards.


L'editor necessario (non fosse altro perché è tuttora quello con le migliori procedure di indentazione automatica del codice) è, ça va sans dire, Emacs (e derivati).

# Correzione Progetti

I progetti saranno pre-processati con i seguenti strumenti UN*X

Il file yourfile.c (o .lisp, o .pl) sarà reindentato da Emacs
Il comando diff sarà invocato sui due file

$ diff yourfile.c yourfile-ind.c

Se ci saranno delle differenze il vostro progetto risulterà insufficiente.
Il comando cut; in particolare i vostri files saranno riscritti così:

$ cut –c -80 yourfile-ind.c > yourfile-processed.c

Il file yourfile-processed.c è quello che sarà usato per la valutazione; ovviamente, i sistemi Lisp, Prolog e C non amano codice incompleto e sintatticamente scorretto.  Se il vostro progetto genera un errore sarà considerato insufficiente.
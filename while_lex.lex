type pos = int
type svalue = Tokens.svalue
 
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val badCh : string * string * int * int -> unit = fn
            (fileName,bad,line,col) =>
            TextIO.output(TextIO.stdOut,fileName^"["
            ^Int.toString line^"."^Int.toString col
            ^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => Tokens.EOF (!lin,!col);


%%
%header (functor WhlLexFun(structure Tokens: Whl_TOKENS));
%arg (fileName:string);
id = [a-zA-Z]+[a-zA-Z0-9]*;
whitespace = [\ \t\b\r]+;
digit =  ["+"|"~"]?[0-9]+;
eol = ("\013\010"|"\010"|"\013");

%%

"||"  => (col:=yypos-(!eolpos); Tokens.OR(!lin,!col));
"&&"  => (col:=yypos-(!eolpos); Tokens.AND(!lin,!col));
"!"  => (col:=yypos-(!eolpos); Tokens.NOT(!lin,!col));
">=" => (col:=yypos-(!eolpos); Tokens.GEQ(!lin,!col));
">"  => (col:=yypos-(!eolpos); Tokens.GT(!lin,!col));
"<=" => (col:=yypos-(!eolpos); Tokens.LEQ(!lin,!col));
"<"  => (col:=yypos-(!eolpos); Tokens.LT(!lin,!col));
"<>" => (col:=yypos-(!eolpos); Tokens.NEQ(!lin,!col));
"=" => (col:=yypos-(!eolpos); Tokens.EQ(!lin,!col));
"/"  => (col:=yypos-(!eolpos); Tokens.DIV(!lin,!col));
"*"  => (col:=yypos-(!eolpos); Tokens.TIMES(!lin,!col));
"%"  => (col:=yypos-(!eolpos); Tokens.MOD(!lin,!col));
"-"  => (col:=yypos-(!eolpos); Tokens.MINUS(!lin,!col));
"~"  => (col:=yypos-(!eolpos); Tokens.NEG(!lin,!col));
"+"  => (col:=yypos-(!eolpos); Tokens.PLUS(!lin,!col));
"{" => (col:=yypos-(!eolpos); Tokens.LBRACE(!lin,!col));
"}" => (col:=yypos-(!eolpos); Tokens.RBRACE(!lin,!col));
"(" => (col:=yypos-(!eolpos); Tokens.LPARAN(!lin,!col));
")" => (col:=yypos-(!eolpos); Tokens.RPARAN(!lin,!col));
":=" => (col:=yypos-(!eolpos); Tokens.SET(!lin,!col));
";" => (col:=yypos-(!eolpos); Tokens.SEMICOLON(!lin,!col));
"::" => (col:=yypos-(!eolpos); Tokens.DOUBCOLON(!lin,!col));
":" => (col:=yypos-(!eolpos); Tokens.COLON(!lin,!col));
"," => (col:=yypos-(!eolpos); Tokens.COMMA(!lin,!col));
"tt" => (col:=yypos-(!eolpos); Tokens.TR(!lin,!col));
"ff" => (col:=yypos-(!eolpos); Tokens.FAL(!lin,!col));

"var"  	 => (col:=yypos-(!eolpos); Tokens.VAR(!lin,!col));
"program"  => (col:=yypos-(!eolpos); Tokens.PROG(!lin,!col));
"int"  	 => (col:=yypos-(!eolpos); Tokens.INT(!lin,!col));
"bool"   => (col:=yypos-(!eolpos); Tokens.BOOL(!lin,!col));
"write"   => (col:=yypos-(!eolpos); Tokens.WRITE(!lin,!col));
"read"   => (col:=yypos-(!eolpos); Tokens.READ(!lin,!col));
"while" => (col:=yypos-(!eolpos); Tokens.WHILE(!lin,!col));
"do" => (col:=yypos-(!eolpos); Tokens.DO(!lin,!col));
"endwh" => (col:=yypos-(!eolpos); Tokens.ENDWH(!lin,!col));
"if" => (col:=yypos-(!eolpos); Tokens.IF(!lin,!col));
"then" => (col:=yypos-(!eolpos); Tokens.THEN(!lin,!col));
"else" => (col:=yypos-(!eolpos); Tokens.ELSE(!lin,!col));
"endif" => (col:=yypos-(!eolpos); Tokens.ENDIF(!lin,!col));

{digit} => (col:=yypos-(!eolpos); Tokens.INT_CONST(yytext,!lin,!col));
{id} => (col:=yypos-(!eolpos); Tokens.ID(yytext,!lin,!col));

{whitespace}+ => (continue());
{eol} => (lin:=(!lin)+1; eolpos:=yypos+size yytext; continue());

. => (col:=yypos-(!eolpos); badCh (fileName,yytext,!lin,!col); continue());
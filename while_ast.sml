structure Wh:
sig val tree : string -> W_datatypes.Program 
    (* val temp : W_datatypes.Program -> string *)
    (* val comt : W_datatypes.Commands list -> string
    val co1t : W_datatypes.Commands -> string *)
end = 
struct

    structure WhlLrVals = WhlLrValsFun(structure Token = LrParser.Token);
    structure WhlLex    = WhlLexFun(structure Tokens = WhlLrVals.Tokens);
    structure WhlParser = JoinWithArg(
                                structure ParserData = WhlLrVals.ParserData
                                structure Lex = WhlLex
                                structure LrParser = LrParser);

    exception WhlError; 
    fun tree fileName = 
        let
            val inStream = TextIO.openIn fileName
	        fun readNext n = if TextIO.endOfStream inStream then ""
	                         else TextIO.inputN (inStream, n)
            val lexer = WhlParser.makeLexer readNext fileName
            val printError : string * int * int -> unit = fn
                (msg,line,col) =>
                print (fileName^"["^Int.toString line^":"
                ^Int.toString col^"] "^msg^"\n");
	        val (ans,rem) = WhlParser.parse (15, lexer, printError, fileName)
            handle WhlParser.ParseError => raise WhlError;
            val _ = TextIO.closeIn inStream;

            (* val aaa = testf ans; *)
	    in
            ans
	end

    (* fun temp(a) = VMC.test(a) *)

    (* fun co1t (W_datatypes.Assign(_,_) : W_datatypes.Commands) = "a"
        |   co1t (W_datatypes.WH(_,_) : W_datatypes.Commands) = "b"
        |   co1t (W_datatypes.IFT(_,_,_) : W_datatypes.Commands) = "c"
        |   co1t (W_datatypes.Read(_) : W_datatypes.Commands) = "d"
        |   co1t (W_datatypes.Write(_) : W_datatypes.Commands) = "e"

    fun comt([] : W_datatypes.Commands list) = "9"
      | comt(a::s : W_datatypes.Commands list) = 
        let
            val aa = comt(s);
            val ab = co1t(a);
            val ad = 4;
        in "12"
        end;
    (* fun comt (W_datatypes.Assign(a, _) ::s1) = a *)
        (* | comt (W_datatypes.WH(_, _) ::s2) = "b"
        | comt (W_datatypes.IFT(_, _, _) ::s3) = "c" *)
        (* | comt (IFT(_, _) ::s3) = "c" *)
        (* | test([Read(b)::c]) = b; *)

    fun test (W_datatypes.PROG(x,W_datatypes.BLK(y, W_datatypes.CMD(a)))) = comt(a) *)
        (* let
            comt(a)
        in ""
        end *)

    (* datatype Combine = 
          PROG of string * W_datatypes.Blocks
        | BLK of W_datatypes.Declarations list * W_datatypes.Commands
        | DEC of string * W_datatypes.ComSep list * W_datatypes.Types
        | DataType of W_datatypes.Types
        | SEQ of string
        | COM1 of W_datatypes.Commands
        | CMD of W_datatypes.Commands list
  		| IFT of W_datatypes.Expressions * W_datatypes.Commands * W_datatypes.Commands
  		| WH of W_datatypes.Expressions * W_datatypes.Commands
  		| Assign of string * W_datatypes.Expressions
        | Write of W_datatypes.Expressions
        | Read of string
        | EXP1 of W_datatypes.Expressions
        | Id       of string
        | Const    of string
        | Bool_Const  of W_datatypes.Bool_terms
        | BEXP     of W_datatypes.Expressions
        | IEXP     of W_datatypes.Expressions
        | Add     of W_datatypes.Expressions * W_datatypes.Expressions
        | Subtract     of W_datatypes.Expressions * W_datatypes.Expressions
        | Multiply     of W_datatypes.Expressions * W_datatypes.Expressions
        | Divide     of W_datatypes.Expressions * W_datatypes.Expressions
        | Modulus     of W_datatypes.Expressions * W_datatypes.Expressions
        | And     of W_datatypes.Expressions * W_datatypes.Expressions
        | Or     of W_datatypes.Expressions * W_datatypes.Expressions
        | Equlaity     of W_datatypes.Expressions * W_datatypes.Expressions
        | NotEqual     of W_datatypes.Expressions * W_datatypes.Expressions
        | LessThen     of W_datatypes.Expressions * W_datatypes.Expressions
        | GreaterThen     of W_datatypes.Expressions * W_datatypes.Expressions
        | GreaterOrEqual     of W_datatypes.Expressions * W_datatypes.Expressions
        | LessOrEqual     of W_datatypes.Expressions * W_datatypes.Expressions
        | Negative        of W_datatypes.Expressions
        | Not        of W_datatypes.Expressions
        | PLUS 	| MINUS | TIMES	| DIV | MOD
        | EQ	| NEQ	| LT	| GT	| GEQ	| LEQ
        | AND	| OR
        | NOT | NEG
        | INT | BOOL
        | TT | FF *)

end
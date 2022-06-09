structure W_datatypes =
struct

  datatype Integer_operators = 
      PLUS 	| MINUS | TIMES	| DIV | MOD;
  
  datatype Relational_operators = 
      EQ	| NEQ	| LT	| GT	| GEQ	| LEQ;
 
  datatype Logical_operators =  
      AND	| OR;

  datatype Unary_operators = 
      NOT | NEG;

  datatype Types = 
      INT | BOOL

  datatype Bool_terms = 
      TT | FF

  datatype Expressions  =
  		  Id       of string
        | Const    of string
        | Bool_Const  of Bool_terms
        | BEXP     of Expressions
        | IEXP     of Expressions
        | Add     of Expressions * Expressions
        | Subtract     of Expressions * Expressions
        | Multiply     of Expressions * Expressions
        | Divide     of Expressions * Expressions
        | Modulus     of Expressions * Expressions
        | And     of Expressions * Expressions
        | Or     of Expressions * Expressions
        | Equlaity     of Expressions * Expressions
        | NotEqual     of Expressions * Expressions
        | LessThen     of Expressions * Expressions
        | GreaterThen     of Expressions * Expressions
        | GreaterOrEqual     of Expressions * Expressions
        | LessOrEqual     of Expressions * Expressions
        | Negative        of Expressions
        | Not        of Expressions;

  datatype Commands = 
          CMD of Commands list
  		| IFT of Expressions * Commands * Commands
  		| WH of Expressions * Commands
  		| Assign of string * Expressions
        | Write of Expressions
        | Read of string;

  datatype ComSep = SEQ of string

  datatype Dtype = DataType of Types

  datatype Declarations = DEC of string * ComSep list * Types

  datatype Blocks = BLK of Declarations list * Commands
  
  datatype Program = PROG of string * Blocks

    val symbolTable = Array.array(200, ("", "", 0));
    val che = Array.array(200, "");
    val loc = Array.array(1,0);
    val lo1 = Array.array(2,0);

  fun bool_up(a : string, []) = 
    if Array.exists( fn (x) => x = a) che = true
        then raise Fail (a ^ " is already defined")  
    else
    let
        val x = Array.sub(lo1, 1)
        val dr = Array.update(lo1, 1, x+1)
        val dc = Array.update(che, 2*x+1, a)
        val x = Array.sub(loc, 0)
        val dr = Array.update(loc, 0, x+1)
        val de = Array.update(symbolTable, x, (a, "BOOL", x))
    in  ""
    end
   |  bool_up(a:string, b::s : string list) = 
        if Array.exists( fn (x) => x = b) che = true
            then raise Fail (b ^ " is already defined")
        else
            let 
                val x = Array.sub(lo1, 1)
                val de = Array.update(lo1, 1, x+1)
                val de = Array.update(che, 2*x+1, b)
                val x = Array.sub(loc, 0)
                val dr = Array.update(loc, 0, x+1)
                val de = Array.update(symbolTable, x, (b, "BOOL", x))
                val x1 = bool_up(a, s)
            in ""
            end

  fun int_up(a : string, []) = 
    if Array.exists( fn (x) => x = a) che = true
        then raise Fail (a ^ " is already defined")  
    else
    let
        val x = Array.sub(lo1, 0)
        val dr = Array.update(lo1, 0, x+1)
        val dc = Array.update(che, 2*x, a)
        val x = Array.sub(loc, 0)
        val dr = Array.update(loc, 0, x+1)
        val de = Array.update(symbolTable, x, (a, "INT", x))
    in  ""
    end
   |  int_up(a:string, b::s : string list) = 
        if Array.exists( fn (x) => x = b) che = true
            then raise Fail (b ^ " is already defined") 
        else
            let 
                val x = Array.sub(lo1, 0)
                val de = Array.update(lo1, 0, x+1)
                val dc = Array.update(che, 2*x, b)
                val x = Array.sub(loc, 0)
                val dr = Array.update(loc, 0, x+1)
                val de = Array.update(symbolTable, x, (b, "INT", x))
                val x1 = int_up(a, s)
            in ""
            end 

    fun iconv1([]) = []
        | iconv1((SEQ a)::s : ComSep list) : string list = (a::iconv1(s))
    fun bconv1([]) = []
        | bconv1((SEQ a)::s : ComSep list) : string list = (a::bconv1(s))

    fun iconv(id: string, s : ComSep list) : string list = (id::iconv1(s))
    fun bconv(id: string, s : ComSep list) : string list = (id::bconv1(s))

(* function to construct symbol table *)
  fun abc (id: string, lit : ComSep list, ty : Types) = 
    if ty = INT then
        let 
            val teml::t2 = iconv(id, lit)
        in
         (int_up(teml, t2); DEC(id,lit,ty))
        end
    else 
        let 
            val teml::t2 = bconv(id, lit)
        in
         (bool_up(teml, t2);
          DEC(id,lit,ty))
        end

  fun d_int () = INT
  fun d_bool () = BOOL

  fun tr_ue () =  Bool_Const(TT)
  fun fl_ase () =  Bool_Const(FF)

end
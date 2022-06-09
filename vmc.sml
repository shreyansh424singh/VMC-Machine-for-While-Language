structure VMC :
sig val po : W_datatypes.Program -> string list
    val comt : W_datatypes.Commands list -> string list
    val co1t : W_datatypes.Commands -> string list
    val postfix : string -> string FunStack.Stack
    val symbolT : string array
    val Memory : int array
    val execute : string -> string
end = 
struct
    open FunStack

    val symbolT = W_datatypes.che;
    val len = Array.length(symbolT)

(* memory of VMC Machine 
  initially all variables have 99999 stored in them*)
    val Memory = Array.array(200, 99999);

    fun prin w = print(w ^ " \n");
    fun pp w = print(w ^ " ");

    val runtim = Array.array(1, 0);

    fun     expt (W_datatypes.Not(e) : W_datatypes.Expressions) =  expt(e)@["NOT"]
        |   expt(W_datatypes.Negative(e) : W_datatypes.Expressions) =  expt(e)@["NEG"]
        |   expt(W_datatypes.LessOrEqual(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["LEQ"]
        |   expt(W_datatypes.GreaterOrEqual(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["GEQ"]
        |   expt(W_datatypes.GreaterThen(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["GT"]
        |   expt(W_datatypes.LessThen(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["LT"]
        |   expt(W_datatypes.NotEqual(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["NEQ"]
        |   expt(W_datatypes.Equlaity(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["EQ"]
        |   expt(W_datatypes.Or(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["OR"]
        |   expt(W_datatypes.And(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["AND"]
        |   expt(W_datatypes.Modulus(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["MOD"]
        |   expt(W_datatypes.Multiply(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["MUL"]
        |   expt(W_datatypes.Divide(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["DIV"]
        |   expt(W_datatypes.Subtract(e1, e2) : W_datatypes.Expressions) =  expt(e1)@expt(e2)@["SUB"]
        |   expt(W_datatypes.Add(e1, e2) : W_datatypes.Expressions) =   expt(e1)@expt(e2)@["ADD"]
        |   expt(W_datatypes.IEXP(e) : W_datatypes.Expressions) =   expt(e)
        |   expt(W_datatypes.BEXP(e) : W_datatypes.Expressions) =   expt(e)
        |   expt(W_datatypes.Bool_Const(W_datatypes.TT) : W_datatypes.Expressions) =  ["TT"] 
        |   expt(W_datatypes.Bool_Const(W_datatypes.FF) : W_datatypes.Expressions) =  ["FF"]
        |   expt(W_datatypes.Const(x: string) : W_datatypes.Expressions) =  [x]
        |   expt(W_datatypes.Id(x: string) : W_datatypes.Expressions) = [x]

    and

        co1t (W_datatypes.Assign(x,e) : W_datatypes.Commands) = [x]@expt(e)@["SET"]
        |   co1t (W_datatypes.WH(e,W_datatypes.CMD(c)) : W_datatypes.Commands) =  ["1W"]@expt(e)@["2W"]@comt(c)@["WH"]
        |   co1t (W_datatypes.IFT(e,W_datatypes.CMD(c),W_datatypes.CMD(d)) : W_datatypes.Commands) = expt(e)@["1I"]@comt(c)@["2I"]@comt(d)@["IFT"]
        |   co1t (W_datatypes.Read(x) : W_datatypes.Commands) = [x]@["READ"]
        |   co1t (W_datatypes.Write(e) : W_datatypes.Commands) = expt(e)@["WRITE"]

    and
        comt([] : W_datatypes.Commands list) = []
      | comt(a::s : W_datatypes.Commands list) = co1t(a)@comt(s)

    fun po (W_datatypes.PROG(x,W_datatypes.BLK(y, W_datatypes.CMD(a)))) = comt(a)

(* postfix function takes input a filename and return the FunStack
  it calls an function po to convert AST to postfix*)
    fun postfix filename = 
        let
            val t1 = Wh.tree filename
            val t2 = po(t1)
        in
            list2stack t2
        end;

(* function that takes input a filename and executes it using AST and postfix function *)
    fun execute filename = 
      let
        val t1 = Wh.tree filename
        val t2 = po(t1)
        val C_stack = list2stack t2
        val V_stack = create("")
        val V_stack = pop(V_stack)

        val wd = abc1(V_stack, C_stack)
      in  
        wd
      end
    
and

    (* takes an element and returns the location of that element in the symbol table
      initially i=0*)
    findi(b, i) = 
      if i >= len then raise Fail (b ^ " variable not defined ")
      else if Array.sub(symbolT, i) = b then i
      else findi(b, i+1)

and

    (* takes an element and returns the location of that element in the symbol table
      initially i=0*)
    findi1(b, i) = 
      if i >= len then ~1
      else if Array.sub(symbolT, i) = b then i
      else findi(b, i+1)

and

    (* takes an stack and an element and returns the location of the element 
      in =0 initially*)
    stafindi(sta, t, inde) = 
      if nth(sta, inde) = t then inde
      else stafindi(sta, t, inde+1)

and

(* searchs the variable b in symbol table and update its value to a*)
     asign(b, a) = 
      let
        val ind = findi(b, 0)
      in 
        Array.update(Memory, ind, a)
      end

and

(* takes value stack as input and pushes a into it. 
  if a is a value then push directly or if a is TT or FF then push "1" or "0"
  and if a is a variable then push the value of variable a *)
    pushinst(sta, a) = 
      if (String.sub(a,0) = #"+" orelse  String.sub(a,0) = #"~") then push(a, sta)
      else if (String.sub(a,0) = #"0" orelse  String.sub(a,0) = #"1" orelse  String.sub(a,0) = #"2" orelse  String.sub(a,0) = #"3" orelse  String.sub(a,0) = #"4" orelse  String.sub(a,0) = #"5" orelse  String.sub(a,0) = #"6" orelse  String.sub(a,0) = #"7" orelse  String.sub(a,0) = #"8" orelse  String.sub(a,0) = #"9")
        then push(a, sta)
      else if (a = "FF") then push("0", sta)
      else if (a = "TT") then push("1", sta)
      else let 
            val ind = findi(a, 0)  
            val re = Array.sub(Memory, ind)
            val se = Int.toString re;
            in push(se, sta)
            end
    
and

    (* pushes first num elements of sta2 into sta1 
      initially num = n and locat = 0*)
    pushmul(sta1, sta2, num, locat) = 
      if num = 0 then sta1
      else 
        let
          val te = nth(sta2, locat)
          val s3 = push(te, sta1)
        in 
          pushmul(s3, sta2, num-1, locat+1)
        end

and

(* takes value stack as input and return the value at the top of stack to be used for evaluating expressions *)
    rint(V_stack) = 
      let
        val t =top(V_stack)
        val a1 = if (String.sub(t,0) = #"+" orelse  String.sub(t,0) = #"~") then 
                    let val SOME a2 = Int.fromString(t) 
                    in a2 
                    end
                  else if (String.sub(t,0) = #"0" orelse  String.sub(t,0) = #"1" orelse  String.sub(t,0) = #"2" orelse  String.sub(t,0) = #"3" orelse  String.sub(t,0) = #"4" orelse  String.sub(t,0) = #"5" orelse  String.sub(t,0) = #"6" orelse  String.sub(t,0) = #"7" orelse  String.sub(t,0) = #"8" orelse  String.sub(t,0) = #"9")
                    then let val SOME a2 = Int.fromString(t) 
                    in a2 
                    end
                  else if (t = "TT") then 1
                  else if (t = "FF") then 0
                  else 
                    let 
                      val ind = findi(t, 0) 
                    in Array.sub(Memory, ind) 
                    end
        in 
          if a1 = 99999 then raise Fail ("variable is not initialised")
          else a1
      end

and

(* function to print the stack. useful for debugging *)
   prinstack(sta) = 
    if depth(sta)>0 then
      let val cec = pp(top(sta))
          val sta1 = pop(sta)
      in prinstack(sta1)
      end
    else 
      let val crfv = pp("\n")
      in "okay"
      end

and

(* main VMC machine function that changes stacks from source to target and execute commands 
  it takes a value stack and a control stack *)

    abc1(_, []) = "done"
    | abc1(V_stack, a::C_stack) =

let
(* val de = prinstack(V_stack)
val de = prinstack(a::C_stack) *)

in

      if (String.sub(a,0) = #"+" orelse  String.sub(a,0) = #"~" orelse a = "TT" orelse a = "FF") then
        let
          val V1 = pushinst(V_stack, a)
        in abc1(V1, C_stack)
        end
      else if (a="") then abc1(V_stack, C_stack)
      else if (a = "SET") then
        let
          val t =top(V_stack)
          val SOME t1 = Int.fromString t
          val v1 = pop(V_stack)
          val b = top(v1)
          val v2 = pop(v1)
          val dw = asign(b, t1)
        in abc1(v2, C_stack)
        end
      else if (a = "ADD") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val d1 = Int.toString (a1+b1)
          val v3 = push(d1, v2)
        in abc1(v3, C_stack)
        end
      else if (a = "SUB") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val d1 = Int.toString (b1-a1)
          val v3 = push(d1, v2)
        in abc1(v3, C_stack)
        end
      else if (a = "MUL") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val d1 = Int.toString (a1*b1)
          val v3 = push(d1, v2)
        in abc1(v3, C_stack)
        end
      else if (a = "DIV") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val c1 = b1 div a1
          val d1 = Int.toString (c1)
          val v3 = push(d1, v2)
        in abc1(v3, C_stack)
        end
      else if (a = "MOD") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val c1 = b1 mod a1
          val d1 = Int.toString (c1)
          val v3 = push(d1, v2)
        in abc1(v3, C_stack)
        end
      else if (a = "NEG") then
        let
          (* val t =top(V_stack)
          val v1 = pop(V_stack)
          val SOME a1 = Int.fromString(t) *)
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val c1 = ~1 * a1
          val d1 = Int.toString (c1)
          val v3 = push(d1, v1)
        in abc1(v3, C_stack)
        end
      else if (a = "NOT") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val d1 = Int.toString (a1)
          val v3 = push(d1, v1)
        in abc1(v3, C_stack)
        end
      else if (a = "AND") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val ans = if (a1 = 1 andalso b1 = 1 ) then "1" 
                    else "0"
          val v3 = push(ans, v1)
        in abc1(v3, C_stack)
        end
      else if (a = "OR") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val ans = if (a1 = 1 orelse b1 = 1 ) then "1" 
                    else "0"
          val v3 = push(ans, v1)
        in abc1(v3, C_stack)
        end
      else if (a = "LEQ") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val ans = if (a1 >= b1 ) then "1" 
                    else "0"
          val v3 = push(ans, v1)
        in abc1(v3, C_stack)
        end
      else if (a = "GEQ") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val ans = if (a1 <= b1 ) then "1" 
                    else "0"
          val v3 = push(ans, v1)
        in abc1(v3, C_stack)
        end
      else if (a = "LT") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val ans = if (a1 > b1 ) then "1" 
                    else "0"
          val v3 = push(ans, v1)
        in abc1(v3, C_stack)
        end
      else if (a = "GT") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val ans = if (a1 < b1 ) then "1" 
                    else "0"
          val v3 = push(ans, v1)
        in abc1(v3, C_stack)
        end
      else if (a = "NEQ") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val ans = if (a1 <> b1 ) then "1" 
                    else "0"
          val v3 = push(ans, v1)
        in abc1(v3, C_stack)
        end
      else if (a = "EQ") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val b1 = rint(v1)
          val v2 = pop(v1)
          val ans = if (a1 = b1 ) then "1" 
                    else "0"
          val v3 = push(ans, v1)
        in abc1(v3, C_stack)
        end
      else if (a = "WRITE") then
        let
          val a1 = rint(V_stack)
          val v1 = pop(V_stack)
          val v3 = prin(Int.toString(a1))
        in abc1(v1, C_stack)
        end  
      else if (a = "1I") then
        let 
          val loc1 = stafindi(C_stack, "2I", 0)
          val loc2 = stafindi(C_stack, "IFT", 0)
          val cond = top(V_stack)
          val V_stack1 = pop(V_stack)
          val ts = create("")
          val tempsta = pop(ts)
          val tempsta1 = 
            if cond = "1" then 
              let 
                  val tts1 = pushmul(tempsta, C_stack, loc1, 0)
                  val tts2 = drop(C_stack, loc2+1)
                  val tts3 = pushmul(tts2, tts1, loc1, 0)
              in tts3
              end
            else 
              let 
                  val tts1 = drop(C_stack, loc1+1)
                  val tts2 = pushmul(tempsta, tts1, (loc2-loc1-1), 0)
                  val tts3 = drop(tts1, (loc2-loc1))
                  val tts4 = pushmul(tts3, tts2, (loc2-loc1-1), 0)
              in tts4
              end
        in abc1(V_stack1, tempsta1)
        end
      else if (a = "1W") then
        let 
          val sw = Array.sub(runtim, 0)
          val de = Array.update(runtim, 0, sw+1)
          val loc1 = stafindi(C_stack, "2W", 0)
          val loc2 = stafindi(C_stack, "WH", 0)
          val ts = create("")
          val tempsta1 = pop(ts)
          val tempsta2 = pop(ts)
          val tts1 = pushmul(tempsta1, C_stack, loc1, 0)
          val C_stack1 = drop(C_stack, loc1)
          val tts2 = pushmul(tempsta2, C_stack1, (loc2-loc1), 0)
          val C_stack2 = pushmul(C_stack1, tts1, loc1, 0)
          val C_stack3 = push("1W", C_stack2)
          val C_stack4 = pushmul(C_stack3, tts2, (loc2-loc1), 0)
          val C_stack5 = pushmul(C_stack4, tts1, loc1, 0)
        in if sw < 1000 then abc1(V_stack, C_stack5)
          else raise Fail ("runtime error")
        end
      else if (a = "2W") then
        let
          val cond = top(V_stack)
          val V_stack1 = pop(V_stack)
          val loc = stafindi(C_stack, "WH", 0)
          val C_stack1 = if cond = "1" then C_stack
                        else drop(C_stack, (loc+1))
        in abc1(V_stack1, C_stack1)
        end
      else if (depth(C_stack)>1 andalso (nth(C_stack, 1)="LEQ" orelse nth(C_stack, 1)="GEQ" orelse nth(C_stack, 1)="GT" orelse nth(C_stack, 1)="LT" orelse nth(C_stack, 1)="NEQ" orelse nth(C_stack, 1)="EQ" orelse nth(C_stack, 1)="OR" orelse nth(C_stack, 1)="AND" orelse nth(C_stack, 1)="MUL" orelse nth(C_stack, 1)="DIV" orelse nth(C_stack, 1)="MOD" orelse nth(C_stack, 1)="ADD" orelse nth(C_stack, 1)="SUB"))
      then let 
            val v1 = pushinst(V_stack, a)
            val qa = top(C_stack)
            val c1 = pop(C_stack)
            val v2 = pushinst(v1, qa)
          in abc1(v2, c1)
          end
      else if (depth(C_stack)>1 andalso (nth(C_stack, 1)="SET" andalso not(nth(C_stack, 0)="LEQ" orelse nth(C_stack, 0)="GEQ" orelse nth(C_stack, 0)="GT" orelse nth(C_stack, 0)="LT" orelse nth(C_stack, 0)="NEQ" orelse nth(C_stack, 0)="EQ" orelse nth(C_stack, 0)="OR" orelse nth(C_stack, 0)="AND" orelse nth(C_stack, 0)="MUL" orelse nth(C_stack, 0)="DIV" orelse nth(C_stack, 0)="MOD" orelse nth(C_stack, 0)="ADD" orelse nth(C_stack, 0)="SUB")))
      then let 
            val v1 = push(a, V_stack)
            val qa = top(C_stack)
            val c1 = pop(C_stack)
            val v2 = pushinst(v1, qa)
          in abc1(v2, c1)
          end
      else if (depth(C_stack)>1 andalso (nth(C_stack, 0)="NEG" orelse nth(C_stack, 0)="NOT"))
      then let 
            val v1 = pushinst(V_stack, a)
          in abc1(v1, C_stack)
          end
      else if (depth(C_stack)>1 andalso (nth(C_stack, 0) = "READ")) then
        let
          val fegf = prin("input: ")
          val t = valOf(TextIO.inputLine TextIO.stdIn)
          val t1 = Int.fromString t
          val x1 = valOf(t1)
          val ed = asign(a, x1)
          val C_stack1 = pop(C_stack)
        in abc1(V_stack, C_stack1)
        end
      else 
        let val V_stack1 = push(a, V_stack)
        in abc1(V_stack1, C_stack)
        end
end

end
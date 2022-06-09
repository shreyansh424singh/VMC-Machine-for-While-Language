signature STACK =
sig

    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create: 'a -> 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop : 'a Stack -> ('a * 'a Stack) option
    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int
    val app : ('a -> unit) -> 'a Stack -> unit
    val map : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find : ('a -> bool) -> 'a Stack -> 'a option
    val filter : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists : ('a -> bool) -> 'a Stack -> bool
    val all : ('a -> bool) -> 'a Stack -> bool
    val list2stack : 'a list -> 'a Stack (*Convert a list into a Stack*)
    val stack2list: 'a Stack -> 'a list (* Convert a Stack into a list *)
    val toString: ('a -> string) -> 'a Stack -> string

end;

structure FunStack: STACK = 
struct

    type 'a Stack = 'a list

    exception EmptyStack
    exception Error of string

    (*dont know what to do*)
    fun create(a) = [a] 

    fun push (a, s) = [a]@s

    fun pop ([]) = raise EmptyStack
        | pop (a::s) = s
    
    fun top (a::s) = a
        | top ([]) = raise EmptyStack

    fun empty ([]) = true
        | empty(_::_) = false

    fun poptop (a::s) = SOME(a, s)
        | poptop([]) = raise EmptyStack
    (* fun poptop (s) = List.getItem (s) *)
    
    fun nth (a::s, n) = 
        if n=0 then a
        else if n<0 then raise Error ("Negative Index")
        else nth(s, n-1)
        | nth ([], _) = raise Error ("Index Out Of Bound")
    (* fun nth (s, n) = List.nth(s,n) *)

    (* fun drop (a::s, n) =
        if n=0 then s
        else if n<0 then raise Error ("Negative Index")
        else drop(s, n-1)
        | drop ([], _) = raise Error ("Index Out Of Bound") *)
    fun drop (s, n) = List.drop(s, n)

    fun depth ([]) = 0
        | depth(a::s) = 1+depth(s)

    fun app function (s) = List.app function (s)

    fun map function (s) = List.map function (s)

    fun mapPartial function (s) = List.mapPartial function (s)

    fun find function (s) = List.find function (s)

    fun filter function (s) = List.filter function (s)

    fun foldr function x s = List.foldr function x s

    fun foldl function x s = List.foldl function x s

    fun exists function (s) = List.exists function (s)

    fun all function (s) = List.all function (s)

    fun list2stack s = s

    fun stack2list s = s

 (* fun con (a) = Int.toString a; *)

    fun toString a2s (x::s) = (a2s x) ^ " " ^ toString a2s (s)
      | toString a2s ([]) = ""

end

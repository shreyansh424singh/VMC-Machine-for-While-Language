# Assignment 4

Converts WHILE language code to abstract syntax tree using Ml-Lex and Ml-Yacc.


## How to run:

1. $ `rlwrap sml`
2. -`CM.make("s.cm");`
3. -`Control.Print.printDepth:=30;`
4. -`Control.Print.printLength:=50;`

To execute the program run  
    -`VMC.execute "a";`

(Replace a with filename above commands)


## Files Description

1. while_lex.lex    : Converts input file into tokens.
2. parser.grm       : Holds the grammar and converts it to an abstract syntax tree.
3. data_types.sml   : The ML datatype declarations for the elements in the abstract syntax tree.
4. while_ast.sml    : A glue code and a driver that reads the sources file and displays the output.
5. vmc.sml          : VMc machine
6. stack.sml        : FunStack structure
7. s.cm             : Provides a list of files that the Compiler Manager will use to build the project.


## Auxiliary functions in VMC

* findi(b, i) : takes an element and returns the location of that element in the symbol table, initially i=0
* stafindi(sta, t, inde) : takes an stack and an element and returns the location of the element in =0 initially
* asign(b, a) : searchs the variable b in symbol table and update its value to a
* pushinst(sta, a) : takes value stack as input and pushes a into it. If a is a value then push directly or if a is TT or FF then push "1" or "0" and if a is a variable then push the value of variable a 
* pushmul(sta1, sta2, num, locat) : pushes first num elements of sta2 into sta1 initially num = n and locat = 0
* rint(V_stack) : takes value stack as input and return the value at the top of stack to be used for evaluating expressions 
* prinstack(sta) : function to print the stack. useful for debugging 


## Acknowledgements

* The Array structure of The Standard ML Basis Library
* The List structure of The Standard ML Basis Library
* I referred Stack Overflow for some general doubts and errors in ML-Yacc and Ml-Lex.
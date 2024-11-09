type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
         | OpExp of exp * binop * exp
         | EseqExp of stm * exp

val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

fun lookup ((id1:string), ((a:string), (b:int))::l:((string * int) list)) = if a = id1 then b else lookup (id1, l)
 | lookup (id1, []) = 0

fun update ((id1:string), (val1:int), []) = [(id1, val1)]
 | update ((id1:string), (val1:int), (a, b)::l:((string * int) list)) = 
    if 
     id1 = a 
    then 
     (id1, val1)::l 
    else 
     (a,b)::update(id1, val1, l)

fun evalStm (CompoundStm(stm1, stm2), table:((string * int) list)) = evalStm (stm2, evalStm (stm1, table))
 | evalStm (AssignStm(id1, exp1), table) = update(id1, #1 (evalExp (exp1, table)), table)
 | evalStm (PrintStm([]), table) = let val _ = print "\n" in table end
 | evalStm (PrintStm(a::l), table) = let val _ = (print (Int.toString (#1 (evalExp (a, table))) ^ " ")) in evalStm (PrintStm(l), table) end
and evalExp (IdExp(a), table) = (lookup (a, table), table)
 | evalExp (NumExp(a), table) = (a, table)
 | evalExp (OpExp(exp1, binop1, exp2), table) = 
    let 
     val (a, table) = evalExp(exp1, table)
     val (b, table) = evalExp(exp2, table)
    in (case binop1 of
     Plus => a + b
      | Minus => a - b
      | Times => a * b
      | Div => a div b,
      table)
    end
 | evalExp (EseqExp(stm1, exp1), table) = evalExp (exp1, evalStm (stm1, table))

val _ = evalStm (prog, [])

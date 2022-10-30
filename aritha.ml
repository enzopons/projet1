open Asyntax
open Lexer
open Parser
open Format 
open X86_64

(*FLOOOOOAT*)

let rec calcul exp = match exp with
   | Int x -> movq (imm x) (reg rdi)
   | Plus x -> calcul x
   | Par x -> calcul x
   | Moins x -> calcul x ++ negq (reg rdi)
   | Plusi (x,y) -> calcul x ++
       pushq (reg rdi) ++
       calcul y ++
       popq (rsi) ++
       addq (reg rsi) (reg rdi)
   | Moinsi(x,y) -> calcul x ++
       pushq (reg rdi) ++
       calcul y ++
       popq (rsi) ++
       subq (reg rdi) (reg rsi) ++
      movq (reg rsi) (reg rdi)
   | Multi (x,y) -> calcul x ++
       pushq (reg rdi) ++
       calcul y ++
       popq (rsi) ++
       imulq (reg rsi) (reg rdi) 
   | Divi (x,y) -> calcul x ++  
       pushq (reg rdi) ++
       calcul y ++ 
       movq (reg rdi) (reg rbx) ++
       popq (rax) ++
       xorq (reg rdx) (reg rdx) ++
       idivq (reg rbx) ++
       movq (reg rax) (reg rdi)
   | Mod (x,y) -> calcul x ++
     pushq (reg rdi) ++
     calcul y ++
     movq (reg rdi) (reg rdx) ++
     popq (rdi) ++
     xorq (reg rdx) (reg rdx) ++
     idivq (reg rbx) ++
     movq (reg rdx) (reg rdi) 

let princip expr =
   let code =
     { text = globl "main" ++ label "main" ++ calcul expr ++
     call "print_int" ++ ret ++
     inline "
       print_int:
           movq    %rdi, %rsi
           movq    $S_int, %rdi
           xorq    %rax, %rax
           call printf
           ret
             ";
           data =
               label "S_int" ++ string "%d\n";} in
       let c = open_out "resultat.s" in (*crée fichier*)
       let fmt = formatter_of_out_channel c in 
       X86_64.print_program fmt code; (*ecrit code ds fichier*)
       close_out c (*ferme  fichier*)

(*let _ = princip (Plusi((Moins((Multi (Plus(Int 2), Int 5)))),Int 4));;*)

let _ =
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in 
  let expr = Parser.parse Lexer.token lexbuf in
  let check = Asyntax.bien_typee expr in
  if not check then failwith "Erreur de typage"
  else princip expr



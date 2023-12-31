type t =
  | Mov of int  (* A = imm *)
  | Swap        (* swap(A, B) *)
  | Add         (* A = A + B *)
  | Sub         (* A = A - B *)
  | Load        (* A = mem[A] *)
  | Store       (* mem[A] = B *)
  | Setlt       (* A = (A < B) *)
  | Jmpz of int (* if A == 0 go to imm *)
  | Getc        (* A = getchar() *)
  | Putc        (* putchar(A) *)
  | Exit        (* exit(A) *)
[@@ocamlformat "disable"]

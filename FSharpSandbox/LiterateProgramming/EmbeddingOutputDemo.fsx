(** 
### Evaluation demo 
The following is a simple calculation: *)
let test = 40 + 2
let someString = "aaa"

(** We can print it as follows: *)
(*** define-output:test ***)
printf "Result is: %d" test

(** The result of the previous snippet is: *)
(*** include-output:test ***)

(** And the variable `test` has the following value: *)
(*** include-value: test ***)

(** Hey, I have defined some string somewhere. And here it its value *)
(*** include-value: someString ***)

# Test
The test cases in the code generation folder go over all cases for simple boolean
expressions, prints, adding, ifs, while loops, nested scopes. 
Things that are not implemented (such as nested bool ops maybe) currently stop the entire program
panicking using the todo!() rust macro. As always to run `cargo run PATH/TO/FILE`.
Also a lot of the semantic analysis tests are good for the compiler.

# bool ops
I use a routine which generates a boolean expression to memory so every
place for boolean expression (while, if, assignment, print) all use the same assembly snippets.
This is sometimes ineffienct as with the case with if's there is case where a the boolean
expression will evaluate the expression to the z flag with the correct bit to do the jump (==).
But this slight inoptimization is worth the abstraction power of having a routine which evaluates
all boolean expressions. Also this write to memory will be extremely powerful when combining bool
ops. If I implement nested bool ops, it will likely use a pool of memory addresses which is made
to be as big as the the larged nested bool op requires. This will act a temp memory for the bool ops
and the digits which may be resolved therewithin.

# Optimizations
I did not really do any optimizations. The Op codes were made to be as concise as possible.
A plan to do optimizations would be to write a dummy 6502 cpu which processes the op codes. 
It see if an op code changed the state of the program or output and if not remove it (adjusting jumps and whatnot).
This approach would have to be careful to propagate everything that can not be known at compile time.
In this case, that is nothing because there is no input. Still, in this case I would use this 
approach but then propagate all variables as unknowns. If an unknown was involved in the process then
you would not be able to remove it because you do not know its effect. I think this strategy is very
good (i've never heard of it before but I assume its actually been done). It could iterate over the program
multiple times to ensure that all useless commands are stripped.
I am sort of tempted to try it out as it will also lend to a way for me to programmatically test my op codes.


# edge cases
what if there is a jump bigger than 127 bytes... would have to insert multiple jumps and move code

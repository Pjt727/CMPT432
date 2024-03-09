# Parsing tests
- To run parsing tests do `cargo test parse`
- By default rust will hide the std output so use the toggle `-- --show-output`
- ex: `cargo test parse -- --show-output`
- You can also run specific test functions instead of the whol module by just putting the function name
- ex: `cargo test hello_parse`
- Note that the test does not format programs the same way as the running it through main
- ex: `cargo run .\test_cases\general\hello-compiler`
- this is the nicer output than in the tests
- You can also still run multiple files by passing their directory into cargo run
- ex: `cargo run .\test_cases\general\`

# Interesting tests
- not really a parse test but I added a test case which has interesting results on the main program
- In lex a sequence of tokens is considered a program from the tokens OR warnings it generates up into the eop OR end of file
- Take the following case in "break-main-program":
`{}$/* sorry!`
- Lex used to iterpret this as one program because it only generates the eop warning (non-existant in this case) before the comment warning
- It has now been changed to also interpret this as a second program without an eop which will then be added
- The main program interprets this as two programs because there is a token entry (comment closing warning) after the eop symbol
- therefore when parsing there will be an error on the second program where the inserted $ does not match the expected open block
- Note that if the comment was completed there would be no warning token and thus no second program
- ex: 
`{}$/* you're good! */`

# Parse tests
- I feel like there's not really many edge cases as the parse rules are really well defined
- So the tests just try to cover are many different production combinations as practical
- I did run my tests on the same you provide the cst four in the assignment pdf among other, more general, code
- I noticed that you do not include all the empty statement lists given by your parser
- I would argue that you should still include the prodction but I do understand that the idea is that production's can't be "leaf nodes", however because how I have the types there's no ambiguity in my code
- so in the end I went with modeling your ouput by just changing the output code
- There is still one empty statment list in your code output for example program 2 of hte assignment
`
...
-----------------[{]
-----------------<Statement List>
-----------------[}]
...
`
- So I feel like it is consistant include this one but no the others




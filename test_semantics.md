<!--toc:start-->
- [Semantic tests](#semantic-tests)
- [Interesting tests](#interesting-tests)
<!--toc:end-->

# Semantic tests
- To run semantic tests do `cargo test semantic` (can also do the full name semantically_analyze)
- By default rust will hide the std output so use the toggle `-- --show-output`
- ex: `cargo test semantic -- --show-output` (output will be a lot)
- Note that the test does not format programs the same way as the running it through main
- Semantic tests are validated only by asserting the expected amount of errors because I am lazy
- Can still directly run my test files like you would to yours and 
- ex: `cargo run .\test_cases\general\hello-compiler`
- ex: `cargo run .\test_cases\general\`
- Running it has a nicer output and includes things such as adding missing $'s

# Interesting tests
- There are a lot of interesting tests some of which have been commented to
- stolen tests act to just get a breadth of possibilities
- The focused tests demonstrate features
- used scope propagation has some limitations as only variables are reported and not individual assignments like most lsp's outline in the second scope propagation test
- tested matching type, redeclaration, and uninit and ensured correct type references even in different scopes 

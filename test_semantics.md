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

# Parse tests

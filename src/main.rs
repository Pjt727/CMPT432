use std::env;
// Rust cached library to help with caching the function
use cached::proc_macro::cached;

// Newer mathematical definition of the fib sequence where the first is 0
#[cached]
fn fib_sequence(index: u32) -> u32 {
    if index == 0 { return 0; }
    if index == 1 || index == 2 { return 1; }
    return fib_sequence(index - 1) + fib_sequence(index - 2);
}

#[test]
fn test_fibs() {
    let fib_0: u32 = 0;
    let fib_1: u32 = 1;
    let fib_2: u32 = 1;
    let fib_3: u32 = 2;
    let fib_4: u32 = 3;
    let fib_5: u32 = 5;
    let fib_6: u32 = 8;
    let fib_7: u32 = 13;
    let fib_8: u32 = 21;
    let fib_9: u32 = 34;
    let fib_10: u32 = 55;

    assert_eq!(fib_0, fib_sequence(0), "Testing if fib index {} evaluates to {}", 0, fib_0);
    assert_eq!(fib_1, fib_sequence(1), "Testing if fib index {} evaluates to {}", 1, fib_1);
    assert_eq!(fib_2, fib_sequence(2), "Testing if fib index {} evaluates to {}", 2, fib_2);
    assert_eq!(fib_3, fib_sequence(3), "Testing if fib index {} evaluates to {}", 3, fib_3);
    assert_eq!(fib_4, fib_sequence(4), "Testing if fib index {} evaluates to {}", 4, fib_4);
    assert_eq!(fib_5, fib_sequence(5), "Testing if fib index {} evaluates to {}", 5, fib_5);
    assert_eq!(fib_6, fib_sequence(6), "Testing if fib index {} evaluates to {}", 6, fib_6);
    assert_eq!(fib_7, fib_sequence(7), "Testing if fib index {} evaluates to {}", 7, fib_7);
    assert_eq!(fib_8, fib_sequence(8), "Testing if fib index {} evaluates to {}", 8, fib_8);
    assert_eq!(fib_9, fib_sequence(9), "Testing if fib index {} evaluates to {}", 9, fib_9);
    assert_eq!(fib_10, fib_sequence(10), "Testing if fib index {} evaluates to {}", 10, fib_10);
}

fn main() {
    // the first arg is the target
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 { panic!("\nExpected an index for the Fibonacci sequence!\n") }
    if args.len() > 2 { panic!("\nOnly one index can be computed at a time!\n") }

    let fib_index_input = &args[1];
    let fib_index = fib_index_input.parse::<u32>();
    match fib_index {
        Ok(n) => {
            println!("The Fibonacci sequence at index {} is {}", n, fib_sequence(n))
        }
        Err(e) => panic!("\nThere was an error parsing your whole number: {}\n", e)
    }
}


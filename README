## Panda Language

Panda is a programming language similar to Go: static scoping, strong and static typing, custom data types, and more. The highlight is that it supports automatic type inference broadly to avoid redundant type declarations and treats functions as first-class primitives (e.g. lambda function) to make programming easier and the code more readable.

## Features in the HelloWorld demo

- data types
    
    Panda now has implemented `int`, `float`, `string`, `bool`, and `list` in the below form:
    
    ```
    // int
    i = 1
    // float
    f = 3.1415926
    // string
    s = "hello"
    // bool
    b = true
    // list
    l = [1, 2, 3]
    ```
    
- type casting
    
    Panda now only supports explicitly casting `int` to `float`, `int`/`float`/`bool` to `string`.
    
- variable declarations
The format of variable declarations contains the following three:
    
    ```
    // explicit type declaration, assign init value
    var x: int = 1
    // explicit type declaration, assign default value
    var x: int
    // auto type inference, must assign init value
    var x = 1
    ```
    
- newline as separator
    
    In Panda, people use the newline instead of the semicolon as the separator to make the code even cleaner.
    
- mixed declarations and statements
In the old days of C and its ancestors, variable declarations must appear before any statements. In modern languages, we can mix declarations and statements within a code block, which is also implemented in Panda. For example, the following style is legal.
    
    ```
    {
    	var s1 = "hello"
    	print(s1)
    	var s2 = "world"
    	print(s1 + s2)
    }
    ```
    
- global variables
    
    We allow people to declare variables in the global scope which is visible to all scopes and can be overwritten later.
    
- function declarations
    
    The function declaration is in the following format in Panda:
    
    ```
    func boo(a: int, b: float) : float {
    	return a + b
    }
    ```
    
    We choose to put the type after variables as it’s becoming common sense (though arguably) that this style makes the code much more readable, especially for those functions with complicated arguments list and return types. 
    
    The return type of a function could be ignored which indicates this function only does side-effects and the return type will be inferred to Void as in most languages.
    
- basic control flow
    
    Panda now supports for loop, while loop, if-else statement, break, and continue.
    
- a bunch of operators
    - `+`, `-`, `*`, `/`, `%`
    - `+=`, `-=`, `*=`, `/=`, `%=`
    - `+` allows a string type and an int/float type which essentially first converts the int/float to a string representation and concatenates it to the other string.
    - `>`, `>=`, `<`, `<=` compare scalar types
    - `&&`, `||`, `!` for logical and, or, and not
- type inference
    
    Panda tries to infer type information as much as possible. If it’s enough to infer the type automatically, people do not have to declare the type again. Please see the above examples.
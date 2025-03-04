# Strict JavaScript Validation Rules

## 1. General Syntax
- **Each statement must end with a semicolon (`;`) unless using ASI (Automatic Semicolon Insertion)**
  - `let x = 5` ❌ (invalid unless ASI applies)
  - `let x = 5;` ✅

- **Curly braces `{}` must be properly opened and closed**
  - ```
    if (true) {
      console.log("Valid");
    ```
    ❌ (missing closing `}`)

- **Parentheses `()` must be properly opened and closed**
  - `console.log("Hello";` ❌
  - `console.log("Hello");` ✅

- **Brackets `[]` for arrays must be correctly closed**
  - `let arr = [1, 2, 3;` ❌
  - `let arr = [1, 2, 3];` ✅

- **Strings must be enclosed in matching quotes**
  - `let str = 'Hello";` ❌
  - `let str = "Hello";` ✅
  - `let str = 'Hello';` ✅

- **Comments must be properly formatted**
  - `// This is a valid comment`
  - `/* Multi-line comment */`

---

## 2. Variables & Declarations
- **Variables must be declared before use**
  - `console.log(x); let x = 5;` ❌
  - `let x = 5; console.log(x);` ✅

- **Valid variable declarations**
  - `let`, `const`, and `var` are valid.
  - `var 123x = 5;` ❌ (invalid variable name)
  - `let x-1 = 5;` ❌ (invalid character `-`)
  - `let x1 = 5;` ✅

- **Reserved words cannot be used as variable names**
  - `let return = 5;` ❌
  - `let class = "test";` ❌

---

## 3. Function Definitions
- **Function declarations must be valid**
  - ```
    function sayHello() {
      return "Hello";
    }
    ```
    ✅ (valid function)

- **Arrow functions must have correct syntax**
  - `let add = (a, b) => return a + b;` ❌ (invalid `return` placement)
  - `let add = (a, b) => a + b;` ✅
  - `let add = (a, b) => { return a + b; };` ✅

- **Function calls must have parentheses**
  - `sayHello;` ❌
  - `sayHello();` ✅

---

## 4. Objects & Arrays
- **Objects must have properly formatted key-value pairs**
  - ```
    let obj = {
      name: "Alice",
      age: 25,
    };
    ```
    ✅ (valid)
  - ```
    let obj = {
      name: "Alice"
      age: 25,
    };
    ```
    ❌ (missing comma)

- **Arrays must have properly formatted elements**
  - `let arr = [1, 2, 3];` ✅
  - `let arr = [1 2 3];` ❌ (missing commas)

---

## 5. Operators & Expressions
- **Comparison operators must be valid**
  - `x === 5;` ✅
  - `x == 5;` ✅
  - `x === 5;` ✅
  - `x !== 5;` ✅
  - `x !=== 5;` ❌ (invalid syntax)

- **Arithmetic operators must be used correctly**
  - `let sum = 5 +;` ❌
  - `let sum = 5 + 3;` ✅

- **Logical operators must be used correctly**
  - `if (x && y || z) {}` ✅
  - `if (x && || z) {}` ❌ (invalid operator placement)

---

## 6. Control Structures
- **`if`, `else if`, and `else` must have correct syntax**
  - ```
    if (condition) {
      // Do something
    } else if (otherCondition) {
      // Do something else
    } else {
      // Default case
    }
    ```
    ✅ (valid)

- **`for` loops must be correctly formatted**
  - ```
    for (let i = 0; i < 10; i++) {
      console.log(i);
    }
    ```
    ✅ (valid)
  - ```
    for (let i = 0; i < 10 i++) {
      console.log(i);
    }
    ```
    ❌ (missing semicolon)

- **`while` loops must have valid syntax**
  - ```
    while (condition) {
      // Do something
    }
    ```
    ✅ (valid)
  - ```
    while condition {
      // Do something
    }
    ```
    ❌ (missing parentheses)

---

## 7. Classes & Objects
- **Classes must have a valid `constructor` method**
  - ```
    class Person {
      constructor(name) {
        this.name = name;
      }
    }
    ```
    ✅ (valid)
  - ```
    class Person {
      name = "Alice";
    }
    ```
    ❌ (missing constructor)

- **Methods must have correct syntax**
  - ```
    class Person {
      greet() {
        return "Hello";
      }
    }
    ```
    ✅ (valid)

- **Object instantiation must use `new` for classes**
  - `let p = new Person("Alice");` ✅
  - `let p = Person("Alice");` ❌ (missing `new`)

---

## 8. Async & Promises
- **`async` functions must be correctly defined**
  - ```
    async function fetchData() {
      return await fetch("https://example.com");
    }
    ```
    ✅ (valid)

- **`await` must only be used inside `async` functions**
  - ```
    function getData() {
      let response = await fetch("https://example.com");
    }
    ```
    ❌ (invalid: `await` outside `async` function)

---

## 9. Exceptions & Error Handling
- **`try`, `catch`, and `finally` must be used correctly**
  - ```
    try {
      riskyFunction();
    } catch (error) {
      console.error(error);
    } finally {
      console.log("Cleanup");
    }
    ```
    ✅ (valid)

- **`throw` must be followed by an error object or string**
  - `throw "Error";` ✅
  - `throw new Error("Something went wrong");` ✅
  - `throw;` ❌ (invalid)

---

## 10. Module Imports & Exports
- **`import` statements must be valid**
  - `import x from 'module';` ✅
  - `import { x, y } from 'module';` ✅
  - `import x { from 'module' };` ❌ (invalid syntax)

- **`export` statements must be correctly formatted**
  - `export default function() {};` ✅
  - `export const myVar = 5;` ✅
  - `export x = 5;` ❌ (invalid syntax)

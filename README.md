# Lambda Calculus Interpreter

## Description
This project is a Scala 3 implementation of an untyped Lambda Calculus done by Illia Vasylenko, Mykola Utkin, Danylo Onosov, Ivan Lukianets.

The interpreter evaluates lambda terms using three distinct reduction strategies:
* **Normal Order:** Leftmost-outermost reduction.
* **Call-By-Name:** Lazy evaluation; does not evaluate arguments before substitution and does not reduce under abstractions.
* **Call-By-Value:** Eager evaluation; forces arguments to be fully evaluated to a normal form before substitution and does not reduce under abstractions.

Everything is covered with unit tests

## Concrete Syntax for parser
The parser accepts the following standard lambda calculus syntax. Whitespace is generally ignored or used to delimit tokens.

* **Variables:** Standard string identifiers (e.g., `x`, `y`, `varName`).
* **Abstractions:** Declared using either a backslash `\` or the Unicode lambda `λ`, followed by the parameter identifier, a dot `.`, and the term body.
  * *Examples:* `\x. x` or `λx. x`
  * *Nested:* `\x. \y. x`
* **Applications:** Space-separated terms. Application is strictly **left-associative**.
  * *Example:* `x y z` is parsed as applying `x` to `y`, and then applying that result to `z` (i.e., `((x y) z)`).
* **Grouping:** Parentheses `()` are used for explicit grouping to override default associativity.
  * *Example:* `x (y z)` is parsed as applying `x` to the evaluated result of `y z`.

## Build and Run
This is a standard Scala 3 project. Ensure you have Java and `sbt` (Scala Build Tool) installed on your system.

To compile and run the main program (which demonstrates the evaluation strategies and variable renaming on sample terms), execute the following command from the root directory:

```bash
sbt run
```
## How to Run the Tests
The test suite consists of several property-based specifications (such as AlphaConversionSpecifications, SubstitutionSpecification, ParserSpecification, etc.).

To execute all tests, run:

```bash
sbt test
```

## Chat logs:
https://aistudio.google.com/app/prompts?state=%7B%22ids%22:%5B%221ifWKIN5weKKXd0DXL36s4jK-FhB4B-Z-%22%5D,%22action%22:%22open%22,%22userId%22:%22103189012974006674534%22,%22resourceKeys%22:%7B%7D%7D&usp=sharing 
https://docs.google.com/presentation/d/10DXOoxbypL97NubgS5b0tV7cFL2BC1LkFjapjQc9NTE/edit?slide=id.g34159f8febf_0_51#slide=id.g34159f8febf_0_51
https://claude.ai/share/d356f540-9229-4084-afa5-f995032f5048
https://chatgpt.com/share/69ef2967-83f4-8331-929c-7f9fa5e45368
https://gemini.google.com/share/524ee88fad5c
https://claude.ai/share/e092de49-07b6-4388-a9bc-8dd3bea5f3b5

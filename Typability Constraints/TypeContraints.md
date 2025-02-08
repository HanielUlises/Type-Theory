### Type Inference in Lambda Calculus with Hindley-Milner Typing

#### Type System

In this code, we define a simple type system for lambda calculus, inspired by the Hindley-Milner typing system. It includes two main components: **types** and **terms**.

- **Types** (`Ty`):
    - `TVar Int`: Represents a type variable. These are used to express polymorphic types or fresh variables that can stand for any type.
    - `TArr Ty Ty`: Represents function types. A function type `TArr Ty1 Ty2` denotes a function that takes an argument of type `Ty1` and returns a value of type `Ty2`.

#### Type Inference

The goal of type inference is to determine the type of a term based on its structure and the types of its subterms. This process requires solving type equations between types. We define the following recursive inference rules for different term constructs:

- **Variables (`Var x`)**: 
    - When encountering a variable, the type of the variable is looked up in the environment. If the variable is unbound, an error is raised.

    ```
    infer (env) (Var x) = (lookup x env, [] )
    ```

- **Abstractions (`Abs x t`)**:
    - For a lambda abstraction, a fresh type variable is introduced for the argument, and the type of the body `t` is inferred in the extended environment where `x` is assigned this fresh type.
    - The resulting type of the abstraction is a function type, with the fresh type for the argument and the inferred type of the body.

    ```
    infer (env) (Abs x t) = (TArr ax at, et)
    ```

- **Applications (`App t u`)**:
    - For an application, the types of the function `t` and the argument `u` are inferred. A fresh type variable is introduced for the result, and we generate a type equation stating that the function type of `t` must match the expected type of the argument.
    - The result is the fresh type variable, and the list of type equations is updated with the generated equations.

    ```
    infer (env) (App t u) = (ax, (at, TArr au ax) : et ++ eu)
    ```

#### Fresh Type Variables

The function `fresh` generates fresh type variables using an `IO` monad. This allows the type system to introduce new type variables during the inference process, ensuring that the type environment remains consistent.

#### Type Equations

Type equations are used to express relationships between types. For instance, when applying a function to an argument, we create an equation that the function's type must match the argument's type. These equations are collected in the list `TEq`, which can be used to solve the types after the initial inference.

#### Key Concepts in Type Inference

- **Unification**:
    - When inferring the type of an application, we encounter type equations that must be unified. Unification is the process of finding a substitution that makes two types identical. For example, if we infer that a function has type `TArr Ty1 Ty2`, and the argument has type `Ty1`, we need to unify `Ty2` with the type of the result of the application.

- **Polymorphism**:
    - The use of fresh type variables allows for the inference of polymorphic types. For example, a function that accepts any argument and returns the same type can be inferred with a polymorphic type `TArr Ty Ty`, where `Ty` is a type variable.

#### Example Workflow

Consider the term `λx. (λy. x y)`, where `x` is applied to `y` inside the lambda. The type inference process would proceed as follows:

1. **Type of `x`**: Introduce a fresh type variable `Tx`.
2. **Type of `y`**: Introduce another fresh type variable `Ty`.
3. **Type of `x y`**: The function `x` must be a function from `Ty` to some result type `Tz`. Thus, we generate the type equation `Tx = TArr Ty Tz`.
4. **Final Type**: The resulting type of the entire term is `TArr Ty (TArr Ty Tz)`, indicating a function that takes an argument of type `Ty` and returns a function from `Ty` to `Tz`.
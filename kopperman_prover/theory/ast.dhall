< Var : Text
| App : { func : ./ast.dhall, arg : ./ast.dhall }
| Abs : { var : Text, body : ./ast.dhall }
| Dup : { var : Text, dp1 : Text, dp2 : Text, body : ./ast.dhall }
| Fix : { var : Text, body : ./ast.dhall }
| Cons : { tag : Natural, args : List ./ast.dhall }
| Symbol : Text  -- For Optimizer Hooks (Inp, Sum, etc.)
>

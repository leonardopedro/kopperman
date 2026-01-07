let Term = ./ast.dhall
let var = constructors Term.Var
let app = \(f : Term) -> \(x : Term) -> constructors Term.App { func = f, arg = x }

-- Primitives
let Inp = constructors Term.Symbol "Inp"
let Add = constructors Term.Symbol "Add"

-- Kopperman's D_omega (Infinite Sum Stream) via Cyclic Recursion
let D_omega = \(x : Term) ->
    constructors Term.Fix {
        var = "stream",
        body = constructors Term.Abs {
            var = "n",
            body = constructors Term.Cons {
                tag = 1, -- Stream Tag
                args = [
                    -- (x, e_n) * e_n
                    app (app Inp x) (var "n"), 
                    -- Recursive tail
                    app (var "stream") (var "n") 
                ]
            }
        }
    }

in { D_omega, Inp, Add, Zero = constructors Term.Cons { tag = 0, args = [] : List Term } }

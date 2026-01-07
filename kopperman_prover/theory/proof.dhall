let H = ./hilbert.dhall
-- Prove Linearity: (a + b, c) -> (a, c) + (b, c)
-- The compiler should optimize this symbolically before GPU execution.
in H.app (H.app H.Inp (H.app (H.app H.Add H.Zero) H.Zero)) H.Zero

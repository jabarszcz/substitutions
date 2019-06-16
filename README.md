This is a small implementation of λσ-calculus. The aim is to make it
easy to experiment and use quickcheck to verify properties of the
language.

## An Example

```
> t = (Lambda (Lambda (Ref 1)))
> printEvalRandom (App t t)
(λ#1)[λλ#1 · ε]
λ#1[#0 · (λλ#1 · ε) • ↑1]
λ#1[#0 · (λλ#1)[↑1] · ε • ↑1]
λ#1[#0 · (λλ#1)[↑1] · ↑1]
λ#1[#0 · λ(λ#1)[#0 · ↑1 • ↑1] · ↑1]
λ#1[#0 · λ(λ#1)[#0 · ↑2] · ↑1]
λ#1[#0 · λλ#1[#0 · (#0 · ↑2) • ↑1] · ↑1]
λ#0[λλ#1[#0 · (#0 · ↑2) • ↑1] · ↑1]
λ#0[λλ#0[(#0 · ↑2) • ↑1] · ↑1]
λλλ#0[(#0 · ↑2) • ↑1]
λλλ#0[#0[↑1] · ↑2 • ↑1]
λλλ#0[#1 · ↑2 • ↑1]
λλλ#0[#1 · ↑3]
λλλ#1
```

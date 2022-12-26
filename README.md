# Micro Superoptimizer

A very simple "superoptimizer", without solver.

## Motivating Example

Say, we want to construct an expression that computes the following "truth table" (i.e., the *specification*):

| **x / y** | **01** | **10** | **11** |
|-----------|--------|--------|--------|
| **01**    |     01 |     01 |     01 |
| **10**    |     11 |     01 |     11 |
| **11**    |     11 |     01 |     11 |

We perform *enumerative synthesis* to find the smallest term that computes this table. Behavior on other inputs is irrelevant. Our term may only consist of `not`, `and`, `or`, `xor`, `shl`, `shr`, `add`, `mul`, and constants.

The micro-superoptimizer enumerates terms until it finds: `((y + y) & x) | 1`

## License

BSD-3 -- See `LICENSE`

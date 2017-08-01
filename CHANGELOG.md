# 0.2.1

- Refactored to use the `CoRec` type provided by `vinyl` >= 0.6.0

- Fixed bug in typing mostly-numeric columns
Such columns must be represented as `Text`. Previously, we strove a bit too hard to avoid falling back to `Text` resulting in dropping rows containing non-numeric values for columns we crammed into a numeric type.

- Minor optimization of CSV parsing
In particular, dealing with RFC4180 style quoting

- GHC-8.2.1 compatibility

# 0.1.10

- Added CSV output functions: `produceCSV` and `writeCSV`
- Added an Eq instance for the `Frame` type


# 0.1.9

Fixed column type inference bug that led the inferencer to prefer `Bool` too strongly.

This was fallout from typing columns whose values are all 0 or 1 as `Bool`.

# 0.1.6

Re-export `Frames.CSV.declareColumn` from `Frames`. This makes it much
easier to manually define column types.

# 0.1.4

Use `microlens` instead of `lens-family-core` for demos.

# 0.1.3

GHC-8.0.1 compatibility

# 0.1.2.1

Improved documentation based on suggestions by Alexander Kjeldaas

# 0.1.2

Fixed bug in `Monoid` instance of `Frame` (@dalejordan)

# 0.1.1.0

Added `frameConsA`, `frameSnoc`, and `RecordColumns` to help with
changing row types.

# 0.1.0.0

Initial version pushed to hackage.

# 0.4.0

- Changed types of `mapMethod` and `mapMethodV`

These now rely on explicit `TypeApplications` rather than `Proxy` values.

# 0.3.0

- Pervasive use of `pipes` for CSV data loading

This provides better exception handling (file handles should be closed more reliably), and offers an interface point for customized handling of input texts. An example of this latter point is working with particular file encodings.

A breaking change is that operations that previously returned `IO` values now return `MonadSafe` constrained values.

- Adaptation of `Data.Vinyl.Curry.runcurry` to the Frames `Record` type
This simply strips the column name information from a row before applying the function from `vinyl`.

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

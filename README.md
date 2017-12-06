# Frames
## Data Frames for Haskell

User-friendly, type safe, runtime efficient tooling for working with
tabular data deserialized from comma-separated values (CSV) files. The
type of each row of data is inferred from data, which can then be
streamed from disk, or worked with in memory.

We provide streaming and in-memory interfaces for efficiently working
with datasets that can be safely indexed by column names found in the
data files themselves. This type safety of column access and
manipulation is checked at compile time.

## Use Cases

### Clean data
If you have a CSV data where the values of each column may be classified by a single type, and ideally you have a header row giving each column a name, you may simply want to avoid writing out the Haskell type corresponding to each row. `Frames` provides `TemplateHaskell` machinery to infer a Haskell type for each row of your data set, thus preventing the situation where your code quietly diverges from your data.

### Unwieldy Row Types
If you are primarily interested in a subset of the columns of your data set, you will want to write functions over only those columns you are interested in, while the rest of each row is left polymorphic. `Frames` leverages the `vinyl` library to let you write such functions, and efficiently pass them over large data sets.

### Efficient Traversal of Heterogeneous Tabular Data
`Frames` provides a column-oriented data store that can efficiently pack each homogeneously-typed column while still presenting a heterogeneous row-based API.

## A Short Example
Lets infer a bunch of types from a data file at compile time, then, at runtime, load that data into column-oriented storage in memory. We're going to compute the average ratio of two columns, so we'll use the `foldl` library. Our fold will project the columns we want, and apply a function that divides one by the other after appropriate numeric type conversions. Here is the entirety of that program.

```haskell
{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TemplateHaskell #-}
import qualified Control.Foldl as L
import Data.Vinyl.Curry (runcurry')
import Data.Vinyl (rcast)
import Frames

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes "Row" "data/prestige.csv"

loadRows :: IO (Frame Row)
loadRows = inCoreAoS (readTable "data/prestige.csv")

-- | Compute the ratio of income to prestige for a record containing
-- only those fields.
ratio :: Record '[Income, Prestige] -> Double
ratio = runcurry' (\i p -> fromIntegral i / p)

averageRatio :: IO Double
averageRatio = L.fold (L.premap (ratio . rcast) go) <$> loadRows
  where go = (/) <$> L.sum <*> L.genericLength
```

## Tutorial
For comparison to working with data frames in other languages, see the
[tutorial](http://acowley.github.io/Frames/).

## Demos
There are various
[demos](https://github.com/acowley/Frames/tree/master/demo) in the repository. Be sure to run the `getdata` build target to download the data files used by the demos! You can also download the data files manually and put them in a `data` directory in the directory from which you will be running the executables.

## Benchmarks
The [benchmark](benchmarks/InsuranceBench.hs) shows several ways of
dealing with data when you want to perform multiple traversals.

Another [demo](benchmarks/BenchDemo.hs) shows how to fuse multiple
passes into one so that the full data set is never resident in
memory. A [Pandas version](benchmarks/panda.py) of a similar program
is also provided for comparison.

This is a trivial program, but shows that performance is comparable to
Pandas, and the memory savings of a compiled program are substantial.

![Trivial Benchmark](https://pbs.twimg.com/media/B71az_CCUAAgscq.png:large)

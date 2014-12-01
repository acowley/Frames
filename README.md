# Frames
## Data Frames for Haskell

User-friendly, type safe, runtime efficient tooling for working with tabular data deserialized from comma-separated values (CSV) files. The type of each row of data is inferred from data, which can then be streamed from disk, or worked with in memory.

We provide streaming and in-memory interfaces for efficiently working with datasets that can be safely indexed by column names found in the data files themselves. This type safety of column access and manipulation is checked at compile time.

The [benchmark](benchmarks/InsuranceBench.hs) shows several ways of dealing with data when you want to perform multiple traversals.

Another [demo](benchmarks/BenchDemo.hs) shows how to fuse multiple passes into one so that the full data set is never resident in memory. A [Pandas version](benchmarks/panda.py) of a similar program is also provided for comparison.

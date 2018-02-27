# Demonstration of data frame joins with pandas. Build with
# cabal, then run in bash with something like
#
# $ /usr/bin/time -l python benchmarks/pandas_joins.py 2>&1 | head -n 4
#
# Or
# nix-shell -p 'python.withPackages (p: [p.pandas])' --run '/usr/bin/time -l python benchmarks/pandas_joins.py 2>&1 | head -n 4'

from pandas import DataFrame, read_csv
import pandas as pd

left = pd.read_csv('data/left1.csv')
right = pd.read_csv('data/right1.csv')
left_summary = pd.read_csv('data/left_summary.csv')

print(pd.merge(left, right
                , on = "policyID"
                , how = "inner").shape[0])

print(pd.merge(left, left_summary
                , on = "policyID"
                , how = "inner").shape[0])

print(pd.merge(left, left_summary
               , on = ("policyID", "county")
               , how = "inner").shape[0])

print(pd.merge(left, left_summary
                , on = ("policyID", "county")
                , how = "outer").shape[0])

print(pd.merge(left, left_summary
                , on = ("policyID", "county")
                , how = "left").shape[0])

print(pd.merge(left, left_summary
               , on = ("policyID", "county")
               , how = "right").shape[0])

# Demonstration of streaming data processing. Try building with
# cabal, then running with in bash with something like,
# 
# $ /usr/bin/time -l python benchmarks/panda.py 2>&1 | head -n 4

from pandas import DataFrame, read_csv
import pandas as pd

df = pd.read_csv('data/FL2.csv')
print(df['point_latitude'].mean())
print(df['point_longitude'].mean())

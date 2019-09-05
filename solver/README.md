# Build

in solver/

```cmd
dune build solver.exe
```

# Use

- `-dh` hamming distance
- `-dm` manhattan distance
- `-de` euclidean distance
- `-dc` chebyshev distance
- `-grd` greedy search
- `-w <int>` weight

example:
```cmd
python Puzzle_generator.py -s 4 | ./solver/_build/default/solver.exe -dc -w 5
```

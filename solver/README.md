# Build

in solver/

```cmd
make
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
python Puzzle_generator.py -s 4 | ./solver/solver -dc -w 5
```

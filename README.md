# Dependency

- ocaml
- opam
- dune: `opam install dune`

```cmd
pip3 install -r visu/requirements.txt
```

# Build
in solver/

```cmd
dune build solver.exe
```

# Usage
### Solver
- `-dh` hamming distance
- `-dm` manhattan distance
- `-de` euclidean distance
- `-dc` chebyshev distance
- `-grd` greedy search
- `-w <int>` weight

### Visu

```cmd
python3 visu/main.py --help
```

example:
```cmd
python Puzzle_generator.py -s 4 | ./solver/_build/default/solver.exe -dc -w 5 | python3 visu/main.py -s 4 -i images/image.jpg
```

# 3DConnectFour
3D Connect four game for N-dimensional cube(4\*4\*4 or 6\*6\*6)

## TODO:
- [ ] Optimize static heuristics for 4\*4\*4 cube, add something
- [ ] Add history heuristics and transposition table (hash)
- [ ] Optimize alpha-beta

## Help:
- compiling commands:
  - (compile-file "imefajla.cl")
  - (load "imefajla.fasl")  
- Printing -> stampajStanje (183. line)
- Result -> rezultat (911. line)
- Generator function -> mogucaStanja (899. line)
- Main functions -> igra, glavna, potez (820-892 line)
- Counting functions helpers -> prebroji, basicprebroji... (272-820 line)
- Heuristics -> hprebroji (1003. line)
  - constants (986. line)
  - static heuristics (1102-1196 line)
  - including static h. (327. line)
- alpha-beta (925-981 line)

Various implementation tests and performance benchmarks.

# Benchmarks

The module `Benchmark` implements a benchmark tool. To use it, compile `Benchmark` and run `main`. 

Note for KiCS2 users: Use 
  :set rts -T

# Example output

Using the following system:

- CPU: 10700K@4.7GHz
- RAM: DDR4, Dual Channel, 3733MHz, CL16
- VirtualBox 7.0.10 running Ubuntu 22.04.3 LTS 

The benchmarks output is as follows:


PAKCS:
```
Running benchmarks
==================

Initializing FCY benchmarks...

Running benchmark "FCY with rw-data":
-------------------------------------
Run time:     610ms per run. Total for 3 runs: 1831ms
Elapsed time: 763ms per run. Total for 3 runs: 2290ms
GCs:          37 per run. Total for 3 runs: 112

Running benchmark "FCY with Prelude":
-------------------------------------
Run time:     12425ms
Elapsed time: 14152ms
GCs:          488

Running benchmark "FCY with ReadShowTerm":
------------------------------------------
Run time:     362ms per run. Total for 3 runs: 1087ms
Elapsed time: 495ms per run. Total for 3 runs: 1485ms
GCs:          9 per run. Total for 3 runs: 28

Initializing parameterized FCY benchmarks...

Running benchmark "No string extraction":
-----------------------------------------
Run time:     889ms per run. Total for 3 runs: 2669ms
Elapsed time: 1153ms per run. Total for 3 runs: 3461ms
GCs:          31 per run. Total for 3 runs: 94

Running benchmark "No string inlining":
---------------------------------------
Run time:     594ms per run. Total for 3 runs: 1782ms
Elapsed time: 722ms per run. Total for 3 runs: 2168ms
GCs:          13 per run. Total for 3 runs: 40

Running benchmark "alphabet size 10 (vs 26)":
---------------------------------------------
Run time:     588ms per run. Total for 3 runs: 1764ms
Elapsed time: 725ms per run. Total for 3 runs: 2175ms
GCs:          21 per run. Total for 3 runs: 64

Running benchmark "alphabet size 94 (vs 26)":
---------------------------------------------
Run time:     817ms per run. Total for 3 runs: 2451ms
Elapsed time: 983ms per run. Total for 3 runs: 2950ms
GCs:          12 per run. Total for 3 runs: 37

Preparing container parameters (list, map, trie)...

Running benchmark "FCY with parametrized containers: list":
-----------------------------------------------------------
Run time:     763ms per run. Total for 3 runs: 2291ms
Elapsed time: 920ms per run. Total for 3 runs: 2762ms
GCs:          16 per run. Total for 3 runs: 48

Running benchmark "FCY with parametrized containers: map":
----------------------------------------------------------
Run time:     960ms per run. Total for 3 runs: 2882ms
Elapsed time: 1148ms per run. Total for 3 runs: 3445ms
GCs:          27 per run. Total for 3 runs: 82

Running benchmark "FCY with parametrized containers: trie":
-----------------------------------------------------------
Run time:     607ms per run. Total for 3 runs: 1822ms
Elapsed time: 746ms per run. Total for 3 runs: 2238ms
GCs:          15 per run. Total for 3 runs: 46

Initializing Peano benchmarks...

Running benchmark "Peano with rw-data":
---------------------------------------
Run time:     56ms per run. Total for 3 runs: 168ms
Elapsed time: 60ms per run. Total for 3 runs: 181ms
GCs:          0 per run. Total for 3 runs: 1

Running benchmark "Peano with Prelude":
---------------------------------------
Run time:     5568ms
Elapsed time: 6244ms
GCs:          61

Running benchmark "Peano with ReadShowTerm":
--------------------------------------------
Run time:     185ms per run. Total for 3 runs: 555ms
Elapsed time: 228ms per run. Total for 3 runs: 684ms
GCs:          1 per run. Total for 3 runs: 5

Initializing Binary Tree benchmarks...

Running benchmark "Binary Tree with rw-data":
---------------------------------------------
Run time:     78ms per run. Total for 3 runs: 236ms
Elapsed time: 88ms per run. Total for 3 runs: 264ms
GCs:          0 per run. Total for 3 runs: 1

Running benchmark "Binary Tree with Prelude":
---------------------------------------------
Run time:     6901ms
Elapsed time: 7667ms
GCs:          86

Running benchmark "Binary Tree with ReadShowTerm":
--------------------------------------------------
Run time:     154ms per run. Total for 3 runs: 462ms
Elapsed time: 192ms per run. Total for 3 runs: 578ms
GCs:          1 per run. Total for 3 runs: 4
```

KiCS2:
```
Running benchmarks
==================

Initializing FCY benchmarks...

Running benchmark "FCY with rw-data":
-------------------------------------
Run time:     10ms per run. Total for 30 runs: 323ms
Elapsed time: 10ms per run. Total for 30 runs: 323ms
GCs:          12 per run. Total for 30 runs: 389

Running benchmark "FCY with Prelude":
-------------------------------------
Run time:     344ms per run. Total for 10 runs: 3442ms
Elapsed time: 343ms per run. Total for 10 runs: 3436ms
GCs:          376 per run. Total for 10 runs: 3762

Running benchmark "FCY with ReadShowTerm":
------------------------------------------
Run time:     94ms per run. Total for 30 runs: 2826ms
Elapsed time: 94ms per run. Total for 30 runs: 2820ms
GCs:          106 per run. Total for 30 runs: 3201

Initializing parameterized FCY benchmarks...

Running benchmark "No string extraction":
-----------------------------------------
Run time:     11ms per run. Total for 30 runs: 352ms
Elapsed time: 11ms per run. Total for 30 runs: 351ms
GCs:          14 per run. Total for 30 runs: 426

Running benchmark "No string inlining":
---------------------------------------
Run time:     12ms per run. Total for 30 runs: 364ms
Elapsed time: 12ms per run. Total for 30 runs: 363ms
GCs:          16 per run. Total for 30 runs: 486

Running benchmark "alphabet size 10 (vs 26)":
---------------------------------------------
Run time:     9ms per run. Total for 30 runs: 281ms
Elapsed time: 9ms per run. Total for 30 runs: 280ms
GCs:          12 per run. Total for 30 runs: 369

Running benchmark "alphabet size 94 (vs 26)":
---------------------------------------------
Run time:     13ms per run. Total for 30 runs: 410ms
Elapsed time: 13ms per run. Total for 30 runs: 410ms
GCs:          17 per run. Total for 30 runs: 526

Preparing container parameters (list, map, trie)...

Running benchmark "FCY with parametrized containers: list":
-----------------------------------------------------------
Run time:     17ms per run. Total for 30 runs: 511ms
Elapsed time: 17ms per run. Total for 30 runs: 510ms
GCs:          21 per run. Total for 30 runs: 634

Running benchmark "FCY with parametrized containers: map":
----------------------------------------------------------
Run time:     19ms per run. Total for 30 runs: 583ms
Elapsed time: 19ms per run. Total for 30 runs: 582ms
GCs:          28 per run. Total for 30 runs: 845

Running benchmark "FCY with parametrized containers: trie":
-----------------------------------------------------------
Run time:     12ms per run. Total for 30 runs: 364ms
Elapsed time: 12ms per run. Total for 30 runs: 363ms
GCs:          13 per run. Total for 30 runs: 414

Initializing Peano benchmarks...

Running benchmark "Peano with rw-data":
---------------------------------------
Run time:     6ms per run. Total for 30 runs: 199ms
Elapsed time: 6ms per run. Total for 30 runs: 198ms
GCs:          11 per run. Total for 30 runs: 334

Running benchmark "Peano with Prelude":
---------------------------------------
Run time:     1148ms per run. Total for 10 runs: 11489ms
Elapsed time: 1145ms per run. Total for 10 runs: 11451ms
GCs:          1606 per run. Total for 10 runs: 16069

Running benchmark "Peano with ReadShowTerm":
--------------------------------------------
Run time:     392ms per run. Total for 30 runs: 11762ms
Elapsed time: 390ms per run. Total for 30 runs: 11726ms
GCs:          548 per run. Total for 30 runs: 16465

Initializing Binary Tree benchmarks...

Running benchmark "Binary Tree with rw-data":
---------------------------------------------
Run time:     1ms per run. Total for 30 runs: 34ms
Elapsed time: 1ms per run. Total for 30 runs: 34ms
GCs:          1 per run. Total for 30 runs: 47

Running benchmark "Binary Tree with Prelude":
---------------------------------------------
Run time:     156ms per run. Total for 10 runs: 1563ms
Elapsed time: 155ms per run. Total for 10 runs: 1556ms
GCs:          211 per run. Total for 10 runs: 2115

Running benchmark "Binary Tree with ReadShowTerm":
--------------------------------------------------
Run time:     44ms per run. Total for 30 runs: 1321ms
Elapsed time: 43ms per run. Total for 30 runs: 1315ms
GCs:          50 per run. Total for 30 runs: 1516
```
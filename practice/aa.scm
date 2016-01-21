;aa.scm
;asynchronous rlem cellular automata
;jpt4
;UTC20160121

#|
Three-in/three-out reversible logic elements with memory (RLEMs/rlems) 
tessellate to cover a hexagonal grid. Each RLEM has three neighbors, adjacent
on each of its A, B, and C channels. 

Ex:
        R3 
\B     /C
 R0-A-R1
/C     \B
        R2

Each RLEM executes a reversible automaton operation, moving symbols from input
to output, and changing the value in memory. Of the many three input/output
RLEMs (of the many n input/output RLEMs), 3-453 behaves as follows:

Standard RLEM 3-453:
IN: MEM A B C <-> OUT: MEM A B C
    0/1 0 0 0          0/1 0 0 0
     0  0 0 1           1  1 0 0
     0  0 1 0           1  0 0 1
     0  1 0 0           1  0 1 0

When viewed in diagram form, MEM=0 rotates symbols to the right, while MEM=1
rotates to the left.

Extending this behavior to multiple simultaneous input signals yields the
following:

Multi-signal rlem 3453:
IN: MEM A B C <-> OUT: MEM A B C
    0/1 0 0 0          0/1 0 0 0
     0  0 0 1           1  1 0 0  
     0  0 1 0           1  0 0 1    
     0  0 1 1           1  1 0 1    
     0  1 0 0           1  0 1 0    
     0  1 0 1           1  1 1 0    
     0  1 1 0           1  0 1 1
     0  1 1 1           1  1 1 1    

To maintain full quasi-delay invariance, each RLEM implements pull-based 
asynchronous state update in the manner of a cellular automata. In hardware, or truly concurrent software processes,
this might be done by all RLEMs cyclically polling their neighbors output 
channels for non-zero values. In the software simulated concurrency of this ,
when triggered by the 
master scheduler, only the active RLEM scans its 

To implement this behavior, a pull-based asynchronous 

Pull-based rlem state update:
rlem node queries the output channels of 


|#

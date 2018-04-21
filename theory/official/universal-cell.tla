\* utc20170527
\* jpt4

MODULE Universal-Cell

EXTENDS Naturals, Sequences, Bags

CONSTANTS sta, typ, ups, inp, out, mem, aut, con, buf

VARIABLES s, t, u, i, o, m, a, c, b

UC-asm == <<s, t, u, i, o, m, a, c, b>>

Init == /\ s \in sta
        /\ t \in typ
        /\ u \in ups
        /\ i \in inp
        /\ o \in out
        /\ m \in mem
        /\ a \in aut
        /\ c \in con
        /\ b \in buf

Activate == /\ s = "qa0"
               \/ /\ t = "wire"
                  /\ s' = "qw0"
                  /\ Wire
               \/ /\ t = "proc"
                  /\ s' = "qp0"
                  /\ Proc
               \/ /\ t = "stem"
                  /\ s' = "qs0"
                  /\ Stem

Halt == /\ s = "halt"

Wire == \/ s = "qw0"
           \/ /\ o /= {"_" "_" "_"} /\ s' = "halt" /\ Halt
           \/ /\ o = {"_" "_" "_"} /\ s' = "qw1"
        \/ s = "qw1"
           \/ /\ u

Spec == /\ Init
        /\ s' = "qa0"
        /\ Activate

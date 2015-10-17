# PRC - Pervasively Reconfigurable Computing

Code, prose, etc. for the project to align computers with their
embodiment, bring awareness and tunability to their resource
consumption, and produce a platform acceptable to me and a seed SAI.

The directory *theory* is everything which does not execute (prose),
nn*practice* everything that does (code). Meta-data and compliance
documents (etc.) may not follow this schema.

Developed against GNU Guile.

Released under the MIT License.

## In Which an Argument is Made for the Hidden to Become Visible

###Pathos (or, Propaganda and Rationale):

Semantically, the technology stack of a modern computer grounds its
validity in the non-linear amplification effect of the transistor
phenomenon, which incarnates Boolean logic. Politically, a modern
computer grounds its trust in the OEM, who choses how the transistors
are arranged. Between these two roots and the owner-operator is a
dependency graph of terrific complexity, efforts of which to simplify
or clarify have not kept pace with its growth. Such is the ruthless
reductionism of digital computation that every addition is ultimately
not even a translation, but a transliteration of the ones before it,
and thus in thrall to the intercessory de/re/encoders. No amount of
semantic hygiene [0] can prevent arbitrary modification of those
layers further from the root by those closer to it, through which the
former must pass. The most mature efforts toward full-stack
transparency, Free Software, Open Source Hardware, Clean Code and
literate programming, are socio-cultural ventures, while signed
firmware, trusted cryptographic co-processors, and compilation target
JavaScript are technological impediments. There can be neither a
totally self-modifying artificial intelligence, nor true private
ownership of the means of general purpose computation, without a
technologically feasible, pervasively reconfigurable computing device,
programmatically exposed from the lowest semantic level upwards.

[0] Absent 1. Indistinguishability Obfuscation/Fully Homomorphic
Encryption and/or 
2. Physically Unclonable Functions.

###Logos (or, Technical Contributions):

practice/{3453.scm, 318.scm, and 390.scm} implement Universal Cells
(UC) which expose the logical operation of RLEMs 3-453/18/90,
respectively.

theory/{gelc-universality.txt, figures.pdf} are a note and associated
diagrams describing the relation between RLEMs and GELCs in greater
(though yet terse) detail, proving by construction the universality of
RLEM-based Universal Cells.

###Ethos (or, Prior Work):

theory/history.txt: An oral history of influences during the project's
gestation.

theory/literature: Collated relevant existing research.

####Key References in Brief:

[0] Gershenfeld et al. (2011) Reconfigurable Asynchronous Logic
    Automata

[1] Morita, K. (2013)  Reversible Logic Elements with Memory and Their
    Universality

[2] Siekanowicz, W., and Schilling, W. (1968) A New Type of Latching
    Switchable Ferrite Junction Circulator




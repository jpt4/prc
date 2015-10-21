# PRC - Pervasively Reconfigurable Computing

Code, prose, etc. for the project to align computers with their
embodiment, bring awareness and tunability to their resource
consumption, and produce a platform acceptable to me and a seed SAI.

The directory *theory* is everything which does not execute (prose),
*practice* everything that does (code). Meta-data and compliance
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
Encryption and/or 2. Physically Unclonable Functions.

###Logos (or, Technical Contributions):

The overall Boolean function performed by a network of transistors may
be deduced by composing the formal descriptions of individual
transistors. How transistors are physically composed requires the
auxiliary reasoning of a routing algorithm, because the wiring of the
input network wiring between processing elements is a matter apart
from the processing elements themselves.

The hardware of a PRC system is a component-wise, organically
reconfigurable lattice whose nodes are Universal Cells (UCs),
connected to form Geometrically Explicit Logic Circuits (GELCs). A
single transistor can be specified such that it embodies a Boolean NOR
logic gate, satisfying abstract logical universality. However,
composing sequences of transistors requires the auxiliary
formalization of power, logic, and (circumstantially) clock signal
routing between processing elements.

In constrast, a GELC is deductively and entirely defined by its
formal specification of its component UCs. As the semantic bedrock of PRC, a UC supports:

1. Reversible logic
2. Asynchronous operation
3. Explicit signal and power routing
4. Organic, component-wise reconfigurability

By "component-wise", reconfigurability is an atomic operation; a UC
must be the least viable semantic element, such that if it were
decomposed further it would not retain the complete set of properties
guaranteed above. The process of identifying a suitable physical
incarnation of a UC is the search for the least phenomena which most
exactly capture the above desired capabilities. At present, a
reconfigurable extension of Reversible Elements with Memory (RLEMs)
can be put in close correspondence with the behavior of a switchable,
latching electromagnetic radiation (EMR) circulator, while conforming
to the above criteria.

practice/{3453.scm, 318.scm, and 390.scm} implement Universal Cells
(UC) which expose the logical operation of RLEMs 3-453/18/90,
respectively.

theory/{gelc-universality.txt, figures.pdf} are a note and associated
diagrams describing the relation between RLEMs and GELCs in greater
detail, including a proof by construction of the universality of
RLEM-based Universal Cells.

###Ethos (or, Prior Work):

While provenancially independent, PRC shares much of its current
ideology and abstract model with MIT's Reconfigurable Asynchronous
Logic Automata (RALA) project [0]. Kenichi Morita and his
collaborators developed Reversible Logic Elements with Memory in
several papers over the last 20 years [1]. Switching, latching EMR
circulators have been studied by radio engineers since the mid-20th
century [2].

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

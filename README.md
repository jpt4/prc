# prc
Pervasively Reconfigurable Hardware

Code, etc. for the project.

Developed against GNU Guile.

3453.scm, 318.scm, and 390.scm implement Universal Cells (UC) which
expose the logical operation of RLEMs 3-453/18/90, respectively.

univ-[etc].txt is a note, and hexagon.odg a set of figures, describing
the relation between RLEMs and GELCs in greater (though yet terse)
detail.

Released under the MIT License.

Propaganda and rationale follows:

Semantically, the technology stack of a modern computer grounds its
validity in the non-linear amplification effect of the transistor
phenomenon, which incarnates Boolean logic. Politically, a modern
computer grounds its trust in the OEM, who choses how its transistors
are arranged. Between these two roots and the owner-operator is a
dependency graph of terrific complexity, efforts of which to simplify
or clarify, have not kept pace with its growth. Such is the ruthless
reductionism of digital computation that every addition is ultimately
not even a translation, but a transliteration of the ones before it,
and thus in thrall to the intercessory encoders. No amount of semantic
hygiene [0] can prevent arbitrary modification of those layers further
from the root by those closer to it, through which the former must
pass. The most mature efforts toward full-stack transparency, Free
Software, Open Source Hardware, Clean Code and literate programming,
are socio-cultural ventures, while signed firmware, trusted
cryptographic co-processors, and compilation target JavaScript are
technological impediments. There can be neither a totally
self-modifying artificial intelligence, nor true private ownership of
the means of general purpose computation, without a technologically
feasible, pervasively reconfigurable computing device,
programmatically exposed from the lowest semantic level upwards.

[0] Absent 1) Indistinguishability Obfuscation/Fully Homomorphic
Encryption and 2) Physically Unclonable Functions.

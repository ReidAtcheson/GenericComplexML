# Generic Complex numbers

Simple modules implementing generic complex numbers.

Note that real numbers can be recovered in this framework 
by assigning the imaginary type to a dummy singleton type
and setting `conj x = x` and similar definitions for anything
that explicitly depends on the imaginary parts.

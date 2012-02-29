# Nlopt-haskell

This is a set of bindings to the NLOPT library

Get it with

    git clone https://github.com/ghorn/nlopt-haskell

You have to install Nlopt yourself with:

    ./configure
    make
    make install

then install nlopt-haskell

    cd nlopt-haskell
    cabal install

There is a complete set of foreign bindings (Nlopt.Bindings) that match the nlopt.h header function-for-function.

These are wrapped in some nicer ADTs (Nlopt.Enums) and I wrote a nicer interface in Nlopt.Wrappers.

There are simple user functions (only handling unconstrained gradient-free stuff right now) in Nlopt.

The core data type is NloptOpt (ForeignPtr NloptOptRaw) which has side effects going on. The
current user functions are all safe, but I will rewrite Nlopt.Wrappers using ST so that there will
be a safe and complete set of wrappers.

Ghci can't link nlopt if you run ghci within the nlopt-haskell directory but it works fine outside of it >_<
If you want to run something after cabal installing, copy something from Nlopt.Examples into an external file
and run that.

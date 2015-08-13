Krust
=====

KDB bindings for Rust.

These bindings enable Rust to be used as inside Q to add additional functionality.
More generally they also enable Rust to communicate with KDB in a memory-safe way.

For an example of how to embed Rust code within Q, see `demos/embed`.
For an example of how to perform IPC between Rust and Q, see `demos/ipc`.

Building
--------

To use this library for IPC, you will first need to
compile a static library from the kx-supplied object file `c.o` using `ar`. 
The static library should be placed in `src/c/libkdb.a`.

```
ar rcs libkdb.a c.o
mv libkdb.so src/c
```

`c.o` can be found [here](http://code.kx.com/wsvn/code).

Compatibility
-------------

This is only tested with 64bit Linux (and 64bit Rust/Q).
I sincerely doubt that it would work with Windows or OSX.

It uses unstable features so requires rust-nightly.

License
-------

MIT

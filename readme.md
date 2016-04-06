# fast-builder

This is an efficient implementation of ByteString builder. It should be usable
as a drop-in replacement for the Builder provided by the bytestring package.

This package only supports GHC 7.10 and later, because it heavily relies on the
new oneShot primitive.

## Implementation

The biggest efficiency gain comes from avoiding the continuation passing style.
This means the builder can use the Haskell execution stack rather than a chain
of heap-allocated closures when traversing a tree-shaped data structure.

When generating a strict ByteString, the builder simply accumulates bytes in an
exponentially growing buffer. When generating a lazy ByteString that doesn't
fit in one chunk, a new thread is forked and the builder runs there. This allows
the builder to use the new thread's stack, independently from the consumer of
the ByteString. All this happens transparently to the user.

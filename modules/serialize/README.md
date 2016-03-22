## About Colleen-Serialize
This module is useful to serialise data, especially events. The bulk of the work is handled by cl-store. Additionally, the data is compressed using gzip.

## How To
Just call `serialize` on an object and a target stream, or `deserialize` on a source stream and an alist of discoveries. Currently only two discoveries are supported, for `'consumer`s and `'core`s. Each discovery entry is a cons of the type of discovery and a function that takes a single argument, the id. If no matching discovery is provided, the id is used instead of the full object.

For everything else, see cl-store.

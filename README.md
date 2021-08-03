# gzip-stream2
An extension to the [gzip-stream](https://github.com/mcna/gzip-stream) library.

This project was born from my efforts to use gzip-streams in conjunction with json reading. Specifically, [yason](https://github.com/phmarek/yason/). I found that the original gzip-stream implementation was missing support for `peek-char`, `peek-byte`, `file-position`, and `read-sequence`. This repository seeks to add such support.

This project is under development. I don't have any prior experience working with gray streams so I'm sure to miss important details and best practices when implementing them.

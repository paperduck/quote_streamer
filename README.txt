
-------------------------------------------------------------------------------
CHANGE LOG
-------------------------------------------------------------------------------

What's new in version 0.2.0.0, compared to version 0.1.0.0:

    - Put environment configuration into ReaderT monad transformer.
    - Removed padWithZeros; process packet time as numeric instead of String.
    - Added INLINE pragmas.
    - Filter by packet length to reduce unnecessary parsing.
    - Added GHC option to increase compiler optmizations.
    - Added command line options.
    - Replaced the Network.Transport.Internal.decodeNum functions with
        the myDecodeInt functions for simplicity; one less dependency.
    - Converted the split function to splitAts.
    - Added strictness markers (!).
    - Made code cleaner, more elegant, more idiomatic.
    - Organized code into separate modules.


-------------------------------------------------------------------------------
TECHNICAL SPECIFICATIONS
-------------------------------------------------------------------------------

This project was created using the Haskell Platform for Linux (Debian)
and Cabal.

QUOTE PIPELINE
    Quotes are parsed from the stream and travel through a pipeline before 
    appearing on your screen.

        stream -> drawer -> buffer -> reorder heap -> printer

    "Drawer" refers to a transaction drawer, or "pass thru drawer", between
    two threads that holds one quote.
    Using the "skip channel" option, which utilizes a drawer, disconnects the
    slow quote printing from the quote stream.

    Depending on the command line options, parts of the pipline may be
    omitted. For example, the default behavior uses the simplest pipeline:

        stream -> printer


-------------------------------------------------------------------------------
COMMAND LINE OPTIONS
-------------------------------------------------------------------------------

-v          --verbose         Verbose output
-h          --help            Show help
-r          --reorder         Reorder by quote accept time
-t          --ticker          Each quote overwrites the previous one
-s          --skip-channel    Skip quotes to eliminate printing bottleneck.
-b BUFSIZE  --buffer=BUFSIZE  Buffer up to BUFSIZE quotes to be printed.
                              Implemented mainly as an exercise.
                              Not as useful as it seems.


-------------------------------------------------------------------------------
EXAMPLES
-------------------------------------------------------------------------------

The name of the Cabal project is "cs", short for "code sample", so please use
that when running.

Default, unordered:

    $ cabal run

Reordered:

    $ cabal run cs -- -r

Show help:

    $ cabal run cs -- -h

Verbose, reordered, ticker format, skip channel, with buffer of size 100:

    $ cabal run cs -- -vrtsb 100


-------------------------------------------------------------------------------
KNOWN ISSUES
-------------------------------------------------------------------------------

- The PCAP/UDP parsing is not robust; it is custom tailored to one PCAP file.
- If your terminal is not wide enough to fit a whole quote on one line,
     the ticker option may not work as expected.
- The gist of the buffer is to prevent data loss while prioritizing the
    intake speed of the stream.
    Unfortunately, it appears that MVars impose more latency than
    printing to the screen, rendering the buffer useless because the
    quotes arrive in the buffer slower than they are printed.
    This could be addressed by buffering quotes in the main thread, before
    they are handed off to the drawer. The buffer would fill up quickly and
    would end up preserving a contiguous chunk of quotes from the beginning
    of the stream.


-------------------------------------------------------------------------------
TESTING
-------------------------------------------------------------------------------

Hspec unit tests are available in Tests.hs. Run like this:

    $ cd src/
    $ runhaskell Tests.hs





  5
  6 {- UPDATES SINCE v.1
  7     Removed padWithZeros, process packet time as Int instead of String.
  8     Use myDecodeInt functions instead of Network.Transport.Internal.decodeNum functions
  9         - for simplicity
 10         - The Network functions use unsafeIO
 11         - one less dependancy
 12     add command line options
 13     put environment configuration into ReaderT
 14     added INLINE pragmas
 15     Added GHC option to increase compiler optmizations
 16     Convert split function to splitAts
 17     Added strictness markers (!) where possible
 18     Make code more idiomatic
 19 -}
 20
 21 {- README
 22     > Command line options
 23         - multithreading / skip channel
 24         - buffer
 25         - ticker
 26         - verbosity
 27     > UDP parsing is not robust; custom tailored to one PCAP file.
 28     > quote pipeline:  stream  -> drawer -> buffer -> reorder queue -> printer
 29     > explain what a transaction drawer is
 30     > "A single MVar is used to create a skip channel, eliminating bottleneck"
 31
 32     KNOWN ISSUES
 33     - The gist of the buffer is to prevent data loss while prioritizing the intake speed of the stream.
 34           Unfortunately, it appears that MVars impose more latency than
 35           printing to STDIO, rendering the buffer useless because the quotes arrive in the buffer slower than they are printed.
 36          This could be addressed by buffering quotes in the main thread, before they are handed off to
 37           the drawer. For this particular application, the buffer would fill up quickly and would end up preserving a contiguous chunk of quotes from
 38             the beginning of the stream.
 39     If your terminal is not wide enough to fit a whole quote on one line,
 40          the ticker option may not work as expected.
 41 -}


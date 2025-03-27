rw-data
=======

This package contains an implementation of a compact data term representation
which can be used to write data to files and read data from files
faster compared to the standard instances of `Show` and `Read`.
Furthermore, the accompanying package `rw-data-generator` contains a tool
`curry-rw-data` to generate read and write operations for all types
defined in a given module. For this purpose, the tool
generates instances of the class `ReadWrite` (see module `RW.Base`)
for the given type declarations.

To write and read compact data to and from files, the module `RW.Base`
provides operations, like

    readDataFile  :: ReadWrite a => FilePath -> IO (Maybe a)
    writeDataFile :: ReadWrite a => FilePath -> a -> IO ()

to read and write compact data.

Usage
-----

If the module `Mod` contains definitions of data types,
the command (see package `rw-data-generator` to generate
the executable)

    > curry-rw-data Mod

generates a new Curry module `ModRW` containing instance
definition of class `ReadWrite` for all types defined in `Mod`.
In order to store or read these data in a compact format,
one has to import the module `Mod`, `ModRW` and `RW.Base`
in order to use the operations `writeDataFile` or
`readDataFile`.

Documentation
-------------

The ideas and description of the compact data representation
are described in the bachelor's thesis

[Compact Representation of Data Terms](https://www.michaelhanus.de/lehre/abschlussarbeiten/bsc/Zuengel_Lasse.pdf)
(in German, by Lasse Züngel, CAU Kiel, March 2024)

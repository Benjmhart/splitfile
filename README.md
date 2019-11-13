# splitfile

This is a nifty tool for splitting up CSV and similar files.

## Usage:

`splitfile <filename> <line numbers per file>`

splitfile will preserve the header line, and pull the specified number of subsequent lineslines into each file. 

so a 300 line CSV would turn into a folder of the same name, with 3 files in it, of approximately 101 lines each (the header line plus up to 100 subsequent lines)

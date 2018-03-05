## dependancies
- "pdftotext" program

## known bugs
- using too high a reading speed causing file to lock (cannot be opened).
  the specific speed at which this happens likely varies per machine
- interrupting the program with CTRL-C can happen in-between read/writes
  and result in an empty bookmark file

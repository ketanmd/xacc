# xacc
extend ledger cli with preprocessing support for single-line single- and multi- event records


Configuration:

There should be files of the form */*.csv or */*.passbook in the working directory.
These files are processed into new files generated in a directory named 'processed/' .

*/*.passbook
------
Primary hand-edited files
supports events that imply multiple transactions: Opening a CD, buying a bond, taking a loan.

*/*.csv
-----
Downloaded csv files, usually from brokerage accounts

config/known.*
-------
Configuration files, still being worked on.
Minimizing the required presence of entries in 'config/' is work in progress .

Any file ending with a '~' will be ignored.

I suggest a structure like this:

'<year>/*.csv'              # processed
'<year>/records/<files>'    # not processed
'passbooks/*.passbook'      # processed
'passbooks/records/<files>' # not processed

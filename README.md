# odbc32
Connect to a database using a 32-bit ODBC driver from a 64-bit R-process using the `DBI`-interface.

## Motivation
It is often hard if not impossible to install new odbc-drivers in corporate environments. Moreover ms office does not allow having both 32-bit and 64-bit Access drivers *of the same version* installed at the same time. Thus one of them must go, or an older version needs to be installed, and that might cause issues.

Office will not by default install 64-bit drivers for Access, only 32-bit drivers. Trying to connect to an Access db from a 64-bit R-process using the excellent `odbc` package might yield:

```R
library(DBI)

conn = dbConnect(odbc::odbc(), .connection_string="Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=c:/path/to/database.accdb")
# Error: nanodbc/nanodbc.cpp:1021: IM002: [Microsoft][ODBC Driver Manager] Data source name not found and no default driver specified 
```
This is not `odbc`'s fault since it is the 64-bit driver that is missing. To make it work one would have to either redo the same thing from 32-bit R but then your analyses will be limited to 2Gb memory. One could also just use the `odbc32` package: 

```R
library(DBI)

conn = dbConnect(odbc32::odbc32(), .connection_string="Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=c:/path/to/database.accdb")

df = dbReadTable(conn, "table1")

dbExecute(conn, "insert into table1 values (?, ?)", params = list(1:10, letters[1:10]))

dbDisconnect(conn)

```


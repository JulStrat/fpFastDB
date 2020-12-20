FastDB Main Memory DBMS system version 2.61.

----------------------------------------------------------------------------------------
Installation under Delphi/Kylix
===============================
1. Open Delphi/Kylix IDE
2. Open FastDBd6.dpk project (File/Open Project)
3. In the project viewer (View/Project Manager) click "Compile", and then "Install".  The VCL library will be installed on the component panel under "Data Access" tab.  Two components will be registered: TFastDbSession and TFastDbQuery.

----------------------------------------------------------------------------------------
Known issues
============
1.  TFastDbSession
		  The following sequence of calls corrupts table field descriptor of SomeTableName1:
		  CreateTable("SomeTableName1", ...)
		  DescribeTable("SomeTableName1")
		  CreateTable("SomeTableName2", )
		  DescribeTable("SomeTableName1")

    Workaround: Don't call DescribeTable between CreateTable() calls 

2.  In large multi-threaded projects using LocalCLI interface if concurrently some 
    threads do inserts and others do deletes of records in the database, this may 
    lead to an Access Violation.  This problem was consistently observed in a large 
    database (512M).  It has not been a problem with single-threaded applications.

    Workaround: Modify application not to use record deletions in the database in a 
                multi-threaded content.
----------------------------------------------------------------------------------------


Enjoy!

Serge Aleynikov
(asergey@iname.com)

Also check this site http://www.garret.ru/~knizhnik/fastdb.html for the latest FastDB kernel.

DISCLAIMER OF WARRANTY.

COVERED CODE IS PROVIDED ON AN ``AS IS'' BASIS, WITHOUT WARRANTY OF ANY KIND, 
EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTIES THAT 
THE COVERED CODE IS FREE OF DEFECTS, MERCHANTABLE, FIT FOR A PARTICULAR PURPOSE OR
NON-INFRINGING. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE COVERED 
CODE IS WITH YOU. SHOULD ANY COVERED CODE PROVE DEFECTIVE IN ANY RESPECT, YOU 
(NOT THE INITIAL DEVELOPER OR ANY OTHER CONTRIBUTOR) ASSUME THE COST OF ANY 
NECESSARY SERVICING, REPAIR OR CORRECTION. THIS DISCLAIMER OF WARRANTY 
CONSTITUTES AN ESSENTIAL PART OF THIS LICENSE. NO USE OF ANY COVERED CODE IS 
AUTHORIZED HEREUNDER EXCEPT UNDER THIS DISCLAIMER.

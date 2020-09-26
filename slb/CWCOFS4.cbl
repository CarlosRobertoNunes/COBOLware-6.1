      $SET CallFH"FHREDIR" NoOptional-File
       EXEC COBOLware
       COPY CWCOFH.CBL REPLACING ==CWCOFH== BY ==CWCOFS4==
       END-EXEC.

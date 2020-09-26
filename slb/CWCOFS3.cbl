      $SET CallFH"FHREDIR" NoOptional-File
       EXEC COBOLware
       COPY CWCOFH.CBL REPLACING ==CWCOFH== BY ==CWCOFS3==
       END-EXEC.

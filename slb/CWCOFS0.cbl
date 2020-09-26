      $SET CallFH"FHREDIR" NoOptional-File
       EXEC COBOLware
       COPY CWCOFH.CBL REPLACING ==CWCOFH== BY ==CWCOFS0==
       END-EXEC.

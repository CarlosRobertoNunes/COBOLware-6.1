           SELECT LOCKS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS LOCKS-CHAVE
                  ALTERNATE RECORD KEY IS LOCKS-COMMITS =
                                          LOCKS-USERNAME
                                          LOCKS-DEVICE
                                          WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  LOCK ON MULTIPLE RECORDS
                  FILE STATUS   IS FS-LOCKS.

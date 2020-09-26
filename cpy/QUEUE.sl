           SELECT (FILENAME) ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS (FILENAME)-CHAVE
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-(FILENAME).


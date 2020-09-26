           05 cwuser-POSITION.
              10 cwuser-LINE       PIC  9(002).
              10 cwuser-COLUMN     PIC  9(002).
           05 cwuser-WIDTH         PIC  9(002).
           05 cwuser-HEIGHT        PIC  9(002).
           05 cwuser-MAP.
              10 cwuser-CHARACTERS.
                 15 GUI-LIN                    OCCURS 25.
                    20 GUI-COL     PIC  X(001) OCCURS 80.
              10 cwuser-ATTRIBUTES PIC X(2000).
           05 cwuser-LENGTH-CHAR   PIC  9(004).
           05 cwuser-LENGTH-ATTR   PIC  9(004).
           05 cwuser-CAPTION       PIC  X(080).

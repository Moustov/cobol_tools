        IDENTIFICATION DIVISION.
         PROGRAM-ID. DIFDA2.
         ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
         SOURCE-COMPUTER.
                    UNIX.
         OBJECT-COMPUTER.
                    UNIX.
         DATA DIVISION.
         WORKING-STORAGE SECTION.
         01  META-RENS.
         01  DAT2A-R                 REDEFINES       DAT2A
                                     PICTURE         9(6).
         LINKAGE SECTION.
             COPY    DTFLIEN.
         01  DIFF.
             03 DATE-DE              PICTURE         S9(7)
                                     USAGE           COMPUTATIONAL-3.
             03 DATE-FIN             PICTURE         S9(7)
                                     USAGE           COMPUTATIONAL-3.
             03 DATE-DIFF            PICTURE         S999
                                     USAGE           COMPUTATIONAL-3.
         PROCEDURE DIVISION USING DIFF DTFLNK DTFINP DTFWRK DTFCOM.
         DEBUT SECTION.
         DEB.
             MOVE DAT1A-R TO LK-AAMMJJ6-ENT-2
                     PERFORM         APPEL-SP2000-DEB
                                     THRU
                                     APPEL-SP2000-FIN.
             IF LK-CONV-1 NOT > LK-CONV-2
        *              AND LK-CONV-1 NOT = LK-CONV-2
                OR LK-CONV-1 NOT = LK-CONV-2
               MOVE 0 TO DIFFDATE
               GO TO DEB-FIN
             ELSE
                     PERFORM         ASUP
                                     THRU
                                     ASUP-FIN.
         DEB-FIN.
         ASUP.
            EXIT.
         ASUP-FIN.
         EXIT-GOBACK-PROGRAM.
             EXIT PROGRAM.
        **--------------------------------------------------------
         APPEL-SP2000-DEB.
             CALL "SP2000"    USING ISP2000 DTFLNK DTFINP DTFWRK DTFCOM.
         APPEL-SP2000-FIN.
             EXIT.
        **--------------------------------------------------------

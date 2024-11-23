from unittest import TestCase

from cobol_activity_diagram import parse_cobol, generate_plantuml, get_plantuml_style


class TestCobol(TestCase):
    def same_string(self, actual: str, expected: str, indentation_matters=False) -> bool:
        actual_lines = actual.split('\n')
        expected_lines = expected.split('\n')
        for (line_num, line_string) in enumerate(actual_lines):
            if indentation_matters:
                self.assertEqual(line_string, expected_lines[line_num], f"difference found at line {line_num + 1}")
            else:
                # print(f"actual_line: '{line_string.lstrip()}'")
                # print(f"expected_line: '{expected_lines[line_num].lstrip()}'")
                self.assertEqual(line_string.strip(), expected_lines[line_num].strip(),
                                 f"difference found at line {line_num + 1}")
        return True

    def test_parse_cobol_if8(self):
        code = """IDENTIFICATION DIVISION.
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
                          IF TRI-DT-JC = RUPT-DT-JC                                    
      *DEBUT SIG 115841 : ENREG MDE DE VRS 05, 64 ET 65 POUR LCBL       
              IF TRI-MDE-VRS = RUPT-MDE-VRS                             
      *FIN SIG 115841                                                   
                 IF RUPT-DT-JC < W-DT-JC-DEB                            
                    MOVE TRI-PER-REP TO RUPT-PER-REP                    
                       PERFORM     INITIAL-RUPT-TECR                    
                                   THRU                                 
                                   F-INITIAL-RUPT-TECR                  
                    GO TO BC-RET-ECRI                                   
                 ELSE                                                   
                    IF TRI-PER-REP = RUPT-PER-REP                       
                       PERFORM     INITIAL-RUPT-TECR                    
                                   THRU                                 
                                   F-INITIAL-RUPT-TECR                  
                       GO TO BC-RET-ECRI                                
                    ELSE                                                
                       IF ECRI-CD-DECLE = 1                             
                       OR ECRI-CD-DECLE = 6                             
                          MOVE TRI-PER-REP TO RUPT-PER-REP              
                       PERFORM     INITIAL-RUPT-TECR                    
                                   THRU                                 
                                   F-INITIAL-RUPT-TECR                  
                          GO TO BC-RET-ECRI                             
                       ELSE                                             
                       PERFORM     CONT-REEC                            
                                   THRU                                 
                                   F-CONT-REEC                          
                       PERFORM     INITIAL-RUPT-JC                      
                                   THRU                                 
                                   F-INITIAL-RUPT-JC                    
                          GO TO BC-RET-ECRI.                            
      *DEBUT SIG 115841 : ENREG MDE DE VRS 05, 64 ET 65 POUR LCBL       
           IF TRI-DT-JC = RUPT-DT-JC                                    
              IF TRI-MDE-VRS NOT = RUPT-MDE-VRS                         
      **************                                                    
      *    CHANGEMENT DE MDE VRS POUR UNE MEME PERIODE / MEME JC        
      **************                                                    
                 IF RUPT-DT-JC NOT < W-DT-JC-DEB                        
                       PERFORM     CONT-REEC                            
                                   THRU                                 
                                   F-CONT-REEC                          
                       PERFORM     INITIAL-RUPT-MDE-VRS                 
                                   THRU                                 
                                   F-INITIAL-RUPT-MDE-VRS               
                    GO TO BC-RET-ECRI                                   
                 ELSE                                                   
      *****                                                             
      *    JOURNEES ANTERIEURES                                         
      *****                                                             
      *U                DISPLAY  "AVT INITIAL RUPT JC SI JC ANT"        
      *U                DISPLAY " solde autres "  SOLDE-AUTRES          
      *U                        " tot-cdt-aut " W-TOT-CDT-AUT           
                       PERFORM     INITIAL-RUPT-MDE-VRS                 
                                   THRU                                 
                                   F-INITIAL-RUPT-MDE-VRS               
      *U                DISPLAY "APR - RUPT MD VRS " RUPT-MDE-VRS       
      *U                        " RUPT MD VRS SAVE " RUPT-SAVE-MDE-VRS  
      *U                DISPLAY " solde autres "  SOLDE-AUTRES          
      *U                        " tot-cdt-aut " W-TOT-CDT-AUT           
      *****                                                             
      *    ECRITURE SOLDES AVANT JOURNEES A COMPTABILISER SUR TEMOIN    
      *****                                                             
                    IF TRI-DT-JC < W-DT-JC-DEB                          
                       GO TO BC-RET-ECRI                                
                    END-IF  
                    IF ECRI-CD-TEMO-ENR NOT = "XX"   
                       GO TO BC-RET-ECRI                                
                    END-IF                                              
                    MOVE 0 TO IND                                       
                    GO TO RUPTURE-ECRITURE-40                           
                 END-IF                                                 
              END-IF                                                    
           END-IF.                                                      
      *FIN SIG 115841                                                   
      **************                                                    
      *    CHANGEMENT DE DATE DE JOURNEE POUR UNE MEME PERIODE          
      **************                                                    
      *    JOURNEES A COMPTABILISER                                     
      ******                                                            
      *U       DISPLAY  " CHGT DT JC RUPT DT JC  "  RUPT-DT-JC          
      *U                 " dt jc  deb " W-DT-JC-DEB.                    
           IF RUPT-DT-JC NOT < W-DT-JC-DEB                              
      *U          DISPLAY  " CHGT DT JC RUPT DT JC AVT CONT REEC "      
                       PERFORM     CONT-REEC                            
                                   THRU                                 
                                   F-CONT-REEC                          
      *U          DISPLAY  "AVT INITIAL RUPT JC SI CHGT JC"             
                       PERFORM     INITIAL-RUPT-JC                      
                                   THRU                                 
                                   F-INITIAL-RUPT-JC                    
      *U          DISPLAY "APR - RUPT MD VRS " RUPT-SAVE-MDE-VRS        
             GO TO BC-RET-ECRI.                                         
      *****                                                             
      *    JOURNEES ANTERIEURES                                         
      *****                                                             
UDBG  *DISPLAY  "AVT CONT REEC-ANT ".                           
UDBG  *DISPLAY " solde autres "  SOLDE-AUTRES                   
UDBG  *        " tot-cdt-aut " W-TOT-CDT-AUT                    
                       PERFORM     CONT-REEC-ANT                        
                                   THRU                                 
                                   F-CONT-REEC-ANT.                     
      *U       DISPLAY  "AVT INITIAL RUPT JC SI JC ANT".                
      *U       DISPLAY " solde autres "  SOLDE-AUTRES                   
      *U               " tot-cdt-aut " W-TOT-CDT-AUT                    
                       PERFORM     INITIAL-RUPT-JC                      
                                   THRU                                 
                                   F-INITIAL-RUPT-JC.                   
      *U       DISPLAY "APR - RUPT MD VRS " RUPT-SAVE-MDE-VRS.          
      *U       DISPLAY " solde autres "  SOLDE-AUTRES                   
      *U               " tot-cdt-aut " W-TOT-CDT-AUT                    
      *****                                                             
      *    ECRITURE SOLDES AVANT JOURNEES A COMPTABILISER SUR TEMOIN    
      *****                                                             
           IF TRI-DT-JC < W-DT-JC-DEB                                   
             GO TO BC-RET-ECRI. 
           IF ECRI-CD-TEMO-ENR NOT = "XX"         
             GO TO BC-RET-ECRI.                                         
           MOVE 0 TO IND.                                               
       RUPTURE-ECRITURE-40. 
           """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<
	
note right: //(l:25)//

		if (__IF__ TRI-DT-JC = RUPT-DT-JC ?) then (yes)
						
note right: //(l:26)//

			if (__IF__ TRI-MDE-VRS = RUPT-MDE-VRS ?) then (yes)
							
note right: //(l:28)//

				if (__IF__ RUPT-DT-JC < W-DT-JC-DEB ?) then (yes)
								
note right: //(l:30)//

				:MOVE TRI-PER-REP TO RUPT-PER-REP;
				
note right: //(l:31)//

				#Pink:INITIAL-RUPT-TECR -> F-INITIAL-RUPT-TECR - voir ligne :???; <<procedure>>
				
note right: //(l:32)//

				#Pink:GO TO BC-RET-ECRI - voir ligne //(l:???)//> 
				detach
				else (no)
								
note right: //(l:36)//

					if (__IF__ TRI-PER-REP = RUPT-PER-REP ?) then (yes)
									
note right: //(l:37)//

					#Pink:INITIAL-RUPT-TECR -> F-INITIAL-RUPT-TECR - voir ligne :???; <<procedure>>
					
note right: //(l:38)//

					#Pink:GO TO BC-RET-ECRI - voir ligne //(l:???)//> 
					detach
					else (no)
									
note right: //(l:42)//

						if (__IF__ ECRI-CD-DECLE = 1\\nOR ECRI-CD-DECLE = 6 ?) then (yes)
										
note right: //(l:43)//

						:MOVE TRI-PER-REP TO RUPT-PER-REP;
						
note right: //(l:45)//

						#Pink:INITIAL-RUPT-TECR -> F-INITIAL-RUPT-TECR - voir ligne :???; <<procedure>>
						
note right: //(l:46)//

						#Pink:GO TO BC-RET-ECRI - voir ligne //(l:???)//> 
						detach
						else (no)
										
note right: //(l:50)//

						#Pink:CONT-REEC -> F-CONT-REEC - voir ligne :???; <<procedure>>
						
note right: //(l:51)//

						#Pink:INITIAL-RUPT-JC -> F-INITIAL-RUPT-JC - voir ligne :???; <<procedure>>
						
note right: //(l:54)//

						#Pink:GO TO BC-RET-ECRI - voir ligne //(l:???)//> 
						detach
						endif
					endif
				endif
			endif
		endif
	note
					.=DEBUT SIG 115841 : ENREG MDE DE VRS 05, 64 ET 65 POUR LCBL
	end note
		if (__IF__ TRI-DT-JC = RUPT-DT-JC ?) then (yes)
						
note right: //(l:59)//

			if (__IF__ TRI-MDE-VRS NOT = RUPT-MDE-VRS ?) then (yes)
							
note right: //(l:60)//

				if (__IF__ RUPT-DT-JC NOT < W-DT-JC-DEB ?) then (yes)
								
note right: //(l:64)//

				#Pink:CONT-REEC -> F-CONT-REEC - voir ligne :???; <<procedure>>
				
note right: //(l:65)//

				#Pink:INITIAL-RUPT-MDE-VRS -> F-INITIAL-RUPT-MDE-VRS - voir ligne :???; <<procedure>>
				
note right: //(l:68)//

				#Pink:GO TO BC-RET-ECRI - voir ligne //(l:???)//> 
				detach
				else (no)
								
note right: //(l:72)//

				note
								.=====
								.=    JOURNEES ANTERIEURES
								.=====
								.=U                DISPLAY  "AVT INITIAL RUPT JC SI JC ANT"
								.=U                DISPLAY " solde autres "  SOLDE=AUTRES
								.=U                        " tot=cdt=aut " W=TOT=CDT=AUT
				end note
				#Pink:INITIAL-RUPT-MDE-VRS -> F-INITIAL-RUPT-MDE-VRS - voir ligne :???; <<procedure>>
				
note right: //(l:79)//

					if (__IF__ TRI-DT-JC < W-DT-JC-DEB ?) then (yes)
									
note right: //(l:89)//

					#Pink:GO TO BC-RET-ECRI - voir ligne //(l:???)//> 
					detach
					endif
					if (__IF__ ECRI-CD-TEMO-ENR NOT = "XX" ?) then (yes)
									
note right: //(l:92)//

					#Pink:GO TO BC-RET-ECRI - voir ligne //(l:???)//> 
					detach
					endif
				:MOVE 0 TO IND;
				
note right: //(l:95)//

				#Pink:GO TO RUPTURE-ECRITURE-40 - voir ligne //(l:145)//> 
				detach
				endif
			endif
		endif
	note
					.=FIN SIG 115841
					.==============
					.=    CHANGEMENT DE DATE DE JOURNEE POUR UNE MEME PERIODE
					.==============
					.=    JOURNEES A COMPTABILISER
					.======
					.=U       DISPLAY  " CHGT DT JC RUPT DT JC  "  RUPT=DT=JC
					.=U                 " dt jc  deb " W=DT=JC=DEB.
	end note
		if (__IF__ RUPT-DT-JC NOT < W-DT-JC-DEB ?) then (yes)
						
note right: //(l:108)//

		#Pink:CONT-REEC -> F-CONT-REEC - voir ligne :???; <<procedure>>
		
note right: //(l:110)//

		#Pink:INITIAL-RUPT-JC -> F-INITIAL-RUPT-JC - voir ligne :???; <<procedure>>
		
note right: //(l:114)//

		#Pink:GO TO BC-RET-ECRI - voir ligne //(l:???)//> 
		detach
		endif
	note
					.=====
					.=    JOURNEES ANTERIEURES
					.=====
					.UDBG  =DISPLAY  "AVT CONT REEC=ANT ".
					.UDBG  =DISPLAY " solde autres "  SOLDE=AUTRES
					.UDBG  =        " tot=cdt=aut " W=TOT=CDT=AUT
	end note
	#Pink:CONT-REEC-ANT -> F-CONT-REEC-ANT - voir ligne :???; <<procedure>>
	
note right: //(l:125)//

	note
					.=U       DISPLAY  "AVT INITIAL RUPT JC SI JC ANT".
					.=U       DISPLAY " solde autres "  SOLDE=AUTRES
					.=U               " tot=cdt=aut " W=TOT=CDT=AUT
	end note
	#Pink:INITIAL-RUPT-JC -> F-INITIAL-RUPT-JC - voir ligne :???; <<procedure>>
	
note right: //(l:131)//

	note
					.=U       DISPLAY "APR = RUPT MD VRS " RUPT=SAVE=MDE=VRS.
					.=U       DISPLAY " solde autres "  SOLDE=AUTRES
					.=U               " tot=cdt=aut " W=TOT=CDT=AUT
					.=====
					.=    ECRITURE SOLDES AVANT JOURNEES A COMPTABILISER SUR TEMOIN
					.=====
	end note
		if (__IF__ TRI-DT-JC < W-DT-JC-DEB ?) then (yes)
						
note right: //(l:140)//

		#Pink:GO TO BC-RET-ECRI - voir ligne //(l:???)//> 
		detach
		endif
		if (__IF__ ECRI-CD-TEMO-ENR NOT = "XX" ?) then (yes)
						
note right: //(l:142)//

		#Pink:GO TO BC-RET-ECRI - voir ligne //(l:???)//> 
		detach
		endif
	:MOVE 0 TO IND.;
	
note right: //(l:144)//

	#palegreen:RUPTURE-ECRITURE-40<
	
note right: //(l:145)//

	note
					Usages: lines [96]
	end note
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

        def test_parse_cobol_if_else(self):
            code = """IDENTIFICATION DIVISION.
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
            """
            expected_puml = "@startuml\n" + get_plantuml_style()
            expected_puml += """start
     #palegreen:DEB<
     note right: //(l:25)//
         :MOVE DAT1A-R TO LK-AAMMJJ6-ENT-2 note right: //(l:26)//;
     #Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN:44 - voir ligne None; <<procedure>>
     note right: //(l:27)//
     if (__IF__ LK-CONV-1 NOT > LK-CONV-2 ?) then (yes)
         note right: //(l:30)//
          :MOVE 0 TO DIFFDATE note right: //(l:31)//;
      #Pink:GO TO DEB-FIN - voir ligne note right: //(l:37)//>
      note right: //(l:32)//
      detach
     else (no)
         note right: //(l:30)//
      #Pink:ASUP -> ASUP-FIN:38 - voir ligne None; <<procedure>>
      note right: //(l:34)//
     endif
     #palegreen:DEB-FIN<
     note right: //(l:37)//
     note
         Usages: lines [32]
     end note
     #palegreen:ASUP<
     note right: //(l:38)//
     note
         Usages: lines [34]
     end note
     stop
     #palegreen:ASUP-FIN<
     note right: //(l:40)//
     note
         Usages: lines [34]
     end note
     #palegreen:EXIT-GOBACK-PROGRAM<
     note right: //(l:41)//
     stop
     note
         &#42;&#42;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;

     end note
     #palegreen:APPEL-SP2000-DEB<
     note right: //(l:44)//
     note
         Usages: lines [27]
     end note
     #Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|
     note right: //(l:45)//
     #palegreen:APPEL-SP2000-FIN<
     note right: //(l:46)//
     note
         Usages: lines [27]
     end note
     stop
     note
         &#42;&#42;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;&#45;

     end note
    stop
    @enduml"""
            (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
            puml = generate_plantuml(instructions, labels)
            print(puml)
            self.same_string(puml, expected_puml)

    def test_parse_cobol_if_elseif_imbriques(self):
        code = """IDENTIFICATION DIVISION.
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
                   MOVE 0 TO DIFFDATE
                 ELSE
                   MOVE 1 TO DIFFDATE                
                   IF A1 NOT = A2
                         PERFORM         ASUP
                                         THRU
                                         ASUP-FIN
                   ELSE
                       MOVE 2 TO DIFFDATE
                   END-IF
                 END-IF
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
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<

note right: //(l:25)//

	:MOVE DAT1A-R TO LK-AAMMJJ6-ENT-2;

note right: //(l:26)//

	#Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN - voir ligne :49; <<procedure>>

note right: //(l:27)//

		if (__IF__ LK-CONV-1 NOT > LK-CONV-2 ?) then (yes)

note right: //(l:30)//

		:MOVE 0 TO DIFFDATE;

note right: //(l:31)//

		else (no)

note right: //(l:32)//

		:MOVE 1 TO DIFFDATE;

note right: //(l:33)//

			if (__IF__ A1 NOT = A2 ?) then (yes)

note right: //(l:34)//

			#Pink:ASUP -> ASUP-FIN - voir ligne :43; <<procedure>>

note right: //(l:35)//

			else (no)

note right: //(l:38)//

			:MOVE 2 TO DIFFDATE;

note right: //(l:39)//

			endif
		endif
	#palegreen:DEB-FIN<

note right: //(l:42)//

	#palegreen:ASUP<

note right: //(l:43)//

	note
					Usages: lines [35]
	end note
	stop
	#palegreen:ASUP-FIN<

note right: //(l:45)//

	note
					Usages: lines [35]
	end note
	#palegreen:EXIT-GOBACK-PROGRAM<

note right: //(l:46)//

	stop
	note
					.==========================================================
	end note
	#palegreen:APPEL-SP2000-DEB<

note right: //(l:49)//

	note
					Usages: lines [27]
	end note
	#Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|

note right: //(l:50)//

	#palegreen:APPEL-SP2000-FIN<

note right: //(l:51)//

	note
					Usages: lines [27]
	end note
	stop
	note
					.==========================================================
	end note
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

    def test_parse_cobol_if_imbriques_sans_else(self):
        code = """IDENTIFICATION DIVISION.
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
                   MOVE 0 TO DIFFDATE
                   IF A1 NOT = A2
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
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<

note right: //(l:25)//

	:MOVE DAT1A-R TO LK-AAMMJJ6-ENT-2;

note right: //(l:26)//

	#Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN - voir ligne :43; <<procedure>>

note right: //(l:27)//

		if (__IF__ LK-CONV-1 NOT > LK-CONV-2 ?) then (yes)

note right: //(l:30)//

		:MOVE 0 TO DIFFDATE;

note right: //(l:31)//

			if (__IF__ A1 NOT = A2 ?) then (yes)

note right: //(l:32)//

			#Pink:ASUP -> ASUP-FIN - voir ligne :37; <<procedure>>

note right: //(l:33)//

			endif
		endif
	#palegreen:DEB-FIN<

note right: //(l:36)//

	#palegreen:ASUP<

note right: //(l:37)//

	note
					Usages: lines [33]
	end note
	stop
	#palegreen:ASUP-FIN<

note right: //(l:39)//

	note
					Usages: lines [33]
	end note
	#palegreen:EXIT-GOBACK-PROGRAM<

note right: //(l:40)//

	stop
	note
					.==========================================================
	end note
	#palegreen:APPEL-SP2000-DEB<

note right: //(l:43)//

	note
					Usages: lines [27]
	end note
	#Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|

note right: //(l:44)//

	#palegreen:APPEL-SP2000-FIN<

note right: //(l:45)//

	note
					Usages: lines [27]
	end note
	stop
	note
					.==========================================================
	end note
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

    def test_parse_cobol_if_consecutifs(self):
        code = """IDENTIFICATION DIVISION.
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
                   MOVE 0 TO DIFFDATE
                   GO TO DEB-FIN.
                 IF A1 NOT = A2
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
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<

note right: //(l:25)//

	:MOVE DAT1A-R TO LK-AAMMJJ6-ENT-2;

note right: //(l:26)//

	#Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN - voir ligne :44; <<procedure>>

note right: //(l:27)//

		if (__IF__ LK-CONV-1 NOT > LK-CONV-2 ?) then (yes)

note right: //(l:30)//

		:MOVE 0 TO DIFFDATE;

note right: //(l:31)//

		#Pink:GO TO DEB-FIN - voir ligne //(l:37)//>
		detach
		endif
		if (__IF__ A1 NOT = A2 ?) then (yes)

note right: //(l:33)//

		#Pink:ASUP -> ASUP-FIN - voir ligne :38; <<procedure>>

note right: //(l:34)//

		endif
	#palegreen:DEB-FIN<

note right: //(l:37)//

	note
					Usages: lines [32]
	end note
	#palegreen:ASUP<

note right: //(l:38)//

	note
					Usages: lines [34]
	end note
	stop
	#palegreen:ASUP-FIN<

note right: //(l:40)//

	note
					Usages: lines [34]
	end note
	#palegreen:EXIT-GOBACK-PROGRAM<

note right: //(l:41)//

	stop
	note
					.==========================================================
	end note
	#palegreen:APPEL-SP2000-DEB<

note right: //(l:44)//

	note
					Usages: lines [27]
	end note
	#Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|

note right: //(l:45)//

	#palegreen:APPEL-SP2000-FIN<

note right: //(l:46)//

	note
					Usages: lines [27]
	end note
	stop
	note
					.==========================================================
	end note
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

    def test_parse_cobol_perform_multi_line_varying_commentaire(self):
        code = """IDENTIFICATION DIVISION.
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
             PROCEDURE DIVISION USING DIFF DTFLNK DTFINP DTFWRK DTFCOM.
             DEBUT SECTION.
             DEB.
                          PERFORM     APPEL-SP2000-DEB                
                             THRU                         
                             APPEL-SP2000-FIN VARYING I   
                  *           FROM 1 BY 1 UNTIL I > 2000   
                             FROM 1 BY 1 UNTIL I > WK-NBLIG-MAX-T2 
                  *** RED-633101 FIN 
                             OR WK-CPT-T2S (I) = ZERO.     
             DEB-FIN.
             EXIT-GOBACK-PROGRAM.
                 EXIT PROGRAM.
            **--------------------------------------------------------
             APPEL-SP2000-DEB.
                 CALL "SP2000"    USING ISP2000 DTFLNK DTFINP DTFWRK DTFCOM.
             APPEL-SP2000-FIN.
                 EXIT.
            **--------------------------------------------------------        
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<
	
note right: //(l:16)//

	while (__PERFORM__ [I] FROM [1] BY [1] WHILE [I > WK-NBLIG-MAX-T2 OR WK-CPT-T2S (I) = ZERO.]) is (true)
	
note right: //(l:17)//

		#Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN - voir ligne 28; <<procedure>>
		
note right: //(l:17)//

	endwhile
	#palegreen:DEB-FIN<
	
note right: //(l:24)//

	#palegreen:EXIT-GOBACK-PROGRAM<
	
note right: //(l:25)//

	stop
	note
					.==========================================================
	end note
	#palegreen:APPEL-SP2000-DEB<
	
note right: //(l:28)//

	note
					Usages: lines [17]
	end note
	#Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|
	
note right: //(l:29)//

	#palegreen:APPEL-SP2000-FIN<
	
note right: //(l:30)//

	note
					Usages: lines [17]
	end note
	stop
	note
					.==========================================================
	end note
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(expected_puml, puml)

    def test_parse_cobol_perform_multi_line_varying(self):
        code = """IDENTIFICATION DIVISION.
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
             PROCEDURE DIVISION USING DIFF DTFLNK DTFINP DTFWRK DTFCOM.
             DEBUT SECTION.
             DEB.
                       PERFORM     APPEL-SP2000-DEB                          
                                   THRU                                 
                                   APPEL-SP2000-FIN                        
                                   VARYING I-REEC FROM 1 BY 1           
                                   UNTIL I-REEC  > NB-MAXI-REEC.
             DEB-FIN.
             EXIT-GOBACK-PROGRAM.
                 EXIT PROGRAM.
            **--------------------------------------------------------
             APPEL-SP2000-DEB.
                 CALL "SP2000"    USING ISP2000 DTFLNK DTFINP DTFWRK DTFCOM.
             APPEL-SP2000-FIN.
                 EXIT.
            **--------------------------------------------------------        
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<
	
note right: //(l:16)//

	while (__PERFORM__ [I-REEC] FROM [1] BY [1] WHILE [I-REEC  > NB-MAXI-REEC.]) is (true)
	
note right: //(l:17)//

		#Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN - voir ligne 26; <<procedure>>
		
note right: //(l:17)//

	endwhile
	#palegreen:DEB-FIN<
	
note right: //(l:22)//

	#palegreen:EXIT-GOBACK-PROGRAM<
	
note right: //(l:23)//

	stop
	note
					.==========================================================
	end note
	#palegreen:APPEL-SP2000-DEB<
	
note right: //(l:26)//

	note
					Usages: lines [17]
	end note
	#Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|
	
note right: //(l:27)//

	#palegreen:APPEL-SP2000-FIN<
	
note right: //(l:28)//

	note
					Usages: lines [17]
	end note
	stop
	note
					.==========================================================
	end note
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(expected_puml, puml)

    def test_parse_cobol_one_line_call(self):
        code = """IDENTIFICATION DIVISION.
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
             PROCEDURE DIVISION USING DIFF DTFLNK DTFINP DTFWRK DTFCOM.
             DEBUT SECTION.
             DEB.
                         PERFORM         APPEL-SP2000-DEB
                                         THRU
                                         APPEL-SP2000-FIN.
             DEB-FIN.
             EXIT-GOBACK-PROGRAM.
                 EXIT PROGRAM.
            **--------------------------------------------------------
             APPEL-SP2000-DEB.
                 CALL "SP2000"    USING ISP2000 DTFLNK DTFINP DTFWRK DTFCOM.
             APPEL-SP2000-FIN.
                 EXIT.
            **--------------------------------------------------------        
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<

note right: //(l:14)//

	#Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN - voir ligne :22; <<procedure>>

note right: //(l:15)//

	#palegreen:DEB-FIN<

note right: //(l:18)//

	#palegreen:EXIT-GOBACK-PROGRAM<

note right: //(l:19)//

	stop
	note
					.==========================================================
	end note
	#palegreen:APPEL-SP2000-DEB<

note right: //(l:22)//

	note
					Usages: lines [15]
	end note
	#Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|

note right: //(l:23)//

	#palegreen:APPEL-SP2000-FIN<

note right: //(l:24)//

	note
					Usages: lines [15]
	end note
	stop
	note
					.==========================================================
	end note
stop
@enduml
"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

    def test_parse_cobol_simple_perform(self):
        code = """IDENTIFICATION DIVISION.
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
             PROCEDURE DIVISION USING DIFF DTFLNK DTFINP DTFWRK DTFCOM.
             DEBUT SECTION.
             DEB.
                         PERFORM         APPEL-SP2000-DEB
                                         THRU
                                         APPEL-SP2000-FIN.
             DEB-FIN.
             EXIT-GOBACK-PROGRAM.
                 EXIT PROGRAM.
            **----DEBUT--CALL--------------------------------------------------
             APPEL-SP2000-DEB.
                 CALL "SP2000"    USING ISP2000 DTFLNK DTFINP DTFWRK DTFCOM.
             APPEL-SP2000-FIN.
                 EXIT.
            **----FIN--CALL--------------------------------------------------        
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<
	
note right: //(l:16)//

	#Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN - voir ligne :24; <<procedure>>
	
note right: //(l:17)//

	#palegreen:DEB-FIN<
	
note right: //(l:20)//

	#palegreen:EXIT-GOBACK-PROGRAM<
	
note right: //(l:21)//

	stop
	note
					.======DEBUT==CALL==================================================
	end note
	#palegreen:APPEL-SP2000-DEB<
	
note right: //(l:24)//

	note
					Usages: lines [17]
	end note
	#Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|
	
note right: //(l:25)//

	#palegreen:APPEL-SP2000-FIN<
	
note right: //(l:26)//

	note
					Usages: lines [17]
	end note
	stop
	note
					.======FIN==CALL==================================================
	end note
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

    def test_parse_cobol_multiple_lines_call(self):
        code = """IDENTIFICATION DIVISION.
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
             PROCEDURE DIVISION USING DIFF DTFLNK DTFINP DTFWRK DTFCOM.
             DEBUT SECTION.
             DEB.
                         PERFORM         APPEL-SP2000-DEB
                                         THRU
                                         APPEL-SP2000-FIN.
             DEB-FIN.
             EXIT-GOBACK-PROGRAM.
                 EXIT PROGRAM.
            **--------------------------------------------------------
             APPEL-SP2000-DEB.
                 CALL    "ESMVT9"    USING   LNK-PDB
                                       DTFLNK
                                       DTFINP
                                       DTFWRK
                                       DTFCOM.
             APPEL-SP2000-FIN.
                 EXIT.
            **--------------------------------------------------------        
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<

note right: //(l:14)//

	#Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN - voir ligne :22; <<procedure>>

note right: //(l:15)//

	#palegreen:DEB-FIN<

note right: //(l:18)//

	#palegreen:EXIT-GOBACK-PROGRAM<

note right: //(l:19)//

	stop
	note
					.==========================================================
	end note
	#palegreen:APPEL-SP2000-DEB<

note right: //(l:22)//

	note
					Usages: lines [15]
	end note
	#Gold:CALL ESMVT9(LNK-PDB, DTFLNK, DTFINP, DTFWRK, DTFCOM)|

note right: //(l:23)//

	#palegreen:APPEL-SP2000-FIN<

note right: //(l:28)//

	note
					Usages: lines [15]
	end note
	stop
	note
					.==========================================================
	end note
stop
@enduml
"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

    def test_parse_cobol_3if_1else_imbriques(self):
        code = """IDENTIFICATION DIVISION.
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
    MOVE 1 TO VAR1
    IF DEBIT-SP < DEBIT-DIF
      MOVE 2 TO VAR1
     IF LNK-PDB-RET = "26"
       MOVE 3 TO VAR1
     ELSE
       MOVE 7 TO VAR1
       IF W-NUM-SP = 0
         MOVE 4 TO VAR1
       ELSE
         MOVE 5 TO VAR1
       END-IF
     END-IF
    ELSE
       MOVE 6 TO VAR1
    END-IF
    DEB-FIN.
    EXIT.
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<

note right: //(l:25)//

	:MOVE 1 TO VAR1;

note right: //(l:26)//

		if (__IF__ DEBIT-SP < DEBIT-DIF ?) then (yes)

note right: //(l:27)//

		:MOVE 2 TO VAR1;

note right: //(l:28)//

			if (__IF__ LNK-PDB-RET = "26" ?) then (yes)

note right: //(l:29)//

			:MOVE 3 TO VAR1;

note right: //(l:30)//

			else (no)

note right: //(l:31)//

			:MOVE 7 TO VAR1;

note right: //(l:32)//

				if (__IF__ W-NUM-SP = 0 ?) then (yes)

note right: //(l:33)//

				:MOVE 4 TO VAR1;

note right: //(l:34)//

				else (no)

note right: //(l:35)//

				:MOVE 5 TO VAR1;

note right: //(l:36)//

				endif
			endif
		else (no)

note right: //(l:39)//

		:MOVE 6 TO VAR1;

note right: //(l:40)//

		endif
	#palegreen:DEB-FIN<

note right: //(l:42)//

	stop
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

    def test_parse_cobol_if_else_plusieurs_lignes_commentaire(self):
        code = """IDENTIFICATION DIVISION.
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
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        # num de lignes checkés le 05/NOV/24
        expected_puml += """start
	#palegreen:DEB<

note right: //(l:25)//

	:MOVE DAT1A-R TO LK-AAMMJJ6-ENT-2;

note right: //(l:26)//

	#Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN - voir ligne :46; <<procedure>>

note right: //(l:27)//

		if (__IF__ LK-CONV-1 NOT > LK-CONV-2\\nOR LK-CONV-1 NOT = LK-CONV-2 ?) then (yes)

note right: //(l:30)//

		:MOVE 0 TO DIFFDATE;

note right: //(l:33)//

		#Pink:GO TO DEB-FIN - voir ligne //(l:39)//>
		detach
		else (no)

note right: //(l:35)//

		#Pink:ASUP -> ASUP-FIN - voir ligne :40; <<procedure>>

note right: //(l:36)//

		endif
	#palegreen:DEB-FIN<

note right: //(l:39)//

	note
					Usages: lines [34]
	end note
	#palegreen:ASUP<

note right: //(l:40)//

	note
					Usages: lines [36]
	end note
	stop
	#palegreen:ASUP-FIN<

note right: //(l:42)//

	note
					Usages: lines [36]
	end note
	#palegreen:EXIT-GOBACK-PROGRAM<

note right: //(l:43)//

	stop
	note
					.==========================================================
	end note
	#palegreen:APPEL-SP2000-DEB<

note right: //(l:46)//

	note
					Usages: lines [27]
	end note
	#Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|

note right: //(l:47)//

	#palegreen:APPEL-SP2000-FIN<

note right: //(l:48)//

	note
					Usages: lines [27]
	end note
	stop
	note
					.==========================================================
	end note
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

    def test_parse_cobol_if_perform(self):
        code = """IDENTIFICATION DIVISION.
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
    MOVE 1 TO VAR1
    IF RUPT-DT-JC < W-DT-JC-DEB                            
        MOVE TRI-PER-REP TO RUPT-PER-REP                    
        PERFORM     INITIAL-RUPT-TECR                    
                       THRU                                 
                       F-INITIAL-RUPT-TECR                  
        MOVE TRI-PER-REP TO RUPT-PER-REP                    
        GO TO BC-RET-ECRI                                   
    END-IF
    DEB-FIN.
    BC-RET-ECRI.
    INITIAL-RUPT-TECR.
    F-INITIAL-RUPT-TECR.
    EXIT.
        """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<
	
note right: //(l:25)//

	:MOVE 1 TO VAR1;
	
note right: //(l:26)//

		if (__IF__ RUPT-DT-JC < W-DT-JC-DEB ?) then (yes)
						
note right: //(l:27)//

		:MOVE TRI-PER-REP TO RUPT-PER-REP;
		
note right: //(l:28)//

		#Pink:INITIAL-RUPT-TECR -> F-INITIAL-RUPT-TECR - voir ligne :37; <<procedure>>
		
note right: //(l:29)//

		:MOVE TRI-PER-REP TO RUPT-PER-REP;
		
note right: //(l:32)//

		#Pink:GO TO BC-RET-ECRI - voir ligne //(l:36)//> 
		detach
		endif
	#palegreen:DEB-FIN<
	
note right: //(l:35)//

	#palegreen:BC-RET-ECRI<
	
note right: //(l:36)//

	note
					Usages: lines [33]
	end note
	#palegreen:INITIAL-RUPT-TECR<
	
note right: //(l:37)//

	note
					Usages: lines [29]
	end note
	#palegreen:F-INITIAL-RUPT-TECR<
	
note right: //(l:38)//

	note
					Usages: lines [29]
	end note
	stop
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

    def test_parse_cobol_goto(self):
        code = """IDENTIFICATION DIVISION.
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
                     PROCEDURE DIVISION USING DIFF DTFLNK DTFINP DTFWRK DTFCOM.
                     DEBUT SECTION.
                     DEB.
                          GO TO APPEL-SP2000-DEB.
                     DEB-FIN.
                     EXIT-GOBACK-PROGRAM.
                         EXIT PROGRAM.
                    **--------------------------------------------------------
                     APPEL-SP2000-DEB.
                         CALL "SP2000"    USING ISP2000 DTFLNK DTFINP DTFWRK DTFCOM.
                     APPEL-SP2000-FIN.
                         EXIT.
                    **--------------------------------------------------------        
                """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<

note right: //(l:14)//

	#Pink:GO TO APPEL-SP2000-DEB - voir ligne //(l:20)//>
	detach
	#palegreen:DEB-FIN<

note right: //(l:16)//

	#palegreen:EXIT-GOBACK-PROGRAM<

note right: //(l:17)//

	stop
	note
					.==========================================================
	end note
	#palegreen:APPEL-SP2000-DEB<

note right: //(l:20)//

	note
					Usages: lines [15]
	end note
	#Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|

note right: //(l:21)//

	#palegreen:APPEL-SP2000-FIN<

note right: //(l:22)//

	stop
	note
					.==========================================================
	end note
stop
@enduml"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)

    def test_parse_cobol_perform_thru(self):
        code = """IDENTIFICATION DIVISION.
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
                     PROCEDURE DIVISION USING DIFF DTFLNK DTFINP DTFWRK DTFCOM.
                     DEBUT SECTION.
                     DEB.
                       PERFORM     APPEL-SP2000-DEB                          
                                   THRU                                 
                                   APPEL-SP2000-FIN                        
                     DEB-FIN.
                     EXIT-GOBACK-PROGRAM.
                         EXIT PROGRAM.
                    **--------------------------------------------------------
                     APPEL-SP2000-DEB.
                         CALL "SP2000"    USING ISP2000 DTFLNK DTFINP DTFWRK DTFCOM.
                     APPEL-SP2000-FIN.
                     EXIT.
                    **--------------------------------------------------------        
                """
        expected_puml = "@startuml\n" + get_plantuml_style()
        expected_puml += """start
	#palegreen:DEB<
	
note right: //(l:14)//

	#Pink:APPEL-SP2000-DEB -> APPEL-SP2000-FIN - voir ligne :22; <<procedure>>
	
note right: //(l:15)//

	#palegreen:EXIT-GOBACK-PROGRAM<
	
note right: //(l:19)//

	stop
	note
					.==========================================================
	end note
	#palegreen:APPEL-SP2000-DEB<
	
note right: //(l:22)//

	note
					Usages: lines [15]
	end note
	#Gold:CALL SP2000(ISP2000, DTFLNK, DTFINP, DTFWRK, DTFCOM)|
	
note right: //(l:23)//

	#palegreen:APPEL-SP2000-FIN<
	
note right: //(l:24)//

	note
					Usages: lines [15]
	end note
	stop
	note
					.==========================================================
	end note
stop
@enduml
"""
        (instructions, labels) = parse_cobol(code, 1, [], {}, {}, 0)
        puml = generate_plantuml(instructions, labels)
        print(puml)
        self.same_string(puml, expected_puml)
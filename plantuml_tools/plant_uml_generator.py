import subprocess


def plant_uml_png_generator(plantuml_code: str, file_name: str):
    """
    Génération d'un PNG grâce au jar de plantuml.
    Le jar est dispo ici : https://github.com/plantuml/plantuml/releases/tag/v1.2024.6
    les versions sont ici : https://github.com/plantuml/plantuml/tags
    le projet est ici : https://github.com/plantuml/plantuml
    """
    # Écrire le diagramme dans un fichier temporaire
    with open(file_name+'.txt', 'w') as file:
        file.write(plantuml_code)

    # Générer le fichier PNG en utilisant PlantUML localement
    subprocess.run(['java', '-Xms4g', '-Xmx6g', '-DPLANTUML_LIMIT_SIZE=20000000', '-jar', f'C:\dev\codesnap\couverture\plantuml_tools\plantuml-gplv2-1.2024.8.jar', '-tpng', file_name+'.txt'])
    print(file_name+".png généré")


def plant_uml_svg_generator(plantuml_code: str, file_name: str):
    """
    Génération d'un SVG grâce au jar de plantuml.
    Le jar est dispo ici : https://github.com/plantuml/plantuml/releases/tag/v1.2024.6
    les versions sont ici : https://github.com/plantuml/plantuml/tags
    le projet est ici : https://github.com/plantuml/plantuml
        # Exemple d'utilisation
        plant_uml_svg_generator("@startuml\nAlice -> Bob: Test\n@enduml", "diagram")
    """
    # Écrire le diagramme dans un fichier temporaire
    with open(file_name+'.txt', 'w') as file:
        file.write(plantuml_code)

    # Générer le fichier SVG en utilisant PlantUML localement
    subprocess.run(['java', '-Xms4g', '-Xmx6g', '-DPLANTUML_LIMIT_SIZE=8000000', '-jar', f'C:\dev\codesnap\couverture\plantuml_tools\plantuml-gplv2-1.2024.8.jar', '-tsvg', file_name+'.txt'])
    print(file_name+".svg généré")


gantt = """

    @startgantt
    ' Planning des activité passées généré par le script http://gitlab.altair.recouv/sded/coeur-de-metier-recouvrement/gestion-des-comptes-entreprises-du-rg/dsn/e2e-test/jdd/-/blob/master/couverture/extract_gantt_avalconso.sh
    printscale weekly zoom 4
    hide ressources footbox    
    Project starts the 2024/08/15
    saturday are closed
    sunday are closed
    
    2024/05/01 is colored in Lavender/LightBlue 
    2024/05/08 is colored in Lavender/LightBlue 
    2024/05/09 is colored in Lavender/LightBlue 
    2024/05/20 is colored in Lavender/LightBlue 
    2024/07/14 is colored in Lavender/LightBlue 
    
    [ULGCE - Sprint 38 19/08-23/08]  is colored in Coral/Green and starts on 2024/08/19  and ends on 2024/08/23
[ULGCE - Sprint 39 26/08-30/08]  is colored in Coral/Green and starts on 2024/08/26  and ends on 2024/08/30
[ULGCE - Sprint 40 02/09-06/09]  is colored in Coral/Green and starts on 2024/09/02  and ends on 2024/09/06
[ULGCE - Sprint 41 09/09-13/10]  is colored in Coral/Green and starts on 2024/09/09  and ends on 2024/09/13

[ULGCE - Sprint 38 19/08-23/08] -> [ULGCE - Sprint 39 26/08-30/08]
[ULGCE - Sprint 39 26/08-30/08] -> [ULGCE - Sprint 40 02/09-06/09]
[ULGCE - Sprint 40 02/09-06/09] -> [ULGCE - Sprint 41 09/09-13/10]

    2024-09-06 is colored in GreenYellow/GreenYellow 
    -- ULGCE-1136 - Pilotage Resp Réalisation --
[ULGCE-1136 - création] happens on 2024/02/06
[ULGCE-1136 - création] -> [ULGCE-1263 Pilotage Resp Réalisation - création]
[ULGCE-1263 Pilotage Resp Réalisation - création] happens at 2024/02/28
[ULGCE-1263 Pilotage Resp Réalisation - création] -> [ULGCE-1263 (En Test)-1]
[ULGCE-1263 (En Test)-1] starts 2024/02/28 and ends 2024/02/29
[ULGCE-1263 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1263 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1263]]
[ULGCE-1263 (Terminée)-2] starts 2024/02/29 and ends 2024/05/16
[ULGCE-1263 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1263 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1263]]
[ULGCE-1263 (En Test)-1] -> [ULGCE-1263 (Terminée)-2]
[ULGCE-1263 (A faire)-3] starts 2024/05/16 and ends 2024/05/16
[ULGCE-1263 (A faire)-3] is colored in LightGray/Gray
[ULGCE-1263 (A faire)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1263]]
[ULGCE-1263 (Terminée)-2] -> [ULGCE-1263 (A faire)-3]
[ULGCE-1263 (En Test)-4] starts 2024/05/16 and ends 2024/05/16
[ULGCE-1263 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1263 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1263]]
[ULGCE-1263 (A faire)-3] -> [ULGCE-1263 (En Test)-4]
[ULGCE-1136 - création] -> [ULGCE-1231 Pilotage Resp Réalisation - création]
[ULGCE-1231 Pilotage Resp Réalisation - création] happens at 2024/02/21
[ULGCE-1231 Pilotage Resp Réalisation - création] -> [ULGCE-1231 (En Test)-1]
[ULGCE-1231 (En Test)-1] starts 2024/02/21 and ends 2024/02/21
[ULGCE-1231 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1231 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1231]]
[ULGCE-1231 (Terminée)-2] starts 2024/02/21 and ends 2024/02/25
[ULGCE-1231 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1231 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1231]]
[ULGCE-1231 (En Test)-1] -> [ULGCE-1231 (Terminée)-2]
[ULGCE-1136 - création] -> [ULGCE-1190 Pilotage Resp Réalisation - création]
[ULGCE-1190 Pilotage Resp Réalisation - création] happens at 2024/02/13
[ULGCE-1190 Pilotage Resp Réalisation - création] -> [ULGCE-1190 (En Test)-1]
[ULGCE-1190 (En Test)-1] starts 2024/02/13 and ends 2024/02/13
[ULGCE-1190 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1190 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1190]]
[ULGCE-1190 (Terminée)-2] starts 2024/02/13 and ends 2024/02/18
[ULGCE-1190 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1190 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1190]]
[ULGCE-1190 (En Test)-1] -> [ULGCE-1190 (Terminée)-2]
[ULGCE-1136 - création] -> [ULGCE-1137 Pilotage Resp Réalisation - création]
[ULGCE-1137 Pilotage Resp Réalisation - création] happens at 2024/02/06
[ULGCE-1137 Pilotage Resp Réalisation - création] -> [ULGCE-1137 (IN PROGRESS)-1]
[ULGCE-1137 (IN PROGRESS)-1] starts 2024/02/06 and ends 2024/02/06
[ULGCE-1137 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1137 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1137]]
[ULGCE-1137 (En Attente)-2] starts 2024/02/06 and ends 2024/02/10
[ULGCE-1137 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1137 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1137]]
[ULGCE-1137 (IN PROGRESS)-1] -> [ULGCE-1137 (En Attente)-2]
[ULGCE-1137 (A faire)-3] starts 2024/02/10 and ends 2024/02/10
[ULGCE-1137 (A faire)-3] is colored in LightGray/Gray
[ULGCE-1137 (A faire)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1137]]
[ULGCE-1137 (En Attente)-2] -> [ULGCE-1137 (A faire)-3]
[ULGCE-1137 (En Test)-4] starts 2024/02/10 and ends 2024/02/10
[ULGCE-1137 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1137 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1137]]
[ULGCE-1137 (A faire)-3] -> [ULGCE-1137 (En Test)-4]
[ULGCE-1137 (Terminée)-5] starts 2024/02/10 and ends 2024/02/10
[ULGCE-1137 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1137 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1137]]
[ULGCE-1137 (En Test)-4] -> [ULGCE-1137 (Terminée)-5]
-- ULGCE-1147 - ACADEMIE V2  --
[ULGCE-1147 - création] happens on 2024/02/07
[ULGCE-1147 - création] -> [ULGCE-2150 ACADEMIE V2  - création]
[ULGCE-2150 ACADEMIE V2  - création] happens at 2024/09/06
[ULGCE-1147 - création] -> [ULGCE-2149 ACADEMIE V2  - création]
[ULGCE-2149 ACADEMIE V2  - création] happens at 2024/09/06
[ULGCE-1147 - création] -> [ULGCE-2148 ACADEMIE V2  - création]
[ULGCE-2148 ACADEMIE V2  - création] happens at 2024/09/06
[ULGCE-1147 - création] -> [ULGCE-1423 ACADEMIE V2  - création]
[ULGCE-1423 ACADEMIE V2  - création] happens at 2024/03/25
[ULGCE-1423 ACADEMIE V2  - création] -> [ULGCE-1423 (IN PROGRESS)-1]
[ULGCE-1423 (IN PROGRESS)-1] starts 2024/03/25 and ends 2024/03/25
[ULGCE-1423 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1423 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1423]]
[ULGCE-1423 (En Test)-2] starts 2024/03/25 and ends 2024/03/25
[ULGCE-1423 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1423 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1423]]
[ULGCE-1423 (IN PROGRESS)-1] -> [ULGCE-1423 (En Test)-2]
[ULGCE-1423 (Terminée)-3] starts 2024/03/25 and ends 2024/03/25
[ULGCE-1423 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1423 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1423]]
[ULGCE-1423 (En Test)-2] -> [ULGCE-1423 (Terminée)-3]
[ULGCE-1147 - création] -> [ULGCE-1420 ACADEMIE V2  - création]
[ULGCE-1420 ACADEMIE V2  - création] happens at 2024/03/25
[ULGCE-1420 ACADEMIE V2  - création] -> [ULGCE-1420 (IN PROGRESS)-1]
[ULGCE-1420 (IN PROGRESS)-1] starts 2024/03/25 and ends 2024/03/25
[ULGCE-1420 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1420 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1420]]
[ULGCE-1147 - création] -> [ULGCE-1148 ACADEMIE V2  - création]
[ULGCE-1148 ACADEMIE V2  - création] happens at 2024/02/07
[ULGCE-1148 ACADEMIE V2  - création] -> [ULGCE-1148 (IN PROGRESS)-1]
[ULGCE-1148 (IN PROGRESS)-1] starts 2024/02/07 and ends 2024/02/07
[ULGCE-1148 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1148 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1148]]
[ULGCE-1148 (En Attente)-2] starts 2024/02/07 and ends 2024/02/09
[ULGCE-1148 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1148 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1148]]
[ULGCE-1148 (IN PROGRESS)-1] -> [ULGCE-1148 (En Attente)-2]
-- ULGCE-1208 - [#925468] P22-391 Affectation des paiements Partiel  RACINE --
[ULGCE-1208 - création] happens on 2024/02/15
[ULGCE-1208 - création] -> [ULGCE-1983 #925468_927438_PROJET_TESTS - création]
[ULGCE-1983 #925468_927438_PROJET_TESTS - création] happens at 2024/07/18
[ULGCE-1983 #925468_927438_PROJET_TESTS - création] -> [ULGCE-1983 (IN PROGRESS)-1]
[ULGCE-1983 (IN PROGRESS)-1] starts 2024/07/18 and ends 2024/07/18
[ULGCE-1983 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1983 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1983]]
[ULGCE-1983 (En Test)-2] starts 2024/07/18 and ends 2024/08/02
[ULGCE-1983 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1983 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1983]]
[ULGCE-1983 (IN PROGRESS)-1] -> [ULGCE-1983 (En Test)-2]
[ULGCE-1983 (Terminée)-3] starts 2024/08/02 and ends 2024/08/02
[ULGCE-1983 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1983 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1983]]
[ULGCE-1983 (En Test)-2] -> [ULGCE-1983 (Terminée)-3]
[ULGCE-1208 - création] -> [ULGCE-1982 #925468_927438_PROJET_DEVELOPPEMENT - création]
[ULGCE-1982 #925468_927438_PROJET_DEVELOPPEMENT - création] happens at 2024/07/18
[ULGCE-1982 #925468_927438_PROJET_DEVELOPPEMENT - création] -> [ULGCE-1982 (IN PROGRESS)-1]
[ULGCE-1982 (IN PROGRESS)-1] starts 2024/07/18 and ends 2024/07/18
[ULGCE-1982 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1982 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1982]]
[ULGCE-1982 (En Test)-2] starts 2024/07/18 and ends 2024/08/02
[ULGCE-1982 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1982 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1982]]
[ULGCE-1982 (IN PROGRESS)-1] -> [ULGCE-1982 (En Test)-2]
[ULGCE-1982 (Terminée)-3] starts 2024/08/02 and ends 2024/08/02
[ULGCE-1982 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1982 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1982]]
[ULGCE-1982 (En Test)-2] -> [ULGCE-1982 (Terminée)-3]
[ULGCE-1208 - création] -> [ULGCE-1953 #925468_927438_PROJET_TESTS - création]
[ULGCE-1953 #925468_927438_PROJET_TESTS - création] happens at 2024/07/08
[ULGCE-1953 #925468_927438_PROJET_TESTS - création] -> [ULGCE-1953 (En Test)-1]
[ULGCE-1953 (En Test)-1] starts 2024/07/08 and ends 2024/07/08
[ULGCE-1953 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1953 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1953]]
[ULGCE-1953 (Terminée)-2] starts 2024/07/08 and ends 2024/08/23
[ULGCE-1953 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1953 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1953]]
[ULGCE-1953 (En Test)-1] -> [ULGCE-1953 (Terminée)-2]
[ULGCE-1208 - création] -> [ULGCE-1520 #925468_927438_PROJET_TESTS - création]
[ULGCE-1520 #925468_927438_PROJET_TESTS - création] happens at 2024/04/08
[ULGCE-1520 #925468_927438_PROJET_TESTS - création] -> [ULGCE-1520 (En Test)-1]
[ULGCE-1520 (En Test)-1] starts 2024/04/08 and ends 2024/06/28
[ULGCE-1520 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1520 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1520]]
[ULGCE-1208 - création] -> [ULGCE-1519 #925468_927438_PROJET_STRATEGIE DE TESTS - création]
[ULGCE-1519 #925468_927438_PROJET_STRATEGIE DE TESTS - création] happens at 2024/04/08
[ULGCE-1519 #925468_927438_PROJET_STRATEGIE DE TESTS - création] -> [ULGCE-1519 (IN PROGRESS)-1]
[ULGCE-1519 (IN PROGRESS)-1] starts 2024/04/08 and ends 2024/04/19
[ULGCE-1519 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1519 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1519]]
[ULGCE-1519 (En Attente)-2] starts 2024/04/19 and ends 2024/04/19
[ULGCE-1519 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1519 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1519]]
[ULGCE-1519 (IN PROGRESS)-1] -> [ULGCE-1519 (En Attente)-2]
[ULGCE-1519 (IN PROGRESS)-3] starts 2024/04/19 and ends 2024/07/08
[ULGCE-1519 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1519 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1519]]
[ULGCE-1519 (En Attente)-2] -> [ULGCE-1519 (IN PROGRESS)-3]
[ULGCE-1519 (En Attente)-4] starts 2024/07/08 and ends 2024/07/12
[ULGCE-1519 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1519 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1519]]
[ULGCE-1519 (IN PROGRESS)-3] -> [ULGCE-1519 (En Attente)-4]
[ULGCE-1519 (IN PROGRESS)-5] starts 2024/07/12 and ends 2024/08/02
[ULGCE-1519 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1519 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1519]]
[ULGCE-1519 (En Attente)-4] -> [ULGCE-1519 (IN PROGRESS)-5]
[ULGCE-1519 (En Test)-6] starts 2024/08/02 and ends 2024/08/02
[ULGCE-1519 (En Test)-6] is colored in LightGray/Gray
[ULGCE-1519 (En Test)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1519]]
[ULGCE-1519 (IN PROGRESS)-5] -> [ULGCE-1519 (En Test)-6]
[ULGCE-1519 (Terminée)-7] starts 2024/08/02 and ends 2024/08/02
[ULGCE-1519 (Terminée)-7] is colored in LightGray/Gray
[ULGCE-1519 (Terminée)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1519]]
[ULGCE-1519 (En Test)-6] -> [ULGCE-1519 (Terminée)-7]
[ULGCE-1208 - création] -> [ULGCE-1518 #925468_927438_PROJET_DEVELOPPEMENT - création]
[ULGCE-1518 #925468_927438_PROJET_DEVELOPPEMENT - création] happens at 2024/04/08
[ULGCE-1518 #925468_927438_PROJET_DEVELOPPEMENT - création] -> [ULGCE-1518 (En Test)-1]
[ULGCE-1518 (En Test)-1] starts 2024/04/08 and ends 2024/04/11
[ULGCE-1518 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1518 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1518]]
[ULGCE-1518 (A faire)-2] starts 2024/04/11 and ends 2024/04/11
[ULGCE-1518 (A faire)-2] is colored in LightGray/Gray
[ULGCE-1518 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1518]]
[ULGCE-1518 (En Test)-1] -> [ULGCE-1518 (A faire)-2]
[ULGCE-1518 (IN PROGRESS)-3] starts 2024/04/11 and ends 2024/04/11
[ULGCE-1518 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1518 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1518]]
[ULGCE-1518 (A faire)-2] -> [ULGCE-1518 (IN PROGRESS)-3]
[ULGCE-1518 (En Attente)-4] starts 2024/04/11 and ends 2024/06/11
[ULGCE-1518 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1518 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1518]]
[ULGCE-1518 (IN PROGRESS)-3] -> [ULGCE-1518 (En Attente)-4]
[ULGCE-1518 (IN PROGRESS)-5] starts 2024/06/11 and ends 2024/06/11
[ULGCE-1518 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1518 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1518]]
[ULGCE-1518 (En Attente)-4] -> [ULGCE-1518 (IN PROGRESS)-5]
[ULGCE-1208 - création] -> [ULGCE-1371 #925468_927438_PROJET_CONCEPTION TECHNIQUE - création]
[ULGCE-1371 #925468_927438_PROJET_CONCEPTION TECHNIQUE - création] happens at 2024/03/16
[ULGCE-1371 #925468_927438_PROJET_CONCEPTION TECHNIQUE - création] -> [ULGCE-1371 (IN PROGRESS)-1]
[ULGCE-1371 (IN PROGRESS)-1] starts 2024/03/16 and ends 2024/04/11
[ULGCE-1371 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1371 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1371]]
[ULGCE-1371 (En Attente)-2] starts 2024/04/11 and ends 2024/04/19
[ULGCE-1371 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1371 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1371]]
[ULGCE-1371 (IN PROGRESS)-1] -> [ULGCE-1371 (En Attente)-2]
[ULGCE-1371 (IN PROGRESS)-3] starts 2024/04/19 and ends 2024/06/05
[ULGCE-1371 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1371 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1371]]
[ULGCE-1371 (En Attente)-2] -> [ULGCE-1371 (IN PROGRESS)-3]
[ULGCE-1371 (En Test)-4] starts 2024/06/05 and ends 2024/06/07
[ULGCE-1371 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1371 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1371]]
[ULGCE-1371 (IN PROGRESS)-3] -> [ULGCE-1371 (En Test)-4]
[ULGCE-1371 (Terminée)-5] starts 2024/06/07 and ends 2024/06/07
[ULGCE-1371 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1371 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1371]]
[ULGCE-1371 (En Test)-4] -> [ULGCE-1371 (Terminée)-5]
[ULGCE-1208 - création] -> [ULGCE-1370 #925468_927438_PROJET_CONCEPTION TECHNIQUE - création]
[ULGCE-1370 #925468_927438_PROJET_CONCEPTION TECHNIQUE - création] happens at 2024/03/16
[ULGCE-1370 #925468_927438_PROJET_CONCEPTION TECHNIQUE - création] -> [ULGCE-1370 (IN PROGRESS)-1]
[ULGCE-1370 (IN PROGRESS)-1] starts 2024/03/16 and ends 2024/05/07
[ULGCE-1370 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1370 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1370]]
[ULGCE-1370 (En Attente)-2] starts 2024/05/07 and ends 2024/06/11
[ULGCE-1370 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1370 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1370]]
[ULGCE-1370 (IN PROGRESS)-1] -> [ULGCE-1370 (En Attente)-2]
[ULGCE-1370 (IN PROGRESS)-3] starts 2024/06/11 and ends 2024/06/11
[ULGCE-1370 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1370 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1370]]
[ULGCE-1370 (En Attente)-2] -> [ULGCE-1370 (IN PROGRESS)-3]
[ULGCE-1208 - création] -> [ULGCE-1283 #925468_927438_PROJET_DEVIS - création]
[ULGCE-1283 #925468_927438_PROJET_DEVIS - création] happens at 2024/03/02
[ULGCE-1283 #925468_927438_PROJET_DEVIS - création] -> [ULGCE-1283 (IN PROGRESS)-1]
[ULGCE-1283 (IN PROGRESS)-1] starts 2024/03/02 and ends 2024/03/06
[ULGCE-1283 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1283 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1283]]
[ULGCE-1283 (En Test)-2] starts 2024/03/06 and ends 2024/03/08
[ULGCE-1283 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1283 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1283]]
[ULGCE-1283 (IN PROGRESS)-1] -> [ULGCE-1283 (En Test)-2]
[ULGCE-1283 (Terminée)-3] starts 2024/03/08 and ends 2024/03/11
[ULGCE-1283 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1283 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1283]]
[ULGCE-1283 (En Test)-2] -> [ULGCE-1283 (Terminée)-3]
-- ULGCE-126 - Support collaborateurs --
[ULGCE-126 - création] happens on 2023/11/06
[ULGCE-126 - création] -> [ULGCE-1264 Support collaborateur - création]
[ULGCE-1264 Support collaborateur - création] happens at 2024/02/28
[ULGCE-1264 Support collaborateur - création] -> [ULGCE-1264 (En Test)-1]
[ULGCE-1264 (En Test)-1] starts 2024/02/28 and ends 2024/02/29
[ULGCE-1264 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1264 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1264]]
[ULGCE-126 - création] -> [ULGCE-1178 Support collaborateur - création]
[ULGCE-1178 Support collaborateur - création] happens at 2024/02/10
[ULGCE-1178 Support collaborateur - création] -> [ULGCE-1178 (En Test)-1]
[ULGCE-1178 (En Test)-1] starts 2024/02/10 and ends 2024/02/25
[ULGCE-1178 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1178 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1178]]
[ULGCE-1178 (Terminée)-2] starts 2024/02/25 and ends 2024/02/25
[ULGCE-1178 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1178 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1178]]
[ULGCE-1178 (En Test)-1] -> [ULGCE-1178 (Terminée)-2]
[ULGCE-126 - création] -> [ULGCE-1177 Support collaborateur - création]
[ULGCE-1177 Support collaborateur - création] happens at 2024/02/10
[ULGCE-1177 Support collaborateur - création] -> [ULGCE-1177 (IN PROGRESS)-1]
[ULGCE-1177 (IN PROGRESS)-1] starts 2024/02/10 and ends 2024/02/12
[ULGCE-1177 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1177 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1177]]
[ULGCE-1177 (En Attente)-2] starts 2024/02/12 and ends 2024/02/12
[ULGCE-1177 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1177 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1177]]
[ULGCE-1177 (IN PROGRESS)-1] -> [ULGCE-1177 (En Attente)-2]
[ULGCE-1177 (IN PROGRESS)-3] starts 2024/02/12 and ends 2024/02/21
[ULGCE-1177 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1177 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1177]]
[ULGCE-1177 (En Attente)-2] -> [ULGCE-1177 (IN PROGRESS)-3]
[ULGCE-1177 (En Attente)-4] starts 2024/02/21 and ends 2024/02/21
[ULGCE-1177 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1177 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1177]]
[ULGCE-1177 (IN PROGRESS)-3] -> [ULGCE-1177 (En Attente)-4]
[ULGCE-1177 (IN PROGRESS)-5] starts 2024/02/21 and ends 2024/04/16
[ULGCE-1177 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1177 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1177]]
[ULGCE-1177 (En Attente)-4] -> [ULGCE-1177 (IN PROGRESS)-5]
[ULGCE-1177 (En Attente)-6] starts 2024/04/16 and ends 2024/04/18
[ULGCE-1177 (En Attente)-6] is colored in LightGray/Gray
[ULGCE-1177 (En Attente)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1177]]
[ULGCE-1177 (IN PROGRESS)-5] -> [ULGCE-1177 (En Attente)-6]
[ULGCE-126 - création] -> [ULGCE-1176 Support collaborateur - création]
[ULGCE-1176 Support collaborateur - création] happens at 2024/02/10
[ULGCE-1176 Support collaborateur - création] -> [ULGCE-1176 (IN PROGRESS)-1]
[ULGCE-1176 (IN PROGRESS)-1] starts 2024/02/10 and ends 2024/02/15
[ULGCE-1176 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1176 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1176]]
[ULGCE-1176 (En Attente)-2] starts 2024/02/15 and ends 2024/03/07
[ULGCE-1176 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1176 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1176]]
[ULGCE-1176 (IN PROGRESS)-1] -> [ULGCE-1176 (En Attente)-2]
[ULGCE-1176 (IN PROGRESS)-3] starts 2024/03/07 and ends 2024/03/07
[ULGCE-1176 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1176 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1176]]
[ULGCE-1176 (En Attente)-2] -> [ULGCE-1176 (IN PROGRESS)-3]
[ULGCE-1176 (En Attente)-4] starts 2024/03/07 and ends 2024/03/28
[ULGCE-1176 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1176 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1176]]
[ULGCE-1176 (IN PROGRESS)-3] -> [ULGCE-1176 (En Attente)-4]
[ULGCE-1176 (IN PROGRESS)-5] starts 2024/03/28 and ends 2024/08/29
[ULGCE-1176 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1176 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1176]]
[ULGCE-1176 (En Attente)-4] -> [ULGCE-1176 (IN PROGRESS)-5]
[ULGCE-1176 (En Attente)-6] starts 2024/08/29 and ends 2024/08/29
[ULGCE-1176 (En Attente)-6] is colored in LightGray/Gray
[ULGCE-1176 (En Attente)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1176]]
[ULGCE-1176 (IN PROGRESS)-5] -> [ULGCE-1176 (En Attente)-6]
[ULGCE-126 - création] -> [ULGCE-1138 Support collaborateur - création]
[ULGCE-1138 Support collaborateur - création] happens at 2024/02/06
[ULGCE-1138 Support collaborateur - création] -> [ULGCE-1138 (En Test)-1]
[ULGCE-1138 (En Test)-1] starts 2024/02/06 and ends 2024/02/10
[ULGCE-1138 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1138 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1138]]
[ULGCE-1138 (Terminée)-2] starts 2024/02/10 and ends 2024/02/10
[ULGCE-1138 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1138 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1138]]
[ULGCE-1138 (En Test)-1] -> [ULGCE-1138 (Terminée)-2]
[ULGCE-126 - création] -> [ULGCE-1014 Support collaborateur - création]
[ULGCE-1014 Support collaborateur - création] happens at 2024/01/17
[ULGCE-1014 Support collaborateur - création] -> [ULGCE-1014 (En Test)-1]
[ULGCE-1014 (En Test)-1] starts 2024/01/17 and ends 2024/01/30
[ULGCE-1014 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1014 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1014]]
[ULGCE-1014 (A faire)-2] starts 2024/01/30 and ends 2024/01/31
[ULGCE-1014 (A faire)-2] is colored in LightGray/Gray
[ULGCE-1014 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1014]]
[ULGCE-1014 (En Test)-1] -> [ULGCE-1014 (A faire)-2]
[ULGCE-1014 (IN PROGRESS)-3] starts 2024/01/31 and ends 2024/01/31
[ULGCE-1014 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1014 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1014]]
[ULGCE-1014 (A faire)-2] -> [ULGCE-1014 (IN PROGRESS)-3]
[ULGCE-1014 (En Attente)-4] starts 2024/01/31 and ends 2024/01/31
[ULGCE-1014 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1014 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1014]]
[ULGCE-1014 (IN PROGRESS)-3] -> [ULGCE-1014 (En Attente)-4]
[ULGCE-1014 (IN PROGRESS)-5] starts 2024/01/31 and ends 2024/02/08
[ULGCE-1014 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1014 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1014]]
[ULGCE-1014 (En Attente)-4] -> [ULGCE-1014 (IN PROGRESS)-5]
[ULGCE-1014 (En Attente)-6] starts 2024/02/08 and ends 2024/02/09
[ULGCE-1014 (En Attente)-6] is colored in LightGray/Gray
[ULGCE-1014 (En Attente)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1014]]
[ULGCE-1014 (IN PROGRESS)-5] -> [ULGCE-1014 (En Attente)-6]
[ULGCE-1014 (A faire)-7] starts 2024/02/09 and ends 2024/02/10
[ULGCE-1014 (A faire)-7] is colored in LightGray/Gray
[ULGCE-1014 (A faire)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1014]]
[ULGCE-1014 (En Attente)-6] -> [ULGCE-1014 (A faire)-7]
[ULGCE-1014 (En Test)-8] starts 2024/02/10 and ends 2024/02/10
[ULGCE-1014 (En Test)-8] is colored in LightGray/Gray
[ULGCE-1014 (En Test)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1014]]
[ULGCE-1014 (A faire)-7] -> [ULGCE-1014 (En Test)-8]
[ULGCE-1014 (Terminée)-9] starts 2024/02/10 and ends 2024/02/10
[ULGCE-1014 (Terminée)-9] is colored in LightGray/Gray
[ULGCE-1014 (Terminée)-9] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1014]]
[ULGCE-1014 (En Test)-8] -> [ULGCE-1014 (Terminée)-9]
[ULGCE-126 - création] -> [ULGCE-1013 Support collaborateur - création]
[ULGCE-1013 Support collaborateur - création] happens at 2024/01/17
[ULGCE-1013 Support collaborateur - création] -> [ULGCE-1013 (IN PROGRESS)-1]
[ULGCE-1013 (IN PROGRESS)-1] starts 2024/01/17 and ends 2024/01/19
[ULGCE-1013 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1013 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1013]]
[ULGCE-1013 (En Attente)-2] starts 2024/01/19 and ends 2024/01/19
[ULGCE-1013 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1013 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1013]]
[ULGCE-1013 (IN PROGRESS)-1] -> [ULGCE-1013 (En Attente)-2]
[ULGCE-1013 (IN PROGRESS)-3] starts 2024/01/19 and ends 2024/01/26
[ULGCE-1013 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1013 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1013]]
[ULGCE-1013 (En Attente)-2] -> [ULGCE-1013 (IN PROGRESS)-3]
[ULGCE-1013 (En Test)-4] starts 2024/01/26 and ends 2024/01/26
[ULGCE-1013 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1013 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1013]]
[ULGCE-1013 (IN PROGRESS)-3] -> [ULGCE-1013 (En Test)-4]
[ULGCE-1013 (Terminée)-5] starts 2024/01/26 and ends 2024/01/26
[ULGCE-1013 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1013 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1013]]
[ULGCE-1013 (En Test)-4] -> [ULGCE-1013 (Terminée)-5]
[ULGCE-126 - création] -> [ULGCE-1012 Support collaborateur - création]
[ULGCE-1012 Support collaborateur - création] happens at 2024/01/17
[ULGCE-1012 Support collaborateur - création] -> [ULGCE-1012 (En Test)-1]
[ULGCE-1012 (En Test)-1] starts 2024/01/17 and ends 2024/01/25
[ULGCE-1012 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1012 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1012]]
[ULGCE-1012 (A faire)-2] starts 2024/01/25 and ends 2024/01/26
[ULGCE-1012 (A faire)-2] is colored in LightGray/Gray
[ULGCE-1012 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1012]]
[ULGCE-1012 (En Test)-1] -> [ULGCE-1012 (A faire)-2]
[ULGCE-1012 (IN PROGRESS)-3] starts 2024/01/26 and ends 2024/01/26
[ULGCE-1012 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1012 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1012]]
[ULGCE-1012 (A faire)-2] -> [ULGCE-1012 (IN PROGRESS)-3]
[ULGCE-1012 (En Attente)-4] starts 2024/01/26 and ends 2024/01/26
[ULGCE-1012 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1012 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1012]]
[ULGCE-1012 (IN PROGRESS)-3] -> [ULGCE-1012 (En Attente)-4]
[ULGCE-1012 (A faire)-5] starts 2024/01/26 and ends 2024/02/10
[ULGCE-1012 (A faire)-5] is colored in LightGray/Gray
[ULGCE-1012 (A faire)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1012]]
[ULGCE-1012 (En Attente)-4] -> [ULGCE-1012 (A faire)-5]
[ULGCE-1012 (En Test)-6] starts 2024/02/10 and ends 2024/02/10
[ULGCE-1012 (En Test)-6] is colored in LightGray/Gray
[ULGCE-1012 (En Test)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1012]]
[ULGCE-1012 (A faire)-5] -> [ULGCE-1012 (En Test)-6]
[ULGCE-1012 (Terminée)-7] starts 2024/02/10 and ends 2024/02/10
[ULGCE-1012 (Terminée)-7] is colored in LightGray/Gray
[ULGCE-1012 (Terminée)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1012]]
[ULGCE-1012 (En Test)-6] -> [ULGCE-1012 (Terminée)-7]
[ULGCE-126 - création] -> [ULGCE-1011 Support collaborateur - création]
[ULGCE-1011 Support collaborateur - création] happens at 2024/01/17
[ULGCE-1011 Support collaborateur - création] -> [ULGCE-1011 (IN PROGRESS)-1]
[ULGCE-1011 (IN PROGRESS)-1] starts 2024/01/17 and ends 2024/01/19
[ULGCE-1011 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1011 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1011]]
[ULGCE-1011 (En Attente)-2] starts 2024/01/19 and ends 2024/01/23
[ULGCE-1011 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1011 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1011]]
[ULGCE-1011 (IN PROGRESS)-1] -> [ULGCE-1011 (En Attente)-2]
[ULGCE-1011 (IN PROGRESS)-3] starts 2024/01/23 and ends 2024/01/24
[ULGCE-1011 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1011 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1011]]
[ULGCE-1011 (En Attente)-2] -> [ULGCE-1011 (IN PROGRESS)-3]
[ULGCE-1011 (En Test)-4] starts 2024/01/24 and ends 2024/01/26
[ULGCE-1011 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1011 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1011]]
[ULGCE-1011 (IN PROGRESS)-3] -> [ULGCE-1011 (En Test)-4]
[ULGCE-1011 (Terminée)-5] starts 2024/01/26 and ends 2024/01/26
[ULGCE-1011 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1011 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1011]]
[ULGCE-1011 (En Test)-4] -> [ULGCE-1011 (Terminée)-5]
[ULGCE-126 - création] -> [ULGCE-919 Support collaborateur - création]
[ULGCE-919 Support collaborateur - création] happens at 2024/01/11
[ULGCE-919 Support collaborateur - création] -> [ULGCE-919 (En Test)-1]
[ULGCE-919 (En Test)-1] starts 2024/01/11 and ends 2024/01/17
[ULGCE-919 (En Test)-1] is colored in LightGray/Gray
[ULGCE-919 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-919]]
[ULGCE-919 (Terminée)-2] starts 2024/01/17 and ends 2024/01/17
[ULGCE-919 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-919 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-919]]
[ULGCE-919 (En Test)-1] -> [ULGCE-919 (Terminée)-2]
[ULGCE-126 - création] -> [ULGCE-918 Support collaborateur - création]
[ULGCE-918 Support collaborateur - création] happens at 2024/01/11
[ULGCE-918 Support collaborateur - création] -> [ULGCE-918 (IN PROGRESS)-1]
[ULGCE-918 (IN PROGRESS)-1] starts 2024/01/11 and ends 2024/01/11
[ULGCE-918 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-918 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-918]]
[ULGCE-918 (En Attente)-2] starts 2024/01/11 and ends 2024/01/17
[ULGCE-918 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-918 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-918]]
[ULGCE-918 (IN PROGRESS)-1] -> [ULGCE-918 (En Attente)-2]
[ULGCE-918 (IN PROGRESS)-3] starts 2024/01/17 and ends 2024/01/17
[ULGCE-918 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-918 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-918]]
[ULGCE-918 (En Attente)-2] -> [ULGCE-918 (IN PROGRESS)-3]
[ULGCE-918 (En Test)-4] starts 2024/01/17 and ends 2024/01/17
[ULGCE-918 (En Test)-4] is colored in LightGray/Gray
[ULGCE-918 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-918]]
[ULGCE-918 (IN PROGRESS)-3] -> [ULGCE-918 (En Test)-4]
[ULGCE-918 (Terminée)-5] starts 2024/01/17 and ends 2024/01/17
[ULGCE-918 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-918 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-918]]
[ULGCE-918 (En Test)-4] -> [ULGCE-918 (Terminée)-5]
[ULGCE-126 - création] -> [ULGCE-880 Support collaborateur - création]
[ULGCE-880 Support collaborateur - création] happens at 2024/01/04
[ULGCE-880 Support collaborateur - création] -> [ULGCE-880 (IN PROGRESS)-1]
[ULGCE-880 (IN PROGRESS)-1] starts 2024/01/04 and ends 2024/01/04
[ULGCE-880 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-880 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-880]]
[ULGCE-880 (En Attente)-2] starts 2024/01/04 and ends 2024/01/05
[ULGCE-880 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-880 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-880]]
[ULGCE-880 (IN PROGRESS)-1] -> [ULGCE-880 (En Attente)-2]
[ULGCE-880 (IN PROGRESS)-3] starts 2024/01/05 and ends 2024/01/10
[ULGCE-880 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-880 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-880]]
[ULGCE-880 (En Attente)-2] -> [ULGCE-880 (IN PROGRESS)-3]
[ULGCE-880 (En Test)-4] starts 2024/01/10 and ends 2024/01/10
[ULGCE-880 (En Test)-4] is colored in LightGray/Gray
[ULGCE-880 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-880]]
[ULGCE-880 (IN PROGRESS)-3] -> [ULGCE-880 (En Test)-4]
[ULGCE-880 (Terminée)-5] starts 2024/01/10 and ends 2024/01/10
[ULGCE-880 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-880 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-880]]
[ULGCE-880 (En Test)-4] -> [ULGCE-880 (Terminée)-5]
[ULGCE-126 - création] -> [ULGCE-789 Support collaborateur - création]
[ULGCE-789 Support collaborateur - création] happens at 2023/12/21
[ULGCE-789 Support collaborateur - création] -> [ULGCE-789 (En Test)-1]
[ULGCE-789 (En Test)-1] starts 2023/12/21 and ends 2023/12/26
[ULGCE-789 (En Test)-1] is colored in LightGray/Gray
[ULGCE-789 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-789]]
[ULGCE-789 (A faire)-2] starts 2023/12/26 and ends 2023/12/26
[ULGCE-789 (A faire)-2] is colored in LightGray/Gray
[ULGCE-789 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-789]]
[ULGCE-789 (En Test)-1] -> [ULGCE-789 (A faire)-2]
[ULGCE-789 (IN PROGRESS)-3] starts 2023/12/26 and ends 2023/12/26
[ULGCE-789 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-789 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-789]]
[ULGCE-789 (A faire)-2] -> [ULGCE-789 (IN PROGRESS)-3]
[ULGCE-789 (En Attente)-4] starts 2023/12/26 and ends 2023/12/26
[ULGCE-789 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-789 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-789]]
[ULGCE-789 (IN PROGRESS)-3] -> [ULGCE-789 (En Attente)-4]
[ULGCE-789 (IN PROGRESS)-5] starts 2023/12/26 and ends 2023/12/29
[ULGCE-789 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-789 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-789]]
[ULGCE-789 (En Attente)-4] -> [ULGCE-789 (IN PROGRESS)-5]
[ULGCE-789 (En Test)-6] starts 2023/12/29 and ends 2024/01/04
[ULGCE-789 (En Test)-6] is colored in LightGray/Gray
[ULGCE-789 (En Test)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-789]]
[ULGCE-789 (IN PROGRESS)-5] -> [ULGCE-789 (En Test)-6]
[ULGCE-789 (Terminée)-7] starts 2024/01/04 and ends 2024/01/04
[ULGCE-789 (Terminée)-7] is colored in LightGray/Gray
[ULGCE-789 (Terminée)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-789]]
[ULGCE-789 (En Test)-6] -> [ULGCE-789 (Terminée)-7]
[ULGCE-126 - création] -> [ULGCE-776 Support collaborateur - création]
[ULGCE-776 Support collaborateur - création] happens at 2023/12/20
[ULGCE-776 Support collaborateur - création] -> [ULGCE-776 (IN PROGRESS)-1]
[ULGCE-776 (IN PROGRESS)-1] starts 2023/12/20 and ends 2023/12/21
[ULGCE-776 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-776 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-776]]
[ULGCE-776 (En Attente)-2] starts 2023/12/21 and ends 2023/12/27
[ULGCE-776 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-776 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-776]]
[ULGCE-776 (IN PROGRESS)-1] -> [ULGCE-776 (En Attente)-2]
[ULGCE-776 (A faire)-3] starts 2023/12/27 and ends 2023/12/27
[ULGCE-776 (A faire)-3] is colored in LightGray/Gray
[ULGCE-776 (A faire)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-776]]
[ULGCE-776 (En Attente)-2] -> [ULGCE-776 (A faire)-3]
[ULGCE-776 (IN PROGRESS)-4] starts 2023/12/27 and ends 2023/12/28
[ULGCE-776 (IN PROGRESS)-4] is colored in LightGray/Gray
[ULGCE-776 (IN PROGRESS)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-776]]
[ULGCE-776 (A faire)-3] -> [ULGCE-776 (IN PROGRESS)-4]
[ULGCE-776 (En Attente)-5] starts 2023/12/28 and ends 2023/12/29
[ULGCE-776 (En Attente)-5] is colored in LightGray/Gray
[ULGCE-776 (En Attente)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-776]]
[ULGCE-776 (IN PROGRESS)-4] -> [ULGCE-776 (En Attente)-5]
[ULGCE-776 (IN PROGRESS)-6] starts 2023/12/29 and ends 2024/01/04
[ULGCE-776 (IN PROGRESS)-6] is colored in LightGray/Gray
[ULGCE-776 (IN PROGRESS)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-776]]
[ULGCE-776 (En Attente)-5] -> [ULGCE-776 (IN PROGRESS)-6]
[ULGCE-776 (En Test)-7] starts 2024/01/04 and ends 2024/01/04
[ULGCE-776 (En Test)-7] is colored in LightGray/Gray
[ULGCE-776 (En Test)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-776]]
[ULGCE-776 (IN PROGRESS)-6] -> [ULGCE-776 (En Test)-7]
[ULGCE-776 (Terminée)-8] starts 2024/01/04 and ends 2024/01/04
[ULGCE-776 (Terminée)-8] is colored in LightGray/Gray
[ULGCE-776 (Terminée)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-776]]
[ULGCE-776 (En Test)-7] -> [ULGCE-776 (Terminée)-8]
[ULGCE-126 - création] -> [ULGCE-697 Support collaborateur - création]
[ULGCE-697 Support collaborateur - création] happens at 2023/12/14
[ULGCE-697 Support collaborateur - création] -> [ULGCE-697 (En Test)-1]
[ULGCE-697 (En Test)-1] starts 2023/12/14 and ends 2023/12/21
[ULGCE-697 (En Test)-1] is colored in LightGray/Gray
[ULGCE-697 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-697]]
[ULGCE-697 (Terminée)-2] starts 2023/12/21 and ends 2023/12/21
[ULGCE-697 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-697 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-697]]
[ULGCE-697 (En Test)-1] -> [ULGCE-697 (Terminée)-2]
[ULGCE-126 - création] -> [ULGCE-696 Support collaborateur - création]
[ULGCE-696 Support collaborateur - création] happens at 2023/12/14
[ULGCE-696 Support collaborateur - création] -> [ULGCE-696 (IN PROGRESS)-1]
[ULGCE-696 (IN PROGRESS)-1] starts 2023/12/14 and ends 2023/12/15
[ULGCE-696 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-696 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-696]]
[ULGCE-696 (En Attente)-2] starts 2023/12/15 and ends 2023/12/15
[ULGCE-696 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-696 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-696]]
[ULGCE-696 (IN PROGRESS)-1] -> [ULGCE-696 (En Attente)-2]
[ULGCE-696 (IN PROGRESS)-3] starts 2023/12/15 and ends 2023/12/20
[ULGCE-696 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-696 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-696]]
[ULGCE-696 (En Attente)-2] -> [ULGCE-696 (IN PROGRESS)-3]
[ULGCE-696 (En Test)-4] starts 2023/12/20 and ends 2023/12/20
[ULGCE-696 (En Test)-4] is colored in LightGray/Gray
[ULGCE-696 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-696]]
[ULGCE-696 (IN PROGRESS)-3] -> [ULGCE-696 (En Test)-4]
[ULGCE-696 (Terminée)-5] starts 2023/12/20 and ends 2023/12/20
[ULGCE-696 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-696 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-696]]
[ULGCE-696 (En Test)-4] -> [ULGCE-696 (Terminée)-5]
-- ULGCE-1262 - [#929655] MODCOT TP8A : CAF sans période --
[ULGCE-1262 - création] happens on 2024/02/27
[ULGCE-1262 - création] -> [ULGCE-1312 #929655_929655_ANOMALIE_MCO-TESTS - création]
[ULGCE-1312 #929655_929655_ANOMALIE_MCO-TESTS - création] happens at 2024/03/11
[ULGCE-1262 - création] -> [ULGCE-1305 #929655_929655_ANOMALIE_MCO-DEVELOPPEMENT - création]
[ULGCE-1305 #929655_929655_ANOMALIE_MCO-DEVELOPPEMENT - création] happens at 2024/03/08
[ULGCE-1305 #929655_929655_ANOMALIE_MCO-DEVELOPPEMENT - création] -> [ULGCE-1305 (IN PROGRESS)-1]
[ULGCE-1305 (IN PROGRESS)-1] starts 2024/03/08 and ends 2024/03/14
[ULGCE-1305 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1305 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1305]]
[ULGCE-1305 (En Attente)-2] starts 2024/03/14 and ends 2024/03/14
[ULGCE-1305 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1305 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1305]]
[ULGCE-1305 (IN PROGRESS)-1] -> [ULGCE-1305 (En Attente)-2]
[ULGCE-1262 - création] -> [ULGCE-1304 #929655_929655_ANOMALIE_MCO_ANALYSE - création]
[ULGCE-1304 #929655_929655_ANOMALIE_MCO_ANALYSE - création] happens at 2024/03/08
[ULGCE-1304 #929655_929655_ANOMALIE_MCO_ANALYSE - création] -> [ULGCE-1304 (IN PROGRESS)-1]
[ULGCE-1304 (IN PROGRESS)-1] starts 2024/03/08 and ends 2024/03/08
[ULGCE-1304 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1304 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1304]]
[ULGCE-1304 (En Attente)-2] starts 2024/03/08 and ends 2024/03/13
[ULGCE-1304 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1304 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1304]]
[ULGCE-1304 (IN PROGRESS)-1] -> [ULGCE-1304 (En Attente)-2]
[ULGCE-1304 (IN PROGRESS)-3] starts 2024/03/13 and ends 2024/03/14
[ULGCE-1304 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1304 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1304]]
[ULGCE-1304 (En Attente)-2] -> [ULGCE-1304 (IN PROGRESS)-3]
[ULGCE-1304 (En Test)-4] starts 2024/03/14 and ends 2024/03/14
[ULGCE-1304 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1304 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1304]]
[ULGCE-1304 (IN PROGRESS)-3] -> [ULGCE-1304 (En Test)-4]
[ULGCE-1304 (Terminée)-5] starts 2024/03/14 and ends 2024/03/14
[ULGCE-1304 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1304 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1304]]
[ULGCE-1304 (En Test)-4] -> [ULGCE-1304 (Terminée)-5]
-- ULGCE-1291 - [#931447] Saisie de transferts et remboursement de crédit CAS via OBX4 pour des comptes Artiste Auteur --
[ULGCE-1291 - création] happens on 2024/03/04
[ULGCE-1291 - création] -> [ULGCE-2097 #931447_776337_EVOLUTION__TESTS - création]
[ULGCE-2097 #931447_776337_EVOLUTION__TESTS - création] happens at 2024/08/23
[ULGCE-2097 #931447_776337_EVOLUTION__TESTS - création] -> [ULGCE-2097 (IN PROGRESS)-1]
[ULGCE-2097 (IN PROGRESS)-1] starts 2024/08/23 and ends 2024/09/02
[ULGCE-2097 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2097 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2097]]
[ULGCE-1291 - création] -> [ULGCE-2086 #931447_776337_EVOLUTION__DOSSIER DE TESTS(ALM) - création]
[ULGCE-2086 #931447_776337_EVOLUTION__DOSSIER DE TESTS(ALM) - création] happens at 2024/08/19
[ULGCE-2086 #931447_776337_EVOLUTION__DOSSIER DE TESTS(ALM) - création] -> [ULGCE-2086 (IN PROGRESS)-1]
[ULGCE-2086 (IN PROGRESS)-1] starts 2024/08/19 and ends 2024/09/02
[ULGCE-2086 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2086 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2086]]
[ULGCE-1291 - création] -> [ULGCE-2085 #931447_776337_EVOLUTION__TESTS - création]
[ULGCE-2085 #931447_776337_EVOLUTION__TESTS - création] happens at 2024/08/19
[ULGCE-2085 #931447_776337_EVOLUTION__TESTS - création] -> [ULGCE-2085 (IN PROGRESS)-1]
[ULGCE-2085 (IN PROGRESS)-1] starts 2024/08/19 and ends 2024/08/20
[ULGCE-2085 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2085 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2085]]
[ULGCE-2085 (En Test)-2] starts 2024/08/20 and ends 2024/09/06
[ULGCE-2085 (En Test)-2] is colored in LightGray/Gray
[ULGCE-2085 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2085]]
[ULGCE-2085 (IN PROGRESS)-1] -> [ULGCE-2085 (En Test)-2]
[ULGCE-2085 (Terminée)-3] starts 2024/09/06 and ends 2024/09/06
[ULGCE-2085 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-2085 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2085]]
[ULGCE-2085 (En Test)-2] -> [ULGCE-2085 (Terminée)-3]
[ULGCE-1291 - création] -> [ULGCE-2084 #931447_776337_EVOLUTION__DEVELOPPEMENT - création]
[ULGCE-2084 #931447_776337_EVOLUTION__DEVELOPPEMENT - création] happens at 2024/08/19
[ULGCE-2084 #931447_776337_EVOLUTION__DEVELOPPEMENT - création] -> [ULGCE-2084 (IN PROGRESS)-1]
[ULGCE-2084 (IN PROGRESS)-1] starts 2024/08/19 and ends 2024/08/20
[ULGCE-2084 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2084 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2084]]
[ULGCE-1291 - création] -> [ULGCE-2083 #931447_776337_EVOLUTION__CONCEPTION TECHNIQUE - création]
[ULGCE-2083 #931447_776337_EVOLUTION__CONCEPTION TECHNIQUE - création] happens at 2024/08/19
[ULGCE-2083 #931447_776337_EVOLUTION__CONCEPTION TECHNIQUE - création] -> [ULGCE-2083 (IN PROGRESS)-1]
[ULGCE-2083 (IN PROGRESS)-1] starts 2024/08/19 and ends 2024/08/20
[ULGCE-2083 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2083 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2083]]
[ULGCE-2083 (En Test)-2] starts 2024/08/20 and ends 2024/09/06
[ULGCE-2083 (En Test)-2] is colored in LightGray/Gray
[ULGCE-2083 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2083]]
[ULGCE-2083 (IN PROGRESS)-1] -> [ULGCE-2083 (En Test)-2]
[ULGCE-2083 (Terminée)-3] starts 2024/09/06 and ends 2024/09/06
[ULGCE-2083 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-2083 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2083]]
[ULGCE-2083 (En Test)-2] -> [ULGCE-2083 (Terminée)-3]
[ULGCE-1291 - création] -> [ULGCE-1546 #931447_776337_EVOLUTION__DEVIS - création]
[ULGCE-1546 #931447_776337_EVOLUTION__DEVIS - création] happens at 2024/04/09
[ULGCE-1291 - création] -> [ULGCE-1354 931447_776337_EVOLUTION_DEVIS - création]
[ULGCE-1354 931447_776337_EVOLUTION_DEVIS - création] happens at 2024/03/15
[ULGCE-1354 931447_776337_EVOLUTION_DEVIS - création] -> [ULGCE-1354 (En Test)-1]
[ULGCE-1354 (En Test)-1] starts 2024/03/15 and ends 2024/03/15
[ULGCE-1354 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1354 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1354]]
[ULGCE-1354 (Terminée)-2] starts 2024/03/15 and ends 2024/03/18
[ULGCE-1354 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1354 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1354]]
[ULGCE-1354 (En Test)-1] -> [ULGCE-1354 (Terminée)-2]
[ULGCE-1291 - création] -> [ULGCE-1306 931447_776337_EVOLUTION_DEVIS - création]
[ULGCE-1306 931447_776337_EVOLUTION_DEVIS - création] happens at 2024/03/11
[ULGCE-1306 931447_776337_EVOLUTION_DEVIS - création] -> [ULGCE-1306 (En Test)-1]
[ULGCE-1306 (En Test)-1] starts 2024/03/11 and ends 2024/03/11
[ULGCE-1306 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1306 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1306]]
[ULGCE-1306 (A faire)-2] starts 2024/03/11 and ends 2024/03/12
[ULGCE-1306 (A faire)-2] is colored in LightGray/Gray
[ULGCE-1306 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1306]]
[ULGCE-1306 (En Test)-1] -> [ULGCE-1306 (A faire)-2]
[ULGCE-1306 (IN PROGRESS)-3] starts 2024/03/12 and ends 2024/03/12
[ULGCE-1306 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1306 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1306]]
[ULGCE-1306 (A faire)-2] -> [ULGCE-1306 (IN PROGRESS)-3]
[ULGCE-1306 (En Attente)-4] starts 2024/03/12 and ends 2024/03/12
[ULGCE-1306 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1306 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1306]]
[ULGCE-1306 (IN PROGRESS)-3] -> [ULGCE-1306 (En Attente)-4]
[ULGCE-1306 (IN PROGRESS)-5] starts 2024/03/12 and ends 2024/03/18
[ULGCE-1306 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1306 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1306]]
[ULGCE-1306 (En Attente)-4] -> [ULGCE-1306 (IN PROGRESS)-5]
[ULGCE-1306 (En Test)-6] starts 2024/03/18 and ends 2024/03/18
[ULGCE-1306 (En Test)-6] is colored in LightGray/Gray
[ULGCE-1306 (En Test)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1306]]
[ULGCE-1306 (IN PROGRESS)-5] -> [ULGCE-1306 (En Test)-6]
[ULGCE-1306 (Terminée)-7] starts 2024/03/18 and ends 2024/03/18
[ULGCE-1306 (Terminée)-7] is colored in LightGray/Gray
[ULGCE-1306 (Terminée)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1306]]
[ULGCE-1306 (En Test)-6] -> [ULGCE-1306 (Terminée)-7]
-- ULGCE-1336 - [#934862] Loi Pacte - Changement de périodicité - Mise au format COSI des notifications --
[ULGCE-1336 - création] happens on 2024/03/12
[ULGCE-1336 - création] -> [ULGCE-2113 #934862_990680_EVOLUTION_TESTS - création]
[ULGCE-2113 #934862_990680_EVOLUTION_TESTS - création] happens at 2024/08/28
[ULGCE-2113 #934862_990680_EVOLUTION_TESTS - création] -> [ULGCE-2113 (IN PROGRESS)-1]
[ULGCE-2113 (IN PROGRESS)-1] starts 2024/08/28 and ends 2024/08/28
[ULGCE-2113 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2113 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2113]]
-- ULGCE-1343 - [#935703] [P23-017] Apurement créances prescrites -  module OBPA pour l’apurement des créances dépendantes via ANV pour toutes les populations --
[ULGCE-1343 - création] happens on 2024/03/14
[ULGCE-1343 - création] -> [ULGCE-1550 #935703_935703_EVOLUTION__DOSSIER DE TESTS (ALM) - création]
[ULGCE-1550 #935703_935703_EVOLUTION__DOSSIER DE TESTS (ALM) - création] happens at 2024/04/09
[ULGCE-1343 - création] -> [ULGCE-1549 #935703_935703_EVOLUTION__TESTS - création]
[ULGCE-1549 #935703_935703_EVOLUTION__TESTS - création] happens at 2024/04/09
[ULGCE-1343 - création] -> [ULGCE-1548 #935703_935703_EVOLUTION__DEVELOPPEMENT - création]
[ULGCE-1548 #935703_935703_EVOLUTION__DEVELOPPEMENT - création] happens at 2024/04/09
[ULGCE-1343 - création] -> [ULGCE-1547 #935703_935703_EVOLUTION__ALGORYTHMIE - création]
[ULGCE-1547 #935703_935703_EVOLUTION__ALGORYTHMIE - création] happens at 2024/04/09
[ULGCE-1343 - création] -> [ULGCE-1487 #935703_XXXXXX_EVOLUTION_CONCEPTION_TECHNIQUE - création]
[ULGCE-1487 #935703_XXXXXX_EVOLUTION_CONCEPTION_TECHNIQUE - création] happens at 2024/04/02
[ULGCE-1487 #935703_XXXXXX_EVOLUTION_CONCEPTION_TECHNIQUE - création] -> [ULGCE-1487 (IN PROGRESS)-1]
[ULGCE-1487 (IN PROGRESS)-1] starts 2024/04/02 and ends 2024/04/05
[ULGCE-1487 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1487 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1487]]
[ULGCE-1487 (En Test)-2] starts 2024/04/05 and ends 2024/08/20
[ULGCE-1487 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1487 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1487]]
[ULGCE-1487 (IN PROGRESS)-1] -> [ULGCE-1487 (En Test)-2]
[ULGCE-1487 (Terminée)-3] starts 2024/08/20 and ends 2024/08/20
[ULGCE-1487 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1487 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1487]]
[ULGCE-1487 (En Test)-2] -> [ULGCE-1487 (Terminée)-3]
[ULGCE-1343 - création] -> [ULGCE-1419 #935703_DEVIS - création]
[ULGCE-1419 #935703_DEVIS - création] happens at 2024/03/22
[ULGCE-1419 #935703_DEVIS - création] -> [ULGCE-1419 (IN PROGRESS)-1]
[ULGCE-1419 (IN PROGRESS)-1] starts 2024/03/22 and ends 2024/03/22
[ULGCE-1419 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1419 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1419]]
[ULGCE-1419 (En Test)-2] starts 2024/03/22 and ends 2024/03/29
[ULGCE-1419 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1419 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1419]]
[ULGCE-1419 (IN PROGRESS)-1] -> [ULGCE-1419 (En Test)-2]
[ULGCE-1419 (Terminée)-3] starts 2024/03/29 and ends 2024/03/29
[ULGCE-1419 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1419 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1419]]
[ULGCE-1419 (En Test)-2] -> [ULGCE-1419 (Terminée)-3]
[ULGCE-1419 (A faire)-4] starts 2024/03/29 and ends 2024/03/29
[ULGCE-1419 (A faire)-4] is colored in LightGray/Gray
[ULGCE-1419 (A faire)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1419]]
[ULGCE-1419 (Terminée)-3] -> [ULGCE-1419 (A faire)-4]
[ULGCE-1419 (IN PROGRESS)-5] starts 2024/03/29 and ends 2024/03/29
[ULGCE-1419 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1419 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1419]]
[ULGCE-1419 (A faire)-4] -> [ULGCE-1419 (IN PROGRESS)-5]
[ULGCE-1419 (En Attente)-6] starts 2024/03/29 and ends 2024/03/29
[ULGCE-1419 (En Attente)-6] is colored in LightGray/Gray
[ULGCE-1419 (En Attente)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1419]]
[ULGCE-1419 (IN PROGRESS)-5] -> [ULGCE-1419 (En Attente)-6]
[ULGCE-1419 (IN PROGRESS)-7] starts 2024/03/29 and ends 2024/04/08
[ULGCE-1419 (IN PROGRESS)-7] is colored in LightGray/Gray
[ULGCE-1419 (IN PROGRESS)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1419]]
[ULGCE-1419 (En Attente)-6] -> [ULGCE-1419 (IN PROGRESS)-7]
[ULGCE-1419 (En Test)-8] starts 2024/04/08 and ends 2024/04/08
[ULGCE-1419 (En Test)-8] is colored in LightGray/Gray
[ULGCE-1419 (En Test)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1419]]
[ULGCE-1419 (IN PROGRESS)-7] -> [ULGCE-1419 (En Test)-8]
[ULGCE-1419 (Terminée)-9] starts 2024/04/08 and ends 2024/04/08
[ULGCE-1419 (Terminée)-9] is colored in LightGray/Gray
[ULGCE-1419 (Terminée)-9] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1419]]
[ULGCE-1419 (En Test)-8] -> [ULGCE-1419 (Terminée)-9]
-- ULGCE-1390 - [#765111] MODCOT : numéro de pièce de crédit erronée  --
[ULGCE-1390 - création] happens on 2024/03/20
[ULGCE-1390 - création] -> [ULGCE-1393 #765111_765111_ANOMALIE_MCO_TESTS - création]
[ULGCE-1393 #765111_765111_ANOMALIE_MCO_TESTS - création] happens at 2024/03/20
[ULGCE-1390 - création] -> [ULGCE-1392 #765111_765111_ANOMALIE_MCO_DEVELOPPEMENT - création]
[ULGCE-1392 #765111_765111_ANOMALIE_MCO_DEVELOPPEMENT - création] happens at 2024/03/20
[ULGCE-1390 - création] -> [ULGCE-1391 #765111_765111_ANOMALIE_MCO_ANALYSE - création]
[ULGCE-1391 #765111_765111_ANOMALIE_MCO_ANALYSE - création] happens at 2024/03/20
[ULGCE-1391 #765111_765111_ANOMALIE_MCO_ANALYSE - création] -> [ULGCE-1391 (IN PROGRESS)-1]
[ULGCE-1391 (IN PROGRESS)-1] starts 2024/03/20 and ends 2024/03/22
[ULGCE-1391 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1391 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1391]]
[ULGCE-1391 (En Attente)-2] starts 2024/03/22 and ends 2024/03/28
[ULGCE-1391 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1391 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1391]]
[ULGCE-1391 (IN PROGRESS)-1] -> [ULGCE-1391 (En Attente)-2]
-- ULGCE-1411 - [#682546] PASRAU Reprise des tests : TT1P - Discordance entre les informations remontées dans BIL1 et les fichiers ES10 (T04 et L01-PS03A) --
[ULGCE-1411 - création] happens on 2024/03/22
[ULGCE-1411 - création] -> [ULGCE-1465 #682546_682546_ANOMALIE_MCO-TESTS - création]
[ULGCE-1465 #682546_682546_ANOMALIE_MCO-TESTS - création] happens at 2024/03/27
[ULGCE-1411 - création] -> [ULGCE-1464 #682546_682546_ANOMALIE_MCO-DEVELOPPEMENT - création]
[ULGCE-1464 #682546_682546_ANOMALIE_MCO-DEVELOPPEMENT - création] happens at 2024/03/27
[ULGCE-1411 - création] -> [ULGCE-1463 #682546_682546_ANOMALIE_MCO-ANALYSE - création]
[ULGCE-1463 #682546_682546_ANOMALIE_MCO-ANALYSE - création] happens at 2024/03/27
[ULGCE-1463 #682546_682546_ANOMALIE_MCO-ANALYSE - création] -> [ULGCE-1463 (IN PROGRESS)-1]
[ULGCE-1463 (IN PROGRESS)-1] starts 2024/03/27 and ends 2024/03/29
[ULGCE-1463 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1463 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1463]]
[ULGCE-1463 (En Attente)-2] starts 2024/03/29 and ends 2024/03/29
[ULGCE-1463 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1463 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1463]]
[ULGCE-1463 (IN PROGRESS)-1] -> [ULGCE-1463 (En Attente)-2]
[ULGCE-1463 (IN PROGRESS)-3] starts 2024/03/29 and ends 2024/03/29
[ULGCE-1463 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1463 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1463]]
[ULGCE-1463 (En Attente)-2] -> [ULGCE-1463 (IN PROGRESS)-3]
[ULGCE-1463 (En Attente)-4] starts 2024/03/29 and ends 2024/03/29
[ULGCE-1463 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1463 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1463]]
[ULGCE-1463 (IN PROGRESS)-3] -> [ULGCE-1463 (En Attente)-4]
-- ULGCE-1415 - [#671626] Comptes DSN62 : Divergence priorisation PO sur affectation crédit partiel --
[ULGCE-1415 - création] happens on 2024/03/22
[ULGCE-1415 - création] -> [ULGCE-1453 #671626_671626_ANOMALIE_MCO-TESTS - création]
[ULGCE-1453 #671626_671626_ANOMALIE_MCO-TESTS - création] happens at 2024/03/27
[ULGCE-1415 - création] -> [ULGCE-1452 #671626_671626_ANOMALIE_MCO-DEVELOPPEMENT - création]
[ULGCE-1452 #671626_671626_ANOMALIE_MCO-DEVELOPPEMENT - création] happens at 2024/03/27
[ULGCE-1415 - création] -> [ULGCE-1451 #671626_671626_ANOMALIE_MCO-ANALYSE - création]
[ULGCE-1451 #671626_671626_ANOMALIE_MCO-ANALYSE - création] happens at 2024/03/27
[ULGCE-1451 #671626_671626_ANOMALIE_MCO-ANALYSE - création] -> [ULGCE-1451 (IN PROGRESS)-1]
[ULGCE-1451 (IN PROGRESS)-1] starts 2024/03/27 and ends 2024/03/29
[ULGCE-1451 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1451 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1451]]
[ULGCE-1451 (En Attente)-2] starts 2024/03/29 and ends 2024/03/29
[ULGCE-1451 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1451 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1451]]
[ULGCE-1451 (IN PROGRESS)-1] -> [ULGCE-1451 (En Attente)-2]
-- ULGCE-1733 - [#958133] P24-316 : Frontaliers suisses Versement employeurs --
[ULGCE-1733 - création] happens on 2024/05/21
[ULGCE-1733 - création] -> [ULGCE-1957 958133_970807_EVOLUTION_DOSSIER DE TESTS - création]
[ULGCE-1957 958133_970807_EVOLUTION_DOSSIER DE TESTS - création] happens at 2024/07/09
[ULGCE-1733 - création] -> [ULGCE-1956 958133_970807_EVOLUTION_TESTS - création]
[ULGCE-1956 958133_970807_EVOLUTION_TESTS - création] happens at 2024/07/09
[ULGCE-1956 958133_970807_EVOLUTION_TESTS - création] -> [ULGCE-1956 (IN PROGRESS)-1]
[ULGCE-1956 (IN PROGRESS)-1] starts 2024/07/09 and ends 2024/08/20
[ULGCE-1956 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1956 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1956]]
[ULGCE-1956 (En Test)-2] starts 2024/08/20 and ends 2024/08/20
[ULGCE-1956 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1956 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1956]]
[ULGCE-1956 (IN PROGRESS)-1] -> [ULGCE-1956 (En Test)-2]
[ULGCE-1956 (Terminée)-3] starts 2024/08/20 and ends 2024/08/20
[ULGCE-1956 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1956 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1956]]
[ULGCE-1956 (En Test)-2] -> [ULGCE-1956 (Terminée)-3]
[ULGCE-1733 - création] -> [ULGCE-1955 958133_970807_EVOLUTION_DEVELOPPEMENT - création]
[ULGCE-1955 958133_970807_EVOLUTION_DEVELOPPEMENT - création] happens at 2024/07/09
[ULGCE-1955 958133_970807_EVOLUTION_DEVELOPPEMENT - création] -> [ULGCE-1955 (En Test)-1]
[ULGCE-1955 (En Test)-1] starts 2024/07/09 and ends 2024/07/10
[ULGCE-1955 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1955 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1955]]
[ULGCE-1955 (Terminée)-2] starts 2024/07/10 and ends 2024/08/20
[ULGCE-1955 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1955 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1955]]
[ULGCE-1955 (En Test)-1] -> [ULGCE-1955 (Terminée)-2]
[ULGCE-1733 - création] -> [ULGCE-1954 958133_970807_EVOLUTION_CONCEPTION TECHNIQUE - création]
[ULGCE-1954 958133_970807_EVOLUTION_CONCEPTION TECHNIQUE - création] happens at 2024/07/09
[ULGCE-1954 958133_970807_EVOLUTION_CONCEPTION TECHNIQUE - création] -> [ULGCE-1954 (En Test)-1]
[ULGCE-1954 (En Test)-1] starts 2024/07/09 and ends 2024/07/10
[ULGCE-1954 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1954 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1954]]
[ULGCE-1954 (Terminée)-2] starts 2024/07/10 and ends 2024/07/10
[ULGCE-1954 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1954 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1954]]
[ULGCE-1954 (En Test)-1] -> [ULGCE-1954 (Terminée)-2]
[ULGCE-1733 - création] -> [ULGCE-1744 958133_0_EVOLUTION_DEVIS - création]
[ULGCE-1744 958133_0_EVOLUTION_DEVIS - création] happens at 2024/05/23
[ULGCE-1744 958133_0_EVOLUTION_DEVIS - création] -> [ULGCE-1744 (En Test)-1]
[ULGCE-1744 (En Test)-1] starts 2024/05/23 and ends 2024/05/23
[ULGCE-1744 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1744 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1744]]
[ULGCE-1744 (Terminée)-2] starts 2024/05/23 and ends 2024/05/23
[ULGCE-1744 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1744 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1744]]
[ULGCE-1744 (En Test)-1] -> [ULGCE-1744 (Terminée)-2]
-- ULGCE-1761 - [#963661] P20-185 PERF lot 1.3 : Débit/instance (redmine chapeau portant la demande de devis et de réalisarion) --
[ULGCE-1761 - création] happens on 2024/05/28
[ULGCE-1761 - création] -> [ULGCE-1852 #963661_963661_PROJET_CONCEPTION TECHNIQUE - création]
[ULGCE-1852 #963661_963661_PROJET_CONCEPTION TECHNIQUE - création] happens at 2024/06/17
[ULGCE-1852 #963661_963661_PROJET_CONCEPTION TECHNIQUE - création] -> [ULGCE-1852 (IN PROGRESS)-1]
[ULGCE-1852 (IN PROGRESS)-1] starts 2024/06/17 and ends 2024/06/17
[ULGCE-1852 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1852 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1852]]
[ULGCE-1852 (En Attente)-2] starts 2024/06/17 and ends 2024/07/08
[ULGCE-1852 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1852 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1852]]
[ULGCE-1852 (IN PROGRESS)-1] -> [ULGCE-1852 (En Attente)-2]
[ULGCE-1852 (IN PROGRESS)-3] starts 2024/07/08 and ends 2024/08/28
[ULGCE-1852 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1852 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1852]]
[ULGCE-1852 (En Attente)-2] -> [ULGCE-1852 (IN PROGRESS)-3]
[ULGCE-1761 - création] -> [ULGCE-1795 #963661_963661_PROJET_DEVIS - création]
[ULGCE-1795 #963661_963661_PROJET_DEVIS - création] happens at 2024/06/04
[ULGCE-1795 #963661_963661_PROJET_DEVIS - création] -> [ULGCE-1795 (IN PROGRESS)-1]
[ULGCE-1795 (IN PROGRESS)-1] starts 2024/06/04 and ends 2024/06/04
[ULGCE-1795 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1795 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1795]]
[ULGCE-1795 (En Attente)-2] starts 2024/06/04 and ends 2024/06/04
[ULGCE-1795 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1795 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1795]]
[ULGCE-1795 (IN PROGRESS)-1] -> [ULGCE-1795 (En Attente)-2]
[ULGCE-1795 (IN PROGRESS)-3] starts 2024/06/04 and ends 2024/06/14
[ULGCE-1795 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1795 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1795]]
[ULGCE-1795 (En Attente)-2] -> [ULGCE-1795 (IN PROGRESS)-3]
[ULGCE-1795 (En Test)-4] starts 2024/06/14 and ends 2024/06/14
[ULGCE-1795 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1795 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1795]]
[ULGCE-1795 (IN PROGRESS)-3] -> [ULGCE-1795 (En Test)-4]
[ULGCE-1795 (Terminée)-5] starts 2024/06/14 and ends 2024/06/14
[ULGCE-1795 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1795 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1795]]
[ULGCE-1795 (En Test)-4] -> [ULGCE-1795 (Terminée)-5]
-- ULGCE-1762 - [#963669] Evolution Marins 3en1 pour TO --
[ULGCE-1762 - création] happens on 2024/05/28
[ULGCE-1762 - création] -> [ULGCE-2095 #963669_948595_EVOLUTION_DEVELOPPEMENT - création]
[ULGCE-2095 #963669_948595_EVOLUTION_DEVELOPPEMENT - création] happens at 2024/08/20
[ULGCE-2095 #963669_948595_EVOLUTION_DEVELOPPEMENT - création] -> [ULGCE-2095 (IN PROGRESS)-1]
[ULGCE-2095 (IN PROGRESS)-1] starts 2024/08/20 and ends 2024/08/20
[ULGCE-2095 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2095 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2095]]
[ULGCE-2095 (En Test)-2] starts 2024/08/20 and ends 2024/09/02
[ULGCE-2095 (En Test)-2] is colored in LightGray/Gray
[ULGCE-2095 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2095]]
[ULGCE-2095 (IN PROGRESS)-1] -> [ULGCE-2095 (En Test)-2]
[ULGCE-2095 (Terminée)-3] starts 2024/09/02 and ends 2024/09/02
[ULGCE-2095 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-2095 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2095]]
[ULGCE-2095 (En Test)-2] -> [ULGCE-2095 (Terminée)-3]
-- ULGCE-1772 - [#964647] TT1A : gestion des messages d'erreurs --
[ULGCE-1772 - création] happens on 2024/05/30
[ULGCE-1772 - création] -> [ULGCE-2082 #964647_964635_EVOLUTION_DOSSIER DE TESTS(ALM) - création]
[ULGCE-2082 #964647_964635_EVOLUTION_DOSSIER DE TESTS(ALM) - création] happens at 2024/08/19
[ULGCE-1772 - création] -> [ULGCE-2081 #964647_964635_EVOLUTION_TESTS - création]
[ULGCE-2081 #964647_964635_EVOLUTION_TESTS - création] happens at 2024/08/19
[ULGCE-2081 #964647_964635_EVOLUTION_TESTS - création] -> [ULGCE-2081 (En Test)-1]
[ULGCE-2081 (En Test)-1] starts 2024/08/19 and ends 2024/08/27
[ULGCE-2081 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2081 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2081]]
[ULGCE-2081 (A faire)-2] starts 2024/08/27 and ends 2024/09/06
[ULGCE-2081 (A faire)-2] is colored in LightGray/Gray
[ULGCE-2081 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2081]]
[ULGCE-2081 (En Test)-1] -> [ULGCE-2081 (A faire)-2]
[ULGCE-1772 - création] -> [ULGCE-2080 #964647_964635_EVOLUTION_DEVELOPPEMENT - création]
[ULGCE-2080 #964647_964635_EVOLUTION_DEVELOPPEMENT - création] happens at 2024/08/19
[ULGCE-2080 #964647_964635_EVOLUTION_DEVELOPPEMENT - création] -> [ULGCE-2080 (IN PROGRESS)-1]
[ULGCE-2080 (IN PROGRESS)-1] starts 2024/08/19 and ends 2024/09/02
[ULGCE-2080 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2080 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2080]]
[ULGCE-2080 (En Attente)-2] starts 2024/09/02 and ends 2024/09/05
[ULGCE-2080 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-2080 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2080]]
[ULGCE-2080 (IN PROGRESS)-1] -> [ULGCE-2080 (En Attente)-2]
[ULGCE-2080 (IN PROGRESS)-3] starts 2024/09/05 and ends 2024/09/06
[ULGCE-2080 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-2080 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2080]]
[ULGCE-2080 (En Attente)-2] -> [ULGCE-2080 (IN PROGRESS)-3]
[ULGCE-1772 - création] -> [ULGCE-2079 #964647_964635_EVOLUTION_CONCEPTION TECHNIQUE - création]
[ULGCE-2079 #964647_964635_EVOLUTION_CONCEPTION TECHNIQUE - création] happens at 2024/08/19
[ULGCE-2079 #964647_964635_EVOLUTION_CONCEPTION TECHNIQUE - création] -> [ULGCE-2079 (IN PROGRESS)-1]
[ULGCE-2079 (IN PROGRESS)-1] starts 2024/08/19 and ends 2024/09/02
[ULGCE-2079 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2079 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2079]]
[ULGCE-2079 (En Attente)-2] starts 2024/09/02 and ends 2024/09/05
[ULGCE-2079 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-2079 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2079]]
[ULGCE-2079 (IN PROGRESS)-1] -> [ULGCE-2079 (En Attente)-2]
[ULGCE-2079 (IN PROGRESS)-3] starts 2024/09/05 and ends 2024/09/06
[ULGCE-2079 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-2079 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2079]]
[ULGCE-2079 (En Attente)-2] -> [ULGCE-2079 (IN PROGRESS)-3]
-- ULGCE-1827 - [#968111] Evolutions TPV1 relatives aux règles de concordance, compte code paie 21 et pénalités à laisser en DI --
[ULGCE-1827 - création] happens on 2024/06/12
[ULGCE-1827 - création] -> [ULGCE-2108 #968111_928976_EVOLUTION_TESTS - création]
[ULGCE-2108 #968111_928976_EVOLUTION_TESTS - création] happens at 2024/08/27
[ULGCE-2108 #968111_928976_EVOLUTION_TESTS - création] -> [ULGCE-2108 (IN PROGRESS)-1]
[ULGCE-2108 (IN PROGRESS)-1] starts 2024/08/27 and ends 2024/09/05
[ULGCE-2108 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2108 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2108]]
[ULGCE-2108 (En Attente)-2] starts 2024/09/05 and ends 2024/09/06
[ULGCE-2108 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-2108 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2108]]
[ULGCE-2108 (IN PROGRESS)-1] -> [ULGCE-2108 (En Attente)-2]
[ULGCE-1827 - création] -> [ULGCE-2107 #968111_928976_EVOLUTION_DEVELOPPEMENT - création]
[ULGCE-2107 #968111_928976_EVOLUTION_DEVELOPPEMENT - création] happens at 2024/08/27
[ULGCE-2107 #968111_928976_EVOLUTION_DEVELOPPEMENT - création] -> [ULGCE-2107 (En Test)-1]
[ULGCE-2107 (En Test)-1] starts 2024/08/27 and ends 2024/08/27
[ULGCE-2107 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2107 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2107]]
[ULGCE-2107 (Terminée)-2] starts 2024/08/27 and ends 2024/09/03
[ULGCE-2107 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2107 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2107]]
[ULGCE-2107 (En Test)-1] -> [ULGCE-2107 (Terminée)-2]
[ULGCE-1827 - création] -> [ULGCE-2106 #968111_928976_EVOLUTION_CONCEPTION TECHNIQUE - création]
[ULGCE-2106 #968111_928976_EVOLUTION_CONCEPTION TECHNIQUE - création] happens at 2024/08/27
[ULGCE-2106 #968111_928976_EVOLUTION_CONCEPTION TECHNIQUE - création] -> [ULGCE-2106 (En Test)-1]
[ULGCE-2106 (En Test)-1] starts 2024/08/27 and ends 2024/08/27
[ULGCE-2106 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2106 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2106]]
[ULGCE-2106 (Terminée)-2] starts 2024/08/27 and ends 2024/09/04
[ULGCE-2106 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2106 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2106]]
[ULGCE-2106 (En Test)-1] -> [ULGCE-2106 (Terminée)-2]
[ULGCE-1827 - création] -> [ULGCE-2089 #968111_956789_EVOLUTION_TESTS - création]
[ULGCE-2089 #968111_956789_EVOLUTION_TESTS - création] happens at 2024/08/19
[ULGCE-2089 #968111_956789_EVOLUTION_TESTS - création] -> [ULGCE-2089 (En Test)-1]
[ULGCE-2089 (En Test)-1] starts 2024/08/19 and ends 2024/08/26
[ULGCE-2089 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2089 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2089]]
[ULGCE-1827 - création] -> [ULGCE-2088 #968111_956789_EVOLUTION_DEVELOPPEMENT - création]
[ULGCE-2088 #968111_956789_EVOLUTION_DEVELOPPEMENT - création] happens at 2024/08/19
[ULGCE-2088 #968111_956789_EVOLUTION_DEVELOPPEMENT - création] -> [ULGCE-2088 (En Test)-1]
[ULGCE-2088 (En Test)-1] starts 2024/08/19 and ends 2024/08/26
[ULGCE-2088 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2088 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2088]]
[ULGCE-2088 (Terminée)-2] starts 2024/08/26 and ends 2024/09/03
[ULGCE-2088 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2088 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2088]]
[ULGCE-2088 (En Test)-1] -> [ULGCE-2088 (Terminée)-2]
[ULGCE-1827 - création] -> [ULGCE-2078 #968111_956789_EVOLUTION_CONCEPTION TECHNIQUE - création]
[ULGCE-2078 #968111_956789_EVOLUTION_CONCEPTION TECHNIQUE - création] happens at 2024/08/14
[ULGCE-2078 #968111_956789_EVOLUTION_CONCEPTION TECHNIQUE - création] -> [ULGCE-2078 (En Test)-1]
[ULGCE-2078 (En Test)-1] starts 2024/08/14 and ends 2024/08/23
[ULGCE-2078 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2078 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2078]]
[ULGCE-2078 (Terminée)-2] starts 2024/08/23 and ends 2024/09/04
[ULGCE-2078 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2078 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2078]]
[ULGCE-2078 (En Test)-1] -> [ULGCE-2078 (Terminée)-2]
-- ULGCE-1855 - [#971767] TP78 et répartition des régularisations des cotisations sociales TI - Maj LCBL --
[ULGCE-1855 - création] happens on 2024/06/17
[ULGCE-1855 - création] -> [ULGCE-2104 #971767_955714_EVOLUTION_TESTS - création]
[ULGCE-2104 #971767_955714_EVOLUTION_TESTS - création] happens at 2024/08/26
[ULGCE-2104 #971767_955714_EVOLUTION_TESTS - création] -> [ULGCE-2104 (IN PROGRESS)-1]
[ULGCE-2104 (IN PROGRESS)-1] starts 2024/08/26 and ends 2024/08/29
[ULGCE-2104 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2104 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2104]]
[ULGCE-1855 - création] -> [ULGCE-2103 #971767_955714_EVOLUTION_DEVELOPPEMENT - création]
[ULGCE-2103 #971767_955714_EVOLUTION_DEVELOPPEMENT - création] happens at 2024/08/26
[ULGCE-2103 #971767_955714_EVOLUTION_DEVELOPPEMENT - création] -> [ULGCE-2103 (En Test)-1]
[ULGCE-2103 (En Test)-1] starts 2024/08/26 and ends 2024/08/28
[ULGCE-2103 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2103 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2103]]
[ULGCE-2103 (Terminée)-2] starts 2024/08/28 and ends 2024/09/06
[ULGCE-2103 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2103 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2103]]
[ULGCE-2103 (En Test)-1] -> [ULGCE-2103 (Terminée)-2]
[ULGCE-1855 - création] -> [ULGCE-1938 #971767_955714_EVOLUTION_TESTS - création]
[ULGCE-1938 #971767_955714_EVOLUTION_TESTS - création] happens at 2024/07/04
[ULGCE-1938 #971767_955714_EVOLUTION_TESTS - création] -> [ULGCE-1938 (IN PROGRESS)-1]
[ULGCE-1938 (IN PROGRESS)-1] starts 2024/07/04 and ends 2024/07/12
[ULGCE-1938 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1938 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1938]]
[ULGCE-1938 (En Attente)-2] starts 2024/07/12 and ends 2024/07/12
[ULGCE-1938 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1938 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1938]]
[ULGCE-1938 (IN PROGRESS)-1] -> [ULGCE-1938 (En Attente)-2]
[ULGCE-1855 - création] -> [ULGCE-1937 #971767_955714_EVOLUTION_DEVELOPPEMENT - création]
[ULGCE-1937 #971767_955714_EVOLUTION_DEVELOPPEMENT - création] happens at 2024/07/04
[ULGCE-1937 #971767_955714_EVOLUTION_DEVELOPPEMENT - création] -> [ULGCE-1937 (IN PROGRESS)-1]
[ULGCE-1937 (IN PROGRESS)-1] starts 2024/07/04 and ends 2024/07/12
[ULGCE-1937 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1937 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1937]]
[ULGCE-1937 (En Attente)-2] starts 2024/07/12 and ends 2024/07/18
[ULGCE-1937 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1937 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1937]]
[ULGCE-1937 (IN PROGRESS)-1] -> [ULGCE-1937 (En Attente)-2]
[ULGCE-1855 - création] -> [ULGCE-1936 #971767_955714_EVOLUTION_CONCEPTION TECHNIQUE - création]
[ULGCE-1936 #971767_955714_EVOLUTION_CONCEPTION TECHNIQUE - création] happens at 2024/07/04
[ULGCE-1936 #971767_955714_EVOLUTION_CONCEPTION TECHNIQUE - création] -> [ULGCE-1936 (IN PROGRESS)-1]
[ULGCE-1936 (IN PROGRESS)-1] starts 2024/07/04 and ends 2024/07/05
[ULGCE-1936 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1936 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1936]]
[ULGCE-1936 (En Attente)-2] starts 2024/07/05 and ends 2024/07/05
[ULGCE-1936 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1936 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1936]]
[ULGCE-1936 (IN PROGRESS)-1] -> [ULGCE-1936 (En Attente)-2]
[ULGCE-1936 (IN PROGRESS)-3] starts 2024/07/05 and ends 2024/07/12
[ULGCE-1936 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1936 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1936]]
[ULGCE-1936 (En Attente)-2] -> [ULGCE-1936 (IN PROGRESS)-3]
[ULGCE-1936 (En Test)-4] starts 2024/07/12 and ends 2024/07/12
[ULGCE-1936 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1936 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1936]]
[ULGCE-1936 (IN PROGRESS)-3] -> [ULGCE-1936 (En Test)-4]
[ULGCE-1936 (Terminée)-5] starts 2024/07/12 and ends 2024/07/12
[ULGCE-1936 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1936 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1936]]
[ULGCE-1936 (En Test)-4] -> [ULGCE-1936 (Terminée)-5]
[ULGCE-1855 - création] -> [ULGCE-1915 #971767_972037_EVOLUTION_TESTS - création]
[ULGCE-1915 #971767_972037_EVOLUTION_TESTS - création] happens at 2024/07/01
[ULGCE-1915 #971767_972037_EVOLUTION_TESTS - création] -> [ULGCE-1915 (IN PROGRESS)-1]
[ULGCE-1915 (IN PROGRESS)-1] starts 2024/07/01 and ends 2024/07/04
[ULGCE-1915 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1915 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1915]]
[ULGCE-1915 (En Attente)-2] starts 2024/07/04 and ends 2024/07/05
[ULGCE-1915 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1915 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1915]]
[ULGCE-1915 (IN PROGRESS)-1] -> [ULGCE-1915 (En Attente)-2]
[ULGCE-1915 (IN PROGRESS)-3] starts 2024/07/05 and ends 2024/07/18
[ULGCE-1915 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1915 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1915]]
[ULGCE-1915 (En Attente)-2] -> [ULGCE-1915 (IN PROGRESS)-3]
[ULGCE-1915 (En Attente)-4] starts 2024/07/18 and ends 2024/08/02
[ULGCE-1915 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1915 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1915]]
[ULGCE-1915 (IN PROGRESS)-3] -> [ULGCE-1915 (En Attente)-4]
[ULGCE-1915 (IN PROGRESS)-5] starts 2024/08/02 and ends 2024/08/02
[ULGCE-1915 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1915 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1915]]
[ULGCE-1915 (En Attente)-4] -> [ULGCE-1915 (IN PROGRESS)-5]
[ULGCE-1915 (En Test)-6] starts 2024/08/02 and ends 2024/08/02
[ULGCE-1915 (En Test)-6] is colored in LightGray/Gray
[ULGCE-1915 (En Test)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1915]]
[ULGCE-1915 (IN PROGRESS)-5] -> [ULGCE-1915 (En Test)-6]
[ULGCE-1915 (Terminée)-7] starts 2024/08/02 and ends 2024/08/02
[ULGCE-1915 (Terminée)-7] is colored in LightGray/Gray
[ULGCE-1915 (Terminée)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1915]]
[ULGCE-1915 (En Test)-6] -> [ULGCE-1915 (Terminée)-7]
[ULGCE-1855 - création] -> [ULGCE-1914 #971767_972037_EVOLUTION_DEVELOPPEMENT - création]
[ULGCE-1914 #971767_972037_EVOLUTION_DEVELOPPEMENT - création] happens at 2024/07/01
[ULGCE-1914 #971767_972037_EVOLUTION_DEVELOPPEMENT - création] -> [ULGCE-1914 (En Test)-1]
[ULGCE-1914 (En Test)-1] starts 2024/07/01 and ends 2024/07/04
[ULGCE-1914 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1914 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1914]]
[ULGCE-1914 (Terminée)-2] starts 2024/07/04 and ends 2024/07/04
[ULGCE-1914 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1914 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1914]]
[ULGCE-1914 (En Test)-1] -> [ULGCE-1914 (Terminée)-2]
[ULGCE-1914 (A faire)-3] starts 2024/07/04 and ends 2024/08/01
[ULGCE-1914 (A faire)-3] is colored in LightGray/Gray
[ULGCE-1914 (A faire)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1914]]
[ULGCE-1914 (Terminée)-2] -> [ULGCE-1914 (A faire)-3]
[ULGCE-1914 (IN PROGRESS)-4] starts 2024/08/01 and ends 2024/08/01
[ULGCE-1914 (IN PROGRESS)-4] is colored in LightGray/Gray
[ULGCE-1914 (IN PROGRESS)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1914]]
[ULGCE-1914 (A faire)-3] -> [ULGCE-1914 (IN PROGRESS)-4]
[ULGCE-1914 (En Test)-5] starts 2024/08/01 and ends 2024/08/01
[ULGCE-1914 (En Test)-5] is colored in LightGray/Gray
[ULGCE-1914 (En Test)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1914]]
[ULGCE-1914 (IN PROGRESS)-4] -> [ULGCE-1914 (En Test)-5]
[ULGCE-1914 (Terminée)-6] starts 2024/08/01 and ends 2024/08/01
[ULGCE-1914 (Terminée)-6] is colored in LightGray/Gray
[ULGCE-1914 (Terminée)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1914]]
[ULGCE-1914 (En Test)-5] -> [ULGCE-1914 (Terminée)-6]
[ULGCE-1855 - création] -> [ULGCE-1913 #971767_972037_EVOLUTION_CONCEPTION TECHNIQUE - création]
[ULGCE-1913 #971767_972037_EVOLUTION_CONCEPTION TECHNIQUE - création] happens at 2024/07/01
[ULGCE-1913 #971767_972037_EVOLUTION_CONCEPTION TECHNIQUE - création] -> [ULGCE-1913 (IN PROGRESS)-1]
[ULGCE-1913 (IN PROGRESS)-1] starts 2024/07/01 and ends 2024/07/04
[ULGCE-1913 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1913 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1913]]
[ULGCE-1913 (En Attente)-2] starts 2024/07/04 and ends 2024/07/04
[ULGCE-1913 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1913 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1913]]
[ULGCE-1913 (IN PROGRESS)-1] -> [ULGCE-1913 (En Attente)-2]
[ULGCE-1913 (IN PROGRESS)-3] starts 2024/07/04 and ends 2024/07/12
[ULGCE-1913 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1913 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1913]]
[ULGCE-1913 (En Attente)-2] -> [ULGCE-1913 (IN PROGRESS)-3]
[ULGCE-1913 (En Test)-4] starts 2024/07/12 and ends 2024/07/12
[ULGCE-1913 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1913 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1913]]
[ULGCE-1913 (IN PROGRESS)-3] -> [ULGCE-1913 (En Test)-4]
[ULGCE-1913 (Terminée)-5] starts 2024/07/12 and ends 2024/07/12
[ULGCE-1913 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1913 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1913]]
[ULGCE-1913 (En Test)-4] -> [ULGCE-1913 (Terminée)-5]
[ULGCE-1855 - création] -> [ULGCE-1870 #971767_971767_DEVIS - création]
[ULGCE-1870 #971767_971767_DEVIS - création] happens at 2024/06/20
[ULGCE-1870 #971767_971767_DEVIS - création] -> [ULGCE-1870 (En Test)-1]
[ULGCE-1870 (En Test)-1] starts 2024/06/20 and ends 2024/06/21
[ULGCE-1870 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1870 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1870]]
[ULGCE-1870 (Terminée)-2] starts 2024/06/21 and ends 2024/06/21
[ULGCE-1870 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1870 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1870]]
[ULGCE-1870 (En Test)-1] -> [ULGCE-1870 (Terminée)-2]
-- ULGCE-2052 - [#992919] Comptes DSN code échéance 62 suite TP81 (ano 241 dans le TP78/TDBC) - Ecritures erronées et débit recouvrement générés à tort --
[ULGCE-2052 - création] happens on 2024/08/08
[ULGCE-2052 - création] -> [ULGCE-2067 #992919_992919_ANOMALIE_MCO-TESTS - création]
[ULGCE-2067 #992919_992919_ANOMALIE_MCO-TESTS - création] happens at 2024/08/13
[ULGCE-2067 #992919_992919_ANOMALIE_MCO-TESTS - création] -> [ULGCE-2067 (En Test)-1]
[ULGCE-2067 (En Test)-1] starts 2024/08/13 and ends 2024/08/21
[ULGCE-2067 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2067 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2067]]
[ULGCE-2067 (Terminée)-2] starts 2024/08/21 and ends 2024/08/21
[ULGCE-2067 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2067 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2067]]
[ULGCE-2067 (En Test)-1] -> [ULGCE-2067 (Terminée)-2]
[ULGCE-2052 - création] -> [ULGCE-2066 #992919_992919_ANOMALIE_MCO-DEVELOPPEMENT - création]
[ULGCE-2066 #992919_992919_ANOMALIE_MCO-DEVELOPPEMENT - création] happens at 2024/08/13
[ULGCE-2066 #992919_992919_ANOMALIE_MCO-DEVELOPPEMENT - création] -> [ULGCE-2066 (En Test)-1]
[ULGCE-2066 (En Test)-1] starts 2024/08/13 and ends 2024/08/21
[ULGCE-2066 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2066 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2066]]
[ULGCE-2066 (Terminée)-2] starts 2024/08/21 and ends 2024/08/21
[ULGCE-2066 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2066 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2066]]
[ULGCE-2066 (En Test)-1] -> [ULGCE-2066 (Terminée)-2]
[ULGCE-2052 - création] -> [ULGCE-2065 #992919_992919_ANOMALIE_MCO-ANALYSE - création]
[ULGCE-2065 #992919_992919_ANOMALIE_MCO-ANALYSE - création] happens at 2024/08/13
[ULGCE-2065 #992919_992919_ANOMALIE_MCO-ANALYSE - création] -> [ULGCE-2065 (En Test)-1]
[ULGCE-2065 (En Test)-1] starts 2024/08/13 and ends 2024/08/14
[ULGCE-2065 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2065 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2065]]
[ULGCE-2065 (Terminée)-2] starts 2024/08/14 and ends 2024/08/21
[ULGCE-2065 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2065 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2065]]
[ULGCE-2065 (En Test)-1] -> [ULGCE-2065 (Terminée)-2]
-- ULGCE-2053 - [#991653] Comptes DSN code échéance 62 suite TP81 (ano 241 dans le TP78/TDBC) - Ecritures erronées et débit recouvrement générés à tort --
[ULGCE-2053 - création] happens on 2024/08/08
[ULGCE-2053 - création] -> [ULGCE-2064 #991653_991653_ANOMALIE_MCO-TESTS - création]
[ULGCE-2064 #991653_991653_ANOMALIE_MCO-TESTS - création] happens at 2024/08/13
[ULGCE-2064 #991653_991653_ANOMALIE_MCO-TESTS - création] -> [ULGCE-2064 (En Test)-1]
[ULGCE-2064 (En Test)-1] starts 2024/08/13 and ends 2024/08/21
[ULGCE-2064 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2064 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2064]]
[ULGCE-2064 (Terminée)-2] starts 2024/08/21 and ends 2024/08/23
[ULGCE-2064 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2064 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2064]]
[ULGCE-2064 (En Test)-1] -> [ULGCE-2064 (Terminée)-2]
[ULGCE-2053 - création] -> [ULGCE-2063 #991653_991653_ANOMALIE_MCO-DEVELOPPEMENT - création]
[ULGCE-2063 #991653_991653_ANOMALIE_MCO-DEVELOPPEMENT - création] happens at 2024/08/13
[ULGCE-2063 #991653_991653_ANOMALIE_MCO-DEVELOPPEMENT - création] -> [ULGCE-2063 (En Test)-1]
[ULGCE-2063 (En Test)-1] starts 2024/08/13 and ends 2024/08/21
[ULGCE-2063 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2063 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2063]]
[ULGCE-2063 (Terminée)-2] starts 2024/08/21 and ends 2024/08/23
[ULGCE-2063 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2063 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2063]]
[ULGCE-2063 (En Test)-1] -> [ULGCE-2063 (Terminée)-2]
[ULGCE-2053 - création] -> [ULGCE-2062 #991653_991653_ANOMALIE_MCO-ANALYSE - création]
[ULGCE-2062 #991653_991653_ANOMALIE_MCO-ANALYSE - création] happens at 2024/08/13
[ULGCE-2062 #991653_991653_ANOMALIE_MCO-ANALYSE - création] -> [ULGCE-2062 (En Test)-1]
[ULGCE-2062 (En Test)-1] starts 2024/08/13 and ends 2024/08/13
[ULGCE-2062 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2062 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2062]]
[ULGCE-2062 (Terminée)-2] starts 2024/08/13 and ends 2024/08/21
[ULGCE-2062 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2062 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2062]]
[ULGCE-2062 (En Test)-1] -> [ULGCE-2062 (Terminée)-2]
-- ULGCE-2060 - [#993588] PASRAU - OBP1 - Régularisations créditrices : Absence de message pour un compte V avec SP36 sur la période --
[ULGCE-2060 - création] happens on 2024/08/12
[ULGCE-2060 - création] -> [ULGCE-2098 #993588_993588_ANOMALIE-MCO-ANALYSE - création]
[ULGCE-2098 #993588_993588_ANOMALIE-MCO-ANALYSE - création] happens at 2024/08/23
[ULGCE-2098 #993588_993588_ANOMALIE-MCO-ANALYSE - création] -> [ULGCE-2098 (En Test)-1]
[ULGCE-2098 (En Test)-1] starts 2024/08/23 and ends 2024/08/27
[ULGCE-2098 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2098 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2098]]
[ULGCE-2098 (Terminée)-2] starts 2024/08/27 and ends 2024/08/27
[ULGCE-2098 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2098 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2098]]
[ULGCE-2098 (En Test)-1] -> [ULGCE-2098 (Terminée)-2]
-- ULGCE-2061 - [#994461] Contrôle à Postériori - Revert du ticket #964936 --
[ULGCE-2061 - création] happens on 2024/08/13
[ULGCE-2061 - création] -> [ULGCE-2070 #994461_994461_ANOMALIE_MCO-TESTS - création]
[ULGCE-2070 #994461_994461_ANOMALIE_MCO-TESTS - création] happens at 2024/08/13
[ULGCE-2070 #994461_994461_ANOMALIE_MCO-TESTS - création] -> [ULGCE-2070 (En Test)-1]
[ULGCE-2070 (En Test)-1] starts 2024/08/13 and ends 2024/08/19
[ULGCE-2070 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2070 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2070]]
[ULGCE-2070 (Terminée)-2] starts 2024/08/19 and ends 2024/08/19
[ULGCE-2070 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2070 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2070]]
[ULGCE-2070 (En Test)-1] -> [ULGCE-2070 (Terminée)-2]
[ULGCE-2061 - création] -> [ULGCE-2069 #994461_994461_ANOMALIE_MCO-DEVELOPPEMENT - création]
[ULGCE-2069 #994461_994461_ANOMALIE_MCO-DEVELOPPEMENT - création] happens at 2024/08/13
[ULGCE-2069 #994461_994461_ANOMALIE_MCO-DEVELOPPEMENT - création] -> [ULGCE-2069 (En Test)-1]
[ULGCE-2069 (En Test)-1] starts 2024/08/13 and ends 2024/08/19
[ULGCE-2069 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2069 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2069]]
[ULGCE-2069 (Terminée)-2] starts 2024/08/19 and ends 2024/08/19
[ULGCE-2069 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2069 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2069]]
[ULGCE-2069 (En Test)-1] -> [ULGCE-2069 (Terminée)-2]
[ULGCE-2061 - création] -> [ULGCE-2068 #994461_994461_ANOMALIE_MCO-ANALYSE - création]
[ULGCE-2068 #994461_994461_ANOMALIE_MCO-ANALYSE - création] happens at 2024/08/13
[ULGCE-2068 #994461_994461_ANOMALIE_MCO-ANALYSE - création] -> [ULGCE-2068 (En Test)-1]
[ULGCE-2068 (En Test)-1] starts 2024/08/13 and ends 2024/08/14
[ULGCE-2068 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2068 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2068]]
[ULGCE-2068 (Terminée)-2] starts 2024/08/14 and ends 2024/08/19
[ULGCE-2068 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2068 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2068]]
[ULGCE-2068 (En Test)-1] -> [ULGCE-2068 (Terminée)-2]
-- ULGCE-2101 - Formation LOT4 --
[ULGCE-2101 - création] happens on 2024/08/23
[ULGCE-2101 - création] -> [ULGCE-2151 Formation Lot 4 - Nathalie - création]
[ULGCE-2151 Formation Lot 4 - Nathalie - création] happens at 2024/09/06
[ULGCE-2101 - création] -> [ULGCE-2147 Formation Lot 4 - Christophe - création]
[ULGCE-2147 Formation Lot 4 - Christophe - création] happens at 2024/09/06
[ULGCE-2147 Formation Lot 4 - Christophe - création] -> [ULGCE-2147 (IN PROGRESS)-1]
[ULGCE-2147 (IN PROGRESS)-1] starts 2024/09/06 and ends 2024/09/06
[ULGCE-2147 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2147 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2147]]
[ULGCE-2101 - création] -> [ULGCE-2102 Formation Lot 4 - Rudy  - création]
[ULGCE-2102 Formation Lot 4 - Rudy  - création] happens at 2024/08/23
[ULGCE-2102 Formation Lot 4 - Rudy  - création] -> [ULGCE-2102 (IN PROGRESS)-1]
[ULGCE-2102 (IN PROGRESS)-1] starts 2024/08/23 and ends 2024/08/23
[ULGCE-2102 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2102 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2102]]
-- ULGCE-2111 - [#991480] Contrôle a posteriori - mauvais ciblage des années considérées --
[ULGCE-2111 - création] happens on 2024/08/28
[ULGCE-2111 - création] -> [ULGCE-2125 #991480_991480_ANOMALIE_MCO-TESTS - création]
[ULGCE-2125 #991480_991480_ANOMALIE_MCO-TESTS - création] happens at 2024/09/02
[ULGCE-2111 - création] -> [ULGCE-2124 #991480_991480_ANOMALIE_MCO-DEVELOPPEMENT - création]
[ULGCE-2124 #991480_991480_ANOMALIE_MCO-DEVELOPPEMENT - création] happens at 2024/09/02
[ULGCE-2124 #991480_991480_ANOMALIE_MCO-DEVELOPPEMENT - création] -> [ULGCE-2124 (En Test)-1]
[ULGCE-2124 (En Test)-1] starts 2024/09/02 and ends 2024/09/05
[ULGCE-2124 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2124 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2124]]
[ULGCE-2124 (A faire)-2] starts 2024/09/05 and ends 2024/09/06
[ULGCE-2124 (A faire)-2] is colored in LightGray/Gray
[ULGCE-2124 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2124]]
[ULGCE-2124 (En Test)-1] -> [ULGCE-2124 (A faire)-2]
[ULGCE-2124 (IN PROGRESS)-3] starts 2024/09/06 and ends 2024/09/06
[ULGCE-2124 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-2124 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2124]]
[ULGCE-2124 (A faire)-2] -> [ULGCE-2124 (IN PROGRESS)-3]
[ULGCE-2124 (En Attente)-4] starts 2024/09/06 and ends 2024/09/06
[ULGCE-2124 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-2124 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2124]]
[ULGCE-2124 (IN PROGRESS)-3] -> [ULGCE-2124 (En Attente)-4]
[ULGCE-2111 - création] -> [ULGCE-2119 #991480_991480_ANOMALIE_MCO-ANALYSE - création]
[ULGCE-2119 #991480_991480_ANOMALIE_MCO-ANALYSE - création] happens at 2024/09/02
[ULGCE-2119 #991480_991480_ANOMALIE_MCO-ANALYSE - création] -> [ULGCE-2119 (En Test)-1]
[ULGCE-2119 (En Test)-1] starts 2024/09/02 and ends 2024/09/04
[ULGCE-2119 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2119 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2119]]
[ULGCE-2119 (Terminée)-2] starts 2024/09/04 and ends 2024/09/05
[ULGCE-2119 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2119 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2119]]
[ULGCE-2119 (En Test)-1] -> [ULGCE-2119 (Terminée)-2]
-- ULGCE-2115 - [#991476] Contrôle a posteriori - non respect des exclusions ni du seuil de régul créditrice --
[ULGCE-2115 - création] happens on 2024/08/28
[ULGCE-2115 - création] -> [ULGCE-2128 #991476_991476_ANOMALIE_MCO-TESTS - création]
[ULGCE-2128 #991476_991476_ANOMALIE_MCO-TESTS - création] happens at 2024/09/02
[ULGCE-2128 #991476_991476_ANOMALIE_MCO-TESTS - création] -> [ULGCE-2128 (En Test)-1]
[ULGCE-2128 (En Test)-1] starts 2024/09/02 and ends 2024/09/06
[ULGCE-2128 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2128 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2128]]
[ULGCE-2115 - création] -> [ULGCE-2127 #991476_991476_ANOMALIE_MCO-DEVELOPPEMENT - création]
[ULGCE-2127 #991476_991476_ANOMALIE_MCO-DEVELOPPEMENT - création] happens at 2024/09/02
[ULGCE-2127 #991476_991476_ANOMALIE_MCO-DEVELOPPEMENT - création] -> [ULGCE-2127 (En Test)-1]
[ULGCE-2127 (En Test)-1] starts 2024/09/02 and ends 2024/09/05
[ULGCE-2127 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2127 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2127]]
[ULGCE-2127 (Terminée)-2] starts 2024/09/05 and ends 2024/09/05
[ULGCE-2127 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2127 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2127]]
[ULGCE-2127 (En Test)-1] -> [ULGCE-2127 (Terminée)-2]
[ULGCE-2115 - création] -> [ULGCE-2126 #991476_991476_ANOMALIE_MCO-ANALYSE - création]
[ULGCE-2126 #991476_991476_ANOMALIE_MCO-ANALYSE - création] happens at 2024/09/02
[ULGCE-2126 #991476_991476_ANOMALIE_MCO-ANALYSE - création] -> [ULGCE-2126 (En Test)-1]
[ULGCE-2126 (En Test)-1] starts 2024/09/02 and ends 2024/09/04
[ULGCE-2126 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2126 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2126]]
[ULGCE-2126 (Terminée)-2] starts 2024/09/04 and ends 2024/09/05
[ULGCE-2126 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2126 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2126]]
[ULGCE-2126 (En Test)-1] -> [ULGCE-2126 (Terminée)-2]
-- ULGCE-2141 - TESTS --
[ULGCE-2141 - création] happens on 2024/09/06
[ULGCE-2141 - création] -> [ULGCE-2146 montée en compétence - création]
[ULGCE-2146 montée en compétence - création] happens at 2024/09/06
[ULGCE-2146 montée en compétence - création] -> [ULGCE-2146 (IN PROGRESS)-1]
[ULGCE-2146 (IN PROGRESS)-1] starts 2024/09/06 and ends 2024/09/06
[ULGCE-2146 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2146 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2146]]
[ULGCE-2141 - création] -> [ULGCE-2143 MISE EN PLACE  - création]
[ULGCE-2143 MISE EN PLACE  - création] happens at 2024/09/06
[ULGCE-2141 - création] -> [ULGCE-2142 MISE EN PLACE  - création]
[ULGCE-2142 MISE EN PLACE  - création] happens at 2024/09/06
[ULGCE-2142 MISE EN PLACE  - création] -> [ULGCE-2142 (IN PROGRESS)-1]
[ULGCE-2142 (IN PROGRESS)-1] starts 2024/09/06 and ends 2024/09/06
[ULGCE-2142 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2142 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2142]]
-- ULGCE-505 - [#822678] P20-185 - PERF-LOT1.2 : Evolution de la répartition forfaitaire à recycler cat 2,4,9 --
[ULGCE-505 - création] happens on 2023/11/24
[ULGCE-505 - création] -> [ULGCE-2014 #822678_822678_PROJET_RETOUR RECETTE - création]
[ULGCE-2014 #822678_822678_PROJET_RETOUR RECETTE - création] happens at 2024/07/29
[ULGCE-2014 #822678_822678_PROJET_RETOUR RECETTE - création] -> [ULGCE-2014 (En Test)-1]
[ULGCE-2014 (En Test)-1] starts 2024/07/29 and ends 2024/07/29
[ULGCE-2014 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2014 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2014]]
[ULGCE-2014 (A faire)-2] starts 2024/07/29 and ends 2024/07/31
[ULGCE-2014 (A faire)-2] is colored in LightGray/Gray
[ULGCE-2014 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2014]]
[ULGCE-2014 (En Test)-1] -> [ULGCE-2014 (A faire)-2]
[ULGCE-2014 (IN PROGRESS)-3] starts 2024/07/31 and ends 2024/07/31
[ULGCE-2014 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-2014 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2014]]
[ULGCE-2014 (A faire)-2] -> [ULGCE-2014 (IN PROGRESS)-3]
[ULGCE-2014 (En Attente)-4] starts 2024/07/31 and ends 2024/07/31
[ULGCE-2014 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-2014 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2014]]
[ULGCE-2014 (IN PROGRESS)-3] -> [ULGCE-2014 (En Attente)-4]
[ULGCE-2014 (A faire)-5] starts 2024/07/31 and ends 2024/08/23
[ULGCE-2014 (A faire)-5] is colored in LightGray/Gray
[ULGCE-2014 (A faire)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2014]]
[ULGCE-2014 (En Attente)-4] -> [ULGCE-2014 (A faire)-5]
[ULGCE-2014 (IN PROGRESS)-6] starts 2024/08/23 and ends 2024/08/23
[ULGCE-2014 (IN PROGRESS)-6] is colored in LightGray/Gray
[ULGCE-2014 (IN PROGRESS)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2014]]
[ULGCE-2014 (A faire)-5] -> [ULGCE-2014 (IN PROGRESS)-6]
[ULGCE-2014 (En Attente)-7] starts 2024/08/23 and ends 2024/08/28
[ULGCE-2014 (En Attente)-7] is colored in LightGray/Gray
[ULGCE-2014 (En Attente)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2014]]
[ULGCE-2014 (IN PROGRESS)-6] -> [ULGCE-2014 (En Attente)-7]
[ULGCE-505 - création] -> [ULGCE-1214 #822678-Perf Lot 1.2 - Support pour les tests - création]
[ULGCE-1214 #822678-Perf Lot 1.2 - Support pour les tests - création] happens at 2024/02/16
[ULGCE-1214 #822678-Perf Lot 1.2 - Support pour les tests - création] -> [ULGCE-1214 (IN PROGRESS)-1]
[ULGCE-1214 (IN PROGRESS)-1] starts 2024/02/16 and ends 2024/02/16
[ULGCE-1214 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1214 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1214]]
[ULGCE-1214 (En Test)-2] starts 2024/02/16 and ends 2024/02/25
[ULGCE-1214 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1214 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1214]]
[ULGCE-1214 (IN PROGRESS)-1] -> [ULGCE-1214 (En Test)-2]
[ULGCE-1214 (Terminée)-3] starts 2024/02/25 and ends 2024/02/25
[ULGCE-1214 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1214 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1214]]
[ULGCE-1214 (En Test)-2] -> [ULGCE-1214 (Terminée)-3]
[ULGCE-505 - création] -> [ULGCE-1159 #822678_822678_PROJET_PACKAGING/LIVRAISON - création]
[ULGCE-1159 #822678_822678_PROJET_PACKAGING/LIVRAISON - création] happens at 2024/02/09
[ULGCE-1159 #822678_822678_PROJET_PACKAGING/LIVRAISON - création] -> [ULGCE-1159 (IN PROGRESS)-1]
[ULGCE-1159 (IN PROGRESS)-1] starts 2024/02/09 and ends 2024/02/15
[ULGCE-1159 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1159 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1159]]
[ULGCE-1159 (En Attente)-2] starts 2024/02/15 and ends 2024/02/15
[ULGCE-1159 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1159 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1159]]
[ULGCE-1159 (IN PROGRESS)-1] -> [ULGCE-1159 (En Attente)-2]
[ULGCE-1159 (IN PROGRESS)-3] starts 2024/02/15 and ends 2024/02/15
[ULGCE-1159 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1159 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1159]]
[ULGCE-1159 (En Attente)-2] -> [ULGCE-1159 (IN PROGRESS)-3]
[ULGCE-1159 (En Attente)-4] starts 2024/02/15 and ends 2024/02/16
[ULGCE-1159 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1159 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1159]]
[ULGCE-1159 (IN PROGRESS)-3] -> [ULGCE-1159 (En Attente)-4]
[ULGCE-1159 (A faire)-5] starts 2024/02/16 and ends 2024/02/26
[ULGCE-1159 (A faire)-5] is colored in LightGray/Gray
[ULGCE-1159 (A faire)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1159]]
[ULGCE-1159 (En Attente)-4] -> [ULGCE-1159 (A faire)-5]
[ULGCE-1159 (IN PROGRESS)-6] starts 2024/02/26 and ends 2024/04/10
[ULGCE-1159 (IN PROGRESS)-6] is colored in LightGray/Gray
[ULGCE-1159 (IN PROGRESS)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1159]]
[ULGCE-1159 (A faire)-5] -> [ULGCE-1159 (IN PROGRESS)-6]
[ULGCE-1159 (En Test)-7] starts 2024/04/10 and ends 2024/04/15
[ULGCE-1159 (En Test)-7] is colored in LightGray/Gray
[ULGCE-1159 (En Test)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1159]]
[ULGCE-1159 (IN PROGRESS)-6] -> [ULGCE-1159 (En Test)-7]
[ULGCE-1159 (Terminée)-8] starts 2024/04/15 and ends 2024/04/15
[ULGCE-1159 (Terminée)-8] is colored in LightGray/Gray
[ULGCE-1159 (Terminée)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1159]]
[ULGCE-1159 (En Test)-7] -> [ULGCE-1159 (Terminée)-8]
[ULGCE-505 - création] -> [ULGCE-1105 #822678_822678_PROJET_DEVELOPPEMENT - création]
[ULGCE-1105 #822678_822678_PROJET_DEVELOPPEMENT - création] happens at 2024/02/02
[ULGCE-1105 #822678_822678_PROJET_DEVELOPPEMENT - création] -> [ULGCE-1105 (IN PROGRESS)-1]
[ULGCE-1105 (IN PROGRESS)-1] starts 2024/02/02 and ends 2024/02/05
[ULGCE-1105 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1105 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1105]]
[ULGCE-1105 (En Test)-2] starts 2024/02/05 and ends 2024/02/09
[ULGCE-1105 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1105 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1105]]
[ULGCE-1105 (IN PROGRESS)-1] -> [ULGCE-1105 (En Test)-2]
[ULGCE-1105 (Terminée)-3] starts 2024/02/09 and ends 2024/02/09
[ULGCE-1105 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1105 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1105]]
[ULGCE-1105 (En Test)-2] -> [ULGCE-1105 (Terminée)-3]
[ULGCE-505 - création] -> [ULGCE-1104 #822678_822678_PROJET_TESTS - création]
[ULGCE-1104 #822678_822678_PROJET_TESTS - création] happens at 2024/02/02
[ULGCE-1104 #822678_822678_PROJET_TESTS - création] -> [ULGCE-1104 (En Test)-1]
[ULGCE-1104 (En Test)-1] starts 2024/02/02 and ends 2024/02/05
[ULGCE-1104 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1104 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (A faire)-2] starts 2024/02/05 and ends 2024/02/07
[ULGCE-1104 (A faire)-2] is colored in LightGray/Gray
[ULGCE-1104 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (En Test)-1] -> [ULGCE-1104 (A faire)-2]
[ULGCE-1104 (En Test)-3] starts 2024/02/07 and ends 2024/02/07
[ULGCE-1104 (En Test)-3] is colored in LightGray/Gray
[ULGCE-1104 (En Test)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (A faire)-2] -> [ULGCE-1104 (En Test)-3]
[ULGCE-1104 (A faire)-4] starts 2024/02/07 and ends 2024/02/07
[ULGCE-1104 (A faire)-4] is colored in LightGray/Gray
[ULGCE-1104 (A faire)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (En Test)-3] -> [ULGCE-1104 (A faire)-4]
[ULGCE-1104 (IN PROGRESS)-5] starts 2024/02/07 and ends 2024/02/07
[ULGCE-1104 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1104 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (A faire)-4] -> [ULGCE-1104 (IN PROGRESS)-5]
[ULGCE-1104 (En Attente)-6] starts 2024/02/07 and ends 2024/02/07
[ULGCE-1104 (En Attente)-6] is colored in LightGray/Gray
[ULGCE-1104 (En Attente)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (IN PROGRESS)-5] -> [ULGCE-1104 (En Attente)-6]
[ULGCE-1104 (IN PROGRESS)-7] starts 2024/02/07 and ends 2024/02/08
[ULGCE-1104 (IN PROGRESS)-7] is colored in LightGray/Gray
[ULGCE-1104 (IN PROGRESS)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (En Attente)-6] -> [ULGCE-1104 (IN PROGRESS)-7]
[ULGCE-1104 (En Attente)-8] starts 2024/02/08 and ends 2024/02/16
[ULGCE-1104 (En Attente)-8] is colored in LightGray/Gray
[ULGCE-1104 (En Attente)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (IN PROGRESS)-7] -> [ULGCE-1104 (En Attente)-8]
[ULGCE-1104 (A faire)-9] starts 2024/02/16 and ends 2024/02/25
[ULGCE-1104 (A faire)-9] is colored in LightGray/Gray
[ULGCE-1104 (A faire)-9] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (En Attente)-8] -> [ULGCE-1104 (A faire)-9]
[ULGCE-1104 (En Test)-10] starts 2024/02/25 and ends 2024/02/25
[ULGCE-1104 (En Test)-10] is colored in LightGray/Gray
[ULGCE-1104 (En Test)-10] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (A faire)-9] -> [ULGCE-1104 (En Test)-10]
[ULGCE-1104 (Terminée)-11] starts 2024/02/25 and ends 2024/02/25
[ULGCE-1104 (Terminée)-11] is colored in LightGray/Gray
[ULGCE-1104 (Terminée)-11] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1104]]
[ULGCE-1104 (En Test)-10] -> [ULGCE-1104 (Terminée)-11]
[ULGCE-505 - création] -> [ULGCE-1054 #822678_822678_PROJET_TESTS - création]
[ULGCE-1054 #822678_822678_PROJET_TESTS - création] happens at 2024/01/25
[ULGCE-1054 #822678_822678_PROJET_TESTS - création] -> [ULGCE-1054 (En Test)-1]
[ULGCE-1054 (En Test)-1] starts 2024/01/25 and ends 2024/01/25
[ULGCE-1054 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1054 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1054]]
[ULGCE-1054 (Terminée)-2] starts 2024/01/25 and ends 2024/02/04
[ULGCE-1054 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1054 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1054]]
[ULGCE-1054 (En Test)-1] -> [ULGCE-1054 (Terminée)-2]
[ULGCE-505 - création] -> [ULGCE-1053 #822678_822678_PROJET_DEVELOPPEMENT - création]
[ULGCE-1053 #822678_822678_PROJET_DEVELOPPEMENT - création] happens at 2024/01/25
[ULGCE-1053 #822678_822678_PROJET_DEVELOPPEMENT - création] -> [ULGCE-1053 (IN PROGRESS)-1]
[ULGCE-1053 (IN PROGRESS)-1] starts 2024/01/25 and ends 2024/01/25
[ULGCE-1053 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1053 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1053]]
[ULGCE-1053 (En Test)-2] starts 2024/01/25 and ends 2024/02/04
[ULGCE-1053 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1053 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1053]]
[ULGCE-1053 (IN PROGRESS)-1] -> [ULGCE-1053 (En Test)-2]
[ULGCE-1053 (Terminée)-3] starts 2024/02/04 and ends 2024/02/04
[ULGCE-1053 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1053 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1053]]
[ULGCE-1053 (En Test)-2] -> [ULGCE-1053 (Terminée)-3]
[ULGCE-505 - création] -> [ULGCE-1007 #822678-Perf Lot 1.2 - Support pour les tests - création]
[ULGCE-1007 #822678-Perf Lot 1.2 - Support pour les tests - création] happens at 2024/01/17
[ULGCE-1007 #822678-Perf Lot 1.2 - Support pour les tests - création] -> [ULGCE-1007 (IN PROGRESS)-1]
[ULGCE-1007 (IN PROGRESS)-1] starts 2024/01/17 and ends 2024/01/19
[ULGCE-1007 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1007 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (En Attente)-2] starts 2024/01/19 and ends 2024/01/19
[ULGCE-1007 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1007 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (IN PROGRESS)-1] -> [ULGCE-1007 (En Attente)-2]
[ULGCE-1007 (IN PROGRESS)-3] starts 2024/01/19 and ends 2024/01/23
[ULGCE-1007 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1007 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (En Attente)-2] -> [ULGCE-1007 (IN PROGRESS)-3]
[ULGCE-1007 (En Attente)-4] starts 2024/01/23 and ends 2024/01/23
[ULGCE-1007 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1007 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (IN PROGRESS)-3] -> [ULGCE-1007 (En Attente)-4]
[ULGCE-1007 (IN PROGRESS)-5] starts 2024/01/23 and ends 2024/01/30
[ULGCE-1007 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1007 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (En Attente)-4] -> [ULGCE-1007 (IN PROGRESS)-5]
[ULGCE-1007 (En Attente)-6] starts 2024/01/30 and ends 2024/01/31
[ULGCE-1007 (En Attente)-6] is colored in LightGray/Gray
[ULGCE-1007 (En Attente)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (IN PROGRESS)-5] -> [ULGCE-1007 (En Attente)-6]
[ULGCE-1007 (IN PROGRESS)-7] starts 2024/01/31 and ends 2024/02/05
[ULGCE-1007 (IN PROGRESS)-7] is colored in LightGray/Gray
[ULGCE-1007 (IN PROGRESS)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (En Attente)-6] -> [ULGCE-1007 (IN PROGRESS)-7]
[ULGCE-1007 (En Attente)-8] starts 2024/02/05 and ends 2024/02/09
[ULGCE-1007 (En Attente)-8] is colored in LightGray/Gray
[ULGCE-1007 (En Attente)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (IN PROGRESS)-7] -> [ULGCE-1007 (En Attente)-8]
[ULGCE-1007 (A faire)-9] starts 2024/02/09 and ends 2024/02/10
[ULGCE-1007 (A faire)-9] is colored in LightGray/Gray
[ULGCE-1007 (A faire)-9] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (En Attente)-8] -> [ULGCE-1007 (A faire)-9]
[ULGCE-1007 (En Test)-10] starts 2024/02/10 and ends 2024/02/10
[ULGCE-1007 (En Test)-10] is colored in LightGray/Gray
[ULGCE-1007 (En Test)-10] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (A faire)-9] -> [ULGCE-1007 (En Test)-10]
[ULGCE-1007 (Terminée)-11] starts 2024/02/10 and ends 2024/02/10
[ULGCE-1007 (Terminée)-11] is colored in LightGray/Gray
[ULGCE-1007 (Terminée)-11] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1007]]
[ULGCE-1007 (En Test)-10] -> [ULGCE-1007 (Terminée)-11]
[ULGCE-505 - création] -> [ULGCE-1006 #822678_822678_PROJET_TESTS - création]
[ULGCE-1006 #822678_822678_PROJET_TESTS - création] happens at 2024/01/17
[ULGCE-1006 #822678_822678_PROJET_TESTS - création] -> [ULGCE-1006 (En Test)-1]
[ULGCE-1006 (En Test)-1] starts 2024/01/17 and ends 2024/01/18
[ULGCE-1006 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1006 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1006]]
[ULGCE-1006 (Terminée)-2] starts 2024/01/18 and ends 2024/01/26
[ULGCE-1006 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1006 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1006]]
[ULGCE-1006 (En Test)-1] -> [ULGCE-1006 (Terminée)-2]
[ULGCE-505 - création] -> [ULGCE-1005 #822678_822678_PROJET_DEVELOPPEMENT - création]
[ULGCE-1005 #822678_822678_PROJET_DEVELOPPEMENT - création] happens at 2024/01/17
[ULGCE-1005 #822678_822678_PROJET_DEVELOPPEMENT - création] -> [ULGCE-1005 (IN PROGRESS)-1]
[ULGCE-1005 (IN PROGRESS)-1] starts 2024/01/17 and ends 2024/01/18
[ULGCE-1005 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1005 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1005]]
[ULGCE-1005 (En Test)-2] starts 2024/01/18 and ends 2024/01/26
[ULGCE-1005 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1005 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1005]]
[ULGCE-1005 (IN PROGRESS)-1] -> [ULGCE-1005 (En Test)-2]
[ULGCE-1005 (Terminée)-3] starts 2024/01/26 and ends 2024/01/26
[ULGCE-1005 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1005 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1005]]
[ULGCE-1005 (En Test)-2] -> [ULGCE-1005 (Terminée)-3]
[ULGCE-505 - création] -> [ULGCE-917 #822678-Perf Lot 1.2 - Support pour les tests - création]
[ULGCE-917 #822678-Perf Lot 1.2 - Support pour les tests - création] happens at 2024/01/11
[ULGCE-917 #822678-Perf Lot 1.2 - Support pour les tests - création] -> [ULGCE-917 (IN PROGRESS)-1]
[ULGCE-917 (IN PROGRESS)-1] starts 2024/01/11 and ends 2024/01/12
[ULGCE-917 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-917 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-917]]
[ULGCE-917 (En Attente)-2] starts 2024/01/12 and ends 2024/01/12
[ULGCE-917 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-917 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-917]]
[ULGCE-917 (IN PROGRESS)-1] -> [ULGCE-917 (En Attente)-2]
[ULGCE-917 (IN PROGRESS)-3] starts 2024/01/12 and ends 2024/01/16
[ULGCE-917 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-917 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-917]]
[ULGCE-917 (En Attente)-2] -> [ULGCE-917 (IN PROGRESS)-3]
[ULGCE-917 (En Attente)-4] starts 2024/01/16 and ends 2024/01/17
[ULGCE-917 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-917 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-917]]
[ULGCE-917 (IN PROGRESS)-3] -> [ULGCE-917 (En Attente)-4]
[ULGCE-917 (IN PROGRESS)-5] starts 2024/01/17 and ends 2024/01/26
[ULGCE-917 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-917 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-917]]
[ULGCE-917 (En Attente)-4] -> [ULGCE-917 (IN PROGRESS)-5]
[ULGCE-917 (En Test)-6] starts 2024/01/26 and ends 2024/01/26
[ULGCE-917 (En Test)-6] is colored in LightGray/Gray
[ULGCE-917 (En Test)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-917]]
[ULGCE-917 (IN PROGRESS)-5] -> [ULGCE-917 (En Test)-6]
[ULGCE-917 (Terminée)-7] starts 2024/01/26 and ends 2024/01/26
[ULGCE-917 (Terminée)-7] is colored in LightGray/Gray
[ULGCE-917 (Terminée)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-917]]
[ULGCE-917 (En Test)-6] -> [ULGCE-917 (Terminée)-7]
[ULGCE-505 - création] -> [ULGCE-915 #822678_822678_PROJET_TESTS - création]
[ULGCE-915 #822678_822678_PROJET_TESTS - création] happens at 2024/01/11
[ULGCE-915 #822678_822678_PROJET_TESTS - création] -> [ULGCE-915 (En Test)-1]
[ULGCE-915 (En Test)-1] starts 2024/01/11 and ends 2024/01/11
[ULGCE-915 (En Test)-1] is colored in LightGray/Gray
[ULGCE-915 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-915]]
[ULGCE-915 (A faire)-2] starts 2024/01/11 and ends 2024/01/15
[ULGCE-915 (A faire)-2] is colored in LightGray/Gray
[ULGCE-915 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-915]]
[ULGCE-915 (En Test)-1] -> [ULGCE-915 (A faire)-2]
[ULGCE-915 (IN PROGRESS)-3] starts 2024/01/15 and ends 2024/01/15
[ULGCE-915 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-915 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-915]]
[ULGCE-915 (A faire)-2] -> [ULGCE-915 (IN PROGRESS)-3]
[ULGCE-915 (En Attente)-4] starts 2024/01/15 and ends 2024/01/15
[ULGCE-915 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-915 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-915]]
[ULGCE-915 (IN PROGRESS)-3] -> [ULGCE-915 (En Attente)-4]
[ULGCE-915 (IN PROGRESS)-5] starts 2024/01/15 and ends 2024/01/15
[ULGCE-915 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-915 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-915]]
[ULGCE-915 (En Attente)-4] -> [ULGCE-915 (IN PROGRESS)-5]
[ULGCE-915 (En Attente)-6] starts 2024/01/15 and ends 2024/01/16
[ULGCE-915 (En Attente)-6] is colored in LightGray/Gray
[ULGCE-915 (En Attente)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-915]]
[ULGCE-915 (IN PROGRESS)-5] -> [ULGCE-915 (En Attente)-6]
[ULGCE-915 (IN PROGRESS)-7] starts 2024/01/16 and ends 2024/01/17
[ULGCE-915 (IN PROGRESS)-7] is colored in LightGray/Gray
[ULGCE-915 (IN PROGRESS)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-915]]
[ULGCE-915 (En Attente)-6] -> [ULGCE-915 (IN PROGRESS)-7]
[ULGCE-915 (En Test)-8] starts 2024/01/17 and ends 2024/01/17
[ULGCE-915 (En Test)-8] is colored in LightGray/Gray
[ULGCE-915 (En Test)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-915]]
[ULGCE-915 (IN PROGRESS)-7] -> [ULGCE-915 (En Test)-8]
[ULGCE-915 (Terminée)-9] starts 2024/01/17 and ends 2024/01/17
[ULGCE-915 (Terminée)-9] is colored in LightGray/Gray
[ULGCE-915 (Terminée)-9] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-915]]
[ULGCE-915 (En Test)-8] -> [ULGCE-915 (Terminée)-9]
[ULGCE-505 - création] -> [ULGCE-913 #822678_822678_PROJET_DEVELOPPEMENT - création]
[ULGCE-913 #822678_822678_PROJET_DEVELOPPEMENT - création] happens at 2024/01/11
[ULGCE-913 #822678_822678_PROJET_DEVELOPPEMENT - création] -> [ULGCE-913 (IN PROGRESS)-1]
[ULGCE-913 (IN PROGRESS)-1] starts 2024/01/11 and ends 2024/01/11
[ULGCE-913 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-913 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-913]]
[ULGCE-913 (En Test)-2] starts 2024/01/11 and ends 2024/01/17
[ULGCE-913 (En Test)-2] is colored in LightGray/Gray
[ULGCE-913 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-913]]
[ULGCE-913 (IN PROGRESS)-1] -> [ULGCE-913 (En Test)-2]
[ULGCE-913 (Terminée)-3] starts 2024/01/17 and ends 2024/01/17
[ULGCE-913 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-913 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-913]]
[ULGCE-913 (En Test)-2] -> [ULGCE-913 (Terminée)-3]
[ULGCE-505 - création] -> [ULGCE-875 #822678-Perf Lot 1.2 - Support pour les tests - création]
[ULGCE-875 #822678-Perf Lot 1.2 - Support pour les tests - création] happens at 2024/01/04
[ULGCE-875 #822678-Perf Lot 1.2 - Support pour les tests - création] -> [ULGCE-875 (IN PROGRESS)-1]
[ULGCE-875 (IN PROGRESS)-1] starts 2024/01/04 and ends 2024/01/04
[ULGCE-875 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-875 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-875]]
[ULGCE-875 (En Test)-2] starts 2024/01/04 and ends 2024/01/05
[ULGCE-875 (En Test)-2] is colored in LightGray/Gray
[ULGCE-875 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-875]]
[ULGCE-875 (IN PROGRESS)-1] -> [ULGCE-875 (En Test)-2]
[ULGCE-875 (Terminée)-3] starts 2024/01/05 and ends 2024/01/10
[ULGCE-875 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-875 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-875]]
[ULGCE-875 (En Test)-2] -> [ULGCE-875 (Terminée)-3]
[ULGCE-505 - création] -> [ULGCE-855 #822678_822678_PROJET_DEVELOPPEMENT - création]
[ULGCE-855 #822678_822678_PROJET_DEVELOPPEMENT - création] happens at 2024/01/04
[ULGCE-855 #822678_822678_PROJET_DEVELOPPEMENT - création] -> [ULGCE-855 (IN PROGRESS)-1]
[ULGCE-855 (IN PROGRESS)-1] starts 2024/01/04 and ends 2024/01/04
[ULGCE-855 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-855 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-855]]
[ULGCE-855 (En Test)-2] starts 2024/01/04 and ends 2024/01/10
[ULGCE-855 (En Test)-2] is colored in LightGray/Gray
[ULGCE-855 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-855]]
[ULGCE-855 (IN PROGRESS)-1] -> [ULGCE-855 (En Test)-2]
[ULGCE-855 (Terminée)-3] starts 2024/01/10 and ends 2024/01/10
[ULGCE-855 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-855 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-855]]
[ULGCE-855 (En Test)-2] -> [ULGCE-855 (Terminée)-3]
[ULGCE-505 - création] -> [ULGCE-854 #822678_822678_PROJET_TESTS - création]
[ULGCE-854 #822678_822678_PROJET_TESTS - création] happens at 2024/01/04
[ULGCE-854 #822678_822678_PROJET_TESTS - création] -> [ULGCE-854 (En Test)-1]
[ULGCE-854 (En Test)-1] starts 2024/01/04 and ends 2024/01/04
[ULGCE-854 (En Test)-1] is colored in LightGray/Gray
[ULGCE-854 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-854]]
[ULGCE-854 (Terminée)-2] starts 2024/01/04 and ends 2024/01/10
[ULGCE-854 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-854 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-854]]
[ULGCE-854 (En Test)-1] -> [ULGCE-854 (Terminée)-2]
[ULGCE-505 - création] -> [ULGCE-817 #822678_822678_PROJET_DEVELOPPEMENT - création]
[ULGCE-817 #822678_822678_PROJET_DEVELOPPEMENT - création] happens at 2023/12/27
[ULGCE-817 #822678_822678_PROJET_DEVELOPPEMENT - création] -> [ULGCE-817 (En Test)-1]
[ULGCE-817 (En Test)-1] starts 2023/12/27 and ends 2024/01/02
[ULGCE-817 (En Test)-1] is colored in LightGray/Gray
[ULGCE-817 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-817]]
[ULGCE-817 (Terminée)-2] starts 2024/01/02 and ends 2024/01/04
[ULGCE-817 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-817 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-817]]
[ULGCE-817 (En Test)-1] -> [ULGCE-817 (Terminée)-2]
[ULGCE-505 - création] -> [ULGCE-777 #822678-Perf Lot 1.2 - Support pour les tests - création]
[ULGCE-777 #822678-Perf Lot 1.2 - Support pour les tests - création] happens at 2023/12/20
[ULGCE-777 #822678-Perf Lot 1.2 - Support pour les tests - création] -> [ULGCE-777 (IN PROGRESS)-1]
[ULGCE-777 (IN PROGRESS)-1] starts 2023/12/20 and ends 2023/12/21
[ULGCE-777 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-777 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-777]]
[ULGCE-777 (En Attente)-2] starts 2023/12/21 and ends 2023/12/27
[ULGCE-777 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-777 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-777]]
[ULGCE-777 (IN PROGRESS)-1] -> [ULGCE-777 (En Attente)-2]
[ULGCE-777 (A faire)-3] starts 2023/12/27 and ends 2023/12/27
[ULGCE-777 (A faire)-3] is colored in LightGray/Gray
[ULGCE-777 (A faire)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-777]]
[ULGCE-777 (En Attente)-2] -> [ULGCE-777 (A faire)-3]
[ULGCE-777 (IN PROGRESS)-4] starts 2023/12/27 and ends 2023/12/28
[ULGCE-777 (IN PROGRESS)-4] is colored in LightGray/Gray
[ULGCE-777 (IN PROGRESS)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-777]]
[ULGCE-777 (A faire)-3] -> [ULGCE-777 (IN PROGRESS)-4]
[ULGCE-777 (En Attente)-5] starts 2023/12/28 and ends 2024/01/03
[ULGCE-777 (En Attente)-5] is colored in LightGray/Gray
[ULGCE-777 (En Attente)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-777]]
[ULGCE-777 (IN PROGRESS)-4] -> [ULGCE-777 (En Attente)-5]
[ULGCE-777 (IN PROGRESS)-6] starts 2024/01/03 and ends 2024/01/04
[ULGCE-777 (IN PROGRESS)-6] is colored in LightGray/Gray
[ULGCE-777 (IN PROGRESS)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-777]]
[ULGCE-777 (En Attente)-5] -> [ULGCE-777 (IN PROGRESS)-6]
[ULGCE-777 (En Test)-7] starts 2024/01/04 and ends 2024/01/04
[ULGCE-777 (En Test)-7] is colored in LightGray/Gray
[ULGCE-777 (En Test)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-777]]
[ULGCE-777 (IN PROGRESS)-6] -> [ULGCE-777 (En Test)-7]
[ULGCE-777 (Terminée)-8] starts 2024/01/04 and ends 2024/01/04
[ULGCE-777 (Terminée)-8] is colored in LightGray/Gray
[ULGCE-777 (Terminée)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-777]]
[ULGCE-777 (En Test)-7] -> [ULGCE-777 (Terminée)-8]
[ULGCE-505 - création] -> [ULGCE-775 #822678_822678_PROJET_TESTS - création]
[ULGCE-775 #822678_822678_PROJET_TESTS - création] happens at 2023/12/20
[ULGCE-775 #822678_822678_PROJET_TESTS - création] -> [ULGCE-775 (En Test)-1]
[ULGCE-775 (En Test)-1] starts 2023/12/20 and ends 2023/12/26
[ULGCE-775 (En Test)-1] is colored in LightGray/Gray
[ULGCE-775 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-775]]
[ULGCE-775 (Terminée)-2] starts 2023/12/26 and ends 2024/01/04
[ULGCE-775 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-775 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-775]]
[ULGCE-775 (En Test)-1] -> [ULGCE-775 (Terminée)-2]
[ULGCE-505 - création] -> [ULGCE-742 #822678-Perf Lot 1.2 - Dev - création]
[ULGCE-742 #822678-Perf Lot 1.2 - Dev - création] happens at 2023/12/19
[ULGCE-742 #822678-Perf Lot 1.2 - Dev - création] -> [ULGCE-742 (En Test)-1]
[ULGCE-742 (En Test)-1] starts 2023/12/19 and ends 2023/12/19
[ULGCE-742 (En Test)-1] is colored in LightGray/Gray
[ULGCE-742 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-742]]
[ULGCE-742 (Terminée)-2] starts 2023/12/19 and ends 2023/12/20
[ULGCE-742 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-742 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-742]]
[ULGCE-742 (En Test)-1] -> [ULGCE-742 (Terminée)-2]
[ULGCE-505 - création] -> [ULGCE-706 #822678-Perf Lot 1.2 - Dev - création]
[ULGCE-706 #822678-Perf Lot 1.2 - Dev - création] happens at 2023/12/14
[ULGCE-706 #822678-Perf Lot 1.2 - Dev - création] -> [ULGCE-706 (En Test)-1]
[ULGCE-706 (En Test)-1] starts 2023/12/14 and ends 2023/12/21
[ULGCE-706 (En Test)-1] is colored in LightGray/Gray
[ULGCE-706 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-706]]
[ULGCE-706 (Terminée)-2] starts 2023/12/21 and ends 2023/12/22
[ULGCE-706 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-706 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-706]]
[ULGCE-706 (En Test)-1] -> [ULGCE-706 (Terminée)-2]
[ULGCE-505 - création] -> [ULGCE-671 #822678-Perf Lot 1.2 - TESTS - création]
[ULGCE-671 #822678-Perf Lot 1.2 - TESTS - création] happens at 2023/12/08
[ULGCE-671 #822678-Perf Lot 1.2 - TESTS - création] -> [ULGCE-671 (En Test)-1]
[ULGCE-671 (En Test)-1] starts 2023/12/08 and ends 2023/12/20
[ULGCE-671 (En Test)-1] is colored in LightGray/Gray
[ULGCE-671 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-671]]
[ULGCE-671 (Terminée)-2] starts 2023/12/20 and ends 2023/12/20
[ULGCE-671 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-671 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-671]]
[ULGCE-671 (En Test)-1] -> [ULGCE-671 (Terminée)-2]
[ULGCE-505 - création] -> [ULGCE-670 #822678-Perf Lot 1.2 - TESTS - création]
[ULGCE-670 #822678-Perf Lot 1.2 - TESTS - création] happens at 2023/12/08
[ULGCE-670 #822678-Perf Lot 1.2 - TESTS - création] -> [ULGCE-670 (En Test)-1]
[ULGCE-670 (En Test)-1] starts 2023/12/08 and ends 2023/12/14
[ULGCE-670 (En Test)-1] is colored in LightGray/Gray
[ULGCE-670 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-670]]
[ULGCE-670 (A faire)-2] starts 2023/12/14 and ends 2023/12/14
[ULGCE-670 (A faire)-2] is colored in LightGray/Gray
[ULGCE-670 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-670]]
[ULGCE-670 (En Test)-1] -> [ULGCE-670 (A faire)-2]
[ULGCE-670 (En Test)-3] starts 2023/12/14 and ends 2023/12/14
[ULGCE-670 (En Test)-3] is colored in LightGray/Gray
[ULGCE-670 (En Test)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-670]]
[ULGCE-670 (A faire)-2] -> [ULGCE-670 (En Test)-3]
[ULGCE-670 (Terminée)-4] starts 2023/12/14 and ends 2023/12/19
[ULGCE-670 (Terminée)-4] is colored in LightGray/Gray
[ULGCE-670 (Terminée)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-670]]
[ULGCE-670 (En Test)-3] -> [ULGCE-670 (Terminée)-4]
[ULGCE-505 - création] -> [ULGCE-669 #822678-Perf Lot 1.2 - TESTS - création]
[ULGCE-669 #822678-Perf Lot 1.2 - TESTS - création] happens at 2023/12/08
[ULGCE-669 #822678-Perf Lot 1.2 - TESTS - création] -> [ULGCE-669 (En Test)-1]
[ULGCE-669 (En Test)-1] starts 2023/12/08 and ends 2023/12/14
[ULGCE-669 (En Test)-1] is colored in LightGray/Gray
[ULGCE-669 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-669]]
[ULGCE-669 (A faire)-2] starts 2023/12/14 and ends 2023/12/14
[ULGCE-669 (A faire)-2] is colored in LightGray/Gray
[ULGCE-669 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-669]]
[ULGCE-669 (En Test)-1] -> [ULGCE-669 (A faire)-2]
[ULGCE-669 (En Test)-3] starts 2023/12/14 and ends 2023/12/14
[ULGCE-669 (En Test)-3] is colored in LightGray/Gray
[ULGCE-669 (En Test)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-669]]
[ULGCE-669 (A faire)-2] -> [ULGCE-669 (En Test)-3]
[ULGCE-669 (A faire)-4] starts 2023/12/14 and ends 2023/12/15
[ULGCE-669 (A faire)-4] is colored in LightGray/Gray
[ULGCE-669 (A faire)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-669]]
[ULGCE-669 (En Test)-3] -> [ULGCE-669 (A faire)-4]
[ULGCE-669 (IN PROGRESS)-5] starts 2023/12/15 and ends 2023/12/15
[ULGCE-669 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-669 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-669]]
[ULGCE-669 (A faire)-4] -> [ULGCE-669 (IN PROGRESS)-5]
[ULGCE-669 (En Attente)-6] starts 2023/12/15 and ends 2023/12/15
[ULGCE-669 (En Attente)-6] is colored in LightGray/Gray
[ULGCE-669 (En Attente)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-669]]
[ULGCE-669 (IN PROGRESS)-5] -> [ULGCE-669 (En Attente)-6]
[ULGCE-669 (IN PROGRESS)-7] starts 2023/12/15 and ends 2023/12/20
[ULGCE-669 (IN PROGRESS)-7] is colored in LightGray/Gray
[ULGCE-669 (IN PROGRESS)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-669]]
[ULGCE-669 (En Attente)-6] -> [ULGCE-669 (IN PROGRESS)-7]
[ULGCE-669 (En Test)-8] starts 2023/12/20 and ends 2023/12/20
[ULGCE-669 (En Test)-8] is colored in LightGray/Gray
[ULGCE-669 (En Test)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-669]]
[ULGCE-669 (IN PROGRESS)-7] -> [ULGCE-669 (En Test)-8]
[ULGCE-669 (Terminée)-9] starts 2023/12/20 and ends 2023/12/20
[ULGCE-669 (Terminée)-9] is colored in LightGray/Gray
[ULGCE-669 (Terminée)-9] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-669]]
[ULGCE-669 (En Test)-8] -> [ULGCE-669 (Terminée)-9]
[ULGCE-505 - création] -> [ULGCE-611 #822678-Perf Lot 1.2 - TESTS - création]
[ULGCE-611 #822678-Perf Lot 1.2 - TESTS - création] happens at 2023/12/05
[ULGCE-611 #822678-Perf Lot 1.2 - TESTS - création] -> [ULGCE-611 (IN PROGRESS)-1]
[ULGCE-611 (IN PROGRESS)-1] starts 2023/12/05 and ends 2023/12/05
[ULGCE-611 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-611 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-611]]
[ULGCE-611 (En Attente)-2] starts 2023/12/05 and ends 2023/12/05
[ULGCE-611 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-611 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-611]]
[ULGCE-611 (IN PROGRESS)-1] -> [ULGCE-611 (En Attente)-2]
[ULGCE-611 (IN PROGRESS)-3] starts 2023/12/05 and ends 2023/12/07
[ULGCE-611 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-611 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-611]]
[ULGCE-611 (En Attente)-2] -> [ULGCE-611 (IN PROGRESS)-3]
[ULGCE-611 (En Test)-4] starts 2023/12/07 and ends 2023/12/14
[ULGCE-611 (En Test)-4] is colored in LightGray/Gray
[ULGCE-611 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-611]]
[ULGCE-611 (IN PROGRESS)-3] -> [ULGCE-611 (En Test)-4]
[ULGCE-611 (Terminée)-5] starts 2023/12/14 and ends 2023/12/14
[ULGCE-611 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-611 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-611]]
[ULGCE-611 (En Test)-4] -> [ULGCE-611 (Terminée)-5]
[ULGCE-505 - création] -> [ULGCE-610 #822678-Perf Lot 1.2 - TESTS - création]
[ULGCE-610 #822678-Perf Lot 1.2 - TESTS - création] happens at 2023/12/05
[ULGCE-610 #822678-Perf Lot 1.2 - TESTS - création] -> [ULGCE-610 (IN PROGRESS)-1]
[ULGCE-610 (IN PROGRESS)-1] starts 2023/12/05 and ends 2023/12/05
[ULGCE-610 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-610 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-610]]
[ULGCE-610 (En Attente)-2] starts 2023/12/05 and ends 2023/12/07
[ULGCE-610 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-610 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-610]]
[ULGCE-610 (IN PROGRESS)-1] -> [ULGCE-610 (En Attente)-2]
[ULGCE-610 (IN PROGRESS)-3] starts 2023/12/07 and ends 2023/12/07
[ULGCE-610 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-610 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-610]]
[ULGCE-610 (En Attente)-2] -> [ULGCE-610 (IN PROGRESS)-3]
[ULGCE-610 (En Test)-4] starts 2023/12/07 and ends 2023/12/14
[ULGCE-610 (En Test)-4] is colored in LightGray/Gray
[ULGCE-610 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-610]]
[ULGCE-610 (IN PROGRESS)-3] -> [ULGCE-610 (En Test)-4]
[ULGCE-610 (Terminée)-5] starts 2023/12/14 and ends 2023/12/14
[ULGCE-610 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-610 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-610]]
[ULGCE-610 (En Test)-4] -> [ULGCE-610 (Terminée)-5]
[ULGCE-505 - création] -> [ULGCE-607 #822678-Perf Lot 1.2 - Support pour les tests - création]
[ULGCE-607 #822678-Perf Lot 1.2 - Support pour les tests - création] happens at 2023/12/01
[ULGCE-607 #822678-Perf Lot 1.2 - Support pour les tests - création] -> [ULGCE-607 (IN PROGRESS)-1]
[ULGCE-607 (IN PROGRESS)-1] starts 2023/12/01 and ends 2023/12/01
[ULGCE-607 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-607 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (En Attente)-2] starts 2023/12/01 and ends 2023/12/07
[ULGCE-607 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-607 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (IN PROGRESS)-1] -> [ULGCE-607 (En Attente)-2]
[ULGCE-607 (IN PROGRESS)-3] starts 2023/12/07 and ends 2023/12/07
[ULGCE-607 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-607 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (En Attente)-2] -> [ULGCE-607 (IN PROGRESS)-3]
[ULGCE-607 (En Attente)-4] starts 2023/12/07 and ends 2023/12/07
[ULGCE-607 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-607 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (IN PROGRESS)-3] -> [ULGCE-607 (En Attente)-4]
[ULGCE-607 (IN PROGRESS)-5] starts 2023/12/07 and ends 2023/12/07
[ULGCE-607 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-607 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (En Attente)-4] -> [ULGCE-607 (IN PROGRESS)-5]
[ULGCE-607 (En Attente)-6] starts 2023/12/07 and ends 2023/12/07
[ULGCE-607 (En Attente)-6] is colored in LightGray/Gray
[ULGCE-607 (En Attente)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (IN PROGRESS)-5] -> [ULGCE-607 (En Attente)-6]
[ULGCE-607 (IN PROGRESS)-7] starts 2023/12/07 and ends 2023/12/07
[ULGCE-607 (IN PROGRESS)-7] is colored in LightGray/Gray
[ULGCE-607 (IN PROGRESS)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (En Attente)-6] -> [ULGCE-607 (IN PROGRESS)-7]
[ULGCE-607 (En Attente)-8] starts 2023/12/07 and ends 2023/12/07
[ULGCE-607 (En Attente)-8] is colored in LightGray/Gray
[ULGCE-607 (En Attente)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (IN PROGRESS)-7] -> [ULGCE-607 (En Attente)-8]
[ULGCE-607 (IN PROGRESS)-9] starts 2023/12/07 and ends 2023/12/07
[ULGCE-607 (IN PROGRESS)-9] is colored in LightGray/Gray
[ULGCE-607 (IN PROGRESS)-9] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (En Attente)-8] -> [ULGCE-607 (IN PROGRESS)-9]
[ULGCE-607 (En Test)-10] starts 2023/12/07 and ends 2023/12/11
[ULGCE-607 (En Test)-10] is colored in LightGray/Gray
[ULGCE-607 (En Test)-10] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (IN PROGRESS)-9] -> [ULGCE-607 (En Test)-10]
[ULGCE-607 (A faire)-11] starts 2023/12/11 and ends 2023/12/11
[ULGCE-607 (A faire)-11] is colored in LightGray/Gray
[ULGCE-607 (A faire)-11] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (En Test)-10] -> [ULGCE-607 (A faire)-11]
[ULGCE-607 (IN PROGRESS)-12] starts 2023/12/11 and ends 2023/12/11
[ULGCE-607 (IN PROGRESS)-12] is colored in LightGray/Gray
[ULGCE-607 (IN PROGRESS)-12] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (A faire)-11] -> [ULGCE-607 (IN PROGRESS)-12]
[ULGCE-607 (En Attente)-13] starts 2023/12/11 and ends 2023/12/15
[ULGCE-607 (En Attente)-13] is colored in LightGray/Gray
[ULGCE-607 (En Attente)-13] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (IN PROGRESS)-12] -> [ULGCE-607 (En Attente)-13]
[ULGCE-607 (A faire)-14] starts 2023/12/15 and ends 2023/12/20
[ULGCE-607 (A faire)-14] is colored in LightGray/Gray
[ULGCE-607 (A faire)-14] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (En Attente)-13] -> [ULGCE-607 (A faire)-14]
[ULGCE-607 (En Test)-15] starts 2023/12/20 and ends 2023/12/20
[ULGCE-607 (En Test)-15] is colored in LightGray/Gray
[ULGCE-607 (En Test)-15] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (A faire)-14] -> [ULGCE-607 (En Test)-15]
[ULGCE-607 (Terminée)-16] starts 2023/12/20 and ends 2023/12/20
[ULGCE-607 (Terminée)-16] is colored in LightGray/Gray
[ULGCE-607 (Terminée)-16] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-607]]
[ULGCE-607 (En Test)-15] -> [ULGCE-607 (Terminée)-16]
-- ULGCE-507 - [#855949] P20-185 - PERF-LOT1.2 : Evolution de la répartition forfaitaire à recycler cat 5,7 --
[ULGCE-507 - création] happens on 2023/11/24
[ULGCE-507 - création] -> [ULGCE-1853 #855949 _855949_PROJET_PACKAGING/LIVRAISON - création]
[ULGCE-1853 #855949 _855949_PROJET_PACKAGING/LIVRAISON - création] happens at 2024/06/17
[ULGCE-1853 #855949 _855949_PROJET_PACKAGING/LIVRAISON - création] -> [ULGCE-1853 (IN PROGRESS)-1]
[ULGCE-1853 (IN PROGRESS)-1] starts 2024/06/17 and ends 2024/06/17
[ULGCE-1853 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1853 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1853]]
[ULGCE-1853 (En Attente)-2] starts 2024/06/17 and ends 2024/06/17
[ULGCE-1853 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1853 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1853]]
[ULGCE-1853 (IN PROGRESS)-1] -> [ULGCE-1853 (En Attente)-2]
[ULGCE-1853 (IN PROGRESS)-3] starts 2024/06/17 and ends 2024/08/23
[ULGCE-1853 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1853 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1853]]
[ULGCE-1853 (En Attente)-2] -> [ULGCE-1853 (IN PROGRESS)-3]
[ULGCE-1853 (En Test)-4] starts 2024/08/23 and ends 2024/08/23
[ULGCE-1853 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1853 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1853]]
[ULGCE-1853 (IN PROGRESS)-3] -> [ULGCE-1853 (En Test)-4]
[ULGCE-1853 (Terminée)-5] starts 2024/08/23 and ends 2024/08/23
[ULGCE-1853 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1853 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1853]]
[ULGCE-1853 (En Test)-4] -> [ULGCE-1853 (Terminée)-5]
[ULGCE-507 - création] -> [ULGCE-1181 #855949 _855949_PROJET_TESTS - création]
[ULGCE-1181 #855949 _855949_PROJET_TESTS - création] happens at 2024/02/10
[ULGCE-1181 #855949 _855949_PROJET_TESTS - création] -> [ULGCE-1181 (En Test)-1]
[ULGCE-1181 (En Test)-1] starts 2024/02/10 and ends 2024/02/23
[ULGCE-1181 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1181 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1181]]
[ULGCE-1181 (A faire)-2] starts 2024/02/23 and ends 2024/02/26
[ULGCE-1181 (A faire)-2] is colored in LightGray/Gray
[ULGCE-1181 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1181]]
[ULGCE-1181 (En Test)-1] -> [ULGCE-1181 (A faire)-2]
[ULGCE-1181 (En Test)-3] starts 2024/02/26 and ends 2024/02/28
[ULGCE-1181 (En Test)-3] is colored in LightGray/Gray
[ULGCE-1181 (En Test)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1181]]
[ULGCE-1181 (A faire)-2] -> [ULGCE-1181 (En Test)-3]
[ULGCE-1181 (A faire)-4] starts 2024/02/28 and ends 2024/03/15
[ULGCE-1181 (A faire)-4] is colored in LightGray/Gray
[ULGCE-1181 (A faire)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1181]]
[ULGCE-1181 (En Test)-3] -> [ULGCE-1181 (A faire)-4]
[ULGCE-1181 (En Test)-5] starts 2024/03/15 and ends 2024/03/29
[ULGCE-1181 (En Test)-5] is colored in LightGray/Gray
[ULGCE-1181 (En Test)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1181]]
[ULGCE-1181 (A faire)-4] -> [ULGCE-1181 (En Test)-5]
[ULGCE-1181 (A faire)-6] starts 2024/03/29 and ends 2024/03/29
[ULGCE-1181 (A faire)-6] is colored in LightGray/Gray
[ULGCE-1181 (A faire)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1181]]
[ULGCE-1181 (En Test)-5] -> [ULGCE-1181 (A faire)-6]
[ULGCE-1181 (En Test)-7] starts 2024/03/29 and ends 2024/05/24
[ULGCE-1181 (En Test)-7] is colored in LightGray/Gray
[ULGCE-1181 (En Test)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1181]]
[ULGCE-1181 (A faire)-6] -> [ULGCE-1181 (En Test)-7]
[ULGCE-1181 (Terminée)-8] starts 2024/05/24 and ends 2024/06/14
[ULGCE-1181 (Terminée)-8] is colored in LightGray/Gray
[ULGCE-1181 (Terminée)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1181]]
[ULGCE-1181 (En Test)-7] -> [ULGCE-1181 (Terminée)-8]
[ULGCE-507 - création] -> [ULGCE-1180 #855949 _855949_DOSSIER DE TESTS (ALM) - création]
[ULGCE-1180 #855949 _855949_DOSSIER DE TESTS (ALM) - création] happens at 2024/02/10
[ULGCE-507 - création] -> [ULGCE-1179 #855949 _855949_PROJET_ALGORYTHMIE - création]
[ULGCE-1179 #855949 _855949_PROJET_ALGORYTHMIE - création] happens at 2024/02/10
[ULGCE-1179 #855949 _855949_PROJET_ALGORYTHMIE - création] -> [ULGCE-1179 (IN PROGRESS)-1]
[ULGCE-1179 (IN PROGRESS)-1] starts 2024/02/10 and ends 2024/02/12
[ULGCE-1179 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1179 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1179]]
[ULGCE-1179 (En Attente)-2] starts 2024/02/12 and ends 2024/02/15
[ULGCE-1179 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1179 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1179]]
[ULGCE-1179 (IN PROGRESS)-1] -> [ULGCE-1179 (En Attente)-2]
[ULGCE-1179 (IN PROGRESS)-3] starts 2024/02/15 and ends 2024/02/16
[ULGCE-1179 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1179 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1179]]
[ULGCE-1179 (En Attente)-2] -> [ULGCE-1179 (IN PROGRESS)-3]
[ULGCE-1179 (En Test)-4] starts 2024/02/16 and ends 2024/02/16
[ULGCE-1179 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1179 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1179]]
[ULGCE-1179 (IN PROGRESS)-3] -> [ULGCE-1179 (En Test)-4]
[ULGCE-1179 (Terminée)-5] starts 2024/02/16 and ends 2024/02/16
[ULGCE-1179 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1179 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1179]]
[ULGCE-1179 (En Test)-4] -> [ULGCE-1179 (Terminée)-5]
[ULGCE-507 - création] -> [ULGCE-1103 #855949 _855949_PROJET_DEVELOPPEMENT - création]
[ULGCE-1103 #855949 _855949_PROJET_DEVELOPPEMENT - création] happens at 2024/02/02
[ULGCE-1103 #855949 _855949_PROJET_DEVELOPPEMENT - création] -> [ULGCE-1103 (IN PROGRESS)-1]
[ULGCE-1103 (IN PROGRESS)-1] starts 2024/02/02 and ends 2024/02/16
[ULGCE-1103 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1103 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1103]]
[ULGCE-1103 (En Attente)-2] starts 2024/02/16 and ends 2024/02/23
[ULGCE-1103 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1103 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1103]]
[ULGCE-1103 (IN PROGRESS)-1] -> [ULGCE-1103 (En Attente)-2]
[ULGCE-1103 (IN PROGRESS)-3] starts 2024/02/23 and ends 2024/02/27
[ULGCE-1103 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1103 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1103]]
[ULGCE-1103 (En Attente)-2] -> [ULGCE-1103 (IN PROGRESS)-3]
[ULGCE-1103 (En Attente)-4] starts 2024/02/27 and ends 2024/02/28
[ULGCE-1103 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1103 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1103]]
[ULGCE-1103 (IN PROGRESS)-3] -> [ULGCE-1103 (En Attente)-4]
[ULGCE-1103 (A faire)-5] starts 2024/02/28 and ends 2024/03/15
[ULGCE-1103 (A faire)-5] is colored in LightGray/Gray
[ULGCE-1103 (A faire)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1103]]
[ULGCE-1103 (En Attente)-4] -> [ULGCE-1103 (A faire)-5]
[ULGCE-1103 (IN PROGRESS)-6] starts 2024/03/15 and ends 2024/04/22
[ULGCE-1103 (IN PROGRESS)-6] is colored in LightGray/Gray
[ULGCE-1103 (IN PROGRESS)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1103]]
[ULGCE-1103 (A faire)-5] -> [ULGCE-1103 (IN PROGRESS)-6]
[ULGCE-1103 (En Attente)-7] starts 2024/04/22 and ends 2024/04/23
[ULGCE-1103 (En Attente)-7] is colored in LightGray/Gray
[ULGCE-1103 (En Attente)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1103]]
[ULGCE-1103 (IN PROGRESS)-6] -> [ULGCE-1103 (En Attente)-7]
[ULGCE-1103 (IN PROGRESS)-8] starts 2024/04/23 and ends 2024/06/04
[ULGCE-1103 (IN PROGRESS)-8] is colored in LightGray/Gray
[ULGCE-1103 (IN PROGRESS)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1103]]
[ULGCE-1103 (En Attente)-7] -> [ULGCE-1103 (IN PROGRESS)-8]
[ULGCE-1103 (En Test)-9] starts 2024/06/04 and ends 2024/06/04
[ULGCE-1103 (En Test)-9] is colored in LightGray/Gray
[ULGCE-1103 (En Test)-9] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1103]]
[ULGCE-1103 (IN PROGRESS)-8] -> [ULGCE-1103 (En Test)-9]
[ULGCE-1103 (Terminée)-10] starts 2024/06/04 and ends 2024/06/04
[ULGCE-1103 (Terminée)-10] is colored in LightGray/Gray
[ULGCE-1103 (Terminée)-10] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1103]]
[ULGCE-1103 (En Test)-9] -> [ULGCE-1103 (Terminée)-10]
[ULGCE-507 - création] -> [ULGCE-873 #855949 _855949_PROJET_CONCEPTION TECHNIQUE - création]
[ULGCE-873 #855949 _855949_PROJET_CONCEPTION TECHNIQUE - création] happens at 2024/01/04
[ULGCE-873 #855949 _855949_PROJET_CONCEPTION TECHNIQUE - création] -> [ULGCE-873 (IN PROGRESS)-1]
[ULGCE-873 (IN PROGRESS)-1] starts 2024/01/04 and ends 2024/01/04
[ULGCE-873 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-873 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-873]]
[ULGCE-873 (En Attente)-2] starts 2024/01/04 and ends 2024/01/05
[ULGCE-873 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-873 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-873]]
[ULGCE-873 (IN PROGRESS)-1] -> [ULGCE-873 (En Attente)-2]
[ULGCE-873 (IN PROGRESS)-3] starts 2024/01/05 and ends 2024/01/05
[ULGCE-873 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-873 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-873]]
[ULGCE-873 (En Attente)-2] -> [ULGCE-873 (IN PROGRESS)-3]
[ULGCE-873 (En Attente)-4] starts 2024/01/05 and ends 2024/01/05
[ULGCE-873 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-873 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-873]]
[ULGCE-873 (IN PROGRESS)-3] -> [ULGCE-873 (En Attente)-4]
[ULGCE-873 (IN PROGRESS)-5] starts 2024/01/05 and ends 2024/01/30
[ULGCE-873 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-873 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-873]]
[ULGCE-873 (En Attente)-4] -> [ULGCE-873 (IN PROGRESS)-5]
[ULGCE-873 (En Test)-6] starts 2024/01/30 and ends 2024/01/30
[ULGCE-873 (En Test)-6] is colored in LightGray/Gray
[ULGCE-873 (En Test)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-873]]
[ULGCE-873 (IN PROGRESS)-5] -> [ULGCE-873 (En Test)-6]
[ULGCE-873 (Terminée)-7] starts 2024/01/30 and ends 2024/01/30
[ULGCE-873 (Terminée)-7] is colored in LightGray/Gray
[ULGCE-873 (Terminée)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-873]]
[ULGCE-873 (En Test)-6] -> [ULGCE-873 (Terminée)-7]
[ULGCE-507 - création] -> [ULGCE-508 #855949 _855949_PROJET_DEVIS - création]
[ULGCE-508 #855949 _855949_PROJET_DEVIS - création] happens at 2023/11/24
[ULGCE-508 #855949 _855949_PROJET_DEVIS - création] -> [ULGCE-508 (IN PROGRESS)-1]
[ULGCE-508 (IN PROGRESS)-1] starts 2023/11/24 and ends 2023/12/29
[ULGCE-508 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-508 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-508]]
[ULGCE-508 (En Attente)-2] starts 2023/12/29 and ends 2023/12/29
[ULGCE-508 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-508 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-508]]
[ULGCE-508 (IN PROGRESS)-1] -> [ULGCE-508 (En Attente)-2]
[ULGCE-508 (IN PROGRESS)-3] starts 2023/12/29 and ends 2024/01/03
[ULGCE-508 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-508 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-508]]
[ULGCE-508 (En Attente)-2] -> [ULGCE-508 (IN PROGRESS)-3]
[ULGCE-508 (En Test)-4] starts 2024/01/03 and ends 2024/01/03
[ULGCE-508 (En Test)-4] is colored in LightGray/Gray
[ULGCE-508 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-508]]
[ULGCE-508 (IN PROGRESS)-3] -> [ULGCE-508 (En Test)-4]
[ULGCE-508 (Terminée)-5] starts 2024/01/03 and ends 2024/01/03
[ULGCE-508 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-508 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-508]]
[ULGCE-508 (En Test)-4] -> [ULGCE-508 (Terminée)-5]
-- ULGCE-710 - Release manager- livraison --
[ULGCE-710 - création] happens on 2023/12/14
[ULGCE-710 - création] -> [ULGCE-2087 Release manager- livraison - création]
[ULGCE-2087 Release manager- livraison - création] happens at 2024/08/19
[ULGCE-2087 Release manager- livraison - création] -> [ULGCE-2087 (En Test)-1]
[ULGCE-2087 (En Test)-1] starts 2024/08/19 and ends 2024/08/19
[ULGCE-2087 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2087 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2087]]
[ULGCE-710 - création] -> [ULGCE-1142 Release manager- livraison - création]
[ULGCE-1142 Release manager- livraison - création] happens at 2024/02/06
[ULGCE-1142 Release manager- livraison - création] -> [ULGCE-1142 (En Test)-1]
[ULGCE-1142 (En Test)-1] starts 2024/02/06 and ends 2024/02/06
[ULGCE-1142 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1142 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1142]]
[ULGCE-1142 (A faire)-2] starts 2024/02/06 and ends 2024/03/02
[ULGCE-1142 (A faire)-2] is colored in LightGray/Gray
[ULGCE-1142 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1142]]
[ULGCE-1142 (En Test)-1] -> [ULGCE-1142 (A faire)-2]
[ULGCE-1142 (IN PROGRESS)-3] starts 2024/03/02 and ends 2024/03/07
[ULGCE-1142 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1142 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1142]]
[ULGCE-1142 (A faire)-2] -> [ULGCE-1142 (IN PROGRESS)-3]
[ULGCE-1142 (En Attente)-4] starts 2024/03/07 and ends 2024/04/17
[ULGCE-1142 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1142 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1142]]
[ULGCE-1142 (IN PROGRESS)-3] -> [ULGCE-1142 (En Attente)-4]
[ULGCE-1142 (A faire)-5] starts 2024/04/17 and ends 2024/08/19
[ULGCE-1142 (A faire)-5] is colored in LightGray/Gray
[ULGCE-1142 (A faire)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1142]]
[ULGCE-1142 (En Attente)-4] -> [ULGCE-1142 (A faire)-5]
[ULGCE-710 - création] -> [ULGCE-1102 Release manager- livraison R240500 - création]
[ULGCE-1102 Release manager- livraison R240500 - création] happens at 2024/02/02
[ULGCE-1102 Release manager- livraison R240500 - création] -> [ULGCE-1102 (En Test)-1]
[ULGCE-1102 (En Test)-1] starts 2024/02/02 and ends 2024/02/02
[ULGCE-1102 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1102 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1102]]
[ULGCE-1102 (Terminée)-2] starts 2024/02/02 and ends 2024/02/02
[ULGCE-1102 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1102 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1102]]
[ULGCE-1102 (En Test)-1] -> [ULGCE-1102 (Terminée)-2]
[ULGCE-710 - création] -> [ULGCE-1101 Release manager- livraison - création]
[ULGCE-1101 Release manager- livraison - création] happens at 2024/02/02
[ULGCE-1101 Release manager- livraison - création] -> [ULGCE-1101 (En Test)-1]
[ULGCE-1101 (En Test)-1] starts 2024/02/02 and ends 2024/02/02
[ULGCE-1101 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1101 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1101]]
[ULGCE-1101 (Terminée)-2] starts 2024/02/02 and ends 2024/02/02
[ULGCE-1101 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1101 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1101]]
[ULGCE-1101 (En Test)-1] -> [ULGCE-1101 (Terminée)-2]
[ULGCE-710 - création] -> [ULGCE-948 Release manager- livraison - création]
[ULGCE-948 Release manager- livraison - création] happens at 2024/01/11
[ULGCE-948 Release manager- livraison - création] -> [ULGCE-948 (En Test)-1]
[ULGCE-948 (En Test)-1] starts 2024/01/11 and ends 2024/01/12
[ULGCE-948 (En Test)-1] is colored in LightGray/Gray
[ULGCE-948 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-948]]
[ULGCE-948 (A faire)-2] starts 2024/01/12 and ends 2024/01/12
[ULGCE-948 (A faire)-2] is colored in LightGray/Gray
[ULGCE-948 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-948]]
[ULGCE-948 (En Test)-1] -> [ULGCE-948 (A faire)-2]
[ULGCE-948 (En Test)-3] starts 2024/01/12 and ends 2024/01/17
[ULGCE-948 (En Test)-3] is colored in LightGray/Gray
[ULGCE-948 (En Test)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-948]]
[ULGCE-948 (A faire)-2] -> [ULGCE-948 (En Test)-3]
[ULGCE-948 (Terminée)-4] starts 2024/01/17 and ends 2024/01/17
[ULGCE-948 (Terminée)-4] is colored in LightGray/Gray
[ULGCE-948 (Terminée)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-948]]
[ULGCE-948 (En Test)-3] -> [ULGCE-948 (Terminée)-4]
[ULGCE-710 - création] -> [ULGCE-869 Release manager- livraison - création]
[ULGCE-869 Release manager- livraison - création] happens at 2024/01/04
[ULGCE-869 Release manager- livraison - création] -> [ULGCE-869 (En Test)-1]
[ULGCE-869 (En Test)-1] starts 2024/01/04 and ends 2024/01/04
[ULGCE-869 (En Test)-1] is colored in LightGray/Gray
[ULGCE-869 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-869]]
[ULGCE-869 (A faire)-2] starts 2024/01/04 and ends 2024/01/09
[ULGCE-869 (A faire)-2] is colored in LightGray/Gray
[ULGCE-869 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-869]]
[ULGCE-869 (En Test)-1] -> [ULGCE-869 (A faire)-2]
[ULGCE-869 (En Test)-3] starts 2024/01/09 and ends 2024/01/11
[ULGCE-869 (En Test)-3] is colored in LightGray/Gray
[ULGCE-869 (En Test)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-869]]
[ULGCE-869 (A faire)-2] -> [ULGCE-869 (En Test)-3]
[ULGCE-869 (Terminée)-4] starts 2024/01/11 and ends 2024/01/11
[ULGCE-869 (Terminée)-4] is colored in LightGray/Gray
[ULGCE-869 (Terminée)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-869]]
[ULGCE-869 (En Test)-3] -> [ULGCE-869 (Terminée)-4]
[ULGCE-710 - création] -> [ULGCE-784 Release manager- livraison - création]
[ULGCE-784 Release manager- livraison - création] happens at 2023/12/20
[ULGCE-784 Release manager- livraison - création] -> [ULGCE-784 (En Test)-1]
[ULGCE-784 (En Test)-1] starts 2023/12/20 and ends 2024/01/03
[ULGCE-784 (En Test)-1] is colored in LightGray/Gray
[ULGCE-784 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-784]]
[ULGCE-784 (Terminée)-2] starts 2024/01/03 and ends 2024/01/04
[ULGCE-784 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-784 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-784]]
[ULGCE-784 (En Test)-1] -> [ULGCE-784 (Terminée)-2]
[ULGCE-710 - création] -> [ULGCE-712 Release manager- livraison - création]
[ULGCE-712 Release manager- livraison - création] happens at 2023/12/14
[ULGCE-712 Release manager- livraison - création] -> [ULGCE-712 (En Test)-1]
[ULGCE-712 (En Test)-1] starts 2023/12/14 and ends 2023/12/20
[ULGCE-712 (En Test)-1] is colored in LightGray/Gray
[ULGCE-712 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-712]]
[ULGCE-712 (Terminée)-2] starts 2023/12/20 and ends 2023/12/20
[ULGCE-712 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-712 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-712]]
[ULGCE-712 (En Test)-1] -> [ULGCE-712 (Terminée)-2]
[ULGCE-710 - création] -> [ULGCE-711 Release manager - Inetum - création]
[ULGCE-711 Release manager - Inetum - création] happens at 2023/12/14
[ULGCE-711 Release manager - Inetum - création] -> [ULGCE-711 (IN PROGRESS)-1]
[ULGCE-711 (IN PROGRESS)-1] starts 2023/12/14 and ends 2023/12/15
[ULGCE-711 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-711 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-711]]
[ULGCE-711 (En Test)-2] starts 2023/12/15 and ends 2023/12/20
[ULGCE-711 (En Test)-2] is colored in LightGray/Gray
[ULGCE-711 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-711]]
[ULGCE-711 (IN PROGRESS)-1] -> [ULGCE-711 (En Test)-2]
[ULGCE-711 (Terminée)-3] starts 2023/12/20 and ends 2023/12/20
[ULGCE-711 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-711 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-711]]
[ULGCE-711 (En Test)-2] -> [ULGCE-711 (Terminée)-3]
-- ULGCE-714 - ABSENCES --
[ULGCE-714 - création] happens on 2023/12/14
[ULGCE-714 - création] -> [ULGCE-2130 Absence du mois de septembre 24 - création]
[ULGCE-2130 Absence du mois de septembre 24 - création] happens at 2024/09/03
[ULGCE-2130 Absence du mois de septembre 24 - création] -> [ULGCE-2130 (IN PROGRESS)-1]
[ULGCE-2130 (IN PROGRESS)-1] starts 2024/09/03 and ends 2024/09/06
[ULGCE-2130 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2130 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2130]]
[ULGCE-714 - création] -> [ULGCE-2094 Absences - création]
[ULGCE-2094 Absences - création] happens at 2024/08/20
[ULGCE-2094 Absences - création] -> [ULGCE-2094 (En Test)-1]
[ULGCE-2094 (En Test)-1] starts 2024/08/20 and ends 2024/08/20
[ULGCE-2094 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2094 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2094]]
[ULGCE-2094 (Terminée)-2] starts 2024/08/20 and ends 2024/08/20
[ULGCE-2094 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2094 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2094]]
[ULGCE-2094 (En Test)-1] -> [ULGCE-2094 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-2077 Absence du 16/08 - création]
[ULGCE-2077 Absence du 16/08 - création] happens at 2024/08/14
[ULGCE-2077 Absence du 16/08 - création] -> [ULGCE-2077 (En Test)-1]
[ULGCE-2077 (En Test)-1] starts 2024/08/14 and ends 2024/08/14
[ULGCE-2077 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2077 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2077]]
[ULGCE-2077 (Terminée)-2] starts 2024/08/14 and ends 2024/08/14
[ULGCE-2077 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2077 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2077]]
[ULGCE-2077 (En Test)-1] -> [ULGCE-2077 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1985 Absences - création]
[ULGCE-1985 Absences - création] happens at 2024/07/18
[ULGCE-1985 Absences - création] -> [ULGCE-1985 (IN PROGRESS)-1]
[ULGCE-1985 (IN PROGRESS)-1] starts 2024/07/18 and ends 2024/07/18
[ULGCE-1985 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1985 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1985]]
[ULGCE-1985 (En Test)-2] starts 2024/07/18 and ends 2024/07/18
[ULGCE-1985 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1985 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1985]]
[ULGCE-1985 (IN PROGRESS)-1] -> [ULGCE-1985 (En Test)-2]
[ULGCE-1985 (Terminée)-3] starts 2024/07/18 and ends 2024/07/18
[ULGCE-1985 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1985 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1985]]
[ULGCE-1985 (En Test)-2] -> [ULGCE-1985 (Terminée)-3]
[ULGCE-714 - création] -> [ULGCE-1888 Absences - création]
[ULGCE-1888 Absences - création] happens at 2024/06/24
[ULGCE-1888 Absences - création] -> [ULGCE-1888 (En Test)-1]
[ULGCE-1888 (En Test)-1] starts 2024/06/24 and ends 2024/06/28
[ULGCE-1888 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1888 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1888]]
[ULGCE-1888 (Terminée)-2] starts 2024/06/28 and ends 2024/06/28
[ULGCE-1888 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1888 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1888]]
[ULGCE-1888 (En Test)-1] -> [ULGCE-1888 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1812 Absences - création]
[ULGCE-1812 Absences - création] happens at 2024/06/10
[ULGCE-1812 Absences - création] -> [ULGCE-1812 (En Test)-1]
[ULGCE-1812 (En Test)-1] starts 2024/06/10 and ends 2024/06/13
[ULGCE-1812 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1812 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1812]]
[ULGCE-1812 (Terminée)-2] starts 2024/06/13 and ends 2024/06/13
[ULGCE-1812 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1812 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1812]]
[ULGCE-1812 (En Test)-1] -> [ULGCE-1812 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1688 Absences - création]
[ULGCE-1688 Absences - création] happens at 2024/05/02
[ULGCE-1688 Absences - création] -> [ULGCE-1688 (IN PROGRESS)-1]
[ULGCE-1688 (IN PROGRESS)-1] starts 2024/05/02 and ends 2024/05/02
[ULGCE-1688 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1688 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1688]]
[ULGCE-1688 (En Test)-2] starts 2024/05/02 and ends 2024/05/03
[ULGCE-1688 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1688 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1688]]
[ULGCE-1688 (IN PROGRESS)-1] -> [ULGCE-1688 (En Test)-2]
[ULGCE-1688 (Terminée)-3] starts 2024/05/03 and ends 2024/05/03
[ULGCE-1688 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1688 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1688]]
[ULGCE-1688 (En Test)-2] -> [ULGCE-1688 (Terminée)-3]
[ULGCE-714 - création] -> [ULGCE-1684 Absences - création]
[ULGCE-1684 Absences - création] happens at 2024/04/30
[ULGCE-1684 Absences - création] -> [ULGCE-1684 (En Test)-1]
[ULGCE-1684 (En Test)-1] starts 2024/04/30 and ends 2024/04/30
[ULGCE-1684 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1684 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1684]]
[ULGCE-1684 (Terminée)-2] starts 2024/04/30 and ends 2024/04/30
[ULGCE-1684 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1684 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1684]]
[ULGCE-1684 (En Test)-1] -> [ULGCE-1684 (Terminée)-2]
[ULGCE-1684 (A faire)-3] starts 2024/04/30 and ends 2024/08/28
[ULGCE-1684 (A faire)-3] is colored in LightGray/Gray
[ULGCE-1684 (A faire)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1684]]
[ULGCE-1684 (Terminée)-2] -> [ULGCE-1684 (A faire)-3]
[ULGCE-1684 (En Test)-4] starts 2024/08/28 and ends 2024/08/28
[ULGCE-1684 (En Test)-4] is colored in LightGray/Gray
[ULGCE-1684 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1684]]
[ULGCE-1684 (A faire)-3] -> [ULGCE-1684 (En Test)-4]
[ULGCE-1684 (Terminée)-5] starts 2024/08/28 and ends 2024/08/28
[ULGCE-1684 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-1684 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1684]]
[ULGCE-1684 (En Test)-4] -> [ULGCE-1684 (Terminée)-5]
[ULGCE-714 - création] -> [ULGCE-1678 Absences - création]
[ULGCE-1678 Absences - création] happens at 2024/04/29
[ULGCE-1678 Absences - création] -> [ULGCE-1678 (En Test)-1]
[ULGCE-1678 (En Test)-1] starts 2024/04/29 and ends 2024/05/21
[ULGCE-1678 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1678 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1678]]
[ULGCE-1678 (Terminée)-2] starts 2024/05/21 and ends 2024/05/21
[ULGCE-1678 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1678 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1678]]
[ULGCE-1678 (En Test)-1] -> [ULGCE-1678 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1677 Absences - création]
[ULGCE-1677 Absences - création] happens at 2024/04/29
[ULGCE-1677 Absences - création] -> [ULGCE-1677 (En Test)-1]
[ULGCE-1677 (En Test)-1] starts 2024/04/29 and ends 2024/05/17
[ULGCE-1677 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1677 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1677]]
[ULGCE-1677 (Terminée)-2] starts 2024/05/17 and ends 2024/05/17
[ULGCE-1677 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1677 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1677]]
[ULGCE-1677 (En Test)-1] -> [ULGCE-1677 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1676 Absences - création]
[ULGCE-1676 Absences - création] happens at 2024/04/29
[ULGCE-1676 Absences - création] -> [ULGCE-1676 (En Test)-1]
[ULGCE-1676 (En Test)-1] starts 2024/04/29 and ends 2024/05/02
[ULGCE-1676 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1676 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1676]]
[ULGCE-1676 (Terminée)-2] starts 2024/05/02 and ends 2024/05/02
[ULGCE-1676 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1676 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1676]]
[ULGCE-1676 (En Test)-1] -> [ULGCE-1676 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1564 Absences - création]
[ULGCE-1564 Absences - création] happens at 2024/04/10
[ULGCE-1564 Absences - création] -> [ULGCE-1564 (IN PROGRESS)-1]
[ULGCE-1564 (IN PROGRESS)-1] starts 2024/04/10 and ends 2024/04/11
[ULGCE-1564 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1564 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1564]]
[ULGCE-1564 (En Test)-2] starts 2024/04/11 and ends 2024/04/11
[ULGCE-1564 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1564 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1564]]
[ULGCE-1564 (IN PROGRESS)-1] -> [ULGCE-1564 (En Test)-2]
[ULGCE-1564 (Terminée)-3] starts 2024/04/11 and ends 2024/04/11
[ULGCE-1564 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1564 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1564]]
[ULGCE-1564 (En Test)-2] -> [ULGCE-1564 (Terminée)-3]
[ULGCE-714 - création] -> [ULGCE-1527 Absences - création]
[ULGCE-1527 Absences - création] happens at 2024/04/08
[ULGCE-714 - création] -> [ULGCE-1526 Absences - création]
[ULGCE-1526 Absences - création] happens at 2024/04/08
[ULGCE-1526 Absences - création] -> [ULGCE-1526 (En Test)-1]
[ULGCE-1526 (En Test)-1] starts 2024/04/08 and ends 2024/04/17
[ULGCE-1526 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1526 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1526]]
[ULGCE-1526 (Terminée)-2] starts 2024/04/17 and ends 2024/04/17
[ULGCE-1526 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1526 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1526]]
[ULGCE-1526 (En Test)-1] -> [ULGCE-1526 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1517 Absences - création]
[ULGCE-1517 Absences - création] happens at 2024/04/08
[ULGCE-1517 Absences - création] -> [ULGCE-1517 (En Test)-1]
[ULGCE-1517 (En Test)-1] starts 2024/04/08 and ends 2024/05/27
[ULGCE-1517 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1517 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1517]]
[ULGCE-1517 (Terminée)-2] starts 2024/05/27 and ends 2024/05/27
[ULGCE-1517 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1517 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1517]]
[ULGCE-1517 (En Test)-1] -> [ULGCE-1517 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1516 Absences - création]
[ULGCE-1516 Absences - création] happens at 2024/04/08
[ULGCE-1516 Absences - création] -> [ULGCE-1516 (En Test)-1]
[ULGCE-1516 (En Test)-1] starts 2024/04/08 and ends 2024/05/27
[ULGCE-1516 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1516 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1516]]
[ULGCE-1516 (Terminée)-2] starts 2024/05/27 and ends 2024/05/27
[ULGCE-1516 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1516 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1516]]
[ULGCE-1516 (En Test)-1] -> [ULGCE-1516 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1481 Absences - création]
[ULGCE-1481 Absences - création] happens at 2024/04/02
[ULGCE-1481 Absences - création] -> [ULGCE-1481 (En Test)-1]
[ULGCE-1481 (En Test)-1] starts 2024/04/02 and ends 2024/05/27
[ULGCE-1481 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1481 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1481]]
[ULGCE-1481 (Terminée)-2] starts 2024/05/27 and ends 2024/05/27
[ULGCE-1481 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1481 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1481]]
[ULGCE-1481 (En Test)-1] -> [ULGCE-1481 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1480 Absences - création]
[ULGCE-1480 Absences - création] happens at 2024/04/02
[ULGCE-1480 Absences - création] -> [ULGCE-1480 (En Test)-1]
[ULGCE-1480 (En Test)-1] starts 2024/04/02 and ends 2024/04/08
[ULGCE-1480 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1480 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1480]]
[ULGCE-1480 (Terminée)-2] starts 2024/04/08 and ends 2024/04/08
[ULGCE-1480 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1480 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1480]]
[ULGCE-1480 (En Test)-1] -> [ULGCE-1480 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1479 Absences - création]
[ULGCE-1479 Absences - création] happens at 2024/04/02
[ULGCE-1479 Absences - création] -> [ULGCE-1479 (En Test)-1]
[ULGCE-1479 (En Test)-1] starts 2024/04/02 and ends 2024/04/05
[ULGCE-1479 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1479 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1479]]
[ULGCE-1479 (Terminée)-2] starts 2024/04/05 and ends 2024/04/05
[ULGCE-1479 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1479 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1479]]
[ULGCE-1479 (En Test)-1] -> [ULGCE-1479 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1478 Absences - création]
[ULGCE-1478 Absences - création] happens at 2024/04/02
[ULGCE-1478 Absences - création] -> [ULGCE-1478 (IN PROGRESS)-1]
[ULGCE-1478 (IN PROGRESS)-1] starts 2024/04/02 and ends 2024/04/03
[ULGCE-1478 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1478 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1478]]
[ULGCE-1478 (En Attente)-2] starts 2024/04/03 and ends 2024/04/03
[ULGCE-1478 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1478 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1478]]
[ULGCE-1478 (IN PROGRESS)-1] -> [ULGCE-1478 (En Attente)-2]
[ULGCE-1478 (IN PROGRESS)-3] starts 2024/04/03 and ends 2024/04/03
[ULGCE-1478 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1478 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1478]]
[ULGCE-1478 (En Attente)-2] -> [ULGCE-1478 (IN PROGRESS)-3]
[ULGCE-1478 (En Attente)-4] starts 2024/04/03 and ends 2024/04/03
[ULGCE-1478 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1478 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1478]]
[ULGCE-1478 (IN PROGRESS)-3] -> [ULGCE-1478 (En Attente)-4]
[ULGCE-1478 (A faire)-5] starts 2024/04/03 and ends 2024/04/03
[ULGCE-1478 (A faire)-5] is colored in LightGray/Gray
[ULGCE-1478 (A faire)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1478]]
[ULGCE-1478 (En Attente)-4] -> [ULGCE-1478 (A faire)-5]
[ULGCE-1478 (En Test)-6] starts 2024/04/03 and ends 2024/04/03
[ULGCE-1478 (En Test)-6] is colored in LightGray/Gray
[ULGCE-1478 (En Test)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1478]]
[ULGCE-1478 (A faire)-5] -> [ULGCE-1478 (En Test)-6]
[ULGCE-1478 (Terminée)-7] starts 2024/04/03 and ends 2024/04/03
[ULGCE-1478 (Terminée)-7] is colored in LightGray/Gray
[ULGCE-1478 (Terminée)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1478]]
[ULGCE-1478 (En Test)-6] -> [ULGCE-1478 (Terminée)-7]
[ULGCE-714 - création] -> [ULGCE-1450 Absences - création]
[ULGCE-1450 Absences - création] happens at 2024/03/27
[ULGCE-1450 Absences - création] -> [ULGCE-1450 (IN PROGRESS)-1]
[ULGCE-1450 (IN PROGRESS)-1] starts 2024/03/27 and ends 2024/03/28
[ULGCE-1450 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1450 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (En Attente)-2] starts 2024/03/28 and ends 2024/03/28
[ULGCE-1450 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-1450 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (IN PROGRESS)-1] -> [ULGCE-1450 (En Attente)-2]
[ULGCE-1450 (IN PROGRESS)-3] starts 2024/03/28 and ends 2024/03/28
[ULGCE-1450 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1450 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (En Attente)-2] -> [ULGCE-1450 (IN PROGRESS)-3]
[ULGCE-1450 (En Attente)-4] starts 2024/03/28 and ends 2024/03/28
[ULGCE-1450 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1450 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (IN PROGRESS)-3] -> [ULGCE-1450 (En Attente)-4]
[ULGCE-1450 (IN PROGRESS)-5] starts 2024/03/28 and ends 2024/04/08
[ULGCE-1450 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1450 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (En Attente)-4] -> [ULGCE-1450 (IN PROGRESS)-5]
[ULGCE-1450 (En Test)-6] starts 2024/04/08 and ends 2024/04/08
[ULGCE-1450 (En Test)-6] is colored in LightGray/Gray
[ULGCE-1450 (En Test)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (IN PROGRESS)-5] -> [ULGCE-1450 (En Test)-6]
[ULGCE-1450 (Terminée)-7] starts 2024/04/08 and ends 2024/04/08
[ULGCE-1450 (Terminée)-7] is colored in LightGray/Gray
[ULGCE-1450 (Terminée)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (En Test)-6] -> [ULGCE-1450 (Terminée)-7]
[ULGCE-1450 (A faire)-8] starts 2024/04/08 and ends 2024/04/10
[ULGCE-1450 (A faire)-8] is colored in LightGray/Gray
[ULGCE-1450 (A faire)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (Terminée)-7] -> [ULGCE-1450 (A faire)-8]
[ULGCE-1450 (IN PROGRESS)-9] starts 2024/04/10 and ends 2024/04/10
[ULGCE-1450 (IN PROGRESS)-9] is colored in LightGray/Gray
[ULGCE-1450 (IN PROGRESS)-9] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (A faire)-8] -> [ULGCE-1450 (IN PROGRESS)-9]
[ULGCE-1450 (En Attente)-10] starts 2024/04/10 and ends 2024/04/10
[ULGCE-1450 (En Attente)-10] is colored in LightGray/Gray
[ULGCE-1450 (En Attente)-10] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (IN PROGRESS)-9] -> [ULGCE-1450 (En Attente)-10]
[ULGCE-1450 (A faire)-11] starts 2024/04/10 and ends 2024/04/21
[ULGCE-1450 (A faire)-11] is colored in LightGray/Gray
[ULGCE-1450 (A faire)-11] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (En Attente)-10] -> [ULGCE-1450 (A faire)-11]
[ULGCE-1450 (En Test)-12] starts 2024/04/21 and ends 2024/04/21
[ULGCE-1450 (En Test)-12] is colored in LightGray/Gray
[ULGCE-1450 (En Test)-12] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (A faire)-11] -> [ULGCE-1450 (En Test)-12]
[ULGCE-1450 (Terminée)-13] starts 2024/04/21 and ends 2024/04/21
[ULGCE-1450 (Terminée)-13] is colored in LightGray/Gray
[ULGCE-1450 (Terminée)-13] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1450]]
[ULGCE-1450 (En Test)-12] -> [ULGCE-1450 (Terminée)-13]
[ULGCE-714 - création] -> [ULGCE-1426 Absences - création]
[ULGCE-1426 Absences - création] happens at 2024/03/25
[ULGCE-1426 Absences - création] -> [ULGCE-1426 (En Test)-1]
[ULGCE-1426 (En Test)-1] starts 2024/03/25 and ends 2024/03/27
[ULGCE-1426 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1426 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1426]]
[ULGCE-1426 (Terminée)-2] starts 2024/03/27 and ends 2024/03/27
[ULGCE-1426 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1426 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1426]]
[ULGCE-1426 (En Test)-1] -> [ULGCE-1426 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1325 Absences - création]
[ULGCE-1325 Absences - création] happens at 2024/03/11
[ULGCE-714 - création] -> [ULGCE-1324 Absences - création]
[ULGCE-1324 Absences - création] happens at 2024/03/11
[ULGCE-1324 Absences - création] -> [ULGCE-1324 (En Test)-1]
[ULGCE-1324 (En Test)-1] starts 2024/03/11 and ends 2024/03/25
[ULGCE-1324 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1324 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1324]]
[ULGCE-1324 (Terminée)-2] starts 2024/03/25 and ends 2024/03/25
[ULGCE-1324 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1324 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1324]]
[ULGCE-1324 (En Test)-1] -> [ULGCE-1324 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1320 Absences - création]
[ULGCE-1320 Absences - création] happens at 2024/03/11
[ULGCE-1320 Absences - création] -> [ULGCE-1320 (En Test)-1]
[ULGCE-1320 (En Test)-1] starts 2024/03/11 and ends 2024/03/21
[ULGCE-1320 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1320 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1320]]
[ULGCE-1320 (Terminée)-2] starts 2024/03/21 and ends 2024/03/21
[ULGCE-1320 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1320 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1320]]
[ULGCE-1320 (En Test)-1] -> [ULGCE-1320 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1319 Absences - création]
[ULGCE-1319 Absences - création] happens at 2024/03/11
[ULGCE-1319 Absences - création] -> [ULGCE-1319 (En Test)-1]
[ULGCE-1319 (En Test)-1] starts 2024/03/11 and ends 2024/03/14
[ULGCE-1319 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1319 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1319]]
[ULGCE-1319 (Terminée)-2] starts 2024/03/14 and ends 2024/03/14
[ULGCE-1319 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1319 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1319]]
[ULGCE-1319 (En Test)-1] -> [ULGCE-1319 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1298 Absences - création]
[ULGCE-1298 Absences - création] happens at 2024/03/05
[ULGCE-1298 Absences - création] -> [ULGCE-1298 (IN PROGRESS)-1]
[ULGCE-1298 (IN PROGRESS)-1] starts 2024/03/05 and ends 2024/03/07
[ULGCE-1298 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1298 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1298]]
[ULGCE-1298 (En Test)-2] starts 2024/03/07 and ends 2024/03/07
[ULGCE-1298 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1298 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1298]]
[ULGCE-1298 (IN PROGRESS)-1] -> [ULGCE-1298 (En Test)-2]
[ULGCE-1298 (Terminée)-3] starts 2024/03/07 and ends 2024/03/07
[ULGCE-1298 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1298 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1298]]
[ULGCE-1298 (En Test)-2] -> [ULGCE-1298 (Terminée)-3]
[ULGCE-714 - création] -> [ULGCE-1288 Absences - création]
[ULGCE-1288 Absences - création] happens at 2024/03/04
[ULGCE-1288 Absences - création] -> [ULGCE-1288 (En Test)-1]
[ULGCE-1288 (En Test)-1] starts 2024/03/04 and ends 2024/03/25
[ULGCE-1288 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1288 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1288]]
[ULGCE-1288 (Terminée)-2] starts 2024/03/25 and ends 2024/03/25
[ULGCE-1288 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1288 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1288]]
[ULGCE-1288 (En Test)-1] -> [ULGCE-1288 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1287 Absences - création]
[ULGCE-1287 Absences - création] happens at 2024/03/04
[ULGCE-1287 Absences - création] -> [ULGCE-1287 (En Test)-1]
[ULGCE-1287 (En Test)-1] starts 2024/03/04 and ends 2024/03/04
[ULGCE-1287 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1287 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1287]]
[ULGCE-1287 (Terminée)-2] starts 2024/03/04 and ends 2024/03/04
[ULGCE-1287 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1287 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1287]]
[ULGCE-1287 (En Test)-1] -> [ULGCE-1287 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1260 Absences - création]
[ULGCE-1260 Absences - création] happens at 2024/02/25
[ULGCE-1260 Absences - création] -> [ULGCE-1260 (En Test)-1]
[ULGCE-1260 (En Test)-1] starts 2024/02/25 and ends 2024/03/04
[ULGCE-1260 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1260 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1260]]
[ULGCE-1260 (Terminée)-2] starts 2024/03/04 and ends 2024/03/04
[ULGCE-1260 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1260 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1260]]
[ULGCE-1260 (En Test)-1] -> [ULGCE-1260 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1228 Absences - création]
[ULGCE-1228 Absences - création] happens at 2024/02/20
[ULGCE-1228 Absences - création] -> [ULGCE-1228 (En Test)-1]
[ULGCE-1228 (En Test)-1] starts 2024/02/20 and ends 2024/02/20
[ULGCE-1228 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1228 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1228]]
[ULGCE-1228 (Terminée)-2] starts 2024/02/20 and ends 2024/02/20
[ULGCE-1228 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1228 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1228]]
[ULGCE-1228 (En Test)-1] -> [ULGCE-1228 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-1227 Absences - création]
[ULGCE-1227 Absences - création] happens at 2024/02/20
[ULGCE-1227 Absences - création] -> [ULGCE-1227 (IN PROGRESS)-1]
[ULGCE-1227 (IN PROGRESS)-1] starts 2024/02/20 and ends 2024/02/21
[ULGCE-1227 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1227 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1227]]
[ULGCE-1227 (En Test)-2] starts 2024/02/21 and ends 2024/02/21
[ULGCE-1227 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1227 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1227]]
[ULGCE-1227 (IN PROGRESS)-1] -> [ULGCE-1227 (En Test)-2]
[ULGCE-1227 (Terminée)-3] starts 2024/02/21 and ends 2024/02/21
[ULGCE-1227 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1227 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1227]]
[ULGCE-1227 (En Test)-2] -> [ULGCE-1227 (Terminée)-3]
[ULGCE-714 - création] -> [ULGCE-1160 Absences - création]
[ULGCE-1160 Absences - création] happens at 2024/02/09
[ULGCE-1160 Absences - création] -> [ULGCE-1160 (IN PROGRESS)-1]
[ULGCE-1160 (IN PROGRESS)-1] starts 2024/02/09 and ends 2024/02/15
[ULGCE-1160 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1160 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1160]]
[ULGCE-1160 (En Test)-2] starts 2024/02/15 and ends 2024/02/15
[ULGCE-1160 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1160 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1160]]
[ULGCE-1160 (IN PROGRESS)-1] -> [ULGCE-1160 (En Test)-2]
[ULGCE-1160 (Terminée)-3] starts 2024/02/15 and ends 2024/02/15
[ULGCE-1160 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1160 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1160]]
[ULGCE-1160 (En Test)-2] -> [ULGCE-1160 (Terminée)-3]
[ULGCE-714 - création] -> [ULGCE-1146 Absences - création]
[ULGCE-1146 Absences - création] happens at 2024/02/07
[ULGCE-1146 Absences - création] -> [ULGCE-1146 (IN PROGRESS)-1]
[ULGCE-1146 (IN PROGRESS)-1] starts 2024/02/07 and ends 2024/02/07
[ULGCE-1146 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1146 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1146]]
[ULGCE-1146 (En Test)-2] starts 2024/02/07 and ends 2024/02/07
[ULGCE-1146 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1146 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1146]]
[ULGCE-1146 (IN PROGRESS)-1] -> [ULGCE-1146 (En Test)-2]
[ULGCE-1146 (Terminée)-3] starts 2024/02/07 and ends 2024/02/07
[ULGCE-1146 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1146 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1146]]
[ULGCE-1146 (En Test)-2] -> [ULGCE-1146 (Terminée)-3]
[ULGCE-714 - création] -> [ULGCE-1010 Absences - création]
[ULGCE-1010 Absences - création] happens at 2024/01/17
[ULGCE-1010 Absences - création] -> [ULGCE-1010 (En Test)-1]
[ULGCE-1010 (En Test)-1] starts 2024/01/17 and ends 2024/01/31
[ULGCE-1010 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1010 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1010]]
[ULGCE-1010 (Terminée)-2] starts 2024/01/31 and ends 2024/01/31
[ULGCE-1010 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1010 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1010]]
[ULGCE-1010 (En Test)-1] -> [ULGCE-1010 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-941 Absences - création]
[ULGCE-941 Absences - création] happens at 2024/01/11
[ULGCE-941 Absences - création] -> [ULGCE-941 (En Test)-1]
[ULGCE-941 (En Test)-1] starts 2024/01/11 and ends 2024/01/17
[ULGCE-941 (En Test)-1] is colored in LightGray/Gray
[ULGCE-941 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-941]]
[ULGCE-941 (A faire)-2] starts 2024/01/17 and ends 2024/01/17
[ULGCE-941 (A faire)-2] is colored in LightGray/Gray
[ULGCE-941 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-941]]
[ULGCE-941 (En Test)-1] -> [ULGCE-941 (A faire)-2]
[ULGCE-941 (En Test)-3] starts 2024/01/17 and ends 2024/01/17
[ULGCE-941 (En Test)-3] is colored in LightGray/Gray
[ULGCE-941 (En Test)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-941]]
[ULGCE-941 (A faire)-2] -> [ULGCE-941 (En Test)-3]
[ULGCE-941 (Terminée)-4] starts 2024/01/17 and ends 2024/01/17
[ULGCE-941 (Terminée)-4] is colored in LightGray/Gray
[ULGCE-941 (Terminée)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-941]]
[ULGCE-941 (En Test)-3] -> [ULGCE-941 (Terminée)-4]
[ULGCE-714 - création] -> [ULGCE-928 Absences - création]
[ULGCE-928 Absences - création] happens at 2024/01/11
[ULGCE-928 Absences - création] -> [ULGCE-928 (En Test)-1]
[ULGCE-928 (En Test)-1] starts 2024/01/11 and ends 2024/01/25
[ULGCE-928 (En Test)-1] is colored in LightGray/Gray
[ULGCE-928 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-928]]
[ULGCE-928 (Terminée)-2] starts 2024/01/25 and ends 2024/01/25
[ULGCE-928 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-928 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-928]]
[ULGCE-928 (En Test)-1] -> [ULGCE-928 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-927 Absences - création]
[ULGCE-927 Absences - création] happens at 2024/01/11
[ULGCE-927 Absences - création] -> [ULGCE-927 (En Test)-1]
[ULGCE-927 (En Test)-1] starts 2024/01/11 and ends 2024/01/18
[ULGCE-927 (En Test)-1] is colored in LightGray/Gray
[ULGCE-927 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-927]]
[ULGCE-927 (Terminée)-2] starts 2024/01/18 and ends 2024/01/18
[ULGCE-927 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-927 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-927]]
[ULGCE-927 (En Test)-1] -> [ULGCE-927 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-926 Absences - création]
[ULGCE-926 Absences - création] happens at 2024/01/11
[ULGCE-926 Absences - création] -> [ULGCE-926 (IN PROGRESS)-1]
[ULGCE-926 (IN PROGRESS)-1] starts 2024/01/11 and ends 2024/01/18
[ULGCE-926 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-926 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-926]]
[ULGCE-926 (En Test)-2] starts 2024/01/18 and ends 2024/01/18
[ULGCE-926 (En Test)-2] is colored in LightGray/Gray
[ULGCE-926 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-926]]
[ULGCE-926 (IN PROGRESS)-1] -> [ULGCE-926 (En Test)-2]
[ULGCE-926 (Terminée)-3] starts 2024/01/18 and ends 2024/01/18
[ULGCE-926 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-926 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-926]]
[ULGCE-926 (En Test)-2] -> [ULGCE-926 (Terminée)-3]
[ULGCE-714 - création] -> [ULGCE-925 Absences - création]
[ULGCE-925 Absences - création] happens at 2024/01/11
[ULGCE-925 Absences - création] -> [ULGCE-925 (En Test)-1]
[ULGCE-925 (En Test)-1] starts 2024/01/11 and ends 2024/01/17
[ULGCE-925 (En Test)-1] is colored in LightGray/Gray
[ULGCE-925 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-925]]
[ULGCE-925 (Terminée)-2] starts 2024/01/17 and ends 2024/01/17
[ULGCE-925 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-925 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-925]]
[ULGCE-925 (En Test)-1] -> [ULGCE-925 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-772 Absences - création]
[ULGCE-772 Absences - création] happens at 2023/12/20
[ULGCE-772 Absences - création] -> [ULGCE-772 (En Test)-1]
[ULGCE-772 (En Test)-1] starts 2023/12/20 and ends 2023/12/21
[ULGCE-772 (En Test)-1] is colored in LightGray/Gray
[ULGCE-772 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-772]]
[ULGCE-772 (Terminée)-2] starts 2023/12/21 and ends 2023/12/21
[ULGCE-772 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-772 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-772]]
[ULGCE-772 (En Test)-1] -> [ULGCE-772 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-771 Absences - création]
[ULGCE-771 Absences - création] happens at 2023/12/20
[ULGCE-771 Absences - création] -> [ULGCE-771 (En Test)-1]
[ULGCE-771 (En Test)-1] starts 2023/12/20 and ends 2023/12/27
[ULGCE-771 (En Test)-1] is colored in LightGray/Gray
[ULGCE-771 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-771]]
[ULGCE-771 (Terminée)-2] starts 2023/12/27 and ends 2023/12/27
[ULGCE-771 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-771 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-771]]
[ULGCE-771 (En Test)-1] -> [ULGCE-771 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-770 Absences - création]
[ULGCE-770 Absences - création] happens at 2023/12/20
[ULGCE-770 Absences - création] -> [ULGCE-770 (IN PROGRESS)-1]
[ULGCE-770 (IN PROGRESS)-1] starts 2023/12/20 and ends 2023/12/29
[ULGCE-770 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-770 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-770]]
[ULGCE-770 (En Test)-2] starts 2023/12/29 and ends 2023/12/29
[ULGCE-770 (En Test)-2] is colored in LightGray/Gray
[ULGCE-770 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-770]]
[ULGCE-770 (IN PROGRESS)-1] -> [ULGCE-770 (En Test)-2]
[ULGCE-770 (Terminée)-3] starts 2023/12/29 and ends 2023/12/29
[ULGCE-770 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-770 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-770]]
[ULGCE-770 (En Test)-2] -> [ULGCE-770 (Terminée)-3]
[ULGCE-714 - création] -> [ULGCE-769 Absences - création]
[ULGCE-769 Absences - création] happens at 2023/12/20
[ULGCE-769 Absences - création] -> [ULGCE-769 (En Test)-1]
[ULGCE-769 (En Test)-1] starts 2023/12/20 and ends 2023/12/21
[ULGCE-769 (En Test)-1] is colored in LightGray/Gray
[ULGCE-769 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-769]]
[ULGCE-769 (Terminée)-2] starts 2023/12/21 and ends 2023/12/21
[ULGCE-769 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-769 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-769]]
[ULGCE-769 (En Test)-1] -> [ULGCE-769 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-725 Absences - création]
[ULGCE-725 Absences - création] happens at 2023/12/15
[ULGCE-725 Absences - création] -> [ULGCE-725 (IN PROGRESS)-1]
[ULGCE-725 (IN PROGRESS)-1] starts 2023/12/15 and ends 2024/01/02
[ULGCE-725 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-725 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-725]]
[ULGCE-725 (En Attente)-2] starts 2024/01/02 and ends 2024/01/02
[ULGCE-725 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-725 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-725]]
[ULGCE-725 (IN PROGRESS)-1] -> [ULGCE-725 (En Attente)-2]
[ULGCE-725 (IN PROGRESS)-3] starts 2024/01/02 and ends 2024/01/02
[ULGCE-725 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-725 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-725]]
[ULGCE-725 (En Attente)-2] -> [ULGCE-725 (IN PROGRESS)-3]
[ULGCE-725 (En Test)-4] starts 2024/01/02 and ends 2024/01/02
[ULGCE-725 (En Test)-4] is colored in LightGray/Gray
[ULGCE-725 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-725]]
[ULGCE-725 (IN PROGRESS)-3] -> [ULGCE-725 (En Test)-4]
[ULGCE-725 (Terminée)-5] starts 2024/01/02 and ends 2024/01/02
[ULGCE-725 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-725 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-725]]
[ULGCE-725 (En Test)-4] -> [ULGCE-725 (Terminée)-5]
[ULGCE-714 - création] -> [ULGCE-724 Absences - création]
[ULGCE-724 Absences - création] happens at 2023/12/15
[ULGCE-724 Absences - création] -> [ULGCE-724 (En Test)-1]
[ULGCE-724 (En Test)-1] starts 2023/12/15 and ends 2023/12/27
[ULGCE-724 (En Test)-1] is colored in LightGray/Gray
[ULGCE-724 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-724]]
[ULGCE-724 (Terminée)-2] starts 2023/12/27 and ends 2023/12/27
[ULGCE-724 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-724 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-724]]
[ULGCE-724 (En Test)-1] -> [ULGCE-724 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-717 Absences - création]
[ULGCE-717 Absences - création] happens at 2023/12/14
[ULGCE-717 Absences - création] -> [ULGCE-717 (En Test)-1]
[ULGCE-717 (En Test)-1] starts 2023/12/14 and ends 2023/12/18
[ULGCE-717 (En Test)-1] is colored in LightGray/Gray
[ULGCE-717 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-717]]
[ULGCE-717 (Terminée)-2] starts 2023/12/18 and ends 2023/12/18
[ULGCE-717 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-717 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-717]]
[ULGCE-717 (En Test)-1] -> [ULGCE-717 (Terminée)-2]
[ULGCE-714 - création] -> [ULGCE-716 Absences - création]
[ULGCE-716 Absences - création] happens at 2023/12/14
[ULGCE-716 Absences - création] -> [ULGCE-716 (IN PROGRESS)-1]
[ULGCE-716 (IN PROGRESS)-1] starts 2023/12/14 and ends 2023/12/19
[ULGCE-716 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-716 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-716]]
[ULGCE-716 (En Test)-2] starts 2023/12/19 and ends 2023/12/19
[ULGCE-716 (En Test)-2] is colored in LightGray/Gray
[ULGCE-716 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-716]]
[ULGCE-716 (IN PROGRESS)-1] -> [ULGCE-716 (En Test)-2]
[ULGCE-716 (Terminée)-3] starts 2023/12/19 and ends 2023/12/19
[ULGCE-716 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-716 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-716]]
[ULGCE-716 (En Test)-2] -> [ULGCE-716 (Terminée)-3]
[ULGCE-714 - création] -> [ULGCE-715 Absences - création]
[ULGCE-715 Absences - création] happens at 2023/12/14
[ULGCE-715 Absences - création] -> [ULGCE-715 (IN PROGRESS)-1]
[ULGCE-715 (IN PROGRESS)-1] starts 2023/12/14 and ends 2023/12/14
[ULGCE-715 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-715 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-715]]
[ULGCE-715 (En Test)-2] starts 2023/12/14 and ends 2023/12/14
[ULGCE-715 (En Test)-2] is colored in LightGray/Gray
[ULGCE-715 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-715]]
[ULGCE-715 (IN PROGRESS)-1] -> [ULGCE-715 (En Test)-2]
[ULGCE-715 (Terminée)-3] starts 2023/12/14 and ends 2023/12/14
[ULGCE-715 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-715 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-715]]
[ULGCE-715 (En Test)-2] -> [ULGCE-715 (Terminée)-3]
-- ULGCE-801 - [#905393] TP78 - Evolution Codification CTP/CTP déduction dans tables de répartition REPR/REEC/SRPS catégories 1&8 --
[ULGCE-801 - création] happens on 2023/12/21
[ULGCE-801 - création] -> [ULGCE-1373 #905393_859660_EVOLUTION_ALGORYTHMIE - création]
[ULGCE-1373 #905393_859660_EVOLUTION_ALGORYTHMIE - création] happens at 2024/03/16
[ULGCE-1373 #905393_859660_EVOLUTION_ALGORYTHMIE - création] -> [ULGCE-1373 (IN PROGRESS)-1]
[ULGCE-1373 (IN PROGRESS)-1] starts 2024/03/16 and ends 2024/03/19
[ULGCE-1373 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-1373 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1373]]
[ULGCE-1373 (En Test)-2] starts 2024/03/19 and ends 2024/04/05
[ULGCE-1373 (En Test)-2] is colored in LightGray/Gray
[ULGCE-1373 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1373]]
[ULGCE-1373 (IN PROGRESS)-1] -> [ULGCE-1373 (En Test)-2]
[ULGCE-1373 (Terminée)-3] starts 2024/04/05 and ends 2024/04/05
[ULGCE-1373 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-1373 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1373]]
[ULGCE-1373 (En Test)-2] -> [ULGCE-1373 (Terminée)-3]
[ULGCE-801 - création] -> [ULGCE-825 #905393_859660_EVOLUTION_RETOUR RECETTE - création]
[ULGCE-825 #905393_859660_EVOLUTION_RETOUR RECETTE - création] happens at 2023/12/27
[ULGCE-825 #905393_859660_EVOLUTION_RETOUR RECETTE - création] -> [ULGCE-825 (IN PROGRESS)-1]
[ULGCE-825 (IN PROGRESS)-1] starts 2023/12/27 and ends 2024/05/07
[ULGCE-825 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-825 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-825]]
[ULGCE-825 (En Attente)-2] starts 2024/05/07 and ends 2024/05/15
[ULGCE-825 (En Attente)-2] is colored in LightGray/Gray
[ULGCE-825 (En Attente)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-825]]
[ULGCE-825 (IN PROGRESS)-1] -> [ULGCE-825 (En Attente)-2]
[ULGCE-825 (IN PROGRESS)-3] starts 2024/05/15 and ends 2024/05/21
[ULGCE-825 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-825 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-825]]
[ULGCE-825 (En Attente)-2] -> [ULGCE-825 (IN PROGRESS)-3]
[ULGCE-825 (En Test)-4] starts 2024/05/21 and ends 2024/05/21
[ULGCE-825 (En Test)-4] is colored in LightGray/Gray
[ULGCE-825 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-825]]
[ULGCE-825 (IN PROGRESS)-3] -> [ULGCE-825 (En Test)-4]
[ULGCE-825 (Terminée)-5] starts 2024/05/21 and ends 2024/05/21
[ULGCE-825 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-825 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-825]]
[ULGCE-825 (En Test)-4] -> [ULGCE-825 (Terminée)-5]
[ULGCE-825 (A faire)-6] starts 2024/05/21 and ends 2024/08/05
[ULGCE-825 (A faire)-6] is colored in LightGray/Gray
[ULGCE-825 (A faire)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-825]]
[ULGCE-825 (Terminée)-5] -> [ULGCE-825 (A faire)-6]
[ULGCE-825 (En Test)-7] starts 2024/08/05 and ends 2024/08/13
[ULGCE-825 (En Test)-7] is colored in LightGray/Gray
[ULGCE-825 (En Test)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-825]]
[ULGCE-825 (A faire)-6] -> [ULGCE-825 (En Test)-7]
[ULGCE-825 (A faire)-8] starts 2024/08/13 and ends 2024/09/06
[ULGCE-825 (A faire)-8] is colored in LightGray/Gray
[ULGCE-825 (A faire)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-825]]
[ULGCE-825 (En Test)-7] -> [ULGCE-825 (A faire)-8]
[ULGCE-801 - création] -> [ULGCE-824 #905393_859660_EVOLUTION_TESTS - création]
[ULGCE-824 #905393_859660_EVOLUTION_TESTS - création] happens at 2023/12/27
[ULGCE-824 #905393_859660_EVOLUTION_TESTS - création] -> [ULGCE-824 (IN PROGRESS)-1]
[ULGCE-824 (IN PROGRESS)-1] starts 2023/12/27 and ends 2024/03/26
[ULGCE-824 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-824 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-824]]
[ULGCE-824 (En Test)-2] starts 2024/03/26 and ends 2024/05/03
[ULGCE-824 (En Test)-2] is colored in LightGray/Gray
[ULGCE-824 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-824]]
[ULGCE-824 (IN PROGRESS)-1] -> [ULGCE-824 (En Test)-2]
[ULGCE-824 (Terminée)-3] starts 2024/05/03 and ends 2024/05/03
[ULGCE-824 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-824 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-824]]
[ULGCE-824 (En Test)-2] -> [ULGCE-824 (Terminée)-3]
[ULGCE-801 - création] -> [ULGCE-823 #905393_859660_EVOLUTION_DOSSIER DE TEST (ALM) - création]
[ULGCE-823 #905393_859660_EVOLUTION_DOSSIER DE TEST (ALM) - création] happens at 2023/12/27
[ULGCE-823 #905393_859660_EVOLUTION_DOSSIER DE TEST (ALM) - création] -> [ULGCE-823 (En Test)-1]
[ULGCE-823 (En Test)-1] starts 2023/12/27 and ends 2024/05/03
[ULGCE-823 (En Test)-1] is colored in LightGray/Gray
[ULGCE-823 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-823]]
[ULGCE-823 (Terminée)-2] starts 2024/05/03 and ends 2024/05/03
[ULGCE-823 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-823 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-823]]
[ULGCE-823 (En Test)-1] -> [ULGCE-823 (Terminée)-2]
[ULGCE-801 - création] -> [ULGCE-822 #905393_859660_EVOLUTION_PACKAGING/LIVRAISON - création]
[ULGCE-822 #905393_859660_EVOLUTION_PACKAGING/LIVRAISON - création] happens at 2023/12/27
[ULGCE-801 - création] -> [ULGCE-821 #905393_859660_EVOLUTION_DEVELOPPEMENT - création]
[ULGCE-821 #905393_859660_EVOLUTION_DEVELOPPEMENT - création] happens at 2023/12/27
[ULGCE-821 #905393_859660_EVOLUTION_DEVELOPPEMENT - création] -> [ULGCE-821 (En Test)-1]
[ULGCE-821 (En Test)-1] starts 2023/12/27 and ends 2024/03/19
[ULGCE-821 (En Test)-1] is colored in LightGray/Gray
[ULGCE-821 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-821]]
[ULGCE-821 (Terminée)-2] starts 2024/03/19 and ends 2024/04/23
[ULGCE-821 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-821 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-821]]
[ULGCE-821 (En Test)-1] -> [ULGCE-821 (Terminée)-2]
[ULGCE-821 (A faire)-3] starts 2024/04/23 and ends 2024/04/23
[ULGCE-821 (A faire)-3] is colored in LightGray/Gray
[ULGCE-821 (A faire)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-821]]
[ULGCE-821 (Terminée)-2] -> [ULGCE-821 (A faire)-3]
[ULGCE-821 (En Test)-4] starts 2024/04/23 and ends 2024/04/23
[ULGCE-821 (En Test)-4] is colored in LightGray/Gray
[ULGCE-821 (En Test)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-821]]
[ULGCE-821 (A faire)-3] -> [ULGCE-821 (En Test)-4]
[ULGCE-821 (Terminée)-5] starts 2024/04/23 and ends 2024/04/23
[ULGCE-821 (Terminée)-5] is colored in LightGray/Gray
[ULGCE-821 (Terminée)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-821]]
[ULGCE-821 (En Test)-4] -> [ULGCE-821 (Terminée)-5]
[ULGCE-801 - création] -> [ULGCE-820 #905393_859660_EVOLUTION_STRATEGIE DE TESTS - création]
[ULGCE-820 #905393_859660_EVOLUTION_STRATEGIE DE TESTS - création] happens at 2023/12/27
[ULGCE-801 - création] -> [ULGCE-819 #905393_859660_EVOLUTION_CONCEPTION TECHNIQUE - création]
[ULGCE-819 #905393_859660_EVOLUTION_CONCEPTION TECHNIQUE - création] happens at 2023/12/27
[ULGCE-819 #905393_859660_EVOLUTION_CONCEPTION TECHNIQUE - création] -> [ULGCE-819 (IN PROGRESS)-1]
[ULGCE-819 (IN PROGRESS)-1] starts 2023/12/27 and ends 2024/03/14
[ULGCE-819 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-819 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-819]]
[ULGCE-819 (En Test)-2] starts 2024/03/14 and ends 2024/03/19
[ULGCE-819 (En Test)-2] is colored in LightGray/Gray
[ULGCE-819 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-819]]
[ULGCE-819 (IN PROGRESS)-1] -> [ULGCE-819 (En Test)-2]
[ULGCE-819 (A faire)-3] starts 2024/03/19 and ends 2024/03/22
[ULGCE-819 (A faire)-3] is colored in LightGray/Gray
[ULGCE-819 (A faire)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-819]]
[ULGCE-819 (En Test)-2] -> [ULGCE-819 (A faire)-3]
[ULGCE-819 (IN PROGRESS)-4] starts 2024/03/22 and ends 2024/03/22
[ULGCE-819 (IN PROGRESS)-4] is colored in LightGray/Gray
[ULGCE-819 (IN PROGRESS)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-819]]
[ULGCE-819 (A faire)-3] -> [ULGCE-819 (IN PROGRESS)-4]
[ULGCE-819 (En Attente)-5] starts 2024/03/22 and ends 2024/03/22
[ULGCE-819 (En Attente)-5] is colored in LightGray/Gray
[ULGCE-819 (En Attente)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-819]]
[ULGCE-819 (IN PROGRESS)-4] -> [ULGCE-819 (En Attente)-5]
[ULGCE-819 (IN PROGRESS)-6] starts 2024/03/22 and ends 2024/03/26
[ULGCE-819 (IN PROGRESS)-6] is colored in LightGray/Gray
[ULGCE-819 (IN PROGRESS)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-819]]
[ULGCE-819 (En Attente)-5] -> [ULGCE-819 (IN PROGRESS)-6]
[ULGCE-819 (En Test)-7] starts 2024/03/26 and ends 2024/03/26
[ULGCE-819 (En Test)-7] is colored in LightGray/Gray
[ULGCE-819 (En Test)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-819]]
[ULGCE-819 (IN PROGRESS)-6] -> [ULGCE-819 (En Test)-7]
[ULGCE-819 (Terminée)-8] starts 2024/03/26 and ends 2024/03/26
[ULGCE-819 (Terminée)-8] is colored in LightGray/Gray
[ULGCE-819 (Terminée)-8] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-819]]
[ULGCE-819 (En Test)-7] -> [ULGCE-819 (Terminée)-8]
[ULGCE-801 - création] -> [ULGCE-818 #905393_905393_EVOLUTION_DEVIS - création]
[ULGCE-818 #905393_905393_EVOLUTION_DEVIS - création] happens at 2023/12/27
[ULGCE-818 #905393_905393_EVOLUTION_DEVIS - création] -> [ULGCE-818 (IN PROGRESS)-1]
[ULGCE-818 (IN PROGRESS)-1] starts 2023/12/27 and ends 2023/12/28
[ULGCE-818 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-818 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-818]]
[ULGCE-818 (En Test)-2] starts 2023/12/28 and ends 2023/12/28
[ULGCE-818 (En Test)-2] is colored in LightGray/Gray
[ULGCE-818 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-818]]
[ULGCE-818 (IN PROGRESS)-1] -> [ULGCE-818 (En Test)-2]
[ULGCE-818 (Terminée)-3] starts 2023/12/28 and ends 2023/12/28
[ULGCE-818 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-818 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-818]]
[ULGCE-818 (En Test)-2] -> [ULGCE-818 (Terminée)-3]
-- ULGCE-945 - Incident Technique --
[ULGCE-945 - création] happens on 2024/01/11
[ULGCE-945 - création] -> [ULGCE-2118 Incident Technique - création]
[ULGCE-2118 Incident Technique - création] happens at 2024/09/02
[ULGCE-2118 Incident Technique - création] -> [ULGCE-2118 (IN PROGRESS)-1]
[ULGCE-2118 (IN PROGRESS)-1] starts 2024/09/02 and ends 2024/09/02
[ULGCE-2118 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-2118 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2118]]
[ULGCE-2118 (En Test)-2] starts 2024/09/02 and ends 2024/09/02
[ULGCE-2118 (En Test)-2] is colored in LightGray/Gray
[ULGCE-2118 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2118]]
[ULGCE-2118 (IN PROGRESS)-1] -> [ULGCE-2118 (En Test)-2]
[ULGCE-2118 (Terminée)-3] starts 2024/09/02 and ends 2024/09/02
[ULGCE-2118 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-2118 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2118]]
[ULGCE-2118 (En Test)-2] -> [ULGCE-2118 (Terminée)-3]
[ULGCE-945 - création] -> [ULGCE-2093 Incident Technique - création]
[ULGCE-2093 Incident Technique - création] happens at 2024/08/20
[ULGCE-2093 Incident Technique - création] -> [ULGCE-2093 (En Test)-1]
[ULGCE-2093 (En Test)-1] starts 2024/08/20 and ends 2024/08/21
[ULGCE-2093 (En Test)-1] is colored in LightGray/Gray
[ULGCE-2093 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2093]]
[ULGCE-2093 (Terminée)-2] starts 2024/08/21 and ends 2024/08/27
[ULGCE-2093 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-2093 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-2093]]
[ULGCE-2093 (En Test)-1] -> [ULGCE-2093 (Terminée)-2]
[ULGCE-945 - création] -> [ULGCE-1191 Incident Technique - création]
[ULGCE-1191 Incident Technique - création] happens at 2024/02/13
[ULGCE-1191 Incident Technique - création] -> [ULGCE-1191 (En Test)-1]
[ULGCE-1191 (En Test)-1] starts 2024/02/13 and ends 2024/02/15
[ULGCE-1191 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1191 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1191]]
[ULGCE-1191 (A faire)-2] starts 2024/02/15 and ends 2024/02/15
[ULGCE-1191 (A faire)-2] is colored in LightGray/Gray
[ULGCE-1191 (A faire)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1191]]
[ULGCE-1191 (En Test)-1] -> [ULGCE-1191 (A faire)-2]
[ULGCE-1191 (IN PROGRESS)-3] starts 2024/02/15 and ends 2024/02/15
[ULGCE-1191 (IN PROGRESS)-3] is colored in LightGray/Gray
[ULGCE-1191 (IN PROGRESS)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1191]]
[ULGCE-1191 (A faire)-2] -> [ULGCE-1191 (IN PROGRESS)-3]
[ULGCE-1191 (En Attente)-4] starts 2024/02/15 and ends 2024/02/15
[ULGCE-1191 (En Attente)-4] is colored in LightGray/Gray
[ULGCE-1191 (En Attente)-4] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1191]]
[ULGCE-1191 (IN PROGRESS)-3] -> [ULGCE-1191 (En Attente)-4]
[ULGCE-1191 (IN PROGRESS)-5] starts 2024/02/15 and ends 2024/02/22
[ULGCE-1191 (IN PROGRESS)-5] is colored in LightGray/Gray
[ULGCE-1191 (IN PROGRESS)-5] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1191]]
[ULGCE-1191 (En Attente)-4] -> [ULGCE-1191 (IN PROGRESS)-5]
[ULGCE-1191 (En Test)-6] starts 2024/02/22 and ends 2024/02/22
[ULGCE-1191 (En Test)-6] is colored in LightGray/Gray
[ULGCE-1191 (En Test)-6] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1191]]
[ULGCE-1191 (IN PROGRESS)-5] -> [ULGCE-1191 (En Test)-6]
[ULGCE-1191 (Terminée)-7] starts 2024/02/22 and ends 2024/02/22
[ULGCE-1191 (Terminée)-7] is colored in LightGray/Gray
[ULGCE-1191 (Terminée)-7] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1191]]
[ULGCE-1191 (En Test)-6] -> [ULGCE-1191 (Terminée)-7]
[ULGCE-945 - création] -> [ULGCE-1175 Incident Technique - création]
[ULGCE-1175 Incident Technique - création] happens at 2024/02/09
[ULGCE-1175 Incident Technique - création] -> [ULGCE-1175 (En Test)-1]
[ULGCE-1175 (En Test)-1] starts 2024/02/09 and ends 2024/02/09
[ULGCE-1175 (En Test)-1] is colored in LightGray/Gray
[ULGCE-1175 (En Test)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1175]]
[ULGCE-1175 (Terminée)-2] starts 2024/02/09 and ends 2024/02/09
[ULGCE-1175 (Terminée)-2] is colored in LightGray/Gray
[ULGCE-1175 (Terminée)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-1175]]
[ULGCE-1175 (En Test)-1] -> [ULGCE-1175 (Terminée)-2]
[ULGCE-945 - création] -> [ULGCE-947 Incident Technique - création]
[ULGCE-947 Incident Technique - création] happens at 2024/01/11
[ULGCE-947 Incident Technique - création] -> [ULGCE-947 (IN PROGRESS)-1]
[ULGCE-947 (IN PROGRESS)-1] starts 2024/01/11 and ends 2024/01/11
[ULGCE-947 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-947 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-947]]
[ULGCE-947 (En Test)-2] starts 2024/01/11 and ends 2024/01/12
[ULGCE-947 (En Test)-2] is colored in LightGray/Gray
[ULGCE-947 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-947]]
[ULGCE-947 (IN PROGRESS)-1] -> [ULGCE-947 (En Test)-2]
[ULGCE-947 (Terminée)-3] starts 2024/01/12 and ends 2024/01/12
[ULGCE-947 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-947 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-947]]
[ULGCE-947 (En Test)-2] -> [ULGCE-947 (Terminée)-3]
[ULGCE-945 - création] -> [ULGCE-946 Incident Technique - création]
[ULGCE-946 Incident Technique - création] happens at 2024/01/11
[ULGCE-946 Incident Technique - création] -> [ULGCE-946 (IN PROGRESS)-1]
[ULGCE-946 (IN PROGRESS)-1] starts 2024/01/11 and ends 2024/01/11
[ULGCE-946 (IN PROGRESS)-1] is colored in LightGray/Gray
[ULGCE-946 (IN PROGRESS)-1] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-946]]
[ULGCE-946 (En Test)-2] starts 2024/01/11 and ends 2024/01/11
[ULGCE-946 (En Test)-2] is colored in LightGray/Gray
[ULGCE-946 (En Test)-2] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-946]]
[ULGCE-946 (IN PROGRESS)-1] -> [ULGCE-946 (En Test)-2]
[ULGCE-946 (Terminée)-3] starts 2024/01/11 and ends 2024/01/11
[ULGCE-946 (Terminée)-3] is colored in LightGray/Gray
[ULGCE-946 (Terminée)-3] links to [[https://delivery.inetum.com/jirabrowse/ULGCE-946]]
[ULGCE-946 (En Test)-2] -> [ULGCE-946 (Terminée)-3]

    @endgantt
"""

plant_uml_png_generator(gantt, "gantt")
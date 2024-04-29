       IDENTIFICATION DIVISION.
       PROGRAM-ID. Appariement.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. JVM WITH DEBUGGING MODE.
       SOURCE-COMPUTER. JVM.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT FIC1                  ASSIGN TO
                "C:/Users/HP/Downloads/FIC1.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS L-Fst-In-1.

           SELECT FIC2                  ASSIGN TO
               "C:/Users/HP/Downloads/FIC2.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS L-Fst-In-2.

           SELECT 1PAS2                 ASSIGN TO
           "C:/Users/HP/Downloads/Fic1non2.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS L-Fst-Out-1.

           SELECT 2PAS1                 ASSIGN TO
           "C:/Users/HP/Downloads/Fic2non1.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS L-Fst-Out-2.

           SELECT SOR                   ASSIGN TO
           "C:/Users/HP/Downloads/Fic1et2.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS L-Fst-Out.

           SELECT ERR                   ASSIGN TO
           "C:/Users/HP/Downloads/Fic-Err.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS L-Fst-Err
           .

       DATA DIVISION.
       FILE SECTION.

       FD FIC1.
       01 ENTREE1                            PIC X(11).

       FD FIC2.
       01 ENTREE2                            PIC X(19).

       FD 1PAS2.
       01 SORTIE-1PAS2                       PIC X(11).

       FD 2PAS1.
       01 SORTIE-2PAS1                       PIC X(19).

       FD SOR.
       01 SORTIE                             PIC X(19).

       FD ERR.
       01 ERREUR.
           05 ERREURS                            OCCURS 2.
               10 MESS-ERR                       PIC X(16).
               10 TYP                            PIC X.
               10 MESS-ENT                       PIC X(31).
               10 NUM-ENT                        PIC X(10).
               10 DEUX-PTS                       PIC X(3).
               10 ENR                            PIC X(19).

       WORKING-STORAGE SECTION.

      * Variables File status

       01 L-Fst-In-1                             PIC 99.
       01 L-Fst-In-2                             PIC 99.
       01 L-Fst-Out-1                            PIC 99.
       01 L-Fst-Out-2                            PIC 99.
       01 L-Fst-Out                              PIC 99.
       01 L-Fst-Err                              PIC 99.

      * Structures fichiers en entrée

       01 W-Fic1.
           05 W-Fic1-RefCtr                      PIC X(9).
           05 FILLER                             PIC X.
           05 W-Fic1-Sit-Ctr                     PIC X.
               88 W-Fic1-Sit-Ctr-Crs             VALUE 'C'.
               88 W-Fic1-Sit-Ctr-Rsl             VALUE 'R'.
               88 W-Fic1-Sit-Ctr-Spd             VALUE 'S'.

       01 W-Fic2.
           05 W-Fic2-RefCtr                      PIC X(9).
           05 FILLER                             PIC X.
           05 W-Fic2-CodIdt                      PIC 9(7).
           05 FILLER                             PIC X.
           05 W-Fic2-Sit-Idt                     PIC X.
               88 W-Fic2-Sit-Idt-Crs             VALUE 'C'.
               88 W-Fic2-Sit-Idt-Rsl             VALUE 'R'.
               88 W-Fic2-Sit-Idt-Spd             VALUE 'S'.

      * Booléens test fin de lecture des fichiers

       01 Lec-Fic-1-Fin                 PIC 9.
           88 Lec-Fic-1-Fin-Oui         VALUE 1.
           88 Lec-Fic-1-Fin-Non         VALUE 0.

       01 Lec-Fic-2-Fin                 PIC 9.
           88 Lec-Fic-2-Fin-Oui         VALUE 1.
           88 Lec-Fic-2-Fin-Non         VALUE 0.

      * Booléen qui détermine s'il y eu appariement des deux derniers
      * enregistrements lus

       01 Appar                         PIC 9.
           88 Appar-Oui                 VALUE 1.
           88 Appar-Non                 VALUE 0.

      * Variable type d'erreur avec les types d'erreurs possibles

       01 Type-Err                     PIC 9.
           88 RefCtr-Vide              VALUE 1.
           88 Ctr-Non-CRS              VALUE 2.
           88 Idt-Non-CRS              VALUE 3.

      * Booléens qui détecte les erreurs non passantes dans le fichier 1
      * et dans le fichier 2 et un qui dit si on est dans un cas passant

       01 Cas-Err-1                     PIC 9.
           88 Cas-Non-Pass-1            VALUE 1.
           88 Cas-Passant-1             VALUE 0.

       01 Cas-Err-2                     PIC 9.
           88 Cas-Non-Pass-2            VALUE 1.
           88 Cas-Passant-2             VALUE 0.

       01 Cas-Err                       PIC 9.
           88 Cas-Non-Pass              VALUE 1.
           88 Cas-Passant               VALUE 0.

      * Booléen qui dit dans quel fichier on a fait la dernière lecture
      * (SET dans les paragraphes de lecture)

       01 Dern-Lec                      PIC 9.
           88 Dern-Lec-1                VALUE 1.
           88 Dern-Lec-2                VALUE 2.

      * Compteurs

       01 CPT-LEC-1                     PIC 9(10).
       01 CPT-LEC-2                     PIC 9(10).
       01 CPT-ECR-1PAS2                 PIC 9(10).
       01 CPT-ECR-2PAS1                 PIC 9(10).
       01 CPT-ECR                       PIC 9(10).
       01 CPT-ECR-ERR                   PIC 9(10).

       PROCEDURE DIVISION.

      ****************
       MAIN-PROCEDURE.
      ****************

           PERFORM INITIALISATION   THRU FIN-INITIALISATION

           PERFORM UNTIL Lec-Fic-1-Fin-Oui AND Lec-Fic-2-Fin-Oui
               PERFORM CONTROLE    THRU FIN-CONTROLE
               PERFORM TRAITEMENT  THRU FIN-TRAITEMENT
           END-PERFORM

           PERFORM FIN              THRU FIN-FIN

           GOBACK
           .

      *-----------------------------------------------------------------
      *****************
       INITIALISATION.
      *****************
           DISPLAY '***************************************************'
           DISPLAY '***          Appariement contrats               ***'
           DISPLAY '***************************************************'

      * Initialisation des dfférentes variables et des différents
      * booléens

           INITIALIZE ENTREE1
                      ENTREE2
                      SORTIE-1PAS2
                      SORTIE-2PAS1
                      SORTIE
                      ERREUR
                      L-Fst-In-1
                      L-Fst-In-2
                      L-Fst-Out-1
                      L-Fst-Out-2
                      L-Fst-Out
                      L-Fst-Err
                      W-Fic1
                      W-Fic2
                      Type-Err
                      CPT-LEC-1
                      CPT-LEC-2
                      CPT-ECR-1PAS2
                      CPT-ECR-2PAS1
                      CPT-ECR
                      CPT-ECR-ERR

           SET Lec-Fic-1-Fin-Non TO TRUE
           SET Lec-Fic-2-Fin-Non TO TRUE
           SET Appar-Non         TO TRUE
           SET Cas-Passant-1     TO TRUE
           SET Cas-Passant-2     TO TRUE
           SET Cas-Passant       TO TRUE
           SET Dern-Lec-1        TO TRUE

      * Ouverture des fichiers

           OPEN INPUT   FIC1
           OPEN INPUT   FIC2
           OPEN OUTPUT  1PAS2
           OPEN OUTPUT  2PAS1
           OPEN OUTPUT  SOR
           OPEN OUTPUT  ERR

      * Première lecture des deux fichiers

           PERFORM LECTURE-FICHIER-1 THRU FIN-LECTURE-FICHIER-1
           MOVE ENTREE1 TO W-Fic1
           PERFORM CONTROLE          THRU FIN-CONTROLE

           PERFORM LECTURE-FICHIER-2 THRU FIN-LECTURE-FICHIER-2
           MOVE ENTREE2 TO W-Fic2

           .

      ********************
       FIN-INITIALISATION. EXIT.
      ********************
      *-----------------------------------------------------------------
      **********
       CONTROLE.
      **********

      * On contrôle le dernier enregistrement lu (soit dans le fichier 1
      * soit dans le fichier 2) : les contrôles ne sont pas les mêmes
      * selon les fichiers

           IF Dern-Lec-1
               PERFORM CONTROLE-1 THRU FIN-CONTROLE-1
           END-IF

           IF Dern-Lec-2
               PERFORM CONTROLE-2 THRU FIN-CONTROLE-2
           END-IF

      * On passe en cas passant si les deux enregistrements lus dans les
      * fichiers ne contiennent pas d'erreur non passante

           IF Cas-Passant-1 AND Cas-Passant-2
               SET Cas-Passant TO TRUE
           ELSE
               SET Cas-Non-Pass TO TRUE
           END-IF
           .

      *************
       FIN-CONTROLE. EXIT.
      *************
      *-----------------------------------------------------------------
      ************
       CONTROLE-1.
      ************

       INITIALIZE Type-Err
       SET Cas-Passant-1 TO TRUE

      * Test erreur de type 2

           IF NOT (W-Fic1-Sit-Ctr-Crs OR W-Fic1-Sit-Ctr-Rsl
                                               OR W-Fic1-Sit-Ctr-Spd)
               SET Ctr-Non-CRS TO TRUE
               PERFORM ECR-FICHIER-ERR-1 THRU FIN-ECR-FICHIER-ERR-1
           END-IF

      * Test erreur de type 1

           IF W-Fic1-RefCtr = SPACE
               SET RefCtr-Vide TO TRUE
               SET Cas-Non-Pass-1 TO TRUE
               PERFORM ECR-FICHIER-ERR-1 THRU FIN-ECR-FICHIER-ERR-1
           END-IF
           .

      ***************
       FIN-CONTROLE-1. EXIT.
      ***************
      *-----------------------------------------------------------------
      ************
       CONTROLE-2.
      ************

       INITIALIZE Type-Err
       SET Cas-Passant-2 TO TRUE

      * Test erreur de type 3

           IF NOT (W-Fic2-Sit-Idt-Crs OR W-Fic2-Sit-Idt-Rsl
                                               OR W-Fic2-Sit-Idt-Spd)
               SET Idt-Non-CRS TO TRUE
               PERFORM ECR-FICHIER-ERR-2 THRU FIN-ECR-FICHIER-ERR-2
           END-IF

      * Test erreur de type 1

           IF W-Fic2-RefCtr = SPACE
               SET RefCtr-Vide TO TRUE
               SET Cas-Non-Pass-2 TO TRUE
               PERFORM ECR-FICHIER-ERR-2 THRU FIN-ECR-FICHIER-ERR-2
           END-IF
           .

      ***************
       FIN-CONTROLE-2. EXIT.
      ***************
      *-----------------------------------------------------------------
      *****************
       ECR-FICHIER-ERR-1.
      *****************

           INITIALIZE ERREUR

           MOVE 'Erreur de type ' TO MESS-ERR(1)
           MOVE Type-Err TO TYP(1)
           MOVE ' dans le fichier 1, a la ligne ' TO MESS-ENT(1)
           MOVE CPT-LEC-1 TO NUM-ENT(1)
           MOVE ' : ' TO DEUX-PTS(1)
           MOVE W-Fic1 TO ENR(1)
           WRITE ERREUR
           IF L-Fst-Err NOT = ZERO
               DISPLAY 'Erreur ecriture fichier erreur =' L-Fst-Err
           END-IF
           ADD 1 TO CPT-ECR-ERR

            .
      *********************
       FIN-ECR-FICHIER-ERR-1. EXIT.
      *********************
      *-----------------------------------------------------------------
      *****************
       ECR-FICHIER-ERR-2.
      *****************

           INITIALIZE ERREUR

           MOVE 'Erreur de type ' TO MESS-ERR(2)
           MOVE Type-Err TO TYP(2)
           MOVE ' dans le fichier 2, a la ligne ' TO MESS-ENT(2)
           MOVE CPT-LEC-2 TO NUM-ENT(2)
           MOVE ' : ' TO DEUX-PTS(2)
           MOVE W-Fic2 TO ENR(2)
           WRITE ERREUR
           IF L-Fst-Err NOT = ZERO
               DISPLAY 'Erreur ecriture fichier erreur =' L-Fst-Err
           END-IF
           ADD 1 TO CPT-ECR-ERR

           .
      *********************
       FIN-ECR-FICHIER-ERR-2. EXIT.
      *********************
      *-----------------------------------------------------------------
      ************
       TRAITEMENT.
      ************

      * On compare les enregistrements pour savoir comment traiter càd
      * dans quel fichier écrire

           EVALUATE TRUE

               WHEN W-Fic1-RefCtr = W-Fic2-RefCtr
                   PERFORM APPARIEMENT         THRU FIN-APPARIEMENT

               WHEN W-Fic1-RefCtr < W-Fic2-RefCtr
                   PERFORM TRAITEMENT-1PAS2    THRU FIN-TRAITEMENT-1PAS2

               WHEN W-Fic1-RefCtr > W-Fic2-RefCtr
                   PERFORM TRAITEMENT-2PAS1    THRU FIN-TRAITEMENT-2PAS1

           END-EVALUATE

      * Selon le traitement effectué, on termine en lisant le fichier 1
      * ou le fichier 2, donc, par défaut, on stock les deux dans les
      * variables WS

           MOVE ENTREE1 TO W-Fic1
           MOVE ENTREE2 TO W-Fic2

           .

      ****************
       FIN-TRAITEMENT. EXIT.
      ****************
      *-----------------------------------------------------------------
      ************
       APPARIEMENT.
      ************

      * On écrit dans le fichier d'appariement (à condition d'être dans
      * un cas passant)

           IF Cas-Passant
               MOVE W-Fic2 TO SORTIE
               WRITE SORTIE
               IF L-Fst-Out NOT = ZERO
                   DISPLAY 'Erreur ecriture fichier appar =' L-Fst-Out
               END-IF
               ADD 1 TO CPT-ECR
           END-IF

           SET Appar-Oui TO TRUE

      * On lit ensuite le fichier 2, et s'il est fini, on lit le fichier
      * 1

           PERFORM LECTURE-FICHIER-2  THRU FIN-LECTURE-FICHIER-2

           IF Lec-Fic-2-Fin-Oui
               PERFORM LECTURE-FICHIER-1 THRU FIN-LECTURE-FICHIER-1
           END-IF
           .

      ****************
       FIN-APPARIEMENT. EXIT.
      ****************
      *-----------------------------------------------------------------
      ******************
       TRAITEMENT-1PAS2.
      ******************

      * On écrit dans le fichier 1PAS2 (à condition d'être dans
      * un cas passant) si on n'a pas eu d'appariement juste avant et
      * si on n'est pas à la fin du fichier 1 (permet de ne pas écrire
      * le dernier enregistrement du fichier 1 en double)

           IF Appar-Non AND Lec-Fic-1-Fin-Non AND Cas-Passant
               MOVE W-Fic1 TO SORTIE-1PAS2
               WRITE SORTIE-1PAS2
               IF L-Fst-Out-1 NOT = ZERO
                   DISPLAY 'Erreur ecriture fichier 1pas2 =' L-Fst-Out-1
               END-IF
               ADD 1 TO CPT-ECR-1PAS2
           END-IF

           SET Appar-Non TO TRUE

      * On lit ensuite le fichier 1, et s'il est fini, on va alors
      * devoir écrire dans 2PAS1 car l'enregistrement 2 est plus
      * grand que le dernier enregistrement 1 et donc que tout le
      * fichier 1

           PERFORM LECTURE-FICHIER-1  THRU FIN-LECTURE-FICHIER-1

           IF Lec-Fic-1-Fin-Oui AND Lec-Fic-2-Fin-Non
               PERFORM TRAITEMENT-2PAS1  THRU FIN-TRAITEMENT-2PAS1
           END-IF
           .

      **********************
       FIN-TRAITEMENT-1PAS2. EXIT.
      **********************
      *-----------------------------------------------------------------
      ******************
       TRAITEMENT-2PAS1.
      ******************

      * On écrit dans le fichier 2PAS1 (à condition d'être dans
      * un cas passant) si on n'a pas eu d'appariement juste avant et
      * si on n'est pas à la fin du fichier 2 (permet de ne pas écrire
      * le dernier enregistrement du fichier 2 en double)

           IF Appar-Non AND Lec-Fic-2-Fin-Non AND Cas-Passant
               MOVE W-Fic2 TO SORTIE-2PAS1
               WRITE SORTIE-2PAS1
               IF L-Fst-Out-2 NOT = ZERO
                   DISPLAY 'Erreur ecriture fichier 2pas1 =' L-Fst-Out-2
               END-IF
               ADD 1 TO CPT-ECR-2PAS1
           END-IF

           SET Appar-Non TO TRUE

      * On lit ensuite le fichier 2, et s'il est fini, on va alors
      * devoir écrire dans 1PAS2 car l'enregistrement 1 est plus
      * grand que le dernier enregistrement 2 et donc que tout le
      * fichier 2

           PERFORM LECTURE-FICHIER-2  THRU FIN-LECTURE-FICHIER-2

           IF Lec-Fic-2-Fin-Oui AND Lec-Fic-1-Fin-Non
               PERFORM TRAITEMENT-1PAS2 THRU FIN-TRAITEMENT-1PAS2
           END-IF
           .

      **********************
       FIN-TRAITEMENT-2PAS1. EXIT.
      **********************
      *-----------------------------------------------------------------
      *******************
       LECTURE-FICHIER-1.
      *******************

           READ FIC1
           AT END
               SET Lec-Fic-1-Fin-Oui  TO TRUE
           NOT AT END
               IF L-Fst-In-1 NOT = ZERO
                   DISPLAY 'Erreur lecture fichier 1 =' L-Fst-In-1
               END-IF
               ADD 1 TO CPT-LEC-1
               SET Dern-Lec-1 TO TRUE
           END-READ
           .
      ***********************
       FIN-LECTURE-FICHIER-1. EXIT.
      ***********************
      *-----------------------------------------------------------------
      *******************
       LECTURE-FICHIER-2.
      *******************

           READ FIC2
           AT END
               SET Lec-Fic-2-Fin-Oui  TO TRUE
           NOT AT END
               IF L-Fst-In-2 NOT = ZERO
                   DISPLAY 'Erreur lecture fichier 2 =' L-Fst-In-2
               END-IF
               ADD 1 TO CPT-LEC-2
               SET Dern-Lec-2 TO TRUE
           END-READ
           .
      ***********************
       FIN-LECTURE-FICHIER-2. EXIT.
      ***********************
      *-----------------------------------------------------------------
      ******
       FIN.
      ******

      * Fermeture de tous les fichiers

           CLOSE FIC1
           CLOSE FIC2
           CLOSE 1PAS2
           CLOSE 2PAS1
           CLOSE SOR
           CLOSE ERR

      * Display des compteurs et du nombre d'erreurs s'il y en a

           DISPLAY "Nombre d'enregistrements lus en 1 : "      CPT-LEC-1
           DISPLAY "Nombre d'enregistrements lus en 2 : "      CPT-LEC-2
           DISPLAY "Nombre d'enregistrements ecrits dans 1PAS2 : "
                                                           CPT-ECR-1PAS2
           DISPLAY "Nombre d'enregistrements ecrits dans 2PAS1 : "
                                                           CPT-ECR-2PAS1
           DISPLAY "Nombre d'enregistrements ecrits dans OUT : " CPT-ECR

           IF CPT-ECR-ERR NOT = ZERO
               DISPLAY '***********************************************'
               DISPLAY '***********************************************'
               DISPLAY '**************IL Y A DES ERREURS***************'
               DISPLAY '***********************************************'
               DISPLAY '***********************************************'
               DISPLAY "Nombre d'erreurs : "           CPT-ECR-ERR
               DISPLAY 'Terminologie des erreurs :'
               DISPLAY 'Type 1 : Reference du contrat vide'
               DISPLAY "Type 2 : Sit de contrat mal definie (C, R ou S)"
               DISPLAY "Type 3 : Sit de tete mal definie (C, R ou S)"
           END-IF

           DISPLAY 'Fin de traitement'
           .

      **********
       FIN-FIN.  EXIT.
      **********
      *-----------------------------------------------------------------

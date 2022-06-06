      ******************************************************************
      * Author: WILFREDO CHIPANA GONZALES
      * Date:
      * Purpose:
      * Tectonics: cobc
      *
      * EJERCICIO 3:
      * CALCULAR EL SUELDO ANUAL Y EL BONO DE FIN DE ANIO
      * SEGUN EL SIGUIENTE CRITERIO:
      * 1. SIN EXPERIENCIA: NO COBRA BONO   0K(100)
      * 2. 1 ANIO:          COBRA 150% DEL SUELDO MENSUAL 150K(100*1.5)
      * 3. 2 - 3 ANIOS:     COBRA 200% DEL SUELDO MENSUAL 200K(100*2)
      * 4. 4 - 6 ANIOS:     COBRA 250% DEL SUELDO MENSUAL 400K(160*2.5)
      * 6+ ANIOS:           COBRA 300% DEL SUELDO MENSUAL 600K(200*3)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO3.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WSC-CONSTANTES.
               05 WSC-SENIORITY.
                   10 WSC-SENIOR        PIC X(06) VALUE "SENIOR".
                   10 WSC-SEMISR        PIC X(06) VALUE "SEMISR".
                   10 WSC-JUNIOR        PIC X(06) VALUE "JUNIOR".
               05 WSC-SUELDOS.
                   10 WSC-SUELDO-SENIOR PIC 9(06) VALUE 200000.
                   10 WSC-SUELDO-SEMISR PIC 9(06) VALUE 160000.
                   10 WSC-SUELDO-JUNIOR PIC 9(06) VALUE 100000.

           01 WSV-VARIABLES.
               05 WS-POSTULANTES.
                   10 WSV-POSTU1.
                       15 WSV-NOMBRE1       PIC X(5) VALUE "PEDRO".
                       15 WSV-EXPERIENCIA1  PIC 9(02) VALUE 01.
                       15 WSV-SUELDO1       PIC 9(06).
                       15 WSV-SUELDO-ANUAL1 PIC 9(07).
                       15 WSV-BONO1         PIC 9(06).
                   10 WSV-POSTU2.
                       15 WSV-NOMBRE2       PIC X(5) VALUE "SOFIA".
                       15 WSV-EXPERIENCIA2  PIC 9(02) VALUE 07.
                       15 WSV-SUELDO2       PIC 9(06).
                       15 WSV-SUELDO-ANUAL2 PIC 9(07).
                       15 WSV-BONO2         PIC 9(06).
                   10 WSV-POSTU3.
                       15 WSV-NOMBRE3       PIC X(05) VALUE "LALA".
                       15 WSV-EXPERIENCIA3  PIC 9(02) VALUE 04.
                       15 WSV-SUELDO3       PIC 9(06).
                       15 WSV-SUELDO-ANUAL3 PIC 9(07).
                       15 WSV-BONO3         PIC 9(06).

               05 WSV-POSTULANTE-AUX.
                   10 WSV-NOMBRE-AUX        PIC X(5).
                   10 WSV-EXPERIENCIA-AUX   PIC 9(02).
                       88 WSS-EXP-JUNIOR       VALUE 0 1 2.
                       88 WSS-EXP-SEMISR       VALUE 3 4 5.
                   10 WSV-SUELDO-AUX        PIC 9(06).
                   10 WSV-SUELDO-ANUAL-AUX  PIC 9(07).
                   10 WSV-BONO-AUX          PIC 9(06).

       PROCEDURE DIVISION.

       00-CONTROL.
           PERFORM 10-INICIO.
           PERFORM 20-PROCESO.
       STOP RUN.
       00-CONTROL-END.
       EXIT.

       10-INICIO.
           DISPLAY "HOLA CARACOLA 2022".
           INITIALIZE WSV-POSTULANTE-AUX.
       10-INICIO-END.
       EXIT.

       20-PROCESO.
           MOVE WSV-POSTU1 TO WSV-POSTULANTE-AUX
           PERFORM 25-EVALUAR-SENIORITY
           PERFORM 30-CALCULAR-SUELDO.

           MOVE WSV-POSTU2 TO WSV-POSTULANTE-AUX
           PERFORM 25-EVALUAR-SENIORITY
           PERFORM 30-CALCULAR-SUELDO.

           MOVE WSV-POSTU3 TO WSV-POSTULANTE-AUX
           PERFORM 25-EVALUAR-SENIORITY
           PERFORM 30-CALCULAR-SUELDO.

       20-PROCESO-END.
       EXIT.

       25-EVALUAR-SENIORITY.
           DISPLAY "-------------------------------------------------".

           EVALUATE TRUE
           WHEN WSS-EXP-JUNIOR
               MOVE WSC-SUELDO-JUNIOR TO WSV-SUELDO-AUX
               DISPLAY "EL NIVEL DEL POSTULANTE " WSV-NOMBRE-AUX " ES: "
               WSC-JUNIOR
               DISPLAY "EL SUELDO ES: " WSV-SUELDO-AUX
           WHEN WSS-EXP-SEMISR
               MOVE WSC-SUELDO-SEMISR TO WSV-SUELDO-AUX
               DISPLAY "EL NIVEL DEL POSTULANTE " WSV-NOMBRE-AUX " ES: "
               WSC-SEMISR
               DISPLAY "EL SUELDO ES: " WSV-SUELDO-AUX
           WHEN OTHER
               MOVE WSC-SUELDO-SENIOR TO WSV-SUELDO-AUX
               DISPLAY "EL NIVEL DEL POSTULANTE " WSV-NOMBRE-AUX " ES: "
               WSC-SENIOR
               DISPLAY "EL SUELDO ES: " WSV-SUELDO-AUX
           END-EVALUATE.

       25-EVALUAR-SENIORITY-END.
       EXIT.

       30-CALCULAR-SUELDO.
           MULTIPLY 12 BY WSV-SUELDO-AUX GIVING WSV-SUELDO-ANUAL-AUX

           EVALUATE WSV-EXPERIENCIA-AUX
               WHEN 0
                   MOVE 0 TO WSV-BONO-AUX
               WHEN 1
                   COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 150 / 100
               WHEN 2
               WHEN 3
                   COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 200 / 100
               WHEN 4
               WHEN 5
               WHEN 6
                   COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 250 / 100
               WHEN OTHER
                   COMPUTE WSV-BONO-AUX = WSV-SUELDO-AUX * 300 / 100

           END-EVALUATE.

           DISPLAY "SU SUELDO ANUAL ES: " WSV-SUELDO-ANUAL-AUX
           DISPLAY "SU BONO ES: " WSV-BONO-AUX

           INITIALIZE WSV-POSTULANTE-AUX.

       30-CALCULAR-SUELDO-END.
       EXIT.

       END PROGRAM EJERCICIO3.

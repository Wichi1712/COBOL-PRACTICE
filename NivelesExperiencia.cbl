      ******************************************************************
      * Author: WILFREDO CHIPANA GONZALES
      * Date:
      * Purpose:
      * Tectonics: cobc
      *
      * EJERCICIO 2:
      * ESTE PROGRAMA DEBE SER CAPAZ DE DIFERENCIAR ENTRE PERSONAS CON
      * DIFERENTES NIVELES DE EXPERIENCIA COMO PROGRAMADOR, ASIGNARLES
      * UN SUELDO ACORDE Y DIVIDIRLOS EN:
      * 1. 0 A 2 ANIOS = JUNIOR // SUELDO = $110.000
      * 2. 3 A 5 ANIOS = SEMISENIOR // SUELDO = $160.000
      * 3. 6 A + ANIOS = SENIOR // SUELDO = $200.000
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO2.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WSC-CONSTANTES.
               05 WSC-SENIORITY.
                   10 WSC-SENIOR        PIC X(06) VALUE "SENIOR".
                   10 WSC-SEMISR        PIC X(06) VALUE "SEMISR".
                   10 WSC-JUNIOR        PIC X(06) VALUE "JUNIOR".

           01 WSV-VARIABLES.
               05 WS-POSTULANTES.
                   10 WSV-POSTU1.
                       15 WSV-NOMBRE1       PIC X(5) VALUE "PEDRO".
                       15 WSV-EXPERIENCIA1  PIC 9(02) VALUE 01.
                   10 WSV-POSTU2.
                       15 WSV-NOMBRE2       PIC X(5) VALUE "SOFIA".
                       15 WSV-EXPERIENCIA2  PIC 9(02) VALUE 07.
                   10 WSV-POSTU3.
                       15 WSV-NOMBRE3       PIC X(05) VALUE "LALA".
                       15 WSV-EXPERIENCIA3  PIC 9(02) VALUE 04.

               05 WSV-POSTULANTE-AUX.
                   10 WSV-NOMBRE-AUX        PIC X(5).
                   10 WSV-EXPERIENCIA-AUX   PIC 9(02).
                       88 WSS-EXP-JUNIOR    VALUE 0 1 2.
                       88 WSS-EXP-SEMISR    VALUE 3 4 5.

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
           MOVE WSV-POSTU1 TO WSV-POSTULANTE-AUX.
           PERFORM 25-EVALUAR.

           MOVE WSV-POSTU2 TO WSV-POSTULANTE-AUX.
           PERFORM 25-EVALUAR.

           MOVE WSV-POSTU3 TO WSV-POSTULANTE-AUX.
           PERFORM 25-EVALUAR.

       20-PROCESO-END.
       EXIT.

       25-EVALUAR.
           EVALUATE TRUE
           WHEN WSS-EXP-JUNIOR
               DISPLAY "EL NIVEL DEL POSTULANTE " WSV-NOMBRE-AUX " ES: "
               WSC-JUNIOR
               DISPLAY "EL SUELDO QUE LE CORRESPONDE ES: $110.000"
           WHEN WSS-EXP-SEMISR
               DISPLAY "EL NIVEL DEL POSTULANTE " WSV-NOMBRE-AUX " ES: "
               WSC-SEMISR
               DISPLAY "EL SUELDO QUE LE CORRESPONDE ES: $160.000"
           WHEN OTHER
               DISPLAY "EL NIVEL DEL POSTULANTE " WSV-NOMBRE-AUX " ES: "
               WSC-SENIOR
               DISPLAY "EL SUELDO QUE LE CORRESPONDE ES: $200.000"
           END-EVALUATE.

           DISPLAY "-------------------------------------------------".
           INITIALIZE WSV-POSTULANTE-AUX.

       25-EVALUAR-END.
       EXIT.

       END PROGRAM EJERCICIO2.

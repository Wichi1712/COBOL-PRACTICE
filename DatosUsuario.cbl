      ******************************************************************
      * Author:WILFREDO CHIPAHA
      * Date:30-12-2021
      * Purpose:PRACTICA
      * Tectonics: cobc
      *
      * EJERCICIO 1:
      *SE PIDE UN PROGRAMA QUE VERIFIQUE LOS DATOS DE LOS
      *PARTICIPANTES PARA CURSAR Y RECIBIRSE DE LA CARRERA DE COBOLERO.
      ***********REQUISITOS PARA COBOLEAR****************
      *1. SER MAYOR DE 18 ANIOS.
      *2. HABER TERMINADO SUS ESTUDIOS SECUNDARIOS.
      *3. SER ESTUDIANTE O GRADUADO DE LA CARRERA ING. DE SISTEMAS,
      ****LIC. EN SISTEMAS O AFINES.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO-1.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *------------------------------------------------------------*
      *                VARIABLES Y CONSTANTES                      *
      *------------------------------------------------------------*

           01 WSC-CONSTANTES.
               05 WSC-EDAD-MINIMA      PIC 99 VALUE 18.
               05 WSC-ESTADO-SECUND    PIC X VALUE "T".
               05 WSC-ESTADO-CARRERA-OK.
                   10 WSC-TERMINADO        PIC X VALUE "T".
                   10 WSC-ENCURSO          PIC X VALUE "C".

           01 WSV-VARIABLES.
               05 WS-POSTULANTES.
                   10 WSV-POSTU1.
                       15 WSV-NOMBRE1          PIC X(5) VALUE  "PEDRO".
                       15 WSV-EDAD1            PIC 99 VALUE  15.
                       15 WSV-SECUNDARIO1      PIC X VALUE  "N".
                       15 WSV-CARRERA1         PIC X VALUE  "N".

                   10 WSV-POSTU2.
                       15 WSV-NOMBRE1          PIC X(5) VALUE  "SOFIA".
                       15 WSV-EDAD1            PIC 99 VALUE  25.
                       15 WSV-SECUNDARIO1      PIC X VALUE  "T".
                       15 WSV-CARRERA1         PIC X VALUE  "T".

                   10 WSV-POSTU3.
                       15 WSV-NOMBRE1          PIC X(5) VALUE  "LALA".
                       15 WSV-EDAD1            PIC 99 VALUE  19.
                       15 WSV-SECUNDARIO1      PIC X VALUE  "T".
                       15 WSV-CARRERA1         PIC X VALUE  "N".
               05 WSV-POSTULANTE-AUX.
                   10 WSV-NOMBRE-AUX       PIC X(5).
                   10 WSV-EDAD-AUX         PIC 99.
                   10 WSV-SECUNDARIO-AUX   PIC X.
                   10 WSV-CARRERA-AUX      PIC X.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       00-CONTROL.
           DISPLAY "------------------------".
           DISPLAY "EJERCICIO 1".
           DISPLAY "------------------------".

           MOVE WSV-POSTU1 TO WSV-POSTULANTE-AUX,
           PERFORM 20-EVALUAR.

           MOVE WSV-POSTU2 TO WSV-POSTULANTE-AUX,
           PERFORM 20-EVALUAR.

           MOVE WSV-POSTU3 TO WSV-POSTULANTE-AUX,
           PERFORM 20-EVALUAR.



       STOP RUN.
       00-CONTROL-END.
       EXIT.

       20-EVALUAR.
           IF (WSV-EDAD-AUX >= WSC-EDAD-MINIMA AND
               WSV-SECUNDARIO-AUX EQUAL WSC-ESTADO-SECUND AND
               (WSV-CARRERA-AUX EQUAL WSC-TERMINADO OR
                WSV-CARRERA-AUX EQUAL WSC-ENCURSO))
               DISPLAY WSV-NOMBRE-AUX " CUMPLE CON EL REQUISITO"
           ELSE
               DISPLAY WSV-NOMBRE-AUX " NO CUMPLE CON EL REQUISITO"
           END-IF.

           DISPLAY "-------------------------------------".

           INITIALIZE WSV-POSTULANTE-AUX.

       20-EVALUAR-END.
       EXIT.

       END PROGRAM EJERCICIO-1.

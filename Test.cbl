      ******************************************************************
      * Author:Wilfredo Chipana Gonzales
      * Date:26/09/2020
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MI-PRIMER-PROGRAMA.
      * AUTHOR. Chipana.
      * INSTALLATION. D-O-S.
      * DATE-WRITTEN. 28-09-2020.
      * DATE-COMPILED. 28-09-2020.

       ENVIRONMENT DIVISION.


       DATA DIVISION.
      * FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WORKAREA.
           05 NOMBRE                           PIC A(30).
           05 WKS-NUMERO-1                     PICTURE 9(06).
       77 WKS-NUMERO-2                         PIC 9(10).





       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hola Usuario".
            DISPLAY "Ingresa tu nombre".
            ACCEPT NOMBRE.
            DISPLAY "Bienvenido usuario: " NOMBRE.

            DISPLAY "Ingrese un numero".
            ACCEPT WKS-NUMERO-1.
            DISPLAY "EL primer numero es: " WKS-NUMERO-1.

            DISPLAY "Dame otro numero".
            ACCEPT WKS-NUMERO-2.
            DISPLAY "El segundo numero es: " WKS-NUMERO-2.

            ADD WKS-NUMERO-1 TO WKS-NUMERO-2.
            DISPLAY "La suma de ambos numeros es: " WKS-NUMERO-2.

      *      STOP "PAUSA".--Obsoleto
            STOP RUN.
      * END PROGRAM MI-PRIMER-PROGRAMA.
       STOP RUN.

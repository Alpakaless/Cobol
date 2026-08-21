       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG002
        AUTHOR. PROF GALLAO.
        DATA-WRITTEN 21/08/2026
      * DESCRICAO.....: PRIMEIRA ATIVIDADE.
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS "R$ " WITH PICTURE SYMBOL "$"
           .
       INPUT-OUTPUT SECTION.
      *---------------------*
       FILE-CONTROL.

       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
      *==> LOCAL PARA A FD (DESCRICAO DOS ARQUIVOS)

       WORKING-STORAGE SECTION.
      *-----------------------*

      *-----> AREA AUXILIAR
       77  WS-FIM                 PIC X(01) VALUE "N".
       77  WS-CTEXIB              PIC 9(02).
       77  WS-N1                  PIC 99V99.
       77  WS-MED                 PIC 99V9999.
      *-----> DADOS DE SAIDA VIA SYSOUT
       01  WS-REG-SYSOUT.
           05 WS-ALUNO            PIC A(30).
           05 WS-RA               PIC X(13).
           05 WS-NOTA1            PIC Z9,99.
           05 WS-NOTA2            PIC Z9,99.
           05 WS-MEDIA            PIC Z9,99.

       LINKAGE SECTION.
      *----------------*
       01  LK-PARAMETROS.
           05 LK-NR-DPTO             PIC 9(04).
           05 LK-NOME-DPTO           PIC X(15).
           05 LK-COD-RETORNO         PIC 99.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL                                        *
      *--------------------------------------------------------------*

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 050-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS                                    *
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY "PROGRAMA 2"
           MOVE  ZEROS  TO  WS-CTEXIB
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN                        *
      *--------------------------------------------------------------*
       030-PROCESSAR.

           DISPLAY 'NOME GUSTAVO DE ALMEIDA - NOME GABRIEL PEREIRA'
           MOVE 'ZE GABARITO JR'   TO   WS-ALUNO
           MOVE '1680481117111'    TO   WS-RA
           MOVE '7,25'             TO   WS-N1
           MOVE '9,00'             TO   WS-N2
           COMPUTE WS-MED =(WS-N1 + WS-N2)/2
           MOVE WS-N1              TO   WS-NOTA1.
           MOVE WS-N2              TO   WS-NOTA2.
           MOVE WS-MED             TO   WS-MEDIA
           DISPLAY WS-REG-SYSOUT
           ADD   1               TO   WS-CTEXIB
           MOVE 'S'              TO   WS-FIM
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS                                      *
      *--------------------------------------------------------------*
       050-TERMINAR.

           DISPLAY '** FIM DA EXECUCAO **'
           DISPLAY "TERMINO NORMAL DO PROGRAMA CGPRG002"

           .
      *---------------> FIM DO PROGRAMA CGPRG002 <-------------------*

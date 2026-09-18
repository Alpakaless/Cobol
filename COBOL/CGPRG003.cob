       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG003.
       AUTHOR. GABRIEL PEREIRA - GUSTAVO ALMEIDA.
       DATE-WRITTEN. 28/08/2026.
       INSTALLATION. FATEC SAO CAETANO.
       DATE-COMPILED. <DATA ATUAL>.
       SECURITY. NIVEL BASICO.
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
      *==> LOCAL PARA O SELECT DOS ARQUVOS

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
       77  WS-N2                  PIC 99V99.
       77  WS-MED                 PIC 99V9999.
       77  WS-TOTAL-AULAS         PIC 9(03) VALUE 80.
       77  WS-FALTAS              PIC 9(03).
       77  WS-PORC-FALTAS         PIC 9(03)V99.
       77  WS-HIFENS              PIC X(80) VALUE ALL "-".
      *-----> DADOS DE SAIDA VIA SYSOUT
       01  WS-REG-SYSOUT.
           05 WS-ALUNO            PIC A(30).
           05 WS-RA               PIC X(13).
           05 FILLER              PIC X(02) VALUE SPACES.
           05 WS-NOTA1            PIC Z9,99.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 WS-NOTA2            PIC Z9,99.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 WS-MEDIA            PIC Z9,99.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 WS-PORC-FALTAS-M    PIC ZZ9,99.

       01  WS-CAB.
           05 FILLER              PIC X(04) VALUE "NOME".
           05 FILLER              PIC X(26) VALUE SPACES.
           05 FILLER              PIC X(02) VALUE "RA".
           05 FILLER              PIC X(13) VALUE SPACES.
           05 FILLER              PIC X(05) VALUE "NOTA1".
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(05) VALUE "NOTA2".
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(05) VALUE "MEDIA".
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(07) VALUE "%FALTAS".

       LINKAGE SECTION.
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
           DISPLAY "** PROGRAMA 3 **"

           MOVE  ZEROS  TO  WS-CTEXIB
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN                        *
      *--------------------------------------------------------------*
       030-PROCESSAR.
           DISPLAY 'GABRIEL PEREIRA - GUSTAVO ALMEIDA'
           DISPLAY WS-HIFENS
           DISPLAY WS-CAB
           MOVE 'ZE GABARITO JR'   TO   WS-ALUNO
           MOVE '1680481117111'    TO   WS-RA
           MOVE  7,25              TO   WS-N1
           MOVE  9,00              TO   WS-N2
           MOVE  20                TO   WS-FALTAS
           COMPUTE WS-MED = ((WS-N1 * 2)+(WS-N2 * 3)) / 5
           COMPUTE WS-PORC-FALTAS = (WS-FALTAS / WS-TOTAL-AULAS) * 100
           MOVE  WS-N1             TO   WS-NOTA1
           MOVE  WS-N2             TO   WS-NOTA2
           MOVE  WS-MED            TO   WS-MEDIA
           MOVE  WS-PORC-FALTAS    TO   WS-PORC-FALTAS-M
           DISPLAY WS-REG-SYSOUT
           IF WS-MED >= 6 AND WS-PORC-FALTAS <= 25
              DISPLAY "STATUS: APROVADO"
           ELSE
              DISPLAY "STATUS: REPROVADO"
           END-IF
           DISPLAY WS-HIFENS
           ADD   1               TO   WS-CTEXIB
           MOVE 'S'              TO   WS-FIM
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS                                      *
      *--------------------------------------------------------------*
       050-TERMINAR.

           DISPLAY '** FIM DA EXECUCAO **'

           DISPLAY "REGISTROS EXIBIDOS = " WS-CTEXIB
           DISPLAY "TERMINO NORMAL DO PROGRAMA CGPRG003"
           .
      *---------------> FIM DO PROGRAMA CGPRG003 <-------------------*

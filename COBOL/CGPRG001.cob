       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG001.
      * AUTOR .........: GUSTAVO DE ALMEIDA SILVA E GABRIEL PEREIRA.
      * DATA DE CRIACAO: 14/08/2026.
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

      *-----> DADOS DE SAIDA VIA SYSOUT
       01  WS-REG-SYSOUT.
           05 WS-NOME             PIC A(25).
           05 WS-DPTO             PIC X(10).
           05 WS-FUNCAO           PIC X(15).
           05 WS-SALARIO          PIC 9(04)V99.
           05 WS-DATAADM          PIC 9(08).

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

           DISPLAY "INICIO DA EXECUCAO"

           MOVE  ZEROS  TO  WS-CTEXIB
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN                        *
      *--------------------------------------------------------------*
       030-PROCESSAR.

           DISPLAY 'NOME GUSTAVO DE ALMEIDA - NOME GABRIEL PEREIRA'

           MOVE 'CELSO D. GALLAO'  TO   WS-NOME
           MOVE 'ADS'              TO   WS-DPTO
           MOVE 'PROFESSOR'        TO   WS-FUNCAO
           MOVE  8500,00           TO   WS-SALARIO
           MOVE 21122000           TO   WS-DATAADM
           DISPLAY WS-REG-SYSOUT
           ADD   1               TO   WS-CTEXIB
           MOVE 'S'              TO   WS-FIM
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS                                      *
      *--------------------------------------------------------------*
       050-TERMINAR.

           DISPLAY '** FIM DA EXECUCAO **'
           DISPLAY "REGISTROS EXIBIDOS = " WS-CTEXIB
           DISPLAY "TERMINO NORMAL DO PROGRAMA CGPRG001"
           .
      *---------------> FIM DO PROGRAMA CGPRG001 <-------------------*

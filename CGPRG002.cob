       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID. CGPRG004.
       AUTHOR.   PROF GALLAO.
       DATE-WRITTEN. 20/08/2026.
       DATA-COMPILED. 14/09/2026.
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
       77  AS-CEP                 PIC 9(08).
       77  AS-FRENTE              PIC 9(03)V99.
       77  AS-COMP                PIC 9(03)V99.
       77  AS-VAL-M2              PIC 9(05)V99.
       77  AS-VAL-VENDA           PIC 9(08)V99.
       77  AS-COMISSAO            PIC 9(07)V00.
       77  AS-DATA                PIC 9(08).
       77  AS-HORA                PIC 99.99.999.
       77  AS-HIFENS              PIC 9(80).
      *-----> DADOS DE SAIDA VIA SYSOUT
      01  WS-REG-SYSOUT.
           05 WS-ALUNO            PIC A(30).
           05 WS-RA               PIC X(13).
           05 WS-CPF              PIC X(11).
           05 WS-NOTA1            PIC Z9,99.
           05 WS-NOTA2            PIC Z9,99.
           05 WS-MEDIA            PIC Z9,99.
           05 WS-CEP              PIC 9(08).
           05 FILLER              PIC X(01) VALUES SPACE.
           05 WS-FRENTE           PIC ZZ9,99.
           05 FILLER              PIC X(02) VALUES SPACE "M".
           05 WS-COMP             PIC ZZ9,99.
           05 FILLER              PIC X(02) VALUES SPACE "M".
           05 WS-VAL-M2           PIC $$.$$9,99.
           05 FILLER              PIC X(01) VALUES SPACE.
           05 WS-VAL-VENDA        PIC $$.$$$.$$9,99.
           05 FILLER              PIC X(01) VALUES SPACE.
           05 WS-COMISSAO         PIC $.$$$.$$9,99.
           05 FILLER              PIC X(01) VALUES SPACE.
           05 WS-MENSAGEM         PIC X(12).
      01 WS-CAB
         05 WS-CEP.
         05 WS-FRENTE.
         05 WS-COMP.
         05 WS-PRECO-DO-M2.
         05 WS-PRECO-DE-VENDA.
         05 WS-COMISSAO.
         05 WS-MENSAGEM.
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
           DISPLAY "ATIVIDADE 4"
           DISPLAY "MATHEUS MARTINS VIDO E BRUNO DE ANDRADE E OLIVEIRA"
           DISPLAY WS-PRECO-DE-VENDA
           ACCEPT  WS-DATA-ATUAL FROM DATE
           DISPLAY "DATA ATUAL:  " WS-DATA-ATUAL
           DISPLAY 11.24.0000
           AS-HIFENS                 PIC X(80) VALUES "-"
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           MOVE 09000300             TO AS-CEP
           MOVE 12,32                TO AS-FRENTE
           MOVE 52,00                TO AS-COMP
           MOVE 2315,00              TO AS-VAL-M2
           PERFORM 050-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS                                    *
      *--------------------------------------------------------------*
       010-INICIAR.
           DISPLAY "** AVALIACAO 2 **"

           MOVE  ZEROS  TO  WS-CTEXIB
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN                        *
      *--------------------------------------------------------------*
       030-PROCESSAR.
           DISPLAY '** LEONARDO MAMEDE - MILENA MENEZES **'
           MOVE 'ZE GABARITO JR'   TO   WS-ALUNO.
           MOVE '1680481117111'    TO   WS-RA.
           MOVE '12345678901'      TO   WS-CPF.
           MOVE  7,25              TO   WS-N1.
           MOVE  9,00              TO   WS-N2.
           COMPUTE WS-MED = (WS-N1 + WS-N2)/2.
           MOVE WS-N1              TO   WS-NOTA1.
           MOVE WS-N2              TO   WS-NOTA2.
           MOVE WS-MED             TO   WS-MEDIA.
           DISPLAY WS-REG-SYSOUT
           ADD   1               TO   WS-CTEXIB
           MOVE 'S'              TO   WS-FIM
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS                                      *
      *--------------------------------------------------------------*
       050-TERMINAR.

           DISPLAY '** FIM DA EXECUCAO**'

           DISPLAY "REGISTROS EXIBIDOS = " WS-CTEXIB
           DISPLAY "TERMINO NORMAL DO PROGRAMA CGPRG002"
           .
      *---------------> FIM DO PROGRAMA CGPRG002-------------*

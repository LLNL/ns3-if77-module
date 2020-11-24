      PROGRAM IF77ATA
C
C     Main program to test ATA as a function
C
C     Provide configuration cards on stdin
      
      implicit real*8(A-H,O-Z)
      
 1000 FORMAT(10X,'RANGE ',F6.1,20X,'LOSS 50% (dbW) ',F6.1)
 1001 FORMAT(A)
      
      LOGICAL VERB
      
C     Length of each input line/card
      DIMENSION LEN(4)
      DATA LEN/54,78,79,2/

C     Target range and computed loss, in dBW
      REAL*8 RANG
      REAL*8 LDBW
      REAL*8 ATA
      
C     Lines of input, 3 cards (plus 0 input terminator line)
      CHARACTER*80 CARDS(4)
      
      COMMON/FREAD/CARDS
      
      VERB=.true.
      
      IN=5
      IOT=6
      
C     Card index
      N=1
      
      IF(VERB)WRITE(IOT,1001)'Input lines: '
      DO
         READ(IN,1001)CARDS(N)
         IF(VERB)WRITE(IOT,1001)CARDS(N)(1:LEN(N))
         N=N+1
         IF(N.GT.4) EXIT
      END DO

      IF(VERB)WRITE(IOT,1001)'Generated cards: '
      IF(VERB)WRITE(IOT,1001)CARDS
      
      RANG=100.0
      LDBW=ATA(RANG)
      
      WRITE(IOT,1000)RANG,LDBW
      
      STOP
      END
      

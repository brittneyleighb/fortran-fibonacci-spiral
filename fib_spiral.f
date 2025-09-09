C  fib_spiral_f77.f — ASCII Fibonacci square spiral (Fortran 77 style)
      PROGRAM FIBSPIRAL77
C
C  PARAMETERS (tweak these if your spiral is too large)
      INTEGER MAXN, MAXW, MAXH, MAXPTS
      PARAMETER (MAXN=25, MAXW=120, MAXH=60)
      PARAMETER (MAXPTS=10000)
C
      INTEGER N
      INTEGER FIB(MAXN)
      INTEGER XPTS(MAXPTS), YPTS(MAXPTS)
      INTEGER DX(4), DY(4)
      INTEGER I, K, T, DIR, STEP
      INTEGER X, Y, MINX, MAXX, MINY, MAXY
      INTEGER WIDTH, HEIGHT, R, C, CX, CY
      CHARACTER*1 CANVAS(MAXH,MAXW)
      CHARACTER*120 LINE
C
C  DIRECTIONS: right, up, left, down
      DATA DX / 1,  0, -1,  0 /
      DATA DY / 0, -1,  0,  1 /
C
C  HOW MANY FIBONACCI STEPS?
      N = 10
      IF (N .LT. 1 .OR. N .GT. MAXN) THEN
         PRINT *, 'N out of range'
         STOP
      ENDIF
C
C  BUILD FIBONACCI: 1, 1, 2, 3, ...
      FIB(1) = 1
      IF (N .GE. 2) FIB(2) = 1
      DO 50 I = 3, N
         FIB(I) = FIB(I-1) + FIB(I-2)
   50 CONTINUE
C
C  -------- FIRST PASS: TRACE PATH, RECORD BOUNDS --------
      STEP = 1
      XPTS(STEP) = 0
      YPTS(STEP) = 0
      X = 0
      Y = 0
      MINX = 0
      MAXX = 0
      MINY = 0
      MAXY = 0
      DIR = 1
C
      DO 100 K = 1, N
         DO 110 T = 1, FIB(K)
            X = X + DX(DIR)
            Y = Y + DY(DIR)
            STEP = STEP + 1
            IF (STEP .GT. MAXPTS) GOTO 900
            XPTS(STEP) = X
            YPTS(STEP) = Y
            IF (X .LT. MINX) MINX = X
            IF (X .GT. MAXX) MAXX = X
            IF (Y .lt. MINY) MINY = Y
            IF (Y .gt. MAXY) MAXY = Y
  110    CONTINUE
C        rotate direction: 1->2->3->4->1
         DIR = MOD(DIR,4) + 1
  100 CONTINUE
C
      WIDTH  = MAXX - MINX + 1
      HEIGHT = MAXY - MINY + 1
      IF (WIDTH  .GT. MAXW .OR. HEIGHT .GT. MAXH) GOTO 910
C
C  -------- CLEAR CANVAS --------
      DO 200 R = 1, MAXH
         DO 210 C = 1, MAXW
            CANVAS(R,C) = ' '
  210    CONTINUE
  200 CONTINUE
C
C  -------- PLOT PATH --------
      DO 300 I = 1, STEP
         CY = YPTS(I) - MINY + 1
         CX = XPTS(I) - MINX + 1
         IF (CY .GE. 1 .AND. CY .LE. HEIGHT .AND.
     &       CX .GE. 1 .AND. CX .LE. WIDTH) THEN
            CANVAS(CY,CX) = '*'
         ENDIF
  300 CONTINUE
C  Mark start and end
      CY = YPTS(1)    - MINY + 1
      CX = XPTS(1)    - MINX + 1
      IF (CY .GE. 1 .AND. CY .LE. HEIGHT .AND.
     &    CX .GE. 1 .AND. CX .LE. WIDTH) CANVAS(CY,CX) = 'S'
      CY = YPTS(STEP) - MINY + 1
      CX = XPTS(STEP) - MINX + 1
      IF (CY .GE. 1 .AND. CY .LE. HEIGHT .AND.
     &    CX .GE. 1 .AND. CX .LE. WIDTH) CANVAS(CY,CX) = 'E'
C
C  -------- PRINT CANVAS --------
      DO 400 R = 1, HEIGHT
         LINE = ' '
         DO 410 C = 1, WIDTH
            LINE(C:C) = CANVAS(R,C)
  410    CONTINUE
         WRITE(*,'(A)') LINE(1:WIDTH)
  400 CONTINUE
      STOP
C
C  -------- ERRORS --------
  900 CONTINUE
      PRINT *, 'MAXPTS too small for this N'
      STOP
  910 CONTINUE
      PRINT *, 'Canvas too small: WIDTH=', WIDTH, ' HEIGHT=', HEIGHT
      STOP
      END

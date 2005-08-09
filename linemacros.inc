line macro   x1,y1,x2,y2
	     ld BC,x1*256+y1
	     ld DE,x2*256+y2
		  B_CALL ILine
    .endm


putline macro   x1,y1,x2,y2
	     ld BC,x1*256+(63-y1)
	     ld DE,x2*256+(63-y2)
		  B_CALL ILine
    .endm

putOP1s macro x,y,offset
		  ld HL,y*256+x
		  ld (penCol),HL
		  ld hl, OP1+offset
		  B_CALL VPutS
    .endm

putOP1sn macro x,y,offset,len
		  ld HL,y*256+x
		  ld (penCol),HL
		  ld hl, OP1+offset
		  ld b, len
		  B_CALL VPutSN
    .endm

setpenpos macro x,y
		  ld HL,y*256+x
		  ld (penCol),HL
	.endm

setpenposA macro x,y
		  ld A,x
		  ld (penCol),A
		  ld A,y
		  ld (penRow),A
	.endm


startbuffer macro									; Set 'Draw to Buffer', clear Buffer
		  set textWrite, (IY + sGrFlags) 
		  set bufferOnly,   (IY + plotFlags)
		  B_CALL GrBufClr   ; clear buffer
	.endm

continuebuffer macro								; Set 'Draw to Buffer', don't clear Buffer
		  set textWrite, (IY + sGrFlags) 
		  set bufferOnly,   (IY + plotFlags)
	.endm

endbuffer macro										; Reset 'Draw to Buffer', copy Buffer to Screen
		  res textWrite, (IY + sGrFlags) 
		  res bufferOnly,   (IY + plotFlags)
		  B_CALL GrBufCpy  
    .endm


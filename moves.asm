 DEFINE P3MOVE, SPACE=ROM  
 SEGMENT P3MOVE

 extern _Green
 extern _DrawPkName
 extern _Red
 extern _Select
 extern _Move
 public Move
 extern _LoadPic
 extern _LPicBank1
 extern _DrawMoveName
 public DrawMoveName
 extern _DrawMoveName_AlignRight
 public DrawMoveName_AlignRight
 extern _MoveList
 extern _LoadLevelMoves
 extern _StatCalculator


 include "header.inc"
 include "linemacros.inc"

;line macro   x1,y1,x2,y2
;	     ld BC,x1*256+y1
;	     ld DE,x2*256+y2
;		  B_CALL ILine
;    .endm

;putOP1sn macro x,y,offset,len
;		  ld a, x
;		  ld (penCol), a
;		  ld a, y
;		  ld (penRow), a
;		  ld hl, OP1+offset
;		  ld b, len
;		  B_CALL VPutSN
;    .endm

setpen macro x,y
		  ld a, x
		  ld (penCol), a
		  ld a, y
		  ld (penRow), a
    .endm

Move:
  res showpkmn, (IY+dexstate)

  startbuffer

  call PrepareScreen


  AppOnErr failed_to_get_x
  B_CALL RclX			; get our X
  B_CALL Int		; Round X
  B_CALL ClrOP1S	; Make X positive
  B_CALL StoX		; Store rounded index to X
  ld HL,353
  B_CALL SetXXXXOP2
  B_CALL CpOP1OP2
  jp z,pboostx
  jp nc,failed_to_get_x
 pboostx:
  B_CALL ConvOP1	; put X to DE
  AppOffErr
  push de			; save our X
  B_CALL RclX		; get our X again
  xor a; A <- 0
  ld (penCol), a
  ld (penRow), a	; set pen location to (0,0)
  call DrawMoveName

  ld a,58
  ld (penCol), a
  xor a; A <- 0
  ld (penRow), a	; set pen location to (58,0)
  B_CALL RclX		; get our X again
  B_CALL Plus1		; Get human-friendly #
  ld a,3		; a=3 (# if digits)
  B_CALL DispOP1A	; Display the number

  pop DE
  push DE
  ld HL,d_types
  add HL,DE
  ld A,(HL)
  sla A
  sla A
  ld E,A
  ld D,0
  ld HL,s_types
  add HL,DE
  B_CALL Mov9ToOP1
  putOP1sn 74,0,0,1
  putOP1sn 82,0,1,3

  ld a,16
  ld (penCol), a
  ld a,6
  ld (penRow), a	; set pen location to (82,0)
  pop DE
  push DE
  ld HL,d_power
  call drawdb

  ld a,16
  ld (penCol), a
  ld a,12
  ld (penRow), a	; set pen location to (82,0)
  pop DE
  push DE
  ld HL,d_accuracy
  call drawdb

  ld a,20
  ld (penCol), a
  ld a,18
  ld (penRow), a	; set pen location to (82,0)
  pop DE
  push DE
  ld HL,d_priority
  add HL,DE
  ld A,(HL)
  push AF
  or A
  jr z,priority_zero
  jp p,priority_positive
  ld A,"-"
  B_CALL VPutMap
  pop AF
  neg A
  add "0"
  B_CALL VPutMap
  jr priority_done
 priority_positive:
  ld A,"+"
  B_CALL VPutMap
  pop AF
  add "0"
  B_CALL VPutMap
  jr priority_done
 priority_zero:
  pop AF
  ld a,24
  ld (penCol), a
  ld A,"0"
  B_CALL VPutMap
 priority_done:

  pop DE
  push DE
  ld HL,d_target
  add HL,DE
  ld A,(HL)
  sla A
  sla A
  ld E,A
  ld D,0
  ld HL,s_target
  add HL,DE
  B_CALL Mov9ToOP1
  putOP1sn 28,24,0,4

  ld a,36
  ld (penCol), a
  ld a,10
  ld (penRow), a	; set pen location
  pop DE
  push DE
  ld HL,d_pp
  add HL,DE
  ld A,(HL)
  push AF
  ld E,A
  sla A
  sla A
  add A,E
  B_CALL SetXXOP1
  ld A,(OP1M-1)
  and 1
 jp nz,two_digits
  ld a,40
  ld (penCol), a
  ld A,(OP1M)
  srl A
  srl A
  srl A
  srl A
  or "0"
  B_CALL VPutMap
  jp pp_plus
 two_digits:
  ld A,(OP1M)
  srl A
  srl A
  srl A
  srl A
  or "0"
  B_CALL VPutMap
  ld A,(OP1M)
  and A,0Fh
  or "0"
  B_CALL VPutMap
 pp_plus:
  ld A,36
  ld (penCol), a
  ld a,16
  ld (penRow), a	; set pen location
  ld A,"+"
  B_CALL VPutMap	; technically a static, but this saves a pen location set
  pop AF
  or "0"
  B_CALL VPutMap

 ; ld HL,s_flags
 ; ld DE,OP1	; spans to OP2 & OP3 also
 ; ld BC,28
 ; LDIR	; fill up

  pop DE
  push DE
  ld HL,d_flags
  add HL,DE
  ld A,(HL)
  ld B,A	; save flag byte in B
  and 00100000b
  jp z,no_contact
  ld A,18
  ld (penCol), A
  ld A,30
  ld (penRow), A	; set pen location
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
 no_contact:
  ld A,B
  and 00000010b
  jp z,no_powder
  ld A,18
  ld (penCol), A
  ld A,36
  ld (penRow), A	; set pen location
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
 no_powder:
  ld A,B
  and 00000001b
  jp z,no_rock
  ld A,18
  ld (penCol), A
  ld A,42
  ld (penRow), A	; set pen location
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
 no_rock:
  ld A,B
  and 00010000b
  jp z,no_c4
  ld A,38
  ld (penCol), A
  ld A,30
  ld (penRow), A	; set pen location
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
 no_c4:
  ld A,B
  and 00001000b
  jp z,no_c5
  ld A,38
  ld (penCol), A
  ld A,36
  ld (penRow), A	; set pen location
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
 no_c5:
  ld A,B
  and 00000100b
  jp z,no_c6
  ld A,38
  ld (penCol), A
  ld A,42
  ld (penRow), A	; set pen location
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
 no_c6:
  ld A,B
  and 01000000b
  jp z,no_sound
  ld A,18
  ld (penCol), A
  ld A,48
  ld (penRow), A	; set pen location
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
 no_sound:

  ; effect
  ld A,82
  ld (penCol), A
  ld A,8
  ld (penRow), A	; set pen location\
  pop DE
  push DE
  ld HL,d_effect
  add HL,DE
  ld L,(HL)
  ld H,0	; isolate effect value
  push HL
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  ld a,3	; 3 Digits
  B_CALL DispOP1A

  pop HL	; get contesteffect to HL
  ld D,H
  ld E,L
  sla L
  call c,incinch
  sla L
  call c,inch
  add HL,DE
  ex HL,DE
  ld HL,i_effects
  add HL,DE
  xor A
  ld D,A
  ld E,(HL)
  push DE
  inc HL
  ld E,(HL)
  push DE
  inc HL
  ld E,(HL)
  push DE
  inc HL
  ld E,(HL)
  push DE
  inc HL
  ld E,(HL)
  push DE

  ;the "loop"
  ld A,38
  ld (penRow), A
 ; ld A,46
 ; ld (penCol), A
  pop HL
  call draweffect

  ld A,32
  ld (penRow), A
 ; ld A,46
 ; ld (penCol), A
  pop HL
  call draweffect

  ld A,26
  ld (penRow), A
 ; ld A,46
 ; ld (penCol), A
  pop HL
  call draweffect

  ld A,20
  ld (penRow), A
 ; ld A,46
 ; ld (penCol), A
  pop HL
  call draweffect

  ld A,14
  ld (penRow), A
 ; ld A,46
 ; ld (penCol), A
  pop HL
  call draweffect

  jp skipdraweffect
 draweffect:
  sla L
  call c,inch
  ld DE,si_effects
  add HL,DE
  ld E,(HL)
  inc HL
  ld D,(HL)
  ex HL,DE
  ld DE,OP1+1
  ld BC,14
  LDIR

  ld HL,OP1+1
  xor A
  ld BC,255
  CPIR
  ld A,C
  cpl A
  dec A
  ld HL,OP1
  ld (HL),A

  B_CALL SStringLength

  ld A,2*46+48
  sub A,B
  srl A
  ld (penCol), A

  ld HL,OP1+1
  B_CALL VPutS
  ret
 skipdraweffect:


;  ld HL, s_lazy1
;  ld DE,OP1
;  ld BC,12*5
;  LDIR
;  putOP1sn 46,14,00,12
;  putOP1sn 46,20,12,12
;  putOP1sn 46,26,24,12
;  putOP1sn 46,32,36,12
;  putOP1sn 46,38,48,12


  ; effchance
  ld A,60
  ld (penCol), A
  ld A,8
  ld (penRow), A	; set pen location\
  pop DE
  push DE
  ld HL,d_effchance
  add HL,DE
  ld A,(HL)
  or A
  jp z,draw_dashes
  call drawdb_hl_added
  jp dont_draw_dashes
 draw_dashes:
  ld A,"-"
  ld (OP1+0),A
  ld (OP1+1),A
  ld (OP1+2),A
  ld HL,OP1
  ld B,3
  B_CALL VPutSN
 dont_draw_dashes:

  ; appeal
  ld A,66
  ld (penCol), A
  ld A,50
  ld (penRow), A	; set pen location\
  pop DE
  push DE
  ld HL,d_appeal
  add HL,DE
  ld A,(HL)
  or "0"
  B_CALL VPutMap

  ; jam
  ld A,90
  ld (penCol), A
  ld A,50
  ld (penRow), A	; set pen location\
  pop DE
  push DE
  ld HL,d_jam
  add HL,DE
  ld A,(HL)
  or "0"
  B_CALL VPutMap

  ; contesttype
  ld A,0
  ld (penCol), A
  ld A,56
  ld (penRow), A	; set pen location\
  pop DE
  push DE
  ld HL,d_contesttype
  add HL,DE
  ld A,(HL)
  sla A	; x2
  ld E,A
  sla A	; x4
  add A,E ; x2+x4=x6
  ld E,A
  ld D,0
  ld HL,s_contesttype
  add HL,DE
  ld DE,OP1
  ld BC,6
  LDIR
  ld HL,OP1
  ld B,6
  B_CALL VPutSN

  ; contesteffect
  ld A,30
  ld (penCol), A
  ld A,56
  ld (penRow), A	; set pen location\
  pop DE
  push DE
  ld HL,d_contesteffect
  add HL,DE
  ld A,(HL)
  sla A	; x2
  sla A	; x4
  sla A	; x8
  sla A	; x16
  ld E,A
  ld D,0
  ld HL,s_contesteffect
  add HL,DE
  ld DE,OP1
  ld BC,16
  LDIR
  ld HL,OP1
  ld B,16
  B_CALL VPutSN





  pop DE

  call DrawAmbients
  endbuffer
  jp select_keywait

DrawMoveName:
  ld a,12
  B_CALL SetXXOP2	; set OP2 to 12
  B_CALL FPMult	; multiply OP1 by 12 (OP2)
  B_CALL ConvOP1	; Get 10x from OP1 to DE
  ld hl, d_names	; Load address to HL
  ld a,e				; put e to a
  add l				; add l
  jr nc, startapp_skip_inc	; carry over
  inc d
 startapp_skip_inc:
  ld e,a				; put a to back to e
  ld a,d				; put d to a
  add h				; add h
  ld d,a				; and put it back
  ld h, d			; Move address from De to Hl
  ld l, e			; Move address from dE to hL
  ld de, OP1		; Put address of OP1 to DE
  ld bc, 12			; put 12 (length) to BC
  ldir				; load move name to OP1
  ld hl, OP1		; Put address of OP1 to HL
  ld b, 12			; put 12 (length) to BC
  B_CALL VPutSN	; Draw string... finally!
  ret

DrawMoveName_AlignRight:
  ld a,12
  B_CALL SetXXOP2	; set OP2 to 12
  B_CALL FPMult	; multiply OP1 by 12 (OP2)
  B_CALL ConvOP1	; Get 10x from OP1 to DE
  ld hl, d_names	; Load address to HL
  ld a,e				; put e to a
  add l				; add l
  jr nc, dmn_skip_inc	; carry over
  inc d
 dmn_skip_inc:
  ld e,a				; put a to back to e
  ld a,d				; put d to a
  add h				; add h
  ld d,a				; and put it back
  ld h, d			; Move address from De to Hl
  ld l, e			; Move address from dE to hL
  ld de, OP1		; Put address of OP1 to DE
  ld bc, 12			; put 12 (length) to BC
  ldir				; load move name to OP1
 ; align right
  ld A,(penCol)
  ld B,A
  dec HL
 drawmovenamealignloop:
  ld A,(HL)
  cp 6
  jp nz,drawmovenamealignloop_end
  inc B
  inc B
  inc B
  inc B
  dec HL
  jp drawmovenamealignloop
 drawmovenamealignloop_end:
  ld A,B
  ld (penCol),A
 ; done aligning
  ld hl, OP1		; Put address of OP1 to HL
  ld b, 12			; put 12 (length) to BC
  B_CALL VPutSN	; Draw string... finally!
  ret

drawdb:
  add HL,DE
 drawdb_hl_added:; gets called like a function
  push hl	; save stat address
  ld l,(hl)			; Put stat value to L
  ld h,0				; Set H to 0 to isolate L
  B_CALL SetXXXXOP2	; Put stat value to OP2
  B_CALL OP2ToOP1
  pop hl	; get stat address
  ld a,(hl)			; Put stat value to A
  sub 100
  jp nc,drawdb_display
  ld a,(penCol)
  add 4
  ld (penCol),a
  ld a,(hl)			; Put stat value to A again
  sub 10
  jp nc,drawdb_display
  ld a,(penCol)
  add 4
  ld (penCol),a
 drawdb_display:
  ld a,3	; 3 Digits
  B_CALL DispOP1A
  ret

PrepareScreen:

  ld h,1	; dark line
  line 45,56,94,56	; line under header
  line 30,56,33,56	; line stub to the left of PP
  line 34,55,34,41	; line left of PP
  line 35,40,43,40	; line below PP
  line 44,53,44,41	; line right of PP
  line  0, 8,43, 8	; left line above contest stuff
  line 44, 9,44,17	; vertical line above contest stuff
  line 45,18,93,18	; right line above contest stuff
  line 94, 8,94,17	; line right of contest header

  ; pp bitmap
  ld hl,bmp_ppheader				; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,bmp_ppheader_end-bmp_ppheader	; len
  ldir				; load image to RAM
  ld hl,appBackUpScreen			; HL points to copied image
  ld de,7*256+35					; DE <- coords
  B_CALL DisplayImage

 ret

DrawAmbients:

  ; contest bitmap
  ld hl,bmp_contest				; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,bmp_contest_end-bmp_contest	; len
  ldir				; load image to RAM
  ld hl,appBackUpScreen			; HL points to copied image
  ld de,47*256+47					; DE <- coords
  B_CALL DisplayImage

  ld hl,s_static1
  B_CALL Mov9ToOP1	; load %#/PWRACC.
  putOP1sn 52, 0,1,1	; #
  putOP1sn 72, 8,0,2	; %#
  putOP1sn 28,12,0,1	; %
  putOP1sn  0, 6,2,3	; PWR
  putOP1sn  0,12,5,3	; ACC
  putOP1sn 26,56,8,1	; .

  ld hl,s_static2
  B_CALL Mov9ToOP1	; load SPDTARGET
  putOP1sn  0,18,0,3	; SPD
  putOP1sn  0,24,3,6	; TARGET
  ld A,":"
  B_CALL VPutMap

  ld hl,s_static3
  B_CALL Mov9ToOP1	; load EFF:APPL
  putOP1sn 46, 8,0,4	; EFF:
  putOP1sn 46,50,4,4	; APPL

  ld hl,s_static4
  B_CALL Mov9ToOP1	; load /JAMCONT:
  putOP1sn 70,50,0,4	; /JAM
  putOP1sn  0,30,4,5	; CONT:

  ld hl,s_static5
  B_CALL Mov9ToOP1	; load PWDR:C4:
  putOP1sn  0,36,0,5	; PWDR:
  putOP1sn 28,30,5,3	; C4:

  ld hl,s_static6
  B_CALL Mov9ToOP1	; load ROCK:C5:
  putOP1sn  0,42,0,5	; ROCK:
  putOP1sn 28,36,5,3	; C5:

  ld hl,s_static7
  B_CALL Mov9ToOP1	; load SND:C6:
  putOP1sn  4,48,0,4	; ROCK:
  putOP1sn 28,42,4,3	; C5:

;  ld HL, s_lazy1
;  ld DE,OP1
;  ld BC,12*5
;  LDIR
;  putOP1sn 46,14,00,12
;  putOP1sn 46,20,12,12
;  putOP1sn 46,26,24,12
;  putOP1sn 46,32,36,12
;  putOP1sn 46,38,48,12

 ret

failed_to_get_x:
  B_JUMP Select

incinch:
  inc H
 inch:	; gets called like a function
  inc H
  ret

bmp_ppheader:
 db 3	; Height
 db 15	; Width
 db 01101101b,00111111b
 db 01101100b,01000000b
 db 01001001b,01000000b
 bmp_ppheader_end:

bmp_contest:
 db 3	; Height
 db 45; Width
 db 01001001b,10011101b,11001101b,11000011b,10110011b,10010000b
 db 10010101b,01001001b,10001000b,10000001b,00101011b,00101000b
 db 01001001b,01001001b,11011000b,10000011b,10101010b,00010000b
bmp_contest_end:

s_static1: db "%#","PWR","ACC",0Ch
s_static2: db "SPD","TARGET"
s_static3: db "EFF:","APPL"		; 1 byte left
s_static4: db "/JAM","CONT:"
s_static5: db "PWDR:","C4:"		; 1 byte left
s_static6: db "ROCK:","C5:"		; 1 byte left
s_static7: db "SND:","C6:"			; 2 bytes left
;s_lazy1: db "I'M\6TOO\6LAZY"
;s_lazy2: db "TO\6WRITE\6UP\6"
;s_lazy3: db "\6THE\6EFFECTS"
;s_lazy4: db "\6OF\6ALL\6THE\6"
;s_lazy5: db "\6\6\6MOVES...\6"
 
s_types:
 db "?   ","PNOR","SFIR","SWAT","SELE","SGRA","SICE","PFIG","PPOI"
 db "PGRO","PFLY","SPSY","PBUG","PROC","PGHO","SDRA","SDAR","PSTE"

s_target:
 db "NORM","LAST","????","RAND","BOTH","USER","THRE","FFLD","????"

s_contesttype:
 db "      BEAUTYCOOL  CUTE  SMART TOUGH "

s_contesteffect:
 db "HIGHLY\6APPEALINGAFFECT\6BY\6PREV\6\6JAM5\6COMBO\6STDBY6AP\6IF\6SAME\6TYPEGOOD\6W/\6APPLAUSE"
 db "JAM\6AWAY\6","1/2\6HTSUPS\6CONDITION\6\6\6SCRAMBLES\6ORDER\6AVOIDS\6ALL\6JAMS\6FREEZE\6APPLAUSE\6"
 db "JAM4\6IF\6SAME\6TYPBADLY\6JAM\6OTHERSDOUBLES\6JAMS\6:(\6JAM\6OTHERS!!!\6\6\6GET\6","6AP\6IF\6LAST\6"
 db "UNNERVE\6WAITINGSMISS\6NEXT\6TURN.\6AVOID\6","1\6JAM\6\6\6\6\6JAM\6PREVIOUS\6PKMJAM\6OTHERS\6\6\6\6\6\6"
 db "RESET\6CONDITIONSTURN#:\6","1,3,4,5APMOVE\6TO\6","1ST\6SPOTCAN\6BE\6REPEATED\6AS\6GOOD\6AS\6PREV\6"
 db "CANCEL\6COMBOS\6\6\6NO\6MORE\6APPEALS\6","6AP\6IF\6FIRST\6\6\6\6AS\6GOOD\6AsOTHERSMOVE\6TO\6","4TH\6SPOT"
 db "FOR\6ANY\6CONTEST\6RANDOM:1,2,4,8APCONDITIONx2+1\6AP"

d_names:
 db "POUND\6\6\6\6\6\6\6KARATE\6CHOP\6DOUBLESLAP\6\6COMET\6PUNCH\6MEGA\6PUNCH\6\6PAY\6DAY\6\6\6\6\6FIRE\6PUNCH\6\6ICE\6PUNCH\6\6\6THUNDERPUNCH"
 db "SCRATCH\6\6\6\6\6VICEGRIP\6\6\6\6GUILLOTINE\6\6RAZOR\6WIND\6\6SWORDS\6DANCECUT\6\6\6\6\6\6\6\6\6GUST\6\6\6\6\6\6\6\6WING\6ATTACK\6WHIRLWIND\6\6\6"
 db "FLY\6\6\6\6\6\6\6\6\6BIND\6\6\6\6\6\6\6\6SLAM\6\6\6\6\6\6\6\6VINE\6WHIP\6\6\6STOMP\6\6\6\6\6\6\6DOUBLE\6KICK\6MEGA\6KICK\6\6\6JUMP\6KICK\6\6\6ROLLING\6KICK"
 db "SAND-ATTACK\6HEADBUTT\6\6\6\6HORN\6ATTACK\6FURY\6ATTACK\6HORN\6DRILL\6\6TACKLE\6\6\6\6\6\6BODY\6SLAM\6\6\6WRAP\6\6\6\6\6\6\6\6TAKE\6DOWN\6\6\6"
 db "THRASH\6\6\6\6\6\6DOUBLE-EDGE\6TAIL\6WHIP\6\6\6POISON\6STINGTWINEEDLE\6\6\6PIN\6MISSILE\6LEER\6\6\6\6\6\6\6\6BITE\6\6\6\6\6\6\6\6GROWL\6\6\6\6\6\6\6"
 db "ROAR\6\6\6\6\6\6\6\6SING\6\6\6\6\6\6\6\6SUPERSONIC\6\6SONICBOOM\6\6\6DISABLE\6\6\6\6\6ACID\6\6\6\6\6\6\6\6EMBER\6\6\6\6\6\6\6FLAMETHROWERMIST\6\6\6\6\6\6\6\6"
 db "WATER\6GUN\6\6\6HYDRO\6PUMP\6\6SURF\6\6\6\6\6\6\6\6ICE\6BEAM\6\6\6\6BLIZZARD\6\6\6\6PSYBEAM\6\6\6\6\6BUBBLEBEAM\6\6AURORA\6BEAM\6HYPER\6BEAM\6\6"
 db "PECK\6\6\6\6\6\6\6\6DRILL\6PECK\6\6SUBMISSION\6\6LOW\6KICK\6\6\6\6COUNTER\6\6\6\6\6SEISMIC\6TOSSSTRENGTH\6\6\6\6ABSORB\6\6\6\6\6\6MEGA\6DRAIN\6\6"
 db "LEECH\6SEED\6\6GROWTH\6\6\6\6\6\6RAZOR\6LEAF\6\6SOLARBEAM\6\6\6POISONPOWDERSTUN\6SPORE\6\6SLEEP\6POWDERPETAL\6DANCE\6STRING\6SHOT\6"
 db "DRAGON\6RAGE\6FIRE\6SPIN\6\6\6THUNDERSHOCKTHUNDERBOLT\6THUNDER\6WAVETHUNDER\6\6\6\6\6ROCK\6THROW\6\6EARTHQUAKE\6\6FISSURE\6\6\6\6\6"
 db "DIG\6\6\6\6\6\6\6\6\6TOXIC\6\6\6\6\6\6\6CONFUSION\6\6\6PSYCHIC\6\6\6\6\6HYPNOSIS\6\6\6\6MEDITATE\6\6\6\6AGILITY\6\6\6\6\6QUICK\6ATTACKRAGE\6\6\6\6\6\6\6\6"
 db "TELEPORT\6\6\6\6NIGHT\6SHADE\6MIMIC\6\6\6\6\6\6\6SCREECH\6\6\6\6\6DOUBLE\6TEAM\6RECOVER\6\6\6\6\6HARDEN\6\6\6\6\6\6MINIMIZE\6\6\6\6SMOKESCREEN\6"
 db "CONFUSE\6RAY\6WITHDRAW\6\6\6\6DEFENSE\6CURLBARRIER\6\6\6\6\6LIGHT\6SCREENHAZE\6\6\6\6\6\6\6\6REFLECT\6\6\6\6\6FOCUS\6ENERGYBIDE\6\6\6\6\6\6\6\6"
 db "METRONOME\6\6\6MIRROR\6MOVE\6SELFDESTRUCTEGG\6BOMB\6\6\6\6LICK\6\6\6\6\6\6\6\6SMOG\6\6\6\6\6\6\6\6SLUDGE\6\6\6\6\6\6BONE\6CLUB\6\6\6FIRE\6BLAST\6\6"
 db "WATERFALL\6\6\6CLAMP\6\6\6\6\6\6\6SWIFT\6\6\6\6\6\6\6SKULL\6BASH\6\6SPIKE\6CANNONCONSTRICT\6\6\6AMNESIA\6\6\6\6\6KINESIS\6\6\6\6\6SOFTBOILED\6\6"
 db "HI\6JUMP\6KICKGLARE\6\6\6\6\6\6\6DREAM\6EATER\6POISON\6GAS\6\6BARRAGE\6\6\6\6\6LEECH\6LIFE\6\6LOVELY\6KISS\6SKY\6ATTACK\6\6TRANSFORM\6\6\6"
 db "BUBBLE\6\6\6\6\6\6DIZZY\6PUNCH\6SPORE\6\6\6\6\6\6\6FLASH\6\6\6\6\6\6\6PSYWAVE\6\6\6\6\6SPLASH\6\6\6\6\6\6ACID\6ARMOR\6\6CRABHAMMER\6\6EXPLOSION\6\6\6"
 db "FURY\6SWIPES\6BONEMERANG\6\6REST\6\6\6\6\6\6\6\6ROCK\6SLIDE\6\6HYPER\6FANG\6\6SHARPEN\6\6\6\6\6CONVERSION\6\6TRI\6ATTACK\6\6SUPER\6FANG\6\6"
 db "SLASH\6\6\6\6\6\6\6SUBSTITUTE\6\6STRUGGLE\6\6\6\6SKETCH\6\6\6\6\6\6TRIPLE\6KICK\6THIEF\6\6\6\6\6\6\6SPIDER\6WEB\6\6MIND\6READER\6NIGHTMARE\6\6\6"
 db "FLAME\6WHEEL\6SNORE\6\6\6\6\6\6\6CURSE\6\6\6\6\6\6\6FLAIL\6\6\6\6\6\6\6CONVERSION\6","2AEROBLAST\6\6\6COTTON\6SPOREREVERSAL\6\6\6\6SPITE\6\6\6\6\6\6\6"
 db "POWDER\6SNOW\6PROTECT\6\6\6\6\6MACH\6PUNCH\6\6SCARY\6FACE\6\6FAINT\6ATTACKSWEET\6KISS\6\6BELLY\6DRUM\6\6SLUDGE\6BOMB\6MUD-SLAP\6\6\6\6"
 db "OCTAZOOKA\6\6\6SPIKES\6\6\6\6\6\6ZAP\6CANNON\6\6FORESIGHT\6\6\6DESTINY\6BONDPERISH\6SONG\6ICY\6WIND\6\6\6\6DETECT\6\6\6\6\6\6BONE\6RUSH\6\6\6"
 db "LOCK-ON\6\6\6\6\6OUTRAGE\6\6\6\6\6SANDSTORM\6\6\6GIGA\6DRAIN\6\6ENDURE\6\6\6\6\6\6CHARM\6\6\6\6\6\6\6ROLLOUT\6\6\6\6\6FALSE\6SWIPE\6SWAGGER\6\6\6\6\6"
 db "MILK\6DRINK\6\6SPARK\6\6\6\6\6\6\6FURY\6CUTTER\6STEEL\6WING\6\6MEAN\6LOOK\6\6\6ATTRACT\6\6\6\6\6SLEEP\6TALK\6\6HEAL\6BELL\6\6\6RETURN\6\6\6\6\6\6"
 db "PRESENT\6\6\6\6\6FRUSTRATION\6SAFEGUARD\6\6\6PAIN\6SPLIT\6\6SACRED\6FIRE\6MAGNITUDE\6\6\6DYNAMICPUNCHMEGAHORN\6\6\6\6DRAGONBREATH"
 db "BATON\6PASS\6\6ENCORE\6\6\6\6\6\6PURSUIT\6\6\6\6\6RAPID\6SPIN\6\6SWEET\6SCENT\6IRON\6TAIL\6\6\6METAL\6CLAW\6\6VITAL\6THROW\6MORNING\6SUN\6"
 db "SYNTHESIS\6\6\6MOONLIGHT\6\6\6HIDDEN\6POWERCROSS\6CHOP\6\6TWISTER\6\6\6\6\6RAIN\6DANCE\6\6SUNNY\6DAY\6\6\6CRUNCH\6\6\6\6\6\6MIRROR\6COAT\6"
 db "PSYCH\6UP\6\6\6\6EXTREMESPEEDANCIENTPOWERSHADOW\6BALL\6FUTURE\6SIGHTROCK\6SMASH\6\6WHIRLPOOL\6\6\6BEAT\6UP\6\6\6\6\6FAKE\6OUT\6\6\6\6"
 db "UPROAR\6\6\6\6\6\6STOCKPILE\6\6\6SPIT\6UP\6\6\6\6\6SWALLOW\6\6\6\6\6HEAT\6WAVE\6\6\6HAIL\6\6\6\6\6\6\6\6TORMENT\6\6\6\6\6FLATTER\6\6\6\6\6WILL-O-WISP\6"
 db "MEMENTO\6\6\6\6\6FACADE\6\6\6\6\6\6FOCUS\6PUNCH\6SMELLINGSALTFOLLOW\6ME\6\6\6NATURE\6POWERCHARGE\6\6\6\6\6\6TAUNT\6\6\6\6\6\6\6HELPING\6HAND"
 db "TRICK\6\6\6\6\6\6\6ROLE\6PLAY\6\6\6WISH\6\6\6\6\6\6\6\6ASSIST\6\6\6\6\6\6INGRAIN\6\6\6\6\6SUPERPOWER\6\6MAGIC\6COAT\6\6RECYCLE\6\6\6\6\6REVENGE\6\6\6\6\6"
 db "BRICK\6BREAK\6YAWN\6\6\6\6\6\6\6\6KNOCK\6OFF\6\6\6ENDEAVOR\6\6\6\6ERUPTION\6\6\6\6SKILL\6SWAP\6\6IMPRISON\6\6\6\6REFRESH\6\6\6\6\6GRUDGE\6\6\6\6\6\6"
 db "SNATCH\6\6\6\6\6\6SECRET\6POWERDIVE\6\6\6\6\6\6\6\6ARM\6THRUST\6\6CAMOUFLAGE\6\6TAIL\6GLOW\6\6\6LUSTER\6PURGEMIST\6BALL\6\6\6FEATHERDANCE"
 db "TEETER\6DANCEBLAZE\6KICK\6\6MUD\6SPORT\6\6\6ICE\6BALL\6\6\6\6NEEDLE\6ARM\6\6SLACK\6OFF\6\6\6HYPER\6VOICE\6POISON\6FANG\6CRUSH\6CLAW\6\6"
 db "BLAST\6BURN\6\6HYDRO\6CANNONMETEOR\6MASH\6ASTONISH\6\6\6\6WEATHER\6BALLAROMATHERAPYFAKE\6TEARS\6\6AIR\6CUTTER\6\6OVERHEAT\6\6\6\6"
 db "ODOR\6SLEUTH\6ROCK\6TOMB\6\6\6SILVER\6WIND\6METAL\6SOUND\6GRASSWHISTLETICKLE\6\6\6\6\6\6COSMIC\6POWERWATER\6SPOUT\6SIGNAL\6BEAM\6"
 db "SHADOW\6PUNCHEXTRASENSORYSKY\6UPPERCUTSAND\6TOMB\6\6\6SHEER\6COLD\6\6MUDDY\6WATER\6BULLET\6SEED\6AERIAL\6ACE\6\6ICICLE\6SPEAR"
 db "IRON\6DEFENSEBLOCK\6\6\6\6\6\6\6HOWL\6\6\6\6\6\6\6\6DRAGON\6CLAW\6FRENZY\6PLANTBULK\6UP\6\6\6\6\6BOUNCE\6\6\6\6\6\6MUD\6SHOT\6\6\6\6POISON\6TAIL\6"
 db "COVET\6\6\6\6\6\6\6VOLT\6TACKLE\6MAGICAL\6LEAFWATER\6SPORT\6CALM\6MIND\6\6\6LEAF\6BLADE\6\6DRAGON\6DANCEROCK\6BLAST\6\6SHOCK\6WAVE\6\6"
 db "WATER\6PULSE\6DOOM\6DESIRE\6PSYCHO\6BOOST"

 d_types:
 db  1, 7, 1, 1, 1, 1, 2, 6, 4, 1, 1, 1, 1, 1, 1,10,10, 1,10, 1, 1, 5, 1, 7, 1
 db  7, 7, 9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 8,12,12, 1,16, 1, 1, 1, 1, 1, 1
 db  8, 2, 2, 6, 3, 3, 3, 6, 6,11, 3, 6, 1,10,10, 7, 7, 7, 7, 1, 5, 5, 5, 1, 5
 db  5, 8, 5, 5, 5,12,15, 2, 4, 4, 4, 4,13, 9, 9, 9, 8,11,11,11,11,11, 1, 1,11
 db 14, 1, 1, 1, 1, 1, 1, 1,14, 3, 1,11,11, 6,11, 1, 1, 1,10, 1, 1,14, 8, 8, 9
 db  2, 3, 3, 1, 1, 1, 1,11,11, 1, 7, 1,11, 8, 1,12, 1,10, 1, 3, 1, 5, 1,11, 1
 db  8, 3, 1, 1, 9,11,13, 1, 1, 1, 1, 1, 1, 1, 1, 1, 7,16,12, 1,14, 2, 1, 0, 1
 db  1,10, 5, 7,14, 6, 1, 7, 1,16, 1, 1, 8, 9, 3, 9, 4, 1,14, 1, 6, 7, 9, 1,15
 db 13, 5, 1, 1,13, 1, 1, 1, 4,12,17, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 9, 7,12,15
 db  1, 1,16, 1, 1,17,17, 7, 1, 5, 1, 1, 7,15, 3, 2,16,11, 1, 1,13,14,11, 7, 3
 db 16, 1, 1, 1, 1, 1, 2, 6,16,16, 2,16, 1, 7, 1, 1, 1, 4,16, 1,11,11, 1, 1, 5
 db  7,11, 1, 7, 7, 1,16, 1, 2,11,11, 1,14,16, 1, 3, 7, 1,12,11,11,10, 1, 2, 9
 db  6, 5, 1, 1, 8, 1, 2, 3,17,14, 1, 5,16,10, 2, 1,13,12,17, 5, 1,11, 3,12,14
 db 11, 7, 9, 6, 3, 5,10, 6,17, 1, 1,15, 5, 7,10, 9, 8, 1, 4, 5, 3,11, 5,15,13
 db  4, 3,17,11

d_power:
 db 040,050,015,018,080,040,075,075,075,040,055,001,080,000,050,040,060,000
 db 070,015,080,035,065,030,120,070,060,000,070,065,015,001,035,085,015,090
 db 090,120,000,015,025,014,000,060,000,000,000,000,001,000,040,040,095,000
 db 040,120,095,095,120,065,065,065,150,035,080,080,001,001,001,080,020,040
 db 000,000,055,120,000,000,000,070,000,001,015,040,095,000,120,050,100,001
 db 060,000,050,090,000,000,000,040,020,000,001,000,000,000,000,000,000,000
 db 000,000,000,000,000,000,000,000,001,000,000,200,100,020,020,065,065,120
 db 080,035,060,100,020,010,000,000,000,085,000,100,000,015,020,000,140,000
 db 020,070,000,000,001,000,000,090,250,018,050,000,075,080,000,000,080,001
 db 070,000,050,000,010,040,000,000,000,060,040,000,001,000,100,000,001,000
 db 040,000,040,000,060,000,000,090,020,065,000,100,000,000,000,055,000,025
 db 000,090,000,060,000,000,030,040,000,000,065,010,070,000,000,000,000,001
 db 001,001,000,000,100,001,100,120,060,000,000,040,020,000,100,050,070,000
 db 000,000,001,100,040,000,000,080,001,000,080,060,080,080,020,015,010,040
 db 050,000,100,000,100,000,000,000,000,000,070,150,060,000,000,000,000,000
 db 000,000,000,000,000,120,000,000,060,075,000,020,001,150,000,000,000,000
 db 000,070,060,015,000,000,070,070,000,000,085,000,030,060,000,090,050,075
 db 150,150,100,030,050,000,000,055,140,000,050,060,000,000,000,000,150,075
 db 060,080,085,015,001,095,010,060,010,000,000,000,080,150,000,085,055,050
 db 040,120,060,000,000,070,000,025,060,060,120,140

d_accuracy:
 db 100,100,085,085,085,100,100,100,100,100,100,030,100,000,095,100,100,100
 db 095,075,075,100,100,100,075,095,085,100,100,100,085,030,095,100,085,085
 db 100,100,100,100,100,085,100,100,100,100,055,055,090,055,100,100,100,000
 db 100,080,100,100,070,100,100,100,090,100,100,080,100,100,100,100,100,100
 db 090,000,095,100,075,075,075,100,095,100,070,100,100,100,070,090,100,030
 db 100,085,100,100,060,000,000,100,100,000,100,100,085,000,000,000,000,100
 db 100,000,000,000,000,000,000,000,100,000,000,100,075,100,070,100,085,085
 db 100,075,000,100,100,100,000,080,100,090,075,100,055,085,100,075,090,000
 db 100,100,100,070,080,000,000,085,100,080,090,000,090,090,000,000,100,090
 db 100,000,100,000,090,100,100,100,100,100,100,000,100,100,095,085,100,100
 db 100,000,100,090,000,075,000,100,100,085,000,050,100,000,000,095,000,080
 db 100,100,000,100,000,100,090,100,090,000,100,095,090,100,100,000,000,100
 db 090,100,000,100,095,100,050,085,100,000,100,100,100,100,075,095,100,000
 db 000,000,100,080,100,000,000,100,100,000,100,100,100,090,100,070,100,100
 db 100,000,100,000,090,000,100,100,075,100,100,100,100,100,095,100,100,100
 db 100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100
 db 100,100,100,100,100,100,100,100,100,100,090,100,090,100,100,100,100,095
 db 090,090,085,100,100,000,100,095,090,100,080,100,085,055,100,000,100,100
 db 000,100,090,070,030,085,100,000,100,000,100,000,100,090,000,085,095,100
 db 100,100,000,100,000,100,000,080,000,100,085,090

d_priority:
 db 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,-6,00,00,00,00,00,00,00
 db 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,-6,00,00,00,00
 db 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,-5,00,00,00,00,00,00,00
 db 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01,00,00
 db 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
 db 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
 db 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
 db 00,00,00,00,00,00,03,01,00,00,00,00,00,00,00,00,00,00,00,00,00,03,00,00,00
 db 00,00,03,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
 db 00,00,00,00,00,00,00,-1,00,00,00,00,00,00,00,00,00,-5,00,01,00,00,00,00,00
 db 00,01,00,00,00,00,00,00,00,00,00,00,00,-3,00,03,00,00,00,05,00,00,00,00,00
 db 00,04,00,-4,00,00,00,00,00,00,00,00,00,04,00,00,00,00,00,00,00,00,00,00,00
 db 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
 db 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
 db 00,00,00,00

d_target:
 db 0,0,0,0,0,0,0,0,0,0,0,0,4,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3
 db 0,4,0,0,0,4,0,4,0,0,0,0,0,4,0,0,5,0,0,4,0,4,0,0,0,0,0,0,0,0,1,0,0,0,0,0,5
 db 4,0,0,0,0,3,4,0,0,0,0,0,0,0,6,0,0,0,0,0,0,5,5,0,0,5,0,0,0,5,5,5,5,0,0,5,5
 db 5,5,5,5,5,5,1,1,6,0,0,0,0,0,0,0,0,4,0,0,0,5,0,5,0,0,0,0,0,0,0,0,0,4,0,0,0
 db 0,5,5,0,6,0,0,5,4,0,5,5,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,4,5,0,0,0
 db 0,5,0,0,0,7,0,0,5,5,4,5,0,0,3,5,0,5,0,0,0,0,5,0,0,0,0,0,1,5,0,0,0,5,0,0,6
 db 0,0,0,5,0,0,0,4,0,0,0,5,5,5,0,0,4,5,5,0,1,0,0,0,0,0,0,0,0,0,3,5,0,5,4,5,0
 db 0,0,0,0,0,0,5,1,5,0,5,0,0,5,1,5,0,1,5,0,0,0,0,0,4,0,5,5,5,1,0,0,0,5,5,0,0
 db 0,6,0,5,0,0,5,4,0,0,0,0,0,0,0,5,0,4,0,0,0,0,0,0,0,5,4,0,0,0,0,0,0,4,0,0,0
 db 5,0,5,0,0,5,0,0,0,0,0,0,5,5,0,5,0,0,0,0,0

d_pp:
 db 35/5,25/5,10/5,15/5,20/5,20/5,15/5,15/5,15/5,35/5,30/5,05/5,10/5,30/5,30/5,35/5,35/5,20/5
 db 15/5,20/5,20/5,10/5,20/5,30/5,05/5,25/5,15/5,15/5,15/5,25/5,20/5,05/5,35/5,15/5,20/5,20/5
 db 20/5,15/5,30/5,35/5,20/5,20/5,30/5,25/5,40/5,20/5,15/5,20/5,20/5,20/5,30/5,25/5,15/5,30/5
 db 25/5,05/5,15/5,10/5,05/5,20/5,20/5,20/5,05/5,35/5,20/5,25/5,20/5,20/5,20/5,15/5,20/5,10/5
 db 10/5,40/5,25/5,10/5,35/5,30/5,15/5,20/5,40/5,10/5,15/5,30/5,15/5,20/5,10/5,15/5,10/5,05/5
 db 10/5,10/5,25/5,10/5,20/5,40/5,30/5,30/5,20/5,20/5,15/5,10/5,40/5,15/5,20/5,30/5,20/5,20/5
 db 10/5,40/5,40/5,30/5,30/5,30/5,20/5,30/5,10/5,10/5,20/5,05/5,10/5,30/5,20/5,20/5,20/5,05/5
 db 15/5,10/5,20/5,15/5,15/5,35/5,20/5,15/5,10/5,20/5,30/5,15/5,40/5,20/5,15/5,10/5,05/5,10/5
 db 30/5,10/5,15/5,20/5,15/5,40/5,40/5,10/5,05/5,15/5,10/5,10/5,10/5,15/5,30/5,30/5,10/5,10/5
 db 20/5,10/5,01/5,01/5,10/5,10/5,10/5,05/5,15/5,25/5,15/5,10/5,15/5,30/5,05/5,40/5,15/5,10/5
 db 25/5,10/5,30/5,10/5,20/5,10/5,10/5,10/5,10/5,10/5,20/5,05/5,40/5,05/5,05/5,15/5,05/5,10/5
 db 05/5,15/5,10/5,05/5,10/5,20/5,20/5,40/5,15/5,10/5,20/5,20/5,25/5,05/5,15/5,10/5,05/5,20/5
 db 15/5,20/5,25/5,20/5,05/5,30/5,05/5,10/5,20/5,40/5,05/5,20/5,40/5,20/5,15/5,35/5,10/5,05/5
 db 05/5,05/5,15/5,05/5,20/5,05/5,05/5,15/5,20/5,10/5,05/5,05/5,15/5,15/5,15/5,15/5,10/5,10/5
 db 10/5,10/5,10/5,10/5,10/5,10/5,15/5,15/5,15/5,10/5,20/5,20/5,10/5,20/5,20/5,20/5,20/5,20/5
 db 10/5,10/5,10/5,20/5,20/5,05/5,15/5,10/5,10/5,15/5,10/5,20/5,05/5,05/5,10/5,10/5,20/5,05/5
 db 10/5,20/5,10/5,20/5,20/5,20/5,05/5,05/5,15/5,20/5,10/5,15/5,20/5,15/5,10/5,10/5,15/5,10/5
 db 05/5,05/5,10/5,15/5,10/5,05/5,20/5,25/5,05/5,40/5,10/5,05/5,40/5,15/5,20/5,20/5,05/5,15/5
 db 20/5,30/5,15/5,15/5,05/5,10/5,30/5,20/5,30/5,15/5,05/5,40/5,15/5,05/5,20/5,05/5,15/5,25/5
 db 40/5,15/5,20/5,15/5,20/5,15/5,20/5,10/5,20/5,20/5,05/5,05/5

d_flags:
 db 33h,33h,33h,33h,33h,13h,32h,32h,32h,33h,33h,32h,13h,4h,33h,13h,33h,12h,33h,33h,33h,33h,32h,33h,33h,33h,33h,1Ah,32h,33h,33h
 db 32h,33h,32h,33h,33h,33h,33h,1Ah,12h,12h,13h,1Ah,32h,5Ah,52h,5Ah,5Ah,13h,12h,12h,12h,12h,4h,13h,13h,13h,12h,12h,12h,12h,12h
 db 13h,33h,33h,33h,33h,22h,33h,33h,12h,12h,1Ah,4h,13h,13h,1Ah,1Ah,1Ah,33h,1Ah,13h,13h,12h,12h,1Ah,12h,13h,13h,12h,33h,1Ah,12h
 db 12h,1Ah,4h,4h,33h,33h,0h,13h,10h,5Ah,4h,4h,4h,4h,1Ah,1Ah,4h,4h,4h,4h,10h,4h,4h,31h,0h,0h,13h,13h,32h,12h,12h
 db 12h,12h,33h,33h,13h,33h,13h,32h,4h,12h,6h,33h,1Ah,12h,1Ah,13h,32h,1Ah,13h,0h,12h,32h,1Ah,1Ah,13h,0h,4h,33h,13h,33h,13h
 db 4h,12h,32h,4h,0h,12h,32h,33h,4h,33h,0h,33h,32h,1Ah,12h,12h,32h,53h,0h,33h,0h,13h,1Ah,33h,12h,12h,0h,33h,1Ah,13h,1Ah
 db 4h,12h,12h,12h,0h,12h,12h,0h,40h,12h,0h,13h,12h,33h,0h,12h,0h,1Ah,33h,33h,1Ah,14h,32h,33h,33h,1Ah,1Ah,0h,44h,33h,12h
 db 33h,4h,12h,12h,13h,32h,33h,13h,0h,12h,32h,33h,1Ah,32h,32h,33h,4h,4h,4h,13h,33h,13h,0h,0h,32h,2h,4h,33h,32h,12h,0h
 db 32h,13h,13h,12h,53h,4h,11h,4h,12h,10h,12h,1Ah,1Ah,12h,32h,30h,32h,0h,0h,4h,10h,0h,12h,0h,10h,0h,4h,32h,0h,0h,33h
 db 33h,1Ah,32h,33h,13h,12h,10h,4h,12h,2h,12h,33h,33h,4h,4h,12h,12h,1Ah,10h,32h,0h,33h,32h,4h,52h,32h,32h,13h,13h,33h,32h
 db 13h,4h,1Ah,13h,33h,12h,12h,13h,5Ah,5Ah,1Bh,4h,12h,13h,33h,12h,33h,13h,12h,13h,13h,33h,13h,4h,1Ah,4h,33h,13h,4h,33h,13h
 db 33h,12h,33h,13h,0h,4h,33h,4h,13h,13h,13h,0h,13h

d_effect:
 db 000h,02Bh,01Dh,01Dh,000h,022h,004h,005h,006h,000h,000h,026h,027h,032h,000h,095h,000h,01Ch,09Bh,02Ah,000h,000h,096h,02Ch,000h,02Dh,01Fh,017h,01Fh,000h,01Dh
 db 026h,000h,006h,02Ah,030h,01Bh,0C6h,013h,002h,04Dh,01Dh,013h,01Fh,012h,01Ch,001h,031h,082h,056h,045h,004h,004h,02Eh,000h,000h,000h,005h,005h,04Ch,046h,044h
 db 050h,000h,000h,030h,0C4h,059h,057h,000h,003h,003h,054h,00Dh,02Bh,097h,042h,043h,001h,01Bh,014h,029h,02Ah,006h,006h,043h,098h,000h,093h,026h,09Bh,021h,04Ch
 db 048h,001h,00Ah,034h,067h,051h,099h,057h,052h,03Bh,010h,020h,00Bh,06Ch,017h,031h,00Bh,09Ch,033h,023h,019h,041h,02Fh,01Ah,053h,009h,007h,000h,006h,002h,002h
 db 01Fh,004h,000h,02Ah,011h,091h,01Dh,046h,036h,017h,09Dh,02Dh,043h,008h,042h,01Dh,003h,001h,04Bh,039h,046h,04Ch,001h,017h,058h,055h,033h,02Bh,007h,01Dh,02Ch
 db 025h,01Fh,01Fh,00Ah,01Eh,024h,028h,02Bh,04Fh,030h,05Fh,068h,069h,06Ah,05Eh,06Bh,07Dh,05Ch,06Dh,063h,05Dh,02Bh,03Ch,063h,064h,005h,06Fh,067h,03Ch,011h,031h
 db 08Eh,002h,049h,049h,070h,006h,071h,062h,072h,046h,06Fh,01Dh,05Eh,01Bh,073h,003h,074h,03Ah,075h,065h,076h,09Dh,006h,077h,08Ah,06Ah,078h,061h,066h,079h,07Ah
 db 07Bh,07Ch,05Bh,07Dh,07Eh,04Ch,000h,006h,07Fh,05Ah,080h,081h,018h,045h,08Bh,04Eh,084h,085h,086h,087h,02Bh,092h,088h,089h,048h,090h,08Fh,067h,08Ch,048h,094h
 db 045h,02Ah,09Ah,09Eh,09Fh,0A0h,0A1h,0A2h,004h,0A4h,0A5h,0A6h,0A7h,0A8h,0A9h,0AAh,0ABh,0ACh,0ADh,0AEh,0AFh,0B0h,0B1h,0B2h,0B3h,0B4h,0B5h,0B6h,0B7h,0B8h,0B9h
 db 0BAh,0BBh,0BCh,0BDh,0BEh,0BFh,0C0h,0C1h,0C2h,0C3h,0C5h,09Bh,01Dh,0D5h,035h,048h,047h,03Ah,0C7h,0C8h,0C9h,075h,096h,020h,000h,0CAh,045h,050h,050h,08Bh,096h
 db 0CBh,066h,03Eh,02Bh,0CCh,071h,046h,08Ch,03Eh,001h,0CDh,0CEh,0BEh,04Ch,011h,096h,0CFh,02Ah,026h,049h,01Dh,011h,01Dh,033h,06Ah,00Ah,000h,050h,0D0h,09Bh,046h
 db 0D1h,069h,0C6h,011h,0D2h,0D3h,02Bh,0D4h,01Dh,011h,04Ch,094h,0CCh

d_effchance:
 db 000,000,000,000,000,100,010,010,010,000,000,000,000,000,000,000,000,000,000,100,000,000,030,000,000,000,030,000,030,000,000
 db 000,000,030,100,000,100,000,000,030,020,000,000,030,000,000,000,000,000,000,010,010,010,000,000,000,000,010,010,010,010,010
 db 000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,100,000,000,100,010,010,000,030,000,000,000,000,100,010
 db 010,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,030,040,030
 db 010,010,000,100,000,000,000,010,000,000,000,000,000,000,000,000,000,000,030,000,010,020,000,000,000,000,000,000,000,000,000
 db 000,030,010,000,000,020,000,000,000,000,000,000,100,000,000,000,010,030,000,000,000,000,000,000,000,010,000,000,000,000,000
 db 000,030,100,050,000,100,000,000,000,100,000,000,000,100,000,000,000,000,000,000,100,000,030,000,010,000,000,000,000,000,000
 db 000,000,000,050,000,100,000,030,000,000,000,000,000,030,010,000,000,000,000,000,000,020,000,000,020,000,000,000,010,020,000
 db 050,100,000,000,100,000,000,000,010,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000
 db 000,000,100,000,000,000,000,000,000,000,030,000,000,000,000,050,050,000,000,010,000,000,030,000,000,030,050,000,000,020,030
 db 000,000,000,000,100,000,100,010,000,000,000,000,000,010,000,010,000,100,000,030,000,000,000,000,000,000,000,000,000,030,100
 db 010,100,000,000,000,000,000,000,000,000,020,000,100

d_appeal:
 db 4,3,2,2,4,1,4,4,4,4,4,2,3,1,2,3,2,3,1,3,2,4,1,2,4,6,1,2,2,4,2
 db 2,4,1,3,6,4,6,2,2,2,2,3,1,2,3,2,3,2,2,1,4,4,1,4,4,3,2,4,3,1,2
 db 4,4,4,6,1,2,2,2,2,1,2,1,3,4,3,2,1,4,2,1,3,4,4,2,2,2,1,2,1,3,2
 db 1,1,1,3,3,3,1,2,1,1,2,2,2,2,3,3,1,2,1,1,3,1,1,1,3,1,8,4,1,1,1
 db 2,4,2,3,2,1,2,2,1,3,4,6,1,2,3,2,2,1,3,3,2,1,1,3,2,2,1,3,8,2,4
 db 2,1,1,1,2,2,2,3,2,4,1,4,1,2,3,1,4,4,3,1,2,3,2,2,2,4,1,3,2,2,2
 db 1,2,2,2,2,4,3,8,2,1,2,4,3,4,3,2,2,2,3,1,2,2,1,3,2,2,2,3,2,1,3
 db 1,1,1,4,1,2,2,1,2,2,2,2,1,1,4,3,1,1,1,3,3,3,1,1,1,2,2,3,1,3,3
 db 1,3,2,2,3,2,4,1,4,1,2,2,1,8,2,3,2,3,1,2,2,2,2,1,3,1,1,6,1,3,3
 db 1,2,1,2,1,1,3,1,1,2,1,2,2,3,1,2,1,2,4,4,4,3,1,1,1,3,1,4,4,2,2
 db 4,2,2,2,6,3,3,1,1,1,3,1,1,3,2,1,2,3,2,2,2,2,2,1,2,1,2,4,1,1,1
 db 3,1,6,2,4,2,3,1,2,2,3,3,6

d_jam:
 db 0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,4,0,0,0,3,1,3,0,1
 db 1,0,4,0,0,4,0,0,3,3,1,0,3,0,0,0,0,0,0,4,0,0,0,0,0,0,1,0,0,3,1
 db 4,0,0,0,4,0,1,1,3,4,2,0,0,0,0,1,3,4,3,0,0,0,0,1,2,0,3,1,0,0,3
 db 3,3,0,0,0,0,0,1,0,3,0,1,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,4,3,4
 db 1,0,0,0,0,4,1,3,0,0,0,0,3,2,0,0,3,3,0,0,2,4,3,0,1,0,0,0,0,1,0
 db 0,3,4,0,0,2,1,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0
 db 0,1,1,1,0,0,0,0,1,3,0,0,0,4,0,1,0,1,0,3,0,0,4,0,0,0,0,0,0,0,0
 db 0,0,4,0,0,1,0,3,0,0,1,0,3,4,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0
 db 0,0,1,1,0,0,0,0,0,3,0,0,4,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 4,0,4,0,0,0,0,0,0,1,0,0,1,0,0,3,4,0,4,0,0,0,4,0,3,0,4,4,4,0,3
 db 0,0,0,1,0,0,0,0,3,3,0,0,0,0,0,4,1,0,1,1,1,0,1,0,0,0,1,4,0,0,3
 db 0,0,0,0,0,0,0,0,0,0,0,0,0

d_contesttype:
 db 5,5,5,5,5,4,1,1,2,5,5,2,2,1,2,4,2,4,4,5,5,2,5,2,2,2,2,3,5,2,2
 db 2,5,5,5,5,5,5,3,4,2,2,2,5,3,2,3,4,2,4,4,1,1,1,3,1,1,1,1,1,1,1
 db 2,2,2,2,5,5,5,5,4,4,4,1,2,2,4,4,4,1,4,2,1,2,2,2,2,5,5,5,4,4,4
 db 4,4,1,2,2,2,2,4,3,4,2,4,5,3,4,4,3,2,2,1,1,4,2,5,3,4,1,5,5,5,5
 db 5,1,5,5,2,5,2,5,3,4,1,2,5,4,4,5,4,1,2,4,3,2,1,1,4,3,5,5,1,5,5
 db 3,5,2,3,1,1,5,2,4,2,4,2,5,4,4,4,1,3,5,3,1,2,1,2,5,1,3,2,5,4,3
 db 3,5,3,5,4,2,4,4,1,1,2,5,4,2,5,4,5,3,5,2,3,3,2,2,2,3,3,3,1,3,3
 db 3,1,4,1,5,2,2,2,3,3,4,2,3,2,2,2,1,4,1,4,2,2,5,1,5,1,4,2,5,4,4
 db 5,1,4,3,3,5,5,5,1,1,5,4,1,5,3,5,4,3,1,4,4,4,4,3,3,3,4,5,4,4,5
 db 2,3,4,5,1,4,4,3,5,4,4,1,5,4,1,4,4,1,3,1,3,1,4,3,2,4,2,1,1,2,4
 db 4,4,4,2,1,4,4,1,4,5,3,2,1,1,4,2,2,4,1,5,2,2,1,5,3,2,2,2,1,3,5
 db 4,3,2,1,3,4,2,2,5,2,1,2,4

d_contesteffect:
 db 0,1,2,3,0,4,0,0,0,0,0,5,1,6,5,7,3,7,8,9,10,0,11,3,0,12,13
 db 2,18,0,2,5,0,11,9,12,16,12,14,18,18,2,9,13,14,7,15,7,3,15,11,0,0,8
 db 0,0,1,10,0,7,13,10,16,0,0,12,11,17,10,10,18,11,19,6,1,0,20,5,13,16,18
 db 21,9,0,0,5,19,3,13,5,8,20,18,13,13,6,22,22,23,8,10,24,13,17,10,17,17,25
 db 7,8,17,8,8,20,8,13,8,23,24,26,0,11,13,11,2,0,14,9,27,11,2,18,6,9,0
 db 12,13,19,20,3,18,13,1,23,19,11,13,25,5,14,6,1,26,2,0,17,13,11,6,3,19,5
 db 1,17,0,24,0,28,15,9,13,0,0,29,21,3,1,2,14,14,0,8,22,2,27,15,6,2,2
 db 2,15,0,20,26,5,13,17,0,9,16,7,2,17,10,9,13,27,3,11,23,3,15,15,23,14,30
 db 23,30,8,11,0,4,2,3,13,15,15,5,17,13,11,0,29,31,31,31,23,1,7,4,4,11,17
 db 3,22,6,25,9,32,9,5,10,7,17,0,6,0,13,15,15,11,26,14,29,18,9,4,3,15,15
 db 3,28,9,31,8,12,8,23,29,11,15,11,14,21,28,20,6,21,5,32,17,2,1,6,18,11,14
 db 16,0,0,9,11,21,13,20,11,16,16,3,18,0,14,14,10,12,20,9,6,13,13,20,6,21,7
 db 27,11,10,9,5,2,5,27,10,8,15,6,10,16,6,8,13,20,28,12,27,0,17,1,6,3,27,7,9,12

spacer equ		0
hi equ		1
badly equ		2
more equ		3
less equ		4
ifnot equ		5
iff equ		6
to_ equ		7
or equ		8
no equ		9
for equ	10	
and equ	11	
with equ	12	
next equ	13	
last equ	14	
same equ	15	
that equ	16	
moves equ	17	
eachturn equ		18
nextturn equ		19
nextmove equ		20
thisturn equ		21
ifmiss equ		22
ifuser equ		23
getmoney equ	24	
noeff equ		25
effect equ		26
weight equ		27
level equ		28
perfect equ	29	
completely equ	30	
random equ		31
sunmax equ		32
sunweather equ	33	
rainweather equ	34		
sandweather equ	35		
hailweather equ	36		
weather equ	37	
power equ		38
temporarily equ	39		
permanently equ	40		
stomp equ		41
rollout equ	42	
endureatks equ	43	
isonscreen equ	44	
onlyworxif equ		45
always equ		46
wasteaturn equ	47	
butuser equ	48	
tpofusers equ	49	
substitute equ	50	
item equ		51
ceff1 equ		52
ceff2 equ		53
ceff3 equ		54
ceff4 equ		55
morehp4 equ	56	
morepower equ	57	
protect1 equ	58	
protect2 equ	59	
protect3 equ	60	
protect4 equ	61	
protct5 equ	62	
spikes equ		63
onscreen equ	64	
atleast equ	65	
successive equ	66	
ofwhole equ	67	
tamenesstimes equ	68		
presentpwr equ		69
evenlydivide equ	70	
magnipwr1 equ		71
magnipwr2 equ		72
magnitudes equ		73
passchanges equ	74	
wofail equ		75
dvs equ		76
whenhitby equ	77	
affinity equ	78	
everymember equ	79
party equ		80
ofparty equ	81	
cantbeused equ	82	
surroundings equ	83	
canonlyuse equ		84
dmgdealing equ		85
partners equ		86
ability equ		87
used equ		88
brickbreak1 equ	89	
brickbreak2 equ	90	
foehpminususerhp equ	91		
type equ		92
physical equ	93	
special equ	94	
resistant equ	95	
fire equ		96
ofelectype equ	97	
hp equ		98
maxhp equ	99	
atk equ	100	
def equ	101	
spd equ	102	
sat equ	103	
sde equ	104	
stat equ	105	
allstats equ		106
accuracy equ		107
evade equ		108
crithit equ	109	
ratio equ		110
andsatminus2 equ		111
status equ		112
burn equ		113
freeze equ		114
paralyze equ	115	
sleeps equ		116
poison equ		117
thaw equ		118
flinch equ		119
confuse equ	120	
attract equ	121	
foesasleep equ	122	
statuschange equ	123
foe equ		124
user equ		125
userandfoe equ	126	
pokemon equ	127	
todiggers equ	128	
toflyers equ	129	
everypkmn equ	130	
foestarget equ	131	
move equ		132
attacks equ	133	
prevent equ	134	
lowering equ	135	
force equ		136
flee equ		137
switch equ		138
trap equ		139
trapfoe equ	140	
damage equ		141
recoil equ		142
losecontrol equ		143
disable equ		144
recharge equ		145
dependson equ		146
dealback equ		147
equals equ		148
drain equ		149
skip equ		150
rises equ		151
ishit equ		152
copy equ		153
heal equ		154
orheal equ		155
deal equ		156
dealsmore equ	157	
reset equ		158
use equ		159
faints equ		160
faintsafter equ		161
hits equ		162
can equ		163
transfrmin equ	164	
change equ		165
createa equ	166	
steal equ		167
gets equ		168
maximize equ	169	
ignore equ		170
set equ		171
leaves equ		172
repeats equ	173	
switches equ	174	
releasefrom equ	175		
delayedby equ		176
stored equ		177
has equ		178
becomes equ	179	
reboundsat equ	180	
returns equ	181	
destroy equ	182	
knocksoff equ	183	
knows equ		184
weakens equ	185	
pp0 equ		186
first equ		187
firstturn equ	188	
_1_15x equ		189
onehp equ		190
minus1 equ		191
plus1 equ		192
minus2 equ		193
plus2 equ		194
double equ		195
in2turns equ	196	
fiveturns equ	197	
twoturns equ	198	
twotimes equ	199	
_2xinarow equ	200	
_2_3times equ	201	
loses2_5pp equ	202	
_2_5times equ	203	
_2_5turns equ	204	
upto3x equ		205
_3_6times equ	206
halfmaxhp equ	207	
quartermaxhp equ		208
_116maxhp equ		209
plus10 equ		210
twenty equ		211
forty equ		212
twohundredto equ		213
_255minus equ		214
half equ		215
third equ		216
quarter equ	217	
pt4 equ		218
_1_5xatk equ	219	
raise equ	220
of	equ	221
lastmove	equ	222
undefined	equ	223
firstturnif equ 224
afterwards equ 225
lesspower equ 226
healstatusif equ 227
halfdamage equ 228
fntbyfoes equ 229

si_effects:
 dw mef_spacer,mef_hi,mef_badly,mef_more,mef_less,mef_ifnot,mef_iff,mef_to_,mef_or,mef_no
 dw mef_for,mef_and,mef_with,mef_next,mef_last,mef_same,mef_that,mef_moves,mef_eachturn,mef_nextturn
 dw mef_nextmove,mef_thisturn,mef_ifmiss,mef_ifuser,mef_getmoney,mef_noeff,mef_effect,mef_weight,mef_level,mef_perfect
 dw mef_completely,mef_random,mef_sunmax,mef_sunweather,mef_rainweather,mef_sandweather,mef_hailweather,mef_weather,mef_power,mef_temporarily
 dw mef_permanently,mef_stomp,mef_rollout,mef_endureatks,mef_isonscreen,mef_onlyworkxif,mef_always,mef_wasteaturn,mef_butuser,mef_tpofusers
 dw mef_substitute,mef_item,mef_ceff1,mef_ceff2,mef_ceff3,mef_ceff4,mef_morehp4,mef_morepower,mef_protect1,mef_protect2
 dw mef_protect3,mef_protect4,mef_protct5,mef_spikes,mef_onscreen,mef_atleast,mef_successive,mef_ofwhole,mef_tamenesstimes,mef_presentpwr
 dw mef_evenlydivide,mef_magnipwr1,mef_magnipwr2,mef_magnitudes,mef_passchanges,mef_wofail,mef_dvs,mef_whenhitby,mef_affinity,mef_everymember
 dw mef_party,mef_ofparty,mef_cantbeused,mef_surroundings,mef_canonlyuse,mef_dmgdealing,mef_partners,mef_ability,mef_used,mef_brickbreak1
 dw mef_brickbreak2,mef_foehpminususerhp,mef_type,mef_physical,mef_special,mef_resistant,mef_fire,mef_ofelectype,mef_hp,mef_maxhp
 dw mef_atk,mef_def,mef_spd,mef_sat,mef_sde,mef_stat,mef_allstats,mef_accuracy,mef_evade,mef_crithit
 dw mef_ratio,mef_andsatminus2,mef_status,mef_burn,mef_freeze,mef_paralyze,mef_sleeps,mef_poison,mef_thaw,mef_flinch
 dw mef_confuse,mef_attract,mef_foesasleep,mef_statuschange,mef_foe,mef_user,mef_userandfoe,mef_pokemon,mef_todiggers,mef_toflyers
 dw mef_everypkmn,mef_foestarget,mef_move,mef_attacks,mef_prevent,mef_lowering,mef_force,mef_flee,mef_switch,mef_trap
 dw mef_trapfoe,mef_damage,mef_recoil,mef_losecontrol,mef_disable,mef_recharge,mef_dependson,mef_dealback,mef_equals,mef_drain
 dw mef_skip,mef_rises,mef_ishit,mef_copy,mef_heal,mef_orheal,mef_deal,mef_dealsmore,mef_reset,mef_use
 dw mef_faints,mef_faintsafter,mef_hits,mef_can,mef_transfrmin,mef_change,mef_createa,mef_steal,mef_gets,mef_maximize
 dw mef_ignore,mef_set,mef_leaves,mef_repeats,mef_switches,mef_releasefrom,mef_delayedby,mef_stored,mef_has,mef_becomes
 dw mef_reboundsat,mef_returns,mef_destroy,mef_knocksoff,mef_knows,mef_weakens,mef_pp0,mef_first,mef_firstturn,mef__1_15x
 dw mef_onehp,mef_minus1,mef_plus1,mef_minus2,mef_plus2,mef_double,mef_in2turns,mef_fiveturns,mef_twoturns,mef_twotimes
 dw mef__2xinarow,mef__2_3times,mef_loses2_5pp,mef__2_5times,mef__2_5turns,mef_upto3x,mef__3_6times,mef_halfmaxhp,mef_quartermaxhp,mef__116maxhp
 dw mef_plus10,mef_twenty,mef_forty,mef_twohundredto,mef__255minus,mef_half,mef_third,mef_quarter,mef_pt4,mef__1_5xatk
 dw mef_raise,mef_of,mef_lastmove,mef_undefined,mef_firstturnif,mef_afterwards,mef_lesspower,mef_healstatusif,mef_halfdamage
 dw mef_fntbyfoes

s_effects:
mef_spacer:		db " ",0
mef_hi:		db "HIGH",0
mef_badly:		db "BADLY",0
mef_more:		db "MORE",0
mef_less:		db "LESS",0
mef_ifnot:		db "IF NOT",0
mef_iff:			db "IF",0
mef_to_:			db "TO",0
mef_or:			db "OR",0
mef_no:			db "NO",0
mef_for:		db "FOR",0
mef_and:		db "AND",0
mef_with:		db "WITH",0
mef_next:		db "NEXT",0
mef_last:		db "LAST",0
mef_same:		db "SAME",0
mef_that:		db "THAT",0
mef_moves:		db "MOVES",0
mef_eachturn:	db "EACH TURN",0
mef_nextturn:	db "NEXT TURN",0
mef_nextmove:	db "NEXT MOVE",0
mef_thisturn:	db "THIS TURN",0
mef_ifmiss:		db "IF MISS",0
mef_ifuser:		db "IF USER",0
mef_getmoney:	db "GET MONEY",0
mef_noeff:		db "NO EFFECT",0
mef_effect:		db "EFFECT",0
mef_weight:		db "WEIGHT",0
mef_level:		db "LEVEL",0
mef_perfect:	db "PERFECT",0
mef_completely:	db "COMPLETELY",0
mef_random:		db "RANDOM",0
mef_sunmax:		db "WTH:SUN->MAX",0
mef_sunweather:	db "WEATHER:SUN",0
mef_rainweather:db "WEATHER:RAIN",0
mef_sandweather:db "WEATHER:SAND",0
mef_hailweather:db "WEATHER:HAIL",0
mef_weather:	db "WEATHER",0
mef_power:		db "POWER",0
mef_temporarily:db "TEMPORARILY",0
mef_permanently:db "PERMANENTLY",0
mef_stomp:		db "FOE'S STOMP",0
mef_rollout:	db "USR'S ROLLOUT",0
mef_endureatks:	db "ENDURE ATKS",0
mef_isonscreen:	db "IS ON SCREEN",0
mef_onlyworkxif:	db "ONLY WORX IF",0
mef_always:		db "ALWAYS",0
mef_wasteaturn:	db "WASTE A TURN",0
mef_butuser:	db "BUT USER",0
mef_tpofusers:	db "TP.OF USER'S",0
mef_substitute:	db "SUBSTITUTE",0
mef_item:		db "ITEM",0
mef_ceff1:		db "CURSE FOE IF",0
mef_ceff2:		db "USR'S GHOST;",0
mef_ceff3:		db "OR USR SPD-1",0
mef_ceff4:		db "ATK+1 DEF+1",0
mef_morehp4:	db "MORE HP ->",0
mef_morepower:	db "MORE POWER",0
mef_protect1:	db "PREVENT DMG;",0
mef_protect2:	db "99.6%,\6","49.6%",0
mef_protect3:	db "24.6%,\6","12.1%",0
mef_protect4:	db "\6","5.9%,\6\6","2.7%",0
mef_protct5:	db "\6","1.2%,\6\6","0.4%",0
mef_spikes:		db "SPIKES",0
mef_onscreen:	db "ON SCREEN",0
mef_atleast:	db "AT LEAST",0
mef_successive:	db "SUCCESSIVE",0
mef_ofwhole:	db "OF WHOLE",0
mef_tamenesstimes:db "TAMENESS x",0
mef_presentpwr:	db "40,80,OR 120",0
mef_evenlydivide:db "EVENLYDIVIDE",0
mef_magnipwr1:	db "10,30,50,70,",0
mef_magnipwr2:	db "90,110,150",0
mef_magnitudes:	db "(MAGN. 4-10)",0
mef_passchanges:db "PASS CHANGES",0
mef_wofail:		db "W/O FAIL",0
mef_dvs:		db "DVs",0
mef_whenhitby:	db "WHEN HIT BY",0
mef_affinity:	db "AFFINITY",0
mef_everymember:db "EVERY MEMBER",0
mef_party:		db "PARTY",0
mef_ofparty:	db "OF PARTY",0
mef_cantbeused:	db "CAN'T B USED",0
mef_surroundings:db "SURROUNDINGS",0
mef_canonlyuse:	db "CAN ONLY USE",0
mef_dmgdealing:	db "DMG-DEALING",0
mef_partners:	db "PARTNER'S",0
mef_ability:	db "ABILITY",0
mef_used:		db "USED",0
mef_brickbreak1:db "REFLECT,LGHT",0
mef_brickbreak2:db "SCREEN ETC.",0
mef_foehpminususerhp:db "FOEHP-USERHP",0
; [TYPE],0
mef_type:		db "TYPE",0
mef_physical:	db "PHYSICAL",0
mef_special:	db "SPECIAL",0
mef_resistant:	db "RESISTANT",0
mef_fire:		db "FIRE",0
mef_ofelectype:	db "OF ELEC.TYPE",0
; [STATS ETC],0
mef_hp:			db "HP",0
mef_maxhp:		db "MAX. HP",0
mef_atk:		db "ATTACK",0
mef_def:		db "DEFENSE",0
mef_spd:		db "SPEED",0
mef_sat:		db "SPEC.ATTACK",0
mef_sde:		db "SPEC.DEFENSE",0
mef_stat:		db "STAT",0
mef_allstats:	db "ALL STATS",0
mef_accuracy:	db "ACCURACY",0
mef_evade:		db "EVADE",0
mef_crithit:	db "CRITICAL HIT",0
mef_ratio:		db "RATIO",0
mef_andsatminus2:db "& S.ATK -2",0
; [STATUS],0
mef_status:		db "STATUS",0
mef_burn:		db "BURN",0
mef_freeze:		db "FREEZE",0
mef_paralyze:	db "PARALYZE",0
mef_sleeps:		db "SLEEPS",0
mef_poison:		db "POISON",0
mef_thaw:		db "THAW",0
mef_flinch:		db "FLINCH",0
mef_confuse:	db "CONFUSE",0
mef_attract:	db "ATTRACT",0
mef_foesasleep:	db "FOE'S ASLEEP",0
mef_statuschange:db "STATUSCHANGE",0
; [POKEMON],0
mef_foe:		db "FOE",0
mef_user:		db "USER",0
mef_userandfoe:	db "USER & FOE",0
mef_pokemon:	db "POKEMON",0
mef_todiggers:	db "TO DIGGERS",0
mef_toflyers:	db "TO FLYERS",0
mef_everypkmn:	db "EVERY PKMN",0
mef_foestarget:	db "FOE'S TARGET",0
; [VERBS],0
mef_move:		db "MOVE",0
mef_attacks:	db "ATTACKS",0
mef_prevent:	db "PREVENT",0
mef_lowering:	db "LOWERING",0
mef_force:		db "FORCE",0
mef_flee:		db "FLEE",0
mef_switch:		db "SWITCH",0
mef_trap:		db "TRAP",0
mef_trapfoe:	db "TRAP FOE",0
mef_damage:		db "DAMAGE",0
mef_recoil:		db "RECOIL",0
mef_losecontrol:db "LOSE CONTROL",0
mef_disable:	db "DISABLE",0
mef_recharge:	db "RECHARGE",0
mef_dependson:	db "DEPENDS ON",0
mef_dealback:	db "DEAL BACK",0
mef_equals:		db "EQUALS",0
mef_drain:		db "DRAIN",0
mef_skip:		db "SKIP",0
mef_rises:		db "RISES",0
mef_ishit:		db "IS HIT",0
mef_copy:		db "COPY",0
mef_heal:		db "HEAL",0
mef_orheal:		db "OR HEAL",0
mef_deal:		db "DEAL",0
mef_dealsmore:	db "DEALS MORE",0
mef_reset:		db "RESET",0
mef_use:		db "USE",0
mef_faints:		db "FAINTS",0
mef_faintsafter:db "FAINTS AFTER",0
mef_hits:		db "HITS",0
mef_can:		db "CAN",0
mef_transfrmin:	db "TRANSFRM IN-",0
mef_change:		db "CHANGE",0
mef_createa:	db "CREATE A",0
mef_steal:		db "STEAL",0
mef_gets:		db "GETS",0
mef_maximize:	db "MAXIMIZE",0
mef_ignore:		db "IGNORE",0
mef_set:		db "SET",0
mef_leaves:		db "LEAVES",0
mef_repeats:	db "REPEATS",0
mef_switches:	db "SWITCHES",0
mef_releasefrom:db "RELEASE FROM",0
mef_delayedby:	db "DELAYED BY",0
mef_stored:		db "STORED",0
mef_has:		db "HAS",0
mef_becomes:	db "BECOMES",0
mef_reboundsat:	db "REBOUNDS AT",0
mef_returns:	db "RETURNS",0
mef_destroy:	db "DESTROY",0
mef_knocksoff:	db "KNOCKS OFF",0
mef_knows:		db "KNOWS",0
mef_weakens:	db "WEAKENS",0
;[NUMERIC],0
mef_pp0:		db "PP=0",0
mef_first:		db "FIRST",0
mef_firstturn:	db "1st TURN",0
mef__1_15x:		db "1 TO 1.5 x",0
mef_onehp:		db "1 HP",0
mef_minus1:		db "-1",0
mef_plus1:		db "+1",0
mef_minus2:		db "-2",0
mef_plus2:		db "+2",0
mef_double:		db "DOUBLE",0
mef_in2turns:	db "IN 2 TURNS",0
mef_fiveturns:	db "5 TURNS",0
mef_twoturns:	db "2 TURNS",0
mef_twotimes:	db "TWO TIMES",0
mef__2xinarow:	db "2x IN A ROW",0
mef__2_3times:	db "2 TO 3 TIMES",0
mef_loses2_5pp:	db "LOSES 2-5 PP",0
mef__2_5times:	db "2-5 TIMES",0
mef__2_5turns:	db "2-5 TURNS",0
mef_upto3x:		db "UP TO 3x",0
mef__3_6times:	db "3-6 TIMES",0
mef_halfmaxhp:	db "1/2 MAX. HP",0
mef_quartermaxhp:db "1/4 MAX. HP",0
mef__116maxhp:	db "1/16 MAX. HP",0
mef_plus10:		db "+10",0
mef_twenty:		db "20",0
mef_forty:		db "40",0
mef_twohundredto:db "200 TO",0
mef__255minus:	db "255-",0
mef_half:		db "1/2",0
mef_third:		db "1/3",0
mef_quarter:	db "1/4",0
mef_pt4:		db "0.4",0
mef__1_5xatk:	db "1.5x ATTACK",0
mef_raise:	db "RAISE",0
mef_of:	db "OF",0
mef_lastmove:	db "LAST MOVE",0
mef_undefined:	db "????????????",0
mef_firstturnif:	db "1st TURN IF",0
mef_afterwards:	db "AFTERWARDS",0
mef_lesspower:	db "LESS POWER",0
mef_healstatusif: db "HEAL FOE IF",0
mef_halfdamage: db "1/2 DAMAGE",0
mef_fntbyfoes: db "FNT BY FOE'S",0

i_effects:
 db noeff				,0					,0					,0					,0							; 00
 db foe,				sleeps				,0					,0					,0							; 01
 db poison,				foe					,0					,0					,0						; 02
 db drain,				half,				hp					,0					,0							; 03
 db burn,				foe					,0					,0					,0						; 00
 db freeze,				foe					,0					,0					,0						; 00
 db paralyze,			foe					,0					,0					,0						; 00
 db double,				damage,				spacer,				user,				faints			; 00
 db drain,				half,				damage,				onlyworxif,			foesasleep		; 00
 db use,				foe,				last,				move				,0								; 00
 db user,				atk,				plus1				,0					,0							; 00
 db user,				def,				plus1				,0					,0							; 00
  db undefined,undefined,undefined,undefined,undefined
 db user,				sat,				plus1				,0					,0							; 00
  db undefined,undefined,undefined,undefined,undefined
  db undefined,undefined,undefined,undefined,undefined
 db user,				evade,				plus1				,0					,0						; 00
 db iff,					foe,				isonscreen,			always,				hits				; 00
 db foe,				atk,				minus1				,0					,0							; 00
 db foe,				def,				minus1				,0					,0							; 00
 db foe,				spd,				minus1				,0					,0							; 00
  db undefined,undefined,undefined,undefined,undefined
  db undefined,undefined,undefined,undefined,undefined
 db foe,				accuracy,			minus1				,0					,0						; 00
 db foe,				evade,				minus1				,0					,0						; 00
 db reset,				allstats			,0					,0					,0							; 00
 db endureatks,			_2_3times,			dealback,			double,				damage	; 00
 db _2_3times,			losecontrol,		confuse,			user,				afterwards			; 00
 db flee,				or,					force,foe,switch
 db attacks,			_2_5times			,0					,0					,0						; 00
 db change,				type,				to_,					tpofusers,			move				; 00
 db flinch,				foe					,0					,0					,0						; 00
 db heal,				half,				hp					,0					,0							; 00
 db badly,				poison,				foe					,0					,0					; 00
 db getmoney			,0					,0					,0					,0							; 00
 db special,			moves,				deal,				halfdamage,				fiveturns				; 00
 db burn,				freeze,				or,					paralyze,			foe			; 00
 db heal,				completely,			user,				sleeps,				twoturns			; 00
 db foe,				faints				,0					,0					,0							; 00
 db attacks,			in2turns,			hi,				crithit,			ratio					; 00
 db deal,				halfmaxhp,			damage				,0					,0					; 00
 db damage,				equals,				forty,			hp					,0
 db trapfoe,			_2_5turns,			_116maxhp,			damage,				eachturn
 db hi,				crithit,			ratio				,0					,0
 db attacks,			twotimes			,0					,0					,0
 db ifmiss,				damage,				user				,0					,0
 db prevent,			stat,				lowering			,0					,0
 db user,				crithit,			ratio,				plus1				,0
 db recoil,				quarter,			damage				,0					,0
 db confuse,			foe					,0					,0					,0
 db user,				atk,				plus2				,0					,0
 db user,				def,				plus1				,0					,0
 db user,				spd,				plus2				,0					,0
 db user,				sat,				plus2				,0					,0
 db user,				sde,				plus2				,0					,0
  db undefined,undefined,undefined,undefined,undefined
  db undefined,undefined,undefined,undefined,undefined
 db transfrmin,			to_,					foe					,0					,0
 db foe,				atk,				minus2				,0					,0
 db foe,				def,				minus2				,0					,0
 db foe,				spd,				minus2				,0					,0
  db undefined,undefined,undefined,undefined,undefined
 db foe,				sde,				minus2				,0					,0
  db undefined,undefined,undefined,undefined,undefined
  db undefined,undefined,undefined,undefined,undefined
 db physical,			moves,				deal,				halfdamage,	fiveturns
 db poison,				foe					,0					,0					,0
 db paralyze,			foe					,0					,0					,0
 db foe,				atk,				minus1				,0					,0
 db foe,				def,				minus1				,0					,0
 db foe,				spd,				minus1				,0					,0
 db foe,				sat,				minus1				,0					,0
 db foe,				sde,				minus1				,0					,0
 db foe,				accuracy,		minus1				,0					,0
  db undefined,undefined,undefined,undefined,undefined
 db attacks,			in2turns,			can,				flinch,			foe
 db confuse,			foe					,0					,0					,0
 db attacks,			twotimes,			spacer,				poison,				foe
 db hits,				foe,				wofail				,0					,0
 db createa,			substitute,			with,				quarter,			maxhp
 db recharge,			nextturn			,0					,0					,0
 db power,				rises,				iff,					user,				ishit
 db temporarily,		copy,				foe,				last,				move
 db use,				random,				move				,0					,0
 db drain,				_116maxhp,			eachturn			,0					,0
 db wasteaturn			,0					,0					,0					,0
 db disable,			foe,				last,				move				,0
 db damage,				equals,				level				,0					,0
 db deal,				random,				damage,				_1_15x,				level
 db dealback,			double,				physical,			damage				,0
 db foe,				repeats,			last,				move,				_3_6times
 db evenlydivide,		user,				and,				foe,				hp
 db onlyworxif,			user,				sleeps				,0					,0
 db change,				type,				to_,					resistant,			type
 db nextmove,			gets,				perfect,			accuracy			,0
 db permanently,		copy,				foe,				last,				move
  db undefined,undefined,undefined,undefined,undefined
 db random,				move,				onlyworxif,			user,				sleeps
 db ifuser,				fntbyfoes,				nextmove,			foe,				faints
 db power,				twohundredto,		twenty,				morehp4,			lesspower
 db foe,				last,				move,				loses2_5pp			,0
 db leaves,				atleast,			onehp				,0					,0
 db heal,				status,				ofwhole,			party				,0
 db attacks,				first				,0					,0					,0
 db attacks,			upto3x,				power,				plus10,				eachturn
 db steal,				foe,				item				,0					,0
 db trapfoe				,0					,0					,0					,0
 db quartermaxhp,		damage,				eachturn,			iff,					foesasleep
 db evade,				plus1,				stomp,				dealsmore,			damage
 db ceff1,				ceff2,				ceff3,				ceff4				,0
  db undefined,undefined,undefined,undefined,undefined
 db protect1,			protect2,			protect3,			protect4,			protct5
 db set,				spikes				,0					,0					,0
 db ignore,				foe,				evade,				raise				,0
 db everypkmn,			onscreen,			faintsafter,		fiveturns			,0
 db set,				sandweather			,0					,0					,0
 db foe,				nextmove,			leaves,				atleast,			onehp
 db attacks,			for,				fiveturns,morepower,			eachturn
 db foe,				atk,				plus2,				confuse,			foe
 db power,				rises,				with,				successive,			hits
 db attract,			foe					,0					,0					,0
 db power,				equals,				tamenesstimes,		pt4					,0
 db power,				equals,				presentpwr,			orheal,				quartermaxhp
 db power,				equals,				_255minus,			tamenesstimes,		pt4
 db prevent,			user,				statuschange,		for,				fiveturns
 db burn,				foe,				spacer,				thaw,				user
 db random,				power,				magnipwr1,			magnipwr2,			magnitudes
 db switch,				passchanges			,0					,0					,0
 db double,				damage,				iff,					foe,				switches
 db releasefrom,		trap				,0					,0					,0
 db damage,				equals,				twenty,				hp					,0
  db undefined,undefined,undefined,undefined,undefined
 db heal,				halfmaxhp,			rainweather,		quartermaxhp,			sunmax
 db heal,				halfmaxhp,			rainweather,		quartermaxhp,			sunmax
 db heal,				halfmaxhp,			rainweather,		quartermaxhp,			sunmax
 db power,				and,				type,				dependson,			dvs
 db set,				rainweather			,0					,0					,0
 db set,				sunweather			,0					,0					,0
 db user,				def,				plus1				,0					,0
 db user,				atk,				plus1				,0					,0
 db raise,				allstats			,0					,0					,0
  db undefined,undefined,undefined,undefined,undefined
 db maximize,			user,				atk,				recoil,				halfmaxhp
 db copy,				stat,				change				,0					,0
 db dealback,			double,				special,damage							,0
 db attacks,			in2turns,			def,				plus1,				firstturn
 db flinch,				foe,				double,				damage,				toflyers
 db double,				damage,				todiggers			,0					,0
 db delayedby,			twoturns,			no,					type,				affinity
 db force,				foe,				to_,					switch				,0
 db flinch,				foe					,0					,0					,0
 db attacks,			in2turns,			skip,				firstturnif,	sunweather		
 db paralyze,			foe,				rainweather,		perfect,			accuracy
 db flee,				or,					no,					effect				,0
 db damage,				for,				everymember,		ofwhole,			party
 db attacks,			in2turns,			evade,				attacks,			firstturn
 db def,				plus1,				rollout,			dealsmore,			damage
 db heal,				halfmaxhp			,0					,0					,0
 db flinch,				foe,				noeff,			ifnot,				firstturn
 db no,					pokemon,			sleeps,				for,				_2_5turns
 db raise,				stored,				level				,0					,0
 db power,				equals,				stored,				level				,0
 db heal,				stored,				level				,0					,0
  db undefined,undefined,undefined,undefined,undefined
 db set,				hailweather			,0					,0					,0
 db same,				move,				cantbeused,			_2xinarow			,0
 db foe,				sat,				plus2,				confuse,			foe
 db burn,				foe					,0					,0					,0
 db user,				faints,				foe,				atk,				andsatminus2
 db double,				damage,				ifuser,				has,				statuschange
 db user,				flinch,				iff,					ishit				,0
 db double,				damage,				and,				healstatusif,				paralyze
 db user,				becomes,			foestarget			,0					,0
 db move,				used,				dependson,			surroundings		,0
 db raise,				power,				of,					nextmove,			ofelectype
 db foe,				canonlyuse,			dmgdealing,			moves,				twoturns
 db partners,			move,				gets,				_1_5xatk			,0
 db switches,			user,				foe,				item				,0
 db copy,				ability,			of,					foe					,0
 db heal,				halfmaxhp,			delayedby,			twoturns			,0
 db use,				random,				move,				ofparty,			pokemon
 db trap,				user,				heal,				hp,					eachturn
 db user,				atk,				minus1,				def,				minus1
 db special,			move,				effect,				reboundsat,			foe
 db used,				item,				returns				,0					,0
 db double,				power,				iff,					user,				ishit
 db destroy,			effect,				of,					brickbreak1,		brickbreak2
 db foe,				sleeps,				nextturn			,0					,0
 db knocksoff,			item,				of,					foe					,0
 db damage,				equals,				foehpminususerhp	,0					,0
 db morehp4,			more,				damage				,0					,0
 db switches,			ability,			of,					userandfoe			,0
 db moves,				user,				knows,				cantbeused			,0
 db heal,				burn,				freeze,				or,					paralyze
 db ifuser,				faints,				foe,				lastmove,			pp0
 db steal,				effect,				of,					foe,				move
 db dependson,			foe,				weight				,0					,0
 db effect,				dependson,			surroundings		,0					,0
 db recoil,				third,				damage				,0					,0
 db confuse,			everypkmn,			that,				isonscreen			,0
 db burn,				foe,				hi,				crithit,			ratio
 db weakens,			moves,				ofelectype			,0					,0
 db badly,				poison,				foe					,0					,0
 db effect,				and,				type,				dependson,			weather
 db user,				sat,				minus2				,0					,0
 db foe,				atk,				minus1,				def,				minus1
 db user,				def,				plus1,				sde,				plus1
 db damage,				toflyers			,0					,0					,0
 db user,				atk,				plus1,				def,				plus1
 db poison,				foe,				hi,				crithit,			ratio
 db weakens,			fire,				moves				,0					,0
 db user,				sat,				plus1,				sde,				plus1
 db user,				atk,				plus1,				spd,				plus1
 db change,				type,				dependson,			surroundings		,0

waitkey:
 select_keywait:
  halt				; save batteries
  B_CALL GetCSC	; Get keyboard scan code
  or a	; is a 0?
  jr z,select_keywait	; go again if no key
  ld hl,keys
  ld d,0
  ld e,a		; put scan code to DE (d=0 from above)
  add hl,de	; get "my" code address
  ld a,(hl)	; get "my" code to a
  ; switch(my_code)
  cp 0						; case 0
  jr z,select_keywait	;  go to wait again
  cp "d"
  jr z,waitkey_down
  cp "u"
  jr z,waitkey_up
  cp ">"
  jr z,waitkey_select
  cp "~"
  jr z,waitkey_quit

  cp 1		; case 1 (Green)
  jr z,waitkey_green
  cp 2		; case 2 (Move)
  jr z,select_keywait ; no need to do anything
  cp 3		; case 3 (Select)
  jr z,waitkey_select
  cp 4		; case 4 (Movelist)
  jr z,waitkey_movelist
  cp 5		; case 5 (Help/Extra)
  jr z,waitkey_statcalc

 ;default:
  push AF
  B_CALL ZeroOP1 ; OP1 = 00000000000
  pop AF
  LD (OP1+1),A ; OP1 = var name

  push AF
  AppOnErr waitkey_failed
  B_CALL RclVarSym ; OP1(/OP2) = value
  AppOffErr
  pop AF
  B_CALL CkOP1Real ; ACC = type, Z = 1 if real
  jp nz,waitkey_failed
  B_CALL StoX
  jp Move
 
waitkey_down:
  B_CALL RclX
  B_CALL Plus1
  B_CALL StoX
  jp Move
waitkey_up:
  B_CALL RclX
  B_CALL Minus1
  B_CALL StoX
  jp Move
waitkey_select:
  B_JUMP Select
waitkey_movelist:
  B_JUMP MoveList
waitkey_quit:
  B_JUMP JForceCmdNoChar
waitkey_failed:
  ld A,26
  ld (penCol),A
  ld A,48
  ld (penRow),A
  ld DE,OP1
  ld HL,fail
  ld BC,4
  ldir
  ld b,4
  ld hl,OP1
  B_CALL VPutSN
  pop AF
  B_CALL VPutMap
  jp waitkey
waitkey_green:
  B_JUMP Green
waitkey_statcalc:
  B_JUMP StatCalculator


keys:
 db  SFourSpaces				; (not used)
 db  "d",0,0,"u",0,0,0,0		; v<>^????
 db        ">",0,"WRMH",0,0	; enter+-*/^clear-?
 db           0,"[VQLG",0,0	; -369)tan-vars-?
 db           0,"ZUPKFC",0		; .258(cos-prgm-stat
 db 			  0,"YTOJEB",0		; 0147,sin-apps-xt0n
 db        "~",0,"SNIDA",0		; ?-store-ln-log-square-recip-math-alpha
 db       "\5\4\3\2\1",0,"~"	; graph-trace-zoom-window-y=-2nd-mode
 db 0	; del

fail:
 db "BAD:"




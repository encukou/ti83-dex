 DEFINE P0GREEN, SPACE=ROM  
 SEGMENT P0GREEN 

 public Green
 public DrawPkName
 extern Red
 extern Select
 extern Move
 extern LoadPic
 extern LPicBank1
 extern DrawMoveName
 extern DrawMoveName_AlignRight
 extern MoveList
 extern LoadLevelMoves
 extern LoadBreedMoves
 extern PokeScreen2
 extern GetAbilities

 public StartApp

 include "header.inc"

 db 0 ; Extra padding
 jp StartApp ; jump

 include "jumptable.inc"
 include "linemacros.inc"

;The app starts here
StartApp:
  ei
  set fullScrnDraw, (IY + apiFlg4)
  set showpkmn, (IY+dexstate)
  set graphDraw, (IY + graphFlags) ; Dirty the graph
  res apdRunning,(IY+apdFlags)		; Temporarily disable APD^tm (until GetKey is called => don't call GetKey)
  B_CALL DelRes		; invalidate statistics & make the StatVars RAM area usable
  call drawsplash

  ; setup code here
  call setupvars
  call setupdrawflags

  startbuffer

  ; splash bitmap
  ld hl,p_splash						; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,p_splash_end-p_splash		; put length to BC
  ldir				; load image to RAM
  ld hl,appBackUpScreen			; HL points to copied image
  ld de,14*256+40					; DE <- coords
  B_CALL DisplayImage

  ; btm bitmap
  ld hl,p_btm						; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,p_btm_end-p_btm		; put length to BC
  ldir				; load image to RAM
  ld hl,appBackUpScreen			; HL points to copied image
  ld de,46*256+0					; DE <- coords
  B_CALL DisplayImage

  ; splash text
  ld hl,s_splash						; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,s_splash_end-s_splash		; put length to BC
  ldir				; load string to RAM
  ;ld A,35
  ;ld (penCol),A
  ;ld A,7
  ;ld (penRow),A
  setpenpos 35,7
  ld B,6
  ld HL,appBackUpScreen
  B_CALL VPutSN

  ;ld A,37
  ;ld (penCol),A
  ;ld A,31
  ;ld (penRow),A
  setpenpos 37,31
  ld B,5
  ld HL,appBackUpScreen+6
  B_CALL VPutSN

  ;ld A,36
  ;ld (penCol),A
  ;ld A,37
  ;ld (penRow),A
  setpenpos 36,37
  ld B,6
  ld HL,appBackUpScreen+11
  B_CALL VPutSN

  AppOnErr splash_failed_to_get_x
  B_CALL RclX			; get our X
  B_CALL Int		; Round X
  B_CALL ClrOP1S	; Make X positive
  B_CALL StoX		; Store rounded index to X
  ld HL,385
  B_CALL SetXXXXOP2
  B_CALL CpOP1OP2
  jp z,splash_chimechox
  jp nc,splash_failed_to_get_x
 splash_chimechox:
  B_CALL ConvOP1	; put X to DE
  AppOffErr
  push de			; save our X
  B_CALL RclX		; get our X again
  xor a; A <- 0
  ld (penCol), a
  ld (penRow), a	; set pen location to (0,0)
  call DrawPkName

  ;ld A,10
  ;ld (penCol),A
  ;ld A,19
  ;ld (penRow),A
  setpenpos 10,19
  ld B,8
  ld HL,appBackUpScreen+17
  B_CALL VPutSN

  B_CALL RclX		; get our X again
  ld HL,353
  B_CALL SetXXXXOP2
  B_CALL CpOP1OP2
  jp z,splash_pboostx
  jp nc,splash_failed_to_get_x
 splash_pboostx:
  B_CALL ConvOP1	; put X to DE
  AppOffErr
 ; push de			; save our X
  B_CALL RclX		; get our X again
  ;ld a,47
  ;ld (penCol), a
  ;xor a; A <- 0
  ;ld (penRow), a	; set pen location to (0,0)
  setpenpos 47,0
  B_CALL DrawMoveName_AlignRight

  ;ld A,52
  ;ld (penCol),A
  ;ld A,19
  ;ld (penRow),A
  setpenpos 52,19
  ld B,5
  ld HL,appBackUpScreen+25
  B_CALL VPutSN


  splash_failed_to_get_x:
  endbuffer

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
  cp "1"
  jr z,waitkey_1
  cp "2"
  jr z,waitkey_2
  cp "3"
  jr z,waitkey_3
  cp "4"
  jr z,waitkey_4
  cp "5"
  jr z,waitkey_5
  cp "~"
  jr z,waitkey_quit
  jp waitkey

waitkey_1:	; Green
  jp Green
waitkey_2:	; Move
  B_JUMP Move
waitkey_3:	; Selest
  B_JUMP Select
waitkey_4:	; MList
  B_JUMP MoveList
waitkey_5:	; Plus
  jp StatCalculator
 ;jp waitkey
waitkey_quit:
  B_JUMP JForceCmdNoChar

keys:
 db  SFourSpaces				; (not used)
 db        "5123",0,0,0,0		; v<>^????
 db        ">",0,"____",0,0	; enter+-*/^clear-?
 db           0,"_____",0,0	; -369)tan-vars-?
 db           0,"______",0		; .258(cos-prgm-stat
 db 			  0,"______",0		; 0147,sin-apps-xt0n
 db        "~",0,"_____",0		; ?-store-ln-log-square-recip-math-alpha
 db            "54321",0,"~"	; graph-trace-zoom-window-y=-2nd-mode
 db 0	; del



Green:
 ; call drawsplash
  B_CALL GrBufClr
  set showpkmn, (IY+dexstate)

  AppOnErr failed_to_get_x
  B_CALL RclX			; get our X
  ld HL,385
  B_CALL SetXXXXOP2
  B_CALL CpOP1OP2
  jp z,chimechox
  jp nc,failed_to_get_x
 chimechox:

  startbuffer

  B_CALL ConvOP1	; put X to DE
  AppOffErr
  push de			; save our X
  B_CALL RclX		; get our X again
  xor a; A <- 0
  ld (penCol), a
  ld (penRow), a	; set pen location to (0,0)
  call DrawPkName

  ; National #
  pop de				; Get our X again
  push de			; And save it for later... Again...
  ;ld a, 0
  ;ld (penRow), a
  ;ld a, 82
  ;ld (penCol), a
  setpenpos 82,0
  ld hl, d_national
  call drawdw

  ; Hoenn #
  pop de				; Get our X again
  push de			; And save it for later... Again...
  ;ld a, 0
  ;ld (penRow), a
  ;ld a, 48
  ;ld (penCol), a
  setpenpos 48,0
  ld hl, d_hoenn
  call drawdw

  ; Johto #
  pop de				; Get our X again
  push de			; And save it for later... Again...
  ;ld a, 0
  ;ld (penRow), a
  ;ld a, 65
  ;ld (penCol), a
  setpenpos 65,0
  ld hl, d_johto
  call drawdw

  ; Types
  pop de				; Get our X again
  push de			; And save it for later... Again...
  call dotypes

  ; Stats
  	; HP
  pop de			; get X
  push de		; save for later
  ;ld a, 14
  ;ld (penRow), a
  ;ld a, 16
  ;ld (penCol), a
  setpenpos 16,14
  ld hl, d_hp
  call drawdb
  	; ATK
  pop de			; get X
  push de		; save for later
  ;ld a, 20
  ;ld (penRow), a
  ;ld a, 16
  ;ld (penCol), a
  setpenpos 16,20
  ld hl, d_atk
  call drawdb
  	; DEF
  pop de			; get X
  push de		; save for later
  ;ld a, 26
  ;ld (penRow), a
  ;ld a, 16
  ;ld (penCol), a
  setpenpos 16,26
  ld hl, d_def
  call drawdb
  	; SPD
  pop de			; get X
  push de		; save for later
  ;ld a, 32
  ;ld (penRow), a
  ;ld a, 16
  ;ld (penCol), a
  setpenpos 16,32
  ld hl, d_spd
  call drawdb
  	; SAT
  pop de			; get X
  push de		; save for later
  ;ld a, 38
  ;ld (penRow), a
  ;ld a, 16
  ;ld (penCol), a
  setpenpos 16,38
  ld hl, d_sat
  call drawdb
  	; SDE
  pop de			; get X
  push de		; save for later
  ;ld a, 44
  ;ld (penRow), a
  ;ld a, 16
  ;ld (penCol), a
  setpenpos 16,44
  ld hl, d_sde
  call drawdb

  ; HT
  pop de				; Get our X again
  push de			; And save it for later... Again...
  ;ld a, 8
  ;ld (penRow), a
  ;ld a, 68
  ;ld (penCol), a
  setpenpos 68,8
  ld hl,d_ht
  call getbyteaddress
  ld l,(hl)	; put byte to L
  ld h,0		; isolate L
  B_CALL SetXXXXOP2	; Put height to OP2
  B_CALL OP2ToOP1		; Put height to OP1
  ld a,10
  B_CALL SetXXOP2	; Put 100 to OP2
  B_CALL FPDiv			; Divide hieght/10 (to get m)
  ld a,5
  B_CALL DispOP1A

  ; WT
  pop de				; Get our X again
  push de			; And save it for later... Again...
  ;ld a, 14
  ;ld (penRow), a
  ;ld a, 68
  ;ld (penCol), a
  setpenpos 68,14
  ld hl, d_wt
  call getword	; put weight to DE
  ld h,d
  ld l,e
  B_CALL SetXXXXOP2	; Put weight to OP2
  B_CALL OP2ToOP1		; Put weight to OP1
  ld a,10
  B_CALL SetXXOP2	; Put 100 to OP2
  B_CALL FPDiv			; Divide wieght/10 (to get kg)
  ld a,5
  B_CALL DispOP1A

  ;EXP+Ratio
  ;ld a, 8
  ;ld (penRow), a
  ;ld a, 31
  ;ld (penCol), a
  setpenpos 31,8
  pop de				; Get our X again
  push de			; And save it for later... Again...
  ld hl, d_exp_ratio
  call getbyteaddress
  ld a,(hl)	; Put packed byte to A
  push af	; Save packed byte
  and a,15	; Get M:F ratio only
  sla a ; *2
  sla a ; *4
  sla a ; *8 - to get address offset
  ld hl,s_ratiotext
  add l
  jr nc,ratio_skip_inc
  inc h
 ratio_skip_inc:
  ld l,a		; HL contains address of ratio text
  B_CALL Mov9ToOP1
  ld hl, OP1
  ld b, 8
  B_CALL VPutSN		; Print ratio text
  ;ld a, 20
  ;ld (penRow), a
  ;ld a, 60
  ;ld (penCol), a
  setpenpos 60,20
  pop af		; Recall packed byte (now let's get EXP max)
  and a,15*16	; Get EXP mode only
  sra a ; /2 - to get address offset
  ld hl,s_exptext
  add l
  jr nc,exp_skip_inc
  inc h
 exp_skip_inc:
  ld l,a		; HL contains address of EXP mode text
  B_CALL Mov9ToOP1
  ld hl, OP1
  ld b, 8
  B_CALL VPutSN		; Print ratio text

  ;Breed groups
  ;ld a, 14
  ;ld (penRow), a
  ;ld a, 48
  ;ld (penCol), a
  setpenpos 48,14
  pop de				; Get our X again
  push de			; And save it for later... Again...
  ld hl, d_breed
  call getbyteaddress
  ld a,(hl)	; Put packed byte to A
  push af	; Save packed byte
  and a,15	; Get second number only
  ld hl,s_breed
  add l
  jr nc,breed_skip_inc_1
  inc h
 breed_skip_inc_1:
  ld l,a		; HL contains address of char
  ld a, (hl)
  B_CALL VPutMap	; Put char
  ;ld a, 14
  ;ld (penRow), a
  ;ld a, 40
  ;ld (penCol), a
  setpenpos 40,14
  pop af		; Recall packed byte (now let's get EXP max)
  and a,15*16	; Get EXP mode only
  srl a ; /2
  srl a ; /4
  srl a ; /8
  srl a ; /16 - to get the number
  ld hl,s_breed
  add l
  jr nc,breed_skip_inc_0
  inc h
 breed_skip_inc_0:
  ld l,a		; HL contains address of char
  ld a, (hl)
  B_CALL VPutMap	; Put char

  call drawambients

  pop de				; Retrieve our X again...
  ; X is left in DE for anoter page
  B_JUMP Red		; Go to te Red data page

DrawPkName:
  ld a,10
  B_CALL SetXXOP2	; set OP2 to 10
  B_CALL FPMult	; multiply OP1 by 10 (OP2)
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
  ld bc, 10			; put 10 (length) to BC
  ldir				; load pk.name to OP1
  ld hl, OP1		; Put address of OP1 to HL
  ld b, 10			; put 10 (length) to BC
  B_CALL VPutSN	; Draw string... finally!
  ret

drawsplash:
  B_CALL ForceFullScreen  
 ; B_CALL GrBufCpy  
  B_CALL ClrLCDFull ; clear screen
  B_CALL GrBufClr   ; clear buffer
  ret

setupdrawflags:
  res fracDrawLFont, (IY + fontFlags) 
  set textEraseBelow, (IY + textFlags) 
  res textInverse, (IY + textFlags) 
  set fullScrnDraw, (IY + apiFlg4)		; don't skip
  res textWrite, (IY + sGrFlags) 
  res bufferOnly,   (IY + plotFlag3)	; draw to buffer
  res plotLoc,      (IY + plotFlags)	; copy to buffer
  ret

setupvars:
 ; ld hl, ourvars
 ; ld bc, 768 ;	number of bytes in "our vars"
 ; B_CALL MemClear
 ; ld a, mmList	; set mode of Window 0 to List
 ; ld (wmod0), a
  ret

drawdw:
  ; Inputs:	de - offset
  ;			hl - address
  ;			(penRow), (penCol) - pen row & col
  call getword
  ld h, d			; Move # from De to Hl
  ld l, e			; Move # from dE to hL
  B_CALL SetXXXXOP2	; Put HL to OP2
  B_CALL OP2ToOP1	; Put OP2 to OP1
  ld a,3		; a=3 (# if digits)
  B_CALL DispOP1A	; Display the number (finally!)
  ret

getword:
  ; Inputs:	de - offset
  ;			hl - address
  ; Output: DE
  add HL,DE
  add HL,DE
  ld e,(hl)	; put what HL points to to DE
  inc hl
  ld d,(hl)
  ret

dotypes
  ; Display the 2 types; X is in DE
  ld hl, d_types	; put address to hl
  add hl,de
  add hl,de
  push hl	; save the address of the first type byte

  ld a, 8			; Setup pen
  ld (penRow), a
  ld a, 0
  ld (penCol), a
  call dotypes_writetype

  pop hl				; Recall the first type byte
  inc hl
  push hl			; save the address of the second type byte
  ld a, 16		; Adjust pen
  ld (penCol), a
  call dotypes_writetype

  pop hl				; Recall the second type byte
  ld a,(hl)	; put 2nd move byte to A
  cp 0
  jr z,dotypes_return	; return if second type=0
  ; else print a "/" between the types
  ld a, 12		; Adjust pen
  ld (penCol), a

  ld a,"/"			; setup char
  B_CALL VPutMap	; write char

 dotypes_return:
  ret

dotypes_writetype:
  ld e,(hl)	; put what HL points to to DE
  inc hl
  ld d,(hl)
  ld h, d			; Move # from De to Hl
  ld l, e			; Move # from dE to hL
  ld h, 0
  ; HL now contains the type byte
  ld a,l	; Multiply l by 3	;
  sla l	;	;	;	;	;	;	;
  add l	;	;	;	;	;	;	;
  ld l,a	;	;	;	;	;	;	;
  ld de,s_types
  add hl,de			; Now, HL contains te address of the type string
  ld de,OP1
  ld bc,3
  ldir			; type string copied to OP1
  ld hl, OP1		; string address
  ld b,3				; string length
  B_CALL VPutSN		; Draw the string
  ret

drawdb:
  call getbyteaddress
 draw_byte_from_address:
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

getbyteaddress:
  ; Inputs:	de - X
  ;			hl - starting address
  ; Output: HL (address of stat)
  ld a,l
  add e
  jr nc,drawdb_skipinc
  inc h
 drawdb_skipinc:
  ld l,a
  ld a,h
  add d
  ld h,a	; Now, HL contains address+X (address of individual byte)
  ret

drawambients:
  ; Line
  ld h,1	; dark line
  ld bc,56
  ld de,93*256+56	; top line
  B_CALL ILine
  ld bc,94*256
  ld de,94*256+55	; right line
  B_CALL ILine
  ld bc,62*256
  ld de,62*256+29	; left pic line
  B_CALL ILine
  ld bc,63*256+30
  ld de,93*256+30	; top pic line
  B_CALL ILine

  ;xor a
  ;ld (penRow), a
  ;ld a, 44
  ;ld (penCol), a
  setpenpos 44,0
  ld a,"H"
  B_CALL VPutMap		; H
  ld a, 61
  ld (penCol), a
  ld a,"J"
  B_CALL VPutMap		; J
  ld a, 78
  ld (penCol), a
  ld a,"N"
  B_CALL VPutMap		; N
  ;ld a, 8
  ;ld (penRow), a
  ;ld a, 60
  ;ld (penCol), a
  setpenpos 60,8
  ld a,"H"
  B_CALL VPutMap		; H
  ld a, 88
  ld (penCol), a
  ld a,"M"
  B_CALL VPutMap		; M
  ;ld a, 14
  ;ld (penRow), a
  ;ld a, 60
  ;ld (penCol), a
  setpenpos 60,14
  ld a,"W"
  B_CALL VPutMap		; W
  ld a, 88
  ld (penCol), a
  ld a,"K"
  B_CALL VPutMap		; K
  ;ld a, 20
  ;ld (penRow), a
  ;ld a, 56
  ;ld (penCol), a
  setpenpos 56,20
  ld a,"X"
  B_CALL VPutMap		; X
  ld a,"P"
  B_CALL VPutMap		; P
  ;ld a, 14
  ;ld (penRow), a
  ;ld a, 44
  ;ld (penCol), a
  setpenpos 44,14
  ld a,"-"
 ; B_CALL VPutMap		; -
 ; ld hl,s_static2
 ; B_CALL Mov9ToOP1	; load -(-)>TOC
 ; ld a, 44
 ; ld (penRow), a
 ; ld a, 41
 ; ld (penCol), a
 ; ld hl, OP1
 ; ld b, 7
 ; B_CALL VPutSN		; (-)>TOC
 ; ld hl,s_static3
 ; B_CALL Mov9ToOP1	; load GET HAPPY
 ; ld a, 32
 ; ld (penRow), a
 ; ld a, 40
 ; ld (penCol), a
 ; ld hl, OP1
 ; ld b, 9
 ; B_CALL VPutSN		; GET HAPPY
 ; ld hl,s_static4
 ; B_CALL Mov9ToOP1	; load 2005
 ; ld a, 56
 ; ld (penRow), a
 ; ld a, 54
 ; ld (penCol), a
 ; ld hl, OP1
 ; ld b, 4
 ; B_CALL VPutSN		; 2005

  ld hl,s_hp
  B_CALL Mov9ToOP1	; HP=ATK=
  ;ld a, 4
  ;ld (penCol), a
  ;ld a, 14
  ;ld (penRow), a
  setpenpos 4,14
  ld hl, OP1
  ld b, 3
  B_CALL VPutSN		; HP=
  ;xor a
  ;ld (penCol), a
  ;ld a, 20
  ;ld (penRow), a
  setpenpos 0,20
  ld hl, OP1+3
  ld b, 4
  B_CALL VPutSN		; ATK=

  ld hl,s_def
  B_CALL Mov9ToOP1	; DEF=SPD=
  ;xor a
  ;ld (penCol), a
  ;ld a, 26
  ;ld (penRow), a
  setpenpos 0,26
  ld hl, OP1
  ld b, 4
  B_CALL VPutSN		; DEF=
  ;xor a
  ;ld (penCol), a
  ;ld a, 32
  ;ld (penRow), a
  setpenpos 0,32
  ld hl, OP1+4
  ld b, 4
  B_CALL VPutSN		; SPD=

  ld hl,s_sat
  B_CALL Mov9ToOP1	; SAT=SDE=
  ;xor a
  ;ld (penCol), a
  ;ld a, 38
  ;ld (penRow), a
  setpenpos 0,38
  ld hl, OP1
  ld b, 4
  B_CALL VPutSN		; SAT=
  ;xor a
  ;ld (penCol), a
  ;ld a, 44
  ;ld (penRow), a
  setpenpos 0,44
  ld hl, OP1+4
  ld b, 4
  B_CALL VPutSN		; SDE=

 ret


failed_to_get_x:
  B_JUMP Select

;
; Data section
;
staticstrings:
s_hp:	db "HP="
s_atk:db "ATK="
s_def:db "DEF="
s_spd:db "SPD="
s_sat:db "SAT="
s_sde:db "SDE="
;s_static2:	db "(-)",Sstore,"TOC"
				; 2 more byte can be put here
;s_static3:	db "GET",SFourSpaces,"HAPPY"
;s_static4:	db "2005"
				; 5 more bytes can be put here

s_types:
 db "   ","NOR","FIR","WAT","ELE","GRA","ICE","FIG","POI"
 db "GRO","FLY","PSY","BUG","ROC","GHO","DRA","DAR","STE"

s_exptext:
 ; Padded to 8 bytes
 db "=600000 "	; 0
 db "=800000 "	; 1
 db "=1000000"	; 2
 db "=1059860"	; 3
 db "=1250000"	; 4
 db "=1640000"	; 5

s_ratiotext:
 ; Padded to 8 bytes
 db "   ^0/0+"; 0
 db " ^100/0+"; 1
 db " ^88/12+"; 2
 db " ^75/25+"; 3
 db " ^50/50+"; 4
 db " ^25/75+"; 5
 db " ^12/88+"; 6
 db " ^0/100+"; 7

s_breed:
 db "0123456789ABCDEX"

 ; names
d_names:
 DB "BULBASAUR IVYSAUR   VENUSAUR  CHARMANDERCHARMELEONCHARIZARD SQUIRTLE  WARTORTLE BLASTOISE CATERPIE  "
 DB "METAPOD   BUTTERFREEWEEDLE    KAKUNA    BEEDRILL  PIDGEY    PIDGEOTTO PIDGEOT   RATTATA   RATICATE  "
 DB "SPEAROW   FEAROW    EKANS     ARBOK     PIKACHU   RAICHU    SANDSHREW SANDSLASH NIDORAN + NIDORINA  "
 DB "NIDOQUEEN NIDORAN ^ NIDORINO  NIDOKING  CLEFAIRY  CLEFABLE  VULPIX    NINETALES JIGGLYPUFFWIGGLYTUFF"
 DB "ZUBAT     GOLBAT    ODDISH    GLOOM     VILEPLUME PARAS     PARASECT  VENONAT   VENOMOTH  DIGLETT   "
 DB "DUGTRIO   MEOWTH    PERSIAN   PSYDUCK   GOLDUCK   MANKEY    PRIMEAPE  GROWLITHE ARCANINE  POLIWAG   "
 DB "POLIWHIRL POLIWRATH ABRA      KADABRA   ALAKAZAM  MACHOP    MACHOKE   MACHAMP   BELLSPROUTWEEPINBELL"
 DB "VICTREEBELTENTACOOL TENTACRUELGEODUDE   GRAVELER  GOLEM     PONYTA    RAPIDASH  SLOWPOKE  SLOWBRO   "
 DB "MAGNEMITE MAGNETON  FARFETCH'DDODUO     DODRIO    SEEL      DEWGONG   GRIMER    MUK       SHELLDER  "
 DB "CLOYSTER  GASTLY    HAUNTER   GENGAR    ONIX      DROWZEE   HYPNO     KRABBY    KINGLER   VOLTORB   "
 DB "ELECTRODE EXEGGCUTE EXEGGUTOR CUBONE    MAROWAK   HITMONLEE HITMONCHANLICKITUNG KOFFING   WEEZING   "
 DB "RHYHORN   RHYDON    CHANSEY   TANGELA   KANGASKHANHORSEA    SEADRA    GOLDEEN   SEAKING   STARYU    "
 DB "STARMIE   MR. MIME  SCYTHER   JYNX      ELECTABUZZMAGMAR    PINSIR    TAUROS    MAGIKARP  GYARADOS  "
 DB "LAPRAS    DITTO     EEVEE     VAPOREON  JOLTEON   FLAREON   PORYGON   OMANYTE   OMASTAR   KABUTO    "
 DB "KABUTOPS  AERODACTYLSNORLAX   ARTICUNO  ZAPDOS    MOLTRES   DRATINI   DRAGONAIR DRAGONITE MEWTWO    "
 DB "MEW       CHIKORITA BAYLEEF   MEGANIUM  CYNDAQUIL QUILAVA   TYPHLOSIONTOTODILE  CROCONAW  FERALIGATR"
 DB "SENTRET   FURRET    HOOTHOOT  NOCTOWL   LEDYBA    LEDIAN    SPINARAK  ARIADOS   CROBAT    CHINCHOU  "
 DB "LANTURN   PICHU     CLEFFA    IGGLYBUFF TOGEPI    TOGETIC   NATU      XATU      MAREEP    FLAAFFY   "
 DB "AMPHAROS  BELLOSSOM MARILL    AZUMARILL SUDOWOODO POLITOED  HOPPIP    SKIPLOOM  JUMPLUFF  AIPOM     "
 DB "SUNKERN   SUNFLORA  YANMA     WOOPER    QUAGSIRE  ESPEON    UMBREON   MURKROW   SLOWKING  MISDREAVUS"
 DB "UNOWN     WOBBUFFET GIRAFARIG PINECO    FORRETRESSDUNSPARCE GLIGAR    STEELIX   SNUBBULL  GRANBULL  "
 DB "QWILFISH  SCIZOR    SHUCKLE   HERACROSS SNEASEL   TEDDIURSA URSARING  SLUGMA    MAGCARGO  SWINUB    "
 DB "PILOSWINE CORSOLA   REMORAID  OCTILLERY DELIBIRD  MANTINE   SKARMORY  HOUNDOUR  HOUNDOOM  KINGDRA   "
 DB "PHANPY    DONPHAN   PORYGON2  STANTLER  SMEARGLE  TYROGUE   HITMONTOP SMOOCHUM  ELEKID    MAGBY     "
 DB "MILTANK   BLISSEY   RAIKOU    ENTEI     SUICUNE   LARVITAR  PUPITAR   TYRANITAR LUGIA     HO-OH     "
 DB "CELEBI    TREECKO   GROVYLE   SCEPTILE  TORCHIC   COMBUSKEN BLAZIKEN  MUDKIP    MARSHTOMP SWAMPERT  "
 DB "POOCHYENA MIGHTYENA ZIGZAGOON LINOONE   WURMPLE   SILCOON   BEAUTIFLY CASCOON   DUSTOX    LOTAD     "
 DB "LOMBRE    LUDICOLO  SEEDOT    NUZLEAF   SHIFTRY   NINCADA   NINJASK   SHEDINJA  TAILLOW   SWELLOW   "
 DB "SHROOMISH BRELOOM   SPINDA    WINGULL   PELIPPER  SURSKIT   MASQUERAINWAILMER   WAILORD   SKITTY    "
 DB "DELCATTY  KECLEON   BALTOY    CLAYDOL   NOSEPASS  TORKOAL   SABLEYE   BARBOACH  WHISCASH  LUVDISC   "
 DB "CORPHISH  CRAWDAUNT FEEBAS    MILOTIC   CARVANHA  SHARPEDO  TRAPINCH  VIBRAVA   FLYGON    MAKUHITA  "
 DB "HARIYAMA  ELECTRIKE MANECTRIC NUMEL     CAMERUPT  SPHEAL    SEALEO    WALREIN   CACNEA    CACTURNE  "
 DB "SNORUNT   GLALIE    LUNATONE  SOLROCK   AZURILL   SPOINK    GRUMPIG   PLUSLE    MINUN     MAWILE    "
 DB "MEDITITE  MEDICHAM  SWABLU    ALTARIA   WYNAUT    DUSKULL   DUSCLOPS  ROSELIA   SLAKOTH   VIGOROTH  "
 DB "SLAKING   GULPIN    SWALOT    TROPIUS   WHISMUR   LOUDRED   EXPLOUD   CLAMPERL  HUNTAIL   GOREBYSS  "
 DB "ABSOL     SHUPPET   BANETTE   SEVIPER   ZANGOOSE  RELICANTH ARON      LAIRON    AGGRON    CASTFORM  "
 DB "VOLBEAT   ILLUMISE  LILEEP    CRADILY   ANORITH   ARMALDO   RALTS     KIRLIA    GARDEVOIR BAGON     "
 DB "SHELGON   SALAMENCE BELDUM    METANG    METAGROSS REGIROCK  REGICE    REGISTEEL KYOGRE    GROUDON   "
 DB "RAYQUAZA  LATIAS    LATIOS    JIRACHI   DEOXYS    CHIMECHO  "

d_national:
 dw 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28
 dw 29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53
 dw 54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78
 dw 79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102
 dw 103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120
 dw 121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138
 dw 139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156
 dw 157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174
 dw 175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192
 dw 193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210
 dw 211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228
 dw 229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246
 dw 247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264
 dw 265,266,267,268,269,270,271,272,273,274,275,290,291,292,276,277,285,286
 dw 327,278,279,283,284,320,321,300,301,352,343,344,299,324,302,339,340,370
 dw 341,342,349,350,318,319,328,329,330,296,297,309,310,322,323,363,364,365
 dw 331,332,361,362,337,338,298,325,326,311,312,303,307,308,333,334,360,355
 dw 356,315,287,288,289,316,317,357,293,294,295,366,367,368,359,353,354,336
 dw 335,369,304,305,306,351,313,314,345,346,347,348,280,281,282,371,372,373
 dw 374,375,376,377,378,379,382,383,384,380,381,385,386,358

d_hoenn:
 dw 203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220
 dw 221,222,223,224,225,226,156,157,112,113,227,228,229,230,231,232,233,234
 dw 153,154,138,139,63,64,88,89,90,235,236,237,238,239,240,241,242,158,159
 dw 243,244,245,246,247,248,249,39,40,41,73,74,75,250,251,252,66,67,57,58,59
 dw 253,254,255,256,82,83,257,92,93,258,259,106,107,260,261,262,263,264,265
 dw 266,267,268,269,84,85,270,271,272,273,274,275,276,108,109,169,170,277,278
 dw 279,184,185,50,51,143,144,280,281,282,283,284,167,285,52,53,286,287,288
 dw 289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306
 dw 307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,65,181
 dw 182,155,324,137,325,326,162,163,327,328,329,91,55,56,330,331,332,333,334
 dw 335,336,337,338,339,340,341,342,343,344,345,346,161,164,347,348,349,350
 dw 351,352,353,354,355,356,168,357,358,359,103,104,360,361,180,362,363,364
 dw 365,115,366,367,186,165,166,368,369,370,371,372,373,374,375,376,377,378
 dw 379,380,381,382,383,384,385,386,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17
 dw 18,19,20,21,22,23,24,42,43,44,25,26,34,35,114,27,28,32,33,99,100,61,62
 dw 145,131,132,60,105,68,127,128,183,129,130,140,141,97,98,116,117,118,48,49
 dw 78,79,101,102,173,174,175,119,120,171,172,125,126,54,110,111,80,81,69,76
 dw 77,121,122,160,148,149,94,36,37,38,95,96,150,45,46,47,176,177,178,152,146
 dw 147,124,123,179,70,71,72,142,86,87,133,134,135,136,29,30,31,187,188,189
 dw 190,191,192,193,194,195,198,199,200,196,197,201,202,151

d_johto:
 dw 226,227,228,229,230,231,232,233,234,24,25,26,27,28,29,10,11,12,17,18,13,14
 dw 50,51,22,23,48,49,95,96,97,98,99,100,41,42,125,126,44,45,37,38,83,84,85,70
 dw 71,108,109,132,133,136,137,138,139,134,135,127,128,72,73,74,89,90,91,140
 dw 141,142,64,65,66,162,163,34,35,36,201,202,80,81,118,119,158,199,200,176,177
 dw 116,117,169,170,58,59,60,62,87,88,164,165,120,121,104,105,203,204,144,145
 dw 178,114,115,206,207,217,179,205,186,187,78,79,167,168,156,110,153,155,151
 dw 112,148,76,77,219,92,180,181,182,183,215,220,221,222,223,224,225,235,236
 dw 237,241,242,243,249,250,1,2,3,4,5,6,7,8,9,19,20,15,16,30,31,32,33,39,174
 dw 175,21,40,43,46,47,159,160,53,54,55,86,130,131,106,75,67,68,69,122,102,103
 dw 101,56,57,184,185,208,82,214,61,107,147,93,94,52,189,63,123,124,161,111
 dw 166,113,213,193,194,211,212,191,192,171,172,173,190,197,198,209,210,188
 dw 195,196,216,129,157,143,146,152,154,150,149,218,238,239,240,244,245,246
 dw 247,248,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266
 dw 267,268,269,270,271,272,273,274,275,290,291,292,276,277,285,286,327,278
 dw 279,283,284,320,321,300,301,352,343,344,299,324,302,339,340,370,341,342
 dw 349,350,318,319,328,329,330,296,297,309,310,322,323,363,364,365,331,332
 dw 361,362,337,338,298,325,326,311,312,303,307,308,333,334,360,355,356,315
 dw 287,288,289,316,317,357,293,294,295,366,367,368,359,353,354,336,335,369
 dw 304,305,306,351,313,314,345,346,347,348,280,281,282,371,372,373,374,375
 dw 376,377,378,379,382,383,384,380,381,385,386,358

d_types:
 db 5,8,5,8,5,8,2,0,2,0,2,10,3,0,3,0,3,0,12,0,12,0,12,10,12,8,12,8,12,8,1,10
 db 1,10,1,10,1,0,1,0,1,10,1,10,8,0,8,0,4,0,4,0,9,0,9,0,8,0,8,0,8,9,8,0,8,0
 db 8,9,1,0,1,0,2,0,2,0,1,0,1,0,8,10,8,10,5,8,5,8,5,8,12,5,12,5,12,8,12,8,9,0
 db 9,0,1,0,1,0,3,0,3,0,7,0,7,0,2,0,2,0,3,0,3,0,3,7,11,0,11,0,11,0,7,0,7,0,7,0
 db 5,8,5,8,5,8,3,8,3,8,13,9,13,9,13,9,2,0,2,0,3,11,3,11,4,17,4,17,1,10,1,10
 db 1,10,3,0,3,6,8,0,8,0,3,0,3,6,14,8,14,8,14,8,13,9,11,0,11,0,3,0,3,0,4,0,4,0
 db 5,11,5,11,9,0,9,0,7,0,7,0,1,0,8,0,8,0,9,13,9,13,1,0,5,0,1,0,3,0,3,0,3,0
 db 3,0,3,0,3,11,11,0,12,10,6,11,4,0,2,0,12,0,1,0,3,0,3,10,3,6,1,0,1,0,3,0,4,0
 db 2,0,1,0,13,3,13,3,13,3,13,3,13,10,1,0,6,10,4,10,2,10,15,0,15,0,15,10,11,0
 db 11,0,5,0,5,0,5,0,2,0,2,0,2,0,3,0,3,0,3,0,1,0,1,0,1,10,1,10,12,10,12,10
 db 12,8,12,8,8,10,3,4,3,4,4,0,1,0,1,0,1,0,1,10,11,10,11,10,4,0,4,0,4,0,5,0,3,0
 db 3,0,13,0,3,0,5,10,5,10,5,10,1,0,5,0,5,0,12,10,3,9,3,9,11,0,16,0,16,10,3,11
 db 14,0,11,0,11,0,1,11,12,0,12,17,1,0,9,10,17,9,1,0,1,0,3,8,12,17,12,13,12,7
 db 16,6,1,0,1,0,2,0,2,13,6,9,6,9,3,13,3,0,3,0,6,10,3,10,17,10,16,2,16,2,3,15
 db 9,0,9,0,1,0,1,0,1,0,7,0,7,0,6,11,4,0,2,0,1,0,1,0,4,0,2,0,3,0,13,9,13,9
 db 13,16,11,10,2,10,11,5,5,0,5,0,5,0,2,0,2,7,2,7,3,0,3,9,3,9,16,0,16,0,1,0
 db 1,0,12,0,12,0,12,10,12,0,12,8,3,5,3,5,3,5,5,0,5,16,5,16,12,9,12,10,12,14
 db 1,10,1,10,5,0,5,7,1,0,3,10,3,10,12,3,12,10,3,0,3,0,1,0,1,0,1,0,9,11,9,11
 db 13,0,2,0,16,14,3,9,3,9,3,0,3,0,3,16,3,0,3,0,3,16,3,16,9,0,9,15,9,15,7,0
 db 7,0,4,0,4,0,2,9,2,9,6,3,6,3,6,3,5,0,5,16,6,0,6,0,13,11,13,11,1,0,11,0,11,0
 db 4,0,4,0,17,0,7,11,7,11,1,10,15,10,11,0,14,0,14,0,5,8,1,0,1,0,1,0,8,0,8,0
 db 5,10,1,0,1,0,1,0,3,0,3,0,3,0,16,0,14,0,14,0,8,0,1,0,3,13,17,13,17,13,17,13
 db 1,0,12,0,12,0,13,5,13,5,13,12,13,12,11,0,11,0,11,0,15,0,15,0,15,10,17,11
 db 17,11,17,11,13,0,6,0,17,0,3,0,9,0,15,10,15,11,15,11,17,11,11,0,11,0

d_hp:		;386
 db 45,60,80,39,58,78,44,59,79,45,50,60,40,45,65,40,63,83,30,55,40,65,35,60,35,60
 db 50,75,55,70,90,46,61,81,70,95,38,73,115,140,40,75,45,60,75,35,60,60,70,10,35
 db 40,65,50,80,40,65,55,90,40,65,90,25,40,55,70,80,90,50,65,80,40,80,40,55,80,50
 db 65,90,95,25,50,52,35,60,65,90,80,105,30,50,30,45,60,35,60,85,30,55,40,60,60
 db 95,50,60,50,50,90,40,65,80,105,250,65,105,30,55,45,80,30,60,40,70,65,65,65,65
 db 75,20,95,130,48,55,130,65,65,65,35,70,30,60,80,160,90,90,90,41,61,91,106,100
 db 45,60,80,39,58,78,50,65,85,35,85,60,100,40,55,40,70,85,75,125,20,50,90,35,55
 db 40,65,55,70,90,75,70,100,70,90,35,55,75,55,30,75,65,55,95,65,95,60,95,60,48
 db 190,70,50,75,100,65,75,60,90,65,70,20,80,55,60,90,40,50,50,100,55,35,75,45,65
 db 65,45,75,75,90,90,85,73,55,35,50,45,45,45,95,255,90,115,100,50,70,100,106,106
 db 100,40,50,70,45,60,80,50,70,100,35,70,38,78,45,50,60,50,60,40,60,80,40,70,90
 db 31,61,0,40,60,60,60,60,40,60,40,70,130,170,50,70,60,40,60,30,70,50,50,110,43
 db 43,63,20,95,45,70,45,50,80,72,144,40,70,60,70,70,90,110,50,70,50,80,70,70,50
 db 60,80,60,60,50,30,60,45,75,95,20,40,50,60,80,150,70,100,99,64,84,104,35,55,55
 db 65,44,64,73,73,100,50,60,70,70,65,65,66,86,45,75,28,38,68,45,65,95,40,60,80
 db 80,80,80,100,100,105,80,80,100,50,65
d_atk:	;386
 db 49,62,82,52,64,84,48,63,83,30,20,45,35,25,80,45,60,80,56,81,60,90,60,85,55,90
 db 75,100,47,62,82,57,72,92,45,70,41,76,45,70,45,80,50,65,80,70,95,55,65,55,80
 db 45,70,52,82,80,105,70,110,50,65,85,20,35,50,80,100,130,75,90,105,40,70,80,95
 db 110,85,100,65,75,35,60,65,85,110,45,70,80,105,65,95,35,50,65,45,48,73,105,130
 db 30,50,40,95,50,80,120,105,55,65,90,85,130,5,55,95,40,65,67,92,45,75,45,110,50
 db 83,95,125,100,10,125,85,48,55,65,65,130,60,40,60,80,115,105,110,85,90,100,64
 db 84,134,110,100,49,62,82,52,64,84,65,80,105,46,76,30,50,20,35,60,90,90,38,58
 db 40,25,30,20,40,50,75,40,55,75,80,20,50,100,75,35,45,55,70,30,75,65,45,85,65
 db 65,85,75,60,72,33,80,65,90,70,75,85,80,120,95,130,10,125,95,80,130,40,50,50
 db 100,55,65,105,55,40,80,60,90,95,60,120,80,95,20,35,95,30,63,75,80,10,85,115
 db 75,64,84,134,90,130,100,45,65,85,60,85,120,70,85,110,55,90,30,70,45,35,70,35
 db 50,30,50,70,40,70,100,45,90,90,55,85,40,130,60,30,50,30,60,70,90,45,65,90,40
 db 70,45,85,75,48,78,30,80,120,15,60,90,120,100,70,100,60,120,45,75,60,100,40,60
 db 80,85,115,50,80,55,95,20,25,45,50,40,85,40,60,40,70,23,40,70,60,60,80,160,43
 db 73,68,51,71,91,64,104,84,130,75,115,100,115,90,70,90,110,70,73,47,41,81,95
 db 125,25,35,65,75,95,135,55,75,135,100,50,75,100,150,150,80,90,100,150,50
d_def:	;386
 db 49,63,83,43,58,78,65,80,100,35,55,50,30,50,40,40,55,75,35,60,30,65,44,69,30
 db 55,85,110,52,67,87,40,57,77,48,73,40,75,20,45,35,70,55,70,85,55,80,50,60,25
 db 50,35,60,48,78,35,60,45,80,40,65,95,15,30,45,50,70,80,35,50,65,35,65,100,115
 db 130,55,70,65,110,70,95,55,45,70,55,80,50,75,100,180,30,45,60,160,45,70,90,115
 db 50,70,80,85,95,110,53,79,75,95,120,95,120,5,115,80,70,95,60,65,55,85,65,80,35
 db 57,57,100,95,55,79,80,48,50,60,60,60,70,100,125,90,105,65,65,100,85,90,45,65
 db 95,90,100,65,80,100,43,58,78,64,80,100,34,64,30,50,30,50,40,70,80,38,58,15,28
 db 15,65,85,45,70,40,55,75,85,50,80,115,75,40,50,70,55,30,55,45,45,85,60,110,42
 db 80,60,48,58,65,90,140,70,105,200,50,75,75,100,230,75,55,50,75,40,120,40,80,85
 db 35,75,45,70,140,30,50,95,60,120,90,62,35,35,95,15,37,37,105,10,75,85,115,50
 db 70,110,130,90,100,35,45,65,40,60,70,50,70,90,35,70,41,61,35,55,50,55,70,30,50
 db 70,50,40,60,90,45,45,30,60,60,80,60,30,100,32,62,35,45,45,65,70,55,105,135
 db 140,75,43,73,55,65,85,20,79,20,40,45,50,80,30,60,40,60,40,70,50,70,90,40,60
 db 50,80,65,85,40,35,65,40,50,85,55,75,60,90,48,90,130,45,60,80,100,53,83,83,23
 db 43,63,85,105,105,60,35,65,60,60,130,100,140,180,70,55,55,77,97,50,100,25,35
 db 65,60,100,80,80,100,130,200,100,150,90,140,90,90,80,100,50,70
d_spd:	;386
 db 45,60,80,65,80,100,43,58,78,45,30,70,50,35,75,56,71,91,72,97,70,100,55,80,90
 db 100,40,65,41,56,76,50,65,85,35,60,65,100,20,45,55,90,30,40,50,25,30,45,90,95
 db 120,90,115,55,85,70,95,60,95,90,90,70,90,105,120,35,45,55,40,55,70,70,100,20
 db 35,45,90,105,15,30,45,70,60,75,100,45,70,25,50,40,70,80,95,110,70,42,67,50,75
 db 100,140,40,55,35,45,87,76,30,35,60,25,40,50,60,90,60,85,63,68,85,115,90,105
 db 95,105,93,85,110,80,81,60,48,55,65,130,65,40,35,55,55,80,130,30,85,100,90,50
 db 70,80,130,100,45,60,80,65,80,100,43,58,78,20,90,50,70,55,85,30,40,130,67,67
 db 60,15,15,20,40,70,95,35,45,55,50,40,50,30,70,50,80,110,85,30,30,95,15,35,110
 db 65,91,30,85,48,33,85,15,40,45,85,30,30,45,85,65,5,85,115,40,55,20,30,50,50,35
 db 65,45,75,70,70,65,95,85,40,50,60,85,75,35,70,65,95,83,100,55,115,100,85,41,51
 db 61,110,90,100,70,95,120,45,55,80,40,50,60,35,70,60,100,20,15,65,15,65,30,50
 db 70,30,60,80,40,160,40,85,125,35,70,60,85,65,65,60,60,60,50,70,40,55,75,30,20
 db 50,60,60,97,35,55,80,81,65,95,10,70,100,25,50,65,105,35,40,25,45,65,35,55,50
 db 80,70,70,20,60,80,95,95,50,60,80,50,80,23,25,25,65,30,90,100,40,55,51,28,48
 db 68,32,52,52,75,45,65,65,90,55,30,40,50,70,85,85,23,43,75,45,40,50,80,50,50
 db 100,30,50,70,50,50,50,90,90,95,110,110,100,150,65
d_sat:	;386
 db 65,80,100,60,80,109,50,65,85,20,25,80,20,25,45,35,50,70,25,50,31,61,40,65,50
 db 90,20,45,40,55,75,40,55,85,60,85,50,81,45,75,30,65,75,85,100,45,60,40,90,35
 db 50,40,65,65,95,35,60,70,100,40,50,70,105,120,135,35,50,65,70,85,100,50,80,30
 db 45,55,65,80,40,100,95,120,58,35,60,45,70,40,65,45,85,100,115,130,30,43,73,25
 db 50,55,80,60,125,40,50,35,35,60,60,85,30,45,35,100,40,70,95,35,65,70,100,100
 db 55,115,95,100,55,40,15,60,85,48,45,110,110,95,85,90,115,55,65,60,65,95,125
 db 125,50,70,100,154,100,49,63,83,60,80,109,44,59,79,35,45,36,76,40,55,40,60,70
 db 56,76,35,45,40,40,80,70,95,65,80,115,90,20,50,30,90,35,45,55,40,30,105,75,25
 db 65,130,60,85,100,85,72,33,90,35,60,65,35,55,40,60,55,55,10,40,35,50,75,70,80
 db 30,60,65,65,105,65,80,40,80,110,95,40,60,105,85,20,35,35,85,65,70,40,75,115
 db 90,90,45,65,95,90,110,100,65,85,105,70,85,110,50,60,85,30,60,30,50,20,25,90
 db 25,50,40,60,90,30,60,90,30,50,30,30,50,40,60,60,55,85,50,80,70,90,35,55,60,40
 db 70,45,85,65,46,76,40,50,90,10,100,65,95,45,50,80,20,40,65,105,65,105,55,75,95
 db 85,115,50,80,95,55,20,70,90,85,75,55,40,60,40,70,23,30,60,100,35,55,95,43,73
 db 72,51,71,91,74,94,114,75,63,83,100,60,45,40,50,60,70,47,73,61,81,40,70,45,65
 db 125,40,60,110,35,55,95,50,100,75,150,100,150,110,130,100,150,95
d_sde:	;386
 db 65,80,100,50,65,85,64,80,105,20,25,80,20,25,80,35,50,70,35,70,31,61,54,79,40
 db 80,30,55,40,55,85,40,55,75,65,90,65,100,25,50,40,75,65,75,90,55,80,55,75,45
 db 70,40,65,50,80,45,70,50,80,40,50,90,55,70,85,35,60,85,30,45,60,100,120,30,45
 db 65,65,80,40,80,55,70,62,35,60,70,95,50,100,25,45,35,55,75,45,90,115,25,50,55
 db 80,45,65,50,80,110,110,75,45,70,30,45,105,40,80,25,45,50,80,55,85,120,80,95
 db 85,85,70,70,20,100,95,48,65,95,95,110,75,55,70,45,70,75,110,125,90,85,50,70
 db 100,90,100,65,80,100,50,65,85,48,63,83,45,55,56,96,80,110,40,60,80,56,76,35
 db 55,20,65,105,45,70,45,60,90,100,50,80,65,100,55,65,85,55,30,85,45,25,65,95
 db 130,42,110,85,48,58,65,35,60,65,65,65,40,60,55,80,230,95,75,50,75,40,80,30
 db 60,85,35,75,45,140,70,50,80,95,40,60,95,65,45,35,110,65,55,55,70,135,100,75
 db 115,50,70,100,154,154,100,55,65,85,50,60,70,50,70,90,30,60,41,61,30,25,50,25
 db 90,50,70,100,30,40,60,30,50,30,30,50,60,60,60,30,70,52,82,35,45,35,55,120,70
 db 120,90,70,65,41,71,65,35,55,55,125,20,40,45,50,80,30,60,40,60,45,75,50,70,90
 db 40,60,50,80,85,65,40,80,110,75,85,55,55,75,75,105,48,90,130,80,35,55,65,53,83
 db 87,23,43,63,55,75,75,60,33,63,60,60,65,40,50,60,70,75,75,87,107,50,80,35,55
 db 115,30,50,80,60,80,90,100,200,150,140,90,90,130,110,100,50,80

d_wt:
 dw 69,130,1000,85,190,905,90,225,855,29,99,320,32,100,295,18,300,395,35,185
 dw 20,380,69,650,60,300,120,295,70,200,600,90,195,620,75,400,99,199,55,120
 dw 75,550,54,86,186,54,295,300,125,8,333,42,320,196,766,280,320,190,1550,124
 dw 200,540,195,565,480,195,705,1300,40,64,155,455,550,200,1050,3000,300,950,360,785
 dw 60,600,150,392,852,900,1200,300,300,40,1325,1,1,405,2100,324,756,65,600,104
 dw 666,25,1200,65,450,498,502,655,10,95,1150,1200,346,350,800,80,250,150,390,345
 dw 800,545,560,406,300,445,550,884,100,2350,2200,40,65,290,245,250,365,75,350,115
 dw 405,590,4600,554,526,600,33,165,2100,1220,40,64,158,1005,79,190,795,95,250,888
 dw 60,325,212,408,108,356,85,335,750,120,225,20,30,10,15,32,20,150,78,133
 dw 615,58,85,285,380,339,5,10,30,115,18,85,380,85,750,265,270,21,795,10
 dw 50,285,415,72,1258,140,648,4000,78,487,39,1180,205,540,280,88,1258,350,550,65
 dw 558,50,120,285,160,2200,505,108,350,1520,335,1200,325,712,580,210,480,60,235,214
 dw 755,468,1780,1980,1870,720,1520,2020,2160,1990,50,50,216,522,25,195,520,76,280,819
 dw 136,370,175,325,36,100,284,115,316,26,325,550,40,280,596,55,120,12,23,198
 dw 45,392,50,95,280,17,36,1300,3980,110,326,220,215,1080,970,804,110,19,236,87
 dw 115,328,74,1620,208,888,150,153,820,864,2538,152,402,240,2200,395,876,1506,513,774
 dw 168,2565,1680,1540,20,306,715,42,42,115,112,315,12,206,140,150,306,20,240,465
 dw 1305,103,800,1000,163,405,840,525,270,226,470,23,125,525,403,234,600,1200,3600,8
 dw 177,177,238,604,125,682,66,202,484,421,1105,1026,952,2025,5500,2300,1750,2050,3520,9500
 dw 2065,400,600,11,608,10

d_ht:
 db 7,10,20,6,11,17,5,10,16,3,7,11,3,6,10,3,11,15,3,7,3,12,20,35,4,8,6,10,4,8
 db 130,5,9,14,6,13,6,11,5,10,8,16,5,8,12,3,10,10,15,2,7,4,10,8,17,5,10,7,19,6
 db 10,13,9,13,15,8,15,16,7,10,17,9,16,4,10,14,10,17,12,16,3,10,8,14,18,11,17,9
 db 12,3,15,13,16,15,88,10,16,4,13,5,12,4,20,4,10,15,14,12,6,12,10,19,11,10,22
 db 4,12,6,13,8,11,13,15,14,11,13,15,14,9,65,25,3,3,10,8,9,8,4,10,5,13,18,21,17
 db 16,20,18,40,22,20,4,9,12,18,5,9,17,6,11,23,8,18,7,16,10,14,5,11,18,5,12,3,3
 db 3,3,6,2,15,6,8,14,4,4,8,12,11,4,6,8,8,3,8,12,4,14,9,10,5,20,7,5,13,15,6,12
 db 15,11,92,6,14,5,18,6,15,9,6,18,7,8,4,11,6,6,9,9,21,17,6,14,18,5,11,6,14,12,7
 db 14,4,6,7,12,15,19,21,20,6,12,20,52,38,6,5,9,17,4,9,19,4,7,15,5,10,4,5,3,6,10
 db 7,12,5,12,15,5,10,13,5,8,8,3,7,4,12,11,6,12,5,8,20,145,6,11,10,5,15,10,5,5,4
 db 9,6,6,11,6,62,8,18,7,11,20,10,23,6,15,7,19,8,11,14,4,13,7,15,10,12,2,7,9,4,4
 db 6,6,13,4,11,6,8,16,3,8,14,20,4,17,20,6,10,15,4,17,18,12,6,11,27,13,10,4,9,21
 db 3,7,6,10,15,7,15,4,8,16,6,11,15,6,12,16,17,18,19,45,35,70,14,20,3,17,6

d_exp_ratio:
 db 3*16+2,3*16+2,3*16+2,3*16+2,3*16+2,3*16+2,3*16+2,3*16+2,3*16+2,2*16+4,2*16+4
 db 2*16+4,2*16+4,2*16+4,2*16+4,3*16+4,3*16+4,3*16+4,2*16+4,2*16+4,2*16+4,2*16+4
 db 2*16+4,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4,3*16+7,3*16+7,3*16+7,3*16+1,3*16+1
 db 3*16+1,1*16+5,1*16+5,2*16+5,2*16+5,1*16+5,1*16+5,2*16+4,2*16+4,3*16+4,3*16+4
 db 3*16+4,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4
 db 2*16+4,2*16+4,4*16+3,4*16+3,3*16+4,3*16+4,3*16+4,3*16+3,3*16+3,3*16+3,3*16+3
 db 3*16+3,3*16+3,3*16+4,3*16+4,3*16+4,4*16+4,4*16+4,3*16+4,3*16+4,3*16+4,2*16+4
 db 2*16+4,2*16+4,2*16+4,2*16+0,2*16+0,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4
 db 2*16+4,4*16+4,4*16+4,3*16+4,3*16+4,3*16+4,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4
 db 2*16+0,2*16+0,4*16+4,4*16+4,2*16+4,2*16+4,2*16+1,2*16+1,2*16+4,2*16+4,2*16+4
 db 4*16+4,4*16+4,1*16+7,2*16+4,2*16+7,2*16+4,2*16+4,2*16+4,2*16+4,4*16+0,4*16+0
 db 2*16+4,2*16+4,2*16+7,2*16+3,2*16+3,4*16+4,4*16+1,4*16+4,4*16+4,4*16+4,2*16+0
 db 2*16+2,2*16+2,2*16+2,2*16+2,2*16+0,2*16+2,2*16+2,2*16+2,2*16+2,4*16+2,4*16+2
 db 4*16+0,4*16+0,4*16+0,4*16+4,4*16+4,4*16+4,4*16+0,3*16+0,3*16+2,3*16+2,3*16+2
 db 3*16+2,3*16+2,3*16+2,3*16+2,3*16+2,3*16+2,2*16+4,2*16+4,2*16+4,2*16+4,1*16+4
 db 1*16+4,1*16+4,1*16+4,2*16+4,4*16+4,4*16+4,2*16+4,1*16+5,1*16+5,1*16+2,1*16+2
 db 2*16+4,2*16+4,3*16+4,3*16+4,3*16+4,3*16+4,1*16+4,1*16+4,2*16+4,3*16+4,3*16+4
 db 3*16+4,3*16+4,1*16+4,3*16+4,3*16+4,2*16+4,2*16+4,2*16+4,2*16+2,2*16+2,3*16+4
 db 2*16+4,1*16+4,2*16+0,2*16+4,2*16+4,2*16+4,2*16+4,2*16+4,3*16+4,2*16+4,1*16+5
 db 1*16+5,2*16+4,2*16+4,3*16+4,4*16+4,3*16+4,2*16+4,2*16+4,2*16+4,2*16+4,4*16+4
 db 4*16+4,1*16+5,2*16+4,2*16+4,1*16+4,4*16+4,4*16+4,4*16+4,4*16+4,2*16+4,2*16+4
 db 2*16+4,2*16+0,4*16+4,1*16+4,2*16+1,2*16+1,2*16+7,2*16+3,2*16+3,4*16+7,1*16+7
 db 4*16+0,4*16+0,4*16+0,4*16+4,4*16+4,4*16+4,4*16+0,4*16+0,3*16+0,3*16+2,3*16+2
 db 3*16+2,3*16+2,3*16+2,3*16+2,3*16+2,3*16+2,3*16+2,2*16+4,2*16+4,2*16+4,2*16+4
 db 2*16+4,2*16+4,2*16+4,2*16+4,2*16+4,3*16+4,3*16+4,3*16+4,3*16+4,3*16+4,3*16+4
 db 0*16+4,0*16+4,0*16+0,3*16+4,3*16+4,5*16+4,5*16+4,1*16+4,2*16+4,2*16+4,2*16+4
 db 2*16+4,5*16+4,5*16+4,1*16+5,1*16+5,3*16+4,2*16+0,2*16+0,2*16+4,2*16+4,3*16+4
 db 2*16+4,2*16+4,1*16+5,5*16+4,5*16+4,0*16+4,0*16+4,4*16+4,4*16+4,3*16+4,3*16+4
 db 3*16+4,5*16+3,5*16+3,4*16+4,4*16+4,2*16+4,2*16+4,3*16+4,3*16+4,3*16+4,3*16+4
 db 3*16+4,2*16+4,2*16+4,1*16+0,1*16+0,1*16+5,1*16+4,1*16+4,2*16+4,2*16+4,1*16+4
 db 2*16+4,2*16+4,0*16+4,0*16+4,2*16+4,1*16+4,1*16+4,3*16+4,4*16+4,4*16+4,4*16+4
 db 5*16+4,5*16+4,4*16+4,3*16+4,3*16+4,3*16+4,0*16+4,0*16+4,0*16+4,3*16+4,1*16+4
 db 1*16+4,5*16+4,0*16+4,4*16+2,4*16+4,4*16+4,4*16+4,2*16+4,0*16+1,5*16+7,0*16+2
 db 0*16+2,0*16+2,0*16+2,4*16+4,4*16+4,4*16+4,4*16+4,4*16+4,4*16+4,4*16+0,4*16+0
 db 4*16+0,4*16+0,4*16+0,4*16+0,4*16+0,4*16+0,4*16+0,4*16+7,4*16+1,4*16+0,4*16+0
 db 1*16+4	; Chimecho's sooo special...

d_breed:
 db 017h,017h,017h,01Eh,01Eh,01Eh,012h,012h,012h,033h,033h,033h,033h,033h,033h,044h,044h,044h,055h
 db 055h,044h,044h,05Eh,05Eh,056h,056h,055h,055h,015h,0FFh,0FFh,015h,015h,015h,066h,066h,055h,055h
 db 066h,066h,044h,044h,077h,077h,077h,037h,037h,033h,033h,055h,055h,055h,055h,025h,025h,055h,055h
 db 055h,055h,022h,022h,022h,088h,088h,088h,088h,088h,088h,077h,077h,077h,099h,099h,0AAh,0AAh,0AAh
 db 055h,055h,012h,012h,0AAh,0AAh,045h,044h,044h,025h,025h,0BBh,0BBh,099h,099h,0BBh,0BBh,0BBh,0AAh
 db 088h,088h,099h,099h,0AAh,0AAh,077h,077h,011h,011h,088h,088h,011h,0BBh,0BBh,015h,015h,066h,077h
 db 011h,02Eh,02Eh,0CCh,0CCh,099h,099h,088h,033h,088h,088h,088h,033h,055h,0CEh,0CEh,012h,0DDh,055h
 db 055h,055h,055h,0AAh,029h,029h,029h,029h,044h,011h,0FFh,0FFh,0FFh,02Eh,02Eh,02Eh,0FFh,0FFh,017h
 db 017h,017h,055h,055h,055h,012h,012h,012h,055h,055h,044h,044h,033h,033h,033h,033h,044h,0CCh,0CCh
 db 0FFh,0FFh,0FFh,0FFh,046h,044h,044h,015h,015h,015h,077h,026h,026h,0AAh,022h,067h,067h,067h,055h
 db 077h,077h,033h,025h,025h,055h,055h,044h,012h,0BBh,0FFh,0BBh,055h,033h,033h,055h,033h,0AAh,056h
 db 056h,0CCh,033h,033h,033h,055h,055h,055h,0BBh,0BBh,055h,055h,029h,02Ch,02Ch,025h,022h,044h,055h
 db 055h,02Eh,055h,055h,0AAh,055h,055h,0FFh,088h,0FFh,0FFh,0FFh,055h,066h,0FFh,0FFh,0FFh,011h,011h
 db 011h,0FFh,0FFh,0FFh,01Eh,01Eh,01Eh,055h,055h,055h,012h,012h,012h,055h,055h,055h,055h,033h,033h
 db 033h,033h,033h,027h,027h,027h,057h,057h,057h,033h,033h,0AAh,044h,044h,067h,067h,058h,024h,024h
 db 023h,023h,05Ch,05Ch,056h,056h,055h,0AAh,0AAh,0AAh,055h,088h,0CCh,0CCh,0CCh,029h,029h,02Eh,02Eh
 db 0CCh,0CCh,033h,033h,033h,088h,088h,055h,055h,055h,055h,025h,025h,025h,078h,078h,06Ah,06Ah,0AAh
 db 0AAh,0FFh,055h,055h,066h,066h,056h,088h,088h,04Eh,04Eh,0FFh,0BBh,0BBh,067h,055h,055h,055h,0BBh
 db 0BBh,017h,015h,015h,015h,022h,022h,022h,055h,0BBh,0BBh,05Eh,055h,02Ch,011h,011h,011h,06Bh,038h
 db 038h,099h,099h,099h,099h,0BBh,0BBh,0BBh,0EEh,0EEh,0EEh,0AAh,0AAh,0AAh,0FFh,0FFh,0FFh,0FFh,0FFh
 db 0FFh,0FFh,0FFh,0FFh,0FFh,0BBh

p_splash:
 db 17,13
 db 00000010b,00000000b
 db 00000111b,00000000b
 db 00001111b,10000000b
 db 00100000b,00100000b
 db 01100000b,00110000b
 db 00000000b,00000000b
 db 10011000b,11101000b
 db 10000101b,00001000b
 db 10001001b,00001000b
 db 10000101b,00001000b
 db 10111000b,11001000b
 db 00000000b,00000000b
 db 01100000b,00110000b
 db 00100000b,00100000b
 db 00001111b,10000000b
 db 00000111b,00000000b
 db 00000010b,00000000b
p_splash_end:

p_btm:
 db 16,93
 db 11111111b,11111111b,01111111b,11111111b,11011111b,11111111b,11111111b,01111111b,11111111b,11110111b,11111111b,11111111b
 db 00000000b,00000000b,10000000b,00000000b,00100000b,00000000b,00000000b,10000000b,00000000b,00001000b,00000000b,00000000b
 db 11001010b,10101100b,10101001b,00101011b,10100000b,00110111b,01000000b,10101010b,00100110b,11101011b,00100010b,10011000b
 db 10101010b,11101010b,10111010b,10101010b,00100000b,01000100b,01000000b,10111010b,00101000b,01001010b,10100010b,10100000b
 db 11001100b,10101010b,10101010b,10101011b,00100000b,00100110b,01000000b,10101010b,00100100b,01001011b,00100010b,10010000b
 db 10001010b,10101010b,10101010b,10101010b,00100000b,00010100b,01000000b,10101010b,00100010b,01001010b,00100010b,10001000b
 db 10001010b,10101010b,10101001b,00010011b,10100000b,01100111b,01110000b,10101011b,10101100b,01001010b,00111011b,00110000b
 db 00000000b,00000000b,10000000b,00000000b,00100000b,00000000b,00000000b,10000000b,00000000b,00001000b,00000000b,00000000b
 db 00000011b,10000000b,10000000b,01000000b,00100000b,00111111b,11000000b,10000010b,00111110b,00001000b,00001110b,00000000b
 db 00000111b,11000000b,10000000b,01000000b,00100000b,00000000b,00000000b,10000111b,00000000b,00001000b,00001110b,00000000b
 db 00001111b,11100000b,10000011b,11111000b,00100000b,00111111b,11000000b,10000010b,00111110b,00001000b,00111111b,10000000b
 db 00001110b,11100000b,10000001b,11110000b,00100000b,00000000b,00000000b,10000101b,00000000b,00001000b,00111111b,10000000b
 db 00001001b,00100000b,10000000b,11100000b,00100000b,00111111b,11000000b,10000000b,00111110b,00001000b,00111111b,10000000b
 db 00000100b,01000000b,10000001b,10110000b,00100000b,00000000b,00000000b,10000000b,00000000b,00001000b,00001110b,00000000b
 db 00000011b,10000000b,10000001b,00010000b,00100000b,00111111b,11000000b,10000000b,00111110b,00001000b,00001110b,00000000b
 db 00000000b,00000000b,10000000b,00000000b,00100000b,00000000b,00000000b,10000000b,00000000b,00001000b,00000000b,00000000b
p_btm_end:

s_splash:
 db "SELECTEXTRA& HELPPOKEMON",Sleft,Sconvert,"MOVE"
s_splash_end:

keys_statcalc:
 db  SFourSpaces				; (not used)
 db        "v<>^",0,0,0,0		; v<>^????
 db          "x+-", 0,0,0,0,0	; enter+-*/^clear-?
 db            "?369",0,0,"|",0	; -369)tan-vars-?
 db            0,"258",0,0,0,0	; .258(cos-prgm-stat
 db             "0147",0,0,0,0	; 0147,sin-apps-xt0n
 db           0,0,0,0, 0,0,0,0	; ?-store-ln-log-square-recip-math-alpha
 db          5,4,3,2,1,0,"~"	; graph-trace-zoom-window-y=-2nd-mode

keys_statcalc_alpha:
 db  0		; (not used; DEL for the previous set)
 db        "v-+^",0,0,0,0		; v<>^????
 db         "x+","WRMH","__"	; enter+-*/^clear-?
 db         "?","[VQLG","__"	; -369)tan-vars-?
 db         "_","ZUPKFC","_"	; .258(cos-prgm-stat
 db         "_","YTOJEB","_"	; 0147,sin-apps-xt0n
 db         "_","XSNIDA","_"	; ?-store-ln-log-square-recip-math-alpha
 db          5,4,3,2,1,0,"~"		; graph-trace-zoom-window-y=-2nd-mode
 db "_"	; del

statcalc_waitkey:
  halt				; save batteries
  B_CALL GetCSC	; Get keyboard scan code
  or a	; is a 0?
  jr z,statcalc_waitkey	; go again if no key
  ld hl,keys_statcalc
  bit statcalc_alpha,(IY+tempFlagByte)
  jr nz,statcalc_waitkey_load_alpha_keypoard
 statcalc_waitkey_alpha_keypoard_loaded:
  ld d,0
  ld e,a		; put scan code to DE (d=0 from above)
  add hl,de	; get "my" code address
  ld a,(hl)	; get "my" code to a
  ; switch(my_code)
  or A						; case 0
  jr z,statcalc_waitkey	;  go to wait again
  cp "v"
  jr z,statcalc_ui_down
  cp "^"
  jr z,statcalc_ui_up
  cp "<"
  jr z,statcalc_ui_left
  cp ">"
  jr z,statcalc_ui_right
  cp "+"
  jr z,statcalc_ui_plus
  cp "-"
  jr z,statcalc_ui_minus
  cp "?"
  jr z,statcalc_ui_minmaxmode
  cp "|"
  jr z,statcalc_ui_distribute
  cp "x"
  jr z,statcalc_ui_cancel_input
  cp 1
  jr z,waitkey_1
  cp 2
  jr z,waitkey_2
  cp 3
  jr z,waitkey_3
  cp 4
  jr z,waitkey_4
  cp 5
  jr z,StatCalculator
  cp "~"
  jr z,waitkey_quit
  cp "_"
  jr z,statcalc_waitkey
  cp "9"+1
  jp m,statcalc_ui_number
  cp "["+1
  jp m,statcalc_ui_letter
  jr statcalc_waitkey

statcalc_waitkey_load_alpha_keypoard:
  ld HL,keys_statcalc_alpha
  jr statcalc_waitkey_alpha_keypoard_loaded

statcalc_failed_to_get_x:
  B_JUMP Select

; appBackUpScreen will serve as a place for data.
statcalc_data equ appBackUpScreen+256-(appBackUpScreen%256)	; It's aligned at a 256-byte boundary. That makes
		; it easier to handle as the last byte of the address is always a direct offset.
statcalc_row_hp  equ statcalc_data
statcalc_row_atk equ statcalc_data+8
statcalc_row_def equ statcalc_data+8*2
statcalc_row_spd equ statcalc_data+8*3
statcalc_row_sat equ statcalc_data+8*4
statcalc_row_sde equ statcalc_data+8*5
statcalc_row_min equ statcalc_data+8*6	; A temporary row reserved for Min/Max resolution
statcalc_row_max equ statcalc_data+8*7	; A temporary row reserved for Min/Max resolution
statcalc_row_end equ statcalc_data+8*8
statcalc_row_len equ               8

basestat		equ 0	; add these to the statcalc_row_* offsets
dv				equ 1
ev				equ 2
pers			equ 3
stat_min		equ 4 ; word; so it's @ +5 as well
stat_max		equ 6 ; word; so it's @ +7 as well

statcalc_level equ 	statcalc_row_end+0
statcalc_nature equ	statcalc_row_end+1

statcalcUI equ		statcalc_row_end+2
statcalcUI_row equ	statcalcUI+0
statcalcUI_col equ	statcalcUI+1

statcalcUI_end equ	statcalcUI+2

; UI row equates:
statcalcUI_hp equ		0	; STAT row
statcalcUI_atk equ		1	; STAT row
statcalcUI_def equ		2	; STAT row
statcalcUI_spd equ		3	; STAT row
statcalcUI_sat equ		4	; STAT row
statcalcUI_sde equ		5	; STAT row
statcalcUI_nature equ	6
statcalcUI_level equ	7
statcalcUI_pokemon equ	8

statcalcUI_laststatrow equ statcalcUI_sde
statcalcUI_lastrow equ	statcalcUI_pokemon

; UI column equates (not useful in non-STAT rows, but the byte holds its value):
statcalcUI_dv equ			0
statcalcUI_ev equ			1
statcalcUI_p equ			2

statcalc_natsearcher_data equ	statcalcUI_end
statcalc_natsearcher_count equ	statcalc_natsearcher_data
statcalc_natsearcher_buff equ 	statcalc_natsearcher_data+1
statcalc_natsearcher_end equ	statcalc_natsearcher_buff+8

; The Stat Calculator (UI & Engine)
StatCalculator:
  call clear_statcalc_table
 
 statcalc_start_without_preparing_table:
  res statcalc_entry,(IY+tempFlagByte)
  res statcalc_alpha,(IY+tempFlagByte)

  AppOnErr statcalc_failed_to_get_x
  B_CALL RclX			; get our X
  ld HL,385
  B_CALL SetXXXXOP2
  B_CALL CpOP1OP2
  jr z, statcalc_chimechox
  jr nc, statcalc_failed_to_get_x
 statcalc_chimechox:
  B_CALL ConvOP1	; put X to DE
  AppOffErr
  push de			; save our X

  startbuffer

  call statcalc_drawstatics

  ;ld A,58
  ;ld (penCol),A
  ;xor A
  ;ld (penRow),A
  setpenpos 58,0
  ld A,"N";Sstore
  B_CALL VPutMap
  ;ld A,64
  ;ld (penCol),A
  ld A,"#"
  B_CALL VPutMap
  pop DE
  push DE
  ld HL,d_national	; We have to write the National Number
  add HL,DE
  add HL,DE
  ld E,(HL)
  inc HL
  ld D,(HL)
  ex HL,DE
  call draw_HL

  B_CALL RclX		; get our X again
  xor a; A <- 0
  ld (penCol), a
  ld (penRow), a	; set pen location to (0,0)
  call DrawPkName

  pop DE	; Recall our X

  ; Fill up the Table for the first time:
  ld HL,d_hp
  add HL,DE		; We have HP
  ld A,(HL)
  ld (statcalc_row_hp+basestat),A
  ld DE,386
  add HL,DE		; Now we have ATK
  ld A,(HL)
  ld (statcalc_row_atk+basestat),A
  add HL,DE		; Now we have DEF
  ld A,(HL)
  ld (statcalc_row_def+basestat),A
  add HL,DE		; Now we have SPD
  ld A,(HL)
  ld (statcalc_row_spd+basestat),A
  add HL,DE		; Now we have SAT
  ld A,(HL)
  ld (statcalc_row_sat+basestat),A
  add HL,DE		; Now we have SDE
  ld A,(HL)
  ld (statcalc_row_sde+basestat),A

  call calc_stats
  call write_rows
  call statcalc_ui_drawcursor

  jr statcalc_waitkey

statcalc_drawstatics:
  ;call drawsplash

  ld HL,s_statcalc_statics
  ld DE,OP1
  ld BC,26
  LDIR
 ;putOP1sn 56, 0, 0, 1
  putOP1sn 12, 6, 1,17
  putOP1sn 44,54,18, 2
  putOP1sn 28,48,20, 6

  ld HL,s_hp
  ld DE,OP1
  ld BC,6*4-1
  LDIR

  putOP1sn  4,12, 0    , 2
  putOP1sn  0,18, 3    , 3
  putOP1sn  0,24, 3+4  , 3
  putOP1sn  0,30, 3+4*2, 3
  putOP1sn  0,36, 3+4*3, 3
  putOP1sn  0,42, 3+4*4, 3
  ret

clear_statcalc_table:
  ld HL,d_statcalc_clear
  ld DE,statcalc_row_hp+dv
  ld BC,7
  LDIR
  ld HL,d_statcalc_clear
  ld DE,statcalc_row_atk+dv
  ld BC,7
  LDIR
  ld HL,d_statcalc_clear
  ld DE,statcalc_row_def+dv
  ld BC,7
  LDIR
  ld HL,d_statcalc_clear
  ld DE,statcalc_row_spd+dv
  ld BC,7
  LDIR
  ld HL,d_statcalc_clear
  ld DE,statcalc_row_sat+dv
  ld BC,7
  LDIR
  ld HL,d_statcalc_clear
  ld DE,statcalc_row_sde+dv
  ld BC,7
  LDIR

  ld A,100
  ld (statcalc_level),A
  ld A,neutral
  ld (statcalc_nature),A

  xor A
  ld (statcalcUI_row),A
  ld (statcalcUI_col),A

  ret

calc_stats:
  ld HL,statcalc_row_hp
  ld C,1	; C=1 : HP
  call do_stat
  ld HL,statcalc_row_atk
  ld C,0	; C=0 : Not HP
  call do_stat	; preserves C
  ld HL,statcalc_row_def
  call do_stat	; preserves C
  ld HL,statcalc_row_spd
  call do_stat	; preserves C
  ld HL,statcalc_row_sat
  call do_stat	; preserves C
  ld HL,statcalc_row_sde
  call do_stat	; preserves C, too ^^

  ret

do_stat:
  ; preserves C
  push BC										; Push C (HP/NotHP)
  push HL										; Push HL (Address of Current Stat's Row)
  ld DE,statcalc_row_min						; Set address of target row (min)
  ld B,0	; min								; Set "Min" modifier
  res statcalc_minmax,(IY+tempFlagByte)			; Reset MinMaxMode flag #1
  res statcalc_maxalready,(IY+tempFlagByte)		; Reset MinMaxMode flag #2
  call load_calcstat_values						; Load Stat values to the Min and maybe Max
				; First Pass:	Load to Min Row with "Min" modifier for any MinMaxMode entries
				;				  - If a MinMaxMode entry is found, minmax (flag #1) is set
				;				If minmax (flag #1) is not set, RETURN NOW  -       -   - ---------->
				;			(x)	If maxalready (flag #2) is set, return (doesn't happen now)
				; Prepare:		Set maxalready (flag #2)
				;				Reset minmax (flag #1) so it's clear for the next pass
				;				Load "Max" modifier for the second pass
				;				Setup target row, statcalc_row_max
				;				Call the function again, as the second pass (maxalready, flag #2, is set)
				; Second Pass:	Load to Min Row with "Max" modifier for any MinMaxMode entries
				;				  - If a MinMaxMode entry is found, minmax (flag #1) is set
				;			(x)	If minmax (flag #1) is not set, return (doesn't happen now, it'd return in Pass1)
				;				If maxalready (flag #2) is set, RETURN NOW  -       -   - ---------->
			; ---RESULTS---								
				; If MinMaxMode is not on:
				;							statcalc_row_min contains data copied from the stat row
				;							both MinMaxMode flags are both on
				; If MinMaxMode is on:
				;							statcalc_row_min contains data for the minimum resulting stat
				;							statcalc_row_max contains data for the maximum resulting stat
				;							both MinMaxMode flags are both on
  pop DE										; Get back the Address of Current Stat Row, this time to DE
  ld HL,stat_min								; Offset the row so we're pointing to the Min entry
  add HL,DE										; ...
  ex DE,HL										; Store the pointer to the >>target word<< to DE
  pop BC	; normal stat: C=0    hp: C=1		; Pop C (HP/NotHP)
  call calc_stat	; preserves C				; Calculate the Stat
  				; Prepare:		Set "Source" to statcalc_row_min
				; First Pass:	Calculate a stat from the Source (statcalc_row_min)
				;				Store the result stat to the >>target word<<, [currentrow]+stat_min
				; 				Advance the >>target word<< so that it points to [currentrow]+stat_max
				;				If minmax (flag #1) is not set, GoTo Finalize   -       -   - ----------- F
				;			(x)	If maxalready (flag #2) is not set, return (doesn't happen now)
				; Prepare:		Set "Source" to statcalc_row_max
				;				Reset maxalready (flag #2) so we return after the second pass
				;				Call the function again, as the second pass (maxalready, flag #2, is reset)
				; First Pass:	Calculate a stat from the Source (statcalc_row_max)
				;				Store the result stat to the >>target word<<, [currentrow]+stat_max
				; 				Advance the >>target word<< so that it points to the next row (>>target word<<
				;						won't be used again, so what the heck)
				;			(x)	If minmax (flag #1) is not set, GoTo Finalize (doesn't happen now)
				;				If maxalready (flag #2) is not set, RETURN NOW  -       -   - ---------->
				; Finalize:	(This code is run if not in MinMaxMode, after the first pass)
				;				Load 0FFFFh (undefined) to >>target word<<, [currentrow]+stat_max
				;				Return  -         -       -  -  -  - - - - - - -- -- -- --- ------------>
  ret											; And that's it ^^

load_calcstat_values:
  push HL
  ld C,0
  ld A,(HL)
  ld (DE),A		; load the Base Stat
  inc HL
  inc DE
  inc C
  ld A,(HL)
  cp -1
  call z, statcalc_adjust
  ld (DE),A		; load the DV
  inc HL
  inc DE
  inc C
  ld A,(HL)
  cp -1
  call z, statcalc_adjust
  ld (DE),A		; load the EV
  inc HL
  inc DE
  inc C
  ld A,(HL)
  cp -1
  call z, statcalc_adjust
  ld (DE),A		; load the P
  inc DE
  xor A
  pop HL
  bit statcalc_minmax,(IY+tempFlagByte)
  ret z	; return if not loading the Max
  bit statcalc_maxalready,(IY+tempFlagByte)	; test the "don't-go-again" bit
  ret nz	; if it was set, don't go again
  ; If Min/Max mode is activated:
  ;no ld HL,; HL's already been popped some time before
  ld DE,statcalc_row_max
  ld B,1	; max
  set statcalc_maxalready,(IY+tempFlagByte)	; set the "don't-go-again" bit
  call load_calcstat_values	; load MAX values too
  ret

statcalc_adjust:
  push HL
  ld HL,minmax_values
  ld A,L
  add C
  add C
  add B
  ld L,A
  ld A,(HL)
  pop HL
  set statcalc_minmax,(IY+tempFlagByte)
  ret

check_shedinja:	; not really a function
  ld A,(IX+basestat)
  or A
  jr nz,its_not_shedinja
  ld HL,1	; just put 1 to HL (Resulting Stat)
  jr it_is_shedinja

calc_stat:
  ld IX,statcalc_row_min
 calc_stat_IX_already_set:
  push DE
  push BC		; THIS FUNCTION MUST PRESERVE C

  ld A,C
  or A
  jr nz,check_shedinja
 its_not_shedinja:

  ld H,0
  ld L,(IX+basestat)
  add HL,HL		; BaseStat*2
  ld D,0
  ld E,(IX+dv)
  add HL,DE		; BaseStat*2+DV
  ld E,(IX+ev)
  ;srl E
  ;srl E
  add HL,DE		; BaseStat*2+DV+[EV/4]
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  B_CALL OP2ToOP5		; BaseStat*2+DV+[EV/4]	is in OP5 (and, temporarily, in OP1 & OP2)
  ld A,(statcalc_level)
  ld L,A
  ld H,0
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP6		; Level						is in OP6 (and, temporarily, in OP2)
  B_CALL FPMult
  ld HL,100
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP4		; 100						is in OP4 (and, temporarily, in OP2)
  B_CALL FPDiv
  B_CALL ConvOP1
  pop BC
  push BC
  ld A,C
  or A
  jr z,calc_normalstat
 calc_hp:
  ld HL,10
  add HL,DE		; Add 10 to the stat
  ex HL,DE
  ld A,(statcalc_level)
  ld L,A
  ld H,0
  add HL,DE		; add Level to the stat
  jr calc_different_stats_end
 calc_normalstat:
  ld HL,5
  add HL,DE
  ld A,(IX+pers)
  or A
  jr z,calc_different_stats_end
  push AF
  B_CALL SetXXXXOP2
  pop AF
  cp 1
  jr z,calcstat_personality_beneficial
 calcstat_personality_hindering:
  ld HL,num_zero_point_nine
  jr calcstat_personality_common
 calcstat_personality_beneficial:
  ld HL,num_one_point_one
  jr calcstat_personality_common
 calcstat_personality_common:
  B_CALL Mov9ToOP1
  B_CALL FPMult
  B_CALL ConvOP1
  ex DE,HL
 ;jr calc_different_stats_end
 calc_different_stats_end:
 it_is_shedinja:
  pop BC
  pop DE
  push BC
  ex DE,HL
  ld (HL),D
  inc HL
  ld (HL),E		; First stat is saved
  inc HL
  pop BC
  bit statcalc_minmax,(IY+tempFlagByte)
  jr z,statcalc_zerooutnexttwobytes			; return if not in Min/Max Mode
  ex HL,DE		; put address of next write to DE
  res statcalc_minmax,(IY+tempFlagByte)	; reset that thing so we don't go again
  ld IX,statcalc_row_max	; go for Max
  jr calc_stat_IX_already_set
 statcalc_zerooutnexttwobytes:
  bit statcalc_maxalready,(IY+tempFlagByte)
  ret nz			; return if this cleanup code is called from Min/Max mode (normal Min/Max flag was cleared before)
  ld A,-1
  ld (HL),A
  inc HL
  ld (HL),A
  ret

write_rows:
  continuebuffer
  ld BC,12*256+0*8
  call write_row	; HP
  ld BC,18*256+1*8
  call write_row	; ATK
  ld BC,24*256+2*8
  call write_row	; DEF
  ld BC,30*256+3*8
  call write_row	; SPD
  ld BC,36*256+4*8
  call write_row	; SAT
  ld BC,42*256+5*8
  call write_row	; SDE

  ; Do Nature
  ;ld A,56
  ;ld (penCol),A
  ;ld A,48
  ;ld (penRow),A
  setpenpos 56,48
  ld HL,natures	; nature texts are 256-aligned to start at xx00h, so an offset can be put to H directly
  ld A,(statcalc_nature)
  cp -1
  jr z,write_rows_minmaxmode_nature
  sla A	; x2
  sla A	; x4
  sla A	; x8
  ld L,A
  ld A,(HL)
  or A
  jr z,write_rows__write_neutral_personality_symbol
  ld A,SFourSpaces;"+" ;write Effective personality symbol
  B_CALL VPutMap
 write_rows__personality_symbol_written:
  inc HL
  ld BC,7
 write_rows_minmaxmode_nature_loaded:
  ld DE,OP1
  LDIR
  ld (HL),0
  ld HL,OP1
  B_CALL VPutS

  ; Do Level
  ;ld A,56
  ;ld (penCol),A
  ;ld A,54
  ;ld (penRow),A
  setpenpos 56,54
  ld A,(statcalc_level)
  call draw_A
  endbuffer
  ret
 write_rows__write_neutral_personality_symbol:
  ld A,Scross ;Sblock also works well
  B_CALL VPutMap
  jr write_rows__personality_symbol_written
 write_rows_minmaxmode_nature:
  ld HL,s_fullminmaxmode_nature
  ld BC,8
  jr write_rows_minmaxmode_nature_loaded
  


write_row:

  push BC
  ld A,B
  ld (penRow),A

  ld A,16
  ld (penCol),A

  ld D,0
  ld E,C
  ld HL,statcalc_data
  add HL,DE
 ;push HL

 ;pop HL
  ld A,(HL)
  inc HL
  push HL
  call draw_A

  pop HL
  ld A,(HL)
  inc HL
  push HL
  call draw_A

  pop HL
  ld A,(HL)
  inc HL
  push HL
  call draw_A

  ld A,56
  ld (penCol),A
  pop HL
  ld A,(HL)
  inc HL
  push HL
  or A
  jr z,write_row_personality_skip
  dec A
  jr z,write_row_personality_plus
  cp -2	; -1 (MinMaxMode) decremented by 1 makes -2
  jr z,write_row_personality_minmaxmode
 write_row_personality_minus:
  ld A,"-"
  B_CALL VPutMap
  jr write_row_personality_end
 write_row_personality_plus:
  ld A,"+"
  B_CALL VPutMap
  jr write_row_personality_end
 write_row_personality_minmaxmode:
  ld A,"?"
  B_CALL VPutMap
  jr write_row_personality_end
 write_row_personality_skip:
  ld A,SFourSpaces
  B_CALL VPutMap
 ;jr write_row_personality_end
 write_row_personality_end:

  ld A,64
  ld (penCol),A
  pop HL
  ld D,(HL)
  inc HL
  ld E,(HL)
  inc HL
  push HL
  ex HL,DE
  call draw_HL

  pop HL
  push HL
  ld A,(HL)
  inc A
  jr z,statcalc_drawrow_no_max
  ld D,(HL)
  inc HL
  ld E,(HL)
  inc HL
  ex HL,DE
  ld A,"-"
  B_CALL VPutMap
  call draw_HL
  ld A," "
  B_CALL VPutMap
  pop HL
  pop BC
  ret
 statcalc_drawrow_no_max: ; an alternate ending
  pop HL
  pop BC
  ld HL,aligned_stat_names
  ld D,0
  ld E,C
  srl E
  add HL,DE
  ld DE,OP1
  ld BC,4
  LDIR
  xor A
  ld (OP1+4),A
  ld HL,OP1
  B_CALL VPutS
  ret
 

draw_A:
  cp A,-1
  jr z,draw_questions
 draw_A_no_questions:	; no "??" for the -1 special case; call this as a function
  ld L,A
  xor A
  ld H,A
 draw_HL:			; call this as a function
  ld A,H
  or A
  jr nz,drawA_continue
  ld A,L
  or A
  jr z,drawA_FormBase_ofZero
 drawA_continue:
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  B_CALL FormBase	; puts zero-terminated displayable string to OP3, and its length to BC
 drawA_formBase_done:
  push BC
  dec C
  dec C
  dec C	; we know the string will be 3 chars long max. Now, C would be zero if that's the case
  jr z,drawA_writestring
  ld A,SFourSpaces
  B_CALL VPutMap
  inc C	; if C is zero now, we have 2 chars (+ 1 space already)
  jr z,drawA_writestring
  ld A,SFourSpaces
  B_CALL VPutMap
  inc C	; if C is zero now, we have 1 chars (+ 2 spaces already)
  jr z,drawA_writestring
 drawA_writestring:
  ld HL,OP3
  pop BC
  ld B,C
  B_CALL VPutSN
  ret
 drawA_FormBase_ofZero:
  ld A,"0"
  ld (OP3),A
  xor A
  ld (OP3+1),A
  ld BC,1
  jr drawA_formBase_done
draw_questions:
  ld HL,s_question
  ld DE,OP1
  ld BC,4
  LDIR
  ld HL,OP1
  B_CALL VPutS
  ret

statcalc_ui_up:
  res statcalc_entry,(IY+tempFlagByte)
  call statcalc_ui_erasecursor
  ld A,(statcalcUI_row)
  dec A
  or A
  jp m,setrowtomax
 up_row_set:
  ld (statcalcUI_row),A
  call statcalc_ui_drawcursor
  jr statcalc_waitkey
 setrowtomax:
  ld A,statcalcUI_lastrow
  jr up_row_set

statcalc_ui_down:
  res statcalc_entry,(IY+tempFlagByte)
  call statcalc_ui_erasecursor
  ld A,(statcalcUI_row)
  inc A
  cp statcalcUI_lastrow+1
  jp p,setrowtozero
 dn_row_set:
  ld (statcalcUI_row),A
  call statcalc_ui_drawcursor
  jr statcalc_waitkey
 setrowtozero:
  xor A
  jr dn_row_set

statcalc_ui_left:
  res statcalc_entry,(IY+tempFlagByte)
  call statcalc_ui_erasecursor
  ld A,(statcalcUI_col)
  dec A
  or A
  jp m,setcoltomax
 left_col_set:
  ld (statcalcUI_col),A
  call statcalc_ui_drawcursor
  jr statcalc_waitkey
 setcoltomax:
  ld A,2
  jr left_col_set

statcalc_ui_right:
  res statcalc_entry,(IY+tempFlagByte)
  call statcalc_ui_erasecursor
  ld A,(statcalcUI_col)
  inc A
  cp 3
  jp p,setcoltozero
 rite_col_set:
  ld (statcalcUI_col),A
  call statcalc_ui_drawcursor
  jr statcalc_waitkey
 setcoltozero:
  xor A
  jr rite_col_set

statcalc_ui_plus:
  res statcalc_entry,(IY+tempFlagByte)
  call statcalc_recall
  bit plusminus,C
  jr z,statcalc_reset_personality	; if +/- is pressed in HP's P, reset Nature
  bit nodirectaddress,C
  jr nz,statcalc_next_pokemon	; if +/- cycle through pokemon, jump to the next one
  bit directplusminus,C
  jr nz,statcalc_ui_plus_direct
  inc A
  call statcalc_validate
  call statcalc_update
  call statcalc_ui_drawcursor
  jr statcalc_waitkey
 statcalc_ui_plus_direct:
  ; Change Context to Personality-Changer:
  ld B,A					; B (Current Sign) loaded
  ld A,(statcalc_nature)
  sla A	; x2
  sla A	; x4
  sla A	; x8
  ld E,A
  ld D,0
  ld HL,natures
  add HL,DE
  ld C,(HL)					; C (Current Nature Byte) loaded
  ld A,(statcalcUI_row)
  ld D,A					; D (Current Row) loaded
  ld A,1			;	;	; A (Target Sign) loaded
  ld E,1			;	;	; E (Target Sign copy) loaded
  ld H,11110000b	;	;	; H (Target Mask) loaded
  ld L,2			;	;	; L (Opposite Sign) loaded
  jr statcalc_personality_changer

statcalc_ui_minus:
  res statcalc_entry,(IY+tempFlagByte)
  call statcalc_recall
  bit plusminus,C
  jr z,statcalc_reset_personality	; if +/- is pressed in HP's P, reset Nature
  bit nodirectaddress,C
  jr nz,statcalc_prev_pokemon	; if +/- cycle through pokemon, jump to the previous one
  bit directplusminus,C
  jr nz,statcalc_ui_minus_direct
  dec A
  call statcalc_validate
  call statcalc_update
  call statcalc_ui_drawcursor
  jr statcalc_waitkey
 statcalc_ui_minus_direct:
  ; Change Context to Personality-Changer:
  ld B,A					; B (Current Sign) loaded
  ld A,(statcalc_nature)
  sla A	; x2
  sla A	; x4
  sla A	; x8
  ld E,A
  ld D,0
  ld HL,natures
  add HL,DE
  ld C,(HL)					; C (Current Nature Byte) loaded
  ld A,(statcalcUI_row)
  ld D,A					; D (Current Row) loaded
  ld A,2			;	;	; A (Target Sign) loaded
  ld E,2			;	;	; E (Target Sign copy) loaded
  ld H,00001111b	;	;	; H (Target Mask) loaded
  ld L,1			;	;	; L (Opposite Sign) loaded
  jr statcalc_personality_changer

statcalc_ui_minmaxmode:
  res statcalc_entry,(IY+tempFlagByte)
  call statcalc_recall
  bit minmaxmode,C
  jr z,statcalc_waitkey		; if MinMaxMode is not enabled here, don't change it
  bit numberinput,C
  jr z,statcalc_set_minmaxmode_nature	; change Nature if number input is not allowed (P)
 statcalc_minmaxmode_nature_drawn:
  ld A,-1	; load MinMaxMode
  call statcalc_update		; no validation; if the MinMaxMode flag was set, setting this should be OK
  call statcalc_ui_drawcursor
  jr statcalc_waitkey

statcalc_ui_distribute:
  res statcalc_entry,(IY+tempFlagByte)
  call statcalc_recall
  bit statrow,C
  jr z,statcalc_waitkey		; return if not in a Stat-row (all Stat rows have Distribution allowed)
  ld A,L
  and 7	; get the offset by leaving only the last 3 bits
  ld L,A
  ld (HL),D	; load the Value to HP
  add 8
  ld L,A
  ld (HL),D	; load the Value to ATK
  add 8
  ld L,A
  ld (HL),D	; load the Value to DEF
  add 8
  ld L,A
  ld (HL),D	; load the Value to SPD
  add 8
  ld L,A
  ld (HL),D	; load the Value to SDE
  add 8
  ld L,A
  ld (HL),D	; load the Value to SAT
  bit directplusminus,C
  jr nz,statcalc_ui_distribute_prepare_p
 statcalc_ui_distribute_p_prepared:
  ld B,D	; load Value to B
  push BC	; Push Value & Flags
  call calc_stats
  call write_rows
  call statcalc_ui_drawcursor
  pop BC	; Push Value & Flags
  bit directplusminus,C
  jr z,statcalc_waitkey	; Return if we're not in P column
  ; If we are in P column, check the value and react accordingly (value is in B now):
  dec B ; cp 1
  jr z,statcalc_ui_distribute__beneficial_nat
 statcalc_ui_distribute__hindering_nat:
  ld A,hindering
  ld (statcalc_nature),A
  ld HL,s_hindering_nature
  jr statcalc_ui_distribute__nat
 statcalc_ui_distribute__beneficial_nat:
  ld A,beneficial
  ld (statcalc_nature),A
  ld HL,s_beneficial_nature
 ;jr statcalc_ui_distribute__nat
 statcalc_ui_distribute__nat:
  ld DE,OP1
  ld BC,9
  LDIR
  ;ld A,56
  ;ld (penCol),A
  ;ld A,48
  ;ld (penRow),A
  setpenpos 56,48
  continuebuffer
  ld HL,OP1
  B_CALL VPutS
  endbuffer
  jr statcalc_waitkey	; Get outta here
 statcalc_ui_distribute_prepare_p:
  xor A
  ld (statcalc_row_hp+pers),A	; Reset HP's P
  ld A,D	; Check the Value
  inc A	; cp -1
  jr z,statcalc_set_minmaxmode_nature	; If we're setting Personality to ?MIN/MAX?, jump there & never return...
  dec A	; cp the original A to 0
  jr z,statcalc_reset_personality	; If we're setting Personality to Neutral, jump there & never return...
  jr statcalc_ui_distribute_p_prepared

statcalc_ui_number:
  push AF
  call statcalc_recall
  bit numberinput,C
  jr z,statcalc_waitkey	; don't do it if it's not wanted
  bit statcalc_entry,(IY+tempFlagByte)	; If a number is not being entered yet,
  call z,xorA							;	zero out the value
  set statcalc_entry,(IY+tempFlagByte)	; A number is being entered from now on
  cp -1
  call z,xorA
  or A
  jr z,skip_multiply	; optimization for multiplying 0 by 10
  push BC	; save contents of BC (Col/Row & Flags)
  sla A	; x2
  ld B,A
  sla A	; x4
  jp m,statcalc_ui_number_fail_pop	; pop BC & exit if we've overflown
  sla A	; x8
  jp m,statcalc_ui_number_fail_pop	; pop BC & exit if we've overflown
  add B	; x2+x8=x10
  pop BC	; recall contents of BC (Col/Row & Flags)
  jp m,statcalc_ui_number_fail	; exit if we've overflown
 skip_multiply:
  ld (IY+tempStorageByte),A		; store PrevVal*10 to tempStorageByte
  pop AF
  and ~30h	; get the number from the digit ( ~30h = ~"0" )
  add (IY+tempStorageByte)	; add the stored PrevVal*10
  jp m,statcalc_ui_number_fail	; exit if we've overflown
  call statcalc_validate
  bit statcalc_invalid,(IY+tempFlagByte)
  jr nz,statcalc_ui_number_fail	; exit if we've invalid
  call statcalc_update
  call statcalc_ui_drawcursor
  jr statcalc_waitkey
 statcalc_ui_number_fail_pop:
  pop BC
 statcalc_ui_number_fail:
  res statcalc_entry,(IY+tempFlagByte)	; Calcel number-rntry mode
  call statcalc_ui_drawcursor
  jr statcalc_waitkey

statcalc_ui_cancel_input:
  res statcalc_entry,(IY+tempFlagByte)
  call statcalc_ui_drawcursor
  jr statcalc_waitkey

statcalc_ui_letter:
  push AF
  call statcalc_recall
  pop AF	; we need the A
  bit alpha,C
  jr z,statcalc_waitkey		; return if we're not wanted
  bit nodirectaddress,C
  jr nz,statcalc_load_pokemon_from_var	; handle Pokemon
  ld HL,statcalc_natsearcher_count		; handle Nature
  bit statcalc_entry,(IY+tempFlagByte)
  jr nz,natalphasearcher_skip_zerocount
  ld (HL),0
 natalphasearcher_skip_zerocount:
  inc (HL)
  ld E,(HL)
  ld D,0
  add HL,DE
  ld (HL),A								; save the A (letter) to the buffer
  ld A,E	; load the length
  cp 7
  jp p,statcalc_natsearcher_reset_entry_bit
  set statcalc_entry,(IY+tempFlagByte)
 statcalc_natsearcher_entry_bit_reset:
  ld B,E	; init Counter of Letters
  ld C,0	; init Counter of Natures
  ld E,8	; init Increment (DE); D is zero
  ld HL,natures+1					; init Data Address
  ld IX,statcalc_natsearcher_buff	; init Buffer Address
 natsearcher_loop:
  ld A,(HL)
  cp (IX)
  jr z,natsearcher_inc_buff_and_data
  jp p,natsearcher_fail
 natsearcher_inc_data:
  add HL,DE
  inc C
  jr natsearcher_loop
 natsearcher_inc_buff_and_data:
  inc HL
  inc IX
  dec B
  jr z,natsearcher_success
  jr natsearcher_loop
 natsearcher_success:
  ld A,C
  jr statcalc_set_personality
 statcalc_natsearcher_reset_entry_bit:
  res statcalc_entry,(IY+tempFlagByte)
  jr statcalc_natsearcher_entry_bit_reset
 natsearcher_fail:
  res statcalc_entry,(IY+tempFlagByte)
  call statcalc_ui_drawcursor
  jr statcalc_waitkey


xorA:
  xor A
  ret

statcalc_set_minmaxmode_nature:
  ld A,-1	; set Personality to -1
  jr statcalc_set_personality

statcalc_personality_changer:
  ; If the same sign is already there, don't do anything:
  cp B
  jr z,statcalc_waitkey	; (return)
  ; If the opposite sign is there, set personality to Bashful:
  ld A,B					;;;;;;;; A IS DESTROYED
  or A	; Since there are only 2 possibilities now, there either is the opposite sign or no sign
  jr nz,statcalc_set_neutral_personality	; so, set Neutral on anything but "no sign"
  ; Otherwise (if there's no sign), check the current Nature.
  ; If the current nature is Neutral, jump there
  ld A,C
  or A
  jr z,statcalc_set_nature_from_neutral
  ; Non-Neutral Natures: Only change the current sign
  ld A,H	; load the target mask
  cpl A		; invert the mask (it becomes the mask to what will be kept)
  and C		; mask the current Nature
  ld C,A	; save that			;;;;;;;;; C IS DESTROYED
  ld A,D	; load the Current Row
  sla A	; x2
  sla A	; x4
  sla A	; x8
  sla A	; x16
  add D		; get +CurrentRow/-CurrentRow
  and H		; mask it with the target mask (so only what will be changed remains)
  or C		; combine that with the saved data (combine what changed w/ what was kept)
  jr statcalc_find_n_set_personality	; Find&Set
statcalc_reset_personality:
  ld D,0	; put 0 to current row; row 0 has the "neutral" neutral personality
statcalc_set_neutral_personality:	; only needs the current row in D
  ld E,D		; set the neutral personality that corresponds to current row (doesn't really
  ld D,0		;	matter, but what the heck)
  ld HL,neutral_indexes
  add HL,DE
  ld A,(HL)
  jr statcalc_set_personality
statcalc_set_nature_from_neutral:
  ; If the current nature is Neutral, check the row.
  ; If the row is 1, set this sign to 1st row and the opposite sign to the 2nd row
  ld A,D
  dec A
  jr z,statcalc_set_nature_from_neutral_in_first_row
  ; current sign in this row
  inc A	; get current row again
  sla A	; x2
  sla A	; x4
  sla A	; x8
  sla A	; x16
  or D		; load +CurrentRow/-CurrentRow
  and H		; and with the Target Mask
  ld B,A	; save the target sign in the current row	;;;;;;;; B IS DESTROYED
  ld A,H	; get the current mask
  cpl A		; invert the mask
  and 11h	; mask +FirstRow/-FirstRow with the inverted mask
  or B		; combine with saved data (target sign in current row)
  jr statcalc_find_n_set_personality	; Find&Set
statcalc_set_nature_from_neutral_in_first_row:
  ld A,11h	; load +FirstRow/-FirstRow
  and H		; mask the correct one (plus or minus)
  ld B,A	; save the 1st row	;;;;;;;; B IS DESTROYED
  ld A,H	; load the mask
  cpl A		; invert the mask
  and 22h	; mask +SecondRow/-SecondRow with the mask
  or B		; combine with the 1st row saved before
  jr statcalc_find_n_set_personality	; Find&Set

statcalc_find_n_set_personality:
  ; This finds the Nature that fits te description given in A (12h, 13h, 14h, 15h, 21h, 23h, 24h etc.,
  ;		the first byte of each entry in the 'natures:' structure), then jumps to statcalc_set_personality
  ld C,A	; stash the target value in C
  ld B,0	; initialize the counter
  ld DE,8	; load the Increment
  ld HL,natures	; initialize the pointer
 statcalc_find_personality_loop:
  ld A,(HL)	; load the value
  cp C		; compare to the target
  jr z,statcalc_personality_found		; Yipee ^^
  cp -1		; safety check
  jr z,statcalc_personality_notfound	; Whoops ^^;
  add HL,DE	; advance the pointer
  inc B		; increment counter
  jr statcalc_find_personality_loop		; Go again
 statcalc_personality_found:
  ld A,B	; put the personality index to A from the counter
  jr statcalc_set_personality	; set the personality
 statcalc_personality_notfound:
  ld A,1	; put a default personality index to A
  jr statcalc_set_personality	; set the personality

statcalc_set_personality:
  ; This "recreates" the context from statcalc_recall so that it looks like
  ;		the Nature was changed directly, then it updates that, draws the cursor, and jumps to waitkey.
  ;		Takes A (the target Nature's index), no validation is done.
  							; A (Target Value) is already loaded
  ld C,flags_nat			; C (Flags) loaded
  ld E,24					; E (Max) loaded
  ld B,statcalcUI_nature	; B (col/Row) loaded
  ld HL,statcalc_nature		; HL (pointer to value)	loaded
  ld D,(HL)					; D (Previous value) loaded
  call statcalc_update
  call statcalc_ui_drawcursor
  jr statcalc_waitkey

statcalc_validate: ; Sets/Resets the statcalc_invalid flag, and adjusts A for +/- looping
  ; NEEDS TO BE CALLED WITH (main register) CONTEXT FROM statcalc_recall, ONLY A CAN BE CHANGED!
  res statcalc_invalid,(IY+tempFlagByte)
  bit min1,C
  jr nz,statcalc_validate_check_alternate_minimum
  cp -1
 statcalc_validate_alternate_minimum_ret:
  jr z,statcalc_validate_settomax
 statcalc_validate_settomax_done:
  inc E
  cp E
  jr nc,statcalc_validate_settomin
  dec E
 statcalc_validate_settozero_done:
  ret
 statcalc_validate_settomax:
  set statcalc_invalid,(IY+tempFlagByte)
  ld A,E
  jr statcalc_validate_settomax_done
 statcalc_validate_settomin:
  set statcalc_invalid,(IY+tempFlagByte)
  bit min1,C
  jr nz,statcalc_validate_setto1
 ;jr statcalc_validate_setto0
 statcalc_validate_setto0:
  xor A
  jr statcalc_validate_settozero_done
 statcalc_validate_setto1:
  ld A,1		    
  jr statcalc_validate_settozero_done
 statcalc_validate_check_alternate_minimum:
  or A
  jr statcalc_validate_alternate_minimum_ret

statcalc_update:
  ; NEEDS TO BE CALLED WITH (main register) CONTEXT FROM statcalc_recall, ONLY A CAN BE CHANGED!
  bit statrow,C
  jr z,statcalc_update_notastatrow
  ld (HL),A
  ld A,L
  and 11111000b	; we get to the start of the row by zeroing out the lower 3 bits of the address
  ld L,A
  ld A,B
  ld C,0
  and 0Fh		; we get the row by masking it out
  push AF			; save the row
  jr z,statcalc_update_setCto1
 statcalc_update_setCto1_done:
  call do_stat	; call the calculating mechanism
  pop AF			; recall the row
  push AF			; save the row
  add A	; x2
  ld B,A
  add A	; x4
  add B	; x4+x2=x6
  add 12
  ld B,A		; store pixel-row to B
  pop AF			; recall the row
  add A	; x2
  add A	; x4
  add A	; x8
  ld C,A		; store 8*row to C
  continuebuffer
  call write_row	; call the row-writing mechanism
  endbuffer
  ret
 statcalc_update_setCto1:
  ld C,1
  jr statcalc_update_setCto1_done
 statcalc_update_notastatrow:
  bit nodirectaddress,C
  jr nz,statcalc_update_pokemon
  ld (HL),A
  bit alpha,C	; now, Nature is the only field left that has Alpha.
  jr nz,statcalc_update_nature
  call calc_stats
  call write_rows
  ret
 statcalc_update_nature:
  ; THE CONTEXT CAN BE NOW THOUGHT OF AS DESTROYED, except A (target value), which'll change momentarily
  cp -1
  jr z,statcalc_update_fulminmaxmode_personality
  ld HL,natures
  ld L,A
  sla L	; x2
  sla L	; x4
  sla L	; x8
  ld A,(HL)	; get the Nature byte
  ld B,A	; save target value in B
  ; First, zero out all P's
  xor A
  ld (statcalc_row_hp+ pers),A
 statcalc_update_fulminmaxmode_personality_mergepoint:
  ld (statcalc_row_atk+pers),A
  ld (statcalc_row_def+pers),A
  ld (statcalc_row_spd+pers),A
  ld (statcalc_row_sat+pers),A
  ld (statcalc_row_sde+pers),A
  ; Second, change the Plus Row's P
  ld A,B	; recall target value
  or A
  jr z,statcalc_uptdate_neutral_personality	; Neutral personalities skip everything else
  and 0F0h	; mask out the Plus value
  srl A	; /2 (= (A>>4)*8)
  add pers	; add the offset to Personality
  ld HL,statcalc_data
  ld L,A	; L is a direct offset since statcalc_data is nicely aligned
  ld (HL),1	; load the 1 (Plus) to HL
  ; Third, change the Minus Row's P
  ld A,B	; recall target value
  and 0Fh	; mask out the Minus value
  sla A	; x2
  sla A	; x4
  sla A	; x8
  add pers	; add the offset to Personality
  ld HL,statcalc_data
  ld L,A	; L is a direct offset since statcalc_data is nicely aligned
  ld (HL),2	; load the 2 (Minus) to HL
 statcalc_uptdate_neutral_personality:
  ; Last, recalculate & Redraw  
  call calc_stats
  call write_rows
  ret
 statcalc_update_fulminmaxmode_personality:
  ld A,-1	; Load MinMaxMode to A so it's distributed to all P's
  ld B,0	; Pretend this is a neutral personality so we skip setting +'s and -'s
  jr statcalc_update_fulminmaxmode_personality_mergepoint
 statcalc_update_pokemon:
  ret

  ; -JR-

statcalc_recall:
  ; Creates context for updating a value
  ld A,(statcalcUI_row)
  cp statcalcUI_laststatrow+1
  jp p,statcalc_recall_not_a_stat
 statcalc_recall_a_stat:
  ld A,(statcalcUI_col)
  sla A	; x2
  sla A	; x4
  sla A	; x8
  sla A	; x16
  ld B,A
  ld A,(statcalcUI_row)
  or B
  ld B,A							; B (Col/Row) is loaded
  ld A,(statcalcUI_col)
  sla A
  ld HL,statcalc_value_properties
  ld E,A
  ld D,0
  add HL,DE
  ld E,(HL)							; E (Max) is loaded
  inc HL
  ld C,(HL)							; C (Flags) is pre-loaded
  ld A,B
  cp 20h	; compare to Col/Row of HP's P
  jp z,statcalc_recall_resetplusminusbit
 statcalc_recall_resetplusminusbit_done:	; C (Flags) is adjusted for HP's P weirdness
  push BC							; BC saved
  ld HL,statcalc_row_hp
  ld B,0
  ld A,(statcalcUI_row)
  sla A	; x2
  sla A ; x4
  sla A ; x8
  ld C,A
  add HL,BC
  ld A,(statcalcUI_col)
  inc A
  ld C,A
  add HL,BC
  ld A,(HL)							; A (Value) is loaded
  ld D,A							; D (Value) is loaded
  pop BC							; BC recalled
  ret
 statcalc_recall_not_a_stat:
  cp statcalcUI_nature
  jp z,statcalc_recall_nature
  cp statcalcUI_level
  jp z,statcalc_recall_level
 ;cp statcalcUI_pokemon
 ;jp z,statcalc_recall_pokemon
 statcalc_recall_pokemon:
  ld HL,0
  ld B,statcalcUI_pokemon
  ld C,flags_pk
  ld E,24
  ld A,0
  ld D,A
  ret
 statcalc_recall_nature:
  ld HL,statcalc_nature
  ld B,statcalcUI_nature
  ld C,flags_nat
  ld E,24
  ld A,(HL)
  ld D,A
  ret
 statcalc_recall_level:
  ld HL,statcalc_level
  ld B,statcalcUI_level
  ld C,flags_lv
  ld E,100
  ld A,(HL)
  ld D,A
  ret
 statcalc_recall_resetplusminusbit:	; resets some flags for HP's P
  res plusminus,C
  jp statcalc_recall_resetplusminusbit_done

statcalc_ui_erasecursor:
  ld C,SFourSpaces
  jp statcalc_ui_drawcharatcursorpos

statcalc_ui_drawbackcursor:
  ld C,Sleft
  jp statcalc_ui_drawcharatcursorpos
statcalc_ui_check_illegal_P:
  ld A,(statcalcUI_col)
  cp 2
  jp nz,statcalc_ui_drawnormalcursor
  ld C,Sstore
  jp statcalc_ui_drawcharatcursorpos
statcalc_ui_drawentrycursor:
  ld C,">"
  jp statcalc_ui_drawcharatcursorpos
statcalc_draw_alpha:
  bit statcalc_alpha,(IY + tempFlagByte)
  jp nz,statcalc_alpha_drawn
  set textInverse, (IY + textFlags)
  ld A," "
  B_CALL VPutMap
  ld A,"A"
  B_CALL VPutMap
  res textInverse, (IY + textFlags)
  set statcalc_alpha,(IY + tempFlagByte)
  jp statcalc_alpha_drawn
statcalc_ui_minmaxmode_ev:
  ld HL,s_minmaxmode_ev
  ld DE,OP1
  ld BC,8
  LDIR
  ld HL,OP1
  B_CALL VPutS
  set statcalc_evdrawn,(IY+tempFlagByte)
  jp statcalc_ui_drawcursor_only
statcalc_ui_drawcursor:
  ; in addition to drawing the cursor, this routine also takes care of the "Alpha" A in the top right corner
  ; and the EL->EV conversion & display:
 statcalc_do_alpha: ;;;;;;;; "[A]":
  ;ld A,89
  ;ld (penCol),A
  ;xor A
  ;ld (penRow),A
  setpenpos 89,0
  ld A,(statcalcUI_row)
  cp statcalcUI_pokemon
  jp z,statcalc_draw_alpha
  cp statcalcUI_nature
  jp z,statcalc_draw_alpha
  bit statcalc_alpha,(IY + tempFlagByte)
  jp z,statcalc_alpha_drawn
  ld A," "
  B_CALL VPutMap
  ld A,SFourSpaces
  B_CALL VPutMap
  res statcalc_alpha,(IY + tempFlagByte)
 statcalc_alpha_drawn:
 statcalc_el_to_ev: ;;;;;;;; "EL->EV":
  ;xor A
  ;ld (penCol),A
  ;ld A,54
  ;ld (penRow),A		; prepare the cursor position - we're either erasing or drawing
  setpenpos 0,54
  ld A,(statcalcUI_col)
  cp statcalcUI_ev
  jp nz,statcalc_ui_drawcursor_clearEV
  ld A,(statcalcUI_row)
  cp statcalcUI_laststatrow+1
  jp p,statcalc_ui_drawcursor_clearEV
  ld HL,evis
  ld DE,OP1
  ld BC,4
  LDIR
  ld HL,OP1
  B_CALL VPutS
  ld A,(statcalcUI_row)
  sla A	; x2
  sla A	; x4
  sla A	; x8
  add ev
  ld E,A
  ld D,0
  ld HL,statcalc_data
  add HL,DE
  ld A,(HL)
  cp -1		; Are we in MinMaxMode?
  jp z,statcalc_ui_minmaxmode_ev
  sla A	; x2
  sla A	; x4
  push AF
  call draw_A_no_questions
  ld A,"-"
  B_CALL VPutMap
  pop AF
  add 3
  call draw_A_no_questions
  set statcalc_evdrawn,(IY+tempFlagByte)
  jp statcalc_ui_drawcursor_only
 statcalc_ui_drawcursor_clearEV:
  bit statcalc_evdrawn,(IY+tempFlagByte)
  jp z,statcalc_ui_drawcursor_only
  ld HL,tenbigspaces
  ld DE,OP1
  ld BC,11	; 10 big spaces + a zero
  LDIR
  ld HL,OP1
  B_CALL VPutS
  res statcalc_evdrawn,(IY+tempFlagByte)
 statcalc_ui_drawcursor_only:	;;;;;;; Draw Cursor
  bit statcalc_entry,(IY+tempFlagByte)
  jp nz,statcalc_ui_drawentrycursor
  ld A,(statcalcUI_row)
  cp A,statcalcUI_pokemon
  jp z,statcalc_ui_drawbackcursor
  ld A,(statcalcUI_row)
  or A
  jp z,statcalc_ui_check_illegal_P
 statcalc_ui_drawnormalcursor:
  ld C,Sconvert
 ;jp statcalc_ui_drawcharatcursorpos
statcalc_ui_drawcharatcursorpos:	; Expects a character to draw in C
  ld A,(statcalcUI_col)
  ld B,A
  ld A,(statcalcUI_row)
  ld E,A
  ld A,statcalcUI_laststatrow
  cp E
  jp m,statcalc_ui_drawcharatcursorpos_setBto2
 statcalc_ui_drawcharatcursorpos_Bsetto2:
  ld A,E
  cp statcalcUI_pokemon
  jp z,statcalc_ui_drawcharatcursorpos_set_pokemon_row
  sla A		; x2
  ld D,A
  sla A		; x4
  add D		; x2+x4=x6
  add 12
 statcalc_ui_drawcharatcursorpos_row_set:
  ld (penRow),A
  ld A,B
  sla A		; x2
  sla A		; x4
  ld D,A
  sla A		; x8
  add D		; x8+x4=x12
  add 28
  ld (penCol),A
  ld A,C
  B_CALL VPutMap
  ret
 statcalc_ui_drawcharatcursorpos_setBto2:
  ld B,2
  jp statcalc_ui_drawcharatcursorpos_Bsetto2
 statcalc_ui_drawcharatcursorpos_set_pokemon_row:
  xor A
  jp statcalc_ui_drawcharatcursorpos_row_set

statcalc_next_pokemon:
  B_CALL RclX
  B_CALL Plus1
  B_CALL StoX
  jp statcalc_start_without_preparing_table
statcalc_prev_pokemon:
  B_CALL RclX
  B_CALL Minus1
  B_CALL StoX
  jp statcalc_start_without_preparing_table
statcalc_load_pokemon_from_var:
  push AF
  B_CALL ZeroOP1 ; OP1 = 00000000000
  pop AF
  LD (OP1+1),A ; OP1 = var name
  push AF
  AppOnErr statcalc_loadfromvar_failed
  B_CALL RclVarSym ; OP1(/OP2) = value
  AppOffErr
  pop AF
  B_CALL CkOP1Real ; ACC = type, Z = 1 if real
  jp nz,statcalc_loadfromvar_failed
  B_CALL StoX
  jp statcalc_start_without_preparing_table

statcalc_loadfromvar_failed:
  setpenpos 0,55
  ld DE,OP1
  ld HL,fail
  ld BC,5
  ldir
  ld hl,OP1
  B_CALL VPutS
  pop AF
  B_CALL VPutMap
  set statcalc_evdrawn,(IY+tempFlagByte)
  jp statcalc_waitkey

fail:
  db "BAD:",0

statcalc_value_properties:
statrow equ			0	; A part of the Stat Rows, eg. DV, EV & P
plusminus equ		1	; +/- keys work normally (all fields have it, but it's reset for HP's P)
alpha equ			2	; Changeable via ALPHA
numberinput equ		3	; Numbers can be input heres
minmaxmode equ		4	; The special MinMaxMode can be activated on this field
nodirectaddress equ	5	; This field doesn't have a direct representation in memory (stored in TI's variable)
directplusminus equ	6	; Plus/Minus changes Nature instead
min1 equ			7	; The Minimum is 1, not 0
				; max,flags
properties_dv: db  31,(1<<minmaxmode)+(1<<numberinput)+           (1<<plusminus)+(1<<statrow)
properties_ev: db  63,(1<<minmaxmode)+(1<<numberinput)+           (1<<plusminus)+(1<<statrow)
properties_p : db   2,(1<<minmaxmode)+                            (1<<plusminus)+(1<<statrow)+(1<<directplusminus)
flags_pk equ                                           (1<<alpha)+(1<<plusminus)+               (1<<nodirectaddress)
flags_nat equ         (1<<minmaxmode)+(1<<numberinput)+(1<<alpha)+(1<<plusminus)
flags_lv equ                          (1<<numberinput)+           (1<<plusminus)+         (1<<min1)

s_question:	; includes a trailing zero (below)
 db SFourSpaces,"??",0

num_one_point_one:
 db 00h,80h,11h,[6]0
num_zero_point_nine:
 db 00h,7Fh,90h,[6]0

evis:
 db "EV:",0

tenbigspaces:
 db [10]SFourSpaces,0

s_minmaxmode_ev:
 db [2]SFourSpaces,"0-255",0

s_beneficial_nature:
 db "+BENEFIT",0
s_hindering_nature:
 db "-HINDER",SFourSpaces,0

s_statcalc_statics:
 db Sstore,"BASE\6DV\6EL\6P\6STAT"
 db "LVNATURE"

aligned_stat_names:
 db SFourSpaces,"HP",SFourSpaces
 db SFourSpaces,"ATK"
 db SFourSpaces,"DEF"
 db SFourSpaces,"SPD"
 db SFourSpaces,"SAT"
 db SFourSpaces,"SDE"

d_statcalc_clear:
 db -1,-1,0, 0,0, 0,0	; DV,EV,P,[Min],[Max]

 .align 256		; Align the Natures so that the LSB is always a direct offset...
natures:
 ; Natures are arranged in 8-byte records. The last 7 bytes are the nature name, right-padded with TFourSpaces's.
 ; The entries are arranged alphabetically by name, ending with a stopper of eight 0FFh bytes.
 ; The first byte is either 0 for Neutral natures, or it has the indexes of the stats it modifies (1=ATK,2=DEF,...)
 ; The most significant 4 bits hold the benefited stat, the leats significant 4 bits hold the hindered stat.
 db 14h,"ADAMANT"		; For example, Adamant raises ATK (1) and lowers SAT (4)
 db 00h,"BASHFUL"		; Bashful is neutral.
 db 21h,"BOLD\6\6\6"	; Bold raises DEF (2) and lowers ATK (1)
 db 13h,"BRAVE\6\6"
 db 51h,"CALM\6\6\6"
 db 54h,"CAREFUL"
 db 00h,"DOCILE\6"
 db 52h,"GENTLE\6"
 db 00h,"HARDY\6\6"
 db 32h,"HASTY\6\6"
 db 24h,"IMPISH\6"
 db 34h,"JOLLY\6\6"
 db 25h,"LAX\6\6\6\6"
 db 12h,"LONELY\6"
 db 42h,"MILD\6\6\6"
 db 41h,"MODEST\6"
 db 35h,"NAIVE\6\6"
 db 15h,"NAUGHTY"
 db 43h,"QUIET\6\6"
 db 00h,"QUIRKY\6"
 db 45h,"RASH\6\6\6"
 db 23h,"RELAXED"
 db 53h,"SASSY\6\6"
 db 00h,"SERIOUS"
 db 31h,"TIMID\6\6"
 db 00h,"NEUTRAL"
 db [8]-1				; "Stopper"
 db 00h,"BENEFIT"
 db 00h,"HINDER",SFourSpaces
minmax equ		 -1
adamant equ		 0
bashful equ		 1
bold equ		 2
brave equ		 3
calm equ		 4
careful equ		 5
docile equ		 6
gentle equ		 7
hardy equ		 8
hasty equ		 9
impish equ		10
jolly equ		11
lax equ			12
lonely equ		13
mild equ		14
modest equ		15
naive equ		16
naughty equ		17
quiet equ		18
quirky equ		19
rash equ		20
relaxed equ		21
sassy equ		22
serious equ		23
timid equ		24
neutral equ		25
stopper_nat equ	26
beneficial equ	27
hindering equ	28
;db 00h,"HARDY\6\6"
;db 12h,"LONELY\6"
;db 13h,"BRAVE\6\6"
;db 14h,"ADAMANT"
;db 15h,"NAUGHTY"
;db 21h,"BOLD\6\6\6"
;db 00h,"DOCILE\6"
;db 23h,"RELAXED"
;db 24h,"IMPISH\6"
;db 25h,"LAX\6\6\6\6"
;db 31h,"TIMID\6\6"
;db 32h,"HASTY\6\6"
;db 00h,"SERIOUS"
;db 34h,"JOLLY\6\6"
;db 35h,"NAIVE\6\6"
;db 41h,"MODEST\6"
;db 42h,"MILD\6\6\6"
;db 43h,"QUIET\6\6"
;db 00h,"BASHFUL"
;db 45h,"RASH\6\6\6"
;db 51h,"CALM\6\6\6"
;db 52h,"GENTLE\6"
;db 53h,"SASSY\6\6"
;db 54h,"CAREFUL"
;db 00h,"QUIRKY\6"

 .align 8	; start so that we can add to L only instead of HL (no carry to H occurs)
minmax_values:
 db 0,0
minmax_dv:
 db 0,31 
minmax_ev:
 db 0,63
minmax_p:
 db 1,2

zero: db 0

s_fullminmaxmode_nature:
 db "?HIN/BEN",0

neutral_indexes:
 db neutral		; HP
 db hardy		; ATK
 db docile		; DEF
 db serious 	; SPD
 db bashful 	; SAT
 db quirky		; SDE


 DEFINE P6LEVELMOVE, SPACE=ROM  
 SEGMENT P6LEVELMOVE

 extern _Green
 extern _DrawPkName
 extern _Red
 extern _Select
 extern _Move
 extern _LoadPic
 extern _LPicBank1
 extern _DrawMoveName
 extern _DrawMoveName_AlignRight
 extern _MoveList
 public MoveList
 extern _LoadLevelMoves
 extern _LoadBreedMoves
 extern _StatCalculator

 extern eggmovedatastart

 include "header.inc"

 globals on

 include "linemacros.inc"

 ; IY+asm_Flag2 is used for the total number of moves that are on screen now
 ; use (IY+totalmovesonscreen)
totalmovesonscreen equ tempFlagByte
 ; statVars is used for levelup move data
move_number equ statVars
movedata equ statVars+1
tmhmadv equ movedata
tmhmgsc equ tmhmadv+8
tmhmrby equ tmhmgsc+8
yellowextras equ tmhmrby+8
 ; tempSwapArea is used for random info:
randominfo equ tempSwapArea
mode_screen equ randominfo	; two bytes; usually loaded to BC together
mode equ mode_screen+0			; modes: 0=Level-Up	1=Tutor	2=TM/HM	3=Breed
levelup equ 0
tutor equ 1
tmhm equ 2
breed equ 3
screen equ mode_screen+1	; The current screen

pixrep_total equ randominfo+2	; two bytes, usually loaded to DE
pixrep equ pixrep_total+0	; How many pixels represent 1 screen on the "page line"
total equ pixrep_total+1	; total # of screens

noofmoves equ randominfo+4
tmhm_temp_total equ randominfo+5	; The total number of screens, used temporarily until loaded to (total)

currpos equ randominfo+6	; Current "cursor" position

; appBackUpScreen is used for listing TM/HM moves that are on screen now (to be filled in the drawing routine)
tmhmlistforcurrentpokemon equ appBackUpScreen
 ; saveSScreen IS USED FOR TEMPORARY STORAGE!


; Begin Program
MoveList:
  B_CALL ClrLCDFull
  set showpkmn, (IY+dexstate)
  ld A,levelup
  ld (mode),A
 movelist_noresetmode:	; doesn't reset screen also if mode is TM/HM
  ld A,(mode)
  cp tmhm
  jp z,movelist_noresetscreen
  xor A
  ld (screen),A
 movelist_noresetscreen:
  ; We will reset the current position
  xor A
  ld (currpos),A

  AppOnErr failed_to_get_x
  B_CALL RclX			; get our X
  ld HL,385
  B_CALL SetXXXXOP2
  B_CALL CpOP1OP2
  jp z,chimechox
  jp nc,failed_to_get_x
 chimechox:
  AppOffErr
  B_CALL RclX		; get our X again
  B_CALL ConvOP1	; put X to DE
  push DE

  startbuffer

  call drawsplash

  pop DE
  ; don't push DE - not needed any more

  ld A,(mode)
  or A
  jp z,ld_levelmoves
  dec A
  jp z,ld_tutor
  dec A
  jp z,ld_tmhm
 ;dec A
  jp ld_breedmoves
 ld_levelmoves:
  B_CALL LoadLevelMoves
  jp movesloaded
 ld_breedmoves:
  ld HL,eggmovedataoffsets
  add HL,DE
  add HL,DE
  ld E,(HL)
  inc HL
  ld D,(HL)
  ld A,E
  or A
  jp nz,allisright
  ld A,D
  or A
  jp nz,allisright
 zeromoves:
  xor A
  ld (statVars),A
  jp movesloaded
 allisright:
  ex HL,DE
  B_CALL LoadBreedMoves
  jp movesloaded
 ld_tutor:
  call loadtutormoves
  jp movesloaded
 ld_tmhm:
  call loadmachinemoves
 movesloaded:

  B_CALL RclX		; get our X again
  xor A
  ld (penRow),A
  ld (penCol),A
  B_CALL DrawPkName

  ld A,(statVars)
  ld (noofmoves),A
  cp 32+1
  jp P,five
  cp 24+1
  jp P,four
  cp 16+1
  jp P,three
  cp 8+1
  jp P,two
  ; else, one
 one:
  ld DE,1*256+(12/1)
  jr endnos
 three:
  ld DE,3*256+(12/3)
  jr endnos
 four:
  ld DE,4*256+(12/4)
  jr endnos
 five:
  ld DE,5*256+(12/5)
  jr endnos
 two:	; Two's the most common
  ld DE,2*256+(12/2)
 ; jr endnos
 endnos:
  ld (pixrep_total),DE

 drawscreen_pokemonloaded: ; call this if only mode or screen number were changed
  continuebuffer
  ld BC,(mode_screen)
  ld DE,(pixrep_total)
  ld A,(noofmoves)
  call drawscreen

  call drawstatics

  endbuffer

  ld A,(statVars)
  or A
  jp z,waitkey		; if no moves, don't mess with cursor
  ld HL,currpos
  jp cursor_update_check_upperbound	; this jumps us to waitkey eventually, but checks correct cursor position
  														; before that

drawsplash:
 ; B_CALL ForceFullScreen  
 ; B_CALL ClrLCDFull ; clear screen
 ; B_CALL GrBufClr   ; clear buffer

  ld H,1
  putline 19, 7,52, 7
  putline 54, 7,93, 7
  putline  0, 7, 0,11
  putline 19, 7,19,11
  putline  1,12,18,12
  putline 45, 3,56, 3

  ld A,(mode)
  cp tmhm
  jr z,skip_long_vertical_line
  putline 53, 8,53,62
 skip_long_vertical_line:

  putline 42, 3,43, 3
  putline 43, 2,43, 4
  putline 58, 3,59, 3
  putline 58, 2,58, 4

  ret  

drawstatics:
  ld A,(mode)
  or A
  jp z,drawstatics_levelup
  dec A
  jp z,drawstatics_tutor
  dec A
  jp z,drawstatics_tmhm
 ;dec A
 ;jp drawstatics_breed
 drawstatics_breed:
  ld HL,p_tabs2
  ld DE,OP1
  ld BC,p_tabs2_end-p_tabs2
  LDIR
  ld hl,OP1							; HL points to copied image
  ld de,7*256+2					; DE <- coords
  B_CALL DisplayImage

  ld H,1
  putline 74, 7,74,14
  putline 84, 7,84,14

  ld HL,s_breed
  ld DE,OP1
  ld BC,9
  LDIR
  putOP1sn 27, 8, 0, 5
  putOP1sn 70, 8, 5, 1
  putOP1sn 76, 8, 6, 2
  putOP1sn 86, 8, 8, 1
  ret

 drawstatics_tutor:
  ld HL,p_tabs3
  ld DE,OP1
  ld BC,p_tabs3_end-p_tabs3
  LDIR
  ld hl,OP1							; HL points to copied image
  ld de,7*256+2					; DE <- coords
  B_CALL DisplayImage

  ld H,1
  putline 74, 7,74,14
  putline 84, 7,84,14

  ld HL,s_tutor
  ld DE,OP1
  ld BC,10
  LDIR
  putOP1sn 25, 8, 0, 6
  putOP1sn 70, 8, 6, 1
  putOP1sn 76, 8, 7, 2
  putOP1sn 86, 8, 9, 1
  ret
 drawstatics_tmhm:
  ld HL,p_tabs4
  ld DE,OP1
  ld BC,p_tabs4_end-p_tabs4
  LDIR
  ld hl,OP1							; HL points to copied image
  ld de,7*256+2					; DE <- coords
  B_CALL DisplayImage

  ld H,1
  putline 19, 7,93, 7

  ld HL,s_tmhm
  ld DE,OP1
  ld BC,10
  LDIR
  putOP1sn 21, 8, 0, 5
  ld A,(total)
  ld C,A
  putOP1sn 66, 0, 5, 1
  dec C
  jp z,skip_other_tmhm_headers
  putOP1sn 74, 0, 6, 2
  dec C
  jp z,skip_other_tmhm_headers
  putOP1sn 86, 0, 8, 2
 skip_other_tmhm_headers:
  ld HL,s_nomove_selected
  ld DE,OP1
  ld BC,12
  LDIR
  putOP1sn 46, 8, 0, 12
  ret
 drawstatics_levelup:
  ld HL,p_tabs1
  ld DE,OP1
  ld BC,p_tabs1_end-p_tabs1
  LDIR
  ld hl,OP1							; HL points to copied image
  ld de,7*256+2					; DE <- coords
  B_CALL DisplayImage

  ld H,1
  putline 64, 7,64,14
  putline 78, 7,78,14
  putline 84, 7,84,14

  ld HL,s_lvup
   ld A,(total) 			; DEOXYS WEIRDNESS
   cp 4 						; DEOXYS WEIRDNESS
   call z,preparedeoxys ; DEOXYS WEIRDNESS
  ld DE,OP1
  ld BC,15
  LDIR
  putOP1sn 21, 8, 0, 8
  putOP1sn 56, 8, 8, 2
  putOP1sn 70, 8,10, 2
  putOP1sn 80, 8,12, 1
  putOP1sn 86, 8,13, 2

  ret

preparedeoxys: 		; DEOXYS WEIRDNESS
   ld HL,s_deoxys 	; DEOXYS WEIRDNESS
   ret 					; DEOXYS WEIRDNESS

drawscreen:
  ; total # of screens in D, pixels to represent 1 screen in E, screen number in B, mode in C, total # of moves in A
  ; preserves DE & BC
  push AF
  push DE
  push BC

  ld (IY+totalmovesonscreen),00	; Init the Moves-On-This Screen counter

  ; Check for TM/HM mode - this mode draws totally differently
  ld A,C
  cp tmhm
  jp z,drawscreen_tmhm


  ld H,0	; white line
  putline 45, 2,56, 2
  putline 45, 4,56, 4
  pop BC
  pop DE
  pop AF
  push DE
  push AF
  push BC
  ld A,B
  or A
  jp z,addloopend
  xor A
 addloop:
  add E
  dec B
  jp nz,addloop
 addloopend:
  add 45
  ld B,A
  add E
  dec A
  ld D,A
  ld H,1	; dark line
  ld C,63-2
  ld E,63-2
  B_CALL ILine
  ld C,63-4
  ld E,63-4
  B_CALL ILine
  pop BC
  pop AF
  push BC
  push AF
  ld A,62
  ld (penCol),A
  xor A
  ld (penRow),A
  ld A,B
  sla A	; x2
  sla A	; x4
  sla A	; x8
  inc A	; add 1 for human-friendly numbering
  call draw2digits
  ld A,'-'
  B_CALL VPutMap
  ld A,B
  inc A	; this effectively adds 8
  sla A	; x2
  sla A	; x4
  sla A	; x8
  pop BC ; this popped out what was supposed to be AF
  push BC
  cp B ; if the max is bigger, output the max
  jp m,drawscreenend ; else, give end of this screen
  ld A,B
 drawscreenend:
  call draw2digits
  ld A,'/'
  B_CALL VPutMap
  pop AF

  or A
  jp z,draw2zeros_announcezeromoves_pop2times

  ; don't push AF - not needed any more
  call draw2digits

  pop BC		; screen number/mode
  push BC
  ld A,C		; stash mode number away in A
  sla B	; x2
  sla B	; x4
  sla B	; x8 - 8 moves per screen
  ld C,B
  ld B,0
  ld HL,statVars+1
  add HL,BC ; 1
  add HL,BC ; 2
  or A					; query the stashed mode
  jr nz,skipthirdadd	; if mode is not 0, two bytes per record are enough...
  add HL,BC ; 3 - 3 bytes total per record
 skipthirdadd:
  pop BC		; screen number/mode
  push BC
  ld B,8	; start the counter
  ; -MODE CHECK-
  ld A,C
  or A	; level-up
  jp z,drawscreenloop
  dec A	; Tutor
 jp z,drawscreen_breed ;it's the same as Breed
  dec A	; TM/HM
 ;jp z,drawscreen_tmhm  ;not implemented
  jp drawscreen_breed
 drawscreenloop:
  push BC	; save B, the counter; and C, the mode number
  ld A,8		; Get the number of the line we're to write on (0,1,2,3,4,5,6,7)
  sub B		;    We're getting it from the countdown value (8,7,6,5,4,3,2,1) - so use 8-B -> A
  sla A	; x2
  ld B,A			; store the 2n
  sla A	; x4
  add B			; add the 4n to the 2n --> 6n (6 is the height of a row)
  add 14			; add 14, the penRow of the first move
  ld (penRow),A	; load 6n+14 to pen row.

  							; load first byte.
  ld D,(HL)		; load first movedata byte to D, deal with it later
  inc HL					; load next byte (2nd)
  ld A,(HL)		; load second movedata byte to Acc for safe keeping
  inc HL					; load next byte (3rd)
  push HL	; save HL, the address we're at now
  ld L,(HL)		; load third movedata byte to L since the address in HL is not needed from now on
  ld E,A		; put second movedata byte to E so the firts two bytes are in DE
  push DE	; save DE, the first two bytes of the movedata structure
  and A,1		; isolate the MSb of the move index
  ld H,A			; put the MSb in H so that HL now contains the whole move index
  dec HL			; whoops... Forgot to convert indexes when converting the data...
  ld A,H
  cp -1
  jp z,exit_screenloop_cleanup
  B_CALL SetXXXXOP2	; put move index into OP2
  B_CALL OP2ToOP1		; put move index into OP1
  ld A,4
  ld (penCol),A	; set cursor column
  B_CALL DrawMoveName	; draw the move name
  inc (IY+totalmovesonscreen)

  ld A,66
  ld (penCol),A
  ld HL,thirteenspacesandazero+13-3
  ld DE,OP1
  ld BC,4
  LDIR
  ld HL,OP1
  B_CALL VPutS

  pop DE	; restore DE, the first two bytes of the movedata structure
  push DE	; save first two bytes
  srl E	; >>1
  srl E	; >>2
  srl E	; >>3
  srl E	; >>4
  srl E	; >>5
  sla D	; <<1
  sla D	; <<2
  sla D	; <<3
  ld A,D
  and 00001000b
  add E
  jp z,frlg_same
  dec A
  jp z,notinrsefrlg
  dec A
  jp z,rseonly
  dec A
  jp z,frlgeonly
  dec A
  ld E,A
  add A
  add E
  ld HL,s_frlgplusminus
  ld E,A
  xor A
  ld D,A
  add HL,DE
  ;ex HL,DE
  ld DE,OP1
  ld BC,3
  LDIR
  ld A,66
  ld (penCol),A
  ld B,3
  ld HL,OP1
  B_CALL VPutSN
  ld A,80
  ld (penCol),A
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
  jp drawlevel
 frlg_same:
  ld A,74
  ld (penCol),A
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
  ld A,80
  ld (penCol),A
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
  jr drawlevel
 notinrsefrlg:
  ld A,74
  ld (penCol),A
  ld A,SFourSpaces
  B_CALL VPutMap
  ld A,80
  ld (penCol),A
  ld A,SFourSpaces
  B_CALL VPutMap
  jr drawlevel
 rseonly:
  ld A,74
  ld (penCol),A
  ld A,SFourSpaces
  B_CALL VPutMap
  ld A,80
  ld (penCol),A
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
  jr drawlevel
 frlgeonly:
  ld A,74
  ld (penCol),A
  ld A,10h ; square root ("checkmark")
  B_CALL VPutMap
  ld A,80
  ld (penCol),A
  ld A,SFourSpaces
  B_CALL VPutMap
 ;jr drawlevel

 drawlevel:
  ld A,56
  ld (penCol),A
 drawlevelatcurrentposition:
  pop DE	; restore DE, the first two bytes of the movedata structure
  push DE	; save first two bytes
  srl D	; get the level
  ld A,D
  cp 1
  jr z,firstlevel
  cp 01111111b
  jr z,special
  call draw2digits
  jr leveldrawn
 firstlevel:
  ld A,'S'
  B_CALL VPutMap
  ld A,'t'
  B_CALL VPutMap
  jp leveldrawn
 special:
  ld A,'S'
  B_CALL VPutMap
  ld A,'p'
  B_CALL VPutMap
 leveldrawn:

  pop DE	; restore first 2 bytes
  ld A,E
          ;    GCRY
          ;    S B 
          ; 01234567
  and       00011000b ; GSC
  jp z,gsc_no
  cp        00011000b ; GSC
  jp z,gsc_yes
  cp        00010000b ; GS
  jp z,gsc_gs
 ;cp        00001000b ; C
 ;jp z,gsc_c
  ld B,"C"
  jp gsc_end
 gsc_gs:
  ld B,"F"
  jp gsc_end
 gsc_yes:
  ld B,10h ; square root ("checkmark")
  jp gsc_end
 gsc_no:
  ld B,SFourSpaces
 ;jp gsc_end
 gsc_end:

  ld A,E
          ;    GCRY
          ;    S B 
          ; 01234567
  and       00000110b ; RBY
  jp z,rby_no
  cp        00000110b ; RBY
  jp z,rby_yes
  cp        00000100b ; RB
  jp z,rby_rb
 ;cp        00000010b ; Y
 ;jp z,rby_y
  ld C,"Y"
  jp rby_end
 rby_rb:
  ld C,"F"
  jp rby_end
 rby_yes:
  ld C,10h ; square root ("checkmark")
  jp rby_end
 rby_no:
  ld C,SFourSpaces
 ;jp rby_end
 rby_end:
  ld A,86
  ld (penCol),A
  ld A,B
  B_CALL VPutMap
  ld A,C
  B_CALL VPutMap

  pop HL	; restore HL, the address we're at now
  inc HL	; increment HL once more, for a total of 3 increments per pass. Now HL is at 1st byte of next entry.
  pop BC	; restore B, the counter; and C, the mode number
  dec B
  jp nz,drawscreenloop	; decrement B, jump if still non-zero
  
 exit_screenloop:
  pop BC
  pop DE
  ret
 exit_screenloop_cleanup:
  pop DE
  pop HL
  pop BC
 cleanup_loop:
  push BC	; save B, the counter; and C, the mode number
  ld A,8		; Get the number of the line we're to write on (0,1,2,3,4,5,6,7)
  sub B		;    We're getting it from the countdown value (8,7,6,5,4,3,2,1) - so use 8-B -> A
  sla A	; x2
  ld B,A			; store the 2n
  sla A	; x4
  add B			; add the 4n to the 2n --> 6n (6 is the height of a row)
  add 14			; add 14, the penRow of the first move
  ld (penRow),A	; load 6n+14 to pen row.
  xor A
  ld (penCol),A
  ld HL,thirteenspacesandazero
  ld DE,OP1
  ld BC,14+11
  LDIR
  ld HL,OP1
  B_CALL VPutS
  ld A,56
  ld (penCol),A
  ld HL,OP1+14 ; 9 spaces and a zero
  B_CALL VPutS

  pop BC
  dec B
  jp z,exit_screenloop

  jp cleanup_loop
 draw2zeros_announcezeromoves_pop2times:
  ld A,"0"
  B_CALL VPutMap
  ld A,"0"
  B_CALL VPutMap
  call announcezeromoves
  pop AF	; pop #1
  pop AF	; pop #2

  ret

 announcezeromoves:
  ld A,26
  ld (penCol),A
  ld A,26
  ld (penRow),A
  ld HL,s_nomoves
  ld DE,OP1
  ld BC,8+12
  LDIR
  ld HL,OP1
  ld B,8
  B_CALL VPutSN
  ld A,35
  ld (penCol),A
  ld A,35
  ld (penRow),A
  ld HL,OP1+8
  ld B,12
  B_CALL VPutSN
  ret

  ; This is an alternate loop used for Mode 3 (Egg Moves) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 drawscreen_breed:
  push BC	; save B, the counter; and C, the mode number
  ld A,8		; Get the number of the line we're to write on (0,1,2,3,4,5,6,7)
  sub B		;    We're getting it from the countdown value (8,7,6,5,4,3,2,1) - so use 8-B -> A
  sla A	; x2
  ld B,A			; store the 2n
  sla A	; x4
  add B			; add the 4n to the 2n --> 6n (6 is the height of a row)
  add 14			; add 14, the penRow of the first move
  ld (penRow),A	; load 6n+14 to pen row.

  							; load first byte.
  ld D,(HL)		; load first movedata byte to D, deal with it later
  inc HL					; load next byte (2nd)
  push HL	; save HL, the address we're at now
  ld L,(HL)		; load second movedata byte to L
  ld E,L		; put second movedata byte to E so the firts two bytes are in DE
  push DE	; save DE, the two bytes of the movedata structure
  ld A,D
  and A,1		; isolate the MSb of the move index
  ld H,A			; put the MSb in H so that HL now contains the whole move index
  dec HL			; whoops... Forgot to convert indexes when converting the data...
  ld A,H
  cp -1
  jp z,exit_screenloop_cleanup	; it's all right to jump into another function's subroutine here....?
  B_CALL SetXXXXOP2	; put move index into OP2
  B_CALL OP2ToOP1		; put move index into OP1
  ld A,4
  ld (penCol),A	; set cursor column
  B_CALL DrawMoveName	; draw the move name
  inc (IY+totalmovesonscreen)

  ld A,54			; clear the "check" area
  ld (penCol),A
  ld HL,thirteenspacesandazero
  ld DE,OP1
  ld BC,14
  LDIR
  ld HL,OP1
  B_CALL VPutS

  pop DE	; restore DE, the two bytes of the movedata structure
  push DE	; save two bytes again
  ld A,D
  and 00001000b	; R/S/E/FR/LG
  jp z,skip_breed_rsefl
  ld A,69
  ld (penCol),A
  ld A,10h ; Square root ("checkmark")
  B_CALL VPutMap
 skip_breed_rsefl:
  pop DE	; restore DE, the two bytes of the movedata structure
  push DE	; save two bytes again
  ld A,D
  and 00000100b	; G/S
  jp z,skip_breed_gs
  ld A,77
  ld (penCol),A
  ld A,10h ; Square root ("checkmark")
  B_CALL VPutMap
 skip_breed_gs:
  pop DE	; restore DE, the two bytes of the movedata structure
  push DE	; save two bytes again
  ld A,D
  and 00000010b	; C
  jp z,skip_breed_c
  ld A,85
  ld (penCol),A
  ld A,10h ; Square root ("checkmark")
  B_CALL VPutMap
 skip_breed_c:

  pop DE

  pop HL	; restore HL, the address we're at now
  inc HL	; increment HL once more, for a total of 2 increments per pass. Now HL is at 1st byte of next entry.
  pop BC	; restore B, the counter; and C, the mode number
  dec B
  jp nz,drawscreen_breed ;decrement B, jump if still non-zero
  jp exit_screenloop	; it's all right to jump into another function's subroutine here....?


draw2digits:
  ; draws A as a decimal number 0-99 at current pen position
  ; destroys all registers but BC
  push BC
  or A
  jp z,twozeroes
  B_CALL SetXXOP1
  B_CALL FormBase
  ld A,C
  dec A
  jp nz,twonumbers
  ld A,"0"
  B_CALL VPutMap
  ld A,(OP3)
  jp onenumberonly
 twonumbers:
  ld A,(OP3)
  B_CALL VPutMap
  ld A,(OP3+1)
 onenumberonly:
  B_CALL VPutMap
  pop BC
  ret
 twozeroes:
  ld A,"0"
  B_CALL VPutMap
  ld A,"0"
  B_CALL VPutMap
  ret

cleanup:
 ;ld DE,randominfo
 ;ld HL,alotofspaces
 ;ld BC,128
 ;LDIR
  ret

drawscreen_tmhm:
  call loadmachinemoveinfo	; load learnable-move number information about the current TM/HM screen
  pop BC	; Already pushed 
  pop DE	; Already pushed
  pop AF	; Already pushed
  ld A,(tmhm_temp_total)
  ld (total),A	; Set # of screens
  ; Not useful: total # of screens (=bogus) in D, pixels to represent 1 screen in E, screen number in B, mode (=3) in C
  ; Total # of moves is in A, but that's bogus too

  ; Clear the screen
  ld A,80h+14
  B_CALL ClearRow
  ld A,80h+22
  B_CALL ClearRow
  ld A,80h+30
  B_CALL ClearRow
  ld A,80h+38
  B_CALL ClearRow
  ld A,80h+46
  B_CALL ClearRow
  ld A,80h+54
  B_CALL ClearRow
  ld HL,thirteenspacesandazero	; Erase everything on the top line  where the "screen arrow" [(see below)] goes
  ld DE,OP1
  ld BC,11
  LDIR
  putOP1sn 62, 0, 0, 8

  ; Draw the "screen arrow" (->A GS RY on top of screen)
  xor A
  ld B,A
  ld (penRow),A
  ld A,(screen)
  ld C,A
  ld HL,tmhm_screen_arrow_position
  add HL,BC
  ld A,(HL)
  ld (penCol),A
  ld A,Sconvert
  B_CALL VPutMap	; BC left intact (HL too, btw)
  ld HL,tmhm_screen_data_offsets
  add HL,BC
  add HL,BC
  ld E,(HL)
  inc HL
  ld D,(HL)
  ex HL,DE	; init the nowaddress

  ld BC,0408h		; init the Column & Byte counter
  ld D,1				; init the TM/HM number
  ld A,14
  ld (penRow),A	; init the row
 tmhm_byteloop:
  ld E,80h	; init the TM/HM # and the In-Byte counter/mask
 tmhm_bitloop:
  ld A,B
  ld (penCol),A	; init the column, again...
  ld A,(HL)			; load the Now-@ Byte Value
  and E			; Mask the byte
  jp z,skip_drawing_tm_digits
  inc (IY+totalmovesonscreen)			; Increment Total TM/HM No. on this screen
;;; DRAW DIGITS {
  push BC
  push HL
  push DE
  ld A,D
  cp 10
  jp p,tmhm_draw_two_digits
  ld A,"0";SFourSpaces
  B_CALL VPutMap
  pop AF	; popping what was DE to AF (D holds the number to write)
  push AF	; pushing pack w/o change
  or "0"
  B_CALL VPutMap
  jp tmhm_pop_out_saved_data
 tmhm_draw_two_digits:
  push AF
  B_CALL SetXXOP1
  pop AF
  DAA
  B_CALL FormBase			; OP3 and OP+1 contain digits
  pop AF	; popping what was DE to AF (D holds the number to write)
  push AF	; pushing pack w/o change
  cp 51	; compare to 51 (which is HM #1; drawn as H1)
  jp p,load_h_symbol	; load H instead of the first digit for HMs
  ld A,(OP3)	; otherwise load first digit
  jp first_digit_loaded
 load_h_symbol:
  ld A,"H"
 ;jp first_digit_loaded
 first_digit_loaded:
  B_CALL VPutMap
  ld A,(OP3+1)		; load second digit
  B_CALL VPutMap
 tmhm_pop_out_saved_data:
  pop DE
  pop HL
  pop BC
;;; } DRAW DIGITS
  ld A,(penRow)
  add 6
  ld (penRow),A	; advance to next row
  cp 58
  jp m,dont_advance_column
  ld A,B
  add 12
  ld B,A		; advance the Column
  ld A,14
  ld (penRow),A	; init the row
 dont_advance_column:
 skip_drawing_tm_digits:
  inc D	; advance TM/HM Number
  srl E		; advance the In-Byte counter/mask
  jp nz,tmhm_bitloop	 ; If mask didn't run out, go again...
  inc HL		; move to next byte
  dec C
  jp nz,tmhm_byteloop	; If not @end, go again

  ld A,(IY+totalmovesonscreen)		; save how many moves are on this screen
  or A
  call z,announcezeromoves	; if this is called we don't care about B (column) anyway
  ; DO NOT DESTROY B HERE
  ; Do R/B <-> Y Changes & return from here
  ld A,(screen)
  cp 2
  ret nz	; do nothing if not R/B/Y
  ld A,(yellowextras)
  or a
  ret z	; do nothing if no RB/Y changes
  ld A,B ; load the Column
  add 12
  ld B,A
  ld A,14
  ld (penRow),A
  ld HL,yellow_changes
  ld A,(yellowextras)
  ld E,A
  ld D,0
  add HL,DE
  add HL,DE
  ld E,(HL)
  inc HL
  ld D,(HL)
  ex HL,DE
  ld A,B
  ld DE,saveSScreen
  ld BC,s_plus_many_end-s_plus_many
  LDIR
  ld B,A
  ld HL,saveSScreen
  ld (penCol),A
  B_CALL VPutS		; Row 1
  ld A,20
  ld (penRow),A
  ld A,B
  ld (penCol),A
  B_CALL VPutS		; Row 2
  ld A,26
  ld (penRow),A
  ld A,B
  ld (penCol),A
  B_CALL VPutS		; Row 3
  ld A,32
  ld (penRow),A
  ld A,B
  ld (penCol),A
  B_CALL VPutS		; Row 4
  ld A,38
  ld (penRow),A
  ld A,B
  ld (penCol),A
  B_CALL VPutS		; Row 5
  ld A,44
  ld (penRow),A
  ld A,B
  ld (penCol),A
  B_CALL VPutS		; Row 6
  ld A,50
  ld (penRow),A
  ld A,B
  ld (penCol),A
  B_CALL VPutS		; Row 7
  ld A,56
  ld (penRow),A
  ld A,B
  ld (penCol),A
  B_CALL VPutS		; Row 8
  ;TODO
  ; DO NOT INSERT MORE CODE HERE; except for a few special cases (RB/Y changes) we don't even get here
  ret

drawmachinename:
  ld A,46
  ld (penCol),A
  ld A,8
  ld (penRow),A
  ld HL,thirteenspacesandazero+1
  ld DE,OP1
  ld BC,13
  LDIR
  ld HL,OP1
  B_CALL VPutS
  ld A,46
  ld (penCol),A
  ld HL,tmhmlistforcurrentpokemon
  ld A,(currpos)
  ld E,A
  xor A
  ld D,A
  add HL,DE
  add HL,DE
  ld D,(HL)
  inc HL
  ld E,(HL)
  ex HL,DE
  dec HL	; convert move index, again XD
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  B_CALL DrawMoveName_AlignRight
  ret

loadmachinemoves:
  push DE
  xor A
  ld (statVars),A
  ld (statVars+1),A
  ld (statVars+2),A
  ld HL,0000
  add HL,DE	; x1
  add HL,DE	; x2
  add HL,DE	; x3
  add HL,DE	; x4
  push HL
  pop DE
  add HL,DE	; x8
  push HL		; save pokeindex*8 for later
  ld DE,tmhm_rs
  add HL,DE
  ld DE,tmhmadv
  ld BC,8
  LDIR
  pop HL
  push HL
  ld DE,tmhm_gs
  add HL,DE
  ld DE,tmhmgsc
  ld BC,8
  LDIR
  pop HL
  ld DE,tmhm_rb
  add HL,DE
  ld DE,tmhmrby
  ld BC,7
  LDIR
  xor A
  ld (tmhmrby+7),A
  ld A,(HL)
  ld (yellowextras),A
  pop DE
  ld A,D
  or A
  jp nz,tmhm_set_screen_number_for_thirdgen_pkmn
  ld A,E
  or A
  jp z,tmhm_set_screen_number_for_firstgen_pkmn
  dec A
  srl A
  cp 251>>1
  jp p,tmhm_set_screen_number_for_thirdgen_pkmn
  cp 151>>1
  jp p,tmhm_set_screen_number_for_secondgen_pkmn
 tmhm_set_screen_number_for_firstgen_pkmn:
  ld B,3
  jp tmhm_screen_number_set
 tmhm_set_screen_number_for_secondgen_pkmn:
  ld B,2
  jp tmhm_screen_number_set
 tmhm_set_screen_number_for_thirdgen_pkmn:
  ld B,1
 ;jp tmhm_screen_number_set
 tmhm_screen_number_set:
  ld A,(screen)
  cp B
  jp p,tmhm_adjust_screen_number_too
  ld A,B
  ld (tmhm_temp_total),A
  ret
 tmhm_adjust_screen_number_too:
  ld A,B
  ld (tmhm_temp_total),A
  dec A
  ld (screen),A
  ret

loadmachinemoveinfo: ; This really is just a different init routine that jumps to loadtutormoves to
								; do the job...
  DI			; copied from loadtutormoves
  push IY
  push IX
  ld A,(screen)
  dec A
  jp z,load_gsc_to_IX
  dec A
  jp z,load_rby_to_IX
 load_rse_to_IX:
  ld IX,tms_rse
  jp IX_loaded
 load_gsc_to_IX:
  ld IX,tms_gsc
  jp IX_loaded
 load_rby_to_IX:
  ld IX,tms_rby
 ;jp IX_loaded
 IX_loaded:
  ld A,(screen)
  add A	; x2
  add A	; x4
  add A	; x8
  ld E,A
  xor A
  ld D,A
  ld HL,tmhmadv	; different address than in loadtutormoves
  add HL,DE	; here we load moves from the current page only
  ld DE,tmhmlistforcurrentpokemon
  ld IY,IY_counter_bytes+8
  ld C,0
  jp byteloop

loadtutormoves:
  DI ; Disable interrupts since they depend on IY
  push IY	; Saving TI's treasured flag register allows us to destroy it
  push IX	
  ld HL,tutordata
  add HL,DE	; 5 bytes per record
  add HL,DE
  add HL,DE
  add HL,DE
  add HL,DE	; HL now points to pk-data structure
  ld DE,statVars+1	; DE points to destination
  ld IX,tutormoves		; IX points to move data
  ld IY,IY_counter_bytes+5	; Set up main counter
  ld C,0		; set up total move count
 byteloop:
  ld B,10000000b	; Set B to the first byte
 bitloop:
  ld A,(HL)		; Get current byte from pk-data
  and B			; Mask by Mask
  jp z,skiploading	; Don't do anything if the pk doesn't learn the move
  ; The loading consists of moving two data bytes from the Move index to the DEst index, then incrementing
  ;  the total move count
  ld A,(IX)
  ld (DE),A
  inc DE
  inc IX
  ld A,(IX)
  ld (DE),A
  inc DE
  inc C	; total move count +
  jp skipinc 	; IX was already incremented once
 skiploading:
  inc IX 
 skipinc:
  inc IX		; Increment the move index
  srl B		; advance mask
  jp nz,bitloop	; go again if the mask is still valid
  inc HL		; increment the pk-data pointer
  dec IY		; decrement main counter
  ld A,(IY)
  or A
  jp nz,byteloop	; go again if the main counter is still nonzero
  xor A		; two more zero bytes to provide a cutoff point
  ld (DE),A
  inc DE
  ld (DE),A
  ld A,C		; record the move count
  ld (statVars),A

  pop IX
  pop IY		; There... TI won't know anything ^^
  EI ; Enable interrupts again
  ret

draw_character_at_current_arrow_position:
  ; assumes D holds the character to draw; destroys all registers but BC and HL
  ; row=(A%8)*6+14
  ld A,(currpos)
  and 7	; %8
  ld E,A
  add E	; x2
  add E	; x3
  ld E,A
  add E	; x6
  add 14
  ld (penRow),A
  ; column=int(A/8)*12
  ; column=int(A/8)*4   *3
  ; column=   (A/2 & ~3)*3
  ld A,(currpos)
  srl A	; /2
  and ~3	; &~3
  ld E,A
  add E	; x2
  add E	; x3
  ld (penCol),A
  ld A,D
  B_CALL VPutMap
  ret

p_tabs1: ; level-up
  db 7	; Height
  db 16	;Width
  db 11101010b,10100110b
  db 01001011b,10100101b
  db 01001010b,10100110b
  db 01001010b,10100101b
  db 00001000b,00100110b
  db 00010100b,01010000b
  db 11100011b,10001111b
p_tabs1_end:

p_tabs2:	; breed
  db 7	; Height
  db 16	;Width
  db 10001011b,10100101b
  db 10001001b,00100111b
  db 10001001b,00100101b
  db 11101001b,00100101b
  db 00001000b,00100000b
  db 00010100b,01010000b
  db 11100011b,10001111b
p_tabs2_end:

p_tabs3:	; tutor
  db 7	; Height
  db 16	;Width
  db 10001010b,10100110b
  db 10001011b,10100101b
  db 10001010b,10100110b
  db 11101010b,10100101b
  db 00001000b,00100110b
  db 00010100b,01010000b
  db 11100011b,10001111b
p_tabs3_end:

p_tabs4:	; tmhm
  db 7	; Height
  db 16	;Width
  db 10001011b,10100110b
  db 10001001b,00100101b
  db 10001001b,00100110b
  db 11101001b,00100101b
  db 00001000b,00100110b
  db 00010100b,01010000b
  db 11100011b,10001111b
p_tabs4_end:

s_lvup: db "LEVEL-UP"	; + 0  L 8
        db "LV"			; + 8  L 2
		  db "FL"			; +10  L 2
		  db "A"				; +12  L 1
		  db "21"			; +13  L 2
		  						;     TL15
s_deoxys:db"DEOXYS\6L"	; + 0  L 8
        db "LV"			; + 8  L 2
		  db "RS"			; +10  L 2
		  db "E"				; +12  L 1
		  db "FL"			; +13  L 2
								;     TL15

s_breed:db "BREED"		; + 0  L 5
        db "A"				; + 5  L 1
		  db "GS"			; + 6  L 2
		  db "C"				; + 8  L 1
								;     TL 9

s_tutor:db "TUTORS"		; + 0  L 6
        db "E"				; + 6  L 1
		  db "FL"			; + 7  L 2
		  db "C"				; + 9  L 1
								;     TL10

s_tmhm: db "tM/hM"		; + 0  L 5
		  db "A"				; + 5  L 1
		  db "GS"			; + 6  L 2
        db "RY"			; + 8  L 2
								;     TL10

tmhm_screen_arrow_position:
 db 62,70,82
tmhm_screen_data_offsets:
 dw tmhmadv,tmhmgsc,tmhmrby


;alotofspaces:
; db [128]LdotIcon
thirteenspacesandazero:
 db [13]SFourSpaces,0
tenspacesandazero:; DO NOT MOVE (SEE BELOW)
 db [9]SFourSpaces," "
IY_counter_bytes:	; This label requires a zero and 24 non-zero bytes after it. The zero also belongs to
 db 0						; tenspacesandazero, the bytes after it belong to s_frlgplusminus
s_frlgplusminus:; DO NOT MOVE (SEE ABOVE)
  db SFourSpaces,"-1"
  db SFourSpaces,"+1"
  db SFourSpaces,"-2"
  db SFourSpaces,"+2"
  db SFourSpaces,"-",Sdieresis	; 4
  db SFourSpaces,"+",Sdieresis
  db SFourSpaces,"-",SsupX			; 9
  db SFourSpaces,"+",SsupX
  db             "-",SsupX,SsupX	; 20
  db             "+",SsupX,SsupX
  db 		        "---"	; big
  db             "+++"	;

s_nomoves:
 db "NO\6MOVES"
s_nomoves2:
 db "TO\6SHOW\6HERE"
s_nomove_selected:
 db [12]SFourSpaces

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
  cp "v"
  jr z,waitkey_down
  cp "^"
  jr z,waitkey_up
  cp "<"
  jr z,waitkey_left
  cp ">"
  jr z,waitkey_right
  cp "d"
  jr z,waitkey_curr_down
  cp "u"
  jr z,waitkey_curr_up
  cp "l"
  jr z,waitkey_curr_left
  cp "r"
  jr z,waitkey_curr_right
  cp "*"
  jr z,waitkey_jump_to_sel_move
  cp 4						; this gets 1,2 and 3
  jp m,waitkey_changemode
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
  push AF
  B_CALL CkOP1Real ; ACC = type, Z = 1 if real
  jp nz,waitkey_failed
  pop AF
  B_CALL StoX
  jp movelist_noresetmode

waitkey_1:	; Green
  call cleanup
  B_JUMP Green
waitkey_2:	; Move
  call cleanup
  B_JUMP Move
waitkey_3:	; Select
failed_to_get_x:
  call cleanup
  B_JUMP Select
waitkey_4:	; MList
  jp waitkey	; don't do anything!
waitkey_5:	; Plus
  call cleanup
  jp waitkey_statcalc
waitkey_quit:
  call cleanup
  B_JUMP JForceCmdNoChar

waitkey_down:
  B_CALL RclX
  B_CALL Plus1
  B_CALL StoX
  jp movelist_noresetmode
waitkey_up:
  B_CALL RclX
  B_CALL Minus1
  B_CALL StoX
  jp movelist_noresetmode
waitkey_left:
  ld A,(screen)
  dec A
  jp m,waitkey
  ld (screen),A
  jp drawscreen_pokemonloaded
waitkey_right:
  ld A,(total)
  ld B,A
  ld A,(screen)
  inc A
  cp B
  jp p,waitkey
  ld (screen),A
  jp drawscreen_pokemonloaded
waitkey_changemode:
  dec A
  ld B,A
  ld A,(mode)
  ld C,A
  ld A,B
  cp C
  jp m,dontinca
  inc A
 dontinca:
  ld (mode),A
  jp movelist_noresetmode
 waitkey_curr_down:
  ld A,(statVars)
  or A
  jp z,waitkey		; if no moves, don't mess with cursor
  ld D,SFourSpaces
  call draw_character_at_current_arrow_position
  ld HL,currpos
  inc (HL)
 cursor_update_check_upperbound:	; (assumes HL points to currpos)
  ld A,(HL)
  cp (IY+totalmovesonscreen)
  jp p,cursor_update_set_upperbound
  jp cursor_update
 cursor_update_set_upperbound:	; (assumes HL points to currpos)
  ld A,(IY+totalmovesonscreen)
  dec A
  ld (HL),A
  jp cursor_update
 waitkey_curr_up:
  ld A,(statVars)
  or A
  jp z,waitkey		; if no moves, don't mess with cursor
  ld D,SFourSpaces
  call draw_character_at_current_arrow_position
  ld HL,currpos
  dec (HL)
 cursor_update_check_lowerbound:	; (assumes HL points to currpos)
  ld A,(HL)
  or A
  jp m,cursor_update_set_lowerbound
  jp cursor_update
 cursor_update_set_lowerbound:	; (assumes HL points to currpos)
  xor A
  ld (HL),A
  jp cursor_update
 waitkey_curr_left:
  ld A,(statVars)
  or A
  jp z,waitkey		; if no moves, don't mess with cursor
  ld D,SFourSpaces
  call draw_character_at_current_arrow_position
  ld HL,currpos
  ld A,(HL)
  sub 8
  ld (HL),A
  jp cursor_update_check_lowerbound
 waitkey_curr_right:
  ld A,(statVars)
  or A
  jp z,waitkey		; if no moves, don't mess with cursor
  ld D,SFourSpaces
  call draw_character_at_current_arrow_position
  ld HL,currpos
  ld A,(HL)
  add 8
  ld (HL),A
  jp cursor_update_check_upperbound
 cursor_update:
  ld D,Sconvert
  call draw_character_at_current_arrow_position
  ld A,(mode)
  cp tmhm
  jp nz,waitkey
  call drawmachinename
  jp waitkey
 waitkey_jump_to_sel_move:
  ; Here comes the magic ^^
  ; Mode # (desc)      	:		Base_Address+Modifier		:	Bytes per Record
  ;---------------------|--------------------------------|------------------
  ; MODE-0 (Level-Move)	:		movedata+1	+3*8*(screen)	:	3					 
  ; MODE-1 (Tutor)		:		movedata		+2*8*(screen)	:	2					 
  ; MODE-2 (TM/HM)		:		tmhmlistforcurrentpokemon	:	2					 
  ; MODE-3 (Breed)		:		movedata		+2*8*(screen)	:	2					 
  ; First get the Base Address
  ld A,(mode)
  dec A	; check MODE-1
  jp z,getbaseaddressformode1
  dec A	; check MODE-2
  jp z,getbaseaddressformode2
  dec A	; check MODE-2
  jp z,getbaseaddressformode3
  ; MODE-0 is left
 getbaseaddressformode0:
  ld HL,movedata+1
  jp adjustbaseaddress
 getbaseaddressformode2:
  ld HL,tmhmlistforcurrentpokemon
  jp baseaddressdone	; don't adjust
 getbaseaddressformode1:
 getbaseaddressformode3:
  ld HL,movedata
 ;jp adjustbaseaddress
 adjustbaseaddress:
  ld A,(screen)
  add A	; x2
  add A	; x4
  add A	; x8
  ld E,A
  xor A
  ld D,A
  add HL,DE
  add HL,DE
  ld A,(mode)
  or A
  jp nz,baseaddressdone	; one mode add for MODE-0
  add HL,DE
 baseaddressdone:
  ; Now get the move# address
  ld A,(currpos)
  ld E,A
  xor A
  ld D,A
  add HL,DE
  add HL,DE
  ld A,(mode)
  or A
  jp nz,movenoaddressdone	; one mode add for MODE-0
  add HL,DE
 movenoaddressdone:
  ; Get the address & mask it with 00000001b 11111111b (1 255)
  ld A,(HL)
  and 1
  ld D,A
  inc HL
  ld E,(HL)
  ex HL,DE
  dec HL	; move index conversion
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  B_CALL StoX
  B_JUMP Move



waitkey_failed:
  ld A,74
  ld (penCol),A
  xor A
  ld (penRow),A
  ld DE,OP1
  ld HL,fail
  ld BC,5
  ldir
  ld b,5
  ld hl,OP1
  B_CALL VPutSN
  pop AF
  B_CALL VPutMap
  jp waitkey

waitkey_statcalc:
  B_JUMP StatCalculator

 ;.align 256

fail:
 db "BAD: "
keys:
 db  SFourSpaces				; (not used)
 db        "d<>u",0,0,0,0		; v<>^????
 db        "*",0,"WRMH",0,0	; enter+-*/^clear-?
 db         "r","[VQLG",0,0	; -369)tan-vars-?
 db         "v","ZUPKFC",3		; .258(cos-prgm-stat
 db 			"l","YTOJEB",2		; 0147,sin-apps-xt0n
 db        "~",0,"SNIDA",1		; ?-store-ln-log-square-recip-math-alpha
 db            "54321",0,"~"	; graph-trace-zoom-window-y=-2nd-mode
 db "^"	; del

eggmovedataoffsets:
 dw eggmovedatastart+0000h,0,0                   ,eggmovedatastart+0012h,0,0                   ,eggmovedatastart+0022h
 dw 0,0,0,0,0,0,0,0       ,eggmovedatastart+0034h,0,0                   ,eggmovedatastart+003Eh,0
 dw eggmovedatastart+004Eh,0                     ,eggmovedatastart+005Ch,0,0,0                 ,eggmovedatastart+0068h
 dw 0                     ,eggmovedatastart+007Ah,0,0                   ,eggmovedatastart+0088h,0,0,0,0
 dw eggmovedatastart+0096h,0,0,0                 ,eggmovedatastart+00A6h,0                     ,eggmovedatastart+00B2h
 dw 0,0                   ,eggmovedatastart+00BEh,0                     ,eggmovedatastart+00CEh,0
 dw eggmovedatastart+00D6h,0                     ,eggmovedatastart+00E4h,0                     ,eggmovedatastart+00F0h
 dw 0                     ,eggmovedatastart+0104h,0                     ,eggmovedatastart+0114h,0
 dw eggmovedatastart+0122h,0,0                   ,eggmovedatastart+0130h,0,0                   ,eggmovedatastart+013Eh
 dw 0,0                   ,eggmovedatastart+014Ch,0,0                   ,eggmovedatastart+015Ah,0
 dw eggmovedatastart+0166h,0,0                   ,eggmovedatastart+016Ch,0                     ,eggmovedatastart+017Ah
 dw 0,0,0                 ,eggmovedatastart+0188h,eggmovedatastart+0198h,0                     ,eggmovedatastart+01A4h
 dw 0                     ,eggmovedatastart+01B6h,0                     ,eggmovedatastart+01C4h,0
 dw eggmovedatastart+01D0h,0,0                   ,eggmovedatastart+01DEh,eggmovedatastart+01E6h,0
 dw eggmovedatastart+01F4h,0,0,0                 ,eggmovedatastart+0202h,0                     ,eggmovedatastart+0212h
 dw 0,0,0                 ,eggmovedatastart+0220h,eggmovedatastart+0230h,0                     ,eggmovedatastart+023Ch
 dw 0                     ,eggmovedatastart+0250h,eggmovedatastart+025Ah,eggmovedatastart+0268h,eggmovedatastart+0278h
 dw 0                     ,eggmovedatastart+0286h,0                     ,eggmovedatastart+0290h,0
 dw eggmovedatastart+0296h,eggmovedatastart+02A2h,0,0,0                 ,eggmovedatastart+02B2h,0,0,0
 dw eggmovedatastart+02BAh,0                     ,eggmovedatastart+02CCh,0,0,0,0               ,eggmovedatastart+02D8h
 dw 0                     ,eggmovedatastart+02E6h,0                     ,eggmovedatastart+02F4h,eggmovedatastart+0300h
 dw 0,0,0                 ,eggmovedatastart+030Ch,0,0,0,0               ,eggmovedatastart+0318h,0,0
 dw eggmovedatastart+032Ah,0,0                   ,eggmovedatastart+033Ch,0,0                   ,eggmovedatastart+034Eh
 dw 0                     ,eggmovedatastart+035Eh,0                     ,eggmovedatastart+036Ch,0
 dw eggmovedatastart+0374h,0,0                   ,eggmovedatastart+0380h,0                     ,eggmovedatastart+0388h
 dw eggmovedatastart+0396h,eggmovedatastart+03A6h,eggmovedatastart+03B0h,0                     ,eggmovedatastart+03BEh
 dw 0                     ,eggmovedatastart+03CEh,0,0,0                 ,eggmovedatastart+03DEh,0
 dw eggmovedatastart+03EEh,0                     ,eggmovedatastart+03F0h,0,0                   ,eggmovedatastart+0402h
 dw eggmovedatastart+0412h,0                     ,eggmovedatastart+041Eh,eggmovedatastart+0428h,0,0,0
 dw eggmovedatastart+0438h,0                     ,eggmovedatastart+044Ah,0,0                   ,eggmovedatastart+0452h
 dw eggmovedatastart+0462h,0                     ,eggmovedatastart+046Eh,eggmovedatastart+047Eh,0
 dw eggmovedatastart+0488h,0                     ,eggmovedatastart+049Ch,0                     ,eggmovedatastart+04A6h
 dw eggmovedatastart+04A8h,eggmovedatastart+04B0h,eggmovedatastart+04BEh,0                     ,eggmovedatastart+04D0h
 dw 0                     ,eggmovedatastart+04D4h,0                     ,eggmovedatastart+04E4h,eggmovedatastart+04F6h
 dw 0                     ,eggmovedatastart+0504h,eggmovedatastart+0510h,eggmovedatastart+051Ch,eggmovedatastart+0526h
 dw 0,0                   ,eggmovedatastart+0536h,0,0                   ,eggmovedatastart+0544h,0
 dw eggmovedatastart+0554h,0                     ,eggmovedatastart+055Eh,eggmovedatastart+056Ah,eggmovedatastart+0578h
 dw eggmovedatastart+0584h,0,0,0,0               ,eggmovedatastart+0594h,0,0,0,0,0             ,eggmovedatastart+05A2h
 dw 0,0                   ,eggmovedatastart+05AEh,0,0                   ,eggmovedatastart+05BAh,0,0
 dw eggmovedatastart+05C6h,0                     ,eggmovedatastart+05D0h,0,0,0,0,0,0           ,eggmovedatastart+05DAh
 dw 0,0                   ,eggmovedatastart+05E6h,0,0                   ,eggmovedatastart+05F2h,0,0
 dw eggmovedatastart+05FAh,0                     ,eggmovedatastart+0606h,0                     ,eggmovedatastart+0610h
 dw eggmovedatastart+0620h,0                     ,eggmovedatastart+062Ah,0                     ,eggmovedatastart+0634h
 dw 0                     ,eggmovedatastart+0644h,0                     ,eggmovedatastart+0654h,0,0
 dw eggmovedatastart+065Ah,eggmovedatastart+0660h,eggmovedatastart+0668h,eggmovedatastart+066Eh,0
 dw eggmovedatastart+0674h,eggmovedatastart+067Ch,0                     ,eggmovedatastart+0684h,0
 dw eggmovedatastart+0690h,0                     ,eggmovedatastart+0696h,0,0                   ,eggmovedatastart+069Ch
 dw 0                     ,eggmovedatastart+06ACh,0                     ,eggmovedatastart+06B6h,0
 dw eggmovedatastart+06C2h,0,0                   ,eggmovedatastart+06D2h,0                     ,eggmovedatastart+06DCh
 dw 0,0,0                 ,eggmovedatastart+06E0h,eggmovedatastart+06EAh,0                     ,eggmovedatastart+06F2h
 dw eggmovedatastart+06F6h,eggmovedatastart+06FAh,eggmovedatastart+0706h,0                     ,eggmovedatastart+0714h
 dw 0,0                   ,eggmovedatastart+071Ch,0                     ,eggmovedatastart+0728h,eggmovedatastart+0730h
 dw 0,0                   ,eggmovedatastart+073Eh,0                     ,eggmovedatastart+0746h,eggmovedatastart+0750h
 dw 0,0                   ,eggmovedatastart+075Ah,0,0                   ,eggmovedatastart+0766h,eggmovedatastart+0772h
 dw 0                     ,eggmovedatastart+077Ch,eggmovedatastart+0784h,eggmovedatastart+0790h,eggmovedatastart+079Ch
 dw 0,0                   ,eggmovedatastart+07A4h,eggmovedatastart+07A8h,eggmovedatastart+07AEh,eggmovedatastart+07B4h
 dw 0                     ,eggmovedatastart+07BCh,0                     ,eggmovedatastart+07C4h,0,0
 dw eggmovedatastart+07CEh,0,0,0,0,0,0,0,0       ,0,0,0,0,0,0,0,0       ,0

tutormoves:
 db 00000010b,00110101b;  	Flamethrower  	1	; C
 db 00000010b,00111010b; 	Ice Beam 		2
 db 00000010b,01010101b; 	Thunderbolt 	3
 db 00001100b,00000101b;  	Mega Punch  	1	;	FR/LG/E
 db 00001100b,00001110b; 	Swords Dance 	2
 db 00001100b,00011001b; 	Mega Kick 		3
 db 00001100b,00100010b; 	Body Slam 		4
 db 00001100b,00100110b; 	Double-Edge 	5
 db 00001100b,01000100b; 	Counter 			6
 db 00001100b,01000101b; 	Seismic Toss 	7
 db 00001100b,01010110b; 	Thunder Wave 	8
 db 00001100b,01100110b; 	Mimic 			9
 db 00001100b,01110110b; 	Metronome 		10
 db 00001100b,10000111b; 	Softboiled 		11
 db 00001100b,10001010b; 	Dream Eater 	12
 db 00001100b,10011001b; 	Explosion 		13
 db 00001100b,10011101b; 	Rock Slide		14
 db 00001100b,10100100b; 	Substitute	 	15
 db 00001101b,00110011b; 	Blast Burn 		16
 db 00001101b,00110100b; 	Hydro Cannon 	17
 db 00001101b,01010010b; 	Frenzy Plant 	18
 db 00001000b,11011111b;  	Dynamicpunch  	1	; E
 db 00001000b,11001101b; 	Rollout		 	2
 db 00001000b,11110100b; 	Psych Up			3
 db 00001000b,10101101b; 	Snore 			4
 db 00001000b,11000100b; 	Icy Wind			5
 db 00001000b,11001011b; 	Endure 			6
 db 00001000b,10111101b; 	Mud-Slap			7
 db 00001000b,00001000b; 	Ice Punch		8
 db 00001000b,11001111b; 	Swagger			9
 db 00001000b,11010110b; 	Sleep Talk		10
 db 00001000b,10000001b; 	Swift				11
 db 00001000b,01101111b; 	Defense Curl 	12
 db 00001000b,00001001b; 	Thunderpunch 	13
 db 00001000b,00000111b; 	Fire Punch	 	14
 db 00001000b,11010010b; 	Fury Cutter 	15

tutordata: ; 5 bytes per pokemon
 db 00001011b,00010000b,01000000b,10110110b,10010000b,00001011b,00010000b,01000000b,10110110b,10010000b
 db 00001011b,00010000b,01001000b,10110110b,10010000b,10011111b,11010000b,11000100b,10110111b,10110000b
 db 10011111b,11010000b,11000100b,10110111b,10110000b,10011111b,11010000b,11100100b,10110111b,10110000b
 db 00110111b,11010000b,01000110b,11111110b,10000000b,00110111b,11010000b,01000110b,11111110b,10000000b
 db 00110111b,11010000b,01010110b,11111110b,10000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000001b,00010010b,01000000b,10100111b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00001001b,00010000b,01000000b,10100111b,00010000b,00000001b,00010000b,01000000b,10110111b,00000000b
 db 00000001b,00010000b,01000000b,10110111b,00000000b,00000001b,00010000b,01000000b,10110111b,00000000b
 db 00000011b,10110000b,01000000b,11110111b,10000000b,01100011b,10110000b,01000000b,11110111b,10000000b
 db 00000001b,00010000b,01000000b,10110111b,00000000b,00000001b,00010000b,01000000b,10110111b,00000000b
 db 00000011b,00010000b,11000000b,10100110b,00000000b,00000011b,00010000b,11000000b,10100110b,00000000b
 db 01010111b,11110000b,01000110b,10110111b,11000000b,01010111b,11110000b,01000110b,10110111b,11000000b
 db 00001011b,11010000b,11000110b,10110111b,10010000b,00001011b,11010000b,11000110b,10110111b,10010000b
 db 00000011b,10010000b,01000000b,10110110b,10000000b,00000011b,10010000b,01000000b,10110110b,10000000b
 db 11110111b,11010000b,11000100b,11111110b,11110000b,00000011b,10010000b,01000000b,10110110b,10000000b
 db 00000011b,10010000b,01000000b,10110110b,10000000b,11110111b,11010000b,11000100b,11111110b,11110000b
 db 11110111b,11111110b,01000111b,10111110b,11100000b,11110111b,11111110b,01000111b,10111110b,11100000b
 db 10000011b,00010000b,01000000b,10100111b,00000000b,10000011b,00010000b,01000000b,10100111b,00000000b
 db 11110111b,11110010b,01000111b,10111110b,11100000b,11110111b,11110010b,01000111b,10111110b,11100000b
 db 00000001b,00010000b,01000000b,10100111b,00000000b,00000001b,00010000b,01000000b,10100111b,00000000b
 db 00001001b,00010000b,01000000b,10100110b,00000000b,00001001b,00010000b,01000000b,10100110b,00000000b
 db 00001011b,00010000b,01000000b,10100110b,00000000b,00001011b,10010000b,01000000b,10100110b,00010000b
 db 00001011b,10010000b,01000000b,10100110b,00010000b,00000001b,00010000b,01000000b,10100111b,00000000b
 db 00000001b,00010000b,01000000b,10100111b,00000000b,00000011b,00010000b,11000000b,10110110b,00000000b
 db 00000011b,00010000b,11000000b,10110110b,00000000b,01000011b,00010010b,01000001b,11110111b,10000000b
 db 01000011b,00010010b,01000001b,11110111b,10000000b,00110111b,11010000b,01000101b,11111111b,00000000b
 db 00110111b,11010000b,01000101b,11111111b,00010000b,01010111b,11011000b,11000101b,10111111b,11100000b
 db 01010111b,11011000b,11000101b,10111111b,11100000b,10000011b,00010000b,01000000b,10100111b,00000000b
 db 10000011b,00010000b,01000000b,10100111b,00000000b,00100011b,00010000b,01000000b,11100110b,10000000b
 db 00110111b,11011000b,01000000b,11111110b,10000000b,00110111b,11011000b,01000100b,11111110b,10000000b
 db 00010111b,11111010b,01000101b,10101110b,01100000b,00010111b,11111010b,01000101b,10101110b,01100000b
 db 00010111b,11111010b,01000101b,10101110b,01100000b,10010111b,11011000b,11000100b,10111110b,01100000b
 db 10010111b,11011000b,11000100b,10111110b,01100000b,10010111b,11011000b,11000100b,10111110b,01100000b
 db 00001001b,00010000b,01000000b,10100110b,00000000b,00001001b,00010000b,01000000b,10100110b,00000000b
 db 00001011b,00010000b,01000000b,10100110b,00000000b,00101001b,00010000b,01000000b,11100110b,00000000b
 db 00101001b,00010000b,01000000b,11100110b,00000000b,10010011b,11011001b,11000110b,10110110b,10100000b
 db 10010011b,11011001b,11000110b,10110110b,10100000b,10010111b,11011001b,11000110b,10110110b,10110000b
 db 10000011b,00010000b,01000000b,10100111b,00000000b,10000011b,00010000b,01000000b,10100111b,00000000b
 db 10100011b,00110010b,01000001b,11110111b,00000000b,10110111b,11110010b,01000101b,11111111b,00010000b
 db 01000001b,00110000b,01000010b,10100111b,00000000b,01000001b,00110000b,01000010b,10100111b,00000000b
 db 00001011b,00010000b,01000001b,10110111b,00000000b,00000011b,00010000b,01000000b,10110111b,00000000b
 db 00000011b,00010000b,01000000b,10110111b,00000000b,00100011b,00010000b,01000000b,11100110b,00000000b
 db 00100011b,00010000b,01000000b,11100110b,00000000b,11000010b,00010001b,01000100b,10111110b,01100000b
 db 11000010b,00010001b,01000100b,10111110b,01100000b,00100001b,00010001b,01000000b,11100111b,00000000b
 db 00100001b,00010001b,01000000b,11100111b,00000000b,01000000b,00010011b,01000001b,10100110b,00000000b
 db 01000000b,00010011b,01000001b,10100110b,00000000b,01010111b,11011011b,01000101b,10101110b,01100000b
 db 00000011b,00010001b,11000001b,10110110b,00000000b,00010111b,11111010b,01000101b,10101110b,01100000b
 db 00010111b,11111010b,01000101b,10101110b,01100000b,00101011b,00010000b,01000000b,11110110b,00010000b
 db 00101011b,00010000b,01000000b,11110110b,00010000b,01000000b,00110001b,01000010b,10100111b,00000000b
 db 01000000b,00110001b,01000010b,10100111b,00000000b,00000001b,00010011b,01000011b,10100110b,00000000b
 db 00000001b,00010011b,01000011b,10100110b,00000000b,10111111b,11010000b,11000100b,11110110b,01100000b
 db 10111111b,11010000b,11000100b,11110110b,01100000b,00010111b,11011000b,11000100b,10110111b,00000000b
 db 00010111b,11011000b,11000100b,10111111b,01100000b,11111111b,11010010b,11000111b,11111110b,11100000b
 db 11000000b,00010001b,01000010b,10100110b,00000000b,11000000b,00010001b,01000010b,10100110b,00000000b
 db 11101011b,10010000b,11000010b,11110110b,00000000b,11111111b,11010000b,11000110b,11110110b,01110000b
 db 11110111b,11111110b,01000111b,11110110b,10000000b,00001011b,00010000b,01000001b,10100110b,00000000b
 db 11110111b,11010000b,11000100b,11111110b,01110000b,00100001b,00010000b,01000000b,11100111b,00000000b
 db 00100001b,00010000b,01000000b,11100111b,00000000b,00100001b,00010000b,01000000b,11100111b,00000000b
 db 00100001b,00010000b,01000000b,11100111b,00000000b,01000001b,00110000b,01000001b,11100111b,00000000b
 db 01000001b,00110010b,01000001b,11100111b,00000000b,01010111b,11111010b,01000001b,10111110b,01100000b
 db 00001001b,10010000b,01000000b,10100111b,00010000b,00110111b,11011010b,01000101b,11111110b,00000000b
 db 01010111b,11110000b,01000100b,10111111b,01100000b,10010111b,11010000b,01000100b,10110110b,01100000b
 db 00001011b,01010000b,11000000b,10100110b,00010000b,11100011b,00010000b,01000000b,11100110b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,11100011b,00110000b,01000000b,11100110b,00000000b
 db 01100011b,00010010b,01000000b,11100110b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000011b,00010000b,01000000b,10110111b,00000000b,00100011b,00010000b,01000000b,11110111b,00000000b
 db 01000011b,00110000b,01000000b,10110111b,00000000b,10000011b,00010000b,01000000b,10110111b,00000000b
 db 01100001b,00110010b,01000001b,11100111b,00000000b,00100011b,00010000b,11000010b,11100110b,00000000b
 db 00100011b,01010000b,11000010b,11100110b,00000000b,00100011b,00010000b,11000010b,11100110b,00000000b
 db 00101111b,01010000b,11000010b,11100110b,00010000b,10000001b,00010000b,11000000b,10100111b,00000000b
 db 11110111b,11011000b,11000111b,11111110b,11100000b,00100001b,00010000b,01000000b,11110111b,00000000b
 db 01000001b,00110000b,01000000b,10110111b,00000000b,10000001b,00010000b,01000000b,10110111b,00000000b
 db 11100011b,00110000b,01000000b,11100111b,00000000b,11100011b,00110000b,01000000b,11100111b,00000000b
 db 11100011b,00110000b,01000100b,11111111b,01110000b,11110111b,11111010b,01000101b,11111111b,01100000b
 db 11111111b,11111111b,11000111b,11111111b,11110000b,00001011b,10010000b,01000000b,10110110b,00000000b
 db 00001011b,10010000b,01000000b,10110110b,00010000b,00001011b,10010000b,01000000b,10110110b,00010000b
 db 10000011b,00010000b,01000010b,10110111b,10000000b,10000011b,00010000b,01000010b,10110111b,10010000b
 db 10010111b,11010000b,11000110b,10110111b,11110000b,00111111b,11010000b,11000100b,11111110b,00000000b
 db 00111111b,11010000b,11000100b,11111110b,00010000b,00111111b,11010000b,11000100b,11111110b,00010000b
 db 00000011b,00010000b,01000110b,10111111b,11110000b,00000011b,00010000b,01000110b,10111111b,11110000b
 db 00000001b,00010010b,01000000b,10110111b,00000000b,00000001b,00010010b,01000000b,10110111b,00000000b
 db 00011001b,00010000b,01000110b,10101111b,01000000b,00011001b,00010000b,01000110b,10101111b,01000000b
 db 00000011b,00010000b,01000000b,10100110b,00000000b,00000011b,00010000b,01000000b,10100110b,00000000b
 db 00000001b,00010000b,01000000b,10100111b,00000000b,01100001b,00110000b,01000000b,10100110b,00000000b
 db 01100001b,00110000b,01000000b,10100110b,00000000b,01010111b,11110000b,01000010b,10110111b,10000000b
 db 10010111b,11111110b,01000011b,11110110b,10000000b,10010111b,11110010b,01000011b,11110110b,10000000b
 db 10010111b,11111110b,01000011b,10110111b,10000000b,10010111b,11111110b,01000011b,10110111b,10000000b
 db 00000001b,00110010b,01000001b,10100111b,00000000b,00000001b,00110010b,01000001b,10100111b,00000000b
 db 01000011b,00110000b,01000000b,10100111b,10000000b,01010111b,11110000b,01000100b,10100111b,11100000b
 db 01010111b,11110000b,01000100b,10100111b,11100000b,00001001b,00010000b,01000000b,10100110b,00000000b
 db 00110111b,01010000b,01000110b,11111111b,10000000b,00110111b,01010000b,01000110b,11111111b,10000000b
 db 00010111b,11010001b,11000111b,10111110b,11100000b,00110111b,11011000b,01000100b,11110110b,10000000b
 db 00001001b,00010000b,01000000b,10100110b,10000000b,00001001b,00010000b,01000000b,10100110b,10000000b
 db 00001001b,00010000b,01000000b,10100110b,10000000b,01010111b,11111010b,01000100b,10111111b,11110000b
 db 00001001b,00010000b,01000000b,10100110b,00000000b,00001001b,00010000b,01000000b,10100110b,00000000b
 db 00000001b,00010010b,01000000b,10100111b,00000000b,00100011b,00010000b,01000110b,10111110b,10000000b
 db 00110111b,11010000b,01000110b,10111110b,10000000b,00000011b,00010010b,01000001b,10110111b,00000000b
 db 00000011b,00010010b,01000001b,10110111b,00000000b,00000001b,00110010b,01000001b,11110111b,00000000b
 db 10110111b,11110010b,01000101b,11111111b,00010000b,01000001b,00110010b,01000001b,10100111b,10000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 01000011b,00110010b,01000001b,10110111b,00000000b,00000011b,10010001b,11000010b,10100110b,10000000b
 db 00000011b,10010001b,11000010b,10100110b,10000000b,00000011b,10110010b,11000011b,10110110b,10000000b
 db 00001001b,10010010b,11000000b,10100111b,00010000b,00000011b,00010001b,11000010b,10110110b,10000000b
 db 01010111b,11111000b,01000100b,10111110b,11100000b,01010111b,11111000b,11000100b,10111110b,11100000b
 db 00101001b,00110000b,01000010b,11100111b,10000000b,00001001b,10010000b,01000000b,10100111b,00010000b
 db 00000011b,00010000b,11000010b,10110110b,10000000b,00001011b,11010000b,11000000b,10100110b,00010000b
 db 00101001b,10010010b,01000101b,11111111b,10010000b,00011111b,11011000b,01000110b,10111111b,11110000b
 db 00011111b,11011000b,11000110b,10111111b,11110000b,10000011b,00010000b,11000010b,10110110b,10000000b
 db 10000011b,00010000b,11000010b,10110110b,10000000b,00100011b,00010000b,11000000b,11110110b,10000000b
 db 00100011b,00010000b,11000000b,11110110b,10000000b,00100011b,00010001b,11000010b,10110110b,10000000b
 db 10100001b,00110000b,01000000b,10110111b,10000000b,10100001b,01110000b,01000000b,10110111b,10000000b
 db 00110111b,11010000b,01000000b,11110111b,00000000b,00100011b,00010000b,01000000b,11110111b,00000000b
 db 00000001b,10010000b,11000000b,10110111b,00000000b,10000011b,10010010b,01000000b,10110111b,00000000b
 db 10000011b,10010010b,01000000b,10110111b,00000000b,00100011b,00010000b,01000000b,11100111b,00000000b
 db 00000011b,10010000b,01000010b,10110110b,10000000b,00000011b,10010000b,11000010b,10110110b,10000000b
 db 01100001b,00110010b,01000001b,11100111b,10000000b,00000011b,00110010b,01000001b,10110111b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000111b,11010000b,11000000b,10110111b,00000000b
 db 00000111b,11010000b,11000000b,10110111b,00000000b,00110111b,11011010b,01000101b,11111110b,00000000b
 db 01010111b,11110000b,01000100b,10111111b,01100000b,10010111b,11010000b,01000100b,10110110b,01100000b
 db 01110111b,11111000b,11000111b,11111110b,11100000b,11110111b,11111110b,01000110b,11110110b,10000000b
 db 01000011b,00110000b,01000001b,10110111b,00000000b,10000011b,00010000b,01000001b,10110111b,00000000b
 db 00100011b,00010000b,01000001b,11110111b,00000000b,11100011b,00010000b,11000000b,10110110b,00000000b
 db 11100011b,00010000b,11000000b,10110110b,00000000b,11110111b,11110000b,11000100b,10110110b,00110000b
 db 01100011b,00110010b,01000001b,11110111b,00000000b,11000001b,00110010b,01000001b,10110111b,00000000b
 db 00001001b,00011010b,01000001b,10110111b,10000000b,00011111b,11010000b,01000100b,10110111b,01010000b
 db 00011111b,11010000b,01000100b,10110111b,01010000b,00011111b,11010000b,01000100b,10110111b,01010000b
 db 00011111b,11010000b,11000000b,10110111b,00000000b,00011111b,11010000b,11000100b,10110111b,01110000b
 db 00011111b,11010000b,11000100b,10110111b,01110000b,00000011b,00010000b,01000010b,11110110b,10000000b
 db 00010111b,11010000b,11000110b,11111110b,10000000b,00010111b,11010000b,11000110b,11111110b,10000000b
 db 00000011b,10010000b,01000001b,10110110b,00000000b,00000011b,10010000b,01000001b,10110110b,00000000b
 db 00000011b,00110000b,01000010b,11110111b,10010000b,00000011b,00110000b,01000010b,11110111b,10010000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000001b,00010000b,01000000b,10100111b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000001b,00010000b,01000000b,10100111b,00000000b,00001011b,00010000b,01000000b,11100110b,00000000b
 db 00001011b,00010000b,01000100b,11111110b,01100000b,00011111b,11011000b,01000100b,11111110b,01100000b
 db 00001011b,00010001b,01000010b,10100110b,10000000b,00001111b,00010001b,01000011b,10110111b,10010000b
 db 00001111b,00010001b,01000011b,10110111b,10010000b,00000001b,00010000b,01000000b,10110110b,00010000b
 db 00001001b,00010000b,01000000b,10110111b,00010000b,00000001b,00010010b,01000000b,10110110b,00010000b
 db 00000001b,10010000b,01000000b,10110111b,00000000b,00000001b,10010000b,01000000b,10110111b,00000000b
 db 00001011b,00010000b,01000000b,10100110b,00000000b,00011111b,11010000b,01000100b,10110110b,01010000b
 db 00010111b,11011010b,11000111b,11111111b,11100000b,00000001b,00010000b,01000000b,11110111b,00000000b
 db 00000001b,00010000b,01000000b,11110111b,00000000b,00000001b,00010000b,01000001b,11100111b,00000000b
 db 00000001b,00010000b,01000001b,11100111b,00000000b,00000011b,00010000b,01000010b,11100110b,10000000b
 db 00000011b,00010000b,01000010b,11100110b,10000000b,00000011b,00110010b,01000011b,11110111b,10000000b
 db 00000011b,00110010b,01000011b,11110111b,10000000b,00010111b,11111000b,11000111b,11111111b,11110000b
 db 00000001b,00010011b,11000001b,10110110b,00000000b,00000001b,00010011b,11000001b,10110110b,00000000b
 db 00000011b,00110001b,11000110b,10111110b,11100000b,00000011b,00010001b,11000000b,10110110b,00000000b
 db 00010111b,11011010b,01000101b,10111110b,01110000b,00000001b,00010000b,01000000b,11110110b,00000000b
 db 00000001b,00010000b,11000000b,11110110b,00000000b,00000001b,00010000b,01000001b,11100111b,00000000b
 db 00001011b,10010000b,01000000b,11110110b,00010000b,00001011b,10010000b,01000000b,11110111b,00010000b
 db 00000001b,00010000b,01000000b,11100111b,00000000b,00000011b,00010000b,01000001b,11110111b,00000000b
 db 00000001b,00010000b,01000000b,11110111b,00010000b,00000001b,00010000b,01000000b,11110111b,00010000b
 db 00000011b,00010000b,11000000b,10110110b,00000000b,00000011b,00010000b,11000000b,10110111b,00000000b
 db 00000011b,00010000b,11000000b,10110111b,00110000b,00010111b,11011000b,11000100b,10111110b,01100000b
 db 00010111b,11011000b,11000100b,10111110b,01100000b,00000011b,00110000b,01000000b,10110111b,00000000b
 db 00000011b,00110000b,01000000b,10110111b,00000000b,00000011b,00010000b,11000010b,10110110b,10000000b
 db 00000011b,00010001b,11000010b,10110110b,10000000b,00000011b,00010000b,11000010b,11110110b,10000000b
 db 00000011b,00010000b,11000010b,11110110b,10000000b,00000011b,00010000b,11000010b,11110110b,10000000b
 db 00011011b,11010000b,01000100b,10110110b,01010000b,00011111b,11010000b,01000100b,10110110b,01010000b
 db 00000011b,00010000b,01000000b,11100110b,00000000b,00000011b,00010001b,01000010b,11100110b,10000000b
 db 00000011b,00010011b,11000011b,10100111b,10000000b,00000011b,00010011b,11000011b,10100111b,10000000b
 db 00000011b,00010000b,01000010b,11110111b,10000000b,00000011b,00010010b,01000001b,11100111b,00000000b
 db 00010111b,11010010b,01000101b,11111111b,01100000b,00010111b,11111000b,01000110b,10110111b,11000000b
 db 00010111b,11111000b,01000110b,10110111b,11000000b,00011111b,11010000b,11000101b,11111110b,01000000b
 db 00010111b,11011010b,01000101b,10111111b,01100000b,00010111b,11011010b,11000101b,10111111b,01100000b
 db 00000011b,00010010b,01000001b,10110111b,00000000b,00000011b,00010010b,01000001b,10110111b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000011b,00010010b,01000001b,11100110b,00000000b
 db 00010111b,11011010b,11000101b,11111110b,01100000b,00001011b,00010000b,01000001b,10110111b,00010000b
 db 00010111b,11010000b,11000100b,11111110b,01110000b,00010111b,11010000b,11000100b,11111110b,01110000b
 db 00010111b,11010000b,11000100b,11111110b,01110000b,00000011b,10010011b,01000110b,10111110b,11100000b
 db 00000011b,10010011b,01000110b,10111110b,11100000b,00001011b,00010000b,01000000b,10110110b,00010000b
 db 00010111b,11010000b,01000111b,11111110b,11100000b,00010111b,11010000b,11000111b,11111110b,11100000b
 db 00010111b,11010000b,11000111b,11111110b,11100000b,00000011b,00010000b,01000000b,11100110b,00000000b
 db 00000011b,00010000b,01000000b,11110111b,00000000b,00000011b,00010000b,01000000b,11110111b,00000000b
 db 00001011b,10110010b,11000001b,11110111b,00010000b,00000011b,00110010b,01000001b,11100110b,00000000b
 db 00000011b,00111010b,01000001b,11110110b,00000000b,00000011b,00010000b,01000000b,10110111b,00010000b
 db 00011111b,11110000b,11000110b,11111111b,11110000b,00000011b,00010000b,11000001b,11110110b,00000000b
 db 00000011b,00010000b,11000010b,10110110b,10010000b,00000011b,00010000b,11000010b,10110110b,10010000b
 db 00010111b,11110000b,11000110b,11111110b,11110000b,00000011b,00110000b,01000001b,11100111b,10000000b
 db 00010111b,11111000b,01000101b,10111111b,01000000b,00010111b,11111000b,01000101b,10111111b,01000000b
 db 00000011b,00010000b,11000001b,10110110b,00000000b,00000011b,00010000b,11000001b,10110110b,00000000b
 db 00001011b,00010000b,11000000b,10110110b,00010000b,00001011b,01010000b,11000000b,10110110b,00010000b
 db 00000011b,00110010b,01000001b,11111110b,11100000b,00000011b,00110010b,01000001b,11111110b,11100000b
 db 00000011b,00110010b,01000001b,11111110b,11100000b,00000011b,00010000b,11000000b,10110110b,00010000b
 db 00000011b,00010000b,11000010b,10110110b,10010000b,00000011b,00010000b,11000010b,10110111b,10010000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000011b,00010001b,11000111b,11111111b,11010000b
 db 00000011b,00010001b,11000111b,11111111b,11010000b,00010111b,11110001b,11000111b,10111110b,11100000b
 db 00010111b,11110001b,11000111b,11111110b,11000000b,00010111b,11110001b,11000111b,10111110b,11000000b
 db 00000011b,00110000b,11000001b,11110111b,10000000b,00011111b,11110000b,11000111b,10110111b,11110000b
 db 00000011b,00110000b,11000001b,11110111b,00010000b,00000011b,00110010b,01000001b,11110111b,00010000b
 db 00000011b,00110010b,01000001b,11110111b,00010000b,00000011b,00111010b,01000101b,11111111b,11100000b
 db 00010111b,11110010b,11000101b,11111111b,01100000b,00000001b,00010010b,01000011b,11100110b,10000000b

yellow_changes:
 dw s_no_changes,s_plus_fly,s_plus_flash,s_plus_cut,s_plus_many,s_minus_payday
s_no_changes:
 db 0,0,0,0,0,0,0,0
s_plus_fly:
 db 0,"<- HM2: FLY",0,"in Yellow",0,"Only",0,0,0,0,0,0
s_plus_flash:
 db 0,0,0,0,0,"HM5: FLASH",0,"in Yellow",0,"Only",0
s_plus_cut:
 db 0,0,0,0,0,"HM1: CUT",0,"in Yellow",0,"Only",0
s_plus_many:
 db "Yellow Only:",0,"T2: RAZOR WIND",0,"T3: SWORDS DAN.",0,"T5: MEGA KICK",0,"T15: HYPER BEAM",0
 db "T17: SUBMISSION",0,"T19: SEISM.TOSS",0,"T40: SKULL BASH",0
s_plus_many_end:
s_minus_payday:
 db 0,0,0,0,"TM16:",0,"PAY DAY",0,"in R/B",0,"Only",0


tmhm_rb:
no_changes		equ 0
plus_fly			equ 1
plus_flash		equ 2
plus_cut			equ 3
plus_many		equ 4
minus_payday	equ 5
 db 00100101b,11000000b,00011100b,00000011b,11000000b,00010000b,00100000b,no_changes         ;Bulbasaur
 db 00100101b,11000000b,00011100b,00000011b,11000000b,00010000b,00100000b,no_changes         ;Ivysaur
 db 00100101b,11000010b,00011100b,00000011b,11000000b,00010000b,00100000b,no_changes         ;Venusaur
 db 10101101b,11000000b,11110010b,00010011b,11000111b,00010000b,01100100b,no_changes         ;Charmander
 db 10101101b,11000000b,11110010b,00010011b,11000111b,00010000b,01100100b,no_changes         ;Charmeleon
 db 10101101b,11000010b,11110010b,01110011b,11000111b,00010000b,01110100b,           plus_fly;Charizard
 db 10001101b,11111100b,11110000b,00010011b,11000001b,00010000b,01001100b,no_changes         ;Squirtle
 db 10001101b,11111100b,11110000b,00010011b,11000001b,00010000b,01001100b,no_changes         ;Wartortle
 db 10001101b,11111110b,11110000b,01110011b,11000001b,00010000b,01001100b,no_changes         ;Blastoise
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,no_changes         ;Caterpie
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,no_changes         ;Metapod
 db 01010100b,11000010b,00011100b,00001111b,11000010b,00010100b,01000010b,         plus_flash;Butterfree
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,no_changes         ;Weedle
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,no_changes         ;Kakuna
 db 00100100b,11000010b,00011000b,00000011b,11000011b,00010000b,01100000b,no_changes         ;Beedrill
 db 01010100b,11000000b,00010000b,00000011b,11000010b,00110000b,01010000b,no_changes         ;Pidgey
 db 01010100b,11000000b,00010000b,00000011b,11000010b,00110000b,01010000b,no_changes         ;Pidgeotto
 db 01010100b,10000010b,00010000b,00000011b,11000010b,00110000b,01010000b,no_changes         ;Pidgeot
 db 00000101b,11110100b,00010001b,10000011b,01000011b,00010000b,01000000b,no_changes         ;Rattata
 db 00000101b,11111110b,00010001b,10000011b,01000011b,00010000b,01000000b,no_changes         ;Raticate
 db 01010100b,11000010b,00010000b,00000011b,01000010b,00110000b,01010000b,no_changes         ;Spearow
 db 01010100b,11000010b,00010000b,00000011b,01000010b,00110000b,01010000b,no_changes         ;Fearow
 db 00000101b,11000010b,00011000b,01110011b,01000001b,00010001b,01000100b,no_changes         ;Ekans
 db 00000101b,11000010b,00011000b,01110011b,01000001b,00010001b,01000100b,no_changes         ;Arbok
 db 10001101b,11000001b,10110001b,10000011b,11000010b,00011000b,01000010b,no_changes         ;Pikachu
 db 10001101b,11000011b,10110001b,10000011b,11000010b,00011000b,01000010b,no_changes         ;Raichu
 db 00100101b,11000000b,10110000b,01110011b,01000011b,00010001b,01100100b,no_changes         ;Sandshrew
 db 00100101b,11000010b,10110000b,01110011b,01000011b,00010001b,01100100b,no_changes         ;Sandslash
 db 00000101b,11100000b,00010001b,10000011b,11000001b,00010000b,01000000b,no_changes         ;Nidoran +
 db 00000111b,11111100b,00010001b,10000011b,11000001b,00010000b,01000000b,no_changes         ;Nidorina
 db 10001111b,11111111b,11110001b,11100011b,11000101b,00010001b,01001100b,no_changes         ;Nidoqueen
 db 00000111b,11000100b,00010001b,10000011b,11000001b,00010000b,01000000b,no_changes         ;Nidoran ^
 db 00000111b,11111100b,00010001b,10000011b,11000001b,00010000b,01000000b,no_changes         ;Nidorino
 db 10001111b,11111111b,11110001b,11100011b,11000101b,00010001b,01001100b,no_changes         ;Nidoking
 db 10001101b,11111100b,11110101b,10001111b,11100101b,00011100b,11000110b,no_changes         ;Clefairy
 db 10001101b,11111110b,11110101b,10001111b,11100101b,00011100b,11000110b,no_changes         ;Clefable
 db 00000101b,11000000b,00010000b,00010111b,11000111b,00010000b,01000000b,no_changes         ;Vulpix
 db 00000101b,11000010b,00010000b,00010111b,11000111b,00010000b,01000000b,no_changes         ;Ninetales
 db 10001101b,11111100b,11110101b,10001111b,11000101b,00011100b,11000110b,no_changes         ;Jigglypuff
 db 10001101b,11111110b,11110101b,10001111b,11000101b,00011100b,11000110b,no_changes         ;Wigglytuff
 db 01010100b,11000000b,00011000b,00000011b,01000010b,00010000b,01000000b,no_changes         ;Zubat
 db 01010100b,11000010b,00011000b,00000011b,01000010b,00010000b,01000000b,no_changes         ;Golbat
 db 00100100b,11000000b,00011100b,00000011b,11000000b,00010000b,01100000b,no_changes         ;Oddish
 db 00100100b,11000000b,00011100b,00000011b,11000000b,00010000b,01100000b,no_changes         ;Gloom
 db 00100101b,11000010b,00011100b,00000011b,11000000b,00010000b,01100000b,no_changes         ;Vileplume
 db 00100101b,11000000b,00011100b,00010011b,11000001b,00010000b,01100000b,no_changes         ;Paras
 db 00100101b,11000010b,00011100b,00010011b,11000001b,00010000b,01100000b,no_changes         ;Parasect
 db 01010100b,11000010b,00011100b,00001111b,11000010b,00010100b,01000110b,no_changes         ;Venonat
 db 01001100b,11000010b,00011100b,00001111b,11000010b,00010100b,01000110b,no_changes         ;Venomoth
 db 00000101b,11000000b,00010000b,01110011b,01000000b,00010001b,01100000b,           plus_cut;Diglett
 db 00000101b,11000010b,00010000b,01110011b,01000000b,00010001b,01100000b,           plus_cut;Dugtrio
 db 00000101b,11110001b,00010001b,10000011b,01000011b,00010000b,01000000b,no_changes         ;Meowth
 db 00000101b,11110011b,00010001b,10000011b,01000011b,00010000b,01000000b,no_changes         ;Persian
 db 10001101b,11111101b,11110000b,00010011b,01000011b,00010000b,01001100b,no_changes         ;Psyduck
 db 10001101b,11111111b,11110000b,00010011b,01000011b,00010000b,01001100b,no_changes         ;Golduck
 db 10001101b,11000011b,11110001b,10010011b,01100011b,00010001b,01000100b,no_changes         ;Mankey
 db 10001101b,11000011b,11110001b,10010011b,01100011b,00010001b,01000100b,no_changes         ;Primeape
 db 00000101b,11000000b,00010010b,00010011b,11000111b,00010000b,01000000b,no_changes         ;Growlithe
 db 00000101b,11000010b,00010010b,00010111b,11000111b,00010000b,01000000b,no_changes         ;Arcanine
 db 00000101b,11111100b,00010000b,00001011b,01000001b,00010100b,01001000b,no_changes         ;Poliwag
 db 10001101b,11111100b,11110000b,01101011b,01100001b,00010100b,01001100b,no_changes         ;Poliwhirl
 db 10001101b,11111110b,11110000b,01101011b,01100001b,00010100b,01001100b,no_changes         ;Poliwrath
 db 10001101b,11000000b,11110000b,00001111b,11100001b,00011100b,11000010b,no_changes         ;Abra
 db 10001101b,11000000b,11110000b,00011111b,11100001b,00011100b,11000010b,no_changes         ;Kadabra
 db 10001101b,11000010b,11110000b,00011111b,11100001b,00011100b,11000010b,no_changes         ;Alakazam
 db 10001101b,11000000b,11110000b,01110011b,01100101b,00010001b,01000100b,no_changes         ;Machop
 db 10001101b,11000000b,11110000b,01110011b,01100101b,00010001b,01000100b,no_changes         ;Machoke
 db 10001101b,11000010b,11110000b,01110011b,01100101b,00010001b,01000100b,no_changes         ;Machamp
 db 00100100b,11000000b,00011100b,00000011b,11000000b,00010000b,01100000b,no_changes         ;Bellsprout
 db 00100100b,11000000b,00011100b,00000011b,11000000b,00010000b,01100000b,no_changes         ;Weepinbell
 db 00100101b,11000010b,00011100b,00000011b,11000000b,00010000b,01100000b,no_changes         ;Victreebel
 db 00100100b,11111100b,00011000b,00000011b,11000001b,00010000b,01101000b,no_changes         ;Tentacool
 db 00100100b,11111110b,00011000b,00000011b,11000001b,00010000b,01101000b,no_changes         ;Tentacruel
 db 10000101b,11000000b,11110000b,01110011b,01110100b,00010011b,01000100b,no_changes         ;Geodude
 db 10000101b,11000000b,11110000b,01110011b,01110100b,00010011b,01000100b,no_changes         ;Graveler
 db 10001101b,11000010b,11110000b,01110011b,01110100b,00010011b,01000100b,no_changes         ;Golem
 db 00000111b,11000000b,00010000b,00000011b,11000111b,00010000b,01000000b,no_changes         ;Ponyta
 db 00000111b,11000010b,00010000b,00000011b,11000111b,00010000b,01000000b,no_changes         ;Rapidash
 db 00000101b,11111101b,00010000b,01111111b,11000111b,00011100b,11001110b,no_changes         ;Slowpoke
 db 10001101b,11111111b,11110000b,01111111b,11000111b,00011100b,11001110b,no_changes         ;Slowbro
 db 00000100b,11000000b,00010001b,10000111b,11000010b,00011000b,01000010b,no_changes         ;Magnemite
 db 00000100b,11000010b,00010001b,10000111b,11000010b,00011000b,01000010b,no_changes         ;Magneton
 db 01110101b,11000000b,00010000b,00000011b,00000011b,00010000b,01110000b,no_changes         ;Farfetch'd
 db 00010101b,11000000b,00010000b,00000011b,11000001b,00110000b,11010000b,no_changes         ;Doduo
 db 00010101b,11000010b,00010000b,00000011b,11000001b,00110000b,11010000b,no_changes         ;Dodrio
 db 00000111b,11111101b,00010000b,00000011b,01000001b,00000000b,00001100b,no_changes         ;Seel
 db 00000111b,11111111b,00010000b,00000011b,01000001b,00000000b,00001100b,no_changes         ;Dewgong
 db 00000101b,00000000b,00011001b,10000011b,01010100b,00010010b,01000000b,no_changes         ;Grimer
 db 00000101b,00000010b,00011001b,10000011b,01010100b,00010010b,01000000b,no_changes         ;Muk
 db 00000100b,11111100b,00010000b,00000111b,11010010b,00010010b,11001000b,no_changes         ;Shellder
 db 00000100b,11111110b,00010000b,00000111b,11010010b,00010010b,11001000b,no_changes         ;Cloyster
 db 00000100b,00000000b,00011001b,10001011b,01010000b,01010110b,01000000b,no_changes         ;Gastly
 db 00000100b,00000000b,00011001b,10001011b,01010000b,01010110b,01000000b,no_changes         ;Haunter
 db 10001101b,11000010b,11111001b,10001011b,01110001b,01010110b,01000100b,no_changes         ;Gengar
 db 00000101b,11000000b,00010000b,01110111b,01010001b,00010011b,01000100b,no_changes         ;Onix
 db 10001101b,11000000b,11110000b,00001111b,11100001b,01011100b,11000010b,no_changes         ;Drowzee
 db 10001101b,11000010b,11110000b,00001111b,11100001b,01011100b,11000010b,no_changes         ;Hypno
 db 00100101b,10111110b,00010000b,00000011b,01000001b,00010000b,01101100b,no_changes         ;Krabby
 db 00100101b,10111110b,00010000b,00000011b,01000001b,00010000b,01101100b,no_changes         ;Kingler
 db 00000100b,10000000b,00000001b,10000111b,11010010b,00011010b,01000010b,no_changes         ;Voltorb
 db 00000100b,10000010b,00000001b,10000111b,11010010b,00011010b,01000010b,no_changes         ;Electrode
 db 00000100b,11000000b,00010000b,00001111b,11011000b,00010110b,01000000b,no_changes         ;Exeggcute
 db 00000100b,11000010b,00011100b,00001111b,11011000b,00010110b,01000100b,no_changes         ;Exeggutor
 db 10001101b,11111100b,11110000b,01110011b,01000101b,00010000b,01000100b,no_changes         ;Cubone
 db 10001101b,11111110b,11110000b,01110011b,01000101b,00010000b,01000100b,no_changes         ;Marowak
 db 10001101b,11000000b,11110000b,00000011b,01100011b,00010000b,01000100b,no_changes         ;Hitmonlee
 db 10001101b,11000000b,11110000b,00000011b,01100011b,00010000b,01000100b,no_changes         ;Hitmonchan
 db 10101101b,11111111b,11110001b,11100011b,01000101b,00010000b,01101100b,no_changes         ;Lickitung
 db 00000100b,00000000b,00010001b,10000011b,01010100b,00010010b,01000000b,no_changes         ;Koffing
 db 00000100b,00000010b,00010001b,10000011b,01010100b,00010010b,01000000b,no_changes         ;Weezing
 db 00000111b,11000000b,00010001b,11110111b,01000101b,00010001b,01000100b,no_changes         ;Rhyhorn
 db 10001111b,11111111b,11110001b,11110011b,01000101b,00010001b,01001100b,no_changes         ;Rhydon
 db 10001101b,11111110b,11110101b,10001111b,11101101b,10011100b,11000110b,no_changes         ;Chansey
 db 00100101b,11000010b,00011100b,00000011b,01000001b,00010000b,01100000b,no_changes         ;Tangela
 db 10001101b,11111110b,11110001b,11100011b,01000101b,00010001b,01001100b,no_changes         ;Kangaskhan
 db 00000100b,11111100b,00010000b,00000011b,01000011b,00010000b,01001000b,no_changes         ;Horsea
 db 00000100b,11111110b,00010000b,00000011b,01000011b,00010000b,01001000b,no_changes         ;Seadra
 db 00000110b,11111100b,00010000b,00000011b,01000011b,00010000b,01001000b,no_changes         ;Goldeen
 db 00000110b,11111110b,00010000b,00000011b,01000011b,00010000b,01001000b,no_changes         ;Seaking
 db 00000100b,11111100b,00010001b,10001111b,11000011b,00011100b,11001010b,no_changes         ;Staryu
 db 00000100b,11111110b,00010001b,10001111b,11000011b,00011100b,11001010b,no_changes         ;Starmie
 db 10001101b,11000010b,11110101b,10001111b,11100001b,00011100b,01000010b,no_changes         ;Mr. Mime
 db 00100100b,11000010b,00010000b,00000011b,01000011b,00010000b,01100000b,no_changes         ;Scyther
 db 10001101b,11111110b,11110000b,00001111b,11100001b,00010100b,01000000b,no_changes         ;Jynx
 db 10001101b,11000010b,11110001b,10001111b,11100011b,00011100b,01000110b,no_changes         ;Electabuzz
 db 10001101b,11000010b,11110000b,00001111b,01100101b,00010100b,01000100b,no_changes         ;Magmar
 db 00100101b,11000010b,10110000b,00000011b,01000000b,00010000b,01100000b,no_changes         ;Pinsir
 db 00000111b,11001110b,00010001b,11100011b,01000101b,00000000b,01000100b,no_changes         ;Tauros
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,no_changes         ;Magikarp
 db 00000101b,11111110b,00010011b,10000011b,11000101b,00010000b,01001100b,no_changes         ;Gyarados
 db 00000111b,11111110b,00010111b,10001011b,11000001b,00010100b,01001100b,no_changes         ;Lapras
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,no_changes         ;Ditto
 db 00000101b,11000000b,00010000b,00000011b,11000011b,00010000b,01000000b,no_changes         ;Eevee
 db 00000101b,11111110b,00010000b,00000011b,11000011b,00010000b,01001000b,no_changes         ;Vaporeon
 db 00000101b,11000010b,00010001b,10000011b,11000011b,00011000b,01000010b,no_changes         ;Jolteon
 db 00000101b,11000010b,00010000b,00000011b,11000111b,00010000b,01000000b,no_changes         ;Flareon
 db 00000100b,11001110b,00010001b,10001111b,11000011b,00011100b,11000010b,no_changes         ;Porygon
 db 00000101b,11111100b,00010000b,00000011b,11000000b,00010000b,01001000b,no_changes         ;Omanyte
 db 00000111b,11111110b,10110000b,00000011b,11000001b,00010000b,01001000b,no_changes         ;Omastar
 db 01101101b,11111110b,10110000b,00000011b,11000001b,00010000b,01001000b,          plus_many;Kabuto
; Kabuto: +Razor Wind +Swords Dance +Mega Kick +Hyper Beam +Submission +Seismic Toss +Skull Bash
 db 01101101b,11111110b,10110000b,00000011b,11000001b,00010000b,01101000b,           plus_cut;Kabutops
 db 01010100b,11000010b,00010010b,00000011b,11000110b,00110000b,01010000b,no_changes         ;Aerodactyl
 db 10001101b,11111111b,11110101b,11101011b,11110101b,00010101b,01001100b,no_changes         ;Snorlax
 db 01010100b,11111110b,00010000b,00000011b,11000010b,00110000b,01010000b,no_changes         ;Articuno
 db 01010100b,11000010b,00010001b,10000011b,11000010b,00111000b,01010010b,no_changes         ;Zapdos
 db 01010100b,11000010b,00010000b,00000011b,11000110b,00110000b,00010000b,no_changes         ;Moltres
 db 00000101b,11111100b,00010011b,10000011b,11000111b,00011000b,01001000b,no_changes         ;Dratini
 db 00000111b,11111100b,00010011b,10000011b,11000111b,00011000b,01001000b,no_changes         ;Dragonair
 db 01000111b,11111110b,00010011b,10000011b,11000111b,00011000b,01001100b,no_changes         ;Dragonite
 db 10001101b,11111111b,11110101b,10001111b,11110101b,00011100b,11000110b,       minus_payday;Mewtwo
 db 11111111b,11111111b,11111111b,11111111b,11111111b,11111111b,11111110b,no_changes         ;Mew
tmhm_gs:
 db 01100100b,01111000b,10111100b,00100011b,01100001b,00011000b,10100010b,00000000b
 db 01100100b,01111000b,10111100b,00100011b,01100001b,00011000b,10100010b,00000000b
 db 01101100b,01111010b,10111100b,00100011b,01100001b,00011000b,10100010b,00000000b
 db 11100101b,01101000b,10011011b,00110011b,01100111b,00011001b,10100100b,00000000b
 db 11100101b,01101000b,10011011b,00110011b,01100111b,00011001b,10100100b,00000000b
 db 11101101b,01101010b,10011011b,01110011b,01101111b,00011011b,10110100b,00000000b
 db 11110101b,01001101b,11011010b,00110011b,11100001b,00011000b,00001101b,10000000b
 db 11110101b,01001101b,11011010b,00110011b,11100001b,00011000b,00001101b,10000000b
 db 11111101b,01001111b,11011010b,01110011b,11100001b,00011000b,00001101b,10000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00100100b,01111010b,10111100b,00101001b,01100010b,00011000b,01000010b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00100100b,01111010b,10011000b,00100001b,01110010b,00011000b,00100000b,00000000b
 db 00100100b,01101000b,10011000b,00100011b,01100010b,00111110b,00010000b,00000000b
 db 00100100b,01101000b,10011000b,00100011b,01100010b,00111110b,00010000b,00000000b
 db 00100100b,01101010b,10011000b,00100011b,01100010b,00111110b,00010000b,00000000b
 db 01100101b,01101101b,10011010b,10110111b,01100011b,00011100b,00000000b,00000000b
 db 01101101b,01101111b,10011010b,10110111b,01100011b,00011100b,00100100b,00000000b
 db 00100100b,01101000b,10011000b,00100011b,01100010b,00111110b,00010000b,00000000b
 db 00100100b,01101010b,10011000b,00100011b,01100010b,00111110b,00010000b,00000000b
 db 01100100b,01101000b,10111000b,01110001b,01110000b,00011100b,00000100b,00000000b
 db 01100100b,01101010b,10111000b,01110001b,01110000b,00011100b,00000100b,00000000b
 db 11110110b,01001000b,11011010b,10100011b,01100011b,10111000b,00000110b,00000000b
 db 11110110b,01001010b,11011010b,10100011b,01100011b,10111100b,00000110b,00000000b
 db 11110101b,01101000b,10011010b,01110011b,01101011b,00111100b,10100100b,00000000b
 db 11110101b,01101010b,10011010b,01110011b,01101011b,00111100b,10100100b,00000000b
 db 01100100b,01101100b,11011010b,10100011b,01100001b,00111100b,00000000b,00000000b
 db 01100101b,01101100b,11011010b,10100011b,01100001b,00111100b,00000100b,00000000b
 db 11111101b,01101111b,11011010b,11100111b,11101101b,10111101b,10001100b,00000000b
 db 01100100b,01101100b,11011010b,10100011b,01100001b,00111100b,00000000b,00000000b
 db 01100101b,01101100b,11011010b,10100011b,01100001b,00111100b,00000100b,00000000b
 db 11101101b,01101111b,11011010b,11100111b,11101101b,10111101b,10001100b,00000000b
 db 11110110b,11101100b,11011110b,10101111b,11100101b,11111001b,01000110b,00000000b
 db 11110110b,11101110b,11011110b,10101111b,11100101b,11111001b,01000110b,00000000b
 db 01101100b,01101000b,10011010b,00110001b,01100110b,00011000b,00000000b,00000000b
 db 01101100b,01101010b,10011010b,00110001b,01100110b,00011000b,00000000b,00000000b
 db 11110110b,11101100b,11011100b,10101111b,11100101b,11111001b,01000110b,00000000b
 db 11110110b,11101110b,11011100b,10101111b,11100101b,11111001b,01000110b,00000000b
 db 00100100b,01101000b,10111000b,00100001b,01100010b,00111110b,00000000b,00000000b
 db 00100100b,01101010b,10111000b,00100001b,01100010b,00111110b,00000000b,00000000b
 db 00100100b,01111000b,10111100b,00100001b,01110000b,00011000b,00100010b,00000000b
 db 00100100b,01111000b,10111100b,00100001b,01110000b,00011000b,00100010b,00000000b
 db 00100100b,01111010b,10111100b,00100001b,01110000b,00011000b,00100010b,00000000b
 db 00100101b,01111000b,10111100b,00110001b,01110000b,00011100b,10100010b,00000000b
 db 00100101b,01111010b,10111100b,00110001b,01110000b,00011100b,10100010b,00000000b
 db 00100100b,01111000b,10111100b,00101001b,01110010b,00011100b,00000000b,00000000b
 db 00100100b,01111010b,10111100b,00101001b,01110010b,00011100b,00000010b,00000000b
 db 00100101b,01101000b,10011000b,01110011b,01110000b,00011100b,00100000b,00000000b
 db 00100101b,01101010b,10011000b,01110011b,01110000b,00011100b,00100000b,00000000b
 db 01100110b,11101001b,10011010b,10100111b,01100011b,01111100b,01000000b,00000000b
 db 01101110b,11101011b,10011010b,10100111b,01100011b,01111100b,01000000b,00000000b
 db 11100101b,11001101b,11011010b,00110011b,11100010b,00011000b,00001111b,10000000b
 db 11100101b,11001111b,11011010b,00110011b,11100010b,00011000b,10001111b,10000000b
 db 11100101b,11101000b,10011010b,10110011b,11100011b,10111101b,00000100b,00000000b
 db 11100101b,11101010b,10011010b,10110011b,11100011b,10111101b,00000100b,00000000b
 db 01101101b,01101000b,10011011b,00110001b,01100110b,00011000b,00000000b,00000000b
 db 01101101b,01101010b,10011011b,00110001b,01100110b,00011000b,00000000b,00000000b
 db 01100100b,01001101b,11011000b,00101001b,01100001b,00011100b,00001001b,10000000b
 db 01100101b,01001101b,11011000b,01101011b,11100001b,00111100b,00001101b,10000000b
 db 11100101b,01001111b,11011000b,01101011b,11100001b,00111100b,00001101b,10000000b
 db 11100110b,11101000b,11011000b,00101101b,11100000b,11011101b,01000010b,00000000b
 db 11100110b,11101000b,11011000b,00111101b,11100000b,11011101b,01000010b,00000000b
 db 11100110b,11101010b,11011000b,00111101b,11100000b,11011101b,01000010b,00000000b
 db 11100101b,01101000b,10011000b,01110011b,11100100b,10111101b,00000100b,00000000b
 db 11100101b,01101000b,10011000b,01110011b,11100100b,10111101b,00000100b,00000000b
 db 11100101b,01101010b,10011000b,01110011b,11100100b,10111101b,00000100b,00000000b
 db 00100100b,01111000b,10111100b,00100001b,01110000b,00011000b,00100010b,00000000b
 db 00100100b,01111000b,10111100b,00100001b,01110000b,00011000b,00100010b,00000000b
 db 00100100b,01111010b,10111100b,00100001b,01110000b,00011000b,00100010b,00000000b
 db 00100100b,01001101b,11111000b,00100001b,01110000b,00011000b,00101001b,00000000b
 db 00100100b,01001111b,11111000b,00100001b,01110000b,00011000b,00101001b,00000000b
 db 11111101b,01101000b,10011000b,01110011b,01101101b,00011001b,10000100b,00000000b
 db 11111101b,01101000b,10011000b,01110011b,01101101b,00011001b,10000100b,00000000b
 db 11111101b,01101010b,10011000b,01110011b,01101101b,00011001b,10000100b,00000000b
 db 01100100b,01101000b,10011010b,00100001b,01100110b,00011000b,00000000b,00000000b
 db 01100100b,01101010b,10011010b,00100001b,01100110b,00011000b,00000000b,00000000b
 db 11100110b,11101101b,11011010b,01111111b,11100110b,01011000b,01001110b,00000000b
 db 11100111b,11101111b,11011010b,01111111b,11100110b,01011000b,11001110b,00000000b
 db 00110110b,01001001b,11011000b,10100001b,01100010b,00010000b,00000010b,00000000b
 db 00110110b,01001011b,11011000b,10100001b,01100010b,00010000b,00000010b,00000000b
 db 01100100b,11101000b,10011010b,00100011b,01100010b,00111110b,00110000b,00000000b
 db 00100100b,01101000b,10011100b,00100011b,01100010b,00011110b,00010000b,00000000b
 db 00100100b,01101010b,10011100b,00100011b,01100010b,00011110b,00010000b,00000000b
 db 01100100b,01001101b,11011000b,00100001b,01100000b,00011000b,00001001b,10000000b
 db 01100100b,01001111b,11011000b,00100001b,01100000b,00011000b,00001001b,10000000b
 db 10100110b,01101000b,10111000b,10100011b,11110100b,10011101b,00000000b,00000000b
 db 10100110b,01101010b,10111000b,10100011b,11110100b,10011101b,00000000b,00000000b
 db 00100100b,01001101b,11011000b,00100001b,01100010b,00011000b,00001001b,00000000b
 db 00100100b,01001111b,11011000b,00100001b,01100010b,00011000b,00001001b,00000000b
 db 00100110b,11101000b,11111000b,10101101b,01100000b,01011100b,01000000b,00000000b
 db 00100110b,11101000b,11111000b,10101101b,01100000b,01011100b,01000000b,00000000b
 db 11100111b,11101010b,11111000b,10101101b,11100000b,11011101b,01000100b,00000000b
 db 01101101b,01101000b,10011010b,01110011b,01101000b,00011000b,00000100b,00000000b
 db 11100110b,11101000b,11011000b,00101101b,11100000b,11011001b,01000010b,00000000b
 db 11100110b,11101010b,11011000b,00101101b,11100000b,11011001b,01000010b,00000000b
 db 00100101b,01001111b,11011000b,00100011b,01100000b,00011100b,10101101b,00000000b
 db 00100101b,01001111b,11011000b,00100011b,01100000b,00011100b,10101101b,00000000b
 db 01110110b,01001000b,11011000b,10100001b,01100010b,00010000b,00000010b,00000000b
 db 01110110b,01001010b,11011000b,10100001b,01100010b,00010000b,00000010b,00000000b
 db 01110100b,11101000b,10111100b,00101001b,01110000b,01011100b,01000110b,00000000b
 db 01110100b,11101010b,10111100b,00101001b,01110000b,01011100b,01000110b,00000000b
 db 11100101b,01101101b,10011010b,01110011b,01101100b,10111101b,00000100b,00000000b
 db 11100101b,01101111b,10011010b,01110011b,01101100b,10111101b,00000100b,00000000b
 db 11100101b,01101000b,10010000b,00000011b,01101010b,00111100b,00000100b,00000000b
 db 11100101b,01101000b,10011000b,00100011b,11100010b,10111101b,00000100b,00000000b
 db 11110101b,11101111b,11011010b,11100111b,11101101b,11011101b,01101100b,00000000b
 db 00110110b,01101000b,10011000b,10100001b,01110100b,00011100b,00000000b,00000000b
 db 00110110b,01101010b,10011000b,10100001b,01110100b,00011100b,00000000b,00000000b
 db 01111111b,01101101b,10011010b,11110011b,01101100b,00011000b,00000100b,00000000b
 db 11111111b,01101111b,10011010b,11110011b,01101100b,10011001b,10001100b,00000000b
 db 11110111b,11101111b,11011110b,10101111b,01101101b,01011000b,00000110b,00000000b
 db 01100100b,11111010b,10111100b,00100001b,01110000b,00011100b,00100010b,00000000b
 db 11101111b,01101111b,11011010b,11100111b,11101100b,10011001b,10001100b,00000000b
 db 01100100b,01001101b,11011001b,00100001b,01100010b,00011000b,00001001b,10000000b
 db 01100100b,01001111b,11011001b,00100001b,01100010b,00011000b,00001001b,10000000b
 db 00100100b,01001101b,11011000b,00100001b,01100010b,00011000b,00001000b,10000000b
 db 00100100b,01001111b,11011000b,00100001b,01100010b,00011000b,00001000b,10000000b
 db 00100110b,11001101b,11011000b,10101001b,01100010b,00011000b,00001011b,10000000b
 db 00100110b,11001111b,11011000b,10101001b,01100010b,01011000b,01001011b,10000000b
 db 11100110b,11101010b,10011100b,10101111b,11100000b,11011101b,01000010b,00000000b
 db 01100101b,01101010b,10011000b,00100001b,01100010b,00111110b,10100000b,00000000b
 db 11100100b,11011111b,11011000b,00101111b,11100000b,01011100b,01000000b,00000000b
 db 11100111b,01001010b,11011010b,10101011b,11100010b,10111101b,00000110b,00000000b
 db 11100101b,01101010b,10011010b,00101011b,01100100b,10111101b,00000100b,00000000b
 db 01100101b,01101010b,10011000b,00100001b,01100000b,00011100b,10100100b,00000000b
 db 01100111b,01101111b,10011010b,11100001b,01100100b,00011000b,00001100b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 01101111b,01001111b,11011001b,10100001b,01101100b,00011000b,00001101b,10000000b
 db 01100111b,01001111b,11011011b,10101001b,01100000b,01011000b,01001101b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 01100100b,01101000b,11011010b,00100111b,01100010b,00111000b,00000000b,00000000b
 db 01101100b,01101111b,11011010b,00100111b,01100010b,00111000b,00001001b,10000000b
 db 01101110b,01101010b,11011010b,10100111b,01100010b,00111000b,00000010b,00000000b
 db 01101110b,01101010b,11011010b,00100111b,01100110b,00111000b,00000000b,00000000b
 db 00100110b,11101111b,11011010b,10101001b,01100010b,01010100b,01000010b,00000000b
 db 01110101b,01001111b,11011000b,00100001b,01101000b,00011100b,00001001b,00000000b
 db 01110101b,01001111b,11011000b,00100001b,01101000b,00011100b,00001001b,00000000b
 db 00110101b,01001101b,11111000b,00100001b,01101000b,00011100b,00000000b,00000000b
 db 01110101b,01001111b,11111000b,00100001b,01101000b,00011100b,10101001b,00000000b
 db 01101101b,01001010b,11011011b,01100001b,01101110b,00111010b,00010000b,00000000b
 db 11110111b,11101111b,11011100b,11101111b,11101101b,10011001b,00001100b,00000000b
 db 00101101b,01101111b,11011000b,00100011b,01101010b,00110010b,00010000b,00000000b
 db 00101111b,01101010b,11011000b,10100011b,01101010b,00110010b,00010010b,00000000b
 db 00101101b,01101010b,11011000b,00100011b,01101110b,00110010b,00010000b,00000000b
 db 01100110b,01001101b,11011011b,10100001b,01100110b,00111000b,00001000b,10000000b
 db 01100110b,01001101b,11011011b,10100001b,01100110b,00111000b,00001000b,10000000b
 db 11100111b,01001111b,11011011b,10100011b,11101110b,10111011b,10011101b,10000000b
 db 11100111b,11101111b,11011110b,10101111b,11100110b,11110001b,01000110b,00000000b
 db 11111111b,11111111b,11111111b,11111111b,11111111b,11111111b,11111111b,10000000b
 db 01100100b,01111000b,10111110b,01100011b,01100000b,00111000b,00100010b,00000000b
 db 01100101b,01111000b,10111110b,01100011b,01100000b,00111000b,10100010b,00000000b
 db 01100101b,01111010b,10111110b,01100011b,01100000b,00111000b,10100110b,00000000b
 db 01110100b,01101000b,10011010b,00110011b,01100111b,00111000b,00100000b,00000000b
 db 01111101b,01101000b,10011010b,00110011b,01100111b,00111000b,10100100b,00000000b
 db 11111101b,01101010b,10011010b,01110011b,01100111b,10111001b,10100100b,00000000b
 db 11101101b,01001101b,11011010b,01110011b,11100000b,00111000b,10101001b,00000000b
 db 11101101b,01001101b,11011010b,01110011b,11100000b,00111000b,10101101b,00000000b
 db 11101101b,01001111b,11011010b,01110011b,11100000b,00111000b,10101101b,00000000b
 db 11110100b,01101000b,10011010b,00110111b,11100011b,10111101b,10101000b,00000000b
 db 11110100b,01101010b,10011010b,00110111b,11100011b,10111101b,10101100b,00000000b
 db 00100100b,01101000b,10011000b,00100011b,01100010b,01111110b,01010010b,00000000b
 db 00100100b,01101010b,10011000b,00100011b,01100010b,01111110b,01010010b,00000000b
 db 11110100b,01111000b,10111100b,00110001b,11100010b,10011100b,00000010b,00000000b
 db 11110111b,11111010b,10111100b,00110001b,11100010b,10011100b,00000010b,00000000b
 db 00100100b,01101000b,10111100b,00111001b,01110000b,00011100b,00000010b,00000000b
 db 00100100b,01101010b,10111100b,00111001b,01110000b,00011100b,00000010b,00000000b
 db 00100100b,01101010b,10111000b,00100001b,01100010b,00111110b,00010000b,00000000b
 db 00100110b,01001000b,11011000b,10100001b,01000000b,00011000b,00001011b,10000000b
 db 00100110b,01001010b,11011000b,10100001b,01100000b,00011000b,00001011b,10000000b
 db 01110110b,01001000b,11011010b,10100011b,01100011b,00111000b,00000010b,00000000b
 db 01110110b,11101001b,11011110b,00101111b,01100101b,01111000b,01000010b,00000000b
 db 01110110b,11101001b,11011100b,00101111b,01100101b,01111000b,01000010b,00000000b
 db 01110111b,01101000b,11010100b,00101111b,01100111b,01111000b,00000010b,00000000b
 db 01110111b,11101010b,11011100b,00101111b,01100111b,01111010b,00010010b,00000000b
 db 00100100b,11101110b,10111100b,00101001b,01100010b,01111100b,01000010b,00000000b
 db 00100100b,11101110b,10111100b,00101001b,01100010b,01111100b,01010010b,00000000b
 db 11100111b,01001000b,11011010b,10100001b,01100011b,10011001b,00000010b,00000000b
 db 11100111b,01001000b,11011010b,10100001b,01100011b,10011001b,00000110b,00000000b
 db 11100111b,01001010b,11011010b,10100001b,01100011b,10011001b,00000110b,00000000b
 db 00100100b,01111010b,10111100b,00100001b,01100000b,00011000b,00100010b,00000000b
 db 11110101b,01001101b,11011010b,00100000b,11100011b,00011000b,00001001b,10000000b
 db 11110101b,01001111b,11011010b,00100000b,11100011b,00011000b,00001101b,10000000b
 db 11110101b,11101000b,10011000b,01110011b,11101001b,10011101b,00000100b,00000000b
 db 11100101b,01001111b,11011000b,01101011b,11100001b,00111100b,00001101b,10000000b
 db 01100100b,01111000b,10111100b,00100001b,01100001b,00011111b,11000010b,00000000b
 db 01100100b,01111000b,10111100b,00100001b,01100001b,00011111b,11000010b,00000000b
 db 01100100b,01111010b,10111100b,00100001b,01100001b,00011000b,00000010b,00000000b
 db 11100111b,01101000b,10011010b,10100111b,11100011b,11111101b,11100100b,00000000b
 db 00100100b,01111000b,10111100b,00100001b,01110000b,00011000b,00100010b,00000000b
 db 00100100b,01111010b,10111100b,00100001b,01110000b,00011000b,00100010b,00000000b
 db 01100100b,01101000b,10111100b,00100001b,01100010b,00111100b,00000010b,00000000b
 db 11110101b,01001000b,11011010b,01110011b,11111001b,00011000b,00001011b,00000000b
 db 11110101b,01001010b,11011010b,01110011b,11111001b,00011000b,00001111b,00000000b
 db 01100110b,11101010b,11011010b,00101111b,01100010b,01111000b,01100010b,00000000b
 db 01100110b,11111010b,11011010b,00101111b,01100010b,01111000b,01100010b,00000000b
 db 00100100b,11101001b,10011000b,00100111b,01100010b,01111110b,01010000b,00000000b
 db 11100111b,11101111b,11011010b,01111111b,11100110b,01011000b,11001111b,00000000b
 db 01100110b,01101000b,11010000b,10101101b,01100011b,01011100b,01000010b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 01100111b,11101000b,10011010b,11101111b,01110010b,01011100b,01000100b,00000000b
 db 01110101b,01111000b,10111100b,00100001b,01101001b,00011000b,00000100b,00000000b
 db 01110101b,01111010b,10111100b,00100001b,01101001b,00011000b,00000100b,00000000b
 db 01110111b,11101000b,11011110b,10110011b,01100001b,01011100b,01000100b,00000000b
 db 01100101b,01101000b,10011010b,00100001b,01111010b,00111100b,10100100b,00000000b
 db 01111101b,01101010b,10011011b,01110011b,01101001b,00011000b,00100100b,00000000b
 db 11101111b,01101000b,11011000b,10100111b,11110001b,10111101b,00000100b,00000000b
 db 11101111b,01101010b,11011000b,10100111b,11110001b,10111101b,00000100b,00000000b
 db 01110100b,01001101b,11011000b,00100001b,01110011b,00011000b,00001001b,10000000b
 db 01100101b,01101010b,11011000b,00100001b,01101010b,00111110b,10100100b,00000000b
 db 01110101b,01101000b,10011000b,01110011b,01111001b,00011000b,00000110b,00000000b
 db 01100101b,01101000b,10011000b,01100001b,01100000b,00111100b,10100100b,00000000b
 db 11100101b,11001101b,11011010b,00110111b,11100011b,01111111b,11101100b,00000000b
 db 11111111b,01101000b,10011000b,01110011b,11100011b,10011101b,10100100b,00000000b
 db 11111111b,01101010b,10011000b,01110011b,11100011b,10011101b,10100100b,00000000b
 db 00110101b,01101000b,10011000b,01100011b,01100101b,00011000b,00000100b,00000000b
 db 00110101b,01101010b,10011000b,01100011b,01100101b,00011000b,00000100b,00000000b
 db 01101101b,01001101b,11011000b,01100011b,01100001b,00111000b,00000100b,00000000b
 db 01101101b,01001111b,11011000b,01100011b,01100001b,00111000b,00000100b,00000000b
 db 01110101b,11101000b,11011000b,01101011b,01101001b,00011000b,00001101b,00000000b
 db 00100100b,01001000b,11011000b,00100011b,11100011b,00011100b,00001001b,00000000b
 db 00100100b,01001010b,11011000b,00100011b,11100011b,00011100b,00001001b,00000000b
 db 01100100b,01001101b,11011000b,00100011b,01100010b,00111100b,00010000b,00000000b
 db 01100100b,01001101b,11011000b,00100011b,01100010b,00011000b,00001001b,10000000b
 db 00100100b,01101000b,10011000b,00100011b,01101010b,00111110b,00110000b,00000000b
 db 01101101b,01101000b,10011110b,00100111b,01110110b,01111100b,01000100b,00000000b
 db 01101101b,01101010b,10011110b,00100111b,01110110b,01111100b,01000100b,00000000b
 db 01100100b,01001111b,11011001b,00100001b,01100010b,00011000b,00001001b,10000000b
 db 01111101b,01101000b,10011000b,01100011b,01101001b,00011000b,00000100b,00000000b
 db 01111101b,01101010b,10011000b,01100011b,01101001b,00011000b,00000100b,00000000b
 db 00100110b,11101111b,11011010b,10101001b,01100011b,01010100b,01000010b,00000000b
 db 01101100b,11101000b,11011000b,01101011b,01100010b,01111100b,01000010b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 01100101b,01101000b,10010000b,00100011b,01100010b,00111100b,00000100b,00000000b
 db 01100101b,01101000b,10010000b,00110011b,01100010b,00111100b,00000100b,00000000b
 db 10100100b,01011101b,11010000b,00101111b,11100000b,01011100b,01000000b,00000000b
 db 11100110b,01001000b,11010000b,10101011b,11100010b,10111101b,00000010b,00000000b
 db 11100100b,01101000b,10010010b,00101011b,01100100b,10111101b,00000000b,00000000b
 db 11110111b,01111111b,11010010b,11100111b,11101001b,10011001b,00001100b,00000000b
 db 11110111b,01101111b,11010100b,10101111b,01101101b,01011000b,00000110b,00000000b
 db 01101111b,11101010b,11010010b,10110011b,01101010b,00110000b,00100110b,00000000b
 db 01101101b,11101010b,11010110b,00110011b,01101110b,00110000b,00100110b,00000000b
 db 01101101b,01101111b,11010010b,00110011b,01101010b,00110000b,00101001b,10000000b
 db 11101101b,01101000b,11010011b,01110011b,01101100b,00111001b,11101000b,00000000b
 db 11101101b,01101000b,11010011b,01110011b,01101100b,00111001b,11101000b,00000000b
 db 11101101b,01101010b,11010011b,01110011b,01101100b,00111001b,11101100b,00000000b
 db 01101111b,01101111b,11110011b,11101111b,01101010b,01110010b,01011101b,10000000b
 db 00101111b,01101010b,11110101b,11101111b,01101110b,01110010b,01010110b,00000000b
 db 00100100b,11111010b,11111100b,00101111b,01101011b,01110000b,01000010b,00000000b

tmhm_rs:
 db 00000100b,11100000b,10101100b,00100001b,00010000b,01111000b,00100111b,00000000b
 db 00000100b,11100000b,10101100b,00100001b,00010000b,01111000b,00100111b,00000000b
 db 00001100b,11100010b,10101100b,01100001b,00010000b,01111000b,00100111b,00000000b
 db 11000100b,01100000b,10001010b,00110011b,00100101b,01111000b,01100101b,00000000b
 db 11000100b,01100000b,10001010b,00110011b,00100101b,01111000b,01100101b,00000000b
 db 11001100b,01100010b,10001010b,01110011b,00100101b,01111010b,01110101b,00000000b
 db 10100110b,01001100b,11001010b,00110011b,00000000b,01111000b,00001101b,11000000b
 db 10100110b,01001100b,11001010b,00110011b,00000000b,01111000b,00001101b,11000000b
 db 10101110b,01001110b,11001010b,01110011b,00000000b,01111000b,00001101b,11000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000100b,01100010b,11111100b,00101101b,00000001b,01111101b,00000010b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000100b,01100010b,10101100b,00100011b,00010001b,01111100b,00100001b,00000000b
 db 00000100b,01100000b,11001000b,00100001b,00000001b,01111110b,00010000b,00000000b
 db 00000100b,01100000b,11001000b,00100001b,00000001b,01111110b,00010000b,00000000b
 db 00000100b,01100010b,11001000b,00100001b,00000001b,01111110b,00010000b,00000000b
 db 00000100b,01111100b,11001011b,10110101b,01000000b,01111100b,00100001b,00000000b
 db 00001100b,01111110b,11001011b,10110101b,01000000b,01111100b,00100101b,00000000b
 db 00000100b,01100000b,11001000b,00100001b,00000001b,01111110b,00010000b,00000000b
 db 00000100b,01100010b,11001000b,00100001b,00000001b,01111110b,00010000b,00000000b
 db 00000100b,01100000b,11101010b,01110001b,00010000b,11111100b,10000100b,00000000b
 db 00000100b,01100010b,11101010b,01110001b,00010000b,11111100b,10000100b,00000000b
 db 10000100b,01000001b,11001011b,10110011b,01000000b,01111000b,00000111b,00000000b
 db 10000100b,01000011b,11001011b,10110011b,01000000b,01111100b,00000111b,00000000b
 db 10000100b,01100000b,10001010b,01110011b,00001011b,01111100b,00100101b,00000000b
 db 10000100b,01100010b,10001010b,01110011b,00001011b,01111100b,00100101b,00000000b
 db 00100100b,01101100b,11001011b,10110001b,01010001b,01111100b,00100101b,00000000b
 db 00100100b,01101100b,11001011b,10110001b,01010001b,01111100b,00100101b,00000000b
 db 10101100b,01111110b,11001011b,11110111b,01111111b,11111100b,00101101b,00000000b
 db 00100100b,01101100b,11001011b,10110001b,01010000b,01111100b,00100101b,00000000b
 db 00100100b,01101100b,11001011b,10110001b,01010000b,01111100b,00100101b,00000000b
 db 10101100b,01111110b,11001011b,11110111b,01111110b,11111100b,00101101b,00000000b
 db 10110100b,01101101b,11011111b,10111111b,11000100b,01111000b,10000110b,00000000b
 db 10110100b,01101111b,11011111b,10111111b,11100100b,01111000b,10000110b,00000000b
 db 00001100b,01100000b,10011010b,00110001b,10100100b,01111000b,01000000b,00000000b
 db 00001100b,01100010b,10011010b,00110001b,10100100b,01111000b,01000000b,00000000b
 db 10100100b,01101101b,11011101b,10111111b,11100100b,01111000b,00000110b,00000000b
 db 10100100b,01101111b,11011101b,10111111b,11100100b,01111000b,00000110b,00000000b
 db 00000100b,01110000b,11101000b,00100101b,00010001b,11111110b,10000000b,00000000b
 db 00000100b,01110010b,11101000b,00100101b,00010001b,11111110b,10000000b,00000000b
 db 00000100b,11100000b,10101100b,00100001b,00010000b,01111000b,00100010b,00000000b
 db 00000100b,11100000b,10101100b,00100001b,00010000b,01111000b,00100010b,00000000b
 db 00000100b,11100010b,10101100b,00100001b,00010000b,01111000b,00100010b,00000000b
 db 00000100b,11100000b,10101100b,00110001b,00010001b,01111100b,00100011b,00000000b
 db 00000100b,11100010b,10101100b,00110001b,00010001b,01111100b,00100011b,00000000b
 db 00000100b,01100000b,10101100b,00101001b,00010000b,01111101b,00000010b,00000000b
 db 00000100b,01100010b,10101100b,00101001b,00010001b,01111101b,00000010b,00000000b
 db 00000100b,01100000b,10001000b,01110001b,00010011b,01111100b,00100001b,00000000b
 db 00000100b,01100010b,10001000b,01110001b,00010011b,01111100b,00100001b,00000000b
 db 00100100b,01110000b,11001011b,10110101b,01000001b,11111100b,10100010b,00000000b
 db 00101100b,01110010b,11001011b,10110101b,01000001b,11111100b,10100010b,00000000b
 db 10110110b,01001100b,11001010b,00110011b,00000001b,01111000b,00001111b,11000000b
 db 10110110b,01001110b,11001010b,00110011b,00000001b,01111000b,00001111b,11000000b
 db 10000101b,01110000b,11001011b,11110011b,00000011b,01111100b,01000101b,00000000b
 db 10000101b,01110010b,11001011b,11110011b,00000011b,01111100b,01000101b,00000000b
 db 00001100b,01100000b,10001010b,00110001b,00100101b,01111100b,01000101b,00000000b
 db 00001100b,01100010b,10001010b,00110001b,00100101b,01111100b,01000101b,00000000b
 db 00100110b,01001100b,11001000b,00111001b,00000000b,01111100b,00001001b,11000000b
 db 10100110b,01001100b,11001000b,01111011b,00000000b,01111100b,00001101b,11000000b
 db 10100111b,01001110b,11001000b,01111011b,00000010b,01111100b,00001101b,11000000b
 db 10010100b,01110001b,11010010b,00101101b,11000000b,11111101b,10000010b,00000000b
 db 10010100b,01110001b,11010010b,00101101b,11000000b,11111101b,10000010b,00000000b
 db 10010100b,01110011b,11010010b,00101101b,11000000b,11111101b,10000010b,00000000b
 db 10000101b,01100000b,11001000b,01110011b,00100110b,01111100b,00000101b,00000000b
 db 10000101b,01100000b,11001000b,01110011b,00100110b,01111100b,00000101b,00000000b
 db 10000101b,01100010b,11001000b,01110011b,00100110b,01111100b,00000101b,00000000b
 db 00000100b,11100000b,10101100b,00100001b,00010000b,01111100b,00100010b,00000000b
 db 00000100b,11100000b,10101100b,00100001b,00010000b,01111100b,00100010b,00000000b
 db 00000100b,11100010b,10101100b,00100001b,00010000b,01111100b,00100010b,00000000b
 db 00100110b,01001100b,11101000b,00100001b,00010000b,01111100b,00101000b,11000000b
 db 00100110b,01001110b,11101000b,00100001b,00010000b,01111100b,00101000b,11000000b
 db 10000100b,01100000b,10001000b,01110011b,00101110b,01111000b,00000101b,00000000b
 db 10000100b,01100000b,10001000b,01110011b,00101110b,01111000b,00000101b,00000000b
 db 10000100b,01100010b,10001000b,01110011b,00101110b,01111000b,00000101b,00000000b
 db 00000100b,01100000b,10001110b,00100001b,00100100b,01111000b,01000100b,00000000b
 db 00000100b,01100010b,10001110b,00100001b,00100100b,01111000b,01000100b,00000000b
 db 00110110b,01101100b,11011010b,01111101b,00100100b,01111001b,00001110b,01000000b
 db 10110110b,01101110b,11011010b,01111111b,00100100b,01111001b,00001111b,01000000b
 db 00000100b,01100000b,11001001b,10100001b,11000000b,01110000b,00000010b,00000000b
 db 00000100b,01100010b,11001001b,10100001b,11000000b,01110000b,00000010b,00000000b
 db 00000100b,01100000b,10001010b,00100001b,00000001b,01111110b,00110000b,00000000b
 db 00000100b,01100000b,10001000b,00100001b,00000001b,11111110b,00010000b,00000000b
 db 00000100b,01110010b,10001000b,00100001b,00000001b,11111110b,00010000b,00000000b
 db 00100110b,01001100b,11011000b,00100001b,00000000b,01111100b,00001000b,11000000b
 db 00100110b,01001110b,11011000b,00100001b,00000000b,01111100b,00001000b,11000000b
 db 00000100b,01110000b,11101001b,10110001b,01110110b,11111100b,00000000b,00000000b
 db 10000100b,01110010b,11101001b,10110011b,01110110b,11111100b,00000101b,00000000b
 db 00100110b,01001100b,11001000b,00100001b,00000000b,01111000b,00001000b,01000000b
 db 00100110b,01001110b,11001000b,00100001b,00000000b,11111000b,00001000b,01000000b
 db 00000100b,01110000b,11101001b,00101101b,00010000b,11111101b,10000000b,00000000b
 db 00000100b,01110000b,11101001b,00101101b,00010000b,11111101b,10000000b,00000000b
 db 10000100b,01110010b,11101001b,10101111b,00010000b,11111101b,10000101b,00000000b
 db 00001100b,01110000b,10001010b,01110001b,00001010b,11111000b,00000101b,00000000b
 db 10010100b,01110001b,11011000b,00101111b,10000000b,11111101b,10000010b,00000000b
 db 10010100b,01110011b,11011000b,00101111b,10000000b,11111101b,10000010b,00000000b
 db 00100110b,01001100b,11001000b,00110001b,00000010b,01111100b,00101101b,01000000b
 db 00100110b,01001110b,11001000b,00110001b,00000010b,01111100b,00101101b,01000000b
 db 00000100b,01010001b,11001001b,10100001b,01000000b,11110100b,00000010b,00000000b
 db 00000100b,01010011b,11001001b,10100001b,01000000b,11110100b,00000010b,00000000b
 db 00110100b,11101000b,10111100b,00101001b,01110000b,01011100b,01000110b,00000000b
 db 00000100b,11100011b,10101100b,00101001b,10010000b,01111101b,00000110b,00000000b
 db 10000100b,01101100b,10001010b,01110011b,00101111b,01111100b,00000101b,00000000b
 db 10000100b,01101110b,10001010b,01110011b,00101111b,01111100b,00000101b,00000000b
 db 10000101b,01100000b,11001000b,01100011b,00000010b,01111100b,00000101b,00000000b
 db 10000101b,01100000b,11001000b,01100011b,00000010b,01111100b,00000101b,00000000b
 db 10100100b,01101110b,11001111b,11110111b,01101110b,01111100b,00101101b,00000000b
 db 00000100b,01110000b,11001001b,10100101b,01110100b,11111100b,00000010b,00000000b
 db 00000100b,01110010b,11001001b,10100101b,01110100b,11111100b,00000010b,00000000b
 db 00001100b,01101110b,11001011b,11110011b,01101110b,01111100b,00000101b,00000000b
 db 10001100b,01101110b,11001011b,11110011b,01101110b,01111100b,00101101b,00000000b
 db 10110110b,01101111b,11011111b,11101111b,01101110b,01111001b,10000111b,00000000b
 db 00000100b,11100010b,10101100b,00100001b,00010000b,01111100b,00100011b,00000000b
 db 10101110b,01101110b,11001111b,11110111b,01101111b,01111100b,00101101b,00000000b
 db 00100110b,01001100b,11001000b,00100001b,00000000b,01111000b,00001000b,11000000b
 db 00100110b,01001110b,11001000b,00100001b,00000000b,01111000b,00001000b,11000000b
 db 00100110b,01001100b,11001000b,00100001b,00000000b,01111000b,00001000b,11000000b
 db 00100110b,01001110b,11001000b,00100001b,00000000b,01111000b,00001000b,11000000b
 db 00100110b,01001101b,11001001b,10101001b,10000000b,01110000b,00001010b,11000000b
 db 00100110b,01001111b,11001001b,10101001b,10000000b,01110000b,00001010b,11000000b
 db 10010100b,01110011b,11011101b,10101111b,11000000b,11111101b,10000010b,00000000b
 db 00000100b,01100010b,11001000b,00100001b,00000001b,01111110b,00100001b,00000000b
 db 10110110b,01011111b,11001000b,00101111b,10000000b,11111101b,00000010b,00000000b
 db 10000100b,01000011b,11001011b,10101011b,01000000b,01111100b,00000111b,00000000b
 db 10000100b,01100010b,10001010b,00101011b,00100100b,01111100b,00000101b,00000000b
 db 10000101b,01100010b,11001000b,01110011b,00000010b,01111100b,00100101b,00000000b
 db 00100100b,01101110b,11001111b,11100001b,01101110b,01111000b,00001101b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00101110b,01011110b,11001001b,11100001b,00101100b,11111000b,00001101b,11000000b
 db 00101110b,01001110b,11011011b,10101001b,01000000b,01111000b,00001101b,11000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000100b,01100000b,11001010b,00110101b,00000000b,01111000b,00000000b,00000000b
 db 00101110b,01101110b,11001010b,00110101b,00000000b,01111000b,00001000b,11000000b
 db 00001100b,01100010b,11001011b,10110101b,01000000b,01111000b,00000010b,00000000b
 db 00001100b,01100010b,11001010b,00110101b,00100100b,01111000b,01000000b,00000000b
 db 00000100b,01101110b,11001111b,10101101b,01000001b,01110100b,00000010b,00000000b
 db 00100110b,01001100b,11001000b,00100001b,00001010b,01111100b,00001001b,11000000b
 db 00100110b,01001110b,11001000b,00100001b,00001010b,01111100b,00001001b,11000000b
 db 00100110b,01001100b,11101000b,00110001b,00001011b,01111100b,00001001b,10000000b
 db 00100110b,01001110b,11101000b,00110011b,00001011b,01111100b,00101001b,11000000b
 db 01001100b,01110010b,11001010b,01100001b,00101111b,11111110b,00010101b,00000000b
 db 10100100b,01101110b,11001101b,11101111b,01101110b,01111000b,00001100b,00000000b
 db 00101110b,01101110b,11001000b,00100001b,10001001b,01110010b,00010001b,00000000b
 db 00001100b,01100011b,11001001b,10100001b,01001001b,01110010b,00010011b,00000000b
 db 00001100b,01100010b,11011000b,00100001b,00101101b,01110010b,01010001b,00000000b
 db 00100110b,01101110b,11011011b,10100001b,01100100b,01111000b,00001000b,10000000b
 db 00100110b,01101110b,11011011b,10100001b,01100100b,01111000b,00001000b,10000000b
 db 11101110b,01101110b,11011011b,11100011b,01101111b,01111010b,00111101b,11000000b
 db 10110111b,01111111b,11011111b,11101111b,11101111b,11110001b,10000111b,00000000b
 db 11111111b,11111111b,11111111b,11111111b,11111111b,11111111b,11111111b,11000000b
 db 00000100b,11100001b,10111110b,00100001b,10000000b,01111000b,00100010b,00000000b
 db 00000100b,11100001b,10111110b,00100001b,10000000b,01111000b,00100111b,00000000b
 db 00000100b,11100011b,10111110b,01100001b,10000000b,01111000b,00100111b,00000000b
 db 00000100b,01100000b,10001000b,00110001b,00100101b,01111000b,01100000b,00000000b
 db 10001100b,01100000b,10001000b,00110011b,00100101b,01111000b,01100101b,00000000b
 db 10001100b,01100010b,10001000b,01110011b,00100101b,01111000b,01100101b,00000000b
 db 10100110b,01001100b,11001010b,00110011b,00000001b,01111000b,00101000b,11000000b
 db 10101110b,01001100b,11001010b,00110011b,00000001b,01111000b,00101101b,11000000b
 db 11101110b,01001110b,11001010b,01110011b,00000001b,01111000b,00101101b,11000000b
 db 10100100b,01101000b,11001111b,00110111b,01100000b,01111100b,00101000b,00000000b
 db 10100100b,01101110b,11001111b,10110111b,01100000b,01111100b,00101101b,00000000b
 db 00000100b,01100000b,11001000b,00101101b,10000001b,01111110b,00010010b,00000000b
 db 00000100b,01100010b,11001000b,00101101b,10000001b,01111110b,00010010b,00000000b
 db 10000100b,01100001b,10111100b,00110011b,10000001b,01111100b,00000010b,00000000b
 db 10000100b,01100011b,10111100b,00110011b,10000001b,01111100b,00000010b,00000000b
 db 00000100b,01100000b,10101100b,00111001b,00010000b,01111100b,00000010b,00000000b
 db 00000100b,01100010b,10101100b,00111001b,00010000b,01111100b,00000010b,00000000b
 db 00000100b,01110010b,11101000b,00100101b,00010001b,11111110b,10010000b,00000000b
 db 00100110b,01001100b,11001001b,10100001b,01000000b,01111000b,00001010b,11000000b
 db 00100110b,01001110b,11001001b,10100001b,01000000b,01111000b,00001010b,11000000b
 db 00000100b,01000001b,11001011b,10100001b,01000000b,01111000b,00000010b,00000000b
 db 00100100b,01100001b,11011110b,00111101b,11100100b,01111000b,00000010b,00000000b
 db 00100100b,01100001b,11011100b,00111101b,11100100b,01111000b,00000010b,00000000b
 db 00100100b,01100001b,11011100b,00101101b,11100100b,01111000b,00000011b,00000000b
 db 10100100b,01100011b,11011100b,00101111b,11100100b,01111010b,00010011b,00000000b
 db 00010100b,01100001b,11101100b,00101101b,10000001b,01111111b,00000010b,00000000b
 db 00010100b,01100011b,11101100b,00101101b,10000001b,01111111b,00000010b,00000000b
 db 00000100b,01000001b,11001011b,10100001b,01000000b,01111000b,00000010b,00000000b
 db 10000100b,01000001b,11001011b,10100011b,01000000b,01111000b,00000111b,00000000b
 db 10000100b,01000011b,11001011b,10100011b,01000000b,01111000b,00000111b,00000000b
 db 00000100b,11100010b,10111100b,00100001b,00010000b,01111000b,00100010b,00000000b
 db 10100110b,01001100b,11001000b,00110011b,00000000b,01111000b,00001101b,11000000b
 db 10100110b,01001110b,11001000b,00110011b,00000000b,01111000b,00001101b,11000000b
 db 10010100b,01110000b,10001000b,01110011b,00001010b,01111100b,00000101b,00000000b
 db 10100110b,01001110b,11001000b,01111011b,00000000b,01111100b,00001101b,11000000b
 db 00000100b,11100000b,10101100b,00100001b,00000001b,01111000b,00000010b,00000000b
 db 00000100b,11100000b,10101100b,00100001b,00000001b,01111000b,00000010b,00000000b
 db 00000100b,11100010b,10101100b,00100001b,00000001b,01111000b,00000010b,00000000b
 db 10100100b,01110000b,11001111b,10110111b,01000001b,01111100b,10100101b,00000000b
 db 00000100b,11100001b,10111100b,00100001b,00010000b,01111000b,00100010b,00000000b
 db 00000100b,11100011b,10111100b,00100001b,00010000b,01111000b,00100010b,00000000b
 db 00000100b,01100000b,10101100b,00101101b,00000001b,01111110b,00000010b,00000000b
 db 00100110b,01001100b,11001010b,01110001b,00011000b,01111000b,00001011b,11000000b
 db 10100110b,01001110b,11001010b,01110011b,00011010b,01111000b,00001111b,11000000b
 db 00010100b,01100011b,11001010b,00111101b,10000000b,01111001b,00100010b,00000000b
 db 00000100b,01110010b,11001010b,00111101b,00000000b,11111000b,10100010b,00000000b
 db 00010100b,01110000b,11001000b,00100101b,00000001b,11111110b,10010000b,00000000b
 db 10110110b,01101110b,11011010b,01111111b,00100100b,01111001b,00001111b,01000000b
 db 00010100b,01110000b,11001001b,10101101b,01000001b,11111101b,10000010b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00010100b,01100001b,11001011b,11101101b,11000000b,01111101b,10000111b,00000000b
 db 00000100b,01100001b,10101100b,01110001b,10001000b,01111000b,00000101b,00000000b
 db 00000100b,01100011b,10101100b,01110001b,10001000b,01111000b,00000101b,00000000b
 db 00110100b,01101100b,11001111b,11110101b,01100110b,01111100b,00000101b,00000000b
 db 00000100b,01100000b,11001010b,01110001b,00011011b,01111110b,00100101b,00000000b
 db 00001100b,01110010b,10001010b,01110001b,00001010b,11111000b,00100101b,00000000b
 db 10101101b,01110000b,11001101b,11110111b,01110100b,11111100b,01000101b,00000000b
 db 10101101b,01110010b,11001111b,11110111b,01110110b,11111100b,01000101b,00000000b
 db 00100110b,01001100b,11001000b,00100101b,01010000b,01111000b,00001000b,11000000b
 db 00000100b,01100010b,11001000b,00100001b,00001001b,01111110b,00100101b,00000000b
 db 00000100b,01100000b,10011000b,01110001b,00011010b,01111000b,00000111b,00000000b
 db 10000101b,01100010b,11001000b,01110011b,00000010b,01111100b,00100101b,00000000b
 db 10010110b,01111100b,11001010b,00110111b,00000001b,11111100b,10101101b,00000000b
 db 10001101b,01110000b,11001000b,01110011b,00000001b,11111100b,00100101b,00000000b
 db 10001101b,01110010b,11001000b,01110011b,00000011b,11111100b,00100101b,00000000b
 db 00000100b,01100001b,10001000b,00100001b,10100100b,01111000b,01000001b,00000000b
 db 00000100b,01100011b,10001000b,01100001b,10101110b,01111000b,01000101b,00000000b
 db 00001110b,01001101b,11001000b,01110001b,10001010b,01111000b,00000101b,00000000b
 db 00001110b,01001111b,11001000b,01110001b,10001010b,01111000b,00000101b,00000000b
 db 00110110b,01101101b,11011000b,01111101b,10001010b,01111000b,00001101b,00000000b
 db 00100100b,01101110b,11001000b,00101001b,00100100b,01111100b,00001000b,11000000b
 db 00100100b,11101110b,11001000b,00101001b,00110100b,01111100b,00001000b,11000000b
 db 10100110b,01001100b,11001000b,00100001b,00000001b,01111100b,00010000b,00000000b
 db 00100110b,01101100b,11001000b,01100001b,00000001b,01111000b,00001000b,11000000b
 db 00001100b,01110000b,10001000b,00100001b,00001001b,11111110b,00110001b,00000000b
 db 00001100b,01110000b,10001110b,00100101b,00110100b,11111100b,11000001b,00000000b
 db 00001100b,01110010b,10001110b,00100101b,00110100b,11111100b,11000101b,00000000b
 db 00100110b,01001110b,11001000b,00100001b,00000000b,01111000b,00001000b,11000000b
 db 00001100b,01100000b,10001010b,01100001b,00001010b,01111000b,00000101b,00000000b
 db 00001100b,01100010b,10001010b,01100001b,00001010b,01111000b,00000101b,00000000b
 db 00000100b,01101110b,11001111b,10101101b,01000001b,01110100b,00000010b,00000000b
 db 00011100b,01100001b,11001111b,11101101b,11000000b,01111101b,00000010b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000101b,01100000b,11001000b,01100011b,00000000b,01111100b,00000101b,00000000b
 db 00000101b,01100000b,11001000b,01110011b,00001000b,01111100b,00000101b,00000000b
 db 00110110b,01001101b,11001000b,00101101b,10000000b,01111101b,00000010b,00000000b
 db 10000100b,01000001b,11001001b,10101011b,01000000b,01111100b,00000011b,00000000b
 db 10000100b,01100000b,10001010b,00101011b,00100100b,01111100b,00000001b,00000000b
 db 10100100b,01101110b,11001111b,11100111b,01001010b,01111000b,00001101b,00000000b
 db 10110110b,01101111b,11011111b,11101111b,01101110b,01111001b,10000111b,00000000b
 db 00011100b,01100010b,11001011b,10110001b,11001000b,01110000b,00100111b,00000000b
 db 00011100b,01100010b,11001110b,00110001b,10101100b,01110000b,00100111b,00000000b
 db 00111110b,01101110b,11001110b,00110001b,10001000b,01110000b,00101001b,11000000b
 db 00000100b,01110010b,11001000b,01110011b,00001000b,11111000b,00000001b,00000000b
 db 00000100b,01110010b,11001000b,01110011b,00001000b,11111000b,00000001b,00000000b
 db 11101100b,01111110b,11001011b,11110011b,01101111b,11111000b,00101101b,00000000b
 db 00111110b,01101111b,11111011b,11101101b,11001001b,01110011b,00011101b,11000000b
 db 00011100b,01100011b,11111101b,11101101b,11101101b,01110010b,01010111b,00000000b
 db 00110100b,01100011b,11111100b,00101101b,11001001b,01110001b,00100010b,00000000b
 db 10000100b,11100000b,10111110b,00110011b,00000011b,01111000b,00100111b,00000000b
 db 10000100b,11100000b,10111110b,00110011b,00000011b,01111000b,00100111b,00000000b
 db 10000100b,11100000b,10111110b,00110011b,00000011b,01111000b,00100111b,00000000b
 db 00000100b,01100000b,10001000b,00110011b,00100111b,01111000b,01100101b,00000000b
 db 10000101b,01100000b,10001000b,00110011b,00100111b,01111000b,00100101b,00000000b
 db 10001101b,01100010b,10001000b,01110011b,00100111b,01111000b,01100101b,00000000b
 db 00100110b,01001100b,11001010b,00110001b,00000010b,01111000b,00001101b,11000000b
 db 00100110b,01001100b,11001010b,01110001b,00000010b,01111000b,00001101b,11000000b
 db 10101110b,01001110b,11001010b,01110011b,00000010b,01111000b,00001101b,11000000b
 db 00001100b,01110000b,11001010b,00110101b,00000000b,11111100b,10000101b,00000000b
 db 00001100b,01110000b,11001010b,00110101b,00000000b,11111100b,10000101b,00000000b
 db 00100100b,01101100b,11001011b,10110101b,01000000b,01111100b,00101001b,00000000b
 db 00101100b,01101110b,11001011b,10110101b,01000000b,01111100b,00101001b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000000b,00110010b,10111100b,00101101b,00000001b,01111100b,00000010b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000100b,01100011b,10101100b,00101101b,00010001b,01111100b,00000010b,00000000b
 db 00100110b,11101100b,11101100b,00100001b,00000000b,01111000b,00001010b,00000000b
 db 00100110b,11101100b,11101100b,00100011b,00000000b,01111000b,00001111b,11000000b
 db 10100110b,11101110b,11101100b,00100011b,00000000b,01111000b,00001111b,11000000b
 db 00000100b,11100000b,10101100b,00110101b,00000000b,01111000b,00000011b,00000000b
 db 00000100b,11100010b,10101100b,00110101b,00000010b,11111100b,00100111b,00000000b
 db 00000100b,11100010b,10101100b,00110101b,00000011b,11111100b,00100111b,00000000b
 db 00000100b,01100000b,10101100b,00110101b,00001001b,01110000b,00100010b,00000000b
 db 00000100b,01100010b,10101100b,00110101b,00001001b,01111100b,00100010b,00000000b
 db 00000100b,01100010b,10101100b,00110101b,00001001b,01110100b,00100010b,00000000b
 db 00000100b,01100000b,11001000b,00100001b,00000001b,01111110b,00010000b,00000000b
 db 00000100b,01100010b,11001000b,00100001b,00000001b,01111110b,00010000b,00000000b
 db 00000100b,11100000b,10111100b,00100001b,00010000b,01111000b,10000010b,00000000b
 db 10000101b,11100010b,10111110b,00100011b,00010000b,01111000b,10100111b,00000000b
 db 10110100b,01100000b,11011000b,00111111b,01000010b,01111101b,10000111b,00000000b
 db 00100110b,01001100b,11001000b,00100001b,01000001b,01111110b,00000000b,00000000b
 db 00100110b,01001110b,11001000b,00100001b,01000001b,01111110b,00010000b,00000000b
 db 00100100b,01101100b,11101100b,00100101b,00000000b,01111100b,00000010b,00000000b
 db 00100100b,01101110b,11101100b,00100101b,00000001b,01111100b,00000010b,00000000b
 db 00101110b,01001100b,11001000b,01100001b,00000010b,01111000b,00001101b,11000000b
 db 00101110b,01001110b,11001000b,01100001b,00000010b,01111000b,00001101b,11000000b
 db 00110100b,01101100b,11011111b,10110101b,01000000b,01111000b,00000010b,00000000b
 db 00110100b,01101110b,11011111b,10110101b,01000000b,01111000b,00000111b,00000000b
 db 10100100b,01101100b,11001111b,10110111b,01100111b,01111101b,10100111b,00000000b
 db 00000100b,01101001b,11001100b,01111101b,10001010b,01110001b,00000010b,00000000b
 db 00000100b,01101011b,11001100b,01111101b,10001010b,01110001b,00000111b,00000000b
 db 00000100b,01110000b,10001001b,11100001b,01001010b,11111000b,00000101b,00000000b
 db 00000100b,01100000b,10001010b,00100001b,00110100b,01111000b,01000101b,00000000b
 db 10110100b,01110000b,11001000b,00111111b,01000011b,11111100b,10100011b,00000000b
 db 00100110b,01001100b,11001000b,01100001b,00001010b,01111000b,00001000b,11000000b
 db 00100110b,01001100b,11001000b,01100001b,00001010b,01111000b,00001101b,11000000b
 db 00100110b,01001100b,11011000b,00100001b,00000000b,01111000b,00001000b,11000000b
 db 00100110b,01011100b,11001000b,00110011b,00010011b,01111000b,00101101b,10000000b
 db 00100110b,01011110b,11001000b,00110011b,00010011b,01111000b,00101101b,11000000b
 db 00100110b,01001100b,11001000b,00100001b,00000000b,01111000b,00001000b,11000000b
 db 00100110b,01001110b,11011010b,00100001b,00000000b,01111000b,00001000b,11000000b
 db 00100110b,01011100b,11001000b,00100001b,00000000b,11111100b,00001000b,11000000b
 db 00101110b,01011110b,11001000b,01100001b,00000010b,11111100b,00001101b,11000000b
 db 00000100b,01100010b,10101100b,01110001b,00001010b,01111000b,00000101b,00000000b
 db 00000100b,01100010b,10101100b,01110001b,00001010b,01111010b,00010101b,00000000b
 db 01000100b,01100010b,10101110b,01110001b,00101110b,01111010b,00010101b,00000000b
 db 10000101b,01100000b,11001000b,01110111b,00000010b,01111000b,00001101b,00000000b
 db 10000101b,01100010b,11001000b,01110111b,00000010b,01111000b,00001101b,00000000b
 db 00001100b,01000000b,11001011b,10100001b,01000000b,01111100b,00000110b,00000000b
 db 00001100b,01000010b,11001011b,10100001b,01000000b,01111100b,00000110b,00000000b
 db 00000100b,01100000b,10001000b,01110001b,00101110b,01111000b,01000101b,00000000b
 db 00001100b,01100010b,10001000b,01110001b,00101110b,01111000b,01000101b,00000000b
 db 00100110b,01001100b,11001010b,01100001b,00000010b,01111000b,00001101b,11000000b
 db 00101110b,01001100b,11001010b,01100001b,00000010b,01111000b,00001101b,11000000b
 db 00101110b,01001110b,11001010b,01100001b,00000010b,01111000b,00001101b,11000000b
 db 10000100b,11100000b,10101100b,00100001b,00001000b,01111000b,00100010b,00000000b
 db 10000100b,11100010b,10101100b,00100001b,00001000b,01111000b,00100110b,00000000b
 db 00100110b,01001101b,11011000b,00100101b,00000000b,01111000b,00000010b,00000000b
 db 00100110b,01011111b,11011000b,01100101b,00000000b,11111000b,00000010b,00000000b
 db 00010100b,01001011b,11011000b,01101101b,10001010b,01110001b,00000010b,00000000b
 db 00010100b,01100011b,10011100b,01101101b,10101110b,01110001b,01000010b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00010100b,01110001b,11001010b,00101101b,11000000b,11111101b,10000010b,00000000b
 db 10010100b,01110011b,11001010b,00101101b,11000000b,11111101b,10000010b,00000000b
 db 00000100b,01000001b,11001011b,10100001b,01000000b,01111000b,00000010b,00000000b
 db 00000100b,01000001b,11001011b,10100001b,01000000b,01111000b,00000010b,00000000b
 db 10000100b,01111010b,11001100b,00100011b,00111110b,11111000b,10000101b,00000000b
 db 10010101b,01100001b,11001000b,00101111b,10000010b,01111000b,00000111b,00000000b
 db 10010101b,01100011b,11001000b,00101111b,10000010b,01111000b,00000111b,00000000b
 db 00000100b,01101000b,11001100b,00100001b,00000001b,01111110b,00010000b,00000000b
 db 01001100b,01101010b,11001110b,01100001b,00100101b,01111110b,00010001b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00010100b,01111100b,11001001b,10101101b,00000000b,11111101b,10000010b,00000000b
 db 10010100b,01111110b,11001001b,11101101b,00000010b,11111101b,10000111b,00000000b
 db 00000100b,11100000b,10101100b,00100101b,00010000b,01111000b,00100010b,00000000b
 db 10100101b,01101100b,11001101b,10100111b,01100101b,01111000b,00100101b,00000000b
 db 10101101b,01111100b,11001101b,11100111b,01100101b,01111000b,00100101b,00000000b
 db 10101101b,01111110b,11001101b,11100111b,01100101b,01111000b,00100101b,00000000b
 db 00100100b,11101000b,11101100b,00100101b,01010000b,01111000b,10000101b,00000000b
 db 00100100b,11101010b,11101100b,00100101b,01010000b,01111000b,10000101b,00000000b
 db 00001100b,11100010b,10111100b,01100001b,00000001b,01111010b,00110111b,00000000b
 db 00101100b,01101100b,11001100b,00100101b,01100100b,01111000b,00000000b,00000000b
 db 00101100b,01111100b,11001100b,01100111b,01100100b,11111000b,01000101b,00000000b
 db 00101100b,01111110b,11001100b,01100111b,01100100b,11111000b,01000101b,00000000b
 db 00100110b,01001100b,11001000b,00100001b,00000000b,01111000b,00001000b,11000000b
 db 00100110b,01001110b,11001000b,00100001b,00000010b,01111000b,10001000b,11000000b
 db 00100110b,01001110b,11011000b,00101101b,00000000b,01111000b,00001000b,11000000b
 db 00110110b,01111110b,11001011b,10100101b,01101101b,11111100b,10100111b,00000000b
 db 00010100b,01110000b,11001001b,10101101b,01000000b,11111101b,10000010b,00000000b
 db 00010100b,01110010b,11001001b,10101101b,01000000b,11111101b,10000010b,00000000b
 db 00000100b,01110000b,11101010b,01110001b,00110000b,01111100b,10000101b,00000000b
 db 10101100b,01111100b,11101111b,10110111b,01100101b,01111100b,00000101b,00000000b
 db 00110110b,01101110b,11011000b,01100001b,00001010b,01111000b,00001001b,11000000b
 db 00101100b,01100000b,11001010b,01110001b,01001011b,01111000b,00100101b,00000000b
 db 00101100b,01100000b,11001010b,01110001b,01001011b,01111000b,00100101b,00000000b
 db 11101100b,01111110b,11001111b,11110011b,01101111b,01111000b,00101101b,00000000b
 db 00100110b,01101100b,11001101b,10100101b,01101100b,01111100b,00000010b,00000000b
 db 10100100b,01100001b,11101101b,10100111b,01000001b,01111100b,00000010b,00000000b
 db 10100100b,01100001b,11101101b,10100111b,01000001b,01111100b,00000010b,00000000b
 db 00000100b,11100000b,10101100b,00100001b,00011000b,01111000b,00000000b,00000000b
 db 00000100b,11100010b,10101100b,01100001b,00011010b,01111000b,00000101b,00000000b
 db 00100100b,01100000b,10001000b,00110011b,00001011b,01111000b,00100001b,00000000b
 db 00100100b,01100010b,10001010b,01110011b,00001011b,01111000b,00100101b,00000000b
 db 00010100b,01110001b,11011001b,00101101b,11000000b,11111101b,10000010b,00000000b
 db 00010100b,01110001b,11011001b,00101101b,11000000b,11111101b,10000010b,00000000b
 db 00010100b,01110011b,11011001b,00101101b,11000000b,11111101b,10000010b,00000000b
 db 01001100b,01100000b,11001000b,00100011b,00100111b,01111000b,00100101b,00000000b
 db 01001100b,01100000b,11001000b,00100011b,00100111b,01111000b,00100101b,00000000b
 db 01001100b,01100010b,11001010b,01100011b,00100111b,01111010b,00110101b,00000000b
 db 00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b,00000000b
 db 00000100b,01100011b,11001000b,01101111b,10011011b,01110000b,00100111b,00000000b
 db 00000100b,01100011b,11001000b,01101111b,10011011b,01110000b,00100111b,00000000b
 db 10000100b,01100010b,10011001b,11110011b,01001010b,01110000b,00000101b,00000000b
 db 10000110b,01001110b,11011001b,11100011b,01000000b,01110000b,00000101b,00000000b
 db 10000100b,01100010b,11011001b,11100011b,01001011b,01110000b,00000101b,00000000b
 db 00111110b,01001110b,11011001b,11100011b,01000010b,01110000b,00001101b,11000000b
 db 01001101b,01100010b,10011101b,11110011b,01101111b,01110000b,01100101b,00000000b
 db 01101101b,01101110b,11001111b,11100011b,01101101b,01110000b,01111101b,11000000b
 db 01111100b,01101011b,11011101b,11101101b,11001001b,01111010b,00111010b,11000000b
 db 01111100b,01101011b,11011101b,11101101b,11001001b,01111010b,00111010b,11000000b
 db 00110100b,01100011b,11011001b,10101101b,11001001b,01111001b,00000010b,00000000b
 db 10110100b,01111011b,11011101b,10101111b,11000011b,11111001b,10100111b,00000000b
 db 00010100b,01110001b,11011000b,00101101b,11000000b,11111001b,10000010b,00000000b

; These list what move is in what TM/HM. Each structure is 64 records (128 bytes) long for nicer loading.
tms_rse:	; Advance are the only ones with move indexes greater than 255....
 ; 1-6
 db 00000001b,00001000b,00000001b,01010001b,00000001b,01100000b,00000001b,01011011b,00000000b,00101110b,00000000b,01011100b
 ;7-12
 db 00000001b,00000010b,00000001b,01010011b,00000001b,01001011b,00000000b,11101101b,00000000b,11110001b,00000001b,00001101b
 ;13-18
 db 00000000b,00111010b,00000000b,00111011b,00000000b,00111111b,00000000b,01110001b,00000000b,10110110b,00000000b,11110000b
 ;19-24
 db 00000000b,11001010b,00000000b,11011011b,00000000b,11011010b,00000000b,01001100b,00000000b,11100111b,00000000b,01010101b
 ;25-30
 db 00000000b,01010111b,00000000b,01011001b,00000000b,11011000b,00000000b,01011011b,00000000b,01011110b,00000000b,11110111b
 ;31-36
 db 00000001b,00011000b,00000000b,01101000b,00000000b,01110011b,00000001b,01011111b,00000000b,00110101b,00000000b,10111100b
 ;37-42
 db 00000000b,11001001b,00000000b,01111110b,00000001b,00111101b,00000001b,01001100b,00000001b,00000011b,00000001b,00000111b
 ;43-48
 db 00000001b,00100010b,00000000b,10011100b,00000000b,11010101b,00000000b,10101000b,00000000b,11010011b,00000001b,00011101b
 ;49-54
 db 00000001b,00100001b,00000001b,00111011b,00000000b,00001111b,00000000b,00010011b,00000000b,00111001b,00000000b,01000110b
 ;55-60
 db 00000000b,10010100b,00000000b,11111001b,00000000b,01111111b,00000001b,00100011b,        0,        0,        0,        0
 ;61-64 (4 two-byte entries)
 db 0,0,0,0,0,0,0,0
tms_gsc:
 ; 1-11
 db 0,11011111b,0,00011101b,0,10101110b,0,11001101b,0,00101110b,0,01011100b,0,11000000b,0,11111001b,0,11110100b,0,11101101b,0,11110001b
 ;12-22
 db 0,11100110b,0,10101101b,0,00111011b,0,00111111b,0,11000100b,0,10110110b,0,11110000b,0,11001010b,0,11001011b,0,11011010b,0,01001100b
 ;23-33
 db 0,11100111b,0,11100001b,0,01010111b,0,01011001b,0,11011000b,0,01011011b,0,01011110b,0,11110111b,0,10111101b,0,01101000b,0,00001000b
 ;34-44
 db 0,11001111b,0,11010110b,0,10111100b,0,11001001b,0,01111110b,0,10000001b,0,01101111b,0,00001001b,0,10001010b,0,11000101b,0,10011100b
 ;45-55
 db 0,11010101b,0,10101000b,0,11010011b,0,00000111b,0,11010010b,0,10101011b,0,00001111b,0,00010011b,0,00111001b,0,01000110b,0,10010100b
 ;56-64 (66-2)
 db 0,11111010b,0,01111111b,0,0,        0,0,        0,0,        0,0,        0,0,        0,0,        0,0
tms_rby:
 ;1-9
 db 0,00000101b,0,00001101b,0,00001110b,0,00010010b,0,00011001b,0,01011100b,0,00100000b,0,00100010b,0,00100100b
 ;10-18
 db 0,00100110b,0,00111101b,0,00110111b,0,00111010b,0,00111011b,0,00111111b,0,00000110b,0,01000010b,0,01000100b
 ;19-27
 db 0,01000101b,0,01100011b,0,01001000b,0,01001100b,0,01010010b,0,01010101b,0,01010111b,0,01011001b,0,01011010b
 ;28-36
 db 0,01011011b,0,01011110b,0,01100100b,0,01100110b,0,01101000b,0,01110011b,0,01110101b,0,01110110b,0,01111000b
 ;37-45
 db 0,01111001b,0,01111110b,0,10000001b,0,10000010b,0,10000111b,0,10001010b,0,10001111b,0,10011100b,0,01010110b
 ;48-54
 db 0,10010101b,0,10011001b,0,10011101b,0,10100001b,0,10100100b,0,00001111b,0,00010011b,0,00111001b,0,01000110b
 ;57-63
 db 0,10010100b,0,0,        0,0,        0,0,        0,0,        0,0,        0,0,        0,0,        0,0
 ; 64
 db 0,0


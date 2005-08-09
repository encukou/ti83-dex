 DEFINE P2SELECT, SPACE=ROM  
 SEGMENT P2SELECT

 extern _Green
 extern _DrawPkName
 extern _Red
 extern _Select
 public Select
 extern _Move
 extern _LoadPic
 extern _LPicBank1
 extern _DrawMoveName
 extern _DrawMoveName_AlignRight
 extern _MoveList
 extern _LoadLevelMoves
 extern _StatCalculator


 include "header.inc"
 include "linemacros.inc"

entered equ OP5  ; # of bytes entered so far
string equ OP5+1 ; bytes entered so far (extends to OP6)
now equ OP6+9 ; address of current entry

slaslaslasla macro   label
	     sla &label
	     sla &label
	     sla &label
	     sla &label
       .endm

srlsrlsrlsrl macro   label
	     srl &label
	     srl &label
	     srl &label
	     srl &label
       .endm


Select:
  ei					; enable rude interrupting
  xor a
  ld hl,entered	; len(entered)=0
  ld (hl),a
  ld DE,alpha_list
  ld hl,now			; Load default get current address
  ld (hl),D
  inc hl
  ld (hl),E
  call drawwelcomescreen
  AppOnErr select_keywait
  B_CALL RclX		; get X...
  B_CALL ConvOP1	; get X to DE
  AppOffErr
  LD HL,alpha_list
  bit showpkmn,(IY+dexstate)
  jp nz,getmask_x	  ; jump right to procedure for searching for TI# in DE
  inc DE	; value adjustment for moves
  jp getmask_m ; jump right to procedure for searching for move# in DE
 select_keywait:
  B_CALL RunIndicOff
  ld A,(entered)
  cp 2
  jp m,select_load_normalkeys
  ld A,(string)+1
  cp "["
  jr nz,select_load_normalkeys
  ld de,altkeys
  jr select_skip_loadkeys
 select_load_normalkeys:
  ld de,keys
 select_skip_loadkeys:
  halt				; save batteries
  B_CALL GetCSC	; Get keyboard scan code
  or a	; is a 0?
  jr z,select_keywait	; go again if no key
  ex hl,de
  ld d,0
  ld e,a		; put scan code to DE (d=0 from above)
  add hl,de	; get "my" code address
  ld a,(hl)	; get "my" code to a
  ; switch(my_code)
  cp 0						; case 0
  jr z,select_keywait	;  go to wait again
  cp "~"						; case BREAK
  jp z,exit_dex				;  return
  cp "<"						; case BKSP
  jr z,select_bksp
  cp "`"						; case CLEAR
  jr z,select_clear
  cp "-"						; case "?"
  jr z,select_try_help	;  go to help if it's the first char, otherwise go with the "-" that it also represents
  cp "d"						; case DOWN
  jr z,select_downkey
  cp "u"						; case UP
  jr z,select_upkey
  cp ">"						; case ENTER
  jr z,select_enter
  cp "["						; case THETA
  jr z,select_theta
  cp "#"						; case NUMBER_ENTRY
  jr z,select_numenter
  cp "*"						; case PUT_TO
  jr z,put_to
  cp 1		; case 1 (Green)
  jr z,select_green
  cp 2		; case 2 (Move)
  jr z,select_move
  cp 3		; case 3 (Select)
  jr z,select_keywait ; no need to do anything
  cp 4		; case 4 (Movelist)
  jr z,select_movelist
  cp 5		; case 5 (Help/Extra)
  jr z,waitkey_statcalc
  cp "^"						; case alpha,2nd
  jr z,select_keywait	;  go to wait again

 select_default:			; default
  push af

  cp "0"
  jr nz,select_skip_zero_test	; "0" can't be the first digit of a number
  ld A,(entered)
  cp 2
  jr nz,select_skip_zero_test
  ld A,(string)+1
  cp "["
  jr nz,select_skip_zero_test
  pop AF
  jr select_keywait
 select_skip_zero_test:
 select_test_numlen:				; Also, numbers are smaller (x0nnn)
  ld A,(string)+1
  cp "["
  jr nz,select_skip_num_tests
  ld A,(entered)
  cp 5
  jp nz,select_skip_num_tests
  pop AF
  jr select_keywait
 select_skip_num_tests:

  ld a,(entered)
  cp 12
  jp nz,select_default_continuemerrily	; if at limit, don't accept more
  pop AF
  jp select_keywait
 select_default_continuemerrily:
  B_CALL RunIndicOn
  inc a
  sla a	;*2
  sla a	;*4
  continuebuffer
  ld (penCol), a
  xor a
  ld (penRow), a
  pop af
  push af
  B_CALL VPutMap
  ld a,(entered)
  cp 11
  jp z,skipdrawcursor
  ld a,"_"
  B_CALL VPutMap
  endbuffer
 skipdrawcursor:
  ld hl,entered
  ld d,0
  ld e,(hl)
  ld hl,string
  add hl,de
  pop af
  ld (hl),a
  cp "["
  jr z,drawwelcomescreenif_theta_wasinput
  call updatelistposition
  jr dontdrawwelcomescreenotherwise
 drawwelcomescreenif_theta_wasinput:
  call resetwelcomescreen
  continuebuffer
  ld A,4
  ld (penCol),A
  ld A,14
  ld (penRow),A
  ld DE,OP1
  ld HL,number_input_text
  ld BC,12
  ldir
  ld HL,OP1
  ld B,12
  B_CALL VPutSN
  endbuffer
 dontdrawwelcomescreenotherwise:
  ld hl,entered		; get ready for next char by incrementing len(entered)
  inc (hl)
  jr select_keywait

 select_bksp:
  ld hl,entered
  ld a,(hl)
  or a
  jr z,select_keywait	; if((OP5)==0)break
  dec (hl)
  ld a,(hl)
  cp a,0
  jr z,select_clear	; clear everything if nothing's there
  inc a
  sla a	;*2
  sla a	;*4
  continuebuffer
  ld (penCol), a
  xor a
  ld (penRow), a
  ld a,"_"
  B_CALL VPutMap
  ld a,(entered)
  cp 11
  jp z,skipwrite4spaces
  ld a,SFourSpaces
  B_CALL VPutMap
  endbuffer
 skipwrite4spaces:

  call updatelistposition

  jr select_keywait
 select_clear:
  xor a
  ld (entered),a
  call resetwelcomescreen
  jr select_keywait
 select_try_help:
  ld a,(entered)
  cp 0
  jr z,select_help	; proceed if not the first character
  ld a,"-"
  jr select_default
 select_help:
  ld a,"?"
  jp select_default
 select_downkey:
  ld HL,now
  ld D,(HL)
  inc HL
  ld E,(HL)
  ex DE,HL	; get current address from (now)
  ld DE,alpha_list_last-15
  ld A,H
  cp D
  jp z,select_down_skipcp
  jp p,select_keywait
  jp m,select_forcedown
 select_down_skipcp:
  ld A,L
  cp E
  jp p,select_keywait
 select_forcedown:
  ld BC,16
  add HL,BC	; increment current address
  ex DE,HL	; save current address to (now)
  ld HL,now
  ld (HL),D
  inc HL
  ld (HL),E
  call drawcurrentlist
  jr select_keywait
 select_upkey:
  ld HL,now
  ld D,(HL)
  inc HL
  ld E,(HL)
  ex DE,HL	; get current address from (now)
  ld DE,alpha_list+15
  ld A,H
  cp D
  jp z,select_up_skipcp
  jp m,select_keywait
  jp p,select_forceup
 select_up_skipcp:
  ld A,L
  cp E
  jp m,select_keywait
 select_forceup:
  ld BC,-16
  add HL,BC	; increment current address
  ex DE,HL	; save current address to (now)
  ld HL,now
  ld (HL),D
  inc HL
  ld (HL),E
  call drawcurrentlist
  jr select_keywait
 select_enter:
  ld HL,now
  ld D,(HL)
  inc HL
  ld E,(HL)
  ex DE,HL	; get current address from (now)
  ld BC,14
  add HL,BC
  ld E,(HL)
  inc HL
  ld D,(HL)
  ex DE,HL	; get index to put to X
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  B_CALL StoX
  B_CALL StoTheta ; store to theta too
  ; see if it's a pk or move
  ld HL,now
  ld D,(HL)
  inc HL
  ld E,(HL)
  ex DE,HL	; get current address from (now)
  ld BC,13
  add HL,BC
  ld A,(HL)
  and 10h
  jp z,select_enter_go_to_move
  B_JUMP Green
 select_enter_go_to_move:
  B_JUMP Move
 select_theta:
  ld A,(entered)
  cp 1
  jr nz,select_keywait
  ld A,"["
  jr select_default
 select_numenter:
  B_CALL RunIndicOn
  ld A,(entered)
  cp 3							; x0n
  jp m,select_keywait
  B_CALL ZeroOP1
  ld A,(entered)
  sub 3
  or A,080h
  ld (OP1+1),A
  ld A,(entered)
  sub 2
  ld B,0
  ld C,A
  ld HL,string+2
  ld DE,OP1M
  ldir
  ld C,A
  ld HL,OP1M
 select_numenter_subtract30h_loop:
  ld A,(HL)
  sub '0'
  ld (HL),A
  inc HL
  dec C
  jr nz,select_numenter_subtract30h_loop
  ld A,(OP1M)
  slaslaslasla A
  ld H,A
  ld A,(OP1M+1)
  or A,H
  ld (OP1M),A
  ld A,(OP1M+2)
  slaslaslasla A
  ld (OP1M+1),A
  xor A
  ld (OP1M+2),A
  B_CALL ConvOP1	; Get # to DE

  ld HL,alpha_list;+14
  ; Now get the mask (to B), bit val (to C), flag byte offset (ultimately to E; to H for now)
  ;	and LSB offset (or that with L)
  ; DE must be preserved; or adjusted

  ld A,(string)
  cp "J"
  jp z,getmask_j
  cp "H"
  jp z,getmask_h
  cp "G"
  jp z,getmask_g
  cp "X"
  jp z,getmask_x
  cp "M"
  jp z,getmask_m
  ;cp "N"
  ;jp z,getmask_n
  ;[  --MASK--   Value         LSB byte offset		flag byte offset	Value adjustment
 getmask_n:
  ;N  00010010   10h + msb<<1  12					+1					none
  ld B,00010010b		; mask
  ld A,D
  sla A					; l bit val
  or 10h
  ld C,A
  ld A,12				; LSB offset
  or L
  ld L,A
  push HL
  ld H,1					; flag offset
  jr getmask_done
 getmask_j:
  ;J  00010100   10h + msb<<2  11					+2					none
  ld B,00010100b		; mask
  ld A,D
  sla A					; l bit val
  sla A
  or 10h
  ld C,A
  ld A,11				; LSB offset
  or L
  ld L,A
  push HL
  ld H,2					; flag offset
  jr getmask_done
 getmask_h:
  ;H  00011000   10h + msb<<3  10					+3					none
  ld B,00011000b		; mask
  ld A,D
  sla A					; l bit val
  sla A
  sla A
  or 10h
  ld C,A
  ld A,10				; LSB offset
  or L
  ld L,A
  push HL
  ld H,3					; flag offset
  jr getmask_done
 getmask_g:
  ;G  00010001   10h + msb<<0  15					-1					subtract 1; then subtract (276-252) more if val>255
  ld B,00010001b		; mask
  ld A,D					; l bit val
  or 10h
  ld C,A
  ld A,14				; LSB offset
  or L
  ld L,A
  push HL
  ld H,-1				; flag offset
  ; value adjustment:
  dec DE
  ld A,D
  or A
  jr z,getmask_done	; quit if val<=255
  ld A,E
  sub A,25
  ld E,A
  jr getmask_done
 getmask_x:
  ;X  00010001   10h + msb<<0  15					-1					none
  ld B,00010001b		; mask
  ld A,D					; l bit val
  or 10h
  ld C,A
  ld A,14				; LSB offset
  or L
  ld L,A
  push HL
  ld H,-1				; flag offset
  jr getmask_done
 getmask_m:
  ;M  00010001         msb<<0  15					-1					subtract 1
  dec DE	; value adjustment:
  ld B,00010001b		; mask
  ld C,D					; l bit val
  ld A,14				; LSB offset
  or L
  ld L,A
  push HL
  ld H,-1				; flag offset
  ;jr getmask_done
 getmask_done:

  ; The Find algorithm:
  ld A,E
  ld DE,alpha_list_last
  ld E,H
  pop HL
 loop:
  cp (HL)
  jp Z,first_check_ok
 advance_pointer:
  push AF
 advance_pointer__AF_already_saved:
  ld A,L
  add 16
  jp C,carry_pointer
  ld L,A
  pop AF
  jp loop
 carry_pointer:
  ld L,A
  ld A,H
  inc A
  cp D
  jp Z,nofail
  jp P,fail
  nofail:
  ld H,A
  pop AF
  jp loop
 fail:					; Failed to find
  pop AF
  jr select_keywait
 first_check_ok:
  push AF
  ld A,L
  add E
  ld L,A
  ld A,C
  xor (HL)
  and B
  jp Z,success
  ld A,L
  sub E
  ld L,A
  jp advance_pointer__AF_already_saved
 success:				; Success; cleanup
  pop AF
  ld A,L
  and 0F0h
  ld L,A
  ; Address to found entry is in HL



 ; ld A,1					; put first letter to string so the list'll show up
 ; ld (entered),A
 ; ld A,(HL)
 ; ld (string),A
  xor A
  ld (entered),A
  ex DE,HL				; save current address to (now)
  ld HL,now
  ld (HL),D
  inc HL
  ld (HL),E
  ex HL,DE

  continuebuffer

  ld a, 4
  ld (penCol), a
  xor a
  ld (penRow), a
 ; ld A,(HL)
 ; B_CALL VPutMap
  ld hl, appBackUpScreen+137
  ld b, 12
  B_CALL VPutSN		; Cursor & erase
  call _drawpkname
  endbuffer
  jr select_keywait
  

 ; THESE ARE UNREACHABLE:
  B_CALL GetKey            ;Wait for a keypress
  ret
exit_dex:
  B_JUMP JForceCmdNoChar

updatelistposition:
  ld A,(entered)
  cp 2
  jp m,updatelistposition_force
  ld A,(string)+1
  cp "["
  jr nz,updatelistposition_force
 ; call resetwelcomescreen
  ret
 updatelistposition_force:

  ld d,0	; for testing purposes, set DE to 0
  ld e,0

  xor a
  ld b,a
  ld c,a
 select_getchartocompareagainst:
  ld HL,string
  add HL,BC
  ld A,(HL)
 select_getdatapointer:
  push AF
  ld A,D
  slaslaslasla A
  ld L,E
  srlsrlsrlsrl L
  add L
  ld H,A
  ld L,E
  slaslaslasla L
  push BC
  ld BC,alpha_list
  add HL,BC	; add address
  pop BC
  add HL,BC	; add char@
  pop AF
 select_compare:
  cp (HL)
  jp z,select_HL_same
  jp m,select_HL_bigger
  ; else goto select_HL_smaller
 select_HL_smaller:
  inc DE
  push DE
  ld DE,16
  add HL,DE
  pop DE
  jp select_compare
 select_HL_same:
  push HL
  ld HL,entered
  ld A,(HL)
  pop HL
  cp C
  jp Z,select_end
  inc BC
  jp select_getchartocompareagainst
 select_HL_bigger:
  dec DE		; we went too far
 select_end:

  ; Now, data index is in DE.
  ; Make a data pointer from it:
  ld A,D
  slaslaslasla A
  ld L,E
  srlsrlsrlsrl L
  add L
  ld H,A
  ld L,E
  slaslaslasla L
  ld BC,alpha_list
  add HL,BC	; add address

  ex DE,HL	; save current address to (now)
  ld HL,now
  ld (HL),D
  inc HL
  ld (HL),E

  call drawcurrentlist

  ret

drawcurrentlist:
  continuebuffer
  ; Print 12 spaces
  ld HL,spaces
  ld de, OP2		; Put address of OP1 to DE
  ld bc, 12			; put 10 (length) to BC
  ldir				; load string to OP1
  ld a,14		; penRow
 spaces_loop:
  ld hl, OP2		; Put address of OP1 to HL
  ld b, 12			; put 12 (length) to B
  ld (penRow), a
  push af
  ld a, 4
  ld (penCol), a
  B_CALL VPutSN	; Draw string of fat spaces
  pop af
  add a,6	; advance penRow
  cp a,58	; check penRow
  jp m,spaces_loop

  ld HL,now
  ld D,(HL)
  inc HL
  ld E,(HL)
  ex DE,HL	; get current address from (now)

  ld a,14
 list_loop:
  push HL
  ; Print the string
  ld de, OP1		; Put address of OP1 to DE
  ld bc, 12			; put 10 (length) to BC
  ldir				; load string to OP1
  ld (penRow), a
  push AF
  ld a, 4
  ld (penCol), a
  inc HL
  ld b, 10			; put 10 (pkmn name length) to B
  ld A,(HL)
  and 10h
  jr nz,list_skip_move_incrementing
  inc B
  inc B				; put 12 (move name length) to B
 list_skip_move_incrementing:
  ld hl, OP1		; Put address of OP1 to HL
  B_CALL VPutSN	; Draw string
  pop AF
  pop HL
  ld BC,16
  add HL,BC
  add A,6
  cp A,58
  jp M,list_loop

  endbuffer

  ret


drawwelcomescreen:
  startbuffer

  ld h,1	; dark line
  ld bc, 2*256+62
  ld de, 2*256+57	; line left of inputbox
  B_CALL ILine
  ld bc, 3*256+56
  ld de,51*256+56	; line under inputbox
  B_CALL ILine
  ld bc,52*256+62
  ld de,52*256+57	; line left of inputbox
  B_CALL ILine
  ld bc, 0*256+62
  ld de, 2*256+62	; short top line
  B_CALL ILine
  ld bc,52*256+62
  ld de,93*256+62	; long top line
  B_CALL ILine
  ld bc,94*256+ 0
  ld de,94*256+61	; right line
  B_CALL ILine

  ld bc,70*256
  ld de,70*256+22	; left kana line
  B_CALL ILine
  ld bc,71*256+23
  ld de,93*256+23	; top kana line
  B_CALL ILine

  ; kana bitmap
  ld hl,p_kana						; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,p_kana_end-p_kana		; put length to BC
  ldir				; load image to RAM
  ld hl,appBackUpScreen			; HL points to copied image
  ld de,42*256+71					; DE <- coords
  B_CALL DisplayImage

 resetwelcomescreen:		; gets called as a function; skips the cls & line drawing
  continuebuffer

  ld hl,texts						; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,texts_total_len			; put length to BC
  ldir				; load image to RAM
  ld hl,appBackUpScreen			; HL points to copied texts
  xor a
  ld (penCol), a
  ld a, 8
  ld (penRow), a
  ld hl, appBackUpScreen
  ld b, 13
  B_CALL VPutSN		; INPUT NAME...
  ld a, 4
  ld (penCol), a
  ld a, 20
  ld (penRow), a
  ld hl, appBackUpScreen+27
  ld b, 14
  B_CALL VPutSN		; N#->NAT'L #
  ld a, 4
  ld (penCol), a
  ld a, 26
  ld (penRow), a
  ld hl, appBackUpScreen+41
  ld b, 13
  B_CALL VPutSN		; J#->JOHTO #
  ld a, 4
  ld (penCol), a
  ld a, 32
  ld (penRow), a
  ld hl, appBackUpScreen+55
  ld b, 12
  B_CALL VPutSN		; H#->HOENN #
  ld a, 4
  ld (penCol), a
  ld a, 38
  ld (penRow), a
  ld hl, appBackUpScreen+67
  ld b, 12
  B_CALL VPutSN		; #->GBA #
  ld a, 4
  ld (penCol), a
  ld a, 44
  ld (penRow), a
  ld hl, appBackUpScreen+79
  ld b, 13
  B_CALL VPutSN		; X#->TI #
  ld a, 4
  ld (penCol), a
  ld a, 50
  ld (penRow), a
  ld hl, appBackUpScreen+94
  ld b, 14
  B_CALL VPutSN		; M#->MOVE #
  ld a, 4
  ld (penCol), a
  ld a, 56
  ld (penRow), a
  ld hl, appBackUpScreen+108
  ld b, 15
  B_CALL VPutSN		; XT#n->SWAP...
  ld a, 67
  ld (penCol), a
  ld a, 14
  ld (penRow), a
  ld hl, appBackUpScreen+123
  ld b, 6
  B_CALL VPutSN		; ->ENTER
  ld a, 68
  ld (penCol), a
  ld a, 26
  ld (penRow), a
  ld hl, appBackUpScreen+129
  ld b, 4
  B_CALL VPutSN		; DEL->
  ld a, 76
  ld (penCol), a
  ld a, 32
  ld (penRow), a
  ld hl, appBackUpScreen+133
  ld b, 4
  B_CALL VPutSN		; BKSP

  xor a
  ld (penCol), a
  ld a, 14
  ld (penRow), a
  ld hl, appBackUpScreen+13
  ld b, 14
  B_CALL VPutSN		; arrow and spaces

 _drawpkname:		; gets called as a function; skips everything but input box clearing & pk name drawing
  continuebuffer

  ; draw pk name

  ld HL,now
  ld D,(HL)
  inc HL
  ld E,(HL)
  ex DE,HL	; get current address from (now)

  ld a,14
  ld (penRow), a
  ld a, 4
  ld (penCol), a
  ld de, appBackUpScreen	; Put address of OP1 to DE
  ld bc, 12			; put 10 (length) to BC
  ldir				; load string to OP1
  inc HL
  ld b, 10			; put 10 (pkmn name length) to B
  ld A,(HL)
  and 10h
  jr nz,welcome_skip_move_incrementing
  inc B
  inc B				; put 12 (move name length) to B
 welcome_skip_move_incrementing:
  ld hl, appBackUpScreen		; Put address of OP1 to HL
  B_CALL VPutSN	; Draw string

 clearinputbox:		; gets called as a function; skips everything but input box clearing
  endbuffer

  ld A,(entered)
  or a
  ret nz				; return if something is actually entered

  continuebuffer

  ld a, 4
  ld (penCol), a
  xor a
  ld (penRow), a
  ld hl, appBackUpScreen+137
  ld b, 12
  B_CALL VPutSN		; Cursor & erase

  endbuffer

  ret

put_to: ; not really a function
  ld hl,put_to_text				; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,28			; put length to BC
  ldir				; load image to RAM
  ld hl,appBackUpScreen			; HL points to copied texts
  ld a, 4
  ld (penCol), a
  ld a, 20
  ld (penRow), a
  ld hl, appBackUpScreen
  ld b, 16
  B_CALL VPutSN		; "PUT TO...?"
  ld a, 4
  ld (penCol), a
  ld a, 26
  ld (penRow), a
  ld hl, appBackUpScreen+10
  ld b, 12
  B_CALL VPutSN		; Spaces where J# was
  ld a, 4
  ld (penCol), a
  ld a, 32
  ld (penRow), a
  ld hl, appBackUpScreen+10
  ld b, 12
  B_CALL VPutSN		; Spaces where H# was
  ld a, 4
  ld (penCol), a
  ld a, 38
  ld (penRow), a
  ld hl, appBackUpScreen+10
  ld b, 12
  B_CALL VPutSN		; Spaces where G# was
  ld a, 4
  ld (penCol), a
  ld a, 44
  ld (penRow), a
  ld hl, appBackUpScreen+10
  ld b, 12
  B_CALL VPutSN		; Spaces where X# was
  ld a, 4
  ld (penCol), a
  ld a, 50
  ld (penRow), a
  ld hl, appBackUpScreen+10
  ld b, 12
  B_CALL VPutSN		; Spaces where M# was
  ld a, 4
  ld (penCol), a
  ld a, 56
  ld (penRow), a
  ld hl, appBackUpScreen+10
  ld b, 12
  B_CALL VPutSN		; Spaces where XT#n was
 put_to_wait:
  ld de,keys
  halt				; save batteries
  B_CALL GetCSC	; Get keyboard scan code
  or a	; is a 0?
  jr z,put_to_wait	; go again if no key
  ex hl,de
  ld d,0
  ld e,a		; put scan code to DE
  add hl,de	; get "my" code address
  ld a,(hl)	; get "my" code to a
  cp "^"	; did user press Alpha or 2nd?
  jr z,put_to_wait	;  then go to wait again
  ; switch(my_code)
  cp "A"
  jp m,select_clear
  cp "Z"+1
  jp p,select_clear

	push AF

  ld HL,now
  ld D,(HL)
  inc HL
  ld E,(HL)
  ex DE,HL	; get current address from (now)
  ld DE,14
  add HL,DE
  ld E,(HL)
  inc HL
  ld D,(HL)
  ex HL,DE
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  B_CALL PushRealO1 ; FPST = value
  ;
  B_CALL ZeroOP1
  pop AF
  push AF
  LD (OP1+1),A ; change OP1 to L3 name
  ;
  AppOnErr put_to_fail
  B_CALL StoOther ; store val -> var
  AppOffErr

  ld hl,stored				; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,12			; put length to BC
  ldir				; load image to RAM
  ld hl,appBackUpScreen			; HL points to copied texts
  ld a, 4
  ld (penCol), a
  ld a, 20
  ld (penRow), a
  ld hl, appBackUpScreen
  ld b, 9
  B_CALL VPutSN		; "STORED!"
  xor A
  ld (entered),A
  ld a, 15
  ld (penCol), a
  ld a, 35
  ld (penRow), a
  ld hl, appBackUpScreen+9
  ld b, 3
  B_CALL VPutSN		; "IN "
  pop AF
  B_CALL VPutMap		; var name
  call clearinputbox
  jr select_keywait
 put_to_fail:
  pop AF	; cleanup
  ld hl,failed				; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,16			; put length to BC
  ldir				; load image to RAM
  ld hl,appBackUpScreen			; HL points to copied texts
  ld a, 4
  ld (penCol), a
  ld a, 20
  ld (penRow), a
  ld hl, appBackUpScreen+7
  ld b, 9
  B_CALL VPutSN		; "ERROR!"
  ld a, 9
  ld (penCol), a
  ld a, 35
  ld (penRow), a
  ld hl, appBackUpScreen
  ld b, 7
  B_CALL VPutSN		; "FAILED!"
  jr select_keywait

select_green:
  B_JUMP Green
select_move:
  B_JUMP Move
select_movelist:
  B_JUMP MoveList
waitkey_statcalc:
  B_JUMP StatCalculator


stored:
 db "STORED!",[2]SFourSpaces
 db "IN",SFourSpaces
failed:
 db "FAILED!"
 db "ERROR!",[2]SFourSpaces	; also uses the SFourSpaces below; total length = 9
keys:
 db  SFourSpaces				; (not used; part of above message)
 db  "d",0,0,"u",0,0,0,0		; v<>^????
 db            ">\"WRMH`",0	; enter+-*/^clear-?
 db            "-[VQLG",0,0	; -369)tan-vars-?
 db           0,"ZUPKFC",0		; .258(cos-prgm-stat
 db SFourSpaces,"YTOJEB*"		; 0147,sin-apps-xt0n
 db            "~XSNIDA^"		; ?-store-ln-log-square-recip-math-alpha
 db          "\5\4\3\2\1^~"	; graph-trace-zoom-window-y=-2nd-mode
altkeys:
 db "<"							; del for Keys; also 0th (unused) byte for AltKeys
 db   0 ,0,0, 0 ,0,0,0,0		; v<>^????
 db           "#",[5]0,"`",0	; enter+-*/^clear-?
 db          0,"369",0,0,0,0	; -369)tan-vars-?
 db      	 0,"258",0,0,0,0	; .258(cos-prgm-stat
 db 			  "0147",0,0,0,0	; 0147,sin-apps-xt0n
 db      "~",0,0,0,0,0,0,0		; ?-store-ln-log-square-recip-math-alpha
 db       "\5\4\3\2\1",0,"~"	; graph-trace-zoom-window-y=-2nd-mode
 db "<"							; del for AltKeys

texts:
 db "INPUT",SFourSpaces,"NAME..."						; Start   0 Len 13
 db Sconvert, [13]SFourSpaces								; Start  13 Len 14
 db "N[",Sstore     ,"NAT'L #",[4]SFourSpaces		; Start  27 Len 14
 db "J[",Sstore     ,"JOHTO #",[4]SFourSpaces		; Start  41 Len 13
 db "H[",Sstore     ,"HOENN #",[2]SFourSpaces		; Start  55 Len 12
 db "G[",Sstore     ,"GBA #",[4]SFourSpaces			; Start  67 Len 12
 db "X[",Sstore     ,"TI #",[8]SFourSpaces			; Start  79 Len 13
 db "M[",Sstore     ,"MOVE #",[5]SFourSpaces			; Start  94 Len 14
 db "XT[n",Sstore     ,"PUT TO...",[1]SFourSpaces	; Start 108 Len 15
 db Sstore     ,"ENTER"										; Start 123 Len  6
 db "DEL",Sstore												; Start 129 Len  4
 db "BKSP"														; Start 133 Len  4
 db "_",[12]SFourSpaces										; Start 137 Len 12 (+1 that's not used in welcome screen)
 ; no new entries acceptable
texts_total_len equ 149

put_to_text:
 db "PUT TO...?",[12]SFourSpaces

number_input_text:
 db "NUMBER?",[5]SFourSpaces

 align 16
spaces:
 db [12]SFourSpaces	; 12 Fourspaces (move name len)
 db 0,0,0,0				; 16 (pad)
alpha_list:
						 db "ABRA\6\6\6\6\6\6"     ,027h,059h,03Fh,00010000b,03Eh,000h
						 db "ABSOL\6\6\6\6\6"      ,098h,067h,067h,00010111b,05Eh,001h
						 db "ABSORB\6\6\6\6\6\6"   ,             0,00000000b,046h,000h
						 db "ACID\6\6\6\6\6\6\6\6" ,             0,00000000b,032h,000h
						 db "ACID\6ARMOR\6\6"      ,             0,00000000b,096h,000h
						 db "AERIAL\6ACE\6\6"      ,             0,00000001b,04Bh,001h
						 db "AEROBLAST\6\6\6"      ,             0,00000000b,0B0h,000h
						 db "AERODACTYL"           ,029h,0E0h,08Eh,00011000b,08Dh,000h
						 db "AGGRON\6\6\6\6"       ,048h,032h,032h,00010111b,066h,001h
						 db "AGILITY\6\6\6\6\6"    ,             0,00000000b,060h,000h
						 db "AIPOM\6\6\6\6\6"      ,04Fh,07Ah,0BEh,00011000b,0BDh,000h
						 db "AIR\6CUTTER\6\6"      ,             0,00000001b,039h,001h
						 db "ALAKAZAM\6\6"         ,029h,05Bh,041h,00010000b,040h,000h
						 db "ALTARIA\6\6\6"        ,07Ah,04Eh,04Eh,00010111b,04Dh,001h
						 db "AMNESIA\6\6\6\6\6"    ,             0,00000000b,084h,000h
						 db "AMPHAROS\6\6"         ,049h,037h,0B5h,00011000b,0B4h,000h
						 db "ANCIENTPOWER"         ,             0,00000000b,0F5h,000h
						 db "ANORITH\6\6\6"        ,087h,05Bh,05Bh,00010111b,06Ch,001h
						 db "ARBOK\6\6\6\6\6"      ,0E2h,033h,018h,00010000b,017h,000h
						 db "ARCANINE\6\6"         ,0F6h,080h,03Bh,00010000b,03Ah,000h
						 db "ARIADOS\6\6\6"        ,043h,021h,0A8h,00011000b,0A7h,000h
						 db "ARM\6THRUST\6\6"      ,             0,00000001b,023h,001h
						 db "ARMALDO\6\6\6"        ,088h,05Ch,05Ch,00010111b,06Dh,001h
						 db "AROMATHERAPY"         ,             0,00000001b,037h,001h
						 db "ARON\6\6\6\6\6\6"     ,046h,030h,030h,00010111b,064h,001h
						 db "ARTICUNO\6\6"         ,02Bh,0EBh,090h,00011000b,08Fh,000h
						 db "ASSIST\6\6\6\6\6\6"   ,             0,00000001b,011h,001h
						 db "ASTONISH\6\6\6\6"     ,             0,00000001b,035h,001h
						 db "ATTRACT\6\6\6\6\6"    ,             0,00000000b,0D4h,000h
						 db "AURORA\6BEAM\6"       ,             0,00000000b,03Dh,000h
						 db "AZUMARILL\6"          ,038h,083h,0B8h,00010000b,0B7h,000h
						 db "AZURILL\6\6\6"        ,036h,02Ah,02Ah,00010111b,044h,001h
						 db "BAGON\6\6\6\6\6"      ,0BBh,073h,073h,00010111b,071h,001h
						 db "BALTOY\6\6\6\6"       ,083h,057h,057h,00010111b,024h,001h
						 db "BANETTE\6\6\6"        ,093h,062h,062h,00010111b,060h,001h
						 db "BARBOACH\6\6"         ,07Fh,053h,053h,00010111b,029h,001h
						 db "BARRAGE\6\6\6\6\6"    ,             0,00000000b,08Bh,000h
						 db "BARRIER\6\6\6\6\6"    ,             0,00000000b,06Fh,000h
						 db "BATON\6PASS\6\6"      ,             0,00000000b,0E1h,000h
						 db "BAYLEEF\6\6\6"        ,034h,002h,099h,00011000b,098h,000h
						 db "BEAT\6UP\6\6\6\6\6"   ,             0,00000000b,0FAh,000h
						 db "BEAUTIFLY\6"          ,010h,00Bh,00Bh,00010111b,00Ah,001h
						 db "BEEDRILL\6\6"         ,0D9h,01Dh,00Fh,00010000b,00Eh,000h
						 db "BELDUM\6\6\6\6"       ,0BEh,076h,076h,00010111b,074h,001h
						 db "BELLOSSOM\6"          ,05Bh,056h,0B6h,00010000b,0B5h,000h
						 db "BELLSPROUT"           ,0FAh,040h,045h,00010000b,044h,000h
						 db "BELLY\6DRUM\6\6"      ,             0,00000000b,0BAh,000h
						 db "BIDE\6\6\6\6\6\6\6\6" ,             0,00000000b,074h,000h
						 db "BIND\6\6\6\6\6\6\6\6" ,             0,00000000b,013h,000h
						 db "BITE\6\6\6\6\6\6\6\6" ,             0,00000000b,02Bh,000h
						 db "BLAST\6BURN\6\6"      ,             0,00000001b,032h,001h
						 db "BLASTOISE\6"          ,0D3h,0EAh,009h,00010000b,008h,000h
						 db "BLAZE\6KICK\6\6"      ,             0,00000001b,02Ah,001h
						 db "BLAZIKEN\6\6"         ,006h,001h,001h,00010111b,000h,001h
						 db "BLISSEY\6\6\6"        ,079h,0DAh,0F2h,00011000b,0F1h,000h
						 db "BLIZZARD\6\6\6\6"     ,             0,00000000b,03Ah,000h
						 db "BLOCK\6\6\6\6\6\6\6"  ,             0,00000001b,04Eh,001h
						 db "BODY\6SLAM\6\6\6"     ,             0,00000000b,021h,000h
						 db "BONE\6CLUB\6\6\6"     ,             0,00000000b,07Ch,000h
						 db "BONE\6RUSH\6\6\6"     ,             0,00000000b,0C5h,000h
						 db "BONEMERANG\6\6"       ,             0,00000000b,09Ah,000h
						 db "BOUNCE\6\6\6\6\6\6"   ,             0,00000001b,053h,001h
						 db "BRELOOM\6\6\6"        ,023h,01Eh,01Eh,00010111b,019h,001h
						 db "BRICK\6BREAK\6"       ,             0,00000001b,017h,001h
						 db "BUBBLE\6\6\6\6\6\6"   ,             0,00000000b,090h,000h
						 db "BUBBLEBEAM\6\6"       ,             0,00000000b,03Ch,000h
						 db "BULBASAUR\6"          ,0CBh,0E2h,001h,00010000b,000h,000h
						 db "BULK\6UP\6\6\6\6\6"   ,             0,00000001b,052h,001h
						 db "BULLET\6SEED\6"       ,             0,00000001b,04Ah,001h
						 db "BUTTERFREE"           ,0D6h,01Ah,00Ch,00010000b,00Bh,000h
						 db "CACNEA\6\6\6\6"       ,077h,04Bh,04Bh,00010111b,03Eh,001h
						 db "CACTURNE\6\6"         ,078h,04Ch,04Ch,00010111b,03Fh,001h
						 db "CALM\6MIND\6\6\6"     ,             0,00000001b,05Ah,001h
						 db "CAMERUPT\6\6"         ,066h,043h,043h,00010111b,03Ah,001h
						 db "CAMOUFLAGE\6\6"       ,             0,00000001b,024h,001h
						 db "CARVANHA\6\6"         ,061h,03Eh,03Eh,00010111b,030h,001h
						 db "CASCOON\6\6\6"        ,011h,00Ch,00Ch,00010111b,00Bh,001h
						 db "CASTFORM\6\6"         ,08Eh,05Fh,05Fh,00010111b,067h,001h
						 db "CATERPIE\6\6"         ,0D4h,018h,00Ah,00010000b,009h,000h
						 db "CELEBI\6\6\6\6"       ,082h,0FBh,0FBh,00011000b,0FAh,000h
						 db "CHANSEY\6\6\6"        ,015h,0D9h,071h,00011000b,070h,000h
						 db "CHARGE\6\6\6\6\6\6"   ,             0,00000001b,00Bh,001h
						 db "CHARIZARD\6"          ,0D0h,0E7h,006h,00010000b,005h,000h
						 db "CHARM\6\6\6\6\6\6\6"  ,             0,00000000b,0CBh,000h
						 db "CHARMANDER"           ,0CEh,0E5h,004h,00010000b,003h,000h
						 db "CHARMELEON"           ,0CFh,0E6h,005h,00010000b,004h,000h
						 db "CHIKORITA\6"          ,033h,001h,098h,00011000b,097h,000h
						 db "CHIMECHO\6\6"         ,097h,066h,066h,00010111b,081h,001h
						 db "CHINCHOU\6\6"         ,0B5h,0AEh,0AAh,00010000b,0A9h,000h
						 db "CLAMP\6\6\6\6\6\6\6"  ,             0,00000000b,07Fh,000h
						 db "CLAMPERL\6\6"         ,0B0h,06Eh,06Eh,00010111b,05Bh,001h
						 db "CLAYDOL\6\6\6"        ,084h,058h,058h,00010111b,025h,001h
						 db "CLEFABLE\6\6"         ,0EAh,02Ah,024h,00010000b,023h,000h
						 db "CLEFAIRY\6\6"         ,0E9h,029h,023h,00010000b,022h,000h
						 db "CLEFFA\6\6\6\6"       ,044h,028h,0ADh,00011000b,0ACh,000h
						 db "CLOYSTER\6\6"         ,005h,0AAh,05Bh,00011000b,05Ah,000h
						 db "COMBUSKEN\6"          ,005h,000h,000h,00010110b,0FFh,000h
						 db "COMET\6PUNCH\6"       ,             0,00000000b,003h,000h
						 db "CONFUSE\6RAY\6"       ,             0,00000000b,06Ch,000h
						 db "CONFUSION\6\6\6"      ,             0,00000000b,05Ch,000h
						 db "CONSTRICT\6\6\6"      ,             0,00000000b,083h,000h
						 db "CONVERSION\6\6"       ,             0,00000000b,09Fh,000h
						 db "CONVERSION\6Z"        ,             0,00000000b,0AFh,000h
						 db "CORPHISH\6\6"         ,081h,055h,055h,00010111b,02Ch,001h
						 db "CORSOLA\6\6\6"        ,0B4h,0ABh,0DEh,00010000b,0DDh,000h
						 db "COSMIC\6POWER"        ,             0,00000001b,041h,001h
						 db "COTTON\6SPORE"        ,             0,00000000b,0B1h,000h
						 db "COUNTER\6\6\6\6\6"    ,             0,00000000b,043h,000h
						 db "COVET\6\6\6\6\6\6\6"  ,             0,00000001b,056h,001h
						 db "CRABHAMMER\6\6"       ,             0,00000000b,097h,000h
						 db "CRADILY\6\6\6"        ,086h,05Ah,05Ah,00010111b,06Bh,001h
						 db "CRAWDAUNT\6"          ,082h,056h,056h,00010111b,02Dh,001h
						 db "CROBAT\6\6\6\6"       ,041h,027h,0A9h,00010000b,0A8h,000h
						 db "CROCONAW\6\6"         ,03Ah,008h,09Fh,00011000b,09Eh,000h
						 db "CROSS\6CHOP\6\6"      ,             0,00000000b,0EDh,000h
						 db "CRUNCH\6\6\6\6\6\6"   ,             0,00000000b,0F1h,000h
						 db "CRUSH\6CLAW\6\6"      ,             0,00000001b,031h,001h
						 db "CUBONE\6\6\6\6"       ,010h,0CBh,068h,00011000b,067h,000h
						 db "CURSE\6\6\6\6\6\6\6"  ,             0,00000000b,0ADh,000h
						 db "CUT\6\6\6\6\6\6\6\6\6",             0,00000000b,00Eh,000h
						 db "CYNDAQUIL\6"          ,036h,004h,09Bh,00011000b,09Ah,000h
						 db "DEFENSE\6CURL"        ,             0,00000000b,06Eh,000h
						 db "DELCATTY\6\6"         ,03Eh,02Dh,02Dh,00010111b,022h,001h
						 db "DELIBIRD\6\6"         ,06Ch,0BEh,0E1h,00011000b,0E0h,000h
						 db "DEOXYS\6\6\6\6"       ,0CAh,082h,082h,00010111b,080h,001h
						 db "DESTINY\6BOND"        ,             0,00000000b,0C1h,000h
						 db "DETECT\6\6\6\6\6\6"   ,             0,00000000b,0C4h,000h
						 db "DEWGONG\6\6\6"        ,003h,0B1h,057h,00011000b,056h,000h
						 db "DIG\6\6\6\6\6\6\6\6\6",             0,00000000b,05Ah,000h
						 db "DIGLETT\6\6\6"        ,0EFh,084h,032h,00010000b,031h,000h
						 db "DISABLE\6\6\6\6\6"    ,             0,00000000b,031h,000h
						 db "DITTO\6\6\6\6\6"      ,01Fh,05Ch,084h,00011000b,083h,000h
						 db "DIVE\6\6\6\6\6\6\6\6" ,             0,00000001b,022h,001h
						 db "DIZZY\6PUNCH\6"       ,             0,00000000b,091h,000h
						 db "DODRIO\6\6\6\6"       ,05Dh,0C8h,055h,00010000b,054h,000h
						 db "DODUO\6\6\6\6\6"      ,05Ch,0C7h,054h,00010000b,053h,000h
						 db "DONPHAN\6\6\6"        ,0A6h,0C4h,0E8h,00010000b,0E7h,000h
						 db "DOOM\6DESIRE\6"       ,             0,00000001b,060h,001h
						 db "DOUBLE-EDGE\6"        ,             0,00000000b,025h,000h
						 db "DOUBLE\6KICK\6"       ,             0,00000000b,017h,000h
						 db "DOUBLE\6TEAM\6"       ,             0,00000000b,067h,000h
						 db "DOUBLESLAP\6\6"       ,             0,00000000b,002h,000h
						 db "DRAGON\6CLAW\6"       ,             0,00000001b,050h,001h
						 db "DRAGON\6DANCE"        ,             0,00000001b,05Ch,001h
						 db "DRAGON\6RAGE\6"       ,             0,00000000b,051h,000h
						 db "DRAGONAIR\6"          ,02Fh,0F2h,094h,00011000b,093h,000h
						 db "DRAGONBREATH"         ,             0,00000000b,0E0h,000h
						 db "DRAGONITE\6"          ,030h,0F3h,095h,00011000b,094h,000h
						 db "DRATINI\6\6\6"        ,02Eh,0F1h,093h,00011000b,092h,000h
						 db "DREAM\6EATER\6"       ,             0,00000000b,089h,000h
						 db "DRILL\6PECK\6\6"      ,             0,00000000b,040h,000h
						 db "DROWZEE\6\6\6"        ,00Ah,057h,060h,00011000b,05Fh,000h
						 db "DUGTRIO\6\6\6"        ,0F0h,085h,033h,00010000b,032h,000h
						 db "DUNSPARCE\6"          ,05Dh,034h,0CEh,00011000b,0CDh,000h
						 db "DUSCLOPS\6\6"         ,095h,064h,064h,00010111b,050h,001h
						 db "DUSKULL\6\6\6"        ,094h,063h,063h,00010111b,04Fh,001h
						 db "DUSTOX\6\6\6\6"       ,012h,00Dh,00Dh,00010111b,00Ch,001h
						 db "DYNAMICPUNCH"         ,             0,00000000b,0DEh,000h
						 db "EARTHQUAKE\6\6"       ,             0,00000000b,058h,000h
						 db "EEVEE\6\6\6\6\6"      ,020h,0B4h,085h,00011000b,084h,000h
						 db "EGG\6BOMB\6\6\6\6"    ,             0,00000000b,078h,000h
						 db "EKANS\6\6\6\6\6"      ,0E1h,032h,017h,00010000b,016h,000h
						 db "ELECTABUZZ"           ,01Bh,09Bh,07Dh,00011000b,07Ch,000h
						 db "ELECTRIKE\6"          ,04Eh,035h,035h,00010111b,037h,001h
						 db "ELECTRODE\6"          ,055h,079h,065h,00010000b,064h,000h
						 db "ELEKID\6\6\6\6"       ,076h,09Ah,0EFh,00011000b,0EEh,000h
						 db "EMBER\6\6\6\6\6\6\6"  ,             0,00000000b,033h,000h
						 db "ENCORE\6\6\6\6\6\6"   ,             0,00000000b,0E2h,000h
						 db "ENDEAVOR\6\6\6\6"     ,             0,00000001b,01Ah,001h
						 db "ENDURE\6\6\6\6\6\6"   ,             0,00000000b,0CAh,000h
						 db "ENTEI\6\6\6\6\6"      ,07Bh,0EFh,0F4h,00011000b,0F3h,000h
						 db "ERUPTION\6\6\6\6"     ,             0,00000001b,01Bh,001h
						 db "ESPEON\6\6\6\6"       ,055h,0B8h,0C4h,00011000b,0C3h,000h
						 db "EXEGGCUTE\6"          ,00Eh,068h,066h,00011000b,065h,000h
						 db "EXEGGUTOR\6"          ,00Fh,069h,067h,00011000b,066h,000h
						 db "EXPLOSION\6\6\6"      ,             0,00000000b,098h,000h
						 db "EXPLOUD\6\6\6"        ,02Fh,027h,027h,00010111b,05Ah,001h
						 db "EXTRASENSORY"         ,             0,00000001b,045h,001h
						 db "EXTREMESPEED"         ,             0,00000000b,0F4h,000h
						 db "FACADE\6\6\6\6\6\6"   ,             0,00000001b,006h,001h
						 db "FAINT\6ATTACK"        ,             0,00000000b,0B8h,000h
						 db "FAKE\6OUT\6\6\6\6"    ,             0,00000000b,0FBh,000h
						 db "FAKE\6TEARS\6\6"      ,             0,00000001b,038h,001h
						 db "FALSE\6SWIPE\6"       ,             0,00000000b,0CDh,000h
						 db "FARFETCH\6D"          ,001h,09Eh,053h,00011000b,052h,000h
						 db "FEAROW\6\6\6\6"       ,0E0h,00Eh,016h,00010000b,015h,000h
						 db "FEATHERDANCE"         ,             0,00000001b,028h,001h
						 db "FEEBAS\6\6\6\6"       ,08Ch,05Dh,05Dh,00010111b,02Eh,001h
						 db "FERALIGATR"           ,03Bh,009h,0A0h,00011000b,09Fh,000h
						 db "FIRE\6BLAST\6\6"      ,             0,00000000b,07Dh,000h
						 db "FIRE\6PUNCH\6\6"      ,             0,00000000b,006h,000h
						 db "FIRE\6SPIN\6\6\6"     ,             0,00000000b,052h,000h
						 db "FISSURE\6\6\6\6\6"    ,             0,00000000b,059h,000h
						 db "FLAAFFY\6\6\6"        ,048h,036h,0B4h,00011000b,0B3h,000h
						 db "FLAIL\6\6\6\6\6\6\6"  ,             0,00000000b,0AEh,000h
						 db "FLAME\6WHEEL\6"       ,             0,00000000b,0ABh,000h
						 db "FLAMETHROWER"         ,             0,00000000b,034h,000h
						 db "FLAREON\6\6\6"        ,023h,0B7h,088h,00011000b,087h,000h
						 db "FLASH\6\6\6\6\6\6\6"  ,             0,00000000b,093h,000h
						 db "FLATTER\6\6\6\6\6"    ,             0,00000001b,003h,001h
						 db "FLY\6\6\6\6\6\6\6\6\6",             0,00000000b,012h,000h
						 db "FLYGON\6\6\6\6"       ,076h,04Ah,04Ah,00010111b,034h,001h
						 db "FOCUS\6ENERGY"        ,             0,00000000b,073h,000h
						 db "FOCUS\6PUNCH\6"       ,             0,00000001b,007h,001h
						 db "FOLLOW\6ME\6\6\6"     ,             0,00000001b,009h,001h
						 db "FORESIGHT\6\6\6"      ,             0,00000000b,0C0h,000h
						 db "FORRETRESS"           ,05Ch,05Eh,0CDh,00011000b,0CCh,000h
						 db "FRENZY\6PLANT"        ,             0,00000001b,051h,001h
						 db "FRUSTRATION\6"        ,             0,00000000b,0D9h,000h
						 db "FURRET\6\6\6\6"       ,03Dh,014h,0A2h,00011000b,0A1h,000h
						 db "FURY\6ATTACK\6"       ,             0,00000000b,01Eh,000h
						 db "FURY\6CUTTER\6"       ,             0,00000000b,0D1h,000h
						 db "FURY\6SWIPES\6"       ,             0,00000000b,099h,000h
						 db "FUTURE\6SIGHT"        ,             0,00000000b,0F7h,000h
						 db "GARDEVOIR\6"          ,01Fh,01Ah,01Ah,00010111b,070h,001h
						 db "GASTLY\6\6\6\6"       ,006h,03Ah,05Ch,00011000b,05Bh,000h
						 db "GENGAR\6\6\6\6"       ,008h,03Ch,05Eh,00011000b,05Dh,000h
						 db "GEODUDE\6\6\6"        ,039h,022h,04Ah,00010000b,049h,000h
						 db "GIGA\6DRAIN\6\6"      ,             0,00000000b,0C9h,000h
						 db "GIRAFARIG\6"          ,0A4h,093h,0CBh,00010000b,0CAh,000h
						 db "GLALIE\6\6\6\6"       ,0ACh,06Ah,06Ah,00010111b,041h,001h
						 db "GLARE\6\6\6\6\6\6\6"  ,             0,00000000b,088h,000h
						 db "GLIGAR\6\6\6\6"       ,05Eh,0BDh,0CFh,00011000b,0CEh,000h
						 db "GLOOM\6\6\6\6\6"      ,059h,054h,02Ch,00010000b,02Bh,000h
						 db "GOLBAT\6\6\6\6"       ,040h,026h,02Ah,00010000b,029h,000h
						 db "GOLDEEN\6\6\6"        ,032h,04Eh,076h,00010000b,075h,000h
						 db "GOLDUCK\6\6\6"        ,09Fh,08Bh,037h,00010000b,036h,000h
						 db "GOLEM\6\6\6\6\6"      ,03Bh,024h,04Ch,00010000b,04Bh,000h
						 db "GOREBYSS\6\6"         ,0B2h,070h,070h,00010111b,05Dh,001h
						 db "GRANBULL\6\6"         ,061h,07Ch,0D2h,00011000b,0D1h,000h
						 db "GRASSWHISTLE"         ,             0,00000001b,03Fh,001h
						 db "GRAVELER\6\6"         ,03Ah,023h,04Bh,00010000b,04Ah,000h
						 db "GRIMER\6\6\6\6"       ,06Ah,074h,058h,00010000b,057h,000h
						 db "GROUDON\6\6\6"        ,0C7h,07Fh,07Fh,00010111b,07Bh,001h
						 db "GROVYLE\6\6\6"        ,002h,0FDh,0FDh,00010000b,0FCh,000h
						 db "GROWL\6\6\6\6\6\6\6"  ,             0,00000000b,02Ch,000h
						 db "GROWLITHE\6"          ,0F5h,07Fh,03Ah,00010000b,039h,000h
						 db "GROWTH\6\6\6\6\6\6"   ,             0,00000000b,049h,000h
						 db "GRUDGE\6\6\6\6\6\6"   ,             0,00000001b,01Fh,001h
						 db "GRUMPIG\6\6\6"        ,06Fh,046h,046h,00010111b,046h,001h
						 db "GUILLOTINE\6\6"       ,             0,00000000b,00Bh,000h
						 db "GULPIN\6\6\6\6"       ,05Fh,03Ch,03Ch,00010111b,055h,001h
						 db "GUST\6\6\6\6\6\6\6\6" ,             0,00000000b,00Fh,000h
						 db "GYARADOS\6\6"         ,035h,04Dh,082h,00010000b,081h,000h
						 db "HAIL\6\6\6\6\6\6\6\6" ,             0,00000001b,001h,001h
						 db "HARDEN\6\6\6\6\6\6"   ,             0,00000000b,069h,000h
						 db "HARIYAMA\6\6"         ,031h,029h,029h,00010111b,036h,001h
						 db "HAUNTER\6\6\6"        ,007h,03Bh,05Dh,00011000b,05Ch,000h
						 db "HAZE\6\6\6\6\6\6\6\6" ,             0,00000000b,071h,000h
						 db "HEADBUTT\6\6\6\6"     ,             0,00000000b,01Ch,000h
						 db "HEAL\6BELL\6\6\6"     ,             0,00000000b,0D6h,000h
						 db "HEAT\6WAVE\6\6\6"     ,             0,00000001b,000h,001h
						 db "HELPING\6HAND"        ,             0,00000001b,00Dh,001h
						 db "HERACROSS\6"          ,0A8h,071h,0D6h,00010000b,0D5h,000h
						 db "HI\6JUMP\6KICK"       ,             0,00000000b,087h,000h
						 db "HIDDEN\6POWER"        ,             0,00000000b,0ECh,000h
						 db "HITMONCHAN"           ,013h,091h,06Bh,00011000b,06Ah,000h
						 db "HITMONLEE\6"          ,012h,090h,06Ah,00011000b,069h,000h
						 db "HITMONTOP\6"          ,074h,092h,0EDh,00011000b,0ECh,000h
						 db "HO-OH\6\6\6\6\6"      ,081h,0F8h,0FAh,00011000b,0F9h,000h
						 db "HOOTHOOT\6\6"         ,03Eh,00Fh,0A3h,00011000b,0A2h,000h
						 db "HOPPIP\6\6\6\6"       ,04Ch,043h,0BBh,00011000b,0BAh,000h
						 db "HORN\6ATTACK\6"       ,             0,00000000b,01Dh,000h
						 db "HORN\6DRILL\6\6"      ,             0,00000000b,01Fh,000h
						 db "HORSEA\6\6\6\6"       ,0B8h,0BAh,074h,00010000b,073h,000h
						 db "HOUNDOOM\6\6"         ,06Fh,0D2h,0E5h,00011000b,0E4h,000h
						 db "HOUNDOUR\6\6"         ,06Eh,0D1h,0E4h,00011000b,0E3h,000h
						 db "HOWL\6\6\6\6\6\6\6\6" ,             0,00000001b,04Fh,001h
						 db "HUNTAIL\6\6\6"        ,0B1h,06Fh,06Fh,00010111b,05Ch,001h
						 db "HYDRO\6CANNON"        ,             0,00000001b,033h,001h
						 db "HYDRO\6PUMP\6\6"      ,             0,00000000b,037h,000h
						 db "HYPER\6BEAM\6\6"      ,             0,00000000b,03Eh,000h
						 db "HYPER\6FANG\6\6"      ,             0,00000000b,09Dh,000h
						 db "HYPER\6VOICE\6"       ,             0,00000001b,02Fh,001h
						 db "HYPNO\6\6\6\6\6"      ,00Bh,058h,061h,00011000b,060h,000h
						 db "HYPNOSIS\6\6\6\6"     ,             0,00000000b,05Eh,000h
						 db "ICE\6BALL\6\6\6\6"    ,             0,00000001b,02Ch,001h
						 db "ICE\6BEAM\6\6\6\6"    ,             0,00000000b,039h,000h
						 db "ICE\6PUNCH\6\6\6"     ,             0,00000000b,007h,000h
						 db "ICICLE\6SPEAR"        ,             0,00000001b,04Ch,001h
						 db "ICY\6WIND\6\6\6\6"    ,             0,00000000b,0C3h,000h
						 db "IGGLYBUFF\6"          ,089h,02Bh,0AEh,00010000b,0ADh,000h
						 db "ILLUMISE\6\6"         ,057h,03Ah,03Ah,00010111b,069h,001h
						 db "IMPRISON\6\6\6\6"     ,             0,00000001b,01Dh,001h
						 db "INGRAIN\6\6\6\6\6"    ,             0,00000001b,012h,001h
						 db "IRON\6DEFENSE"        ,             0,00000001b,04Dh,001h
						 db "IRON\6TAIL\6\6\6"     ,             0,00000000b,0E6h,000h
						 db "IVYSAUR\6\6\6"        ,0CCh,0E3h,002h,00010000b,001h,000h
						 db "JIGGLYPUFF"           ,08Ah,02Ch,027h,00010000b,026h,000h
						 db "JIRACHI\6\6\6"        ,0C9h,081h,081h,00010111b,07Fh,001h
						 db "JOLTEON\6\6\6"        ,022h,0B6h,087h,00011000b,086h,000h
						 db "JUMP\6KICK\6\6\6"     ,             0,00000000b,019h,000h
						 db "JUMPLUFF\6\6"         ,04Eh,045h,0BDh,00011000b,0BCh,000h
						 db "JYNX\6\6\6\6\6\6"     ,01Ah,099h,07Ch,00011000b,07Bh,000h
						 db "KABUTO\6\6\6\6"       ,027h,0DEh,08Ch,00011000b,08Bh,000h
						 db "KABUTOPS\6\6"         ,028h,0DFh,08Dh,00011000b,08Ch,000h
						 db "KADABRA\6\6\6"        ,028h,05Ah,040h,00010000b,03Fh,000h
						 db "KAKUNA\6\6\6\6"       ,0D8h,01Ch,00Eh,00010000b,00Dh,000h
						 db "KANGASKHAN"           ,017h,0CDh,073h,00011000b,072h,000h
						 db "KARATE\6CHOP\6"       ,             0,00000000b,001h,000h
						 db "KECLEON\6\6\6"        ,091h,060h,060h,00010111b,023h,001h
						 db "KINESIS\6\6\6\6\6"    ,             0,00000000b,085h,000h
						 db "KINGDRA\6\6\6"        ,0BAh,0BCh,0E6h,00010000b,0E5h,000h
						 db "KINGLER\6\6\6"        ,00Dh,0A5h,063h,00011000b,062h,000h
						 db "KIRLIA\6\6\6\6"       ,01Eh,019h,019h,00010111b,06Fh,001h
						 db "KNOCK\6OFF\6\6\6"     ,             0,00000001b,019h,001h
						 db "KOFFING\6\6\6"        ,06Ch,072h,06Dh,00010000b,06Ch,000h
						 db "KRABBY\6\6\6\6"       ,00Ch,0A4h,062h,00011000b,061h,000h
						 db "KYOGRE\6\6\6\6"       ,0C6h,07Eh,07Eh,00010111b,07Ah,001h
						 db "LAIRON\6\6\6\6"       ,047h,031h,031h,00010111b,065h,001h
						 db "LANTURN\6\6\6"        ,0B6h,0AFh,0ABh,00010000b,0AAh,000h
						 db "LAPRAS\6\6\6\6"       ,01Eh,0DBh,083h,00011000b,082h,000h
						 db "LARVITAR\6\6"         ,07Dh,0F4h,0F6h,00011000b,0F5h,000h
						 db "LATIAS\6\6\6\6"       ,0C4h,07Ch,07Ch,00010111b,07Dh,001h
						 db "LATIOS\6\6\6\6"       ,0C5h,07Dh,07Dh,00010111b,07Eh,001h
						 db "LEAF\6BLADE\6\6"      ,             0,00000001b,05Bh,001h
						 db "LEDIAN\6\6\6\6"       ,041h,01Fh,0A6h,00011000b,0A5h,000h
						 db "LEDYBA\6\6\6\6"       ,040h,01Eh,0A5h,00011000b,0A4h,000h
						 db "LEECH\6LIFE\6\6"      ,             0,00000000b,08Ch,000h
						 db "LEECH\6SEED\6\6"      ,             0,00000000b,048h,000h
						 db "LEER\6\6\6\6\6\6\6\6" ,             0,00000000b,02Ah,000h
						 db "LICK\6\6\6\6\6\6\6\6" ,             0,00000000b,079h,000h
						 db "LICKITUNG\6"          ,014h,0B2h,06Ch,00011000b,06Bh,000h
						 db "LIGHT\6SCREEN"        ,             0,00000000b,070h,000h
						 db "LILEEP\6\6\6\6"       ,085h,059h,059h,00010111b,06Ah,001h
						 db "LINOONE\6\6\6"        ,00Dh,008h,008h,00010111b,007h,001h
						 db "LOCK-ON\6\6\6\6\6"    ,             0,00000000b,0C6h,000h
						 db "LOMBRE\6\6\6\6"       ,014h,00Fh,00Fh,00010111b,00Eh,001h
						 db "LOTAD\6\6\6\6\6"      ,013h,00Eh,00Eh,00010111b,00Dh,001h
						 db "LOUDRED\6\6\6"        ,02Eh,026h,026h,00010111b,059h,001h
						 db "LOVELY\6KISS\6"       ,             0,00000000b,08Dh,000h
						 db "LOW\6KICK\6\6\6\6"    ,             0,00000000b,042h,000h
						 db "LUDICOLO\6\6"         ,015h,010h,010h,00010111b,00Fh,001h
						 db "LUGIA\6\6\6\6\6"      ,080h,0F7h,0F9h,00011000b,0F8h,000h
						 db "LUNATONE\6\6"         ,07Dh,051h,051h,00010111b,042h,001h
						 db "LUSTER\6PURGE"        ,             0,00000001b,026h,001h
						 db "LUVDISC\6\6\6"        ,0B7h,072h,072h,00010111b,02Bh,001h
						 db "MACH\6PUNCH\6\6"      ,             0,00000000b,0B6h,000h
						 db "MACHAMP\6\6\6"        ,04Bh,08Eh,044h,00010000b,043h,000h
						 db "MACHOKE\6\6\6"        ,04Ah,08Dh,043h,00010000b,042h,000h
						 db "MACHOP\6\6\6\6"       ,049h,08Ch,042h,00010000b,041h,000h
						 db "MAGBY\6\6\6\6\6"      ,077h,096h,0F0h,00011000b,0EFh,000h
						 db "MAGCARGO\6\6"         ,068h,0D4h,0DBh,00010000b,0DAh,000h
						 db "MAGIC\6COAT\6\6"      ,             0,00000001b,014h,001h
						 db "MAGICAL\6LEAF"        ,             0,00000001b,058h,001h
						 db "MAGIKARP\6\6"         ,034h,04Ch,081h,00010000b,080h,000h
						 db "MAGMAR\6\6\6\6"       ,01Ch,097h,07Eh,00011000b,07Dh,000h
						 db "MAGNEMITE\6"          ,052h,076h,051h,00010000b,050h,000h
						 db "MAGNETON\6\6"         ,053h,077h,052h,00010000b,051h,000h
						 db "MAGNITUDE\6\6\6"      ,             0,00000000b,0DDh,000h
						 db "MAKUHITA\6\6"         ,030h,028h,028h,00010111b,035h,001h
						 db "MANECTRIC\6"          ,04Fh,036h,036h,00010111b,038h,001h
						 db "MANKEY\6\6\6\6"       ,0F3h,086h,038h,00010000b,037h,000h
						 db "MANTINE\6\6\6"        ,06Dh,0C5h,0E2h,00011000b,0E1h,000h
						 db "MAREEP\6\6\6\6"       ,047h,035h,0B3h,00011000b,0B2h,000h
						 db "MARILL\6\6\6\6"       ,037h,082h,0B7h,00010000b,0B6h,000h
						 db "MAROWAK\6\6\6"        ,011h,0CCh,069h,00011000b,068h,000h
						 db "MARSHTOMP\6"          ,008h,003h,003h,00010111b,002h,001h
						 db "MASQUERAIN"           ,021h,01Ch,01Ch,00010111b,01Eh,001h
						 db "MAWILE\6\6\6\6"       ,045h,02Fh,02Fh,00010111b,049h,001h
						 db "MEAN\6LOOK\6\6\6"     ,             0,00000000b,0D3h,000h
						 db "MEDICHAM\6\6"         ,04Dh,034h,034h,00010111b,04Bh,001h
						 db "MEDITATE\6\6\6\6"     ,             0,00000000b,05Fh,000h
						 db "MEDITITE\6\6"         ,04Ch,033h,033h,00010111b,04Ah,001h
						 db "MEGA\6DRAIN\6\6"      ,             0,00000000b,047h,000h
						 db "MEGA\6KICK\6\6\6"     ,             0,00000000b,018h,000h
						 db "MEGA\6PUNCH\6\6"      ,             0,00000000b,004h,000h
						 db "MEGAHORN\6\6\6\6"     ,             0,00000000b,0DFh,000h
						 db "MEGANIUM\6\6"         ,035h,003h,09Ah,00011000b,099h,000h
						 db "MEMENTO\6\6\6\6\6"    ,             0,00000001b,005h,001h
						 db "MEOWTH\6\6\6\6"       ,0F1h,088h,034h,00010000b,033h,000h
						 db "METAGROSS\6"          ,0C0h,078h,078h,00010111b,076h,001h
						 db "METAL\6CLAW\6\6"      ,             0,00000000b,0E7h,000h
						 db "METAL\6SOUND\6"       ,             0,00000001b,03Eh,001h
						 db "METANG\6\6\6\6"       ,0BFh,077h,077h,00010111b,075h,001h
						 db "METAPOD\6\6\6"        ,0D5h,019h,00Bh,00010000b,00Ah,000h
						 db "METEOR\6MASH\6"       ,             0,00000001b,034h,001h
						 db "METRONOME\6\6\6"      ,             0,00000000b,075h,000h
						 db "MEW\6\6\6\6\6\6\6"    ,032h,0FAh,097h,00011000b,096h,000h
						 db "MEWTWO\6\6\6\6"       ,031h,0F9h,096h,00011000b,095h,000h
						 db "MIGHTYENA\6"          ,00Bh,006h,006h,00010111b,005h,001h
						 db "MILK\6DRINK\6\6"      ,             0,00000000b,0CFh,000h
						 db "MILOTIC\6\6\6"        ,08Dh,05Eh,05Eh,00010111b,02Fh,001h
						 db "MILTANK\6\6\6"        ,078h,095h,0F1h,00011000b,0F0h,000h
						 db "MIMIC\6\6\6\6\6\6\6"  ,             0,00000000b,065h,000h
						 db "MIND\6READER\6"       ,             0,00000000b,0A9h,000h
						 db "MINIMIZE\6\6\6\6"     ,             0,00000000b,06Ah,000h
						 db "MINUN\6\6\6\6\6"      ,051h,038h,038h,00010111b,048h,001h
						 db "MIRROR\6COAT\6"       ,             0,00000000b,0F2h,000h
						 db "MIRROR\6MOVE\6"       ,             0,00000000b,076h,000h
						 db "MISDREAVUS"           ,059h,0D6h,0C8h,00011000b,0C7h,000h
						 db "MIST\6\6\6\6\6\6\6\6" ,             0,00000000b,035h,000h
						 db "MIST\6BALL\6\6\6"     ,             0,00000001b,027h,001h
						 db "MOLTRES\6\6\6"        ,02Dh,0EDh,092h,00011000b,091h,000h
						 db "MOONLIGHT\6\6\6"      ,             0,00000000b,0EBh,000h
						 db "MORNING\6SUN\6"       ,             0,00000000b,0E9h,000h
						 db "MR\6MIME\6\6\6"       ,018h,09Ch,07Ah,00011000b,079h,000h
						 db "MUD-SLAP\6\6\6\6"     ,             0,00000000b,0BCh,000h
						 db "MUD\6SHOT\6\6\6\6"    ,             0,00000001b,054h,001h
						 db "MUD\6SPORT\6\6\6"     ,             0,00000001b,02Bh,001h
						 db "MUDDY\6WATER\6"       ,             0,00000001b,049h,001h
						 db "MUDKIP\6\6\6\6"       ,007h,002h,002h,00010111b,001h,001h
						 db "MUK\6\6\6\6\6\6\6"    ,06Bh,075h,059h,00010000b,058h,000h
						 db "MURKROW\6\6\6"        ,057h,0D0h,0C6h,00011000b,0C5h,000h
						 db "NATU\6\6\6\6\6\6"     ,0A2h,09Fh,0B1h,00010000b,0B0h,000h
						 db "NATURE\6POWER"        ,             0,00000001b,00Ah,001h
						 db "NEEDLE\6ARM\6\6"      ,             0,00000001b,02Dh,001h
						 db "NIDOKING\6\6"         ,0E8h,064h,022h,00010000b,021h,000h
						 db "NIDOQUEEN\6"          ,0E5h,061h,01Fh,00010000b,01Eh,000h
						 db "NIDORAN\6F\6"         ,0E6h,062h,020h,00010000b,01Fh,000h
						 db "NIDORAN\6M\6"         ,0E3h,05Fh,01Dh,00010000b,01Ch,000h
						 db "NIDORINA\6\6"         ,0E4h,060h,01Eh,00010000b,01Dh,000h
						 db "NIDORINO\6\6"         ,0E7h,063h,021h,00010000b,020h,000h
						 db "NIGHT\6SHADE\6"       ,             0,00000000b,064h,000h
						 db "NIGHTMARE\6\6\6"      ,             0,00000000b,0AAh,000h
						 db "NINCADA\6\6\6"        ,02Ah,022h,022h,00010111b,013h,001h
						 db "NINETALES\6"          ,09Ah,07Eh,026h,00010000b,025h,000h
						 db "NINJASK\6\6\6"        ,02Bh,023h,023h,00010111b,014h,001h
						 db "NOCTOWL\6\6\6"        ,03Fh,010h,0A4h,00011000b,0A3h,000h
						 db "NOSEPASS\6\6"         ,03Ch,02Bh,02Bh,00010111b,026h,001h
						 db "NUMEL\6\6\6\6\6"      ,065h,042h,042h,00010111b,039h,001h
						 db "NUZLEAF\6\6\6"        ,017h,012h,012h,00010111b,011h,001h
						 db "OCTAZOOKA\6\6\6"      ,             0,00000000b,0BDh,000h
						 db "OCTILLERY\6"          ,06Bh,0ADh,0E0h,00011000b,0DFh,000h
						 db "ODDISH\6\6\6\6"       ,058h,053h,02Bh,00010000b,02Ah,000h
						 db "ODOR\6SLEUTH\6"       ,             0,00000001b,03Bh,001h
						 db "OMANYTE\6\6\6"        ,025h,0DCh,08Ah,00011000b,089h,000h
						 db "OMASTAR\6\6\6"        ,026h,0DDh,08Bh,00011000b,08Ah,000h
						 db "ONIX\6\6\6\6\6\6"     ,009h,03Eh,05Fh,00011000b,05Eh,000h
						 db "OUTRAGE\6\6\6\6\6"    ,             0,00000000b,0C7h,000h
						 db "OVERHEAT\6\6\6\6"     ,             0,00000001b,03Ah,001h
						 db "PAIN\6SPLIT\6\6"      ,             0,00000000b,0DBh,000h
						 db "PARAS\6\6\6\6\6"      ,0EBh,046h,02Eh,00010000b,02Dh,000h
						 db "PARASECT\6\6"         ,0ECh,047h,02Fh,00010000b,02Eh,000h
						 db "PAY\6DAY\6\6\6\6\6"   ,             0,00000000b,005h,000h
						 db "PECK\6\6\6\6\6\6\6\6" ,             0,00000000b,03Fh,000h
						 db "PELIPPER\6\6"         ,01Ch,017h,017h,00010111b,01Ch,001h
						 db "PERISH\6SONG\6"       ,             0,00000000b,0C2h,000h
						 db "PERSIAN\6\6\6"        ,0F2h,089h,035h,00010000b,034h,000h
						 db "PETAL\6DANCE\6"       ,             0,00000000b,04Fh,000h
						 db "PHANPY\6\6\6\6"       ,0A5h,0C3h,0E7h,00010000b,0E6h,000h
						 db "PICHU\6\6\6\6\6"      ,09Bh,015h,0ACh,00010000b,0ABh,000h
						 db "PIDGEOT\6\6\6"        ,0DCh,00Ch,012h,00010000b,011h,000h
						 db "PIDGEOTTO\6"          ,0DBh,00Bh,011h,00010000b,010h,000h
						 db "PIDGEY\6\6\6\6"       ,0DAh,00Ah,010h,00010000b,00Fh,000h
						 db "PIKACHU\6\6\6"        ,09Ch,016h,019h,00010000b,018h,000h
						 db "PILOSWINE\6"          ,069h,0C0h,0DDh,00011000b,0DCh,000h
						 db "PIN\6MISSILE\6"       ,             0,00000000b,029h,000h
						 db "PINECO\6\6\6\6"       ,05Bh,05Dh,0CCh,00011000b,0CBh,000h
						 db "PINSIR\6\6\6\6"       ,0A7h,070h,07Fh,00010000b,07Eh,000h
						 db "PLUSLE\6\6\6\6"       ,050h,037h,037h,00010111b,047h,001h
						 db "POISON\6FANG\6"       ,             0,00000001b,030h,001h
						 db "POISON\6GAS\6\6"      ,             0,00000000b,08Ah,000h
						 db "POISON\6STING"        ,             0,00000000b,027h,000h
						 db "POISON\6TAIL\6"       ,             0,00000001b,055h,001h
						 db "POISONPOWDER"         ,             0,00000000b,04Ch,000h
						 db "POLITOED\6\6"         ,04Bh,04Bh,0BAh,00011000b,0B9h,000h
						 db "POLIWAG\6\6\6"        ,0F7h,048h,03Ch,00010000b,03Bh,000h
						 db "POLIWHIRL\6"          ,0F8h,049h,03Dh,00010000b,03Ch,000h
						 db "POLIWRATH\6"          ,0F9h,04Ah,03Eh,00010000b,03Dh,000h
						 db "PONYTA\6\6\6\6"       ,0FDh,0C9h,04Dh,00010000b,04Ch,000h
						 db "POOCHYENA\6"          ,00Ah,005h,005h,00010111b,004h,001h
						 db "PORYGON\6\6\6"        ,024h,0D7h,089h,00011000b,088h,000h
						 db "PORYGONZ\6\6"         ,070h,0D8h,0E9h,00011000b,0E8h,000h
						 db "POUND\6\6\6\6\6\6\6"  ,             0,00000000b,000h,000h
						 db "POWDER\6SNOW\6"       ,             0,00000000b,0B4h,000h
						 db "PRESENT\6\6\6\6\6"    ,             0,00000000b,0D8h,000h
						 db "PRIMEAPE\6\6"         ,0F4h,087h,039h,00010000b,038h,000h
						 db "PROTECT\6\6\6\6\6"    ,             0,00000000b,0B5h,000h
						 db "PSYBEAM\6\6\6\6\6"    ,             0,00000000b,03Bh,000h
						 db "PSYCH\6UP\6\6\6\6"    ,             0,00000000b,0F3h,000h
						 db "PSYCHIC\6\6\6\6\6"    ,             0,00000000b,05Dh,000h
						 db "PSYCHO\6BOOST"        ,             0,00000001b,061h,001h
						 db "PSYDUCK\6\6\6"        ,09Eh,08Ah,036h,00010000b,035h,000h
						 db "PSYWAVE\6\6\6\6\6"    ,             0,00000000b,094h,000h
						 db "PUPITAR\6\6\6"        ,07Eh,0F5h,0F7h,00011000b,0F6h,000h
						 db "PURSUIT\6\6\6\6\6"    ,             0,00000000b,0E3h,000h
						 db "QUAGSIRE\6\6"         ,054h,039h,0C3h,00011000b,0C2h,000h
						 db "QUICK\6ATTACK"        ,             0,00000000b,061h,000h
						 db "QUILAVA\6\6\6"        ,037h,005h,09Ch,00011000b,09Bh,000h
						 db "QWILFISH\6\6"         ,062h,0A1h,0D3h,00011000b,0D2h,000h
						 db "RAGE\6\6\6\6\6\6\6\6" ,             0,00000000b,062h,000h
						 db "RAICHU\6\6\6\6"       ,09Dh,017h,01Ah,00010000b,019h,000h
						 db "RAIKOU\6\6\6\6"       ,07Ah,0EEh,0F3h,00011000b,0F2h,000h
						 db "RAIN\6DANCE\6\6"      ,             0,00000000b,0EFh,000h
						 db "RALTS\6\6\6\6\6"      ,01Dh,018h,018h,00010111b,06Eh,001h
						 db "RAPID\6SPIN\6\6"      ,             0,00000000b,0E4h,000h
						 db "RAPIDASH\6\6"         ,0FEh,0CAh,04Eh,00010000b,04Dh,000h
						 db "RATICATE\6\6"         ,0DEh,012h,014h,00010000b,013h,000h
						 db "RATTATA\6\6\6"        ,0DDh,011h,013h,00010000b,012h,000h
						 db "RAYQUAZA\6\6"         ,0C8h,080h,080h,00010111b,07Ch,001h
						 db "RAZOR\6LEAF\6\6"      ,             0,00000000b,04Ah,000h
						 db "RAZOR\6WIND\6\6"      ,             0,00000000b,00Ch,000h
						 db "RECOVER\6\6\6\6\6"    ,             0,00000000b,068h,000h
						 db "RECYCLE\6\6\6\6\6"    ,             0,00000001b,015h,001h
						 db "REFLECT\6\6\6\6\6"    ,             0,00000000b,072h,000h
						 db "REFRESH\6\6\6\6\6"    ,             0,00000001b,01Eh,001h
						 db "REGICE\6\6\6\6"       ,0C2h,07Ah,07Ah,00010111b,078h,001h
						 db "REGIROCK\6\6"         ,0C1h,079h,079h,00010111b,077h,001h
						 db "REGISTEEL\6"          ,0C3h,07Bh,07Bh,00010111b,079h,001h
						 db "RELICANTH\6"          ,0B3h,071h,071h,00010111b,063h,001h
						 db "REMORAID\6\6"         ,06Ah,0ACh,0DFh,00011000b,0DEh,000h
						 db "REST\6\6\6\6\6\6\6\6" ,             0,00000000b,09Bh,000h
						 db "RETURN\6\6\6\6\6\6"   ,             0,00000000b,0D7h,000h
						 db "REVENGE\6\6\6\6\6"    ,             0,00000001b,016h,001h
						 db "REVERSAL\6\6\6\6"     ,             0,00000000b,0B2h,000h
						 db "RHYDON\6\6\6\6"       ,0AAh,0CFh,070h,00010000b,06Fh,000h
						 db "RHYHORN\6\6\6"        ,0A9h,0CEh,06Fh,00010000b,06Eh,000h
						 db "ROAR\6\6\6\6\6\6\6\6" ,             0,00000000b,02Dh,000h
						 db "ROCK\6BLAST\6\6"      ,             0,00000001b,05Dh,001h
						 db "ROCK\6SLIDE\6\6"      ,             0,00000000b,09Ch,000h
						 db "ROCK\6SMASH\6\6"      ,             0,00000000b,0F8h,000h
						 db "ROCK\6THROW\6\6"      ,             0,00000000b,057h,000h
						 db "ROCK\6TOMB\6\6\6"     ,             0,00000001b,03Ch,001h
						 db "ROLE\6PLAY\6\6\6"     ,             0,00000001b,00Fh,001h
						 db "ROLLING\6KICK"        ,             0,00000000b,01Ah,000h
						 db "ROLLOUT\6\6\6\6\6"    ,             0,00000000b,0CCh,000h
						 db "ROSELIA\6\6\6"        ,05Eh,03Bh,03Bh,00010111b,051h,001h
						 db "SABLEYE\6\6\6"        ,044h,02Eh,02Eh,00010111b,028h,001h
						 db "SACRED\6FIRE\6"       ,             0,00000000b,0DCh,000h
						 db "SAFEGUARD\6\6\6"      ,             0,00000000b,0DAh,000h
						 db "SALAMENCE\6"          ,0BDh,075h,075h,00010111b,073h,001h
						 db "SAND-ATTACK\6"        ,             0,00000000b,01Bh,000h
						 db "SAND\6TOMB\6\6\6"     ,             0,00000001b,047h,001h
						 db "SANDSHREW\6"          ,070h,030h,01Bh,00010000b,01Ah,000h
						 db "SANDSLASH\6"          ,071h,031h,01Ch,00010000b,01Bh,000h
						 db "SANDSTORM\6\6\6"      ,             0,00000000b,0C8h,000h
						 db "SCARY\6FACE\6\6"      ,             0,00000000b,0B7h,000h
						 db "SCEPTILE\6\6"         ,003h,0FEh,0FEh,00010000b,0FDh,000h
						 db "SCIZOR\6\6\6\6"       ,063h,06Fh,0D4h,00011000b,0D3h,000h
						 db "SCRATCH\6\6\6\6\6"    ,             0,00000000b,009h,000h
						 db "SCREECH\6\6\6\6\6"    ,             0,00000000b,066h,000h
						 db "SCYTHER\6\6\6"        ,019h,06Eh,07Bh,00011000b,07Ah,000h
						 db "SEADRA\6\6\6\6"       ,0B9h,0BBh,075h,00010000b,074h,000h
						 db "SEAKING\6\6\6"        ,033h,04Fh,077h,00010000b,076h,000h
						 db "SEALEO\6\6\6\6"       ,0AEh,06Ch,06Ch,00010111b,03Ch,001h
						 db "SECRET\6POWER"        ,             0,00000001b,021h,001h
						 db "SEEDOT\6\6\6\6"       ,016h,011h,011h,00010111b,010h,001h
						 db "SEEL\6\6\6\6\6\6"     ,002h,0B0h,056h,00011000b,055h,000h
						 db "SEISMIC\6TOSS"        ,             0,00000000b,044h,000h
						 db "SELFDESTRUCT"         ,             0,00000000b,077h,000h
						 db "SENTRET\6\6\6"        ,03Ch,013h,0A1h,00011000b,0A0h,000h
						 db "SEVIPER\6\6\6"        ,07Ch,050h,050h,00010111b,061h,001h
						 db "SHADOW\6BALL\6"       ,             0,00000000b,0F6h,000h
						 db "SHADOW\6PUNCH"        ,             0,00000001b,044h,001h
						 db "SHARPEDO\6\6"         ,062h,03Fh,03Fh,00010111b,031h,001h
						 db "SHARPEN\6\6\6\6\6"    ,             0,00000000b,09Eh,000h
						 db "SHEDINJA\6\6"         ,02Ch,024h,024h,00010111b,015h,001h
						 db "SHEER\6COLD\6\6"      ,             0,00000001b,048h,001h
						 db "SHELGON\6\6\6"        ,0BCh,074h,074h,00010111b,072h,001h
						 db "SHELLDER\6\6"         ,004h,0A9h,05Ah,00011000b,059h,000h
						 db "SHIFTRY\6\6\6"        ,018h,013h,013h,00010111b,012h,001h
						 db "SHOCK\6WAVE\6\6"      ,             0,00000001b,05Eh,001h
						 db "SHROOMISH\6"          ,022h,01Dh,01Dh,00010111b,018h,001h
						 db "SHUCKLE\6\6\6"        ,064h,0A6h,0D5h,00011000b,0D4h,000h
						 db "SHUPPET\6\6\6"        ,092h,061h,061h,00010111b,05Fh,001h
						 db "SIGNAL\6BEAM\6"       ,             0,00000001b,043h,001h
						 db "SILCOON\6\6\6"        ,00Fh,00Ah,00Ah,00010111b,009h,001h
						 db "SILVER\6WIND\6"       ,             0,00000001b,03Dh,001h
						 db "SING\6\6\6\6\6\6\6\6" ,             0,00000000b,02Eh,000h
						 db "SKARMORY\6\6"         ,073h,0C6h,0E3h,00010000b,0E2h,000h
						 db "SKETCH\6\6\6\6\6\6"   ,             0,00000000b,0A5h,000h
						 db "SKILL\6SWAP\6\6"      ,             0,00000001b,01Ch,001h
						 db "SKIPLOOM\6\6"         ,04Dh,044h,0BCh,00011000b,0BBh,000h
						 db "SKITTY\6\6\6\6"       ,03Dh,02Ch,02Ch,00010111b,021h,001h
						 db "SKULL\6BASH\6\6"      ,             0,00000000b,081h,000h
						 db "SKY\6ATTACK\6\6"      ,             0,00000000b,08Eh,000h
						 db "SKY\6UPPERCUT"        ,             0,00000001b,046h,001h
						 db "SLACK\6OFF\6\6\6"     ,             0,00000001b,02Eh,001h
						 db "SLAKING\6\6\6"        ,026h,021h,021h,00010111b,054h,001h
						 db "SLAKOTH\6\6\6"        ,024h,01Fh,01Fh,00010111b,052h,001h
						 db "SLAM\6\6\6\6\6\6\6\6" ,             0,00000000b,014h,000h
						 db "SLASH\6\6\6\6\6\6\6"  ,             0,00000000b,0A2h,000h
						 db "SLEEP\6POWDER"        ,             0,00000000b,04Eh,000h
						 db "SLEEP\6TALK\6\6"      ,             0,00000000b,0D5h,000h
						 db "SLOWBRO\6\6\6"        ,000h,051h,050h,00011000b,04Fh,000h
						 db "SLOWKING\6\6"         ,058h,052h,0C7h,00011000b,0C6h,000h
						 db "SLOWPOKE\6\6"         ,0FFh,050h,04Fh,00010000b,04Eh,000h
						 db "SLUDGE\6\6\6\6\6\6"   ,             0,00000000b,07Bh,000h
						 db "SLUDGE\6BOMB\6"       ,             0,00000000b,0BBh,000h
						 db "SLUGMA\6\6\6\6"       ,067h,0D3h,0DAh,00010000b,0D9h,000h
						 db "SMEARGLE\6\6"         ,072h,09Dh,0EBh,00011000b,0EAh,000h
						 db "SMELLINGSALT"         ,             0,00000001b,008h,001h
						 db "SMOG\6\6\6\6\6\6\6\6" ,             0,00000000b,07Ah,000h
						 db "SMOKESCREEN\6"        ,             0,00000000b,06Bh,000h
						 db "SMOOCHUM\6\6"         ,075h,098h,0EEh,00011000b,0EDh,000h
						 db "SNATCH\6\6\6\6\6\6"   ,             0,00000001b,020h,001h
						 db "SNEASEL\6\6\6"        ,065h,0D5h,0D7h,00011000b,0D6h,000h
						 db "SNORE\6\6\6\6\6\6\6"  ,             0,00000000b,0ACh,000h
						 db "SNORLAX\6\6\6"        ,02Ah,0E1h,08Fh,00011000b,08Eh,000h
						 db "SNORUNT\6\6\6"        ,0ABh,069h,069h,00010111b,040h,001h
						 db "SNUBBULL\6\6"         ,060h,07Bh,0D1h,00011000b,0D0h,000h
						 db "SOFTBOILED\6\6"       ,             0,00000000b,086h,000h
						 db "SOLARBEAM\6\6\6"      ,             0,00000000b,04Bh,000h
						 db "SOLROCK\6\6\6"        ,07Eh,052h,052h,00010111b,043h,001h
						 db "SONICBOOM\6\6\6"      ,             0,00000000b,030h,000h
						 db "SPARK\6\6\6\6\6\6\6"  ,             0,00000000b,0D0h,000h
						 db "SPEAROW\6\6\6"        ,0DFh,00Dh,015h,00010000b,014h,000h
						 db "SPHEAL\6\6\6\6"       ,0ADh,06Bh,06Bh,00010111b,03Bh,001h
						 db "SPIDER\6WEB\6\6"      ,             0,00000000b,0A8h,000h
						 db "SPIKE\6CANNON"        ,             0,00000000b,082h,000h
						 db "SPIKES\6\6\6\6\6\6"   ,             0,00000000b,0BEh,000h
						 db "SPINARAK\6\6"         ,042h,020h,0A7h,00011000b,0A6h,000h
						 db "SPINDA\6\6\6\6"       ,072h,047h,047h,00010111b,01Ah,001h
						 db "SPIT\6UP\6\6\6\6\6"   ,             0,00000000b,0FEh,000h
						 db "SPITE\6\6\6\6\6\6\6"  ,             0,00000000b,0B3h,000h
						 db "SPLASH\6\6\6\6\6\6"   ,             0,00000000b,095h,000h
						 db "SPOINK\6\6\6\6"       ,06Eh,045h,045h,00010111b,045h,001h
						 db "SPORE\6\6\6\6\6\6\6"  ,             0,00000000b,092h,000h
						 db "SQUIRTLE\6\6"         ,0D1h,0E8h,007h,00010000b,006h,000h
						 db "STANTLER\6\6"         ,071h,081h,0EAh,00011000b,0E9h,000h
						 db "STARMIE\6\6\6"        ,090h,0A8h,079h,00010000b,078h,000h
						 db "STARYU\6\6\6\6"       ,08Fh,0A7h,078h,00010000b,077h,000h
						 db "STEEL\6WING\6\6"      ,             0,00000000b,0D2h,000h
						 db "STEELIX\6\6\6"        ,05Fh,03Fh,0D0h,00011000b,0CFh,000h
						 db "STOCKPILE\6\6\6"      ,             0,00000000b,0FDh,000h
						 db "STOMP\6\6\6\6\6\6\6"  ,             0,00000000b,016h,000h
						 db "STRENGTH\6\6\6\6"     ,             0,00000000b,045h,000h
						 db "STRING\6SHOT\6"       ,             0,00000000b,050h,000h
						 db "STRUGGLE\6\6\6\6"     ,             0,00000000b,0A4h,000h
						 db "STUN\6SPORE\6\6"      ,             0,00000000b,04Dh,000h
						 db "SUBMISSION\6\6"       ,             0,00000000b,041h,000h
						 db "SUBSTITUTE\6\6"       ,             0,00000000b,0A3h,000h
						 db "SUDOWOODO\6"          ,04Ah,06Ah,0B9h,00011000b,0B8h,000h
						 db "SUICUNE\6\6\6"        ,07Ch,0F0h,0F5h,00011000b,0F4h,000h
						 db "SUNFLORA\6\6"         ,051h,067h,0C0h,00011000b,0BFh,000h
						 db "SUNKERN\6\6\6"        ,050h,066h,0BFh,00011000b,0BEh,000h
						 db "SUNNY\6DAY\6\6\6"     ,             0,00000000b,0F0h,000h
						 db "SUPER\6FANG\6\6"      ,             0,00000000b,0A1h,000h
						 db "SUPERPOWER\6\6"       ,             0,00000001b,013h,001h
						 db "SUPERSONIC\6\6"       ,             0,00000000b,02Fh,000h
						 db "SURF\6\6\6\6\6\6\6\6" ,             0,00000000b,038h,000h
						 db "SURSKIT\6\6\6"        ,020h,01Bh,01Bh,00010111b,01Dh,001h
						 db "SWABLU\6\6\6\6"       ,079h,04Dh,04Dh,00010111b,04Ch,001h
						 db "SWAGGER\6\6\6\6\6"    ,             0,00000000b,0CEh,000h
						 db "SWALLOW\6\6\6\6\6"    ,             0,00000000b,0FFh,000h
						 db "SWALOT\6\6\6\6"       ,060h,03Dh,03Dh,00010111b,056h,001h
						 db "SWAMPERT\6\6"         ,009h,004h,004h,00010111b,003h,001h
						 db "SWEET\6KISS\6\6"      ,             0,00000000b,0B9h,000h
						 db "SWEET\6SCENT\6"       ,             0,00000000b,0E5h,000h
						 db "SWELLOW\6\6\6"        ,01Ah,015h,015h,00010111b,017h,001h
						 db "SWIFT\6\6\6\6\6\6\6"  ,             0,00000000b,080h,000h
						 db "SWINUB\6\6\6\6"       ,068h,0BFh,0DCh,00011000b,0DBh,000h
						 db "SWORDS\6DANCE"        ,             0,00000000b,00Dh,000h
						 db "SYNTHESIS\6\6\6"      ,             0,00000000b,0EAh,000h
						 db "TACKLE\6\6\6\6\6\6"   ,             0,00000000b,020h,000h
						 db "TAIL\6GLOW\6\6\6"     ,             0,00000001b,025h,001h
						 db "TAIL\6WHIP\6\6\6"     ,             0,00000000b,026h,000h
						 db "TAILLOW\6\6\6"        ,019h,014h,014h,00010111b,016h,001h
						 db "TAKE\6DOWN\6\6\6"     ,             0,00000000b,023h,000h
						 db "TANGELA\6\6\6"        ,016h,0B3h,072h,00011000b,071h,000h
						 db "TAUNT\6\6\6\6\6\6\6"  ,             0,00000001b,00Ch,001h
						 db "TAUROS\6\6\6\6"       ,01Dh,094h,080h,00011000b,07Fh,000h
						 db "TEDDIURSA\6"          ,066h,0C1h,0D8h,00011000b,0D7h,000h
						 db "TEETER\6DANCE"        ,             0,00000001b,029h,001h
						 db "TELEPORT\6\6\6\6"     ,             0,00000000b,063h,000h
						 db "TENTACOOL\6"          ,042h,0A2h,048h,00010000b,047h,000h
						 db "TENTACRUEL"           ,043h,0A3h,049h,00010000b,048h,000h
						 db "THIEF\6\6\6\6\6\6\6"  ,             0,00000000b,0A7h,000h
						 db "THRASH\6\6\6\6\6\6"   ,             0,00000000b,024h,000h
						 db "THUNDER\6\6\6\6\6"    ,             0,00000000b,056h,000h
						 db "THUNDER\6WAVE"        ,             0,00000000b,055h,000h
						 db "THUNDERBOLT\6"        ,             0,00000000b,054h,000h
						 db "THUNDERPUNCH"         ,             0,00000000b,008h,000h
						 db "THUNDERSHOCK"         ,             0,00000000b,053h,000h
						 db "TICKLE\6\6\6\6\6\6"   ,             0,00000001b,040h,001h
						 db "TOGEPI\6\6\6\6"       ,045h,02Eh,0AFh,00011000b,0AEh,000h
						 db "TOGETIC\6\6\6"        ,046h,02Fh,0B0h,00011000b,0AFh,000h
						 db "TORCHIC\6\6\6"        ,004h,0FFh,0FFh,00010000b,0FEh,000h
						 db "TORKOAL\6\6\6"        ,069h,044h,044h,00010111b,027h,001h
						 db "TORMENT\6\6\6\6\6"    ,             0,00000001b,002h,001h
						 db "TOTODILE\6\6"         ,039h,007h,09Eh,00011000b,09Dh,000h
						 db "TOXIC\6\6\6\6\6\6\6"  ,             0,00000000b,05Bh,000h
						 db "TRANSFORM\6\6\6"      ,             0,00000000b,08Fh,000h
						 db "TRAPINCH\6\6"         ,074h,048h,048h,00010111b,032h,001h
						 db "TREECKO\6\6\6"        ,001h,0FCh,0FCh,00010000b,0FBh,000h
						 db "TRI\6ATTACK\6\6"      ,             0,00000000b,0A0h,000h
						 db "TRICK\6\6\6\6\6\6\6"  ,             0,00000001b,00Eh,001h
						 db "TRIPLE\6KICK\6"       ,             0,00000000b,0A6h,000h
						 db "TROPIUS\6\6\6"        ,096h,065h,065h,00010111b,057h,001h
						 db "TWINEEDLE\6\6\6"      ,             0,00000000b,028h,000h
						 db "TWISTER\6\6\6\6\6"    ,             0,00000000b,0EEh,000h
						 db "TYPHLOSION"           ,038h,006h,09Dh,00011000b,09Ch,000h
						 db "TYRANITAR\6"          ,07Fh,0F6h,0F8h,00011000b,0F7h,000h
						 db "TYROGUE\6\6\6"        ,073h,08Fh,0ECh,00011000b,0EBh,000h
						 db "UMBREON\6\6\6"        ,056h,0B9h,0C5h,00011000b,0C4h,000h
						 db "UNOWN\6\6\6\6\6"      ,05Ah,03Dh,0C9h,00011000b,0C8h,000h
						 db "UPROAR\6\6\6\6\6\6"   ,             0,00000000b,0FCh,000h
						 db "URSARING\6\6"         ,067h,0C2h,0D9h,00011000b,0D8h,000h
						 db "VAPOREON\6\6"         ,021h,0B5h,086h,00011000b,085h,000h
						 db "VENOMOTH\6\6"         ,0EEh,06Dh,031h,00010000b,030h,000h
						 db "VENONAT\6\6\6"        ,0EDh,06Ch,030h,00010000b,02Fh,000h
						 db "VENUSAUR\6\6"         ,0CDh,0E4h,003h,00010000b,002h,000h
						 db "VIBRAVA\6\6\6"        ,075h,049h,049h,00010111b,033h,001h
						 db "VICEGRIP\6\6\6\6"     ,             0,00000000b,00Ah,000h
						 db "VICTREEBEL"           ,0FCh,042h,047h,00010000b,046h,000h
						 db "VIGOROTH\6\6"         ,025h,020h,020h,00010111b,053h,001h
						 db "VILEPLUME\6"          ,05Ah,055h,02Dh,00010000b,02Ch,000h
						 db "VINE\6WHIP\6\6\6"     ,             0,00000000b,015h,000h
						 db "VITAL\6THROW\6"       ,             0,00000000b,0E8h,000h
						 db "VOLBEAT\6\6\6"        ,056h,039h,039h,00010111b,068h,001h
						 db "VOLT\6TACKLE\6"       ,             0,00000001b,057h,001h
						 db "VOLTORB\6\6\6"        ,054h,078h,064h,00010000b,063h,000h
						 db "VULPIX\6\6\6\6"       ,099h,07Dh,025h,00010000b,024h,000h
						 db "WAILMER\6\6\6"        ,063h,040h,040h,00010111b,01Fh,001h
						 db "WAILORD\6\6\6"        ,064h,041h,041h,00010111b,020h,001h
						 db "WALREIN\6\6\6"        ,0AFh,06Dh,06Dh,00010111b,03Dh,001h
						 db "WARTORTLE\6"          ,0D2h,0E9h,008h,00010000b,007h,000h
						 db "WATER\6GUN\6\6\6"     ,             0,00000000b,036h,000h
						 db "WATER\6PULSE\6"       ,             0,00000001b,05Fh,001h
						 db "WATER\6SPORT\6"       ,             0,00000001b,059h,001h
						 db "WATER\6SPOUT\6"       ,             0,00000001b,042h,001h
						 db "WATERFALL\6\6\6"      ,             0,00000000b,07Eh,000h
						 db "WEATHER\6BALL"        ,             0,00000001b,036h,001h
						 db "WEEDLE\6\6\6\6"       ,0D7h,01Bh,00Dh,00010000b,00Ch,000h
						 db "WEEPINBELL"           ,0FBh,041h,046h,00010000b,045h,000h
						 db "WEEZING\6\6\6"        ,06Dh,073h,06Eh,00010000b,06Dh,000h
						 db "WHIRLPOOL\6\6\6"      ,             0,00000000b,0F9h,000h
						 db "WHIRLWIND\6\6\6"      ,             0,00000000b,011h,000h
						 db "WHISCASH\6\6"         ,080h,054h,054h,00010111b,02Ah,001h
						 db "WHISMUR\6\6\6"        ,02Dh,025h,025h,00010111b,058h,001h
						 db "WIGGLYTUFF"           ,08Bh,02Dh,028h,00010000b,027h,000h
						 db "WILL-O-WISP\6"        ,             0,00000001b,004h,001h
						 db "WING\6ATTACK\6"       ,             0,00000000b,010h,000h
						 db "WINGULL\6\6\6"        ,01Bh,016h,016h,00010111b,01Bh,001h
						 db "WISH\6\6\6\6\6\6\6\6" ,             0,00000001b,010h,001h
						 db "WITHDRAW\6\6\6\6"     ,             0,00000000b,06Dh,000h
						 db "WOBBUFFET\6"          ,0A1h,06Bh,0CAh,00010000b,0C9h,000h
						 db "WOOPER\6\6\6\6"       ,053h,038h,0C2h,00011000b,0C1h,000h
						 db "WRAP\6\6\6\6\6\6\6\6" ,             0,00000000b,022h,000h
						 db "WURMPLE\6\6\6"        ,00Eh,009h,009h,00010111b,008h,001h
						 db "WYNAUT\6\6\6\6"       ,0A0h,068h,068h,00010111b,04Eh,001h
						 db "XATU\6\6\6\6\6\6"     ,0A3h,0A0h,0B2h,00010000b,0B1h,000h
						 db "YANMA\6\6\6\6\6"      ,052h,065h,0C1h,00011000b,0C0h,000h
						 db "YAWN\6\6\6\6\6\6\6\6" ,             0,00000001b,018h,001h
						 db "ZANGOOSE\6\6"         ,07Bh,04Fh,04Fh,00010111b,062h,001h
						 db "ZAP\6CANNON\6\6"      ,             0,00000000b,0BFh,000h
						 db "ZAPDOS\6\6\6\6"       ,02Ch,0ECh,091h,00011000b,090h,000h
						 db "ZIGZAGOON\6"          ,00Ch,007h,007h,00010111b,006h,001h
alpha_list_last:		 db "ZUBAT\6\6\6\6\6"      ,03Fh,025h,029h,00010000b,028h,000h
alpha_list_end:
 	; to break the search algorithm & prevent displaying gibberish - 6 entries of Stemp characters
 db [7*16]0CBh

p_kana:
 db 22	; Height
 db 22	; Width
 db 000h,080h  ,070h ;         ;
 db 07Ah,040h  ,020h ;         ; E
 db 001h,000h  ,070h ;         ;
 db 07Ch,000h  ,000h ; DE
 db 010h,070h/2,050h ;    ;    ;
 db 010h,0A8h/2,010h ;    ;    ; N
 db 020h,0A8h/2,060h ;    ; no ;
 db 000h,048h/2,000h      ;
 db 02Ah,001h  ,050h ;         ;
 db 002h,028h/2,010h ;(K) ;    ; CU
 db 00Ch,0F4h/2,060h ;    ;    ;
 db 000h,028h/2,000h      ; PO
 db 010h,0A8h/2,070h ;    ;    ;
 db 01Ch,020h/2,010h ;    ;    ; KO
 db 024h,000h  ,070h ; KU      ;
 db 008h,040h/2,000h ;    ;
 db 010h,07Ch/2,020h ;    ;    ;
 db 000h,090h/2,0F8h      ; KE ;
 db 01Ch,010h/2,088h ;    ;    ;
 db 004h,020h/2,008h ; SU ;    ; U
 db 008h,000h  ,010h ;         ;
 db 014h,000h  ,060h ;         ;
p_kana_end:


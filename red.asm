
 DEFINE P1RED, SPACE=ROM  
 SEGMENT P1RED


 extern _Green
 extern _DrawPkName
 extern _Red
 public Red
 extern _Select
 extern _Move
 extern _LoadPic
 public LoadPic
 extern _LPicBank1
 extern _DrawMoveName
 extern _DrawMoveName_AlignRight
 extern _MoveList
 extern _LoadLevelMoves
 extern _StatCalculator
 extern _PokeScreen2
 public GetAbilities


 include "header.inc"
 include "linemacros.inc"

 ; statVars is used to store the Ability bytes

Red:
  ; we get X in DE
  push de		; save X

  push DE
  pop DE

  ; Color
  ld a, 20
  ld (penRow), a
  ld a, 39
  ld (penCol), a
  ld hl,d_color_evolm
  call getbyteaddress
  ld a,(hl)	; Get packed byte
  push af		; Save packed byte
  and 0F8h		; Mask out color only
  sra a	; /2 (That is, /8 to get color and *4 to get string offset)
  ld hl,s_color
  add l
  jr nc,color_skip_inc
  inc h
 color_skip_inc:
  ld l,a			; HL now contains string address
  B_CALL Mov9ToOP1
  ld hl, OP1
  ld b, 4
  B_CALL VPutSN		; Print color text

  ; Evol method
  ld a, 26
  ld (penRow), a
  ld a, 38
  ld (penCol), a
  pop af		; Retrieve packed byte
  and 7			; Mask out evol method only
  jr z,red_skip_evol	; Skip whole pre-evolution line if no evolution
  pop de
  push de
  push af	; Save evol method
  push de
  ld hl,s_evolparam
  add l
  jr nc,evolm_skip_inc
  inc h
 evolm_skip_inc:
  ld l,a			; HL now contains string address
  ld a,(hl)
  B_CALL VPutMap		; Print color text

  ;Evol param
  ld a, 26
  ld (penRow), a
  ld a, 42
  ld (penCol), a
  pop de			; get X
  ld hl,d_evol_param
  call getbyteaddress
  pop af		; Get evol method
  cp 1		; Is evol method 1 (L)?
  jr z,evol_param_number
  ld a,(hl)			; load char to a
  B_CALL VPutMap	; print char
  ld a,SdotIcon	; load spiffication char to a
  B_CALL VPutMap	; print spiffication char
  jr evol_param_after
 evol_param_number:
  ld a,(hl)			; load number to a
  B_CALL SetXXOP1	; put number to OP1
  ld a,2	; 2 Digits
  B_CALL DispOP1A	; print number
 evol_param_after:

  ; Evol symbol
  ld a, 26
  ld (penRow), a
  ld a, 50
  ld (penCol), a
  ld a,Sangle
  B_CALL VPutMap

  ; Evol From
  ld a, 26
  ld (penRow), a
  ld a, 54
  ld (penCol), a
  pop de	; recall X
  push de
  ld hl,d_evol_from
  call getword
  ld h,d
  ld l,e
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  B_CALL DrawPkName
  jr red_skip_first_stage

 red_skip_evol:
  ; penCol & penRow already initialized
  ld hl,s_stage1
  ld de,OP1
  ld bc,s_stage1_end-s_stage1
  ldir
  ld hl,OP1
  ld b,s_stage1_end-s_stage1
  B_CALL VPutSN
 red_skip_first_stage:

  ; Abilities
  ld a, 50
  ld (penRow), a
  xor a
  ld (penCol), a
  pop de		; get x
  push de
  call GetAbilities
  ld H,0
  ld A,(statVars)
  ld L,A
  call printability
  ld a, 56
  ld (penRow), a
  xor a
  ld (penCol), a
  ld H,0
  ld A,(statVars+1)
  ld L,A
  call printability

  ;ld hl,d_abilities
  ;call getword
  ;push de		; save abilities
  ;ld h,0
  ;ld l,e	; HL now contains first ability
  ;ld A,L
  ;ld (statVars),A
  ;call printability
  ;ld a, 56
  ;ld (penRow), a
  ;xor a
  ;ld (penCol), a
  ;pop de		; get abilities
  ;ld h,0
  ;ld l,d	; HL now contains second ability
  ;ld A,L
  ;ld (statVars+1),A
  ;call printability

  ; Catch rate
  ld a, 32
  ld (penRow), a
  ld a, 50
  ld (penCol), a
  pop de		; get x
  push de
  ld hl,d_catch
  call getbyteaddress
  ld l,(hl)
  ld h,0
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  ld a,3
  B_CALL FormBase
  ld HL,OP3
  ld B,C
  B_CALL VPutSN

  ; Base Tameness
  ld a, 38
  ld (penRow), a
  ld a, 50
  ld (penCol), a
  pop de		; get x
  push de
  ld hl,d_happy
  call getbyteaddress
  ld l,(hl)
  ld h,0
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  ld a,3
  B_CALL FormBase
  ld HL,OP3
  ld B,C
  B_CALL VPutSN

  ; Egg Steps
  ld a, 44
  ld (penRow), a
  ld a, 50
  ld (penCol), a
  pop de		; get x
  push de
  ld hl,d_eggstep
  call getbyteaddress
  ld l,(hl)
  ld h,0
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  ld a,3
  B_CALL FormBase
  ld HL,OP3
  ld B,C
  B_CALL VPutSN

  ; EV Bonus: SDE
  ld a, 44
  ld (penRow), a
  ld a, 28
  ld (penCol), a
  pop de		; get x
  push de
  ld hl,d_ev
  call getword
  push de		; save packed EVbonus word
  ld a,e
  and 00000011b
  jr z,evbonus_skipsde
  push af
  ld a,'+'
  B_CALL VPutMap
  pop af
  add '0'
  B_CALL VPutMap
 evbonus_skipsde:
  ; SAT
  ld a, 38
  ld (penRow), a
  ld a, 28
  ld (penCol), a
  pop de			; recall packed EVbonus word
  push de
  ld a,e
  rra
  rra
  and 00000011b
  jr z,evbonus_skipsat
  push af
  ld a,'+'
  B_CALL VPutMap
  pop af
  add '0'
  B_CALL VPutMap
 evbonus_skipsat:
  ; SPD
  ld a, 32
  ld (penRow), a
  ld a, 28
  ld (penCol), a
  pop de			; recall packed EVbonus word
  push de
  ld a,e
  rra
  rra
  rra
  rra
  and 00000011b
  jr z,evbonus_skipspd
  push af
  ld a,'+'
  B_CALL VPutMap
  pop af
  add '0'
  B_CALL VPutMap
 evbonus_skipspd:
  ; DEF
  ld a, 26
  ld (penRow), a
  ld a, 28
  ld (penCol), a
  pop de			; recall packed EVbonus word
  push de
  ld a,e
  rra
  rra
  rra
  rra
  rra
  rra
  and 00000011b
  jr z,evbonus_skipdef
  push af
  ld a,'+'
  B_CALL VPutMap
  pop af
  add '0'
  B_CALL VPutMap
 evbonus_skipdef:
  ; ATK
  ld a, 20
  ld (penRow), a
  ld a, 28
  ld (penCol), a
  pop de			; recall packed EVbonus word
  push de
  ld a,d
  and 00000011b
  jr z,evbonus_skipatk
  push af
  ld a,'+'
  B_CALL VPutMap
  pop af
  add '0'
  B_CALL VPutMap
 evbonus_skipatk:
  ; HP
  ld a, 14
  ld (penRow), a
  ld a, 28
  ld (penCol), a
  pop de			; recall packed EVbonus word
  push de
  ld a,d
  rra
  rra
  and 00000011b
  jr z,evbonus_skiphp
  push af
  ld a,'+'
  B_CALL VPutMap
  pop af
  add '0'
  B_CALL VPutMap
 evbonus_skiphp:
  pop de			; recall packed EVbonus word (needed here too because of all the skips)

  endbuffer

  ; picture
  pop DE
  push DE
  call LoadPic
 ; ld DE,42*256+71					; DE <- coords
  ld HL,appBackUpScreen			; HL points to copied image
  ld B,(HL)
  inc HL
  ld C,(HL)
  dec HL	; HL returns to pointing to appBackUpScreen
  srl B
  srl C
  ld A,34+15
  sub A,B
  ld D,A
  ld A,63+15
  sub A,C
  ld E,A
 ; ld DE,34*256+63					; DE <- coords
  ld HL,appBackUpScreen			; HL points to copied image
  B_CALL DisplayImage


  pop de	; get X again so it won't mess up the stack

 ; STATIC
  ld hl,p_icons					; source pointer
  ld de, appBackUpScreen		; a temp RAM area
  ld bc,25			; put length to BC
  ldir				; load image to RAM
  ld hl,appBackUpScreen			; HL points to copied image
  ld de,33*256+44					; DE <- coords
  B_CALL DisplayImage

  ld hl,appBackUpScreen+13		; HL points to copied image
  ld A,(HL)
  inc HL
  ld A,(HL)
  dec HL
  ld de,45*256+40					; DE <- coords
  B_CALL DisplayImage

  jp waitkey
  ;ret

GetAbilities:
  ld hl,d_abilities
  call getword
  ld A,E
  ld (statVars),A
  ld A,D
  ld (statVars+1),A
  set abilitiesinstatvars,(IY+dexstate)
  ret

printability:
  B_CALL SetXXXXOP2
  B_CALL OP2ToOP1
  ld a,12
  B_CALL SetXXOP2
  B_CALL FPMult
  B_CALL ConvOP1		; DE now contains offset to 12-byte string
  ld bc,s_abilities
  ld a,e
  add c
  jr nc,ability1_skip_inc_0
  inc b
 ability1_skip_inc_0:
  ld l,a
  ld a,d
  add b
  ld h,a	; HL now contains address to 12-byte string
  ld de,OP1
  ld bc,12
  ldir
  ld hl,OP1
  ld b,12
  B_CALL VPutSN
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

getword:
  ; Inputs:	de - offset
  ;			hl - address
  ; Output: DE
  add hl,de
  add hl,de
  ld e,(hl)	; put what HL points to to DE
  inc hl
  ld d,(hl)
  ret

s_stage1:	db "(FIRST STAGE)"
s_stage1_end:

s_color:
 db "Red "			; Red =0
 db "B",Sbar,"ue"	; Blue=1
 db "Y",Sbar,"w "	; Ylw =2
 db "Grn "			; Grn =3
 db "B",Sbar,"ak"	; Blak=4
 db "Brw "			; Brw =5
 db "Prp",Sbar		; Prpl=6
 db "Gray"			; Gray=7
 db "Whit"			; Whit=8
 db "Pink"			; Pink=9

s_evolparam:
 db " " ; 0
 db "L" ; 1
 db "H" ; 2
 db "S" ; 3
 db "T" ; 4
 db "W" ; 5
 db "X" ; 6
 db "C" ; 7

s_abilities:
 db "            "
 db "STENCH",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"DRIZZLE",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"SPEED",SFourSpaces,"BOOST",SFourSpaces,"BATTLE",SFourSpaces,"ARMOR"
 db "STURDY",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"DAMP",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"LIMBER",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"SAND",SFourSpaces,"VEIL",SFourSpaces,SFourSpaces,SFourSpaces,"STATIC",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces
 db "VOLT",SFourSpaces,"ABSORB",SFourSpaces,"WATER",SFourSpaces,"ABSORB","OBLIVIOUS",SFourSpaces,SFourSpaces,SFourSpaces,"CLOUD",SFourSpaces,"NINE",SFourSpaces,SFourSpaces,"COMPOUNDEYES"
 db "INSOMNIA",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"COLOR",SFourSpaces,"CHANGE","IMMUNITY",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"FLASH",SFourSpaces,"FIRE",SFourSpaces,SFourSpaces,"SHIELD",SFourSpaces,"DUST",SFourSpaces
 db "OWN",SFourSpaces,"TEMPO",SFourSpaces,SFourSpaces,SFourSpaces,"SUCTION",SFourSpaces,"CUPS","INTIMIDATE",SFourSpaces,SFourSpaces,"SHADOW",SFourSpaces,"TAG",SFourSpaces,SFourSpaces,"ROUGH",SFourSpaces,"SKIN",SFourSpaces,SFourSpaces
 db "WONDER",SFourSpaces,"GUARD","LEVITATE",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"EFFECT",SFourSpaces,"SPORE","SYNCHRONIZE",SFourSpaces,"CLEAR",SFourSpaces,"BODY",SFourSpaces,SFourSpaces
 db "NATURAL",SFourSpaces,"CURE","LIGHTNINGROD","SERENE",SFourSpaces,"GRACE","SWIFT",SFourSpaces,"SWIM",SFourSpaces,SFourSpaces,"CHLOROPHYLL",SFourSpaces
 db "ILLUMINATE",SFourSpaces,SFourSpaces,"TRACE",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"HUGE",SFourSpaces,"POWER",SFourSpaces,SFourSpaces,"POISON",SFourSpaces,"POINT","INNER",SFourSpaces,"FOCUS",SFourSpaces
 db "MAGMA",SFourSpaces,"ARMOR",SFourSpaces,"WATER",SFourSpaces,"VEIL",SFourSpaces,SFourSpaces,"MAGNET",SFourSpaces,"PULL",SFourSpaces,"SOUNDPROOF",SFourSpaces,SFourSpaces,"RAIN",SFourSpaces,"DISH",SFourSpaces,SFourSpaces,SFourSpaces
 db "SAND",SFourSpaces,"STREAM",SFourSpaces,"PRESSURE",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"THICK",SFourSpaces,"FAT",SFourSpaces,SFourSpaces,SFourSpaces,"EARLY",SFourSpaces,"BIRD",SFourSpaces,SFourSpaces,"FLAME",SFourSpaces,"BODY",SFourSpaces,SFourSpaces
 db "RUN",SFourSpaces,"AWAY",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"KEEN",SFourSpaces,"EYE",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"HYPER",SFourSpaces,"CUTTER","PICKUP",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"TRUANT",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces
 db "HUSTLE",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"CUTE",SFourSpaces,"CHARM",SFourSpaces,SFourSpaces,"PLUS",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"MINUS",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"FORECAST",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces
 db "STICKY",SFourSpaces,"HOLD",SFourSpaces,"SHED",SFourSpaces,"SKIN",SFourSpaces,SFourSpaces,SFourSpaces,"GUTS",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"MARVEL",SFourSpaces,"SCALE","LIQUID",SFourSpaces,"OOZE",SFourSpaces
 db "OVERGROW",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"BLAZE",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"TORRENT",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"SWARM",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"ROCK",SFourSpaces,"HEAD",SFourSpaces,SFourSpaces,SFourSpaces
 db "DROUGHT",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,"ARENA",SFourSpaces,"TRAP",SFourSpaces,SFourSpaces,"VITAL",SFourSpaces,"SPIRIT","WHITE",SFourSpaces,"SMOKE",SFourSpaces,"PURE",SFourSpaces,"POWER",SFourSpaces,SFourSpaces
 db "SHELL",SFourSpaces,"ARMOR",SFourSpaces,"CACOPHONY",SFourSpaces,SFourSpaces,SFourSpaces,"AIR",SFourSpaces,"LOCK",SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces,SFourSpaces
 ;DB                "STENCH      ","DRIZZLE     ","SPEED BOOST ","BATTLE ARMOR"
 ;DB "STURDY      ","DAMP        ","LIMBER      ","SAND VEIL   ","STATIC      "
 ;DB "VOLT ABSORB ","WATER ABSORB","OBLIVIOUS   ","CLOUD NINE  ","COMPOUNDEYES"
 ;DB "INSOMNIA    ","COLOR CHANGE","IMMUNITY    ","FLASH FIRE  ","SHIELD DUST "
 ;DB "OWN TEMPO   ","SUCTION CUPS","INTIMIDATE  ","SHADOW TAG  ","ROUGH SKIN  "
 ;DB "WONDER GUARD","LEVITATE    ","EFFECT SPORE","SYNCHRONIZE ","CLEAR BODY  "
 ;DB "NATURAL CURE","LIGHTNINGROD","SERENE GRACE","SWIFT SWIM  ","CHLOROPHYLL "
 ;DB "ILLUMINATE  ","TRACE       ","HUGE POWER  ","POISON POINT","INNER FOCUS "
 ;DB "MAGMA ARMOR ","WATER VEIL  ","MAGNET PULL ","SOUNDPROOF  ","RAIN DISH   "
 ;DB "SAND STREAM ","PRESSURE    ","THICK FAT   ","EARLY BIRD  ","FLAME BODY  "
 ;DB "RUN AWAY    ","KEEN EYE    ","HYPER CUTTER","PICKUP      ","TRUANT      "
 ;DB "HUSTLE      ","CUTE CHARM  ","PLUS        ","MINUS       ","FORECAST    "
 ;DB "STICKY HOLD ","SHED SKIN   ","GUTS        ","MARVEL SCALE","LIQUID OOZE "
 ;DB "OVERGROW    ","BLAZE       ","TORRENT     ","SWARM       ","ROCK HEAD   "
 ;DB "DROUGHT     ","ARENA TRAP  ","VITAL SPIRIT","WHITE SMOKE ","PURE POWER  "
 ;DB "SHELL ARMOR ","CACOPHONY   ","AIR LOCK    ","NULL ABILITY"

p_icons:
 db 11,5
 db 01110000b
 db 11111000b
 db 11111000b
 db 10001000b
 db 01110000b
 db 00000000b
 db 01010000b
 db 11111000b
 db 11111000b
 db 01110000b
 db 00100000b
p_icons2:
 db 5,9
 db 00010000b, 0
 db 00101000b, 0
 db 01001010b, 10000000b
 db 01001001b, 0
 db 00110010b, 10000000b

d_color_evolm:
 db 3*8+0,3*8+1,3*8+1,0*8+0,0*8+1,0*8+1,1*8+0,1*8+1,1*8+1,3*8+0,3*8+1
 db 8*8+1,5*8+0,2*8+1,2*8+1,5*8+0,5*8+1,5*8+1,6*8+0,5*8+1,5*8+0,5*8+1
 db 6*8+0,6*8+1,2*8+2,2*8+3,2*8+0,2*8+1,1*8+0,1*8+1,1*8+3,6*8+0,6*8+1
 db 6*8+3,9*8+2,9*8+3,5*8+0,2*8+3,9*8+2,9*8+3,6*8+0,6*8+1,1*8+0,1*8+1
 db 0*8+3,0*8+0,0*8+1,6*8+0,6*8+1,5*8+0,5*8+1,2*8+0,2*8+1,2*8+0,1*8+1
 db 5*8+0,5*8+1,5*8+0,5*8+3,1*8+0,1*8+1,1*8+3,5*8+0,5*8+1,5*8+4,7*8+0
 db 7*8+1,7*8+4,3*8+0,3*8+1,3*8+3,1*8+0,1*8+1,5*8+0,5*8+1,5*8+4,2*8+0
 db 2*8+1,9*8+0,9*8+1,7*8+0,7*8+1,5*8+0,5*8+0,5*8+1,8*8+0,8*8+1,6*8+0
 db 6*8+1,6*8+0,6*8+3,6*8+0,6*8+1,6*8+4,7*8+0,2*8+0,2*8+1,0*8+0,0*8+1
 db 0*8+0,0*8+1,9*8+0,2*8+3,5*8+0,5*8+1,5*8+5,5*8+5,9*8+0,6*8+0,6*8+1
 db 7*8+0,7*8+1,9*8+0,1*8+0,5*8+0,1*8+0,1*8+1,0*8+0,0*8+1,5*8+0,6*8+3
 db 9*8+0,3*8+0,0*8+1,2*8+1,0*8+1,5*8+0,5*8+0,0*8+0,1*8+1,1*8+0,6*8+0
 db 5*8+0,1*8+3,2*8+3,0*8+3,9*8+0,1*8+0,1*8+1,5*8+0,5*8+1,6*8+0,4*8+0
 db 1*8+0,2*8+0,2*8+0,1*8+0,1*8+1,5*8+1,6*8+0,9*8+0,3*8+0,3*8+1,3*8+1
 db 2*8+0,2*8+1,2*8+1,1*8+0,1*8+1,1*8+1,5*8+0,5*8+1,5*8+0,5*8+1,0*8+0
 db 0*8+1,3*8+0,0*8+1,6*8+2,1*8+0,1*8+1,2*8+0,9*8+0,9*8+0,8*8+0,8*8+2
 db 3*8+0,3*8+1,8*8+0,9*8+1,2*8+1,3*8+3,1*8+2,1*8+1,5*8+0,3*8+4,9*8+0
 db 3*8+1,1*8+1,6*8+0,2*8+0,2*8+3,0*8+0,1*8+0,1*8+1,6*8+2,4*8+2,4*8+0
 db 9*8+4,7*8+0,4*8+0,1*8+1,2*8+0,7*8+0,6*8+1,2*8+0,6*8+0,7*8+4,9*8+0
 db 6*8+1,7*8+0,0*8+4,2*8+0,1*8+0,4*8+0,5*8+0,5*8+1,0*8+0,0*8+1,5*8+0
 db 5*8+1,9*8+0,7*8+0,0*8+1,0*8+0,6*8+0,7*8+0,4*8+0,4*8+1,1*8+4,1*8+0
 db 7*8+1,0*8+4,5*8+0,8*8+0,6*8+0,5*8+5,9*8+0,2*8+0,0*8+0,9*8+0,9*8+2
 db 2*8+0,5*8+0,1*8+0,3*8+0,7*8+1,3*8+1,8*8+0,0*8+0,3*8+0,3*8+0,3*8+1
 db 3*8+1,0*8+0,0*8+1,0*8+1,1*8+0,1*8+1,1*8+1,7*8+0,7*8+1,5*8+0,8*8+1
 db 0*8+0,8*8+6,2*8+1,6*8+6,3*8+1,3*8+0,3*8+1,3*8+3,5*8+0,5*8+1,5*8+3
 db 7*8+0,2*8+1,5*8+5,1*8+0,1*8+1,5*8+0,3*8+1,5*8+0,8*8+0,2*8+1,1*8+0
 db 1*8+1,1*8+0,1*8+1,9*8+0,6*8+3,3*8+0,5*8+0,4*8+1,7*8+0,5*8+0,6*8+0
 db 7*8+0,1*8+1,9*8+0,0*8+0,0*8+1,5*8+0,9*8+7,0*8+0,1*8+1,5*8+0,3*8+1
 db 3*8+1,2*8+0,5*8+1,3*8+0,2*8+1,2*8+0,0*8+1,1*8+0,1*8+1,1*8+1,3*8+0
 db 3*8+1,7*8+0,7*8+1,2*8+0,0*8+0,1*8+0,4*8+0,6*8+1,2*8+0,2*8+0,4*8+0
 db 1*8+0,0*8+1,1*8+0,1*8+1,1*8+0,4*8+0,4*8+1,3*8+0,5*8+0,8*8+1,5*8+1
 db 3*8+0,6*8+1,3*8+0,9*8+0,1*8+1,1*8+1,1*8+0,1*8+4,9*8+4,8*8+0,4*8+0
 db 4*8+1,4*8+0,8*8+0,7*8+0,7*8+0,7*8+1,7*8+1,8*8+0,7*8+0,6*8+0,6*8+0
 db 3*8+1,7*8+0,7*8+1,8*8+0,8*8+1,8*8+1,1*8+0,8*8+1,1*8+1,1*8+0,1*8+1
 db 1*8+1,5*8+0,1*8+0,7*8+0,1*8+0,0*8+0,3*8+0,0*8+0,1*8+0,2*8+0,0*8+0
 db 1*8+0	; Chimecho's soooo special...

d_evol_param:
 db 00,16,32,00,16,36,00,16,36,00,07,10,00,07,10,00,18,36,00,20,00,20
 db 00,22,SdotIcon,"T",00,22,00,16,"M",00,16,"M",SdotIcon,"M",00,"F",SdotIcon,"M",00,22,00,21
 db "L",00,24,00,31,00,26,00,28,00,33,00,28,00,"F",00,25,"W",00,16,SdotIcon,00
 db 28,SdotIcon,00,21,"L",00,30,00,25,SdotIcon,00,40,00,37,00,30,00,00,31,00,34,00
 db 38,00,"W",00,25,SdotIcon,00,00,26,00,28,00,30,00,"L",00,28,"<","=",00,00,35
 db 00,42,00,00,00,00,32,00,33,00,"W",00,00,30,30,30,00,00,00,20,00,00
 db 00,"W","T","F",00,00,40,00,40,00,00,00,00,00,00,30,55,00,00,00,16,32
 db 00,14,36,00,18,30,00,15,00,20,00,18,00,22,SdotIcon,00,27,00,00,00,00,SdotIcon
 db 00,25,00,15,30,"S",SdotIcon,18,00,"K",00,18,27,00,00,"S",00,00,20,"D","N",00
 db "K",00,00,15,00,00,31,00,00,"M",00,23,00,"M",00,00,00,00,30,00,38,00
 db 33,00,00,25,00,00,00,00,24,"D",00,25,"U",00,00,00,">",00,00,00,00,SdotIcon
 db 00,00,00,00,30,55,00,00,00,00,16,36,00,16,36,00,16,36,00,18,00,20
 db 00,"F",10,"M",10,00,14,"W",00,14,"L",00,20,"N",00,22,00,23,00,00,25,00
 db 22,00,40,00,"M",00,00,36,00,00,00,00,30,00,00,30,00,"B",00,30,00,35
 db 45,00,24,00,26,00,33,00,32,44,00,32,00,42,00,00,00,00,32,00,00,00
 db 00,37,00,35,00,00,37,00,00,18,36,00,26,00,00,20,40,00,"T","S",00,00
 db 37,00,00,00,00,32,42,00,00,00,00,40,00,40,00,20,30,00,30,50,00,20
 db 45,00,00,00,00,00,00,00,00,00,00,00,00

d_evol_from:
n equ -1
 dw n,0,1,n,3,4,n,6,7,n,9,10,n,12,13,n,15,16,n,18,n,20,n,22,171,24,n,26,n,28
 dw 29,n,31,32,172,34,n,36,173,38,n,40,n,42,43,n,45,n,47,n,49,n,51,n,53,n,55
 dw n,57,n,59,60,n,62,63,n,65,66,n,68,69,n,71,n,73,74,n,76,n,78,n,80,n,n,83
 dw n,85,n,87,n,89,n,91,92,n,n,95,n,97,n,99,n,101,n,103,235,235,n,n,108,n,110
 dw n,n,n,n,115,n,117,n,119,n,n,237,238,239,n,n,n,128,n,n,n,132,132,132,n,n
 dw 137,n,139,n,n,n,n,n,n,146,147,n,n,n,151,152,n,154,155,n,157,158,n,160,n
 dw 162,n,164,n,166,41,n,169,n,n,n,n,174,n,176,n,178,179,43,324,182,n,60,n
 dw 186,187,n,n,190,n,n,193,132,132,n,78,n,n,334,n,n,203,n,n,94,n,208,n,122
 dw n,n,n,n,215,n,217,n,219,n,n,222,n,n,n,n,227,116,n,230,136,n,n,n,235,n,n
 dw n,n,112,n,n,n,n,245,246,n,n,n,n,251,252,n,254,255,n,257,258,n,260,n,262
 dw n,264,265,264,267,n,269,270,n,272,273,n,275,275,n,278,n,280,n,n,283,n,285
 dw n,287,n,289,n,n,292,n,n,n,n,297,n,n,300,n,302,n,304,n,306,307,n,309,n,311
 dw n,313,n,315,316,n,318,n,320,n,n,n,n,325,n,n,n,n,330,n,332,n,n,335,n,n,338
 dw 339,n,341,n,n,344,345,n,347,347,n,n,351,n,n,n,n,356,357,n,n,n,n,362,n,364
 dw n,366,367,n,369,370,n,372,373,n,n,n,n,n,n,n,n,n,n,n,n

d_abilities:
 db 65,0,65,0,65,0,66,0,66,0,66,0,67,0,67,0,67,0,19,0,61,0
 db 14,0,19,0,61,0,68,0,51,0,51,0,51,0,50,62,50,62,51,0,51,0
 db 22,61,22,61,9,0,9,0,8,0,8,0,38,0,38,0,38,0,38,0,38,0
 db 38,0,56,0,56,0,18,0,18,0,56,0,56,0,39,0,39,0,34,0,34,0
 db 34,0,27,0,27,0,14,0,19,0,8,71,8,71,53,0,7,0,6,13,6,13
 db 72,0,72,0,22,18,22,18,11,6,11,6,11,6,28,39,28,39,28,39,62,0
 db 62,0,62,0,34,0,34,0,34,0,29,64,29,64,69,5,69,5,69,5,50,18
 db 50,18,12,20,12,20,42,5,42,5,51,39,50,48,50,48,47,0,47,0,1,60
 db 1,60,75,0,75,0,26,0,26,0,26,0,69,5,15,0,15,0,52,75,52,75
 db 43,9,43,9,34,0,34,0,69,31,69,31,7,0,51,0,20,12,26,0,26,0
 db 31,69,31,69,30,32,34,0,48,0,33,0,38,0,33,41,33,41,35,30,35,30
 db 43,0,68,0,12,0,9,0,49,0,52,0,22,0,33,0,22,0,11,75,7,0
 db 50,0,11,0,10,0,18,0,36,0,33,75,33,75,33,4,33,4,69,46,17,47
 db 46,0,46,0,46,0,61,0,61,0,39,0,46,0,28,0,65,0,65,0,65,0
 db 66,0,66,0,66,0,67,0,67,0,67,0,50,51,50,51,15,51,15,51,68,48
 db 68,48,68,15,68,15,39,0,10,35,10,35,9,0,56,0,56,0,55,32,55,32
 db 28,48,28,48,9,0,9,0,9,0,34,0,47,37,47,37,5,69,11,6,34,0
 db 34,0,34,0,50,53,34,0,34,0,3,14,6,11,6,11,28,0,28,0,15,0
 db 12,20,26,0,26,0,23,0,39,48,5,0,5,0,32,50,52,8,69,5,22,50
 db 22,0,38,33,68,0,5,0,68,62,39,51,53,0,62,0,40,49,40,49,12,0
 db 12,0,55,30,55,0,21,0,72,55,33,11,51,5,48,18,48,18,33,0,53,0
 db 5,0,36,0,22,0,20,0,62,0,22,0,12,0,9,0,49,0,47,0,30,32
 db 46,0,46,0,46,0,62,0,61,0,45,0,46,0,46,0,30,0,65,0,65,0
 db 65,0,66,0,66,0,66,0,67,0,67,0,67,0,50,0,22,0,53,0,53,0
 db 19,0,61,0,68,0,61,0,19,0,33,44,33,44,33,44,34,48,34,48,34,48
 db 14,0,3,0,25,0,62,0,62,0,27,0,27,0,20,0,51,0,51,0,33,0
 db 22,0,41,12,41,12,56,0,56,0,16,0,26,0,26,0,5,42,73,0,51,0
 db 12,0,12,0,33,0,52,75,52,75,33,0,63,0,24,0,24,0,52,71,26,0
 db 26,0,47,62,47,62,9,31,9,31,12,0,40,0,47,0,47,0,47,0,8,0
 db 8,0,39,0,39,0,26,0,26,0,47,37,47,20,47,20,57,0,58,0,52,22
 db 74,0,74,0,30,0,30,0,23,0,26,0,46,0,30,38,54,0,72,0,54,0
 db 64,60,64,60,34,0,43,0,43,0,43,0,75,0,33,0,33,0,46,0,15,0
 db 15,0,61,0,17,0,33,69,5,69,5,69,5,69,59,0,35,68,12,0,21,0
 db 21,0,4,0,4,0,28,36,28,36,28,36,69,0,69,0,22,0,29,0,29,0
 db 29,0,29,0,29,0,29,0,2,0,70,0,77,0,26,0,26,0,32,0,46,0,26,0

d_catch:
 db 45,45,45,45,45,45,45,45,45,255,120,45,255,120,45,255,120,45,255,127
 db 255,90,255,90,190,75,255,90,235,120,45,235,120,45,150,25,190,75,170,50
 db 255,90,255,120,45,190,75,190,75,255,50,255,90,190,75,190,75,190,75,255
 db 120,45,200,100,50,180,90,45,255,120,45,190,60,255,120,45,190,60,190,75
 db 190,60,45,190,45,190,75,190,75,190,60,190,90,45,45,190,75,225,60,190
 db 60,90,45,190,75,45,45,45,190,60,120,60,30,45,45,225,75,225,60,225
 db 60,45,45,45,45,45,45,45,255,45,45,35,45,45,45,45,45,45,45,45
 db 45,45,25,3,3,3,45,45,45,3,45,45,45,45,45,45,45,45,45,45
 db 255,90,255,90,255,90,255,90,90,190,75,190,150,170,190,75,190,75,235,120
 db 45,45,190,75,65,45,255,120,45,45,235,120,75,255,90,45,45,30,70,45
 db 225,45,60,190,75,190,60,25,190,75,45,25,190,45,60,120,60,190,75,225
 db 75,60,190,75,45,25,25,120,45,45,120,60,45,45,45,75,45,45,45,45
 db 45,30,3,3,3,45,45,45,3,3,45,45,45,45,45,45,45,45,45,45
 db 255,127,255,90,255,120,45,120,45,255,120,45,255,120,45,255,120,45,200,45
 db 255,90,255,190,45,200,75,125,60,255,60,200,255,90,255,90,45,190,75,225
 db 205,155,255,60,225,60,255,120,45,180,200,120,45,255,150,255,120,45,190,60
 db 190,75,45,45,150,255,60,200,200,45,180,90,255,45,125,190,90,150,255,120
 db 45,225,75,200,190,120,45,255,60,60,30,225,45,90,90,25,180,90,45,45
 db 150,150,45,45,45,45,235,120,45,45,45,45,3,3,3,3,3,3,5,5
 db 3,3,3,3,3,45

d_happy:
 db 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70
 db 70,70,70,70,70,70,70,70,70,70,70,140,140,70,70,70,70,70,70,70,70,70,70
 db 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70
 db 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70
 db 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,140,70,70
 db 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70
 db 70,70,70,70,70,35,35,35,35,35,35,0,100,70,70,70,70,70,70,70,70,70,70
 db 70,70,70,70,70,70,70,70,70,70,70,140,70,70,70,70,70,70,70,70,70,70,70
 db 70,70,70,70,70,70,70,70,70,70,70,70,35,35,70,35,70,70,70,70,70,70,70
 db 70,70,70,70,70,70,70,35,70,70,70,70,70,70,70,70,70,70,70,70,35,35,70
 db 70,70,70,70,70,70,70,70,70,70,70,140,35,35,35,35,35,35,0,0,100,70,70
 db 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70
 db 70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,35,70,70
 db 70,70,70,70,70,35,35,70,70,70,70,70,70,70,70,70,70,70,70,35,35,70,70
 db 70,70,70,70,70,70,70,70,70,70,70,70,70,35,35,70,70,70,70,70,70,70,70
 db 70,70,70,70,70,35,35,35,70,70,70,35,35,35,70,70,70,70,70,70,70,35,35
 db 35,35,35,35,35,35,35,35,35,35,0,0,0,90,90,100,0,70

d_ev:
 dw 000000000100b,000000000101b,000000001001b,000000010000b,000000010100b,000000001100b
 dw 000001000000b,000001000001b,000000000011b,010000000000b,000010000000b,000000001001b
 dw 000000010000b,000010000000b,001000000001b,000000010000b,000000100000b,000000110000b
 dw 000000010000b,000000100000b,000000010000b,000000100000b,000100000000b,001000000000b
 dw 000000100000b,000000110000b,000001000000b,000010000000b,010000000000b,100000000000b
 dw 110000000000b,000100000000b,001000000000b,001100000000b,100000000000b,110000000000b
 dw 000000010000b,000000010001b,100000000000b,110000000000b,000000010000b,000000100000b
 dw 000000000100b,000000001000b,000000001100b,000100000000b,001001000000b,000000000001b
 dw 000000010100b,000000010000b,000000100000b,000000010000b,000000100000b,000000000100b
 dw 000000001000b,000100000000b,001000000000b,000100000000b,001000000000b,000000010000b
 dw 000000100000b,000011000000b,000000000100b,000000001000b,000000001100b,000100000000b
 dw 001000000000b,001100000000b,000100000000b,001000000000b,001100000000b,000000000001b
 dw 000000000010b,000001000000b,000010000000b,000011000000b,000000010000b,000000100000b
 dw 010000000000b,000010000000b,000000000100b,000000001000b,000100000000b,000100000000b
 dw 001000000000b,000000000001b,000000000010b,010000000000b,010100000000b,000001000000b
 dw 000010000000b,000000000100b,000000001000b,000000001100b,000001000000b,000000000001b
 dw 000000000010b,000100000000b,001000000000b,000000010000b,000000100000b,000001000000b
 dw 000000001000b,000001000000b,000010000000b,001000000000b,000000000010b,100000000000b
 dw 000001000000b,000010000000b,000001000000b,001000000000b,100000000000b,000001000000b
 dw 100000000000b,000000000100b,000001000100b,000100000000b,001000000000b,000000010000b
 dw 000000100000b,000000000010b,000100000000b,000000001000b,000000100000b,000000001000b
 dw 001000000000b,000100010000b,000000010000b,001000000000b,100000000000b,010000000000b
 dw 000000000001b,100000000000b,000000100000b,001000000000b,000000000100b,000001000000b
 dw 000010000000b,000001000000b,001000000000b,000000100000b,100000000000b,000000000011b
 dw 000000001100b,000000001100b,000100000000b,001000000000b,001100000000b,000000001100b
 dw 110000000000b,000000000001b,000001000001b,000001000010b,000000010000b,000000010100b
 dw 000000001100b,000100000000b,000101000000b,001001000000b,000100000000b,000000100000b
 dw 010000000000b,100000000000b,000000000001b,000000000010b,000100000000b,001000000000b
 dw 000000110000b,010000000000b,100000000000b,000000010000b,000000000001b,010000000000b
 dw 000000000001b,000000000010b,000000000100b,000000010100b,000000000100b,000000001000b
 dw 000000001100b,000000000011b,100000000000b,110000000000b,000010000000b,000000000011b
 dw 000000000001b,000000100000b,000000110000b,000000010000b,000000000100b,000000001000b
 dw 000000100000b,010000000000b,100000000000b,000000001000b,000000000010b,000000010000b
 dw 000000000011b,000000000101b,000100000100b,100000000000b,000000001000b,000001000000b
 dw 000010000000b,010000000000b,000001000000b,000010000000b,000100000000b,001000000000b
 dw 000100000000b,001000000000b,000001000001b,001000000000b,000000010000b,000100000000b
 dw 001000000000b,000000000100b,000010000000b,000100000000b,010100000000b,000001000001b
 dw 000000000100b,000100000100b,000000010000b,000000000010b,000010000000b,000000000100b
 dw 000000001000b,000100000101b,010000000000b,000101000000b,000000001000b,000100000000b
 dw 000000010000b,000100000000b,000000000010b,000000000100b,000000010000b,000000010000b
 dw 000010000000b,100000000000b,000000100100b,011000000000b,000001000010b,000100000000b
 dw 001000000000b,001100000000b,000000000011b,000000000011b,110000000000b,000000010000b
 dw 000000100000b,000000110000b,000000000100b,000100000100b,001100000000b,000100000000b
 dw 001000000000b,001100000000b,000100000000b,001000000000b,000000010000b,000000100000b
 dw 010000000000b,000010000000b,000000001100b,000010000000b,000000000011b,000000000001b
 dw 000000000010b,000000000011b,000001000000b,001000000000b,001100000000b,000001000000b
 dw 000000100000b,100000000000b,000000010000b,000000100000b,010000000000b,001000000000b
 dw 000000000100b,000000010000b,000010000000b,000000010000b,000000000101b,010000000000b
 dw 100000000000b,000000010000b,010000010000b,000000000001b,000000000001b,000000000010b
 dw 000001000000b,000010000000b,000101000000b,010000000000b,100000000000b,000000010000b
 dw 000100000000b,001000000000b,000000010000b,000000000010b,000100000000b,001000000000b
 dw 000100000000b,000100010000b,000100100000b,010000000000b,100000000000b,000000010000b
 dw 000000100000b,000000000100b,000100000100b,010000000000b,100000000000b,110000000000b
 dw 000000000100b,000100000100b,010000000000b,100000000000b,000000001000b,001000000000b
 dw 010000000000b,000000000001b,000000000010b,000000010000b,000000010000b,000101000000b
 dw 000000010000b,000000100000b,000000000001b,000000000010b,010000000000b,000001000001b
 dw 000001000010b,000000000100b,010000000000b,000000100000b,110000000000b,010000000000b
 dw 100000000000b,100000000000b,010000000000b,100000000000b,110000000000b,000001000000b
 dw 000101000000b,000000001000b,001000000000b,000100000000b,001000000000b,000100000100b
 dw 001000000000b,010001000000b,000001000000b,000010000000b,000011000000b,010000000000b
 dw 000000010000b,000000010000b,000000000001b,000000000010b,000100000000b,001000000000b
 dw 000000000100b,000000001000b,000000001100b,000100000000b,000010000000b,001100000000b
 dw 000001000000b,000010000000b,000011000000b,000011000000b,000000000011b,000010000001b
 dw 000000001100b,001100000000b,001000000100b,000000000011b,000000001100b,110000000000b
 dw 000100010100b,000000000101b

d_eggstep:
 db  20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15
 db  15, 15, 15, 20, 20, 10, 10, 20, 20, 20, 20, 20, 20, 20, 20, 10, 10, 20, 20
 db  10, 10, 15, 15, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20
 db  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 15, 15
 db  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 25
 db  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 25, 25, 20, 20, 20, 20, 20, 40, 20
 db  20, 20, 20, 20, 20, 20, 20, 25, 25, 25, 25, 25, 25, 20,  5,  5, 40, 20, 35
 db  35, 35, 35, 20, 30, 30, 30, 30, 35, 40, 80, 80, 80, 40, 40, 40,120,120, 20
 db  20, 20, 20, 20, 20, 20, 20, 20, 15, 15, 15, 15, 15, 15, 15, 15, 15, 20, 20
 db  10, 10, 10, 10, 10, 20, 20, 20, 20, 20, 20, 10, 10, 20, 20, 20, 20, 20, 20
 db  20, 20, 20, 20, 20, 35, 35, 20, 20, 25, 40, 20, 20, 20, 20, 20, 20, 25, 20
 db  20, 20, 25, 20, 25, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 25, 25, 20
 db  20, 20, 20, 20, 20, 20, 20, 25, 25, 25, 25, 25, 20, 40, 80, 80, 80, 40, 40
 db  40,120,120,120, 20, 20, 20, 20, 20, 20, 20, 20, 20, 15, 15, 15, 15, 15, 15
 db  15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 20, 20
 db  15, 15, 40, 40, 15, 15, 20, 20, 20, 20, 20, 25, 20, 20, 20, 15, 15, 20, 20
 db  20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 25
 db  25, 10, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 25, 25, 20, 15, 15, 15, 20
 db  20, 25, 20, 20, 20, 20, 20, 20, 25, 25, 25, 20, 20, 40, 35, 35, 35, 25, 15
 db  15, 30, 30, 30, 30, 20, 20, 20, 40, 40, 40, 40, 40, 40, 80, 80, 80,120,120
 db 120,120,120,120,120, 25

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
  or a						; case 0
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
  jr z,waitkey_screen2
  cp 2		; case 2 (Move)
  jr z,waitkey_move
  cp 3		; case 3 (Select)
  jr z,waitkey_select
  cp 4		; case 4 (Movelist)
  jr z,waitkey_movelist
  cp 5		; case 5 (->StatCalculator)
  jr z,waitkey_statcalc
 ;default:
  push AF
  B_CALL ZeroOP1 ; OP1 = 00000000000
  pop AF
  push AF
  LD (OP1+1),A ; OP1 = var name

  AppOnErr waitkey_failed
  B_CALL RclVarSym ; OP1(/OP2) = value
  AppOffErr
  pop AF
  B_CALL CkOP1Real ; ACC = type, Z = 1 if real
  jp nz,waitkey_failed
  B_CALL StoX
  B_JUMP Green
 
waitkey_down:
  B_CALL RclX
  B_CALL Plus1
  B_CALL StoX
  B_JUMP Green
waitkey_up:
  B_CALL RclX
  B_CALL Minus1
  B_CALL StoX
  B_JUMP Green
waitkey_screen2:
  B_JUMP PokeScreen2
waitkey_select:
  B_JUMP Select
waitkey_movelist:
  B_JUMP MoveList
waitkey_quit:
  B_JUMP JForceCmdNoChar
waitkey_failed:
  ld A,50
  ld (penCol),A
  ld A,50
  ld (penRow),A
  ld DE,OP1
  ld HL,fail
  ld BC,3
  ldir
  ld b,3
  ld hl,OP1
  B_CALL VPutSN
  ld A,58
  ld (penCol),A
  ld A,56
  ld (penRow),A
  pop AF
  B_CALL VPutMap
  jp waitkey
waitkey_move:
  B_JUMP Move
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
 db "BAD"

 public p240
 public p241,p242,p243,p244,p245,p246,p247,p248,p249,p250,p251,p252,p253,p254,p255
 
 public p256,p257,p258,p259,p260
 public p261,p262,p263,p264,p265,p266,p267,p268,p269,p270,p271,p272,p273,p274,p275,p276,p277,p278,p279,p280
 public p281,p282,p283,p284,p285,p286,p287,p288,p289,p290,p291,p292,p293,p294,p295,p296,p297,p298,p299,p300
 public p301,p302,p303,p304,p305,p306,p307,p308,p309,p310,p311,p312,p313,p314,p315,p316,p317,p318,p319,p320
 public p321,p322,p323,p324,p325,p326,p327,p328,p329,p330,p331,p332,p333,p334,p335,p336,p337,p338,p339,p340
 public p341,p342,p343,p344,p345,p346,p347,p348,p349,p350,p351,p352,p353,p354,p355,p356,p357,p358,p359,p360
 public p361,p362,p363,p364,p365,p366,p367,p368,p369,p370,p371,p372,p373,p374,p375,p376,p377,p378,p379,p380
 public p381,p382,p383,p384,p385

p240: db 22,18; Generated from in\241.raw
 db 00000010b, 00010000b, 00000000b
 db 00000101b, 00101000b, 00000000b
 db 00001101b, 11100100b, 00000000b
 db 00111111b, 11111110b, 00000000b
 db 01111111b, 11111111b, 10000000b
 db 00011011b, 10011111b, 11000000b
 db 00001100b, 10001111b, 10000000b
 db 00010000b, 10001100b, 00000000b
 db 00010110b, 00001100b, 00000000b
 db 00001000b, 00011110b, 00000000b
 db 00011100b, 00111110b, 00000000b
 db 00100111b, 11111010b, 00000000b
 db 01001011b, 11100001b, 00000000b
 db 11010100b, 00000000b, 10000000b
 db 11010000b, 00100000b, 01000000b
 db 00110000b, 00000100b, 11000000b
 db 01010100b, 00010011b, 11000000b
 db 11010000b, 00100000b, 10000000b
 db 11101000b, 00101010b, 10000000b
 db 01111100b, 00101110b, 10000000b
 db 00000011b, 11111111b, 00000000b
 db 00000000b, 00001110b, 00000000b
p241: db 22,22; Generated from in\242.raw
 db 00000001b, 11100000b, 00000000b
 db 00000010b, 01010000b, 00000000b
 db 00000100b, 01111100b, 11100000b
 db 00000101b, 10000011b, 00010000b
 db 00001010b, 00000000b, 01001000b
 db 00100100b, 00000010b, 00101000b
 db 01001001b, 00000100b, 00101000b
 db 01001000b, 00100100b, 01110000b
 db 00101000b, 10000011b, 10100000b
 db 00110000b, 00000000b, 00010000b
 db 00100011b, 00000110b, 00100000b
 db 01000100b, 10001000b, 00010000b
 db 01001000b, 01001000b, 11110000b
 db 10001000b, 01000111b, 00101000b
 db 10101000b, 01000000b, 00010100b
 db 01100100b, 10000000b, 01001100b
 db 00010011b, 00000010b, 00101000b
 db 00001100b, 00010001b, 00110000b
 db 00010011b, 00001000b, 11000000b
 db 00001110b, 11111111b, 00000000b
 db 00000000b, 00100010b, 00000000b
 db 00000000b, 00011100b, 00000000b
p242: db 22,27; Generated from in\243.raw
 db 00000000b, 00000111b, 10000000b, 00000000b
 db 00000000b, 00111000b, 01000000b, 00000000b
 db 00011011b, 11001000b, 01000000b, 00000000b
 db 00100100b, 01111100b, 11100001b, 10100000b
 db 01100100b, 11010011b, 00100000b, 01000000b
 db 01110101b, 11100111b, 01100011b, 00100000b
 db 00111111b, 11111100b, 11010000b, 11000000b
 db 01111101b, 01011111b, 11011000b, 10000000b
 db 10011011b, 01001011b, 01011001b, 00000000b
 db 10010100b, 00001010b, 00110101b, 00000000b
 db 11101001b, 11001000b, 10001010b, 00000000b
 db 11000000b, 10010000b, 10001000b, 00000000b
 db 01110011b, 11100011b, 01001000b, 00000000b
 db 00011111b, 10001011b, 01101000b, 00000000b
 db 00001111b, 10100111b, 10011000b, 00000000b
 db 00000100b, 10100110b, 10001000b, 00000000b
 db 00000110b, 01001000b, 01110000b, 00000000b
 db 00001001b, 11001000b, 00000000b, 00000000b
 db 00001000b, 01110100b, 00000000b, 00000000b
 db 00000111b, 11001100b, 00000000b, 00000000b
 db 00000000b, 10000100b, 00000000b, 00000000b
 db 00000000b, 01111000b, 00000000b, 00000000b
p243: db 22,24; Generated from in\244.raw
 db 00000000b, 00111000b, 00000000b
 db 00000011b, 10010100b, 00000000b
 db 00100101b, 00100011b, 11000000b
 db 01011001b, 00110010b, 00110000b
 db 01000000b, 11010000b, 00101000b
 db 00100000b, 00101000b, 00001010b
 db 00100001b, 11000000b, 01100101b
 db 01000001b, 01111001b, 10110001b
 db 10000000b, 00001110b, 00010110b
 db 01000000b, 00001001b, 11111000b
 db 00101100b, 00010110b, 00110000b
 db 00101101b, 10100000b, 00010000b
 db 00011101b, 10000001b, 00001000b
 db 00001010b, 10000011b, 11111000b
 db 00001000b, 11000111b, 01111000b
 db 00011111b, 01000110b, 10001000b
 db 00011111b, 00111100b, 01110000b
 db 00100010b, 00111100b, 00000000b
 db 01000010b, 01100100b, 00000000b
 db 00111100b, 10000010b, 00000000b
 db 00000000b, 10101100b, 00000000b
 db 00000000b, 01111000b, 00000000b
p244: db 22,23; Generated from in\245.raw
 db 00000000b, 01100111b, 10000000b
 db 00000000b, 10011000b, 01001000b
 db 00001111b, 00000000b, 00110100b
 db 00010011b, 10001111b, 01000100b
 db 00011100b, 01010000b, 10011100b
 db 00010011b, 01110000b, 11100010b
 db 00100110b, 01000111b, 00111010b
 db 01101100b, 10001000b, 01000100b
 db 10101001b, 00001000b, 10111000b
 db 10100110b, 10110000b, 10110000b
 db 01000001b, 11000011b, 01001000b
 db 01001110b, 00001101b, 01001000b
 db 01111000b, 00110010b, 11101000b
 db 01000011b, 10100011b, 00101000b
 db 01001111b, 11000100b, 00101000b
 db 00110101b, 00100100b, 01001000b
 db 00000101b, 00100100b, 00110000b
 db 00001001b, 00101000b, 00000000b
 db 00001001b, 00101000b, 00000000b
 db 00000110b, 01001000b, 00000000b
 db 00000000b, 01001000b, 00000000b
 db 00000000b, 00110000b, 00000000b
p245: db 20,16; Generated from in\246.raw
 db 00000011b, 00000000b
 db 00000100b, 10000000b
 db 00000101b, 10000000b
 db 00001011b, 10000000b
 db 00001011b, 10000000b
 db 00011011b, 11000000b
 db 00101000b, 01000000b
 db 01000011b, 00100000b
 db 01000101b, 00111100b
 db 10001101b, 00010010b
 db 10000111b, 00010011b
 db 01100010b, 00011101b
 db 01000000b, 00011001b
 db 00110001b, 01011001b
 db 01000011b, 01011111b
 db 01111101b, 01111111b
 db 01000000b, 11111110b
 db 11111111b, 11111100b
 db 11111111b, 11100000b
 db 00000001b, 11000000b
p246: db 19,14; Generated from in\247.raw
 db 00001000b, 00000000b
 db 01010100b, 00100000b
 db 10110100b, 11010000b
 db 11011011b, 00010000b
 db 10100110b, 00100000b
 db 10101000b, 00111000b
 db 10010110b, 00000100b
 db 10001100b, 00001000b
 db 01000111b, 01110000b
 db 01001100b, 10010000b
 db 01001001b, 10001000b
 db 01010111b, 11110000b
 db 00111000b, 10100000b
 db 00111110b, 10010000b
 db 00011001b, 00110000b
 db 00011100b, 11010000b
 db 00001111b, 00001000b
 db 00000111b, 11001000b
 db 00000001b, 11110000b
p247: db 22,23; Generated from in\248.raw
 db 00000110b, 00000000b, 00000000b
 db 00101010b, 00000000b, 00000000b
 db 01010111b, 11100000b, 00000000b
 db 01100000b, 10010000b, 00000000b
 db 01000000b, 01100000b, 00000000b
 db 10001000b, 00111000b, 00000000b
 db 10000000b, 00100100b, 00000000b
 db 01111001b, 10011000b, 00000000b
 db 00111011b, 10010010b, 00000000b
 db 00011000b, 00001101b, 00000000b
 db 00101010b, 11000101b, 00000000b
 db 01001110b, 00000110b, 00000000b
 db 10010001b, 00100011b, 10000000b
 db 10011111b, 10010010b, 10000000b
 db 01110001b, 01001111b, 00100000b
 db 00101111b, 00100111b, 11011100b
 db 00101010b, 01001111b, 00001010b
 db 00100100b, 01011001b, 00011100b
 db 00010011b, 00100011b, 01101000b
 db 00100100b, 11110111b, 10010000b
 db 00011000b, 00100110b, 00000000b
 db 00000000b, 00111000b, 00000000b
p248: db 22,31; Generated from in\249.raw
 db 00000111b, 00000000b, 00000000b, 00110100b
 db 00001010b, 10000000b, 00000000b, 01111010b
 db 00111010b, 10000000b, 00000000b, 00100100b
 db 00101000b, 10000000b, 00000000b, 01001110b
 db 00100000b, 10000001b, 11100001b, 10011100b
 db 00010000b, 01000111b, 10111110b, 00010000b
 db 00010000b, 01001101b, 00100000b, 00100000b
 db 00001000b, 00110100b, 00000100b, 01000000b
 db 00000110b, 00000000b, 00000110b, 10000000b
 db 00000001b, 00100000b, 00010001b, 10000000b
 db 00000011b, 11000000b, 10010000b, 10000000b
 db 00101100b, 10000000b, 11000001b, 10000000b
 db 00110001b, 00000000b, 00011011b, 10011000b
 db 01100000b, 10000100b, 00001111b, 01100100b
 db 01000011b, 10111011b, 00000001b, 10000100b
 db 01000111b, 00100000b, 11000000b, 00100100b
 db 11000111b, 01000000b, 00100001b, 00010100b
 db 11001101b, 01000000b, 00010000b, 10011000b
 db 10001101b, 10000000b, 00001100b, 11100000b
 db 01001011b, 00000000b, 00000011b, 00000000b
 db 01001100b, 00000000b, 00000000b, 00000000b
 db 00110000b, 00000000b, 00000000b, 00000000b
p249: db 23,31; Generated from in\250.raw
 db 00000000b, 00000000b, 00000101b, 10000000b
 db 00000000b, 11011000b, 00001010b, 01100000b
 db 00000111b, 00100111b, 10011000b, 10011000b
 db 00011001b, 00010000b, 11101001b, 00100100b
 db 00100000b, 10010000b, 00101000b, 01001100b
 db 01010110b, 00000011b, 10100000b, 00000010b
 db 01101001b, 00001000b, 00111000b, 00001100b
 db 10101000b, 11010000b, 00001100b, 00110100b
 db 10101010b, 11110000b, 00011110b, 00000010b
 db 11110101b, 11100000b, 00100010b, 00111100b
 db 00010100b, 10000010b, 00000011b, 00001000b
 db 00001101b, 00001111b, 00001100b, 11111000b
 db 00010010b, 10110000b, 10000000b, 01000000b
 db 00110100b, 11000000b, 10000000b, 11000000b
 db 01000011b, 10000000b, 01000011b, 10000000b
 db 01000110b, 00000000b, 01000000b, 01000000b
 db 10011000b, 00000000b, 01000000b, 01000000b
 db 01100000b, 00000000b, 10000010b, 01000000b
 db 00000000b, 00000000b, 10001001b, 10000000b
 db 00000000b, 00000000b, 10001001b, 00000000b
 db 00000000b, 00000000b, 10001001b, 00000000b
 db 00000000b, 00000000b, 10010110b, 00000000b
 db 00000000b, 00000000b, 01100000b, 00000000b
p250: db 19,14; Generated from in\251.raw
 db 00100000b, 10000000b
 db 01010011b, 01000000b
 db 10100101b, 00100000b
 db 10101010b, 00100000b
 db 10111010b, 00010000b
 db 01001010b, 00010000b
 db 10000100b, 00010000b
 db 11000111b, 10010000b
 db 11001100b, 10100000b
 db 11001000b, 10111000b
 db 01001000b, 11100100b
 db 00110111b, 01001000b
 db 00101110b, 10111000b
 db 01010000b, 10010000b
 db 10110000b, 10010000b
 db 01011100b, 01001000b
 db 01001010b, 11100100b
 db 00111001b, 00011000b
 db 00000110b, 00000000b
p251: db 20,19; Generated from in\252.raw
 db 00000110b, 11000000b, 00000000b
 db 00001001b, 00100000b, 00000000b
 db 00000000b, 00100000b, 00000000b
 db 00010010b, 00010000b, 00000000b
 db 00010010b, 01110000b, 00000000b
 db 00100000b, 11010000b, 00000000b
 db 00100000b, 01010000b, 00000000b
 db 01100000b, 00010000b, 00000000b
 db 10010100b, 00100000b, 00000000b
 db 01101110b, 00110000b, 00000000b
 db 00011111b, 00001111b, 00000000b
 db 00000111b, 10100000b, 11000000b
 db 00001111b, 11011001b, 00100000b
 db 00001111b, 11010110b, 00100000b
 db 00000111b, 10010000b, 00100000b
 db 00001101b, 10011000b, 01000000b
 db 00010001b, 01010111b, 10000000b
 db 00001111b, 10001000b, 00000000b
 db 00000000b, 10101000b, 00000000b
 db 00000000b, 01110000b, 00000000b
p252: db 21,24; Generated from in\253.raw
 db 00001111b, 00000000b, 00000000b
 db 00010000b, 11000000b, 00000000b
 db 00100000b, 00100000b, 00000000b
 db 00101111b, 00010000b, 00000000b
 db 01010001b, 11001000b, 00000000b
 db 01000000b, 10101000b, 00000000b
 db 01000000b, 01010100b, 00000000b
 db 10000101b, 01001010b, 00110000b
 db 10000111b, 00100110b, 01010000b
 db 10101111b, 00011000b, 10010000b
 db 01111111b, 10000100b, 10100110b
 db 00000101b, 11010011b, 00111001b
 db 00011001b, 11010101b, 01100010b
 db 00100010b, 10100101b, 11000100b
 db 00010101b, 00001000b, 10011000b
 db 00001000b, 11011000b, 11100000b
 db 00000000b, 11100100b, 10000000b
 db 00000001b, 01111000b, 10000000b
 db 00000000b, 11111101b, 00000000b
 db 00000000b, 00001000b, 10000000b
 db 00000000b, 00000111b, 00000000b
p253: db 22,24; Generated from in\254.raw
 db 01100011b, 11000000b, 00000000b
 db 10111110b, 01100000b, 00000000b
 db 10111000b, 11100000b, 00000000b
 db 11100011b, 11000000b, 00000000b
 db 11111110b, 11000000b, 00000000b
 db 11011010b, 10000000b, 00000000b
 db 10001101b, 10110000b, 00000000b
 db 01111111b, 11001000b, 00000000b
 db 00011111b, 11101000b, 00111000b
 db 00000111b, 11011000b, 11010110b
 db 00111111b, 10001011b, 01011101b
 db 01011111b, 11011111b, 01110011b
 db 10111110b, 11111111b, 11101111b
 db 01010101b, 11111111b, 10011111b
 db 00100111b, 11111111b, 11111110b
 db 00000111b, 11111111b, 11111100b
 db 00000011b, 11100111b, 11111000b
 db 00000011b, 11100111b, 11100000b
 db 00000111b, 11111110b, 00000000b
 db 00000111b, 10011110b, 00000000b
 db 00000000b, 00101010b, 00000000b
 db 00000000b, 00011100b, 00000000b
p254: db 19,13; Generated from in\255.raw
 db 00000000b, 11100000b
 db 00000111b, 00100000b
 db 00001000b, 00110000b
 db 00001000b, 00010000b
 db 00011000b, 00010000b
 db 00110100b, 01100000b
 db 01000101b, 10110000b
 db 10000000b, 00010000b
 db 10000000b, 00011000b
 db 11000000b, 00011000b
 db 11001000b, 00111000b
 db 01100111b, 11110000b
 db 00111111b, 11110000b
 db 00100111b, 10001000b
 db 00101110b, 00011000b
 db 00011011b, 00010000b
 db 00010111b, 10100000b
 db 00011001b, 01000000b
 db 00000001b, 10000000b
p255: db 22,22; Generated from in\256.raw
 db 00000010b, 00000000b, 00000000b
 db 00000101b, 00000000b, 00000000b
 db 00011001b, 10000000b, 00000000b
 db 00101010b, 10000000b, 00000000b
 db 00100000b, 11000000b, 00000000b
 db 00100001b, 00100000b, 00000000b
 db 00100010b, 00010000b, 00000000b
 db 01000000b, 00010000b, 00000000b
 db 01000000b, 10001000b, 00000000b
 db 00000001b, 00001000b, 00000000b
 db 00100000b, 00000110b, 00000000b
 db 00010001b, 00000001b, 11000000b
 db 00100000b, 00000000b, 00110000b
 db 00111110b, 00001100b, 00001000b
 db 01000011b, 00000011b, 00000100b
 db 10001110b, 00000111b, 10000100b
 db 10010100b, 00000011b, 01000100b
 db 10100101b, 00100001b, 00100100b
 db 01000011b, 11100001b, 00101000b
 db 00000011b, 11110011b, 00010000b
 db 00000000b, 00010001b, 00000000b
 db 00000000b, 00001110b, 00000000b
p256: db 22,26; Generated from in\257.raw
 db 00000010b, 01111000b, 00000000b, 00000000b
 db 00000101b, 10000110b, 00000000b, 00000000b
 db 00000101b, 00001011b, 10000000b, 00000000b
 db 00000101b, 00100110b, 01000000b, 00000000b
 db 00001011b, 01001110b, 00110000b, 00000000b
 db 00001011b, 00011110b, 00001000b, 00000000b
 db 00001010b, 00111110b, 00000110b, 00000000b
 db 00010000b, 01111111b, 00000001b, 00000000b
 db 00010000b, 11011111b, 00000000b, 10000000b
 db 01101000b, 01011001b, 10000010b, 01000000b
 db 10001001b, 11100010b, 01101001b, 01000000b
 db 10101101b, 10000100b, 00011101b, 10000000b
 db 10110011b, 00000011b, 10000110b, 00000000b
 db 01000000b, 11100010b, 11100001b, 00000000b
 db 00000000b, 11011100b, 10101101b, 00000000b
 db 00000001b, 01000001b, 10011001b, 00000000b
 db 00000001b, 00100010b, 01000110b, 00000000b
 db 00000001b, 00011100b, 00100000b, 00000000b
 db 00000000b, 10001100b, 00100000b, 00000000b
 db 00000000b, 01110010b, 00010000b, 00000000b
 db 00000000b, 00000010b, 00010000b, 00000000b
 db 00000000b, 00000001b, 11100000b, 00000000b
p257: db 17,19; Generated from in\258.raw
 db 00000001b, 10000000b, 00000000b
 db 00000010b, 01000000b, 00000000b
 db 00000100b, 01000000b, 00000000b
 db 00000100b, 01100000b, 11000000b
 db 00001100b, 01110001b, 00100000b
 db 00010010b, 01001010b, 00100000b
 db 01100010b, 00000100b, 00100000b
 db 10000000b, 00000100b, 00100000b
 db 01001000b, 00000100b, 00100000b
 db 10000000b, 10000010b, 01000000b
 db 01100000b, 00001111b, 11000000b
 db 00011100b, 00100101b, 10000000b
 db 00001111b, 11011001b, 00000000b
 db 00000100b, 00000101b, 00000000b
 db 00000100b, 11001110b, 00000000b
 db 00000011b, 11001000b, 00000000b
 db 00000000b, 00110000b, 00000000b
p258: db 20,17; Generated from in\259.raw
 db 00000011b, 11000000b, 00000000b
 db 00000101b, 11100000b, 00000000b
 db 00000101b, 11110000b, 00000000b
 db 00000101b, 11110000b, 00000000b
 db 00000111b, 11111000b, 00000000b
 db 00001011b, 11111000b, 00000000b
 db 00010011b, 10001000b, 00000000b
 db 00100000b, 00000100b, 00000000b
 db 01000000b, 00000100b, 00000000b
 db 11001000b, 00000110b, 00000000b
 db 01001000b, 10000110b, 00000000b
 db 01100000b, 10001001b, 00000000b
 db 01011000b, 00001110b, 00000000b
 db 00100111b, 10011011b, 00000000b
 db 00011100b, 00000000b, 10000000b
 db 00011100b, 00011000b, 10000000b
 db 00001010b, 00000111b, 00000000b
 db 00001001b, 11100011b, 00000000b
 db 00000110b, 00010010b, 00000000b
 db 00000000b, 00001100b, 00000000b
p259: db 22,24; Generated from in\260.raw
 db 00111000b, 00000111b, 00000000b
 db 01011100b, 00011001b, 10000000b
 db 01011110b, 00101111b, 11000000b
 db 00101111b, 11011111b, 11000000b
 db 00011100b, 01111111b, 10000000b
 db 00011000b, 11111111b, 00000000b
 db 01111000b, 11100110b, 00000110b
 db 10010000b, 11000010b, 00001011b
 db 01001000b, 01000011b, 00110111b
 db 10000000b, 10000000b, 11001111b
 db 01100010b, 00011111b, 00111111b
 db 00011000b, 00101011b, 11111111b
 db 00101111b, 11110001b, 11111111b
 db 01000011b, 00000101b, 01111111b
 db 10100111b, 10011001b, 00111110b
 db 01001001b, 10100011b, 00111110b
 db 00110010b, 01000010b, 00011100b
 db 00000010b, 01010101b, 00011000b
 db 00000010b, 00111111b, 00001000b
 db 00000001b, 11000000b, 10000100b
 db 00000000b, 00000000b, 01000100b
 db 00000000b, 00000000b, 00111100b
p260: db 17,20; Generated from in\261.raw
 db 00010000b, 00000010b, 11000000b
 db 00101001b, 00000101b, 01000000b
 db 00101110b, 10000100b, 01100000b
 db 00100010b, 01001100b, 10100000b
 db 00110100b, 01110000b, 01000000b
 db 01111111b, 00100010b, 01000000b
 db 11111100b, 10000001b, 00100000b
 db 11111010b, 11100001b, 10100000b
 db 10011111b, 10000000b, 10100000b
 db 01111110b, 11000000b, 10010000b
 db 00111111b, 11000010b, 11100000b
 db 00001111b, 10001101b, 10000000b
 db 00000101b, 10001111b, 10000000b
 db 00000111b, 10001011b, 00000000b
 db 00000011b, 11110000b, 00000000b
 db 00000000b, 11110000b, 00000000b
 db 00000000b, 01100000b, 00000000b
p261: db 21,24; Generated from in\262.raw
 db 00000000b, 00000000b, 00110011b
 db 00000000b, 00000000b, 01111111b
 db 00010000b, 01000000b, 11111111b
 db 00111000b, 10100000b, 11111111b
 db 00111001b, 10100000b, 11111110b
 db 01111111b, 01100000b, 01111100b
 db 01110010b, 11100011b, 11110000b
 db 01100110b, 11111111b, 11110000b
 db 01101100b, 01111111b, 11111000b
 db 01001010b, 00111111b, 11111000b
 db 10000100b, 10111111b, 11111100b
 db 10000001b, 10111010b, 01111000b
 db 01001100b, 10010000b, 11110000b
 db 00111000b, 00000101b, 11110000b
 db 00001111b, 00000111b, 11110000b
 db 00000111b, 00000111b, 11100000b
 db 00000111b, 10101101b, 01100000b
 db 00000011b, 11111100b, 11000000b
 db 00000001b, 01111100b, 00000000b
 db 00000001b, 11101000b, 00000000b
 db 00000000b, 00111000b, 00000000b
p262: db 16,21; Generated from in\263.raw
 db 00000000b, 00000000b, 11000000b
 db 00000000b, 00011001b, 01000000b
 db 00000001b, 10100110b, 01110000b
 db 00011010b, 01100100b, 00010000b
 db 00100100b, 00000011b, 00010000b
 db 00101100b, 11000000b, 10001000b
 db 01000011b, 00100001b, 00110000b
 db 01100000b, 00100001b, 11000000b
 db 01111010b, 01000000b, 10000000b
 db 11111010b, 00100001b, 10000000b
 db 10011110b, 01000110b, 10000000b
 db 11100000b, 00100100b, 10000000b
 db 01000010b, 11111011b, 00000000b
 db 00111111b, 10001000b, 00000000b
 db 00000000b, 10010000b, 00000000b
 db 00000000b, 01100000b, 00000000b
p263: db 20,22; Generated from in\264.raw
 db 00000000b, 00000001b, 11100000b
 db 00000000b, 00000010b, 00010000b
 db 00000000b, 00000100b, 00100000b
 db 00000000b, 00000100b, 00100000b
 db 00000000b, 00000010b, 00010000b
 db 00000100b, 00001100b, 00001000b
 db 00001010b, 01110000b, 00000100b
 db 00010011b, 10000000b, 00000100b
 db 00010000b, 00100000b, 00000100b
 db 00100000b, 01010000b, 00001000b
 db 00100000b, 10010000b, 00001000b
 db 01000000b, 00000000b, 00000100b
 db 01000000b, 00000000b, 00000100b
 db 10000000b, 00000000b, 00100100b
 db 10000110b, 00000000b, 11001000b
 db 10001010b, 00000001b, 00110000b
 db 00000000b, 01110110b, 00000000b
 db 00111111b, 11001100b, 00000000b
 db 00000110b, 10010000b, 00000000b
 db 00000000b, 01100000b, 00000000b
p264: db 16,18; Generated from in\265.raw
 db 00000011b, 00000000b, 00000000b
 db 00000100b, 10000000b, 00000000b
 db 00000100b, 10000000b, 00000000b
 db 00001000b, 01000000b, 00000000b
 db 11110000b, 01000100b, 10000000b
 db 10001000b, 01001011b, 01000000b
 db 01001000b, 00111000b, 01000000b
 db 01110000b, 00100010b, 10000000b
 db 11000010b, 00000001b, 00000000b
 db 10000010b, 00000001b, 10000000b
 db 10000000b, 00000010b, 10000000b
 db 01000001b, 00000111b, 00000000b
 db 00111110b, 00011010b, 00000000b
 db 00001010b, 01101100b, 00000000b
 db 00000111b, 10110000b, 00000000b
 db 00000000b, 11000000b, 00000000b
p265: db 17,22; Generated from in\266.raw
 db 00000000b, 01100000b, 00000000b
 db 00010000b, 10111110b, 00000000b
 db 00101111b, 11000001b, 10010000b
 db 00010000b, 00000000b, 01101000b
 db 00011000b, 00000001b, 10010000b
 db 00110000b, 00000000b, 00110000b
 db 00100000b, 00000000b, 00010000b
 db 00100000b, 00000000b, 10010000b
 db 01000000b, 00000000b, 01010000b
 db 01000000b, 10000010b, 01100000b
 db 01010001b, 10100010b, 01100000b
 db 01010001b, 00100010b, 10011000b
 db 10110000b, 11000101b, 11100100b
 db 11111000b, 00001110b, 00011100b
 db 00000111b, 10111000b, 00000000b
 db 00000001b, 01000000b, 00000000b
 db 00000001b, 10000000b, 00000000b
p266: db 21,26; Generated from in\267.raw
 db 00000000b, 00001101b, 00000000b, 00000000b
 db 00000000b, 00011111b, 00000000b, 00000000b
 db 00000000b, 00011111b, 01000000b, 00000000b
 db 00000000b, 00111111b, 11000000b, 00000000b
 db 00000000b, 11111110b, 00011111b, 10000000b
 db 00000011b, 00011110b, 01100000b, 11000000b
 db 00000100b, 00111110b, 10011111b, 01000000b
 db 00001000b, 00111101b, 01110011b, 11000000b
 db 00010000b, 00011110b, 11001011b, 10000000b
 db 00100000b, 00001101b, 10001111b, 00000000b
 db 00100000b, 00001011b, 00001111b, 00000000b
 db 00100000b, 11001111b, 00111110b, 00000000b
 db 00011000b, 10011110b, 01101100b, 00000000b
 db 00111000b, 00101100b, 10001000b, 00000000b
 db 01001111b, 11000111b, 00111000b, 00000000b
 db 11000001b, 00000111b, 11110000b, 00000000b
 db 10111001b, 10001111b, 11100001b, 11000000b
 db 10101001b, 01010010b, 00110010b, 11000000b
 db 11001000b, 11110010b, 01110111b, 10000000b
 db 01110000b, 00011101b, 11111100b, 00000000b
 db 00000000b, 00000000b, 11000000b, 00000000b
p267: db 18,22; Generated from in\268.raw
 db 00000001b, 00000001b, 10000000b
 db 00000010b, 10000110b, 10000000b
 db 00000010b, 10111001b, 00000000b
 db 01100010b, 11000010b, 11000000b
 db 01011110b, 01000000b, 00100000b
 db 00101010b, 00000000b, 00010000b
 db 00110100b, 00000000b, 00101000b
 db 00100000b, 00000000b, 10011000b
 db 00100000b, 00000100b, 01001100b
 db 01000000b, 01110100b, 01001100b
 db 01001000b, 10110010b, 01010000b
 db 01010001b, 10110010b, 01100000b
 db 01010001b, 00110010b, 10011000b
 db 10110000b, 11100101b, 11100100b
 db 11111000b, 00001110b, 00011100b
 db 00000111b, 01111000b, 00000000b
 db 00000010b, 10000000b, 00000000b
 db 00000011b, 00000000b, 00000000b
p268: db 19,26; Generated from in\269.raw
 db 00000000b, 00000011b, 00000000b, 00000000b
 db 01100000b, 00001101b, 00000000b, 00000000b
 db 01010000b, 00010001b, 00000000b, 00000000b
 db 01001011b, 10100110b, 00000000b, 00000000b
 db 01000100b, 01000010b, 00000001b, 11000000b
 db 00101000b, 01011100b, 00001110b, 01000000b
 db 11101000b, 01001100b, 11110000b, 01000000b
 db 10010000b, 00110111b, 00000000b, 01000000b
 db 10010000b, 01100010b, 01111100b, 01000000b
 db 01010000b, 00010010b, 11001111b, 10000000b
 db 01010001b, 00010001b, 01111001b, 00000000b
 db 00101000b, 10010111b, 00000000b, 10000000b
 db 00011011b, 11101111b, 01110000b, 10000000b
 db 00001101b, 10011111b, 10000001b, 00000000b
 db 00001110b, 00011111b, 11000001b, 00000000b
 db 00000111b, 10011111b, 11000010b, 00000000b
 db 00000011b, 11111111b, 11001100b, 00000000b
 db 00000000b, 01111111b, 10110000b, 00000000b
 db 00000000b, 00011111b, 11000000b, 00000000b
p269: db 15,17; Generated from in\270.raw
 db 00000011b, 11110000b, 00000000b
 db 00001100b, 00010000b, 00000000b
 db 00110011b, 11100000b, 00000000b
 db 01001100b, 01001111b, 10000000b
 db 10110000b, 11110001b, 10000000b
 db 10100000b, 00000110b, 10000000b
 db 10011000b, 00111001b, 00000000b
 db 11001111b, 11000010b, 00000000b
 db 01100000b, 00011111b, 00000000b
 db 00111111b, 11111111b, 00000000b
 db 01011101b, 01111110b, 00000000b
 db 01000110b, 11111100b, 00000000b
 db 00100000b, 01111100b, 00000000b
 db 00011111b, 11111000b, 00000000b
 db 00000000b, 01100000b, 00000000b
p270: db 20,18; Generated from in\271.raw
 db 00000001b, 11111000b, 00000000b
 db 00001110b, 00001000b, 00000000b
 db 00110000b, 00001000b, 00000000b
 db 01000000b, 00010000b, 11000000b
 db 10010000b, 00111111b, 01000000b
 db 10100000b, 01110001b, 01000000b
 db 11100000b, 00000110b, 01000000b
 db 10110000b, 00111000b, 01000000b
 db 10001111b, 11000000b, 10000000b
 db 01000000b, 00000001b, 00000000b
 db 00110000b, 00001110b, 00000000b
 db 00111111b, 11110100b, 00000000b
 db 00011000b, 00000100b, 00000000b
 db 00001111b, 10010010b, 00000000b
 db 00010011b, 11101010b, 00000000b
 db 00100011b, 00001001b, 00000000b
 db 00100101b, 10001001b, 00000000b
 db 01111111b, 01110001b, 00000000b
 db 01111001b, 11011111b, 00000000b
 db 00000000b, 00011111b, 00000000b
p271: db 22,21; Generated from in\272.raw
 db 00000000b, 01000000b, 00000000b
 db 00000001b, 10111110b, 00000000b
 db 00001110b, 00101001b, 11000000b
 db 00110011b, 00100100b, 00100000b
 db 01000010b, 11000110b, 01010000b
 db 10101010b, 00000100b, 10110000b
 db 10110001b, 00001001b, 10010000b
 db 10101001b, 11111010b, 10010000b
 db 10000110b, 10101100b, 00100000b
 db 01000100b, 01000000b, 01000000b
 db 00100000b, 00000011b, 11000000b
 db 00011111b, 11111100b, 01000000b
 db 00010000b, 01000000b, 00100000b
 db 01100000b, 00000000b, 01100000b
 db 10100111b, 00000011b, 10110000b
 db 10100001b, 11100100b, 01011000b
 db 01010001b, 10000100b, 01001000b
 db 00110000b, 00001000b, 10010000b
 db 00001000b, 00001111b, 10100000b
 db 00001100b, 00010000b, 11000000b
 db 00000111b, 00111000b, 10000000b
 db 00000000b, 11000111b, 00000000b
p272: db 17,15; Generated from in\273.raw
 db 00000011b, 10000000b
 db 00000110b, 11000000b
 db 00011010b, 10110000b
 db 00100010b, 10001000b
 db 01000000b, 00000100b
 db 01000000b, 00000100b
 db 10000000b, 00000010b
 db 10000000b, 00000010b
 db 01000000b, 00000100b
 db 01110000b, 00011100b
 db 01011111b, 11100100b
 db 00110100b, 10001000b
 db 00100011b, 00001000b
 db 00011000b, 00110000b
 db 00100111b, 11010000b
 db 00011001b, 00010000b
 db 00000000b, 11100000b
p273: db 21,16; Generated from in\274.raw
 db 00000000b, 11111000b
 db 00000011b, 00001000b
 db 00000110b, 00010000b
 db 00000111b, 11100000b
 db 00011111b, 10011000b
 db 00100111b, 00100100b
 db 00100000b, 11000100b
 db 01000000b, 00000010b
 db 01000001b, 10000010b
 db 00100010b, 00000010b
 db 01000110b, 00001110b
 db 10000000b, 00010010b
 db 01111000b, 00010001b
 db 00010111b, 10001111b
 db 00001010b, 00000001b
 db 00000100b, 00001110b
 db 00001000b, 00001000b
 db 00001000b, 00000100b
 db 00001001b, 10000100b
 db 00000110b, 01001000b
 db 00000000b, 00110000b
p274: db 22,22; Generated from in\275.raw
 db 00001000b, 00000000b, 00000000b
 db 00010100b, 11000110b, 00000000b
 db 00010111b, 00111010b, 00000000b
 db 00011100b, 00010110b, 00000000b
 db 00011000b, 00100110b, 00000000b
 db 00010000b, 00101101b, 00000000b
 db 00100000b, 00011100b, 10000000b
 db 00100000b, 00001000b, 01000000b
 db 00010000b, 00000000b, 01000000b
 db 00011100b, 10000110b, 00100000b
 db 00101110b, 00001001b, 00100000b
 db 01011101b, 00101011b, 00010000b
 db 01110110b, 10111110b, 10111000b
 db 10001011b, 11111111b, 01000100b
 db 01100101b, 00111111b, 10001000b
 db 01000101b, 00111001b, 10000100b
 db 01110101b, 11110011b, 10100100b
 db 00001011b, 11111111b, 10111100b
 db 00000011b, 11001111b, 01000000b
 db 00000000b, 10001111b, 00000000b
 db 00000000b, 00001111b, 00000000b
 db 00000000b, 00000110b, 00000000b
p275: db 14,23; Generated from in\290.raw
 db 00000000b, 01111100b, 00000000b
 db 00000011b, 10000011b, 00000000b
 db 00000101b, 00000111b, 10000000b
 db 00000111b, 10001001b, 01000000b
 db 00001000b, 01010010b, 11100000b
 db 00010000b, 00101111b, 00010000b
 db 00010000b, 10100010b, 00001000b
 db 00110001b, 00110110b, 11000100b
 db 01010000b, 10110011b, 00100010b
 db 10101000b, 01110010b, 00010010b
 db 11010101b, 10010110b, 00100100b
 db 01010101b, 00001110b, 00011000b
 db 00100101b, 00001110b, 00000000b
 db 00000010b, 00000100b, 00000000b
p276: db 19,21; Generated from in\291.raw
 db 00000001b, 00000000b, 00000000b
 db 00000010b, 10000000b, 00000000b
 db 00000010b, 10000000b, 00000000b
 db 00000101b, 10000000b, 11110000b
 db 00000100b, 10000011b, 00001000b
 db 00001101b, 10000100b, 00111000b
 db 00011011b, 11110100b, 01110000b
 db 00010111b, 11011001b, 11100000b
 db 00100001b, 10010001b, 10000000b
 db 01001100b, 00011111b, 00000000b
 db 10101000b, 00001111b, 00000000b
 db 11110000b, 11001111b, 00000000b
 db 01101111b, 01111110b, 00000000b
 db 10100011b, 11111100b, 00000000b
 db 10110111b, 11011000b, 00000000b
 db 01110000b, 01011000b, 00000000b
 db 00111000b, 00111000b, 00000000b
 db 00011000b, 00011100b, 00000000b
 db 00000000b, 00001100b, 00000000b
p277: db 19,20; Generated from in\292.raw
 db 00001100b, 00000000b, 00000000b
 db 00010010b, 00000000b, 00000000b
 db 00010010b, 00000000b, 00000000b
 db 00001001b, 00000000b, 11000000b
 db 00010111b, 11000011b, 00100000b
 db 00010000b, 00110100b, 00100000b
 db 01100000b, 01011000b, 11100000b
 db 10100000b, 00011001b, 00010000b
 db 10000000b, 00000100b, 00010000b
 db 11000000b, 11000101b, 11100000b
 db 01010001b, 00100100b, 01000000b
 db 00110011b, 10010111b, 10000000b
 db 00110000b, 01011001b, 00000000b
 db 00010001b, 00110011b, 00000000b
 db 00001111b, 11001101b, 00000000b
 db 00000101b, 11001010b, 00000000b
 db 00000011b, 11101010b, 00000000b
 db 00000000b, 01101100b, 00000000b
 db 00000000b, 00011000b, 00000000b
p278: db 14,19; Generated from in\276.raw
 db 00000000b, 00000100b, 00000000b
 db 00000000b, 00001010b, 01100000b
 db 00000000b, 00001010b, 10100000b
 db 00000000b, 00011101b, 01000000b
 db 00011110b, 00011111b, 10000000b
 db 00100001b, 11100111b, 00000000b
 db 01000000b, 11011111b, 11000000b
 db 01000000b, 11101111b, 11100000b
 db 01000101b, 11011111b, 11000000b
 db 10000101b, 11111111b, 00000000b
 db 01100001b, 11111110b, 00000000b
 db 00100000b, 01111100b, 00000000b
 db 00011000b, 00110010b, 00000000b
 db 00000111b, 11001100b, 00000000b
p279: db 18,23; Generated from in\277.raw
 db 00000000b, 00000000b, 01100000b
 db 00000000b, 00000000b, 10100000b
 db 00000000b, 00000000b, 10100110b
 db 00000000b, 11110001b, 01001010b
 db 00000011b, 11111001b, 01010100b
 db 00011111b, 11111011b, 10101000b
 db 00100000b, 11111111b, 11110000b
 db 01000000b, 01110011b, 11100000b
 db 01000010b, 01110111b, 11100000b
 db 01001100b, 11111111b, 11100000b
 db 10000100b, 11100111b, 11100000b
 db 01100001b, 11011111b, 11000000b
 db 00111111b, 01111111b, 11000000b
 db 00111110b, 01111111b, 10000000b
 db 00010000b, 00111111b, 00000000b
 db 00001100b, 00111101b, 00000000b
 db 00000011b, 11110010b, 00000000b
 db 00000000b, 00001100b, 00000000b
p280: db 15,18; Generated from in\285.raw
 db 00000001b, 11100000b, 00000000b
 db 00000010b, 00010000b, 00000000b
 db 00000110b, 00011000b, 00000000b
 db 00001000b, 00000100b, 00000000b
 db 00010000b, 00000010b, 00000000b
 db 00011000b, 00000010b, 00000000b
 db 00111001b, 11000001b, 00000000b
 db 00110000b, 10000001b, 00000000b
 db 01101100b, 01100001b, 10000000b
 db 10000010b, 01100000b, 01000000b
 db 10010000b, 00000010b, 01000000b
 db 01100000b, 00000001b, 10000000b
 db 00100111b, 00111001b, 00000000b
 db 00011111b, 00111110b, 00000000b
 db 00000000b, 11111000b, 00000000b
p281: db 22,20; Generated from in\286.raw
 db 00011111b, 00000000b, 00000000b
 db 01100000b, 11000000b, 00000000b
 db 10000000b, 00100000b, 00000000b
 db 10000000b, 00010000b, 00000000b
 db 10000000b, 00010000b, 00000000b
 db 01000001b, 10001000b, 00000000b
 db 01110010b, 11001000b, 10000000b
 db 01001111b, 11001001b, 01100000b
 db 10000011b, 11110010b, 11010000b
 db 10000110b, 01110011b, 00110000b
 db 10011000b, 11001001b, 00100000b
 db 01100110b, 00011000b, 11100000b
 db 00000101b, 00000100b, 10100000b
 db 00001000b, 00100111b, 00100000b
 db 00001001b, 00011100b, 01000000b
 db 00000111b, 00010100b, 01000000b
 db 00001010b, 11100010b, 10000000b
 db 00001010b, 01000011b, 00000000b
 db 00000101b, 11000010b, 00000000b
 db 00000111b, 01100100b, 00000000b
 db 00000000b, 01000100b, 00000000b
 db 00000000b, 00111000b, 00000000b
p282: db 21,16; Generated from in\327.raw
 db 00110000b, 00000000b
 db 01001000b, 00001100b
 db 10010100b, 00010010b
 db 10101100b, 00101001b
 db 10101100b, 00100101b
 db 01000111b, 11000101b
 db 00110000b, 01001010b
 db 00100000b, 00000100b
 db 01100000b, 00011100b
 db 01010001b, 10000100b
 db 01010010b, 00000100b
 db 01100010b, 10100100b
 db 00101001b, 00101000b
 db 00011000b, 01110000b
 db 00111111b, 11010000b
 db 00101000b, 00001000b
 db 00011000b, 00100100b
 db 00100000b, 00011000b
 db 00110000b, 11100000b
 db 00011111b, 00100000b
 db 00000001b, 11000000b
p283: db 14,26; Generated from in\278.raw
 db 00000000b, 00000001b, 10000000b, 00000000b
 db 00110000b, 00010110b, 11100000b, 00000000b
 db 01001110b, 00101011b, 00110000b, 00000000b
 db 10000001b, 11000001b, 01010000b, 00000000b
 db 10000000b, 10000000b, 10100000b, 00000000b
 db 01110001b, 00000000b, 11000000b, 00000000b
 db 00001111b, 00001001b, 11000000b, 00000000b
 db 00000001b, 00010010b, 00111100b, 00000000b
 db 00000001b, 10010010b, 00000011b, 10000000b
 db 00000001b, 00000111b, 00000000b, 01000000b
 db 00000010b, 00111001b, 11100000b, 01000000b
 db 00000111b, 01000000b, 00011100b, 01000000b
 db 00000111b, 10000000b, 00000011b, 10000000b
 db 00000011b, 00000000b, 00000000b, 00000000b
p284: db 19,20; Generated from in\279.raw
 db 00000000b, 01111000b, 00000000b
 db 00000000b, 10000100b, 00000000b
 db 00000001b, 00000010b, 00000000b
 db 00000001b, 00000100b, 00000000b
 db 00000000b, 10100100b, 00000000b
 db 00000001b, 00000010b, 00000000b
 db 00011110b, 00100001b, 11000000b
 db 01100000b, 00000000b, 00100000b
 db 10011111b, 11100001b, 00100000b
 db 01101011b, 11001001b, 00100000b
 db 00010111b, 10001000b, 10100000b
 db 00101110b, 00001000b, 11000000b
 db 00100000b, 00001000b, 01000000b
 db 00010000b, 00000100b, 01000000b
 db 00011000b, 00000100b, 00100000b
 db 00100110b, 00001100b, 00100000b
 db 00011111b, 11110010b, 10010000b
 db 00000000b, 00000010b, 10010000b
 db 00000000b, 00000001b, 11100000b
p285: db 18,23; Generated from in\283.raw
 db 00000000b, 00011000b, 00000000b
 db 00000000b, 00101000b, 00000000b
 db 00000000b, 01010000b, 00000000b
 db 00000000b, 11011000b, 00000000b
 db 00000011b, 00000110b, 00000000b
 db 00000110b, 00000011b, 00000000b
 db 00000100b, 00000011b, 00000000b
 db 00001000b, 00000111b, 10000000b
 db 00001010b, 00000111b, 11000000b
 db 00001000b, 00011111b, 11110000b
 db 00011110b, 00000111b, 11111000b
 db 00111011b, 11111111b, 00111100b
 db 01010001b, 11111111b, 10001110b
 db 10100001b, 10000001b, 11000110b
 db 11000000b, 00000001b, 01000000b
 db 00000000b, 00000000b, 10100000b
 db 00000000b, 00000000b, 10100000b
 db 00000000b, 00000000b, 01000000b
p286: db 21,26; Generated from in\284.raw
 db 00100000b, 00000000b, 00000000b, 00000000b
 db 01010000b, 00000000b, 00000000b, 11000000b
 db 01010000b, 00000100b, 00000001b, 01000000b
 db 01001000b, 00001010b, 00000010b, 01000000b
 db 10001000b, 00001010b, 00000100b, 10000000b
 db 10001100b, 00010010b, 00001000b, 10000000b
 db 10001100b, 00010100b, 00010000b, 01000000b
 db 10001110b, 00010100b, 00111000b, 01000000b
 db 10000110b, 00100100b, 01111000b, 01000000b
 db 01000001b, 01100100b, 11110000b, 01000000b
 db 01000000b, 10000010b, 10000000b, 01000000b
 db 00100001b, 00000001b, 00000000b, 10000000b
 db 00011001b, 00000010b, 00000001b, 00000000b
 db 00000110b, 01000010b, 00000110b, 00000000b
 db 00011110b, 11111001b, 11111000b, 00000000b
 db 00100010b, 11111100b, 10001100b, 00000000b
 db 01000111b, 01111001b, 01100010b, 00000000b
 db 00111000b, 11000111b, 00011100b, 00000000b
 db 00000000b, 00111000b, 10001000b, 00000000b
 db 00000000b, 00000000b, 10001000b, 00000000b
 db 00000000b, 00000000b, 01111000b, 00000000b
p287: db 16,22; Generated from in\320.raw
 db 00000000b, 11111110b, 00000000b
 db 00000011b, 00000011b, 10000000b
 db 00000110b, 01000001b, 11000000b
 db 00011110b, 00010001b, 11100000b
 db 00111111b, 00000011b, 11110000b
 db 01011111b, 11111111b, 11110000b
 db 10101111b, 11111111b, 11111000b
 db 01110001b, 11111111b, 11111000b
 db 00111100b, 00000011b, 11101000b
 db 00110111b, 11111111b, 11010100b
 db 00101010b, 11111111b, 10101100b
 db 00010110b, 10101110b, 11011000b
 db 00010011b, 11111000b, 01110000b
 db 00001001b, 00010000b, 00100000b
 db 00000111b, 00001000b, 11000000b
 db 00000001b, 11111111b, 00000000b
p288: db 21,30; Generated from in\321.raw
 db 00000000b, 00000000b, 00000000b, 01111000b
 db 00000000b, 00000000b, 00000001b, 10001000b
 db 00000000b, 00000000b, 01111110b, 00110000b
 db 00000000b, 00000111b, 11111101b, 10001000b
 db 00000000b, 00111100b, 00011011b, 11100100b
 db 00000001b, 11100000b, 00001110b, 10111100b
 db 00000111b, 00000000b, 00001100b, 11000000b
 db 00011100b, 00000000b, 00001100b, 11000000b
 db 00110000b, 00000000b, 00011000b, 10100000b
 db 01110000b, 00000000b, 00111001b, 00010000b
 db 01110000b, 00000000b, 11111110b, 00010000b
 db 11111000b, 00000011b, 11111011b, 11100000b
 db 10111110b, 00011111b, 11110000b, 10000000b
 db 10111111b, 11111111b, 11101001b, 10000000b
 db 10111111b, 11111111b, 11000111b, 00000000b
 db 01011111b, 11111111b, 01101100b, 00000000b
 db 01000111b, 11111110b, 00111000b, 00000000b
 db 00110101b, 01000110b, 01001000b, 00000000b
 db 00011010b, 10110011b, 00110000b, 00000000b
 db 00000111b, 01001111b, 11100000b, 00000000b
 db 00000001b, 11111100b, 00000000b, 00000000b
p289: db 20,20; Generated from in\300.raw
 db 00000000b, 00000000b, 10000000b
 db 00100000b, 00000011b, 01100000b
 db 01010000b, 00000101b, 11010000b
 db 01001000b, 00000011b, 10100000b
 db 01000100b, 00000010b, 01000000b
 db 01101111b, 10001111b, 00100000b
 db 01110000b, 01110000b, 10100000b
 db 01000000b, 01000001b, 10100000b
 db 01000000b, 01000111b, 00100000b
 db 10000000b, 01011110b, 00100000b
 db 10100000b, 00111110b, 01000000b
 db 10100000b, 00011001b, 10000000b
 db 01000001b, 00001110b, 00000000b
 db 00100010b, 00011100b, 00000000b
 db 00011000b, 01100100b, 00000000b
 db 00010111b, 10001100b, 00000000b
 db 00001000b, 00010100b, 00000000b
 db 00001011b, 00111000b, 00000000b
 db 00000100b, 10100000b, 00000000b
 db 00000000b, 01000000b, 00000000b
p290: db 20,23; Generated from in\301.raw
 db 00101000b, 00000000b, 00000000b
 db 01011100b, 00000001b, 01000000b
 db 01111100b, 00000010b, 10100000b
 db 11111111b, 00001110b, 01100000b
 db 01111111b, 11110001b, 11110000b
 db 00011111b, 11100111b, 11110100b
 db 00011111b, 11001111b, 11111110b
 db 00001110b, 01111111b, 10111110b
 db 00010110b, 01111111b, 00011100b
 db 00101011b, 11110110b, 00001000b
 db 00011100b, 01010001b, 10101000b
 db 00001000b, 01100110b, 01010000b
 db 00000100b, 00011110b, 00100000b
 db 00001111b, 11111100b, 00100000b
 db 00000111b, 00111110b, 00100000b
 db 00001011b, 11111000b, 01000000b
 db 00000001b, 01110110b, 01000000b
 db 00000001b, 01010101b, 01000000b
 db 00000001b, 10010101b, 10000000b
 db 00000000b, 00011000b, 00000000b
p291: db 19,20; Generated from in\352.raw
 db 00001101b, 01010000b, 00000000b
 db 00001010b, 10101000b, 00000000b
 db 00001000b, 10001100b, 00000000b
 db 00001000b, 10000010b, 00000000b
 db 00010001b, 00000100b, 00000000b
 db 00010001b, 00000010b, 00000000b
 db 00010000b, 00001100b, 00000000b
 db 00100000b, 01001001b, 11000000b
 db 00100000b, 00001010b, 00100000b
 db 00100000b, 00010100b, 10010000b
 db 00100000b, 00010101b, 01010000b
 db 00011000b, 00100101b, 01010000b
 db 01100111b, 00000100b, 10100000b
 db 10001011b, 10100111b, 00100000b
 db 11010111b, 11001000b, 11000000b
 db 00111001b, 10001111b, 00000000b
 db 00001111b, 00010100b, 00000000b
 db 00000000b, 10100100b, 00000000b
 db 00000000b, 01111000b, 00000000b
p292: db 18,16; Generated from in\343.raw
 db 00000001b, 00000000b
 db 00000010b, 10000000b
 db 00000110b, 01100000b
 db 00011010b, 01011000b
 db 00111001b, 10001100b
 db 00111100b, 00000100b
 db 01011111b, 11000010b
 db 01101111b, 11100010b
 db 01011111b, 11110010b
 db 10101000b, 01110110b
 db 10111011b, 10100100b
 db 11011100b, 00001000b
 db 00010111b, 11110100b
 db 00011110b, 01000010b
 db 00001001b, 00110001b
 db 00000111b, 01111001b
 db 00000011b, 11001001b
 db 00000001b, 00000110b
p293: db 20,18; Generated from in\344.raw
 db 00000000b, 11000000b, 00000000b
 db 00000001b, 00100000b, 00000000b
 db 00000111b, 11111000b, 00000000b
 db 00001001b, 11100100b, 00000000b
 db 00110011b, 11110011b, 00000000b
 db 00000001b, 11100000b, 00000000b
 db 10000000b, 00000000b, 01000000b
 db 11000000b, 00000010b, 01000000b
 db 10100000b, 00000001b, 11000000b
 db 10100000b, 00000011b, 01000000b
 db 11110101b, 11111101b, 11000000b
 db 01110001b, 11100011b, 10000000b
 db 01111111b, 00111111b, 00000000b
 db 01111111b, 00111111b, 00000000b
 db 01110111b, 11111100b, 10000000b
 db 00110111b, 10011100b, 10000000b
 db 00001111b, 01101111b, 10000000b
 db 00001111b, 10011111b, 00000000b
 db 00000111b, 11111000b, 00000000b
 db 00000001b, 11100000b, 00000000b
p294: db 17,14; Generated from in\299.raw
 db 00000111b, 10000000b
 db 00001000b, 01000000b
 db 00010000b, 01100000b
 db 00100000b, 11100000b
 db 00100000b, 11110000b
 db 01000000b, 01110000b
 db 01000000b, 01001000b
 db 11100001b, 11000100b
 db 10111100b, 01000100b
 db 10111111b, 11001100b
 db 01101111b, 00111100b
 db 01000111b, 00011000b
 db 01000110b, 00001000b
 db 00111100b, 00110000b
 db 00011111b, 11010000b
 db 00001110b, 00010000b
 db 00000001b, 11100000b
p295: db 17,21; Generated from in\324.raw
 db 00000000b, 01111000b, 00000000b
 db 00000001b, 11111110b, 00000000b
 db 00000111b, 11111111b, 01010000b
 db 00011001b, 11111111b, 10101000b
 db 00100000b, 11111111b, 11001000b
 db 01010001b, 00111111b, 11001000b
 db 10001110b, 00011111b, 11101000b
 db 11000000b, 00011111b, 11110000b
 db 10000100b, 00010111b, 10100000b
 db 10100011b, 00001010b, 01100000b
 db 01001000b, 00001001b, 11000000b
 db 00100000b, 00000110b, 01000000b
 db 00111111b, 00000100b, 01000000b
 db 00101100b, 00011011b, 10000000b
 db 00100111b, 00010000b, 00000000b
 db 00011001b, 00010000b, 00000000b
 db 00000000b, 11100000b, 00000000b
p296: db 20,18; Generated from in\302.raw
 db 00001000b, 00000000b, 00000000b
 db 00010100b, 00000000b, 11000000b
 db 00010111b, 11000001b, 01000000b
 db 00010110b, 01110010b, 01000000b
 db 00011000b, 00001100b, 10000000b
 db 00010000b, 00001000b, 10000000b
 db 00100000b, 00000001b, 00000000b
 db 00100000b, 00000001b, 10000000b
 db 00101000b, 11100010b, 10000000b
 db 01101001b, 00010001b, 00000000b
 db 10111001b, 00010011b, 10000000b
 db 10011001b, 00011110b, 01000000b
 db 01111000b, 11100000b, 11000000b
 db 00000111b, 11000011b, 10000000b
 db 00000000b, 01101100b, 10000000b
 db 00000000b, 01110000b, 10000000b
 db 00000000b, 10011100b, 01000000b
 db 00000000b, 01001010b, 01000000b
 db 00000000b, 00111010b, 01000000b
 db 00000000b, 00000001b, 10000000b
p297: db 17,17; Generated from in\339.raw
 db 00001000b, 00000000b, 00000000b
 db 00010000b, 01100000b, 00000000b
 db 00110111b, 10000000b, 00000000b
 db 00111011b, 11000110b, 00000000b
 db 01100110b, 00101001b, 00000000b
 db 01110100b, 00110010b, 10000000b
 db 01001110b, 01111000b, 10000000b
 db 11010000b, 11111101b, 00000000b
 db 00100111b, 11111110b, 00000000b
 db 00011111b, 11111100b, 00000000b
 db 00001001b, 11111100b, 00000000b
 db 00000010b, 11111000b, 00000000b
 db 00011001b, 01111000b, 00000000b
 db 00100110b, 01110000b, 00000000b
 db 00100000b, 11100000b, 00000000b
 db 00010011b, 10000000b, 00000000b
 db 00001100b, 00000000b, 00000000b
p298: db 22,20; Generated from in\340.raw
 db 00000000b, 00001110b, 11000000b
 db 00000000b, 00010001b, 00100000b
 db 00000000b, 00010000b, 00010000b
 db 00000000b, 00010110b, 00010000b
 db 00000000b, 11001111b, 00010000b
 db 00000001b, 00101111b, 00100000b
 db 00110001b, 01011111b, 11000000b
 db 01000011b, 00111111b, 10000000b
 db 01001111b, 11111111b, 00000000b
 db 10011010b, 10111110b, 01100000b
 db 10111000b, 00111110b, 10010000b
 db 10101010b, 01111110b, 10000000b
 db 01001111b, 10001100b, 10000000b
 db 01101111b, 10001101b, 10000000b
 db 01001111b, 10101101b, 11000000b
 db 01001111b, 10001001b, 11100000b
 db 00100011b, 11100011b, 11100000b
 db 00110000b, 00001111b, 11000000b
 db 01001110b, 00011100b, 00000000b
 db 00110001b, 11110110b, 00000000b
 db 00000000b, 00001001b, 00000000b
 db 00000000b, 00000110b, 00000000b
p299: db 17,11; Generated from in\370.raw
 db 00000011b, 10000000b
 db 00001100b, 01000000b
 db 00010000b, 01000000b
 db 00010000b, 00100000b
 db 00100000b, 00100000b
 db 00100000b, 00100000b
 db 01001000b, 01000000b
 db 01000000b, 01000000b
 db 10001000b, 10000000b
 db 10000000b, 01000000b
 db 01000000b, 01000000b
 db 01000000b, 00100000b
 db 00100000b, 00100000b
 db 00100000b, 00100000b
 db 00010000b, 01000000b
 db 00001100b, 01000000b
 db 00000011b, 10000000b
p300: db 17,19; Generated from in\341.raw
 db 00000000b, 10000000b, 00000000b
 db 00000001b, 01000000b, 00000000b
 db 00000101b, 01001100b, 00000000b
 db 00111010b, 01110100b, 00000000b
 db 01001110b, 10101000b, 00000000b
 db 10000101b, 00011001b, 11000000b
 db 10001000b, 00000110b, 00100000b
 db 01001011b, 00100100b, 00100000b
 db 01010100b, 10000100b, 00100000b
 db 00110000b, 00001000b, 01000000b
 db 00011000b, 01001000b, 01000000b
 db 00101000b, 01001100b, 10000000b
 db 00101100b, 00101111b, 10000000b
 db 00101011b, 00010010b, 10000000b
 db 00010000b, 11110010b, 10000000b
 db 00000000b, 00010101b, 00000000b
 db 00000000b, 00001000b, 00000000b
p301: db 21,22; Generated from in\342.raw
 db 00000000b, 00111000b, 00000000b
 db 00000001b, 11001000b, 00000000b
 db 00000010b, 00001000b, 00000000b
 db 00000001b, 00000100b, 00000000b
 db 00000001b, 00001000b, 00000000b
 db 00000010b, 10010100b, 00000000b
 db 00000011b, 01010100b, 00000000b
 db 00111100b, 00100010b, 00000000b
 db 01001110b, 00010010b, 01110000b
 db 10001100b, 01000111b, 10011000b
 db 10011001b, 10101111b, 00001100b
 db 10111000b, 00111010b, 10001100b
 db 01101000b, 00010010b, 11011100b
 db 00011100b, 00010110b, 01111100b
 db 00001101b, 01011111b, 00011000b
 db 00010011b, 11111111b, 11110000b
 db 00010100b, 11101111b, 11110000b
 db 00011111b, 00001101b, 11110000b
 db 00001000b, 11111101b, 01010000b
 db 00000000b, 00000111b, 11100000b
 db 00000000b, 00000011b, 00000000b
p302: db 17,18; Generated from in\349.raw
 db 00000011b, 00000000b, 00000000b
 db 00000100b, 11000001b, 00000000b
 db 00000101b, 00100010b, 10000000b
 db 00000101b, 10110010b, 10000000b
 db 00000101b, 11001101b, 11000000b
 db 00001011b, 00000011b, 01000000b
 db 00010000b, 00000010b, 10000000b
 db 00010000b, 00000001b, 10000000b
 db 00100000b, 01000000b, 01000000b
 db 00100000b, 00100001b, 11000000b
 db 01100001b, 00100001b, 10000000b
 db 10010001b, 10100001b, 00000000b
 db 01010101b, 00100111b, 00000000b
 db 00100000b, 11000000b, 10000000b
 db 00010000b, 00000111b, 00000000b
 db 00001100b, 00010001b, 00000000b
 db 00000011b, 11111110b, 00000000b
p303: db 22,24; Generated from in\350.raw
 db 00000011b, 10000000b, 00000000b
 db 00110100b, 10000000b, 00000000b
 db 00101100b, 00000000b, 00000000b
 db 00100100b, 00000110b, 00000000b
 db 00010010b, 00000010b, 00010000b
 db 00010000b, 11000010b, 00101010b
 db 00010001b, 11100110b, 00100101b
 db 00110011b, 11111100b, 01100101b
 db 00100010b, 11111000b, 01010001b
 db 00100000b, 11101000b, 01010001b
 db 01000001b, 11100111b, 00101010b
 db 01000011b, 11100100b, 11011010b
 db 00100000b, 11100110b, 00110010b
 db 01111111b, 11100010b, 00000100b
 db 01110000b, 11110010b, 00000100b
 db 11100010b, 11110011b, 00011000b
 db 11100001b, 11111010b, 11100000b
 db 01100000b, 01111100b, 00000000b
 db 00100000b, 01111111b, 00000000b
 db 00010000b, 00111111b, 10000000b
 db 00001100b, 00011111b, 10000000b
 db 00000011b, 11100111b, 00000000b
p304: db 19,19; Generated from in\318.raw
 db 00001000b, 01000001b, 00000000b
 db 00010100b, 10100011b, 00000000b
 db 00010100b, 10100110b, 00000000b
 db 00010001b, 00110100b, 00000000b
 db 00010011b, 00111100b, 00000000b
 db 00010000b, 00011100b, 00000000b
 db 00010000b, 10011110b, 00000000b
 db 01100000b, 00111110b, 00000000b
 db 10110001b, 01111100b, 11000000b
 db 01110001b, 10110000b, 00100000b
 db 00101010b, 10111000b, 11000000b
 db 00101110b, 01111111b, 00000000b
 db 00011111b, 11111110b, 00000000b
 db 00011001b, 11111110b, 00000000b
 db 00001100b, 11111110b, 00000000b
 db 00000011b, 11000011b, 00000000b
 db 00000001b, 00000001b, 00000000b
 db 00000001b, 01000000b, 00000000b
 db 00000000b, 10000000b, 00000000b
p305: db 19,19; Generated from in\319.raw
 db 00000000b, 00110000b, 00000000b
 db 00000000b, 01010000b, 00000000b
 db 00000000b, 10010000b, 00000000b
 db 00000000b, 10110000b, 00000000b
 db 00011001b, 01010000b, 00000000b
 db 00011101b, 00111000b, 00000000b
 db 00011110b, 01011100b, 00000000b
 db 00011010b, 00111110b, 00000000b
 db 00010010b, 01111111b, 00000000b
 db 00100001b, 10111111b, 00000000b
 db 01000000b, 00111111b, 00000000b
 db 01000000b, 01100001b, 00000000b
 db 11100000b, 10000011b, 10000000b
 db 11111111b, 00001100b, 01000000b
 db 10001110b, 11010000b, 00100000b
 db 01100011b, 11111111b, 11100000b
 db 00011111b, 11110000b, 00000000b
 db 00000000b, 01010000b, 00000000b
 db 00000000b, 00110000b, 00000000b
p306: db 17,18; Generated from in\328.raw
 db 00001111b, 00000000b, 00000000b
 db 00110000b, 11000000b, 00000000b
 db 01110000b, 00100000b, 00000000b
 db 01001000b, 00010000b, 00000000b
 db 10001000b, 00110000b, 00000000b
 db 10001111b, 01011000b, 00000000b
 db 10000001b, 00101000b, 00000000b
 db 10000001b, 00001100b, 00000000b
 db 01000000b, 11001010b, 00000000b
 db 01000000b, 00010001b, 00000000b
 db 00100000b, 00010001b, 10000000b
 db 00011000b, 01100000b, 01000000b
 db 00000111b, 10000010b, 01000000b
 db 00000101b, 00010011b, 10000000b
 db 00000100b, 11110001b, 00000000b
 db 00000011b, 00001001b, 00000000b
 db 00000000b, 00000110b, 00000000b
p307: db 17,22; Generated from in\329.raw
 db 00000000b, 00000000b, 11110000b
 db 00000000b, 00001111b, 11111100b
 db 00000000b, 00111111b, 10111100b
 db 00000000b, 01110011b, 10001100b
 db 00000000b, 01100011b, 01111100b
 db 00000000b, 01100111b, 11111000b
 db 00000000b, 11000111b, 10011000b
 db 00000011b, 11001110b, 00011000b
 db 00011111b, 11011000b, 00011000b
 db 01100000b, 01110000b, 01111000b
 db 10001000b, 01100001b, 11110000b
 db 11110001b, 11111111b, 11000000b
 db 00100110b, 00111111b, 00000000b
 db 00111100b, 00111011b, 10000000b
 db 00010101b, 01011101b, 10000000b
 db 00001111b, 10111110b, 00000000b
 db 00000000b, 00001110b, 00000000b
p308: db 21,22; Generated from in\330.raw
 db 00110000b, 11111110b, 00000000b
 db 01011111b, 11110000b, 11000000b
 db 01011000b, 01111111b, 00100000b
 db 01100001b, 10000001b, 10100000b
 db 01000000b, 11111110b, 00110000b
 db 11100011b, 11100100b, 01001000b
 db 10111111b, 01100011b, 11100100b
 db 11011110b, 00100100b, 01110100b
 db 01000010b, 11101000b, 00001100b
 db 00100011b, 11110000b, 00000100b
 db 00011111b, 10100000b, 00011100b
 db 00000100b, 00011110b, 01111000b
 db 00001000b, 00001111b, 11101000b
 db 00000110b, 00001001b, 10010000b
 db 00001010b, 10011000b, 00100000b
 db 00001010b, 01100100b, 11000000b
 db 00000101b, 01000111b, 00000000b
 db 00001011b, 11001100b, 00000000b
 db 00000110b, 01111000b, 00000000b
 db 00000000b, 10010000b, 00000000b
 db 00000000b, 01100000b, 00000000b
p309: db 19,18; Generated from in\296.raw
 db 00000110b, 00000000b, 00000000b
 db 00000100b, 11000000b, 00000000b
 db 00000101b, 00111100b, 00000000b
 db 00000011b, 00100100b, 00000000b
 db 00000100b, 00011000b, 00000000b
 db 00001000b, 00001100b, 00000000b
 db 01110100b, 00000100b, 00000000b
 db 10100100b, 01100010b, 00000000b
 db 10100000b, 10000010b, 00000000b
 db 11110100b, 00100011b, 00000000b
 db 01111011b, 11000100b, 10000000b
 db 00010110b, 00011011b, 10000000b
 db 00010001b, 11100100b, 11000000b
 db 00011001b, 10001100b, 11000000b
 db 00010100b, 00001111b, 11000000b
 db 00001111b, 00000111b, 10000000b
 db 00000000b, 11101111b, 00000000b
 db 00000000b, 01000100b, 00000000b
 db 00000000b, 00111000b, 00000000b
p310: db 21,22; Generated from in\297.raw
 db 00000000b, 01111000b, 00000000b
 db 00000001b, 10000100b, 00000000b
 db 00000010b, 00000100b, 00000000b
 db 00011100b, 00111110b, 00000000b
 db 00111100b, 01111111b, 00000000b
 db 01111100b, 11111111b, 00000000b
 db 11111111b, 11110011b, 00110000b
 db 11111111b, 11000100b, 11111000b
 db 01111011b, 00011000b, 01111000b
 db 00111000b, 11100011b, 11111100b
 db 00101011b, 00000111b, 11111100b
 db 01000111b, 11000111b, 11111000b
 db 01001111b, 11101111b, 11111000b
 db 00110011b, 11110011b, 11110000b
 db 00100001b, 10100001b, 11100000b
 db 00011001b, 00010000b, 01000000b
 db 00010111b, 00001000b, 11000000b
 db 00001111b, 11111111b, 10000000b
 db 00000000b, 00111111b, 00000000b
 db 00000000b, 00001001b, 00000000b
 db 00000000b, 00000110b, 00000000b
p311: db 16,19; Generated from in\309.raw
 db 00000000b, 00000000b, 10000000b
 db 00000000b, 00000001b, 01000000b
 db 00000001b, 11110001b, 01000000b
 db 00000110b, 00111011b, 11000000b
 db 00011000b, 01111111b, 11000000b
 db 00100000b, 11000101b, 11100000b
 db 01000001b, 00000100b, 11100000b
 db 10000010b, 00001011b, 11100000b
 db 10010011b, 00011111b, 11000000b
 db 01100101b, 00111111b, 11000000b
 db 01011101b, 01111111b, 11000000b
 db 10000000b, 11111111b, 10000000b
 db 10000111b, 11111100b, 00000000b
 db 01111111b, 11111100b, 00000000b
 db 00011110b, 01001000b, 00000000b
 db 00000000b, 00110000b, 00000000b
p312: db 20,19; Generated from in\310.raw
 db 00000011b, 00000000b, 00000000b
 db 00000100b, 10000110b, 00000000b
 db 00000100b, 10000101b, 00000000b
 db 00011000b, 10000010b, 10000000b
 db 00101000b, 01000010b, 10000000b
 db 00100000b, 01011100b, 01000000b
 db 00100000b, 00100010b, 01000000b
 db 01000000b, 00100001b, 11000000b
 db 01000000b, 00010001b, 10100000b
 db 01000001b, 00010010b, 00100000b
 db 01000110b, 00010010b, 00100000b
 db 10001010b, 00000100b, 00100000b
 db 10010000b, 01000100b, 00100000b
 db 01000011b, 10000100b, 01000000b
 db 00111100b, 00001011b, 11000000b
 db 00001011b, 00001010b, 01000000b
 db 00000100b, 10010001b, 10000000b
 db 00000010b, 10010000b, 00000000b
 db 00000011b, 00010000b, 00000000b
 db 00000001b, 11100000b, 00000000b
p313: db 17,18; Generated from in\322.raw
 db 00000000b, 11110000b, 00000000b
 db 00000011b, 00001100b, 00000000b
 db 00000100b, 01100010b, 00000000b
 db 00001000b, 00000001b, 00000000b
 db 00001000b, 00000001b, 00000000b
 db 01110011b, 00000000b, 10000000b
 db 10011100b, 10000000b, 10000000b
 db 10100100b, 10000000b, 01000000b
 db 01000011b, 00000000b, 01000000b
 db 11000100b, 10000000b, 01000000b
 db 10111100b, 10000001b, 01000000b
 db 10000110b, 10000001b, 01000000b
 db 11000011b, 00001010b, 01000000b
 db 01000001b, 01000111b, 10000000b
 db 01100011b, 11000100b, 00000000b
 db 01111110b, 01000100b, 00000000b
 db 00111000b, 00111000b, 00000000b
p314: db 21,28; Generated from in\323.raw
 db 00000000b, 00000000b, 00011110b, 00000000b
 db 00000000b, 00011111b, 00101101b, 00000000b
 db 00000000b, 00101110b, 10110011b, 00000000b
 db 00000000b, 00100001b, 10101111b, 00000000b
 db 00000000b, 00100001b, 11100111b, 10000000b
 db 00000000b, 01101011b, 11110111b, 10000000b
 db 00000000b, 01010101b, 01100011b, 11000000b
 db 00001100b, 10000000b, 01101111b, 11000000b
 db 00010011b, 10000000b, 01111111b, 11100000b
 db 00001111b, 01000000b, 11111111b, 11100000b
 db 01110100b, 00111000b, 11111111b, 11100000b
 db 11100000b, 00000101b, 11111111b, 11100000b
 db 01100000b, 11011011b, 11111111b, 11100000b
 db 00110001b, 00111111b, 11111111b, 11110000b
 db 00111111b, 11111111b, 11111111b, 11100000b
 db 00011111b, 10111111b, 11111111b, 11000000b
 db 00100000b, 11111111b, 11111110b, 11100000b
 db 00010010b, 11111111b, 11101111b, 11100000b
 db 00001111b, 11101100b, 11100111b, 11000000b
 db 00000111b, 11001111b, 11100000b, 00000000b
 db 00000000b, 00000111b, 11000000b, 00000000b
p315: db 16,17; Generated from in\363.raw
 db 00011000b, 00000000b, 00000000b
 db 00101111b, 10000000b, 00000000b
 db 00110000b, 11110000b, 00000000b
 db 00100000b, 10011000b, 00000000b
 db 01000000b, 01011100b, 00000000b
 db 11000010b, 00111100b, 00000000b
 db 10100110b, 00000110b, 00000000b
 db 01100010b, 00000110b, 00000000b
 db 10011100b, 00001110b, 00000000b
 db 10010100b, 00011111b, 00000000b
 db 10001000b, 01110111b, 10000000b
 db 01000000b, 01100111b, 10000000b
 db 10110000b, 00111111b, 00000000b
 db 11111110b, 00111100b, 00000000b
 db 00000111b, 00010000b, 00000000b
 db 00000000b, 11100000b, 00000000b
p316: db 18,20; Generated from in\364.raw
 db 00000111b, 11000000b, 00000000b
 db 00001100b, 01110000b, 00000000b
 db 00010000b, 00011000b, 00000000b
 db 00111000b, 10011000b, 00000000b
 db 00110010b, 01111100b, 00000000b
 db 01000100b, 11111100b, 00000000b
 db 10000010b, 01111110b, 00000000b
 db 10000010b, 00011101b, 10000000b
 db 10100010b, 00000100b, 11000000b
 db 01100001b, 00000110b, 01100000b
 db 00100001b, 00001111b, 11100000b
 db 00010000b, 10011111b, 11110000b
 db 00111000b, 01111111b, 11110000b
 db 01011100b, 00011111b, 11100000b
 db 00111111b, 00111111b, 10000000b
 db 00000011b, 11100111b, 00000000b
 db 00000000b, 11110011b, 00000000b
 db 00000000b, 00011110b, 00000000b
p317: db 21,27; Generated from in\365.raw
 db 00000000b, 11110000b, 00000000b, 00000000b
 db 00000111b, 00001100b, 00000000b, 00000000b
 db 00001000b, 10000011b, 00000000b, 00000000b
 db 00010000b, 01001000b, 10000000b, 00000000b
 db 00110001b, 01101000b, 10000111b, 00000000b
 db 01010010b, 01110000b, 01101010b, 11000000b
 db 01001000b, 11000000b, 00011010b, 10100000b
 db 10010111b, 01000000b, 00101100b, 10100000b
 db 01110101b, 00100000b, 00011001b, 01000000b
 db 00010111b, 00110000b, 00001100b, 01000000b
 db 00011000b, 10101000b, 00010010b, 10000000b
 db 00001000b, 10110010b, 01100001b, 00000000b
 db 00001000b, 01101101b, 10000011b, 00000000b
 db 00011100b, 00000000b, 00000010b, 00000000b
 db 00011100b, 00000000b, 00000110b, 00000000b
 db 00011110b, 00000000b, 11110100b, 00000000b
 db 00001111b, 00000000b, 01001000b, 00000000b
 db 00000001b, 10100000b, 00001000b, 00000000b
 db 00000000b, 01110000b, 00010000b, 00000000b
 db 00000000b, 00001100b, 01100000b, 00000000b
 db 00000000b, 00000011b, 10000000b, 00000000b
p318: db 18,26; Generated from in\331.raw
 db 00000000b, 01111100b, 00000000b, 00000000b
 db 00000000b, 10100010b, 00000000b, 00000000b
 db 00100001b, 01001001b, 10000000b, 00000000b
 db 01011001b, 11100011b, 01000000b, 00000000b
 db 10100111b, 11111110b, 01100000b, 00000000b
 db 11000010b, 11111110b, 10100000b, 00000000b
 db 11110010b, 11110001b, 10110000b, 00000000b
 db 01011100b, 11000000b, 11010000b, 00000000b
 db 00111100b, 00001100b, 00010000b, 00000000b
 db 00000110b, 00010110b, 01110000b, 00000000b
 db 00000101b, 00001010b, 10001100b, 00000000b
 db 00000011b, 01000100b, 11000011b, 00000000b
 db 00000011b, 01010100b, 10100000b, 11000000b
 db 00000011b, 10000001b, 11010001b, 01000000b
 db 00000000b, 01111111b, 10101001b, 11000000b
 db 00000000b, 00001111b, 10011100b, 01000000b
 db 00000000b, 00000011b, 00010111b, 10000000b
 db 00000000b, 00000000b, 00001111b, 00000000b
p319: db 22,15; Generated from in\332.raw
 db 00000011b, 10000000b
 db 00000110b, 11000000b
 db 00000110b, 11000000b
 db 00011101b, 11100000b
 db 01111101b, 11110000b
 db 11111011b, 11111100b
 db 01111011b, 11111110b
 db 00110111b, 11111100b
 db 00111111b, 01110000b
 db 00111111b, 01110000b
 db 00011111b, 11100000b
 db 00001111b, 11000000b
 db 00111111b, 11100000b
 db 00111010b, 00110000b
 db 01111000b, 10011100b
 db 01110100b, 11011100b
 db 00110000b, 11011100b
 db 00011001b, 11100100b
 db 00011111b, 11111000b
 db 00011111b, 11100000b
 db 00001111b, 11100000b
 db 00000001b, 11000000b
p320: db 18,14; Generated from in\361.raw
 db 00000111b, 00000000b
 db 00001000b, 10000000b
 db 00010000b, 01000000b
 db 00010100b, 01000000b
 db 00101110b, 00100000b
 db 00101110b, 00100000b
 db 00111111b, 00010000b
 db 01001111b, 10010000b
 db 01011110b, 10001000b
 db 01100001b, 10001000b
 db 10111000b, 10000100b
 db 10111110b, 00000100b
 db 10111110b, 00000100b
 db 01001110b, 00000100b
 db 00110100b, 00011000b
 db 00111111b, 11100000b
 db 00011001b, 11100000b
 db 00000000b, 11000000b
p321: db 20,19; Generated from in\362.raw
 db 00110000b, 00000000b, 00000000b
 db 00111000b, 00000000b, 00000000b
 db 00111100b, 00000000b, 00000000b
 db 00111111b, 10000000b, 00000000b
 db 00111100b, 11111000b, 11100000b
 db 01010001b, 11100111b, 11100000b
 db 01100011b, 11001111b, 11000000b
 db 01000001b, 10011111b, 11000000b
 db 01001100b, 00011111b, 10000000b
 db 11011110b, 00010111b, 11000000b
 db 10011111b, 00000011b, 11000000b
 db 11011111b, 00000111b, 11000000b
 db 11001110b, 00000011b, 11000000b
 db 10101100b, 00111011b, 11000000b
 db 01100000b, 11011011b, 10000000b
 db 01111001b, 01011011b, 10000000b
 db 00101101b, 10111011b, 00000000b
 db 00011010b, 11110110b, 00000000b
 db 00001110b, 00011100b, 00000000b
 db 00000011b, 11110000b, 00000000b
p322: db 18,13; Generated from in\337.raw
 db 00001110b, 00000000b
 db 00110001b, 10000000b
 db 01000000b, 01000000b
 db 10111000b, 00100000b
 db 11100100b, 00010000b
 db 00110010b, 00010000b
 db 00010010b, 01001000b
 db 00011101b, 11101000b
 db 00010011b, 11101000b
 db 00010001b, 11101000b
 db 00100001b, 01001000b
 db 00100111b, 00001000b
 db 00011010b, 00010000b
 db 11110100b, 00010000b
 db 10111000b, 00100000b
 db 01000000b, 01000000b
 db 00110001b, 10000000b
 db 00001110b, 00000000b
p323: db 18,19; Generated from in\338.raw
 db 00000000b, 01100000b, 00000000b
 db 00000000b, 10100000b, 00000000b
 db 00011001b, 00100000b, 00000000b
 db 00010100b, 10100001b, 10000000b
 db 00010101b, 11110010b, 10000000b
 db 00001011b, 00011100b, 10000000b
 db 01100100b, 00001101b, 00000000b
 db 10011100b, 00000110b, 00000000b
 db 01001100b, 00000110b, 00000000b
 db 00111100b, 01100111b, 10000000b
 db 00001100b, 01101000b, 01000000b
 db 00011110b, 01111110b, 00100000b
 db 00100111b, 11111101b, 11000000b
 db 01001111b, 11111100b, 00000000b
 db 01110001b, 11110010b, 00000000b
 db 00000001b, 00101001b, 00000000b
 db 00000000b, 10100111b, 00000000b
 db 00000000b, 01000000b, 00000000b
p324: db 17,22; Generated from in\298.raw
 db 00011000b, 00000000b, 00000000b
 db 00100100b, 00110000b, 00000000b
 db 00100100b, 01011000b, 00000000b
 db 00101111b, 10001100b, 00000000b
 db 00010000b, 10001100b, 00000000b
 db 00100000b, 00001100b, 00000000b
 db 01000000b, 00011000b, 11110000b
 db 01100000b, 00111001b, 11111000b
 db 10000001b, 00011011b, 11001100b
 db 10010000b, 00011011b, 11001100b
 db 10000000b, 00111011b, 11111100b
 db 01000000b, 01111111b, 11111100b
 db 11111111b, 11111101b, 11111000b
 db 11111111b, 11110101b, 11110000b
 db 01111111b, 11100110b, 01100000b
 db 00000011b, 11100000b, 00100000b
 db 00000001b, 11000000b, 00000000b
p325: db 20,13; Generated from in\325.raw
 db 00000111b, 00000000b
 db 00001000b, 10000000b
 db 00010000b, 01000000b
 db 00010000b, 01000000b
 db 00010000b, 01000000b
 db 00101000b, 10100000b
 db 00100111b, 00100000b
 db 01010000b, 00110000b
 db 01110010b, 00110000b
 db 10001100b, 00111000b
 db 10100010b, 01111000b
 db 11101011b, 11111000b
 db 11111111b, 11010000b
 db 01111111b, 11110000b
 db 00111111b, 11100000b
 db 00111111b, 11100000b
 db 00010001b, 11000000b
 db 00001111b, 10000000b
 db 00000001b, 00000000b
 db 00000110b, 00000000b
p326: db 17,19; Generated from in\326.raw
 db 00000110b, 00000000b, 00000000b
 db 00000111b, 00000000b, 00000000b
 db 00000111b, 11011111b, 00000000b
 db 00001101b, 00111111b, 00000000b
 db 00001011b, 00110010b, 00000000b
 db 00001000b, 00100100b, 00000000b
 db 00110100b, 00011011b, 10000000b
 db 01110000b, 10001100b, 01000000b
 db 11100000b, 10011110b, 00100000b
 db 10100000b, 01110111b, 00100000b
 db 01110000b, 11111111b, 00100000b
 db 00011111b, 11111110b, 01000000b
 db 00011110b, 11001111b, 10000000b
 db 00001001b, 01111111b, 00000000b
 db 00000111b, 11111111b, 00000000b
 db 00000000b, 01001110b, 00000000b
 db 00000000b, 00111100b, 00000000b
p327: db 15,19; Generated from in\311.raw
 db 00000111b, 00000000b, 00000000b
 db 00011000b, 10001111b, 00000000b
 db 00100011b, 00110000b, 10000000b
 db 01001111b, 11000000b, 10000000b
 db 10011000b, 10000011b, 00000000b
 db 10000000b, 00011100b, 00000000b
 db 01000000b, 00101000b, 11100000b
 db 00110000b, 00001011b, 00100000b
 db 00110001b, 00001100b, 01000000b
 db 00100001b, 00001011b, 00100000b
 db 00010000b, 00010101b, 11000000b
 db 00001100b, 00100010b, 00000000b
 db 00000011b, 10000110b, 00000000b
 db 00000000b, 11001010b, 00000000b
 db 00000000b, 01111100b, 00000000b
p328: db 15,19; Generated from in\312.raw
 db 00000111b, 00000000b, 00000000b
 db 00011111b, 10001111b, 00000000b
 db 00101111b, 00110000b, 10000000b
 db 01011111b, 11000111b, 10000000b
 db 10011000b, 10011111b, 00000000b
 db 10000000b, 00011100b, 00000000b
 db 01000000b, 00101000b, 00000000b
 db 00110000b, 00001011b, 00000000b
 db 00110001b, 00111100b, 11000000b
 db 00100001b, 01111011b, 11100000b
 db 00010000b, 10010101b, 11000000b
 db 00001100b, 10100010b, 00000000b
 db 00000011b, 10000110b, 00000000b
 db 00000000b, 11001010b, 00000000b
 db 00000000b, 01111100b, 00000000b
p329: db 20,24; Generated from in\303.raw
 db 00000000b, 11111000b, 00000000b
 db 00000001b, 00000110b, 00000000b
 db 00000010b, 00000001b, 00000000b
 db 00000100b, 00000000b, 11100000b
 db 00000100b, 00000100b, 01011000b
 db 00001000b, 00001110b, 01001100b
 db 00001000b, 00001111b, 00010100b
 db 00001000b, 00001110b, 10001100b
 db 00010000b, 00010100b, 00100100b
 db 00010000b, 00011110b, 00010110b
 db 00010000b, 00011101b, 11101011b
 db 00100000b, 00111010b, 00001011b
 db 00100000b, 00101010b, 00000110b
 db 01000000b, 01111010b, 00000001b
 db 01000000b, 01110100b, 00000001b
 db 01000000b, 10100010b, 00001110b
 db 10000001b, 11000011b, 11001000b
 db 10000111b, 10000000b, 01111000b
 db 10011100b, 00000000b, 00000000b
 db 01110000b, 00000000b, 00000000b
p330: db 21,15; Generated from in\307.raw
 db 00000001b, 10000000b
 db 00000010b, 01000000b
 db 00001100b, 01100000b
 db 00010000b, 00010000b
 db 00100000b, 00001000b
 db 00100000b, 00001000b
 db 01100000b, 00001000b
 db 10111000b, 00011100b
 db 10101111b, 11100010b
 db 10110000b, 10111010b
 db 01100100b, 11101010b
 db 00110100b, 00111100b
 db 01001111b, 11100000b
 db 01110100b, 11111000b
 db 00011011b, 11100100b
 db 00101100b, 00111100b
 db 00100111b, 00110000b
 db 00100100b, 11110000b
 db 00100100b, 11110000b
 db 00011100b, 11100000b
 db 00000011b, 00000000b
p331: db 22,16; Generated from in\308.raw
 db 00000000b, 01110000b
 db 00000011b, 10110000b
 db 00001101b, 11100000b
 db 00010000b, 11100000b
 db 11100000b, 11110000b
 db 10100001b, 11110000b
 db 11101011b, 11111100b
 db 00110111b, 00111100b
 db 00100000b, 10111010b
 db 00101110b, 00100101b
 db 00011110b, 01111001b
 db 11001111b, 11000010b
 db 10110010b, 10111100b
 db 10000110b, 11000000b
 db 01111000b, 00110000b
 db 00010000b, 00011000b
 db 00100101b, 00011100b
 db 00100111b, 10011100b
 db 00011111b, 11101100b
 db 00111110b, 11111100b
 db 00111000b, 01111100b
 db 00000000b, 00011000b
p332: db 14,18; Generated from in\333.raw
 db 00000011b, 01100000b, 00000000b
 db 00000000b, 10000000b, 00000000b
 db 00000000b, 10000110b, 00000000b
 db 01110011b, 11101001b, 00000000b
 db 10001100b, 10010010b, 00000000b
 db 10001000b, 00001100b, 00000000b
 db 01001100b, 00000100b, 00000000b
 db 00110001b, 00000100b, 00000000b
 db 00010000b, 00000111b, 00000000b
 db 00001110b, 00000000b, 10000000b
 db 00000011b, 00000000b, 01000000b
 db 00000000b, 11110000b, 01000000b
 db 00000000b, 00001100b, 01000000b
 db 00000000b, 00000011b, 10000000b
p333: db 20,22; Generated from in\334.raw
 db 00000001b, 11100000b, 00000000b
 db 00000010b, 11111110b, 00000000b
 db 00000101b, 00000001b, 10000000b
 db 00000111b, 01111100b, 01100000b
 db 00000101b, 10010011b, 00010000b
 db 00001000b, 00001100b, 11001000b
 db 00011000b, 00001011b, 00110100b
 db 00101000b, 00001000b, 10001100b
 db 01010000b, 10001000b, 01000000b
 db 01010000b, 00010000b, 01000000b
 db 01001111b, 10010000b, 01000000b
 db 10000001b, 00000000b, 00100000b
 db 10000000b, 10000000b, 00010000b
 db 01100000b, 00000000b, 00010000b
 db 10010000b, 00000000b, 00010000b
 db 01101000b, 00000010b, 00100000b
 db 00011000b, 01100001b, 11000000b
 db 00000100b, 10010010b, 00000000b
 db 00000011b, 01101100b, 00000000b
 db 00000000b, 00010000b, 00000000b
p334: db 19,19; Generated from in\360.raw
 db 00000111b, 00000000b, 00000000b
 db 00001000b, 11000000b, 00000000b
 db 00010000b, 00110000b, 00000000b
 db 00010000b, 00101100b, 00000000b
 db 00010000b, 00000010b, 00000000b
 db 00001000b, 00000010b, 00000000b
 db 00010111b, 00000001b, 00000000b
 db 00010000b, 00000001b, 00000000b
 db 00101000b, 01000001b, 00000000b
 db 00100000b, 10100011b, 00000000b
 db 00110100b, 00000010b, 00000000b
 db 00111110b, 10110010b, 00000000b
 db 01011111b, 11010011b, 11000000b
 db 01001111b, 10010011b, 11100000b
 db 10010111b, 00110001b, 11000000b
 db 10011000b, 00001001b, 00000000b
 db 01101110b, 00011000b, 10000000b
 db 00000111b, 11111100b, 10000000b
 db 00000000b, 11110011b, 00000000b
p335: db 18,18; Generated from in\355.raw
 db 00000000b, 11000000b, 00000000b
 db 00000011b, 11100000b, 00000000b
 db 00000111b, 11000000b, 00000000b
 db 00001111b, 11100000b, 00000000b
 db 00011011b, 11111100b, 00000000b
 db 00110001b, 11110111b, 00000000b
 db 00100000b, 11111011b, 10000000b
 db 01100000b, 11111111b, 11000000b
 db 01100110b, 01111111b, 11000000b
 db 01101111b, 01111111b, 11000000b
 db 01001101b, 01111111b, 11000000b
 db 10100110b, 01111111b, 11000000b
 db 01110000b, 11111111b, 10000000b
 db 00101111b, 11111111b, 00000000b
 db 00011100b, 11111110b, 00000000b
 db 00000111b, 11111111b, 00000000b
 db 00000001b, 11111110b, 00000000b
 db 00000000b, 11111000b, 00000000b
p336: db 22,22; Generated from in\356.raw
 db 00000000b, 11110000b, 00000000b
 db 00000001b, 00111000b, 00000000b
 db 00000001b, 00110000b, 00000000b
 db 00000010b, 11110000b, 00000000b
 db 00000011b, 00110000b, 00000000b
 db 00000101b, 11111000b, 00000000b
 db 00001000b, 00011100b, 00000000b
 db 00001100b, 00011100b, 00000000b
 db 00010000b, 01111110b, 00000000b
 db 00011000b, 00001111b, 00000000b
 db 00100000b, 11111111b, 10000000b
 db 01001000b, 00001110b, 10000000b
 db 01110001b, 00001110b, 01000000b
 db 11111111b, 00001110b, 01000000b
 db 10101111b, 11111111b, 00111000b
 db 01110111b, 11110101b, 00011100b
 db 00111111b, 11111110b, 11111000b
 db 00110111b, 11111111b, 11100000b
 db 00000111b, 11111111b, 10000000b
 db 00000011b, 11111111b, 10000000b
 db 00000000b, 00011111b, 10000000b
 db 00000000b, 00001111b, 00000000b
p337: db 21,25; Generated from in\315.raw
 db 00000000b, 00000110b, 00000000b, 00000000b
 db 00000001b, 00001010b, 00000000b, 00000000b
 db 00000010b, 10110010b, 00000000b, 00000000b
 db 00000010b, 01100010b, 00000000b, 00000000b
 db 00000010b, 10000011b, 00000000b, 00000000b
 db 00000010b, 00000010b, 10000000b, 00000000b
 db 00000010b, 00100000b, 01000000b, 00000000b
 db 00011101b, 01010001b, 11100000b, 00000000b
 db 00100110b, 10010100b, 00100000b, 00000000b
 db 01000110b, 00011010b, 01100000b, 00000000b
 db 01000011b, 00000010b, 10111100b, 00000000b
 db 10100010b, 00100001b, 01101010b, 00000000b
 db 10011001b, 00011001b, 11010011b, 00000000b
 db 01000111b, 11000011b, 10010101b, 00000000b
 db 00111101b, 00111100b, 10010101b, 10000000b
 db 00000001b, 00001111b, 10110010b, 10000000b
 db 00000001b, 00001001b, 01001000b, 10000000b
 db 00000000b, 10010001b, 11100111b, 00000000b
 db 00000000b, 10101101b, 00011110b, 00000000b
 db 00000000b, 01000101b, 00000000b, 00000000b
 db 00000000b, 00000010b, 00000000b, 00000000b
p338: db 12,22; Generated from in\287.raw
 db 00000101b, 00000111b, 11000000b
 db 00001010b, 11011011b, 01110000b
 db 01110100b, 10111101b, 10011000b
 db 10100001b, 00110110b, 00000100b
 db 11000000b, 01010000b, 00111100b
 db 11000000b, 00000000b, 11100000b
 db 10100111b, 00001000b, 00110000b
 db 11101001b, 10001000b, 00001000b
 db 01111101b, 10011111b, 10000100b
 db 00100111b, 00110000b, 11110100b
 db 00011000b, 11100000b, 00011000b
 db 00000111b, 10000000b, 00000000b
p339: db 21,19; Generated from in\288.raw
 db 00000011b, 00000000b, 00000000b
 db 00000101b, 10000000b, 00000000b
 db 00011010b, 11000000b, 00000000b
 db 00100000b, 10110000b, 00000000b
 db 01000000b, 10001001b, 01000000b
 db 01000001b, 00111001b, 11000000b
 db 01000001b, 01010110b, 01000000b
 db 10100010b, 00010110b, 01000000b
 db 10011100b, 00000100b, 01000000b
 db 11000000b, 00000000b, 10000000b
 db 01000100b, 00000000b, 10000000b
 db 00100000b, 00000001b, 01000000b
 db 00011000b, 00000010b, 10100000b
 db 00000111b, 10000001b, 00100000b
 db 00000010b, 01000001b, 01000000b
 db 00000010b, 01100000b, 10000000b
 db 00000100b, 10100000b, 10000000b
 db 00000100b, 10010010b, 10000000b
 db 00000101b, 01001100b, 01000000b
 db 00000111b, 00110010b, 01000000b
 db 00000101b, 00000001b, 10000000b
p340: db 21,28; Generated from in\289.raw
 db 00000000b, 00000000b, 00000001b, 00000000b
 db 00000000b, 00000000b, 00000010b, 10000000b
 db 00000000b, 00000000b, 00111110b, 11100000b
 db 00000000b, 00111000b, 11000001b, 00010000b
 db 00000000b, 01000111b, 00000100b, 01100000b
 db 00000000b, 10000011b, 00000010b, 01000000b
 db 00000001b, 00000100b, 11000010b, 00100000b
 db 00110111b, 00001111b, 00100001b, 00100000b
 db 01001110b, 01111110b, 11101001b, 00100000b
 db 10011010b, 00001111b, 11100001b, 00100000b
 db 10101010b, 01011001b, 11010011b, 01000000b
 db 01100111b, 01101011b, 11111111b, 10100000b
 db 00100010b, 11001001b, 11011100b, 01100000b
 db 00010010b, 00100100b, 01111011b, 00100000b
 db 00001101b, 00100011b, 00001100b, 01000000b
 db 00000011b, 00010000b, 11111000b, 00100000b
 db 00000000b, 10001110b, 00000100b, 00100000b
 db 00000000b, 01100001b, 10111010b, 00010000b
 db 00000000b, 00011100b, 01100000b, 00010000b
 db 00000000b, 00000111b, 11111100b, 00100000b
 db 00000000b, 00000000b, 00000111b, 11000000b
p341: db 16,16; Generated from in\316.raw
 db 00000000b, 01100000b
 db 00000001b, 10100000b
 db 00000010b, 00100000b
 db 00000100b, 01100000b
 db 00011000b, 01110000b
 db 00111000b, 10011000b
 db 01101011b, 00011100b
 db 01000000b, 00011100b
 db 10000000b, 00011111b
 db 10100000b, 00000111b
 db 11010010b, 00000111b
 db 10011001b, 00001110b
 db 11010000b, 00000100b
 db 01111000b, 10010100b
 db 00111111b, 11001000b
 db 00001111b, 11110000b
p342: db 16,19; Generated from in\317.raw
 db 00000001b, 11100000b, 00000000b
 db 00000110b, 00011000b, 00000000b
 db 00001100b, 00000100b, 00000000b
 db 00001000b, 00000010b, 00000000b
 db 00010000b, 00000010b, 00000000b
 db 00010000b, 00000001b, 00000000b
 db 00100010b, 00000001b, 00000000b
 db 01000100b, 00000001b, 00000000b
 db 01010010b, 00000100b, 10000000b
 db 01111000b, 00000010b, 10000000b
 db 10011100b, 00010010b, 01000000b
 db 10001000b, 01101100b, 00100000b
 db 01100000b, 11110000b, 00100000b
 db 00010000b, 00000000b, 11000000b
 db 00001100b, 11001001b, 00000000b
 db 00000011b, 00110110b, 00000000b
p343: db 22,30; Generated from in\357.raw
 db 00000000b, 00000000b, 00000111b, 00000000b
 db 00000111b, 11100000b, 00001001b, 11100000b
 db 00011000b, 00011000b, 00010010b, 00010000b
 db 00100000b, 01100100b, 00010100b, 00001000b
 db 01000000b, 10011010b, 00100100b, 01110000b
 db 01000001b, 00010111b, 00101000b, 10000000b
 db 10000011b, 00010101b, 00101001b, 11000000b
 db 10000101b, 00100100b, 10110010b, 00110000b
 db 10001100b, 11001001b, 01110100b, 00001000b
 db 10010001b, 01001110b, 01001011b, 00000100b
 db 01100111b, 11110100b, 00101111b, 11111000b
 db 00011111b, 10000110b, 00101010b, 11000000b
 db 00000000b, 00001010b, 01001010b, 01000000b
 db 00000000b, 00010001b, 10001010b, 01100000b
 db 00000000b, 00010000b, 10010100b, 00100000b
 db 00000000b, 00010000b, 11100100b, 00100000b
 db 00000000b, 00001001b, 00101000b, 10100000b
 db 00000000b, 00001101b, 00010011b, 00100000b
 db 00000000b, 00001001b, 10001111b, 11000000b
 db 00000000b, 00000111b, 10001000b, 00000000b
 db 00000000b, 00000000b, 10001000b, 00000000b
 db 00000000b, 00000000b, 01110000b, 00000000b
p344: db 16,17; Generated from in\293.raw
 db 00011100b, 00000000b, 00000000b
 db 00100010b, 00000000b, 00000000b
 db 01000101b, 00011100b, 00000000b
 db 01000101b, 11100010b, 00000000b
 db 10001000b, 01000001b, 00000000b
 db 10011000b, 01000001b, 00000000b
 db 01110000b, 01000000b, 10000000b
 db 00010000b, 00100000b, 10000000b
 db 00101000b, 00100000b, 10000000b
 db 00100000b, 10010001b, 00000000b
 db 00100110b, 00001110b, 00000000b
 db 00100011b, 00100100b, 00000000b
 db 00010000b, 01001000b, 00000000b
 db 00011100b, 00111000b, 00000000b
 db 00000011b, 11001000b, 00000000b
 db 00000000b, 01110000b, 00000000b
p345: db 21,20; Generated from in\294.raw
 db 00000110b, 00000000b, 00000000b
 db 00001011b, 00000111b, 00000000b
 db 00010001b, 10001001b, 10000000b
 db 00010101b, 11110000b, 11000000b
 db 00010001b, 00010100b, 11000000b
 db 00001010b, 00010000b, 11000000b
 db 00010000b, 00001001b, 10000000b
 db 00011001b, 10000010b, 10000000b
 db 01100010b, 01010000b, 11000000b
 db 10100000b, 10000000b, 11000000b
 db 10110000b, 00000101b, 00100000b
 db 11110100b, 00000011b, 00110000b
 db 01111111b, 11100011b, 11110000b
 db 00110000b, 00110011b, 11110000b
 db 00100000b, 00000111b, 11100000b
 db 00100000b, 00000111b, 11000000b
 db 01111100b, 00000111b, 10000000b
 db 01011111b, 11001100b, 10000000b
 db 00111111b, 11111000b, 11000000b
 db 00000000b, 00001101b, 01000000b
 db 00000000b, 00000111b, 10000000b
p346: db 22,25; Generated from in\295.raw
 db 00000000b, 11100000b, 00000000b, 00000000b
 db 00000111b, 00111000b, 00000000b, 00000000b
 db 00001010b, 01000100b, 00000000b, 00000000b
 db 00010100b, 10000110b, 00001110b, 00000000b
 db 00011001b, 00001101b, 00010001b, 00000000b
 db 00110000b, 00011000b, 10100000b, 10000000b
 db 00100010b, 01000000b, 10100000b, 10000000b
 db 01000010b, 10000001b, 11000001b, 00000000b
 db 01000000b, 10000111b, 11011010b, 00000000b
 db 00100100b, 00001111b, 11011100b, 00000000b
 db 00110011b, 00000111b, 01111100b, 00000000b
 db 01011111b, 11000011b, 10101000b, 00000000b
 db 10011111b, 11100011b, 10001000b, 00000000b
 db 11111110b, 01110111b, 10001000b, 00000000b
 db 01111100b, 00111111b, 10011000b, 00000000b
 db 00111000b, 00011101b, 00010000b, 00000000b
 db 00011000b, 00011001b, 00110000b, 00000000b
 db 00111110b, 00111101b, 11100000b, 00000000b
 db 00101111b, 11111111b, 11100000b, 00000000b
 db 00011111b, 11111100b, 00100000b, 00000000b
 db 00000000b, 00001110b, 10100000b, 00000000b
 db 00000000b, 00000111b, 11000000b, 00000000b
p347: db 17,18; Generated from in\366.raw
 db 00001111b, 11000000b, 00000000b
 db 00111001b, 11110000b, 00000000b
 db 01000111b, 11111100b, 00000000b
 db 10011100b, 00111110b, 00000000b
 db 10000000b, 11111111b, 00000000b
 db 10110111b, 11000111b, 10000000b
 db 01110001b, 00011111b, 10000000b
 db 00110110b, 00110001b, 11000000b
 db 00111110b, 11000011b, 11000000b
 db 01100101b, 11010111b, 11000000b
 db 10100100b, 01111001b, 11000000b
 db 10100100b, 10001110b, 01000000b
 db 10001100b, 10000110b, 10000000b
 db 01010011b, 11111001b, 10000000b
 db 00110000b, 00000111b, 00000000b
 db 00011111b, 00011110b, 00000000b
 db 00000111b, 11111000b, 00000000b
p348: db 22,25; Generated from in\367.raw
 db 00000000b, 00000000b, 00011000b, 00000000b
 db 00000000b, 00000000b, 00010100b, 00000000b
 db 00000000b, 00000011b, 11001110b, 00000000b
 db 00000000b, 00001101b, 01111001b, 00000000b
 db 00000000b, 00010100b, 00000000b, 10000000b
 db 00001111b, 10100010b, 11111001b, 10000000b
 db 00010000b, 01101001b, 00000111b, 00000000b
 db 00100000b, 00100001b, 00001010b, 00000000b
 db 00100000b, 00010000b, 10001100b, 00000000b
 db 00010000b, 11010100b, 10000000b, 00000000b
 db 00101110b, 00010000b, 10000000b, 00000000b
 db 01000000b, 00000001b, 00000000b, 00000000b
 db 01000000b, 10000001b, 00000000b, 00000000b
 db 01100000b, 00000010b, 00000000b, 00000000b
 db 01011111b, 00000100b, 00000000b, 00000000b
 db 00110101b, 10010100b, 00000000b, 00000000b
 db 01000011b, 10011000b, 00000000b, 00000000b
 db 10100000b, 10001000b, 00000000b, 00000000b
 db 10001110b, 00001000b, 00000000b, 00000000b
 db 01000000b, 00001000b, 00000000b, 00000000b
 db 00111110b, 00010000b, 00000000b, 00000000b
 db 00000000b, 11100000b, 00000000b, 00000000b
p349: db 20,24; Generated from in\368.raw
 db 00000000b, 00000000b, 00011100b
 db 00000000b, 01100000b, 11100010b
 db 00000001b, 10100011b, 00000001b
 db 00000010b, 01000100b, 11100001b
 db 00000100b, 10001001b, 00010010b
 db 00001001b, 00001001b, 00001100b
 db 00001001b, 00001001b, 00000000b
 db 00010010b, 00001010b, 00000000b
 db 00010101b, 11110010b, 00000000b
 db 00010110b, 00000110b, 00000000b
 db 00010100b, 00000110b, 00000000b
 db 01010000b, 00000110b, 00000000b
 db 00100001b, 10011100b, 00000000b
 db 00100010b, 00100100b, 00000000b
 db 01000100b, 00100100b, 00000000b
 db 01000100b, 00011111b, 00000000b
 db 01000000b, 11111110b, 00000000b
 db 10000111b, 11111000b, 00000000b
 db 10111000b, 00000000b, 00000000b
 db 11000000b, 00000000b, 00000000b
p350: db 22,24; Generated from in\359.raw
 db 00000110b, 00000000b, 00000000b
 db 00011100b, 00000000b, 00000111b
 db 00111011b, 10000000b, 00011110b
 db 01111100b, 01100000b, 00111100b
 db 01110000b, 00010000b, 01111000b
 db 11100100b, 00010000b, 01111000b
 db 11100100b, 00001100b, 11111000b
 db 11110000b, 10001011b, 11111000b
 db 11011111b, 01001001b, 01100000b
 db 00011101b, 01001010b, 00100000b
 db 00011111b, 11010001b, 00100000b
 db 00001111b, 11010010b, 00010000b
 db 00000111b, 01100010b, 00010000b
 db 00000100b, 00001101b, 00010000b
 db 00000010b, 00000101b, 11001000b
 db 00000010b, 00001010b, 01001000b
 db 00000101b, 00110010b, 10001000b
 db 00001001b, 11010011b, 11110000b
 db 00011111b, 00100010b, 10100000b
 db 00001010b, 01101010b, 00000000b
 db 00000000b, 01111100b, 00000000b
 db 00000000b, 00101000b, 00000000b
p351: db 18,15; Generated from in\353.raw
 db 00011000b, 00000000b
 db 00011100b, 00000000b
 db 00011111b, 11000000b
 db 00111111b, 11100000b
 db 01111111b, 11110000b
 db 11111111b, 11111000b
 db 10111111b, 11111000b
 db 10111111b, 11111000b
 db 11111100b, 01111000b
 db 01110000b, 11110000b
 db 00111111b, 11111000b
 db 00011111b, 11111100b
 db 00111111b, 11111110b
 db 01111111b, 11111110b
 db 01111111b, 11111100b
 db 00111111b, 11110000b
 db 00001111b, 11100000b
 db 00000011b, 11000000b
p352: db 18,20; Generated from in\354.raw
 db 00000110b, 00000000b, 00000000b
 db 00011111b, 11000000b, 00000000b
 db 00111111b, 11110000b, 00000000b
 db 01111111b, 11111100b, 00000000b
 db 01111111b, 11111110b, 00000000b
 db 11111111b, 11111111b, 01100000b
 db 10111111b, 11111111b, 11110000b
 db 10111110b, 11100011b, 11110000b
 db 11111110b, 11000000b, 11100000b
 db 01001111b, 11110001b, 10100000b
 db 00110011b, 11111011b, 11100000b
 db 00011111b, 11111111b, 11000000b
 db 00011111b, 11111110b, 00000000b
 db 00011111b, 11111100b, 00000000b
 db 00001011b, 11111100b, 00000000b
 db 00000011b, 11111100b, 00000000b
 db 00000011b, 00111000b, 00000000b
 db 00000000b, 00011000b, 00000000b
p353: db 19,20; Generated from in\336.raw
 db 00000000b, 00000001b, 10000000b
 db 00000000b, 00000001b, 01000000b
 db 00000111b, 10000001b, 10100000b
 db 00011111b, 11000001b, 10010000b
 db 00100111b, 11000000b, 11010000b
 db 01000101b, 11000000b, 01100000b
 db 01001001b, 10111000b, 01100000b
 db 10011011b, 11001111b, 01100000b
 db 11000101b, 10001110b, 11110000b
 db 11110001b, 10011110b, 01010000b
 db 01100011b, 11111111b, 11110000b
 db 01011110b, 01111111b, 11110000b
 db 01010100b, 11111111b, 11100000b
 db 00101000b, 11111110b, 11100000b
 db 00001011b, 11111100b, 11000000b
 db 00001111b, 10111111b, 00000000b
 db 00001111b, 00111110b, 00000000b
 db 00000111b, 11100000b, 00000000b
 db 00000011b, 11000000b, 00000000b
p354: db 20,22; Generated from in\335.raw
 db 00000110b, 00000000b, 00000000b
 db 00000101b, 00000000b, 00000000b
 db 00001001b, 00000110b, 00000000b
 db 00001011b, 10011011b, 00000000b
 db 00001000b, 01100001b, 00000000b
 db 00010000b, 00000010b, 01000000b
 db 00010000b, 11000100b, 10101000b
 db 00100011b, 00001011b, 00110100b
 db 00101001b, 00001100b, 01100100b
 db 00110100b, 00011000b, 10001000b
 db 00010000b, 00010000b, 00001000b
 db 00100000b, 00001000b, 00010000b
 db 01000000b, 01001000b, 01100000b
 db 10010000b, 01000100b, 10000000b
 db 10110000b, 01000100b, 10000000b
 db 01111000b, 00100111b, 00000000b
 db 00010110b, 00111010b, 00000000b
 db 00001111b, 11101010b, 00000000b
 db 00000001b, 00101100b, 00000000b
 db 00000000b, 11110000b, 00000000b
p355: db 21,25; Generated from in\369.raw
 db 00000000b, 00000000b, 01110000b, 00000000b
 db 00000000b, 00000000b, 11110000b, 00000000b
 db 00000000b, 01110001b, 11110000b, 00000000b
 db 00000001b, 10010001b, 11100110b, 00000000b
 db 00000010b, 00010011b, 11101111b, 00000000b
 db 00000100b, 00010111b, 11111111b, 00000000b
 db 00000100b, 00011000b, 11111010b, 10000000b
 db 00001000b, 00100011b, 11110000b, 10000000b
 db 00010000b, 01110011b, 11100011b, 00000000b
 db 00100000b, 01111111b, 11000111b, 00000000b
 db 00100000b, 00111111b, 11000011b, 10000000b
 db 01000000b, 00111111b, 11000011b, 10000000b
 db 01000000b, 00011111b, 00001111b, 00000000b
 db 11000000b, 11000111b, 00011110b, 00000000b
 db 11000010b, 00000110b, 00011111b, 00000000b
 db 11000000b, 00000111b, 10101111b, 10000000b
 db 01000100b, 00000111b, 11000111b, 00000000b
 db 01110011b, 10001111b, 11100000b, 00000000b
 db 00100000b, 01001101b, 11100000b, 00000000b
 db 00011000b, 11010000b, 11000000b, 00000000b
 db 00000111b, 00100000b, 00000000b, 00000000b
p356: db 12,16; Generated from in\304.raw
 db 00001111b, 00011000b
 db 00010000b, 11100100b
 db 00100000b, 11100110b
 db 01000110b, 01011101b
 db 01000110b, 01101101b
 db 10000000b, 01100011b
 db 10000011b, 01110101b
 db 10000110b, 11001110b
 db 01000010b, 11001000b
 db 01000101b, 11001000b
 db 00101011b, 00110000b
 db 00011110b, 00000000b
p357: db 17,19; Generated from in\305.raw
 db 00000000b, 11001100b, 00000000b
 db 00000011b, 00110010b, 00000000b
 db 00000101b, 00100010b, 00000000b
 db 00001001b, 10110001b, 00000000b
 db 00111000b, 10001000b, 10000000b
 db 01010000b, 10000011b, 10000000b
 db 01010000b, 01101111b, 11000000b
 db 10000001b, 10011111b, 11000000b
 db 11000001b, 10011111b, 11000000b
 db 11000110b, 00111111b, 11100000b
 db 10000110b, 01111111b, 11100000b
 db 10001001b, 01111111b, 11100000b
 db 10000011b, 01111110b, 10100000b
 db 01000111b, 11100011b, 11000000b
 db 01001111b, 11011110b, 00000000b
 db 00111110b, 00101010b, 00000000b
 db 00000000b, 00011100b, 00000000b
p358: db 22,22; Generated from in\306.raw
 db 00000000b, 00111110b, 00000000b
 db 00000000b, 11000001b, 00000000b
 db 00000001b, 00000001b, 00000000b
 db 00011110b, 00011001b, 00000000b
 db 01100001b, 11011001b, 10000000b
 db 10011000b, 00100010b, 01100000b
 db 01101110b, 00100010b, 00010000b
 db 00011001b, 11001100b, 01100000b
 db 00010000b, 00011111b, 11100000b
 db 00010000b, 01101011b, 11110000b
 db 00010001b, 10101011b, 11110000b
 db 00100010b, 00110111b, 11111000b
 db 00101100b, 11010100b, 01111000b
 db 00110010b, 00000100b, 01111000b
 db 00000011b, 00001011b, 00111100b
 db 00000001b, 00011111b, 01111100b
 db 00000001b, 11111101b, 10000100b
 db 00000001b, 11111111b, 00000100b
 db 00000001b, 01111111b, 01110100b
 db 00000000b, 11111111b, 11111000b
 db 00000000b, 00000111b, 10101000b
 db 00000000b, 00000000b, 11110000b
p359: db 17,12; Generated from in\351.raw
 db 00000011b, 10000000b
 db 00000100b, 10000000b
 db 00011001b, 10000000b
 db 00110010b, 11000000b
 db 01000000b, 01100000b
 db 10000000b, 01100000b
 db 10000000b, 01110000b
 db 10100000b, 11110000b
 db 11100001b, 01110000b
 db 10000011b, 01110000b
 db 01100000b, 11100000b
 db 00111111b, 11000000b
 db 00111111b, 11100000b
 db 01001111b, 11110000b
 db 01010001b, 11100000b
 db 00111000b, 11000000b
 db 00001111b, 00000000b
p360: db 20,19; Generated from in\313.raw
 db 01110000b, 00000000b, 00000000b
 db 10001000b, 00010000b, 00000000b
 db 01000111b, 11101000b, 00000000b
 db 01010100b, 10001000b, 00000000b
 db 01000000b, 10101100b, 00000000b
 db 00110000b, 00001010b, 00000000b
 db 00100000b, 00110011b, 00000000b
 db 01000000b, 01000001b, 00000000b
 db 01000000b, 00000101b, 10000000b
 db 01010000b, 00000101b, 10000000b
 db 00101100b, 00011011b, 10000000b
 db 00010001b, 00100111b, 11000000b
 db 00111100b, 01111111b, 00100000b
 db 00011111b, 11101110b, 00100000b
 db 00000100b, 01111100b, 01000000b
 db 00000010b, 00111111b, 10000000b
 db 00000010b, 00001110b, 00000000b
 db 00000011b, 10011110b, 00000000b
 db 00000010b, 01111100b, 00000000b
 db 00000000b, 00010000b, 00000000b
p361: db 20,18; Generated from in\314.raw
 db 00100000b, 00000000b, 00000000b
 db 01010000b, 00000000b, 00000000b
 db 01011111b, 10110000b, 00000000b
 db 01001000b, 01010000b, 00000000b
 db 01001000b, 10011000b, 00000000b
 db 01001100b, 10010100b, 00000000b
 db 10000001b, 00100010b, 00000000b
 db 11000000b, 01000010b, 00000000b
 db 10100000b, 00101001b, 00000000b
 db 10100001b, 10010001b, 00000000b
 db 01010010b, 10110001b, 00000000b
 db 00110010b, 11000011b, 10000000b
 db 01111100b, 11100010b, 01000000b
 db 00111111b, 11010101b, 11000000b
 db 00001000b, 11101111b, 10000000b
 db 00000100b, 01111111b, 00000000b
 db 00000101b, 00101100b, 00000000b
 db 00000111b, 10110100b, 00000000b
 db 00000100b, 11111000b, 00000000b
 db 00000000b, 00100000b, 00000000b
p362: db 20,16; Generated from in\345.raw
 db 00000001b, 10000000b
 db 00011010b, 01011000b
 db 00100110b, 01100100b
 db 01000111b, 11000010b
 db 01011100b, 11111001b
 db 00101001b, 10011111b
 db 01000111b, 00100100b
 db 10011010b, 01100010b
 db 10111110b, 01011001b
 db 11010010b, 01111111b
 db 00010010b, 10111000b
 db 00001001b, 00111000b
 db 00000100b, 01110000b
 db 00000011b, 11100000b
 db 00000001b, 01110000b
 db 00000011b, 01011000b
 db 00000100b, 00011100b
 db 00000011b, 00011100b
 db 00000000b, 10110000b
 db 00000000b, 01100000b
p363: db 22,25; Generated from in\346.raw
 db 00001111b, 00110000b, 00000000b, 00000000b
 db 00011011b, 11001000b, 00000000b, 00000000b
 db 00111110b, 01111111b, 00000000b, 00000000b
 db 01101110b, 01001000b, 11000000b, 00000000b
 db 01101001b, 11001111b, 01100000b, 00000000b
 db 00111010b, 00110010b, 10100000b, 00000000b
 db 00001111b, 00001010b, 11010000b, 00000000b
 db 00010000b, 01000111b, 11010000b, 00000000b
 db 00100000b, 00010100b, 10010000b, 00000000b
 db 01000000b, 00001100b, 11010000b, 00000000b
 db 01000000b, 00001111b, 11010000b, 00000000b
 db 01000000b, 00001001b, 10010000b, 00000000b
 db 10100000b, 00001001b, 00100000b, 00000000b
 db 10111000b, 00001110b, 00110000b, 00000000b
 db 01011111b, 00101010b, 00101000b, 00000000b
 db 01101111b, 10011010b, 00000111b, 00000000b
 db 00110000b, 00101000b, 00000010b, 10000000b
 db 00001111b, 11101000b, 01110010b, 10000000b
 db 00000000b, 00011000b, 10001011b, 00000000b
 db 00000000b, 00000110b, 10011100b, 00000000b
 db 00000000b, 00000001b, 11011100b, 00000000b
 db 00000000b, 00000000b, 00111000b, 00000000b
p364: db 20,20; Generated from in\347.raw
 db 00000011b, 00000110b, 00000000b
 db 00011100b, 10001110b, 11000000b
 db 00100010b, 01001011b, 11000000b
 db 01110001b, 01111010b, 11000000b
 db 10001101b, 11100100b, 10000000b
 db 10000011b, 00010011b, 10000000b
 db 01110100b, 11001010b, 01000000b
 db 10001111b, 00111011b, 00100000b
 db 10111111b, 11001000b, 10010000b
 db 10111011b, 11110110b, 01110000b
 db 01110010b, 01101001b, 00100000b
 db 01010110b, 01011000b, 11100000b
 db 01001000b, 11000100b, 01000000b
 db 01011111b, 01011110b, 01000000b
 db 01110010b, 01011101b, 11000000b
 db 01110010b, 01111000b, 00000000b
 db 00100010b, 11000000b, 00000000b
 db 00000011b, 10000000b, 00000000b
 db 00000011b, 10000000b, 00000000b
 db 00000001b, 00000000b, 00000000b
p365: db 22,23; Generated from in\348.raw
 db 01100000b, 00000000b, 00000000b
 db 10010000b, 00000000b, 00000000b
 db 10101111b, 00000000b, 00000000b
 db 10111111b, 11011100b, 00000000b
 db 01100111b, 11100010b, 00000000b
 db 01001100b, 11010110b, 00000000b
 db 01011001b, 10010100b, 00000000b
 db 01011110b, 00111110b, 00000000b
 db 00100000b, 11001001b, 00001000b
 db 00011111b, 01101111b, 00010100b
 db 00000101b, 11100100b, 10100100b
 db 00000101b, 11110111b, 11001110b
 db 00011101b, 11000010b, 01001110b
 db 00100110b, 01110111b, 11111000b
 db 01011110b, 10011111b, 01110000b
 db 01011111b, 01111111b, 10100000b
 db 01101101b, 01111100b, 11010000b
 db 00001001b, 11111000b, 01110000b
 db 00010111b, 11111000b, 01000000b
 db 00011111b, 01111100b, 10000000b
 db 00000000b, 00001011b, 01000000b
 db 00000000b, 00001111b, 11000000b
p366: db 19,12; Generated from in\280.raw
 db 01100000b, 00000000b
 db 11111000b, 00000000b
 db 11111111b, 00000000b
 db 01111100b, 11000000b
 db 01111000b, 00100000b
 db 10110000b, 00100000b
 db 10010000b, 00010000b
 db 10000000b, 00010000b
 db 01000000b, 00010000b
 db 01010000b, 00100000b
 db 00100111b, 11000000b
 db 00010000b, 10000000b
 db 00001111b, 00000000b
 db 00010001b, 00000000b
 db 00010000b, 10000000b
 db 00100010b, 11000000b
 db 00101001b, 00100000b
 db 00011000b, 11000000b
 db 00000111b, 00000000b
p367: db 21,18; Generated from in\281.raw
 db 00011001b, 11100000b, 00000000b
 db 00011110b, 11111100b, 00000000b
 db 00011001b, 00010110b, 00000000b
 db 00010010b, 00101110b, 00000000b
 db 00010100b, 00101110b, 00000000b
 db 00101000b, 00010001b, 00000000b
 db 00101000b, 00010101b, 00000000b
 db 01010100b, 00110100b, 10000000b
 db 10010100b, 11010010b, 10000000b
 db 10101011b, 01001010b, 10000000b
 db 01001110b, 00011010b, 01000000b
 db 00110001b, 11101001b, 01000000b
 db 00000100b, 00011001b, 10000000b
 db 00000011b, 00001111b, 00000000b
 db 00010000b, 01100100b, 00000000b
 db 00010001b, 00011000b, 00000000b
 db 00001101b, 00000010b, 00000000b
 db 00000010b, 10000001b, 00000000b
 db 00000001b, 11001110b, 00000000b
 db 00000000b, 10111000b, 00000000b
 db 00000000b, 01000000b, 00000000b
p368: db 22,24; Generated from in\282.raw
 db 00000011b, 11000000b, 00000000b
 db 00001100b, 00110000b, 00000000b
 db 00010000b, 00001000b, 00000000b
 db 00100000b, 00000100b, 00000000b
 db 00100000b, 00001100b, 00000000b
 db 01000000b, 00010110b, 00000000b
 db 01000000b, 00101011b, 11100000b
 db 00100000b, 01001001b, 00010000b
 db 01100001b, 10010001b, 00001100b
 db 10010010b, 10000001b, 00110010b
 db 10001100b, 00110001b, 11000001b
 db 01100011b, 11100010b, 01110001b
 db 00011110b, 01001100b, 00001001b
 db 00001001b, 11110111b, 00010001b
 db 00010000b, 11100000b, 11100001b
 db 00010010b, 00000000b, 00110010b
 db 00010100b, 00100000b, 00001100b
 db 00001000b, 01100000b, 00000100b
 db 00001000b, 01110000b, 00000100b
 db 00000100b, 11010000b, 00001000b
 db 00000010b, 11001000b, 00110000b
 db 00000001b, 10000111b, 11000000b
p369: db 18,16; Generated from in\371.raw
 db 00001111b, 00000000b
 db 01110001b, 11000000b
 db 10100011b, 00100000b
 db 10100100b, 00010000b
 db 01101001b, 11010000b
 db 01111011b, 01110000b
 db 10000111b, 01011000b
 db 10000010b, 01010110b
 db 11000111b, 11111011b
 db 01111101b, 11111111b
 db 00110001b, 11101110b
 db 00111111b, 11100110b
 db 00010100b, 01001001b
 db 00010100b, 00110011b
 db 00011111b, 00011111b
 db 00001110b, 11111111b
 db 00000000b, 00010101b
 db 00000000b, 00001110b
p370: db 21,20; Generated from in\372.raw
 db 00000000b, 01111000b, 00000000b
 db 00000011b, 10000100b, 00000000b
 db 00001100b, 00011111b, 00000000b
 db 00011000b, 01100000b, 10000000b
 db 00100000b, 10000001b, 10000000b
 db 01000001b, 00000000b, 01000000b
 db 01000010b, 00000000b, 00100000b
 db 10000000b, 00000000b, 00100000b
 db 10000000b, 00000000b, 01010000b
 db 10001110b, 00000011b, 00010000b
 db 01011111b, 10001000b, 00010000b
 db 10101011b, 10000000b, 00100000b
 db 10010111b, 01000000b, 11100000b
 db 01011111b, 01111100b, 00100000b
 db 01001111b, 10000000b, 00100000b
 db 00110111b, 00000000b, 01100000b
 db 00101000b, 01100000b, 11100000b
 db 00100110b, 00011111b, 11100000b
 db 00011101b, 11111101b, 11000000b
 db 00000000b, 01001100b, 00000000b
 db 00000000b, 00111000b, 00000000b
p371: db 22,30; Generated from in\373.raw
 db 00000000b, 00001111b, 10000000b, 00000000b
 db 00110000b, 01100000b, 01111110b, 00000000b
 db 00101000b, 10000000b, 11000001b, 10000000b
 db 00010101b, 00000001b, 00111110b, 01000000b
 db 00010011b, 01110000b, 11111111b, 10100000b
 db 00001101b, 10010111b, 00011111b, 11110000b
 db 00110010b, 00011000b, 00111111b, 11110000b
 db 01000110b, 00000000b, 11011111b, 11111000b
 db 10000010b, 10000111b, 01101111b, 11111000b
 db 01100000b, 00000000b, 11010000b, 11111000b
 db 00011110b, 00001111b, 01000000b, 00110100b
 db 00001001b, 11000100b, 00100000b, 11110100b
 db 00000100b, 00110100b, 00000001b, 00010100b
 db 00000100b, 01111100b, 00000001b, 00001000b
 db 00000011b, 11011110b, 00111001b, 00001000b
 db 00000000b, 01010011b, 01000100b, 10000100b
 db 00000000b, 10001000b, 10000011b, 11000100b
 db 00000000b, 10000110b, 10000010b, 01111000b
 db 00000000b, 01111111b, 10000010b, 00000000b
 db 00000000b, 00000000b, 01000010b, 00000000b
 db 00000000b, 00000000b, 00101010b, 00000000b
 db 00000000b, 00000000b, 00010100b, 00000000b
p372: db 20,22; Generated from in\374.raw
 db 00000111b, 00000000b, 00000000b
 db 00011001b, 10000000b, 00000000b
 db 01100001b, 01000000b, 00000000b
 db 10000110b, 00100000b, 00000000b
 db 10011111b, 00011000b, 00000000b
 db 01111111b, 10000100b, 00000000b
 db 00110000b, 11000010b, 00000000b
 db 01000000b, 00100001b, 00000000b
 db 01000000b, 00110001b, 00000000b
 db 10000000b, 00011000b, 10000000b
 db 10011100b, 00011100b, 11100000b
 db 10101110b, 00011100b, 01010000b
 db 11111110b, 00111110b, 01001000b
 db 01111111b, 01111110b, 01001000b
 db 01111111b, 11111110b, 10001000b
 db 00111111b, 11011111b, 00001000b
 db 00001111b, 00101110b, 00010100b
 db 00000000b, 00100000b, 00111000b
 db 00000000b, 00010000b, 11000000b
 db 00000000b, 00001111b, 00000000b
p373: db 21,30; Generated from in\375.raw
 db 00000000b, 01111111b, 11100000b, 00000000b
 db 00000000b, 10011000b, 00011000b, 00000000b
 db 00000001b, 01100000b, 00000100b, 00000000b
 db 00000001b, 10000000b, 00000010b, 00000000b
 db 00000001b, 10000000b, 00000011b, 00000000b
 db 00000011b, 00001000b, 00000001b, 00000000b
 db 00000101b, 11001000b, 00000000b, 11110000b
 db 00011100b, 11010000b, 00000000b, 11001000b
 db 00110100b, 10110000b, 00000000b, 10111100b
 db 01011010b, 10010001b, 11000001b, 00010100b
 db 10001010b, 10001011b, 11100110b, 00011000b
 db 10011111b, 01001111b, 11111111b, 00101000b
 db 10111101b, 11110001b, 11111000b, 11001000b
 db 01010101b, 11111001b, 10010000b, 00010000b
 db 00100110b, 01111111b, 10001000b, 00100000b
 db 00011100b, 00111100b, 11001000b, 00100000b
 db 00000000b, 00001000b, 01000100b, 01000000b
 db 00000000b, 00001001b, 11100101b, 10000000b
 db 00000000b, 00001010b, 10100110b, 00000000b
 db 00000000b, 00000101b, 00101100b, 00000000b
 db 00000000b, 00000000b, 11110000b, 00000000b
p374: db 22,31; Generated from in\376.raw
 db 00000000b, 00000000b, 10000000b, 00000000b
 db 00000000b, 00000001b, 01000000b, 00000000b
 db 00000000b, 00000010b, 11000000b, 10000000b
 db 00000001b, 10000011b, 11000001b, 01000000b
 db 00000010b, 11000111b, 11110001b, 00100000b
 db 00000100b, 11111001b, 00111111b, 00100000b
 db 00001000b, 11100011b, 00011111b, 10010000b
 db 00011000b, 10111100b, 10001111b, 10010000b
 db 00111101b, 10110000b, 01111110b, 11001000b
 db 00100011b, 10010011b, 10110010b, 11001000b
 db 01100011b, 11001100b, 00010010b, 11001100b
 db 01000111b, 11000000b, 00110010b, 11110100b
 db 01000110b, 11100011b, 11100011b, 11000100b
 db 11100110b, 11000111b, 11100111b, 11000010b
 db 10101100b, 01111111b, 11100011b, 01000110b
 db 11010100b, 00111111b, 11111111b, 01111110b
 db 00011000b, 00001111b, 11110011b, 00111010b
 db 00000000b, 00000000b, 01100011b, 00010100b
 db 00000000b, 00000000b, 01100011b, 00001000b
 db 00000000b, 00000000b, 01100110b, 00000000b
 db 00000000b, 00000000b, 01011010b, 00000000b
 db 00000000b, 00000000b, 00100100b, 00000000b
p375: db 21,20; Generated from in\377.raw
 db 00000111b, 10000000b, 00000000b
 db 01101001b, 01000000b, 00000000b
 db 10010000b, 10100110b, 00000000b
 db 10010000b, 10111001b, 11000000b
 db 10100000b, 01001101b, 00100000b
 db 01100000b, 01000010b, 00100000b
 db 01000000b, 00110101b, 01010000b
 db 01000000b, 00001100b, 10010000b
 db 01100000b, 00011100b, 10010000b
 db 01011000b, 01100110b, 10100000b
 db 00100111b, 10000101b, 11100000b
 db 01010100b, 01000010b, 10100000b
 db 01001110b, 00111010b, 01000000b
 db 01101001b, 01000111b, 01000000b
 db 10011111b, 01001000b, 10000000b
 db 10001011b, 11110001b, 00000000b
 db 01111000b, 10010001b, 00000000b
 db 00001000b, 11001110b, 00000000b
 db 00000111b, 01110100b, 00000000b
 db 00000000b, 01000100b, 00000000b
 db 00000000b, 00111000b, 00000000b
p376: db 21,19; Generated from in\378.raw
 db 00000110b, 11000000b, 00000000b
 db 00001011b, 00100000b, 00000000b
 db 00001110b, 00110011b, 00000000b
 db 00000100b, 00111100b, 10000000b
 db 00001100b, 00011001b, 10000000b
 db 00001100b, 00011111b, 11000000b
 db 00110011b, 11111111b, 00100000b
 db 01010010b, 00111110b, 01100000b
 db 10110100b, 00111110b, 11000000b
 db 01110100b, 00110011b, 10000000b
 db 10100100b, 00111001b, 10000000b
 db 01101000b, 00111100b, 10000000b
 db 01111000b, 00011111b, 01000000b
 db 01011000b, 00011110b, 10000000b
 db 00111111b, 00011111b, 00000000b
 db 00011100b, 11111110b, 00000000b
 db 00011111b, 00111100b, 00000000b
 db 00011111b, 11111000b, 00000000b
 db 00001101b, 00111000b, 00000000b
 db 00000000b, 10110000b, 00000000b
 db 00000000b, 01100000b, 00000000b
p377: db 21,23; Generated from in\379.raw
 db 00000000b, 01110000b, 00000000b
 db 00000001b, 10001001b, 10000000b
 db 00000010b, 00111110b, 01000000b
 db 00001100b, 01111100b, 00000000b
 db 00010100b, 11111000b, 00100000b
 db 00101111b, 11111000b, 01111000b
 db 00111001b, 11110000b, 11001100b
 db 00111001b, 01111111b, 11001100b
 db 00011001b, 10100000b, 11111100b
 db 00001101b, 01100000b, 11111100b
 db 00000111b, 10100001b, 11111100b
 db 00001111b, 11111111b, 11111100b
 db 00111111b, 11111111b, 11011110b
 db 01011110b, 11111111b, 11011110b
 db 10111110b, 00011111b, 11111110b
 db 01101001b, 00000111b, 10111110b
 db 00010011b, 11000111b, 11101100b
 db 00000010b, 11111111b, 10111100b
 db 00000001b, 11100001b, 11101000b
 db 00000000b, 00010001b, 00110000b
 db 00000000b, 00001110b, 00000000b
p378: db 22,29; Generated from in\382.raw
 db 00000000b, 00110000b, 00000000b, 00000000b
 db 00000000b, 01001000b, 00001000b, 00000000b
 db 00000000b, 01001000b, 00010110b, 00000000b
 db 00000000b, 10011000b, 00010101b, 00000000b
 db 00000000b, 10011111b, 10110001b, 01100000b
 db 00111101b, 11111111b, 11111001b, 10110000b
 db 01010110b, 11111111b, 11111110b, 10101000b
 db 10011100b, 00111111b, 11111111b, 00101000b
 db 01111100b, 00011111b, 11110111b, 01111000b
 db 00011000b, 00011111b, 11100111b, 11110000b
 db 00011000b, 00010000b, 11001111b, 11100000b
 db 00011100b, 00100000b, 10111111b, 10000000b
 db 00010111b, 11100000b, 10011100b, 00000000b
 db 00001011b, 11100001b, 00101100b, 00000000b
 db 00001001b, 11110011b, 11101111b, 00000000b
 db 00000110b, 00010100b, 11100011b, 11000000b
 db 00000001b, 11111111b, 11101101b, 10100000b
 db 00000000b, 00111100b, 01110001b, 10100000b
 db 00000000b, 00000000b, 01111111b, 01000000b
 db 00000000b, 00000000b, 00111110b, 01000000b
 db 00000000b, 00000000b, 00010011b, 10000000b
 db 00000000b, 00000000b, 00001100b, 00000000b
p379: db 22,27; Generated from in\383.raw
 db 00000010b, 11010000b, 00000000b, 00000000b
 db 00000101b, 00111000b, 00000000b, 00000000b
 db 00000100b, 01111110b, 00000000b, 00000000b
 db 00000101b, 11111111b, 00000000b, 00000000b
 db 00001011b, 11111111b, 10000000b, 00000000b
 db 00010111b, 01111001b, 11000000b, 00000000b
 db 00101111b, 11111111b, 11100000b, 00000000b
 db 00101111b, 11100110b, 01101100b, 00000000b
 db 00111111b, 10100111b, 11110110b, 00000000b
 db 00010111b, 11101111b, 01110101b, 10000000b
 db 00001010b, 11001100b, 11110001b, 11000000b
 db 00001111b, 11011101b, 00011001b, 11000000b
 db 00110001b, 11111010b, 00011111b, 00100000b
 db 01010111b, 11111100b, 00111101b, 11000000b
 db 10111111b, 10111100b, 01111101b, 00100000b
 db 01001111b, 11110011b, 11111111b, 11000000b
 db 10010111b, 00111101b, 11000111b, 10000000b
 db 01101110b, 01111010b, 10000011b, 00000000b
 db 00010010b, 10111101b, 11000111b, 00000000b
 db 00000001b, 01110111b, 01111110b, 00000000b
 db 00000000b, 00000000b, 11010100b, 00000000b
 db 00000000b, 00000000b, 00101000b, 00000000b
p380: db 22,31; Generated from in\384.raw
 db 00001100b, 00000000b, 00000000b, 00000000b
 db 00010010b, 00000000b, 00000000b, 00000000b
 db 00010001b, 00000000b, 11100000b, 00000000b
 db 00001000b, 10000011b, 00100000b, 00000000b
 db 00001110b, 01101100b, 01000000b, 00010000b
 db 00011000b, 00010001b, 10000000b, 00101000b
 db 00100000b, 00000011b, 11100000b, 11101110b
 db 01000000b, 00000110b, 00011000b, 10101010b
 db 10000000b, 11110100b, 00000110b, 10101010b
 db 10000010b, 11110010b, 00001011b, 10101010b
 db 10000011b, 11110001b, 00001101b, 11001100b
 db 01111100b, 11110100b, 10010010b, 11001000b
 db 00000101b, 11100110b, 01010010b, 01010000b
 db 00000101b, 11111011b, 10110001b, 10110000b
 db 00001011b, 01101010b, 10101001b, 00100000b
 db 00000111b, 11111100b, 11100010b, 00100000b
 db 00000011b, 11011101b, 00110010b, 00100000b
 db 00000001b, 00010110b, 00101110b, 01000000b
 db 00000000b, 10110010b, 00111001b, 11000000b
 db 00000000b, 01100001b, 01000100b, 10000000b
 db 00000000b, 00000000b, 11000011b, 00000000b
 db 00000000b, 00000000b, 00111100b, 00000000b
p381: db 17,30; Generated from in\380.raw
 db 00000000b, 00010000b, 00000000b, 00000000b
 db 00000000b, 11101000b, 00000000b, 00000000b
 db 01100001b, 10001000b, 00000000b, 00000000b
 db 10110001b, 01000110b, 00000011b, 00000000b
 db 10101010b, 10100001b, 10000100b, 10010000b
 db 10001110b, 10100000b, 01111000b, 11101100b
 db 10110100b, 00110001b, 10100111b, 00000100b
 db 11000000b, 00101111b, 01001000b, 00001000b
 db 11000000b, 00000010b, 10010000b, 00110000b
 db 10000001b, 00000000b, 10100000b, 11000000b
 db 10000010b, 11000000b, 10100011b, 01000000b
 db 10000000b, 01100000b, 10011111b, 00100000b
 db 01000010b, 10110000b, 01011110b, 00010000b
 db 01000011b, 00101000b, 00100101b, 01100000b
 db 00100110b, 00010000b, 01000100b, 10000000b
 db 00011000b, 00001100b, 10001000b, 00000000b
 db 00000000b, 00000011b, 11110000b, 00000000b
p382: db 17,30; Generated from in\381.raw
 db 00000000b, 00010000b, 00000000b, 00000000b
 db 00000000b, 11111000b, 00000000b, 00000000b
 db 00110001b, 10111000b, 00000000b, 00000000b
 db 01011001b, 01011110b, 00000011b, 00000000b
 db 01001001b, 01111111b, 10001111b, 10110000b
 db 10001110b, 10101111b, 11111111b, 11111100b
 db 10110010b, 00110111b, 10101111b, 11110100b
 db 11100100b, 00101111b, 01001111b, 11001000b
 db 11000000b, 00000111b, 10011111b, 00110000b
 db 10000001b, 00001000b, 00111100b, 11100000b
 db 10000010b, 11000110b, 00100011b, 11110000b
 db 10000110b, 01100001b, 11111111b, 11111000b
 db 01000100b, 10110000b, 01111111b, 11110000b
 db 01001111b, 00111000b, 00100101b, 11100000b
 db 00111110b, 00011100b, 01011100b, 10000000b
 db 00011000b, 00001111b, 10111000b, 00000000b
 db 00000000b, 00000011b, 11110000b, 00000000b
p383: db 19,19; Generated from in\385.raw
 db 00000111b, 10000000b, 00000000b
 db 00001010b, 01100000b, 00000000b
 db 00001010b, 00010000b, 00000000b
 db 00111010b, 00001000b, 00000000b
 db 01001110b, 00001100b, 00000000b
 db 01000000b, 00010011b, 00000000b
 db 10100000b, 00000000b, 10000000b
 db 10100000b, 00000000b, 10000000b
 db 10110100b, 00000000b, 01000000b
 db 11101100b, 10000000b, 10100000b
 db 00011000b, 10010011b, 10100000b
 db 00000100b, 00111100b, 10100000b
 db 00001011b, 11010000b, 11100000b
 db 00011110b, 00001110b, 00000000b
 db 00100101b, 00110001b, 00000000b
 db 00010011b, 11011000b, 11000000b
 db 00001100b, 00100100b, 00100000b
 db 00000000b, 00000100b, 11000000b
 db 00000000b, 00000011b, 00000000b
p384: db 21,30; Generated from in\386.raw
 db 00000000b, 00000001b, 11000000b, 00000000b
 db 00000000b, 00000110b, 00110000b, 00000000b
 db 00000000b, 00111000b, 00011000b, 00000000b
 db 00000000b, 01111010b, 00111000b, 01100000b
 db 00000000b, 01110010b, 11111100b, 01010000b
 db 00000000b, 00110100b, 01111010b, 01010000b
 db 00000000b, 00011100b, 10110001b, 00101000b
 db 00000000b, 00001100b, 00110000b, 10101000b
 db 00000000b, 00000110b, 01111100b, 10101000b
 db 00000000b, 00001111b, 11111111b, 00101000b
 db 00111000b, 00111111b, 11111000b, 01001000b
 db 01100100b, 11011111b, 11111111b, 10010000b
 db 10010011b, 01110111b, 11111000b, 01101100b
 db 11100100b, 11001111b, 11111111b, 10011100b
 db 10000100b, 10010011b, 11111001b, 00101100b
 db 01110011b, 00010011b, 11111100b, 11111000b
 db 00001100b, 00010010b, 01111011b, 01110000b
 db 00000000b, 00001010b, 01111000b, 11000000b
 db 00000000b, 00000110b, 11110000b, 00000000b
 db 00000000b, 00000000b, 11100000b, 00000000b
 db 00000000b, 00000000b, 01000000b, 00000000b
p385: db 20,12; Generated from in\358.raw
 db 00001110b, 00000000b
 db 00010001b, 00000000b
 db 00100000b, 10000000b
 db 00100000b, 11000000b
 db 01100000b, 11100000b
 db 01000001b, 11110000b
 db 10000000b, 11110000b
 db 10100000b, 01010000b
 db 10011001b, 00010000b
 db 01000010b, 10100000b
 db 00111111b, 11000000b
 db 00011111b, 10000000b
 db 00000100b, 00000000b
 db 00011011b, 00000000b
 db 00010001b, 00000000b
 db 00010001b, 00000000b
 db 00010000b, 10000000b
 db 00001000b, 10000000b
 db 00001000b, 01000000b
 db 00000111b, 11000000b

LoadPic:
  ; Inputs: DE - picture index
  ; Outputs: (appBackUpScreen+) - image structure to be passed to DisplayImage
  ; all registers are destroyed
  ld HL,picoffsets
  add HL,DE
  add HL,DE
  ld C,(HL)
  inc HL
  ld B,(HL)
  ld H,B
  ld L,C
  ld BC,2+32*32/4
  ld A,D
  or A
  jp nz,bank2
  ld A,E
  srl A
  cp 0F0h>>1 ; X#240
  jp m,bank1
 bank2:
  ld DE,appBackUpScreen
  LDIR
  ret
 bank1:
  ld DE,appBackUpScreen
  B_CALL LPicBank1
  ret

 extern p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31
 extern p32,p33,p34,p35,p36,p37,p38,p39,p40,p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60
 extern p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p80,p81,p82,p83,p84,p85,p86,p87,p88,p89
 extern p90,p91,p92,p93,p94,p95,p96,p97,p98,p99,p100,p101,p102,p103,p104,p105,p106,p107,p108,p109,p110,p111,p112,p113,p114
 extern p115,p116,p117,p118,p119,p120,p121,p122,p123,p124,p125,p126,p127,p128,p129,p130,p131,p132,p133,p134,p135,p136,p137
 extern p138,p139,p140,p141,p142,p143,p144,p145,p146,p147,p148,p149,p150,p151,p152,p153,p154,p155,p156,p157,p158,p159,p160
 extern p161,p162,p163,p164,p165,p166,p167,p168,p169,p170,p171,p172,p173,p174,p175,p176,p177,p178,p179,p180,p181,p182,p183
 extern p184,p185,p186,p187,p188,p189,p190,p191,p192,p193,p194,p195,p196,p197,p198,p199,p200,p201,p202,p203,p204,p205,p206
 extern p207,p208,p209,p210,p211,p212,p213,p214,p215,p216,p217,p218,p219,p220,p221,p222,p223,p224,p225,p226,p227,p228,p229
 extern p230,p231,p232,p233,p234,p235,p236,p237,p238,p239

picoffsets:
 dw p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20
 dw p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40
 dw p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60
 dw p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p80
 dw p81,p82,p83,p84,p85,p86,p87,p88,p89,p90,p91,p92,p93,p94,p95,p96,p97,p98,p99,p100
 dw p101,p102,p103,p104,p105,p106,p107,p108,p109,p110,p111,p112,p113,p114,p115,p116,p117,p118,p119,p120
 dw p121,p122,p123,p124,p125,p126,p127,p128,p129,p130,p131,p132,p133,p134,p135,p136,p137,p138,p139,p140
 dw p141,p142,p143,p144,p145,p146,p147,p148,p149,p150,p151,p152,p153,p154,p155,p156,p157,p158,p159,p160
 dw p161,p162,p163,p164,p165,p166,p167,p168,p169,p170,p171,p172,p173,p174,p175,p176,p177,p178,p179,p180
 dw p181,p182,p183,p184,p185,p186,p187,p188,p189,p190,p191,p192,p193,p194,p195,p196,p197,p198,p199,p200
 dw p201,p202,p203,p204,p205,p206,p207,p208,p209,p210,p211,p212,p213,p214,p215,p216,p217,p218,p219,p220
 dw p221,p222,p223,p224,p225,p226,p227,p228,p229,p230,p231,p232,p233,p234,p235,p236,p237,p238,p239,p240
 dw p241,p242,p243,p244,p245,p246,p247,p248,p249,p250,p251,p252,p253,p254,p255,p256,p257,p258,p259,p260
 dw p261,p262,p263,p264,p265,p266,p267,p268,p269,p270,p271,p272,p273,p274,p275,p276,p277,p278,p279,p280
 dw p281,p282,p283,p284,p285,p286,p287,p288,p289,p290,p291,p292,p293,p294,p295,p296,p297,p298,p299,p300
 dw p301,p302,p303,p304,p305,p306,p307,p308,p309,p310,p311,p312,p313,p314,p315,p316,p317,p318,p319,p320
 dw p321,p322,p323,p324,p325,p326,p327,p328,p329,p330,p331,p332,p333,p334,p335,p336,p337,p338,p339,p340
 dw p341,p342,p343,p344,p345,p346,p347,p348,p349,p350,p351,p352,p353,p354,p355,p356,p357,p358,p359,p360
 dw p361,p362,p363,p364,p365,p366,p367,p368,p369,p370,p371,p372,p373,p374,p375,p376,p377,p378,p379,p380
 dw p381,p382,p383,p384,p385


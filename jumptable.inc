 public _Green
_Green			equ 44*3
 dw Green
 db 0

 public _Red
_Red				equ 45*3
 dw Red
 db 1

 public _DrawPkName
_DrawPkName		equ 46*3
 dw DrawPkName
 db 0

 public _Select
_Select			equ 47*3
 dw Select
 db 2

 public _Move
_Move				equ 48*3
 dw Move
 db 3

 public _LoadPic
_LoadPic			equ 49*3
 dw LoadPic
 db 1

 public _LPicBank1
_LPicBank1		equ 50*3
 dw LPicBank1
 db 4

 public _DrawMoveName
_DrawMoveName	equ 51*3
 dw DrawMoveName
 db 3

 public _DrawMoveName_AlignRight
_DrawMoveName_AlignRight	equ 52*3
 dw DrawMoveName_AlignRight
 db 3

 public _LoadLevelMoves
_LoadLevelMoves	equ 53*3
 dw LoadLevelMoves
 db 5

 public _MoveList
_MoveList		equ 54*3
 dw MoveList
 db 6

 public _LoadBreedMoves
_LoadBreedMoves	equ 55*3
 dw LoadBreedMoves
 db 5

 public _StatCalculator
_StatCalculator equ 56*3
 dw StatCalculator
 db 0

 public _PokeScreen2
_PokeScreen2 equ 57*3
 dw PokeScreen2
 db 7

 public _GetAbilities
_GetAbilities equ 58*3
 dw GetAbilities
 db 1


 db 0,0,0	; sync the debugger

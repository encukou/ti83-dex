;Include file needed to access system routines
 ; (Include file omitted in listing!)
 .list OFF
  include "ti83plus.inc"
 .list

EXT_APP equ 1   ;This definition is required of all apps
 ;globals on ; Note space before directive 

dexstate equ asm_Flag1
showpkmn equ 0
abilitiesinstatvars equ 1

tempFlagByte equ asm_Flag2
statcalc_minmax equ			0
statcalc_maxalready equ		1
statcalc_invalid equ		2
statcalc_evdrawn equ		3
statcalc_alpha equ			4
statcalc_entry equ			5

tempStorageByte  equ asm_Flag3

cseg            ;This linker directive is required of all apps.

;This is the application header definition area required for all apps.
 db 080h,0Fh    ;Field: Program length
 db   00h,00h,00h,00h ;Length=0 (N/A for unsigned apps)
 db 080h,012h    ;Field: Program type
 db   01h,04h  ;Type= Shareware, TI-83Plus
 db 080h,021h    ;Field: App ID
 db   01h       ;Id = 1
 db 080h,031h    ;Field: App Build
 db   01h       ;Build = 1
 db 080h,048h    ;Field: App Name
 db "BC-Dex H";Name - must be 8 characters
 db 080h,081h    ;Field: App Pages
 db 08h         ;App Pages != 1
 db 080h,090h    ;No default splash screen
 db 03h,026h ,09h,04h, 04h,06fh,01bh,80h     ;Field: Date stamp- 5/12/1999
 db 02h,0dh,040h                             ;Dummy encrypted TI date stamp signature
 db 0a1h ,06bh ,099h ,0f6h ,059h ,0bch ,067h 
 db 0f5h ,085h ,09ch ,09h ,06ch ,0fh ,0b4h ,03h ,09bh ,0c9h 
 db 03h ,032h ,02ch ,0e0h ,03h ,020h ,0e3h ,02ch ,0f4h ,02dh 
 db 073h ,0b4h ,027h ,0c4h ,0a0h ,072h ,054h ,0b9h ,0eah ,07ch 
 db 03bh ,0aah ,016h ,0f6h ,077h ,083h ,07ah ,0eeh ,01ah ,0d4h 
 db 042h ,04ch ,06bh ,08bh ,013h ,01fh ,0bbh ,093h ,08bh ,0fch 
 db 019h ,01ch ,03ch ,0ech ,04dh ,0e5h ,075h 
 db 80h,7Fh      ;Field: Program Image length
 db   0,0,0,0    ;Length=0, N/A
 db   0,0,0,0    ;Reserved
 db   0,0,0,0    ;Reserved
 db   0,0,0,0    ;Reserved
 db   0,0,0,0    ;Reserved

;End of header data

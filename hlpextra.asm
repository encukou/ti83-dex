 DEFINE P7HLPEXTRA, SPACE=ROM  
 SEGMENT P7HLPEXTRA

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
 extern _LoadLevelMoves
 extern _LoadBreedMoves
 extern _StatCalculator
 extern _GetAbilities
 public TypeChart
 public PokeScreen2

 include "header.inc"
 include "linemacros.inc"

poke2_waitkey:
  halt				; save batteries
  B_CALL GetCSC	; Get keyboard scan code
  or a	; is a 0?
  jr z,poke2_waitkey	; go again if no key
  ld hl,poke2_keys
  ld d,0
  ld e,a		; put scan code to DE (d=0 from above)
  add hl,de	; get "my" code address
  ld a,(hl)	; get "my" code to a
  ; switch(my_code)
  or a						; case 0
  jr z,poke2_waitkey	;  go to wait again
  cp "d"
  jr z,poke2waitkey_down
  cp "u"
  jr z,poke2waitkey_up
  cp ">"
  jr z,waitkey_select
  cp "~"
  jr z,waitkey_quit
  cp 1		; case 1 (Green)
  jr z,poke2waitkey_screen1
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
  LD (OP1+1),A ; OP1 = var name

  AppOnErr waitkey_failed
  B_CALL RclVarSym ; OP1(/OP2) = value
  AppOffErr
  pop AF
  B_CALL CkOP1Real ; ACC = type, Z = 1 if real
  jp nz,waitkey_failed
  B_CALL StoX
  B_JUMP Green
 
poke2waitkey_down:
  B_CALL RclX
  B_CALL Plus1
  B_CALL StoX
  jp PokeScreen2
poke2waitkey_up:
  B_CALL RclX
  B_CALL Minus1
  B_CALL StoX
  jp PokeScreen2
poke2waitkey_screen1:
  B_JUMP Green
failed_to_get_x:
waitkey_select:
  B_JUMP Select
waitkey_movelist:
  B_JUMP MoveList
waitkey_quit:
  B_JUMP JForceCmdNoChar
waitkey_failed:
  setpenpos 76,0
  ld DE,OP1
  ld HL,fail
  ld BC,5
  ldir
  ld hl,OP1
  B_CALL VPutS
  pop AF
  B_CALL VPutMap
  jp poke2_waitkey
waitkey_move:
  B_JUMP Move
waitkey_statcalc:
  B_JUMP StatCalculator

fail:
 db "BAD:",0


poke2_keys:
 db  SFourSpaces				; (not used)
 db  "d",0,0,"u",0,0,0,0		; v<>^????
 db        ">",0,"WRMH",0,0	; enter+-*/^clear-?
 db           0,"[VQLG",0,0	; -369)tan-vars-?
 db           0,"ZUPKFC",0		; .258(cos-prgm-stat
 db 			  0,"YTOJEB",0		; 0147,sin-apps-xt0n
 db        "~",0,"SNIDA",0		; ?-store-ln-log-square-recip-math-alpha
 db       "\5\4\3\2\1",0,"~"	; graph-trace-zoom-window-y=-2nd-mode
 db 0	; del


 ; statVars contains the Ability bytes.
PokeScreen2:
  AppOnErr failed_to_get_x
  B_CALL RclX			; get our X
  ld HL,385
  B_CALL SetXXXXOP2
  B_CALL CpOP1OP2
  jp z,poke2_chimechox
  jp nc,failed_to_get_x
 poke2_chimechox:
  B_CALL ConvOP1	; put X to DE
  AppOffErr
  push DE

  startbuffer

  B_CALL RclX
  xor A
  ld (penCol),A
  ld (penRow),A
  B_CALL DrawPkName

  ld HL,s_page2
  ld DE,OP1
  ld BC,8+13+9
  LDIR
  putOP1s 40, 0, 0
  putOP1s  0, 6, 8
  putOP1s  0,12,11
  putOP1s  0,18,14

  pop DE
  push DE
  ld HL,0
  add HL,DE	; x1
  add HL,DE	; x2
  add HL,DE	; x3
  push HL
  pop DE
  add HL,DE	; x6
  add HL,DE	; x9
  ld DE,d_helditems
  add HL,DE	; x9+data
  ld DE,OP1
  ld BC,9
  LDIR					; OP1 holds the indexes of our 9 items

  ld H,1	; dark line
  putline  0,25,93,25

  setpenpos 10,6
  ld A,(OP1)
  call printitem
  ld A,"/"
  B_CALL VPutMap
  ld A,(OP1+1)
  call printitem
  setpenpos 30,6
  ld A,(OP1+2)
  call printitem_ret

  setpenpos 10,12
  ld A,(OP1+3)
  call printitem
  ld A,"/"
  B_CALL VPutMap
  ld A,(OP1+4)
  call printitem
  setpenpos 30,12
  ld A,(OP1+5)
  call printitem_ret

  setpenpos 10,18
  ld A,(OP1+6)
  call printitem
  ld A,"/"
  B_CALL VPutMap
  ld A,(OP1+7)
  call printitem
  setpenpos 30,18
  ld A,(OP1+8)
  call printitem_ret

  bit abilitiesinstatvars,(IY+dexstate)
  jp nz,dontrecallabilities
  pop DE
  B_CALL GetAbilities
  push DE

 dontrecallabilities:
  res abilitiesinstatvars,(IY+dexstate)

  ld A,(statVars)
  ld E,A
  ld D,0
  ld HL,ability_descriptions
  add HL,DE	; 1
  add HL,DE	; 2
  add HL,DE	; 3
  add HL,DE	; 4
  add HL,DE	; 5
  add HL,DE	; 6
  ld DE,OP1
  ld BC,6
  LDIR				; OP1 contains the 6 pointer bytes to the texts of the first ability

  ld A,(statVars+1)
  ld E,A
  ld D,0
  ld HL,ability_descriptions
  add HL,DE	; 1
  add HL,DE	; 2
  add HL,DE	; 3
  add HL,DE	; 4
  add HL,DE	; 5
  add HL,DE	; 6
  ld DE,OP1+6
  ld BC,6
  LDIR				; OP1+6 contains the 6 pointer bytes to the textsof the second ability


  setpenpos 0,26	; Row 1
  ld HL,(OP1)
  call printline

  setpenpos 0,32	; Row 2
  ld HL,(OP1+2)
  call printline

  setpenpos 0,38	; Row 3
  ld HL,(OP1+4)
  call printline

  setpenpos 0,44	; Row 4
  ld HL,(OP1+6)
  call printline

  setpenpos 0,50	; Row 5
  ld HL,(OP1+8)
  call printline

  setpenpos 0,56	; Row 6
  ld HL,(OP1+10)
  call printline

  pop DE

  endbuffer

  jp poke2_waitkey

printline:	; HL points to a string in ROM (or is ~ NULL); prints this string. Uses OP3,OP4. 30 chars max.
			; Destroys all registers but BC
  ld A,H
  or a
  ret z
  ld DE,OP3
  ld BC,30
  LDIR
  ld HL,OP3
  B_CALL VPutS
  ret

printitem_ret:
  sla A
  ret z
  push AF
  ld A,"-"
  B_CALL VPutMap
  pop AF
  jp printitem_slad

printitem:
  sla A		; x2
 printitem_slad:
  ld D,0
  ld E,A
  ld H,0
  ld L,A
  add HL,DE	; x4
  add HL,DE	; x6
  add HL,DE	; x8
  add HL,DE	; x10
  ld DE,itemtexts
  add HL,DE
  ld DE,OP2
  ld BC,10
  LDIR
  ld HL,OP2
  ld B,10
  B_CALL VPutSN
  ret

s_page2:
 db "-ITEMS:",0
 db "GS",0,"RS",0,"FL",0

abitext_ equ 0
ability_descriptions:
 ; Automatically generated from macro-modified ability description file
 ; Don't look at the weird labels ^^
 ; Each set of ability description pointers is 6 bytes long.
 abidesc_: dw abitext_, abitext_, abitext_
 abidesc_stench: dw abitext_stench__wild_pkmn_appe_, abitext_ar_____as_much_if__st, abitext_
 abidesc_drizzle: dw abitext_drizzle__summons_rain, abitext_when_sent_to_battle, abitext_
 abidesc_speedboost: dw abitext_speed_boost__raises_spd, abitext_at_end_of_every_turn, abitext_
 abidesc_battlearmor: dw abitext_battle_armor__foe_s, abitext_moves_don_t_criticalhit, abitext_
 abidesc_sturdy: dw abitext_sturdy____hit_ko_moves, abitext_don_t_work_on_it, abitext_
 abidesc_damp: dw abitext_damp__prevents_self_, abitext_destructing_moves, abitext_
 abidesc_limber: dw abitext_limber__cannot_become, abitext_paralyzed, abitext_
 abidesc_sandveil: dw abitext_sand_veil__higher_evade, abitext_in_sandstorm_instead, abitext_E_less_wild_sandstorm
 abidesc_static: dw abitext_static______chance_to, abitext_paralyze_on_contact, abitext_E_electric_ambush
 abidesc_voltabsorb: dw abitext_volt_absorb__electric, abitext_attacks_heal_up_to____, abitext_of_max_hp_insteadof_dmg
 abidesc_waterabsorb: dw abitext_water_absorb__water, abitext_attacks_heal_up_to____, abitext_of_max_hp_insteadof_dmg
 abidesc_oblivious: dw abitext_oblivious__cannot_fall, abitext_in_love, abitext_
 abidesc_cloudnine: dw abitext_cloud_nine__prevents, abitext_effects_of_weather, abitext_
 abidesc_compoundeyes: dw abitext_compoundeyes__boosts, abitext_accuracy_by____, abitext_E_compound
 abidesc_insomnia: dw abitext_insomnia__cannot_fall, abitext_asleep, abitext_
 abidesc_colorchange: dw abitext_color_change__when_hit_, abitext_changes_its_type, abitext_to_type_of_that_move
 abidesc_immunity: dw abitext_immunity__cannot_be, abitext_poisoned, abitext_
 abidesc_flashfire: dw abitext_flash_fire_fireimmunity, abitext_______fire_damage_boost, abitext_
 abidesc_shielddust: dw abitext_shield_dust__prevents, abitext_secndary_effects_of_foe, abitext__s_moves
 abidesc_owntempo: dw abitext_own_tempo__cannot, abitext_become_confused, abitext_
 abidesc_suctioncups: dw abitext_suction_cups__foe_can_t, abitext_make_it_retreat, abitext_E_easy_fishin
 abidesc_intimidate: dw abitext_intimidate_lowers_foes_, abitext_attack_when_sent_out, abitext_E_hi_wild
 abidesc_shadowtag: dw abitext_shadow_tag__foe_cannot, abitext_retreat, abitext_
 abidesc_roughskin: dw abitext_rough_skin__hurts_foe, abitext_on_hit_by_contact_move, abitext_
 abidesc_wonderguard: dw abitext_wonder_guard__prevents, abitext_non_super_effective_dmg, abitext__except_typeless_
 abidesc_levitate: dw abitext_levitate__ground_moves, abitext_miss_prevent_arena_trap, abitext_etc_
 abidesc_effectspore: dw abitext_effect_spore______par_, abitext_slp_or_psn_on_contact, abitext_moves_by_foe
 abidesc_synchronize: dw abitext_synchronize__when_gets, abitext_par_slp_psn__transfers, abitext_E_same_nature
 abidesc_clearbody: dw abitext_clear_body__foe_cannot, abitext_lower_stats, abitext_of_this_pokemon
 abidesc_naturalcure: dw abitext_natural_cure__heals, abitext_status_when_recalled, abitext_
 abidesc_lightningrod: dw abitext_lightningrod__electric, abitext_moves_target_it_in_____, abitext_E_entry_call_madness
 abidesc_serenegrace: dw abitext_serene_grace__doubles, abitext___of_secondary_effects, abitext_of_its_moves
 abidesc_swiftswim: dw abitext_swift_swim__higher_spd, abitext_in_rainy_weather, abitext_
 abidesc_chlorophyll: dw abitext_chlorophyll__double_spd, abitext_in_sunny_weather, abitext_
 abidesc_illuminate: dw abitext_illuminate__double_wild, abitext_pkmn_appearances_if__st, abitext_in_the_party
 abidesc_trace: dw abitext_trace__copies_foe_s, abitext_ability_when_sent_out, abitext_
 abidesc_hugepower: dw abitext_huge_power__doubles_atk, abitext_, abitext_
 abidesc_poisonpoint: dw abitext_poison_point_____chance, abitext___poison_foe_on_contact, abitext_
 abidesc_innerfocus: dw abitext_inner_focus__doesn_t, abitext_flinch, abitext_
 abidesc_magmaarmor: dw abitext_magma_armor__cannot_be, abitext_frozen, abitext_E_fast_eggs
 abidesc_waterveil: dw abitext_water_veil__cannot_be, abitext_burned, abitext_
 abidesc_magnetpull: dw abitext_magnet_pull__steel_foes, abitext_cannot_retreat, abitext_E_steel_ambush
 abidesc_soundproof: dw abitext_soundproof_not_affected, abitext_by_sound_based_moves, abitext_
 abidesc_raindish: dw abitext_rain_dish__heals_hp_in, abitext_rainy_weather, abitext_
 abidesc_sandstream: dw abitext_sand_stream__starts_a, abitext_sandstorm_when_sent_out, abitext_
 abidesc_pressure: dw abitext_pressure__doubles_foe_s, abitext_pp_usage, abitext_E_less_wild
 abidesc_thickfat: dw abitext_thick_fat__halves_fire_, abitext_ice_damage, abitext_
 abidesc_earlybird: dw abitext_early_bird__sleep, abitext_duration_is_halved, abitext_
 abidesc_flamebody: dw abitext_flame_body______to_burn, abitext_foe_on_contact, abitext_E_fast_eggs
 abidesc_runaway: dw abitext_run_away__can_always, abitext_retreat, abitext_
 abidesc_keeneye: dw abitext_keen_eye_prevents_foe_s, abitext_acc_red_ATT, abitext_E_hi_wild
 abidesc_hypercutter: dw abitext_hyper_cutter__prevents, abitext_foe_s_atk_reduction, abitext_E_more_grass
 abidesc_pickup: dw abitext_pickup__get_items_after, abitext_won_battles, abitext_
 abidesc_truant: dw abitext_truant__only_attacks, abitext_every_other_turn, abitext_
 abidesc_hustle: dw abitext_hustle__does______dmg, abitext_but_with_____accuracy, abitext_E_low_wilds
 abidesc_cutecharm: dw abitext_cute_charm______chance, abitext_CUTECHARM, abitext_E_oppgender
 abidesc_plus: dw abitext_plus__if_used_w__minus, abitext_in_______raises_sat, abitext_
 abidesc_minus: dw abitext_minus__if_used_w__plus, abitext_in_______raises_sat, abitext_
 abidesc_forecast: dw abitext_forecast__changes_type_, abitext_appearance_in_weather_, abitext_castform_only
 abidesc_stickyhold: dw abitext_sticky_hold__holds_onto, abitext_held_items, abitext_E_easy_fishin
 abidesc_shedskin: dw abitext_shed_skin______chance__, abitext_heal_status_each_turn, abitext__par_psn_slp_frz_brn_
 abidesc_guts: dw abitext_guts__status_changes, abitext_boost_attack_by____, abitext__par_psn_slp_frz_brn_
 abidesc_marvelscale: dw abitext_marvel_scale__status, abitext_boosts_defense_by____, abitext__par_psn_slp_frz_brn_
 abidesc_liquidooze: dw abitext_liquid_ooze__foe_is, abitext_hurt_insteadof_draining, abitext_hp_absorb_leechseed____
 abidesc_overgrow: dw abitext_overgrow__boosts_grass, abitext_dmg_by_____if_hp_max__, abitext_
 abidesc_blaze: dw abitext_blaze__boosts_fire, abitext_dmg_by_____if_hp_max__, abitext_
 abidesc_torrent: dw abitext_torrent__boosts_water, abitext_dmg_by_____if_hp_max__, abitext_
 abidesc_swarm: dw abitext_swarm__boosts_bug, abitext_dmg_by_____if_hp_max__, abitext_E_more_roars
 abidesc_rockhead: dw abitext_rock_head__not_hurt_by, abitext_recoil_damage, abitext__except_struggle_
 abidesc_drought: dw abitext_drought__cause_sunshine, abitext_when_sent_to_battle, abitext_
 abidesc_arenatrap: dw abitext_arena_trap__non_flying, abitext_foes_cannot_escape, abitext_E_more_wild
 abidesc_vitalspirit: dw abitext_vital_spirit__cannot_be, abitext_put_to_sleep, abitext_E_low_wilds
 abidesc_whitesmoke: dw abitext_white_smoke__foe_cannot, abitext_lower_stats, abitext_E_less_wild
 abidesc_purepower: dw abitext_pure_power__doubles_atk, abitext_, abitext_
 abidesc_shellarmor: dw abitext_shell_armor__foe_s, abitext_moves_don_t_criticalhit, abitext_
 abidesc_cacophony: dw abitext_cacophony__not_affected, abitext_by_sound_based_moves, abitext_
 abidesc_airlock: dw abitext_air_lock__negates_all, abitext_effects_of_weather, abitext_
 abidesc_null: dw abitext_, abitext_, abitext_
ability_texts:
							   ;12345678901234567890123
 abitext_E_compound:		db "E:\6WILD\6PKMN:MORE\6ITEMS",0
 abitext_CUTECHARM:			db "  FALL\6IN\6LOVE\6ON\6CONTACT",0
 abitext_E_oppgender:		db "E:2/3 WILD PK: OPPOSITE SEX",0
 abitext_E_fast_eggs:		db "E:\6EGGS\6HATCH\6","2x\6FASTER",0
 abitext_E_low_wilds:		db "E:\6LOW-LEVELED\6WILD\6PKMN",0
 abitext_E_more_grass:		db "E:\6CUT\6CUTS\6MORE\6GRASS",0
 abitext_E_hi_wild:			db "E:\6HIGH-LEVELED\6WILD\6PKM",0
 abitext_E_entry_call_madness:db "E:\6LOTS\6OF\6ENTRY\6CALLS",0
 abitext_E_steel_ambush:	db "E:\6MORE\6WILD\6STEEL\6TYPES",0
 abitext_E_less_wild:		db "E:\6LESS\6WILD\6POKEMON",0
 abitext_E_more_wild:		db "E:\6MORE\6WILD\6POKEMON",0
 abitext_E_less_wild_sandstorm:db "E:LESS WILD PKMN IN STORM",0
 abitext_E_electric_ambush:	db "E:\6MORE\6WILD\6ELEC.\6TYPES",0
 abitext_E_easy_fishin:		db "E:\6FISHING\6IS\6EASIER",0
 abitext_E_more_roars:		db "E:\6HEAR\6MORE\6PKMN",0
 abitext_E_same_nature:		db "E:\6WILD\6PKMN: SAME\6NATURE",0
 ; Please remove double entries by hand
 abitext_stench__wild_pkmn_appe_: db "STENCH:\6WILD\6PKMN\6APPE-",       0
 abitext_ar_____as_much_if__st:   db "  AR\6","1/2\6AS\6MUCH\6IF\6","1ST",0
 abitext_in_the_party:            db "  IN\6THE\6PARTY",                  0
 abitext_drizzle__summons_rain:   db "DRIZZLE:\6SUMMONS\6RAIN",          0
 abitext_when_sent_to_battle:     db "  WHEN\6SENT\6TO\6BATTLE",          0
 abitext_speed_boost__raises_spd: db "SPEED\6BOOST:\6RAISES\6SPD",       0
 abitext_at_end_of_every_turn:    db "  AT\6END\6OF\6EVERY\6TURN",        0
 abitext_battle_armor__foe_s:     db "BATTLE\6ARMOR:\6FOE'S",            0
 abitext_moves_don_t_criticalhit: db "  MOVES\6DON'T\6CRITICALHIT",       0
 abitext_sturdy____hit_ko_moves:  db "STURDY:\6","1-HIT-KO\6MOVES",      0
 abitext_don_t_work_on_it:        db "  DON'T\6WORK\6ON\6IT",             0
 abitext_damp__prevents_self_:    db "DAMP:\6PREVENTS\6SELF-",           0
 abitext_destructing_moves:       db "  DESTRUCTING\6MOVES",              0
 abitext_limber__cannot_become:   db "LIMBER:\6CANNOT\6BECOME",          0
 abitext_paralyzed:               db "  PARALYZED",                       0
 abitext_sand_veil__higher_evade: db "SAND\6VEIL:\6HIGHER\6EVADE",       0
 abitext_in_sandstorm_instead:    db "  IN\6SANDSTORM\6INSTEAD",          0
 abitext_of_taking_damage:        db "  OF\6TAKING\6DAMAGE",              0
 abitext_static______chance_to:   db "STATIC:\6","30%\6CHANCE\6TO",      0
 abitext_paralyze_on_contact:     db "  PARALYZE\6ON\6CONTACT",           0
 abitext_volt_absorb__electric:   db "VOLT\6ABSORB:\6ELECTRIC",          0
 abitext_attacks_heal_up_to____:  db "  ATTACKS\6HEAL\6UP\6TO\6","1/4",   0
 abitext_of_max_hp_insteadof_dmg: db "  OF\6MAX\6HP\6INSTEADOF\6DMG",     0
 abitext_water_absorb__water:     db "WATER\6ABSORB:\6WATER",            0
 abitext_oblivious__cannot_fall:  db "OBLIVIOUS:\6CANNOT\6FALL",         0
 abitext_in_love:                 db "  IN\6LOVE",                        0
 abitext_cloud_nine__prevents:    db "CLOUD\6NINE:\6PREVENTS",           0
 abitext_effects_of_weather:      db "  EFFECTS\6OF\6WEATHER",            0
 abitext_compoundeyes__boosts:    db "COMPOUNDEYES:\6BOOSTS",            0
 abitext_accuracy_by____:         db "  ACCURACY\6BY\6","30%",            0
 abitext_insomnia__cannot_fall:   db "INSOMNIA:\6CANNOT\6FALL",          0
 abitext_asleep:                  db "  ASLEEP",                          0
 abitext_color_change__when_hit_: db "COLOR\6CHANGE:\6WHEN\6HIT,",       0
 abitext_changes_its_type:        db "  CHANGES\6ITS\6TYPE",              0
 abitext_to_type_of_that_move:    db "  TO\6TYPE\6OF\6THAT\6MOVE",        0
 abitext_immunity__cannot_be:     db "IMMUNITY:\6CANNOT\6BE",            0
 abitext_poisoned:                db "  POISONED",                        0
 abitext_flash_fire_fireimmunity: db "FLASH\6FIRE:FIREIMMUNITY",         0
 abitext_______fire_damage_boost: db "  &\6","50%\6FIRE\6DAMAGE\6BOOST",  0
 abitext_shield_dust__prevents:   db "SHIELD\6DUST:\6PREVENTS",          0
 abitext_secndary_effects_of_foe: db "  SECNDARY\6EFFECTS\6OF\6FOE",      0
 abitext__s_moves:                db "  'S\6MOVES",                       0
 abitext_own_tempo__cannot:       db "OWN\6TEMPO:\6CANNOT",              0
 abitext_become_confused:         db "  BECOME\6CONFUSED",                0
 abitext_suction_cups__foe_can_t: db "SUCTION\6CUPS:\6FOE\6CAN'T",       0
 abitext_make_it_retreat:         db "  MAKE\6IT\6RETREAT",               0
 abitext_intimidate_lowers_foes_: db "INTIMIDATE:LOWERS\6FOES'",         0
 abitext_attack_when_sent_out:    db "  ATTACK\6WHEN\6SENT\6OUT",         0
 abitext_shadow_tag__foe_cannot:  db "SHADOW\6TAG:\6FOE\6CANNOT",        0
 abitext_retreat:                 db "  RETREAT",                         0
 abitext_rough_skin__hurts_foe:   db "ROUGH\6SKIN:\6HURTS\6FOE",         0
 abitext_on_hit_by_contact_move:  db "  ON\6HIT\6BY\6CONTACT\6MOVE",      0
 abitext_wonder_guard__prevents:  db "WONDER\6GUARD:\6PREVENTS",         0
 abitext_non_super_effective_dmg: db "  NON-SUPER-EFFECTIVE\6DMG",        0
 abitext__except_typeless_:       db "  (EXCEPT\6TYPELESS)",              0
 abitext_levitate__ground_moves:  db "LEVITATE:\6GROUND\6MOVES",         0
 abitext_miss_prevent_arena_trap: db "  MISS,PREVENT\6ARENA\6TRAP",       0
 abitext_etc_:                    db "  ETC.",                            0
 abitext_effect_spore______par_:  db "EFFECT\6SPORE:\6","30%\6PAR,",     0
 abitext_slp_or_psn_on_contact:   db "  SLP\6OR\6PSN\6ON\6CONTACT",       0
 abitext_moves_by_foe:            db "  MOVES\6BY\6FOE",                  0
 abitext_synchronize__when_gets:  db "SYNCHRONIZE:\6TRANSFERS",         0
 abitext_par_slp_psn__transfers:  db "  PAR,SLP,PSN\6BACK\6TO\6FOE",         0
 abitext_status_to_foe_as_well:   db "  STATUS\6TO\6FOE\6AS\6WELL",       0
 abitext_clear_body__foe_cannot:  db "CLEAR\6BODY:\6FOE\6CANNOT",        0
 abitext_lower_stats:             db "  LOWER\6STATS",                    0
 abitext_of_this_pokemon:         db "  OF\6THIS\6POKEMON",               0
 abitext_natural_cure__heals:     db "NATURAL\6CURE:\6HEALS",            0
 abitext_status_when_recalled:    db "  STATUS\6WHEN\6RECALLED",          0
 abitext_lightningrod__electric:  db "LIGHTNINGROD:\6ELECTRIC",          0
 abitext_moves_target_it_in_____: db "  MOVES\6TARGET\6IT\6IN\6","2vs2",  0
 abitext_battles:                 db "  BATTLES",                         0
 abitext_serene_grace__doubles:   db "SERENE\6GRACE:\6DOUBLE",           0
 abitext___of_secondary_effects:  db "  %\6OF\6SECONDARY\6EFFECTS",       0
 abitext_of_its_moves:            db "  OF\6ITS\6MOVES",                  0
 abitext_swift_swim__higher_spd:  db "SWIFT\6SWIM:\6HIGHER\6SPD",        0
 abitext_in_rainy_weather:        db "  IN\6RAINY\6WEATHER",              0
 abitext_chlorophyll__double_spd: db "CHLOROPHYLL:\6DOUBLE\6SPD",        0
 abitext_in_sunny_weather:        db "  IN\6SUNNY\6WEATHER",              0
 abitext_illuminate__double_wild: db "ILLUMINATE:\6DOUBLE\6WILD",        0
 abitext_pkmn_appearances_if__st: db "  PKMN\6APPEARANCES\6IF\6","1ST",   0
 abitext_trace__copies_foe_s:     db "TRACE:\6COPIES\6FOE'S",            0
 abitext_ability_when_sent_out:   db "  ABILITY\6WHEN\6SENT\6OUT",        0
 abitext_huge_power__doubles_atk: db "HUGE\6POWER:\6DOUBLES\6ATK",       0
 abitext_poison_point_____chance: db "POISON\6POINT:\6","30%CHANCE",     0
 abitext___poison_foe_on_contact: db "  2\6POISON\6FOE\6ON\6CONTACT",     0
 abitext_inner_focus__doesn_t:    db "INNER\6FOCUS:\6DOESN'T",           0
 abitext_flinch:                  db "  FLINCH",                          0
 abitext_magma_armor__cannot_be:  db "MAGMA\6ARMOR:\6CANNOT\6BE",        0
 abitext_frozen:                  db "  FROZEN",                          0
 abitext_water_veil__cannot_be:   db "WATER\6VEIL:\6CANNOT\6BE",         0
 abitext_burned:                  db "  BURNED",                          0
 abitext_magnet_pull__steel_foes: db "MAGNET\6PULL:\6STEEL\6FOES",       0
 abitext_cannot_retreat:          db "  CANNOT\6RETREAT",                 0
 abitext_soundproof_not_affected: db "SOUNDPROOF:NOT\6AFFECTED",         0
 abitext_by_sound_based_moves:    db "  BY\6SOUND-BASED\6MOVES",          0
 abitext_rain_dish__heals_hp_in:  db "RAIN\6DISH:\6HEALS\6HP\6IN",       0
 abitext_rainy_weather:           db "  RAINY\6WEATHER",                  0
 abitext_sand_stream__starts_a:   db "SAND\6STREAM:\6STARTS\6A",         0
 abitext_sandstorm_when_sent_out: db "  SANDSTORM\6WHEN\6SENT\6OUT",      0
 abitext_pressure__doubles_foe_s: db "PRESSURE:\6DOUBLES\6FOE'S",        0
 abitext_pp_usage:                db "  PP\6USAGE",                       0
 abitext_thick_fat__halves_fire_: db "THICK\6FAT:\6HALVES\6FIRE&",       0
 abitext_ice_damage:              db "  ICE\6DAMAGE",                     0
 abitext_early_bird__sleep:       db "EARLY\6BIRD:\6SLEEP",              0
 abitext_duration_is_halved:      db "  DURATION\6IS\6HALVED",            0
 abitext_flame_body______to_burn: db "FLAME\6BODY:\6","30%\6TO\6BURN",   0
 abitext_foe_on_contact:          db "  FOE\6ON\6CONTACT",                0
 abitext_run_away__can_always:    db "RUN\6AWAY:\6CAN\6ALWAYS",          0
 abitext_keen_eye_prevents_foe_s: db "KEEN\6EYE:PREVENTS\6FOE'S",        0
 ;abitext_accuracy_reduction:      db "  ACCURACY\6REDUCTION",             0
 abitext_acc_red_ATT:      db "  ACC\6REDUCTION\6ATTEMPTS",             0
 abitext_attempts:                db "  ATTEMPTS",                        0
 abitext_hyper_cutter__prevents:  db "HYPER\6CUTTER:\6PREVENTS",         0
 abitext_foe_s_atk_reduction:     db "  FOE'S\6ATK\6REDUCTION",           0
 abitext_pickup__get_items_after: db "PICKUP:\6GET\6ITEMS\6AFTER",       0
 abitext_won_battles:             db "  WON\6BATTLES",                    0
 abitext_truant__only_attacks:    db "TRUANT:\6ONLY\6ATTACKS",           0
 abitext_every_other_turn:        db "  EVERY\6OTHER\6TURN",              0
 abitext_hustle__does______dmg:   db "HUSTLE:\6DOES\6+50%\6DMG",         0
 abitext_but_with_____accuracy:   db "  BUT\6WITH\6","80%\6ACCURACY",     0
 abitext_only:                    db "  ONLY",                            0
 abitext_cute_charm______chance:  db "CUTE\6CHARM:\6","30%\6CHANCE",     0
 ;abitext_for_foe_to_fall_in_love: db "  FOR\6FOE\6TO\6FALL\6IN\6LOVE",    0
 ;abitext_on_contact:              db "  ON\6CONTACT",                     0
 abitext_plus__if_used_w__minus:  db "PLUS:\6IF\6USED\6W/\6MINUS",       0
 abitext_in_______raises_sat:     db "  IN\6","2vs2,\6RAISES\6SAT",       0
 abitext_minus__if_used_w__plus:  db "MINUS:\6IF\6USED\6W/\6PLUS",       0
 abitext_forecast__changes_type_: db "FORECAST:\6CHANGES\6TYPE&",        0
 abitext_appearance_in_weather_:  db "  APPEARANCE\6IN\6WEATHER.",        0
 abitext_castform_only:           db "  CASTFORM\6ONLY",                  0
 abitext_sticky_hold__holds_onto: db "STICKY\6HOLD:\6HOLDS\6ONTO",       0
 abitext_held_items:              db "  HELD\6ITEMS",                     0
 abitext_shed_skin______chance__: db "SHED\6SKIN:\6","50%\6CHANCE\6","2",0
 abitext_heal_status_each_turn:   db "  HEAL\6STATUS\6EACH\6TURN",        0
 abitext__par_psn_slp_frz_brn_:   db "  (PAR,PSN,SLP,FRZ,BRN)",           0
 abitext_guts__status_changes:    db "GUTS:\6STATUS\6CHANGES",           0
 abitext_boost_attack_by____:     db "  BOOST\6ATTACK\6BY\6","50%",       0
 abitext_marvel_scale__status:    db "MARVEL\6SCALE:\6STATUS",           0
 abitext_boosts_defense_by____:   db "  BOOSTS\6DEFENSE\6BY\6","50%",     0
 abitext_liquid_ooze__foe_is:     db "LIQUID\6OOZE:\6FOE\6IS",           0
 abitext_hurt_insteadof_draining: db "  HURT\6INSTEADOF\6DRAINING",       0
 abitext_hp_absorb_leechseed____: db "  HP(ABSORB,LEECHSEED,..)",         0
 abitext_overgrow__boosts_grass:  db "OVERGROW:\6BOOSTS\6GRASS",         0
 abitext_dmg_by_____if_hp_max__:  db "  DMG\6BY\6","50%\6IF\6HP<MAX/3",   0
 abitext_blaze__boosts_fire:      db "BLAZE:\6BOOSTS\6FIRE",             0
 abitext_torrent__boosts_water:   db "TORRENT:\6BOOSTS\6WATER",          0
 abitext_swarm__boosts_bug:       db "SWARM:\6BOOSTS\6BUG",              0
 abitext_rock_head__not_hurt_by:  db "ROCK\6HEAD:\6NOT\6HURT\6BY",       0
 abitext_recoil_damage:           db "  RECOIL\6DAMAGE",                  0
 abitext__except_struggle_:       db "  (EXCEPT\6STRUGGLE)",              0
 abitext_drought__cause_sunshine: db "DROUGHT:\6CAUSE\6SUNSHINE",        0
 abitext_arena_trap__non_flying:  db "ARENA\6TRAP:\6NON-FLYING",         0
 abitext_foes_cannot_escape:      db "  FOES\6CANNOT\6ESCAPE",            0
 abitext_vital_spirit__cannot_be: db "VITAL\6SPIRIT:\6CANNOT\6BE",       0
 abitext_put_to_sleep:            db "  PUT\6TO\6SLEEP",                  0
 abitext_white_smoke__foe_cannot: db "WHITE\6SMOKE:\6FOE\6CANNOT",       0
 abitext_pure_power__doubles_atk: db "PURE\6POWER:\6DOUBLES\6ATK",       0
 abitext_shell_armor__foe_s:      db "SHELL\6ARMOR:\6FOE'S",             0
 abitext_cacophony__not_affected: db "CACOPHONY:\6NOT\6AFFECTED",        0
 abitext_air_lock__negates_all:   db "AIR\6LOCK:\6NEGATES\6ALL",         0

silverpowder	equ 1
poisonbarb		equ 2
bigmushroom		equ 3
tinymushroom	equ 4
twistedspoon	equ 5
kingsrock		equ 6
hardstone		equ 7
everstone 		equ 8
magnet			equ 9
metalcoat		equ 10
sharpbeak		equ 11
stick			equ 12
bigpearl		equ 13
nugget			equ 14
pearl			equ 15
thickclub		equ 16
luckyegg		equ 17
smokeball		equ 18
dragonscale		equ 19
stardust		equ 20
starpiece		equ 21
burntberry		equ 22
leppaberry		equ 23
mysteryberry	equ 24
leftovers		equ 25
metalpowder		equ 26
berserkgene		equ 27
dragonfang		equ 28
chestoberry		equ 29
lumberry		equ 30
berry			equ 31
miracleseed		equ 32
sitrusberry		equ 33
oranberry		equ 34
goldberry		equ 35
persimberry		equ 36
spelltag 		equ 37
redshard		equ 38
aspearberry		equ 39
nevermeltice	equ 40
moomoomilk		equ 41
rawstberry		equ 42
moonstone		equ 43
sunstone		equ 44
softsand		equ 45
blueshard		equ 46
greenshard		equ 47
mysticwater		equ 48
iceberry		equ 49
heartscale		equ 50
pechaberry		equ 51
sacredash		equ 52
upgrade			equ 53
quickclaw		equ 54
yellowshard		equ 55
lightball		equ 56
focusband		equ 57
berryjuice		equ 58
d_helditems:
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db silverpowder,0,0,silverpowder,0,0,silverpowder,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db poisonbarb,0,0,poisonbarb,0,0,poisonbarb,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,sitrusberry,oranberry,0,0,0,0,0,0,0,0,0,0
 db sharpbeak,0,0,sharpbeak,0,0,sharpbeak,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,poisonbarb,0,0
 db berry,0,0,lightball,oranberry,0,0,0,0,berry,0,0,oranberry,0,0,0,0,0,0,0,0,quickclaw,0,0,0,0,0
 db 0,0,0,quickclaw,0,0,softsand,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,moonstone,mysteryberry,0,moonstone,leppaberry,0,moonstone,0,0
 db moonstone,mysteryberry,0,moonstone,leppaberry,0,moonstone,0,0,iceberry,iceberry,0,0,0,rawstberry,0,rawstberry,0
 db iceberry,iceberry,0,0,0,rawstberry,0,rawstberry,0,0,0,0,0,0,0,0,oranberry,0,0,0,0,0,0,0,0,oranberry,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db bigmushroom,tinymushroom,0,bigmushroom,tinymushroom,0,bigmushroom,tinymushroom,0
 db bigmushroom,tinymushroom,0,bigmushroom,tinymushroom,0,bigmushroom,tinymushroom,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,silverpowder,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,nugget,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db iceberry,iceberry,0,0,0,rawstberry,0,rawstberry,0,iceberry,iceberry,0,0,0,rawstberry,0,rawstberry,0
 db 0,0,0,0,0,0,0,0,0,kingsrock,0,0,kingsrock,0,0,0,0,0,kingsrock,0,0,kingsrock,0,0,0,0,0
 db 0,0,0,twistedspoon,0,0,twistedspoon,0,0,0,0,0,twistedspoon,0,0,twistedspoon,0,0
 db 0,0,0,twistedspoon,0,0,twistedspoon,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,focusband,0,0,0,0,0,0,0,0,focusband,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db everstone,0,0,everstone,0,0,0,0,0,everstone,0,0,everstone,0,0,hardstone,0,0
 db everstone,0,0,everstone,0,0,hardstone,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db kingsrock,0,0,kingsrock,0,0,0,0,0,kingsrock,0,0,kingsrock,0,0,0,0,0,metalcoat,0,0,metalcoat,0,0,0,0,0
 db metalcoat,0,0,metalcoat,0,0,magnet,0,0,stick,0,0,stick,0,0,stick,0,0,0,0,0,sharpbeak,0,0,0,0,0
 db sharpbeak,0,0,sharpbeak,0,0,sharpbeak,0,0,0,0,0,0,0,0,0,aspearberry,0,0,0,0,0,0,0,nevermeltice,aspearberry,0
 db nugget,0,0,nugget,0,0,0,0,0,nugget,0,0,nugget,0,0,0,0,0,bigpearl,pearl,0,bigpearl,pearl,0,bigpearl,pearl,0
 db bigpearl,pearl,0,bigpearl,pearl,0,bigpearl,pearl,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,spelltag,0,0
 db 0,0,0,0,0,0,spelltag,0,0,0,0,0,0,0,0,hardstone,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db thickclub,thickclub,0,thickclub,0,0,thickclub,0,0,thickclub,thickclub,0,thickclub,0,0,thickclub,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,smokeball,0,0,0,0,0,0,0,0,smokeball,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,luckyegg,0,0,luckyegg,0,0,luckyegg,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db dragonscale,0,0,dragonscale,0,0,0,0,0,dragonscale,0,0,dragonscale,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db starpiece,stardust,0,starpiece,stardust,0,starpiece,stardust,0
 db starpiece,stardust,0,starpiece,stardust,0,starpiece,stardust,0,mysteryberry,0,0,leppaberry,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,burntberry,burntberry,0,0,0,aspearberry,0,0,0,0,0,0,0,0,0,0,0,0
 db iceberry,iceberry,0,0,0,rawstberry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,metalpowder,0,0,metalpowder,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,leftovers,0,0,leftovers,0,0,chestoberry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,dragonscale,0,0,dragonscale,0,0,0,0,0,dragonscale,0,0,dragonscale,0,0,dragonfang,0,0
 db dragonscale,0,0,dragonscale,0,0,dragonfang,0,0,berserkgene,0,0,0,0,0,0,0,0,miracleseed,0,0,0,0,lumberry,0,0,0
 db 0,0,berry,0,0,0,0,0,lumberry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,berry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,berry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,berry,0,oranberry,0,0,oranberry,0,0,goldberry,berry,0,sitrusberry,oranberry,0,sitrusberry,oranberry,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,yellowshard,0,0,yellowshard,0,0,0,0,0,yellowshard,0,0,yellowshard,0,0
 db berry,0,0,oranberry,0,0,0,0,0,moonstone,mysteryberry,0,moonstone,leppaberry,0,moonstone,0,0
 db 0,0,0,0,0,0,0,oranberry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,kingsrock,0,0,kingsrock,0,0,kingsrock,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db kingsrock,0,0,kingsrock,0,0,kingsrock,0,0,spelltag,0,0,spelltag,0,0,spelltag,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,persimberry,0,0,persimberry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,metalcoat,0,0,metalcoat,0,0,metalcoat,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,berry,0,0,oranberry,0,0,berryjuice,0,0,0,0,0,0,0,0,0
 db quickclaw,0,0,quickclaw,0,0,0,0,0,0,0,0,0,0,0,sitrusberry,oranberry,0,0,0,0,0,0,0,sitrusberry,oranberry,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,aspearberry,0,0,0,0,0,0,0,nevermeltice,aspearberry,0
 db 0,0,0,redshard,0,0,redshard,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,sharpbeak,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,dragonscale,0,0,dragonscale,0,0,dragonscale,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,upgrade,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,burntberry,burntberry,0,0,0,aspearberry,0,0,0,0,0,0,0,0,0,0,0,0
 db iceberry,iceberry,0,0,0,rawstberry,0,0,0,moomoomilk,moomoomilk,0,0,0,moomoomilk,0,0,moomoomilk
 db luckyegg,0,0,luckyegg,0,0,luckyegg,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,sacredash,0,0,sacredash,0,0,0
 db miracleseed,0,0,0,0,lumberry,0,0,lumberry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,pechaberry,0,0,pechaberry,0,0,0,0,0,pechaberry,0,0,pechaberry,0,0,0,0,0,oranberry,0,0,oranberry,0,0
 db 0,0,0,sitrusberry,oranberry,0,sitrusberry,oranberry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,silverpowder,0,0,silverpowder,0,0,0,0,0,0,0,0,0,0,0,0,0,0,silverpowder,0,0,silverpowder,0,0
 zeros:	; stealing "a few" 0-bytes for other purposes as well
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,chestoberry,0,0,chestoberry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,silverpowder,0,0,silverpowder,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,leppaberry,0,0,leppaberry,0,0
 db 0,0,0,leppaberry,0,0,leppaberry,0,0,0,0,0,persimberry,0,0,persimberry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,heartscale,0,0,heartscale,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,softsand,0,0,softsand,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,kingsrock,0,0,kingsrock,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,rawstberry,0,0,rawstberry,0,0,0,0,0,rawstberry,0,0,rawstberry,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,poisonbarb,0,0,poisonbarb,0,0,0,0,0,poisonbarb,0,0,poisonbarb,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,nevermeltice,0,0,nevermeltice,0,0,0,0,0,moonstone,0,0,moonstone,0,0,0,0,0,sunstone,0,0,sunstone,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,spelltag,0,0,spelltag,0,0,0,0,0,spelltag,0,0,spelltag,0,0,0,0,0,poisonbarb,0,0,poisonbarb,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,bigpearl,0,0,bigpearl,0,0
 db 0,0,0,bigpearl,0,0,bigpearl,0,0,0,0,0,0,0,0,0,0,0,0,0,0,chestoberry,0,0,chestoberry,0,0
 db 0,0,0,chestoberry,0,0,chestoberry,0,0,0,0,0,chestoberry,0,0,chestoberry,0,0,0,0,0,blueshard,0,0,blueshard,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,spelltag,0,0,spelltag,0,0
 db 0,0,0,spelltag,0,0,spelltag,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,greenshard,0,0,greenshard,0,0
 db 0,0,0,hardstone,0,0,hardstone,0,0,0,0,0,hardstone,0,0,hardstone,0,0,0,0,0,hardstone,0,0,hardstone,0,0
 db 0,0,0,0,0,mysticwater,0,0,mysticwater,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,dragonscale,0,0,dragonscale,0,0,0,0,0,dragonscale,0,0,dragonscale,0,0
 db 0,0,0,dragonscale,0,0,dragonscale,0,0,0,0,0,metalcoat,0,0,metalcoat,0,0,0,0,0,metalcoat,0,0,metalcoat,0,0
 db 0,0,0,metalcoat,0,0,metalcoat,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,starpiece,0,0,starpiece
 db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

itemtexts:
 db [10]SFourSpaces
 db "SILVERPWDR"
 db "POISONBARB"
 db "BIGMSHROOM"
 db "TINYMSHROM"
 db "TWISTEDSPN"
 db "KINGS\6ROCK"
 db "HARD\6STONE"
 db "EVERSTONE\6"
 db "MAGNET\6\6\6\6"
 db "METAL\6COAT"
 db "SHARP\6BEAK"
 db "STICK\6\6\6\6\6"
 db "BIG\6PEARL\6"
 db "NUGGET\6\6\6\6"
 db "PEARL\6\6\6\6\6"
 db "THICK\6CLUB"
 db "LUCKY\6EGG\6"
 db "SMOKE\6BALL"
 db "DRAGNSCALE"
 db "STARDUST\6\6"
 db "STAR\6PIECE"
 db "BURNTBERRY"
 db "LEPPABERRY"
 db "MYSTRYBERY"
 db "LEFTOVERS\6"
 db "METALPWDER"
 db "BERSRKGENE"
 db "DRAGONFANG"
 db "CHESTOBRRY"
 db "LUM\6BERRY\6"
 db "BERRY\6\6\6\6\6"
 db "MIRACLSEED"
 db "SITRUSBERY"
 db "ORAN\6BERRY"
 db "GOLD\6BERRY"
 db "PERSIMBERY"
 db "SPELL\6TAG\6"
 db "RED\6SHARD\6"
 db "ASPEARBERY"
 db "NEVRMLTICE"
 db "MOOMOOMILK"
 db "RAWSTBERRY"
 db "MOON\6STONE"
 db "SUN\6STONE\6"
 db "SOFT\6SAND\6"
 db "BLUE\6SHARD"
 db "GREENSHARD"
 db "MYSTCWATER"
 db "ICE\6BERRY\6"
 db "HEARTSCALE"
 db "PECHABERRY"
 db "SACRED\6ASH"
 db "UPGRADE\6\6\6"
 db "QUICK\6CLAW"
 db "YELOWSHARD"
 db "LIGHT\6BALL"
 db "FOCUS\6BAND"
 db "BERRYJUICE"

TypeChart:

p_t1: db 2,9,9; Generated from in2\01.raw
 db 00111110b, 00000000b
 db 01000001b, 00000000b
 db 10001000b, 10000000b
 db 10101010b, 10000000b
 db 10011100b, 10000000b
 db 10101010b, 10000000b
 db 10001000b, 10000000b
 db 01000001b, 00000000b
 db 00111110b, 00000000b
p_t2: db 1,12,12; Generated from in2\02.raw
 db 00001000b, 00000000b
 db 00000100b, 11000000b
 db 01000100b, 01100000b
 db 01001110b, 01100000b
 db 11001110b, 01110000b
 db 11011011b, 10110000b
 db 11111101b, 11110000b
 db 11111101b, 11110000b
 db 01101001b, 10100000b
 db 01101000b, 01100000b
 db 00110000b, 11000000b
 db 00001001b, 00000000b
p_t3: db 1,12,10; Generated from in2\03.raw
 db 00001111b, 00000000b
 db 00111111b, 11000000b
 db 01111100b, 00000000b
 db 01111000b, 00000000b
 db 11111100b, 00000000b
 db 11111111b, 00000000b
 db 11111111b, 10000000b
 db 11111111b, 11000000b
 db 01011111b, 11000000b
 db 01100111b, 11000000b
 db 00111111b, 10000000b
 db 00001111b, 00000000b
p_t4: db 1,12,8; Generated from in2\04.raw
 db 00010000b
 db 00110000b
 db 00110000b
 db 01110000b
 db 01110000b
 db 11111111b
 db 11111111b
 db 00001110b
 db 00001110b
 db 00001100b
 db 00001100b
 db 00001000b
p_t5: db 2,9,9; Generated from in2\05.raw
 db 00011111b, 10000000b
 db 01111111b, 10000000b
 db 11111011b, 10000000b
 db 11111001b, 10000000b
 db 11101111b, 10000000b
 db 00100111b, 10000000b
 db 00111111b, 00000000b
 db 01001111b, 00000000b
 db 10001110b, 00000000b
p_t6: db 1,12,12; Generated from in2\06.raw
 db 00000110b, 00000000b
 db 00000110b, 00000000b
 db 11000110b, 00110000b
 db 11100110b, 01110000b
 db 01111111b, 11100000b
 db 00011111b, 10000000b
 db 00011111b, 10000000b
 db 01111111b, 11100000b
 db 11100110b, 01110000b
 db 11000110b, 00110000b
 db 00000110b, 00000000b
 db 00000110b, 00000000b
p_t7: db 1,12,11; Generated from in2\07.raw
 db 00011011b, 01000000b
 db 01011011b, 01100000b
 db 01011011b, 01100000b
 db 11011011b, 01100000b
 db 11011011b, 01100000b
 db 11100000b, 00000000b
 db 11111111b, 11100000b
 db 11111111b, 11100000b
 db 01111111b, 11100000b
 db 00111111b, 11000000b
 db 00011111b, 10000000b
 db 00011111b, 10000000b
p_t8: db 1,12,10; Generated from in2\08.raw
 db 00011110b, 00000000b
 db 00111111b, 00000000b
 db 01101101b, 10000000b
 db 01111111b, 10000000b
 db 01111111b, 10000000b
 db 00011110b, 00000000b
 db 00010010b, 00000000b
 db 01000000b, 10000000b
 db 11100001b, 11000000b
 db 00011110b, 00000000b
 db 11100001b, 11000000b
 db 01000000b, 10000000b
p_t9: db 7,6,12; Generated from in2\09.raw
 db 11111111b, 11110000b
 db 11111111b, 11110000b
 db 01111111b, 11100000b
 db 01111111b, 11100000b
 db 00111111b, 11000000b
 db 00001111b, 00000000b
p_t10: db 4,4,12; Generated from in2\10.raw
 db 01111001b, 11100000b
 db 11111111b, 11110000b
 db 10001111b, 00010000b
 db 00000110b, 00000000b
p_t11: db 2,8,12; Generated from in2\11.raw
 db 00001111b, 00000000b
 db 00110000b, 11000000b
 db 01000110b, 00100000b
 db 10001001b, 00010000b
 db 10001001b, 00010000b
 db 01000110b, 00100000b
 db 00110000b, 11000000b
 db 00001111b, 00000000b
p_t12: db 1,12,12; Generated from in2\12.raw
 db 00000110b, 00000000b
 db 00001111b, 00000000b
 db 00001111b, 00000000b
 db 01100110b, 01100000b
 db 00011111b, 10000000b
 db 11101111b, 01110000b
 db 00011111b, 10000000b
 db 01100110b, 01100000b
 db 00001111b, 00000000b
 db 00001111b, 00000000b
 db 00001111b, 00000000b
 db 00000110b, 00000000b
p_t13: db 1,11,11; Generated from in2\13.raw
 db 00001110b, 00000000b
 db 00011101b, 10000000b
 db 00101110b, 11000000b
 db 01010101b, 01100000b
 db 10101111b, 11100000b
 db 11010111b, 11100000b
 db 01101111b, 11100000b
 db 01010111b, 11100000b
 db 00101111b, 11000000b
 db 00011111b, 11000000b
 db 00001111b, 10000000b
p_t14: db 1,11,8; Generated from in2\14.raw
 db 00111100b
 db 01111110b
 db 01011010b
 db 11111111b
 db 10111101b
 db 11000011b
 db 11111111b
 db 11111111b
 db 11111111b
 db 11111111b
 db 10011001b
p_t15: db 0,14,13; Generated from in2\15.raw
 db 00010000b, 00000000b
 db 11111100b, 00000000b
 db 01111111b, 00000000b
 db 11111111b, 10000000b
 db 00000001b, 10000000b
 db 00000001b, 11000000b
 db 00000011b, 11000000b
 db 00001111b, 10010000b
 db 00111110b, 00111000b
 db 01111000b, 00010000b
 db 01100000b, 00100000b
 db 01100000b, 00100000b
 db 00110000b, 11000000b
 db 00011111b, 00000000b
p_t16: db 1,12,12; Generated from in2\16.raw
 db 00001111b, 00000000b
 db 00110011b, 11000000b
 db 01000111b, 11100000b
 db 01000111b, 11100000b
 db 10001111b, 11110000b
 db 10001111b, 11110000b
 db 10001111b, 11110000b
 db 10001111b, 11110000b
 db 01000111b, 11100000b
 db 01000111b, 11100000b
 db 00110011b, 11000000b
 db 00001111b, 00000000b
p_t17: db 1,12,5; Generated from in2\17.raw
 db 01010000b
 db 11011000b
 db 11111000b
 db 00100000b
 db 00110000b
 db 01100000b
 db 00110000b
 db 01100000b
 db 00110000b
 db 01100000b
 db 00110000b
 db 01100000b

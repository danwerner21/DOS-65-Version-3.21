;**************************************************************************************************
;*                                                                                                *
;*                        C02 BIOS/Monitor JMP Table for Version 5.x                              *
;*                                                                                                *
;*                                                                                                *
;*                                  10/04/2025 (Day/Month/Year)                                   *
;*                                                                                                *
;**************************************************************************************************
;                                                                                                 *
; C02BIOS Version 5.0                                                                             *
; - All Jump Table entries are defined in this single source file.                                *
; - Be sure to include this file at the start of any source file that needs it.                   *
;                                                                                                 *
;**************************************************************************************************
;                                                                                                 *
;Monitor JUMP table: 2 JUMP calls are available.                                                  *
;                                                                                                 *
;**************************************************************************************************
;
M_COLD_MON      .EQU    $F000           ;Call 00        Monitor Cold Start
M_WARM_MON      .EQU    $F003           ;Call 01        Monitor Warm Start
;
;**************************************************************************************************
;                                                                                                 *
;BIOS JUMP Table starts here:                                                                     *
;       - BIOS calls are listed below - total of 32                                               *
;       - Reserved calls are for future hardware support                                          *
;                                                                                                 *
;**************************************************************************************************
;
B_IDE_RESET             .EQU    $FF00   ;Call 00
B_IDE_GET_STAT          .EQU    $FF03   ;Call 01
B_IDE_IDENTIFY          .EQU    $FF06   ;Call 02
B_IDE_READ_LBA          .EQU    $FF09   ;Call 03
B_IDE_WRITE_LBA         .EQU    $FF0C   ;Call 04
B_IDE_SET_LBA           .EQU    $FF0F   ;Call 05
B_IDE_SET_ADDR          .EQU    $FF12   ;Call 06
B_IDE_SET_CACHE         .EQU    $FF15   ;Call 07
;
B_CHR_STAT              .EQU    $FF18   ;Call 08
B_CHRIN_NW              .EQU    $FF1B   ;Call 09
B_CHRIN                 .EQU    $FF1E   ;Call 10
B_CHROUT                .EQU    $FF21   ;Call 11
;
B_CHRIN2                .EQU    $FF24   ;Call 12
B_CHROUT2               .EQU    $FF27   ;Call 13
;
B_CNT_INIT              .EQU    $FF2A   ;Call 14
B_CNT_STRT              .EQU    $FF2D   ;Call 15
B_CNT_STOP              .EQU    $FF30   ;Call 16
B_CNT_DISP              .EQU    $FF33   ;Call 17
;
B_SET_DLY               .EQU    $FF36   ;Call 18
B_EXE_MSDLY             .EQU    $FF39   ;Call 19
B_EXE_LGDLY             .EQU    $FF3C   ;Call 20
;
B_PROMPTR               .EQU    $FF3F   ;Call 21
;
B_RTC_INIT              .EQU    $FF42   ;Call 22
;
B_PRSTAT                .EQU    $FF45   ;Call 23
;
B_RESERVE0              .EQU    $FF48   ;Call 24
;
B_INIT_VEC              .EQU    $FF4B   ;Call 25
B_INIT_CFG              .EQU    $FF4E   ;Call 26
;
B_INIT_28L92            .EQU    $FF51   ;Call 27
B_RESET_28L92           .EQU    $FF54   ;Call 28
;
B_PANIC                 .EQU    $FF57   ;Call 29
B_IDE_BOOT              .EQU    $FF5A   ;Call 30
;
B_COLDSTRT              .EQU    $FF5D   ;Call 31
;
BIOS_MSG                .EQU    $FFD0   ;BIOS Startup Message is hard-coded here
;
;**************************************************************************************************
;
        .END


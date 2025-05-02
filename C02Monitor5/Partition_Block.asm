;**************************************************************************************************
;*                                                                                                *
;*                        Microdrive Partition Block for Booting an OS                            *
;*                                                                                                *
;*                                                                                                *
;*                                  10/04/2025 (Day/Month/Year)                                   *
;*                                                                                                *
;*                                    Copyright Kevin Maier                                       *
;*                                                                                                *
;*                                     GNU GPL V3 License                                         *
;*                                                                                                *
;**************************************************************************************************
; Partition Block 1.01                                                                            *
; - Initial Partition Block format for enabling a boot from the Microdrive.                       *
; - Based on the  Ontrack Disk Manager Partition Table, but with 65C02 boot software.             *
;                                                                                                 *
;**************************************************************************************************
        PL      66      ;Page Length
        PW      132     ;Page Width (# of char/line)
        CHIP    W65C02S ;Enable WDC 65C02 instructions
        PASS1   OFF     ;Set ON for debugging
        INCLIST ON      ;Set ON for listing Include files
;**************************************************************************************************
 ;Include required constants/equates/tables to assemble
;
        NOLIST                                  ;Turn off list function, too much text
        INCLUDE         C02Constants5.asm       ;Constants/Equates - C02 BIOS/Monitor/Hardware
        INCLUDE         C02JMP_Table_5.asm      ;Jump Table entries for C02 BIOS/Monitor
        LIST                                    ;Turn List function back on
;
;**************************************************************************************************
;
; Note that the ORG statement below is bogus, it just marks the starting offset of the block
; - The BIOS/Monitor will load the Partition Record into low memory first, from LBA 0.
;       - The block is tested for the signature at offset 253 for $0265 (6502 as a Hex word)
;       - If this siganture test passes, the BIOS/Monitor will jump to the start of the LBA load
;       - If the signature test fails, the BIOS/Monitor posts an error and warm boots the Monitor
;
; Once control is passed here (partition record loaded), the following sequence happens:
;       - Sends an "*" to the console to show it's loaded and has execution
;       - Checks the stack to figure out where itself has been loaded in memory
;       - Checks for the classic 2-byte signature at the end ($AA55)
;       - If that fails, en error is sent and a jump to warm boot the Monitor is done.
;       - If that passes, it checks the table entries looking at active table ($80 first byte)
;       - If that fails, an error is sent and a jump to warm boot the Monitor is done.
;
; If an active partition record is found, the following sequence happens;
;       - We check the starting LBA to ensure it's in the range of the BIOS (24-bit only)
;       - If that fails, we send an error and warm boot the Monitor
;       - If that passes, we load the boot record from the active partition into memory
;       - We chack for any BIOS error in loading the active record, warm boot Monitor if it failed
;       - If the active boot record loaded properly, we jump to the start of it and we're done.
;
        .ORG    $1000           ;Start of partition block offset - bogus, can't be $0000
;
        LDA     #'*'            ;Get an asterisk
        JSR     B_CHROUT        ;Send to the console
;
; We send an asterisk to the console first for two reasons:
; - it's a visual sign that the partition record was successfully loaded and executed
; - we need to know where it's loaded, so we can now look at the stack and get the return address
;
        TSX                     ;Get stack pointer
        LDA     $0100,X         ;Get return address high byte
        STA     $01             ;Save it as a Page Zero pointer
        DEX                     ;Decrement pointer
        LDA     $0100,X         ;Get return address low byte
        STA     $00             ;Save it as a Page Zero pointer
;
; Now, subtract 4 from the 16-bit address to point to our entry address
;
        SEC                     ;Set carry for subtraction
        LDA     $00             ;Get low byte address
        SBC     #$04            ;Subtract 4
        STA     $00             ;Store it back
        STA     $02             ;Save it for Text Printing
        LDA     $01             ;Get high byte address
        SBC     #$00            ;Subtract carry flag
        STA     $01             ;Store it back
        STA     $03             ;Store it for Text Printing
;
; We now have the location the partition record was loaded to as a Page Zero indirect pointer
; - We now need to check the partition record for the correct signature. The signature is
; - the standard $AA55 at the end, which is on the next page.
; - Increment the high byte address pointer to access the second page.
;
        INC     $01             ;Increment high byte address to get to second page of record
        LDY     #$FF            ;Load index to last byte of partition record
        LDA     ($00),Y         ;Get signature of last byte
        CMP     #$AA            ;Check for correct bit pattern
        BNE     BAD_PART        ;Branch if not equal
        DEY                     ;Decrement index to next signature byte
        LDA     ($00),Y         ;Get signature of next to last byte
        CMP     #$55            ;Check for correct bit pattern
        BNE     BAD_PART        ;Branch if not equal
;
; Partition Record has the correct signature, yay!
; - Now we need to scan the boot record entries to see if we have an active one
; - If not, we send a message to the console showing no active parition, then
; - we warm boot to the Monitor code.
;
; If we find an active partition, we will check the starting block address, ensure
; - it's within the range of our BIOS, then load the parameters and read the boot block
; - Once loaded, we will jump to the starting address of the boot block, we're done!
;
        DEC     $01             ;Decrement back to the first page
        LDX     #16             ;Set count for 16 Boot Records
BOOT_REC_LP
        LDY     #<BOOT_RECORD   ;Get low byte offset to Boot record start
        LDA     ($00),Y         ;Get first byte of Boot record
        BMI     BOOT_FOUND      ;If bit 7 set, active Boot record found!
        DEX                     ;Decrement count
        BEQ     NO_ACTIVE       ;No active Boot Record found, branch
;
; Next, add 16 to the page zero indirect address.
; - This is done so we can step through each of the boot records scanning for one
; - that is active. It's just a simple add of 16 to the pointers.
;
        CLC                     ;Clear carry for add
        LDA     $00             ;Get low byte of pointer
        ADC     #16             ;Add 16 for offset to next boot record
        STA     $00             ;Store it back
        LDA     $01             ;Get high byte of pointer
        ADC     #00             ;Add in carry
        STA     $01             ;Store it back
;
        BRA     BOOT_REC_LP     ;Loop back for next
;
BOOT_FOUND
; An active boot record is found! Now we need to look at the starting LBA
; for the active boot record. The C02BIOS only provides for 24-bit LBA addressing!
; As the partition record has a long word offset, if the high order byte is a
; non-zero value, it's out of the BIOS range and a Boot Error is shown.
;
        CLC                     ;Clear carry for add
        LDA     $00             ;Get Boot record offset
        ADC     #$FE            ;Add offset to LBA start
        STA     $00             ;Store it back
        LDA     $01             ;Get high byte
        ADC     #$00            ;Add in carry
        STA     $01             ;store it back
;
; The start of the active partition record is now in page zero as an indirect address.
; Now grab the 24-bit LBA offset (3 bytes) and save to page zero. The 4th byte must
; be zero. If not, the partition starting LBA is beyond the BIOS access.
;
        LDY     #08             ;Get offset to Boot record first LBA
        LDA     ($00),Y         ;Get first byte
        STA     $04             ;Store it
        INY                     ;Increment to next byte value
        LDA     ($00),Y         ;Get second byte
        STA     $05             ;Store it
        INY                     ;Increment to next byte value
        LDA     ($00),Y         ;Get third byte
        STA     $06             ;Store it
        INY                     ;Increment to next byte value
        LDA     ($00),Y         ;Get fourth byte
        BNE     BOOT_ERROR      ;If not zero, out of BIOS range
;
        LDA     $04             ;Set LBA number to load (24-bit)
        LDY     $05
        LDX     $06   
        JSR     B_IDE_SET_LBA   ;Call BIOS to Set address
;
        LDX     #$01            ;Set Block count to 1
        LDA     #<BOOT_BUFFER   ;Set low byte of BOOT Buffer
        LDY     #>BOOT_BUFFER   ;Set high byte of BOOT buffer
        JSR     B_IDE_SET_ADDR  ;Call BIOS to set address/count
;
        JSR     B_IDE_READ_LBA  ;Call BIOS to read block into memory
        LDA     IDE_STATUS_RAM  ;Get Status from BIOS call
        LSR     A               ;Shift error bit to carry
        BCS     BOOT_ERROR      ;Branch if error
        JMP     BOOT_BUFFER     ;Jump to Boot record, we're done!
;
BOOT_ERROR
        LDA     #<BAD_BLOCK     ;Get low byte offset
        BRA     MSG_FINISH      ;Use routine below to finish message and halt
;
; We have not found any of the Boot Records to have an active flag. Therefore, we can not
; attempt to load a boot record and continue booting from the disk. We simply send a message
; to the console and go back to the Monitor via the Warm Boot entry.
;
NO_ACTIVE
        LDA     #<NO_ACT_PART   ;Get low byte offset
        BRA     MSG_FINISH      ;Use routine above to finish message and Monitor entry
;
; We have a bad partition record! The two signature bytes at the end of the record are
; not correct. Therefore, we send a message out the to console, then halt the CPU.
;
BAD_PART
        LDA     #<BAD_REC_MSG   ;Get low byte offset
MSG_FINISH
        CLC                     ;Clear carry for add
        ADC     $02             ;Add any offset from record location
        STA     $02             ;Store it back
        LDY     #$00            ;Zero Y index
MSG_LOOP
        LDA     ($02),Y         ;Get Message
        BEQ     HALT_CPU        ;If end of message, branch
        JSR     B_CHROUT        ;Send to Console
        INY                     ;Increment Index
        BRA     MSG_LOOP        ;Branch back until null found
HALT_CPU
        WAI                     ;Halt CPU, force user interaction
;
BAD_REC_MSG
        .DB     13,10,'Bad Partition Record',13,10,0  ;Bad partition record message
;
NO_ACT_PART
        .DB     13,10,'No Active Partition',13,10,0   ;No active partition message
;
BAD_BLOCK
        .DB     13,10,'Bad Boot Block!',13,10,0 ;Bad boot block message
;
COPYRIGHT
        .DB     'K.E. Maier'
;
        .ORG    $10FC           ;Offset for 2-byte signature
;
; The C02BIOS will test for the following 2-byte signature!
; As a disk might have been attached with a typical PC (aka Intel) partition record, we don't
; want to attempt executing Intel code by accident.
;
; The 2-byte signature validates the partition record is for 65XX code execution. If this is
; not found, the C02Monitor will show an error message and warm boot itself.
;
        .DW     $6502           ;Litte-Endian signature for 6502 partition
;
;**************************************************************************************************
; 
;Partition Records are 16 bytes in length and have the following format:
;
;       Offset          Length          Description
;       0x00            1 byte          Status: bit 7 used for active (1), all other bits zero
;       0x01            3 bytes         CHS address of first sector in partition
;       0x04            1 byte          Partition Type: "db" is for CPM
;       0x05            3 bytes         CHS addres of last sector in partition
;       0X08            4 bytes         LBA of first Block in partition
;       0x0C            4 bytes         Number of Blocks in partition
;
; note: if LBA addressing is used, both CHS fields should be zeroed out!
;
; For Partitioning, LBA will is used, as the BIOS only supports LBA addressing for the drive.
;
;**************************************************************************************************
;
;       .ORG    $10FE           ;Offset to boot records
;
BOOT_RECORD                     ;Start of Boot records
;
;Partition Records start here:
; - The first 12 records are Expanded Partition Entries per Ontrack Disk Manager
; - The last 4 records are the standard Primary partition Entries
;
Partition_0x04
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x05
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x06
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x07
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x08
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition

;
Partition_0x09
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x0A
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x0B
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x0C
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x0D
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x0E
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x0F
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   $0              ;First LBA Block in partition
        .LONG   $0              ;Number of Blocks in partition
;
Partition_0x00
;
        .DB     %10000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   64              ;First LBA Block in partition
        .LONG   262144          ;Number of Blocks in partition
;
Partition_0x01
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   262208          ;First LBA Block in partition
        .LONG   262144          ;Number of Blocks in partition
;
Partition_0x02
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   524352          ;First LBA Block in partition
        .LONG   262144          ;Number of Blocks in partition
;
Partition_0x03
;
        .DB     %00000000       ;Bit Mask for active partition bit 7 shows as active
        .DB     $00, $00, $00   ;First CHS Field - zeros as LBA mode is used
        .DB     $DB             ;CPM Partition identifier
        .DB     $00, $00, $00   ;Last CHS Field - zeros as LBA mode is used
        .LONG   786496          ;First LBA Block in partition
        .LONG   262144          ;Number of Blocks in partition
;
;**************************************************************************************************
; Partition Block ends with standard 2-byte signature to show valid record
;
        .DW     $AA55           ;Signature bytes - mark as valid partition record
;
;**************************************************************************************************
        .END
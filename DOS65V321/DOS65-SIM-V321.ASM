;
;----------------------------------------
;DOS/65 system interface module (SIM)
;----------------------------------------
;
; Version 3.21 - 6th March 2025
;
;This version is designed to work with the C02 Pocket SBC with RTC/CF-Card adapter:
; - modified for 3.3V for 35-pin PATA MicroDrive, SC28L92 DUART and DS1318 RTC
; - note that RTC is not currently used by DOS/65, but supported in C02BIOS 5.0
;
;Note: Updated disk configuration to 4KB block sizes.
; Allocation tables are now half the size, 256 bytes vs 512 bytes.
;
;Microdrive Disk configuration:
; a 8MB
; b 8MB
; c 8MB
; d 8MB
; e 8MB
; f 8MB
; g 8MB
; h 8MB
;
;This SIM version no longer uses Track/Sector parameters to access the Microdrive!
; - All calls and paraneters from PEM are now based on Record numbers.
; - Records are still set as 128-bytes per record.
; - Record number is a 16-bit value (0-65,535), so an 8MB limit still exists.
;
;The Disk Control Block parameter table has been modified.
; This is done to accomodate the change from Track/Sector parameters to records, and
; eliminates the checksum table, as IDE and other nonremovable block devices don't require it.
; The new DCB table entry is configured as:
;
; Note: example shows an 8MB drive setup.
; - A total of 2048 blocks, where each block is 4096 bytes
; - the number of records per physical block is 4 (4*128=512)
; - the total number of reserved records is 256 (256*128=32,768 bytes)
; - Note: Reserved Records must be a multiple of 4!
;
;     .DW     2039            ;max block number
;     .DW     4               ;records per physical block (LBA)
;     .DW     256             ;number of reserved records (OS Boot area)
;     .DB     2               ;block size = 4096
;     .DW     1023            ;max directory
;     .DW     almpa           ;address of allocation map
;
;A modified PEM module is required to use this new SIM version.
; PEM 3.20 and later also contains the changes for using Record numbers only!
;
;Note:  02 January 2024 Update!
; SIM Version 3.21 now receives Reserved Record count from PEM!
; this requires that SIM calculates this into the Record request from PEM.
; this calculation is done in the "get_lba" routine.
; - this saves space and execution time in PEM.
; - this adds some space to SIM but additional execution time in SIM is less overall.
;
;Note: This SIM version makes calls to the C02BIOS directly!
; This results in a smaller code size and one less module to write and maintain.
;
;As this SIM supports a prototype C02 Pocket, the following devices exist:
; - 35-pin PATA Microdrive (IDE Drive) - supports 8 drives at 8MB each
; - Dual port UART - provides console and reader/punch functions
; - Realtime Clock - not currently supported/used
;
;Note that this SIM version uses an additional 4-byte offset for LBA accesses!
; - This is required due to being a bootable system. The Microdrive uses a Partition Record
; - which will load the Boot Block from the active partition. The Boot Block and Boot Image are
; - part of the allocated disk space that becomes the drive.
;
; - The Boot Block code will use the Load Header at the start of drive image to provide the offset
; - to allocated space and update the Partition Offset variable in this SIM module (part_offset).
; - This design allows the Boot Image to be easily moved to a new location on the drive and not
; - have to worry about the disk access. Only the Boot Block needs to be updated.
;
;**************************************************************************************************
;Include required constants/equates/tables for SIM to assemble
;
        NOLIST
        INCLUDE         C02Constants_5.asm      ;Constants/Equates - C02 BIOS/Monitor/Hardware
        INCLUDE         C02JMP_Table_5.asm      ;Jump Table entries for C02 BIOS/Monitor
        LIST
;
;**************************************************************************************************
;
;Main program, Start of SIM
; this is the cold start for DOS/65. The boot block code jumps to here.
;
; Note: The following calls are no longer needed or supported, but are still listed in SIM to
;       maintain the SIM functions at the correct address offsets.
;       Also, the Console Definition Table has a hard-coded section within SIM.
;
;       SIMRAM+24: Set Home Drive
;       SIMRAM+51: Translate Sector
;
; Set Home Drive is specific to diskette based media.
; - As multiple drive letters now exist on a single block device, there's no need to
;   tell the drive to go to home (which would be cylinder zero).
; 
; Translate Sectors is obsolete.
; - There are no diskette drives supported in this version of DOS/65.
;
; Note: Printer Output is not supported, but could be using additional hardware if required.
;
simram          jmp     sim_boot        ;Cold start SIM-DOS/65  simram+0 (3)
sim_wboote      jmp     sim_wboot       ;Warm boot DOS/65       simram+3 (3)
                jmp     mon_consts      ;Check for input        simram+6 (3)
                jmp     mon_conrde      ;Get input              simram+9 (3)
                jmp     mon_conwrt      ;Send to terminal       simram+12 (3)
                jmp     print_output    ;Printer output         simram+15 (3)
                jmp     mon_punch       ;Punch output           simram+18 (3)
                jmp     mon_reader      ;Reader input           simram+21 (3)
                jmp     dummy           ;Set home drive         simram+24 (3)
                jmp     sim_seldsk      ;Select active disk     simram+27 (3)
                jmp     sim_setres      ;Set reserved records   simram+30 (3)
                jmp     sim_setrec      ;Set record number      simram+33 (3)
                jmp     sim_setdma      ;Set buffer address     simram+36 (3)
                jmp     sim_read        ;Read PEM record        simram+39 (3)
                jmp     sim_write       ;Write PEM record       simram+42 (3)
                jmp     print_status    ;Printer always ready   simram+45 (3)
                jmp     rtc_read        ;Clock entry            simram+48 (3)
                jmp     dummy           ;Translate (obsolete)   simram+51 (3)
;
;console definition block - simram+54
                .DB     0               ;Scratch
sysdef          .DB     8               ;Backspace
                .DB     1               ;Clear to end of line
                .DB     $0C             ;Forward space
                .DB     0               ;Normal video
                .DB     '^'             ;Invert video
                .DB     24              ;Lines per screen
                .DB     80              ;Char per line
                .DB     $0C             ;Form feed
                .DB     $1E             ;Home
                .DB     2               ;Clear to end of screen
;
dummy           rts                     ;Return (6)
;
;SIM Messages
sim_opnmsg      .DB     CR,LF,"SIM Version 3.21"
                .DB     CR,LF,"DOS/65 Version 3.21",0
;
io_err_msg      .DB     CR,LF,"Disk I/O Error!",0
;
warm_msg        .DB     CR,LF,"*",0
;
;Start of SIM cold boot
sim_boot        jsr     b_ide_reset     ;Reset Microdrive (safety) (6)
;
                lda     #<sim_opnmsg    ;Point to message (2)
                ldy     #>sim_opnmsg    ; (2)
                jsr     mon_prompt      ;Send it to console (6)
;
                ldx     #$FF            ;Set stack pointer (2)
                txs                     ; to top of stack space (2)
;
; To improve Disk I/O performance, the partition offset will be added
; to the drive offsets. This has to be a one-time calculation when
; SIM is started, otherwise the disk offset will be added each time
; SIM is reset, which doesn't work! This will take some additional
; memory space, but saves execution time in the get_lba routine.
; Once added in, the partition offset values below are zeroed out.
;
                ldx     #0              ;Zero index for drive offset (2)
                ldy     #maxdrv         ;Load index for Max drive count (2)
drv_update_lp
                clc                     ;Clear carry for add (2)
                lda     drv_offset+0,x  ;Get the drive offset (4)
                adc     part_offset+0   ;Add the partition offset (4)
                sta     drv_offset+0,x  ;Save it back (5)
;
                lda     drv_offset+1,x  ;Get the drive offset (4)
                adc     part_offset+1   ;Add the partition offset (4)
                sta     drv_offset+1,x  ;Save it back (5)
;
                lda     drv_offset+2,x  ;Get the drive offset (4)
                adc     part_offset+2   ;Add the partition offset (4)
                sta     drv_offset+2,x  ;Save it back (5)
;
                inx                     ;Increment drive offset (2)
                inx                     ; by 4 bytes per drive (2)
                inx                     ; (2)
                inx                     ; (2)
;
                dey                     ;Decrement drive count (2)
                bne     drv_update_lp   ;Loop back for all drives (2/3)
;
;Now that all drive offsets have been updated with the partition offset,
; we will zero out the partition offset data (which is in RAM) so that it
; can't be added in again ;-) Ah... RAM based routines!
;
                stz     part_offset+0   ;Zero out Partition Offset (4)
                stz     part_offset+1   ; (4)
                stz     part_offset+2   ; (4)
                stz     part_offset+3   ; (4)
;
;Initialize iostat - resets to user 0 and drive 0
; Note: this can be changed to a specific drive/user if needed.
                stz     iostat          ;Clear I/O status (4)
;
;SIM Setup
;First clear disk access flags
                stz     sim_recflg      ;Clear SIM record flag byte (3)
;
;Setup PEM entry in Page 1
; note that a warm boot also branches here to finish up
sim_wsetup
                ldy     #6              ;Set index to 6 (2)
simset_lp       lda     sim_inttbl-1,y  ;Get SIM table data (4)
                sta     $0100-1,y       ;Store into page $01 PEM area (5)
                dey                     ;Decrement index (2)
                bne     simset_lp       ;Loop until done (2/3)
;
;Setup default buffer for PEM (128 bytes)
sim_finish
                lda     #<dflbuf        ;Get default buffer (2)
                ldy     #>dflbuf        ; address (2)
                jsr     sim_setdma      ;Then set DMA for DOS/65 (6)
;
;Setup the BIOS for the default host buffer and block count.
; The BIOS reads or writes a single 512-byte block into it's own
; Host Buffer. The SIM code calls the C02 Pocket BIOS for all hardware
; access.
                lda     #<hstbuf        ;Get host buffer address (2)
                ldy     #>hstbuf        ; high byte (2)
                ldx     #1              ;Block count =1 (2)
                jsr     b_ide_set_addr  ;Call BIOS (6)
;
                jmp     ccmram          ;Then jump to CCM to complete startup (3)
;
;Initialization table
sim_inttbl      .DB     $4C             ;JMP opcode
                .DW     sim_wboote      ;Warm Boot location
                .DB     $4C             ;JMP opcode
                .DW     pemram          ;PEM RAM location
;
;SIM Warm Boot - When any application exits, it should make a call to Warm Boot
; the system. That call will be handled here. As CCM can be over-written by any
; application to access additional memory if/when needed, the warm boot routine
; is responsible for reloading CCM. The updated 3.21 Version does not require
; that PEM is reloaded, so that can save a small fraction of time for warm boot.
; note that SIM should never be reloaded once DOS/65 is booted.
;
;NOTE: IOSTAT and hence the default drive/user are not altered by Warm Boot.
; After CCM is reloaded, the current user/drive will be restored to the
; same as before a warm boot was initiated.
;
;Clear out the user space in Page Zero, this can cause intermittant crashes on
; a warm start if leftover data from the last app is present.
; Apps like the Basic Compiler and Runtime are good examples.
; Granted, this is likely something specific in CCM, but clearing out page zero
; is just a good idea and can be an aid in any application debugging.
;
sim_wboot
                ldx     #pzend-1        ;Get page zero end of space as index (2)
clr_pg0
                stz     $00,x           ;Clear page zero user space (4)
                dex                     ;Increment index (2)
                bne     clr_pg0         ;Loop back till done (2/3)
;
; X Reg = 0 from above
                dex                     ;Decrement X to $FF (2)
                txs                     ;To top of stack - safety (2)
;
;Show a simple asterisk when a warm boot is executed
                lda     #<warm_msg      ;Point to message (2)
                ldy     #>warm_msg      ; (2)
                jsr     mon_prompt      ;Send it to console (6)
;
;As this is a bootable version of DOS/65, we need to reload CCM from disk for warm boot.
; CCM, PEM, SIM and the (cleared) disk allocation tables are stored in the Reserved Records
; area of the bootable drive partition. This eliminates the need to clear the allocation maps
; when the system is initially booted.
;
;The actual Cold Boot is handled by the Boot Record, which is loaded from the Drive.
; This is the result of the Partition Loader loading and executing the active Boot Record.
; All of this is setup outside of DOS/65 and SIM. A separate disk utility is used to setup
; the drive and load the DOS/65 code to the drive in the reserved area of the disk.
;
;As the default boot drive is A:, we only need to load the first set of drive offsets and
; set these up in the BIOS. Note that these values will contain the actual starting LBA
; for the first drive, as the partition offset value has been added to all drive offsets.
;
;As Blocking/De-blocking is handled in SIM, we need to test the Dirty Block flag to see
; if there is a pending block write. If there is, we'll flush the buffer to disk.
;
                bbr6    sim_recflg,no_flush     ;Check dirty block flag, branch if clear (5/6)
;
;SIM Flush Drive
; When PEM calls SIM to read or write a Record, blocking and de-blocking is done
; here in the routines below. To enhance performance for block writes, SIM will
; delay a block write as there are four records per block. If a different record
; is required by PEM, the current disk buffer will be checked via a dirty block
; flag and flushed to disk before loading the new calculated block. It's also
; important to check the dirty block flag via a warm boot, as applications don't
; know about this SIM feature.
;
; Write the current LBA in the bufer to the disk.
;
                jsr     b_ide_write_lba ;Call BIOS to Write LBA (6)
                lda     ide_status_ram  ;Get Status from BIOS call (3)
                lsr     a               ;Shift error bit to carry (2)
                bcs     ide_rw_err      ;Branch if error (2/3)
;
                rmb6    sim_recflg      ;Reset dirty block flag (5)
;
no_flush
                lda     drv_offset+0    ;Get LBA parameters (4)
                ldy     drv_offset+1    ; (4)
                ldx     drv_offset+2    ; (4)
                jsr     b_ide_set_lba   ;Call BIOS to set parameters (6)
;
;Next, set the load address for BIOS to reload the CCM module.
; The X reg contains the number of blocks to load. CCM starts at
; "ramstrt", which is defined in the main source file.
; CCM uses 10 pages (256 bytes per page), 2560 bytes, which is 5 blocks from the Microdrive.
;
                lda     #<ramstrt       ;Get load address lo (2)
                ldy     #>ramstrt       ;Get load address hi (2)
                ldx     #5              ;Get block count (2.5KB for CCM only) (2)
                jsr     b_ide_set_addr  ;Call BIOS to set address and size (6)
                jsr     b_ide_read_lba  ;Call BIOS to load CCM to memory (6)
                lda     ide_status_ram  ;Get Status from BIOS call (3)
                lsr     a               ;Shift error bit to carry (2)
                bcc     sim_wsetup      ;If no error, reset PEM and start CCM (2/3)
ide_rw_err
                jmp     get_lba_err     ;Else, use common error routine (3)
;
;**************************************************************************************************
;
;Console Input and Output Routines
; There are three routines for Console I/O
; 1- CONSTS - Console Status: tests to see if there is a character available
; 2- CONRDE - Console Read: waits for a character to be available, then returns with it
; 3- CONWRT - Console Write: sends a character to the console
;
;The console status routine does NOT read any data from the console itself.
; It just checks to see if data has been entered. If no data is waiting, the A Reg has to be
; zero. If data is waiting, the A Reg has to be a non-zero value, hence any or all bits set
; active will work. The BIOS uses the page zero variable icnt_a to track the data count in
; the input buffer. By simply loading the A Reg from icnt_a, the A reg will be either zero
; or have a non-zero value. Note that the Z flag is also set if icnt_a is zero.
;
; Note: C02BIOS5 will include a routine to call for this function. This will eliminate the SIM
; module from accessing BIOS variables directly.
;
mon_consts
                jmp     b_chr_stat      ;check console status
;
;The console read routine calls the C02BIOS routine to get a character from the console.
; As the C02BIOS can transfer 8-bit binary data, the upper bit is masked off to ensure ASCII
; only. Also, the standard Delete key code from the console is a Backspace ($08). The PEM
; routine checks for the Delete key, so this routine will check for Backspace and replace it
; with a Delete code.
;
mon_conrde
                jsr     b_chrin         ;Call BIOS for character in (waits) (6)
                and     #$7F            ;Strip off high bit (ASCII only) (2)
                cmp     #bs             ;Check for Backspace character (2)
                bne     conrdx          ;If not, branch (2/3)
                lda     #delete         ;Else, replace with Delete code (2)
conrdx          rts                     ;Return to Caller (6)
;
;The console write routine is straightforward. As per the System Interface Guide, the SIM
; routine does not need to preserve the character being written to the console. We only take the
; approach of stripping off the high bit to ensure ASCII only.
;
mon_conwrt
                and     #$7F            ;Strip off high bit for ASCII (2)
                jmp     b_chrout        ;Send to BIOS and return (3)
;
;Prompt routine for sending a string to the console:
; To eliminate the ROM Monitor routine usage, this routine is the same code to
; send a null terminated string to the console using the BIOS chrout call.
;
mon_prompt      sta     b_stringl       ;Store low byte (3)
                sty     b_stringh       ;Store high byte (3)
;
;mon_prompt_lp routine: prints message at address (B_STRINGL) till null character found
mon_prompt_lp   lda     (b_stringl)     ;Get string data (5)
                beq     conrdx          ;If null character, exit (borrowed RTS) (2/3)
                jsr     b_chrout        ;Send character to terminal (6)
                inc     B_stringl       ;Increment low byte index (5)
                bne     mon_prompt_lp   ;Loop back for next character (2/3)
                inc     B_stringh       ;Increment high byte index (5)
                bra     mon_prompt_lp   ;Loop back and continue printing (3)
;
;**************************************************************************************************
;
;Additional routines for using the second serial port via C02BIOS routines
; these are simple binary passthru routines, no data input or output is modified.
;
mon_punch
                jmp     b_chrout2       ;Send data to serial port 2 (3)
;
mon_reader
                jmp     b_chrin2        ;Get data from serial port 2 (3)
;
;**************************************************************************************************
;
;Place holder for Printer Output routines
; these are just simple dead end routines. One for Printer Status always showing available and
; the second for printer output that just return without doing anything.
;
print_status
                lda     #1              ;Get active flag status (2)
print_output
                rts                     ;Return (6)
;
;**************************************************************************************************
;
;Place holder for realtime clock routines
; This routine simply returns with #128 in the X register, showing no clock present/
;
rtc_read
                ldx     #128            ;Get active flag status (2)
                rts                     ;Eeturn (6)
;
;**************************************************************************************************
;Start of Disk I/O Routines:
; Note that these routines rely on the C02BIOS calls for physical access of the Microdrive!
; 
; Blocking/De-blocking is always used for IDE devices (default LBA size = 512 bytes)
; All drives are configured with the same defaults as:
;
; - 2048 total blocks
; - 4 records per LBA (drive physical block size)
; - 32 records per block (DOS/65 Allocation size per extent)
;
; Records is a 16-bit field and record lengths are 128-bytes. These routines provide a simple
; translation layer that will read and write blocks from the Microdrive. Each 512-byte block
; contains 4- 128-byte records. This calculates to a total of 8MB of disk space allocated via
; the 64K of records per drive letter.
;
; There are several routines which are required to support the SIM functions
;
;Record Move routines
; there are two sets of Record Move routines;
; - set one moves a Record from the LBA buffer to the DMA address specified by PEM
; - set two moves a Record from the DMA address specified by PEM to the LBA buffer
; In short, Set one is for reading a Record from disk and moving to memory, while
; set two is for writing a Record from memory and moving to disk.
;
;Select disk
; drive range is now validated by PEM before making this SIM call.
;
sim_seldsk
                sta     sim_seldrv      ;Save as current drive (4)
                asl     a               ;Multiply by two (2)
                tax                     ;Make an index (2)
                lda     sim_dcbtbl,x    ;Get DCB address (4)
                ldy     sim_dcbtbl+1,x  ; in A/Y (4)
                rts                     ;Return (6)
;
;Set DOS/65 memory buffer address
;
sim_setdma
                sta     sim_dmaadr      ;Save address low (4)
                sty     sim_dmaadr+1    ;Save address high (4)
                rts                     ;Return (6)
;
;Set Requested Reserved Records
; Note that PEM uses the Reserved Records from the DCB for the active drive.
;
sim_setres
                sta     sim_resrec      ;Save reserved record low (4)
                sty     sim_resrec+1    ;Save reserved record high (4)
                rts                     ;Return (6)
;
;Set Requested Record
; Note that PEM does NOT add the Reserved Record count to the requested Record.
; this is handled in the get_lba routine below.
;
sim_setrec
                sta     sim_pemrec      ;Save record low (4)
                sty     sim_pemrec+1    ;Save record high (4)
                rts                     ;Return (6)
;
;*************************************************************************************************
;
;Read the selected DOS/65 record.
;
sim_read
                smb5    sim_recflg      ;Set read op flag (5)
                jsr     get_lba         ;Calculate/get LBA to read (6)
;
;Transfer the record from buffer to DMA
;
                lda     sim_recflg      ;Get recflg (4)
                and     #%00000011      ;Mask for lower 2-bits (2)
                asl     a               ;Multiply by 2 for index (2)
                tax                     ;Xfer to X reg as index (2)
;
                lda     buff_offset,x   ;Get buffer offset address (4)
                ldy     buff_offset+1,x ; (4)
                sta     mover+1         ;Save to routine lo (4)
                sty     mover+2         ;Save to routine hi (4)
;
                lda     sim_dmaadr+0    ;Get dma address lo (4)
                ldy     sim_dmaadr+1    ;Get dma address hi (4)
                sta     mover+4         ;Save to routine lo (4)
                sty     mover+5         ;Save to routine hi (4)
                ldy     #0              ;Zero index (2)
;
mover
                lda     $FFFF,y         ;Get from source address (4)
                sta     $FFFF,y         ;Move to destination address (5)
                iny                     ;Increment offset (2)
                bpl     mover           ;Loop back until done (2/3)
;
                rmb5    sim_recflg      ;Reset read op flag (5)
                lda     #0              ;Zero for no error (2)
                rts                     ;Return (6)
;
;Write the selected DOS/65 record.
;
sim_write
                smb4    sim_recflg      ;Set write op flag (5)
                jsr     get_lba         ;Calculate/get LBA to write (6)
;
;Transfer the record from DMA to buffer
;
                lda     sim_recflg      ;Get recflg (4)
                and     #%00000011      ;Mask for lower 2-bits (2)
                asl     a               ;Multiply by 2 for index (2)
                tax                     ;Xfer to X reg as index (2)
;
                lda     buff_offset,x   ;Get buffer offset address (4)
                ldy     buff_offset+1,x ; (4)
                sta     movew+4         ;Save to routine lo (4)
                sty     movew+5         ;Save to routine hi (4)
;
                lda     sim_dmaadr+0    ;Get dma address lo (4)
                ldy     sim_dmaadr+1    ;Get dma address hi (4)
                sta     movew+1         ;Save to routine lo (4)
                sty     movew+2         ;Save to routine hi (4)
                ldy     #0              ;Zero index (2)
;
movew
                lda     $FFFF,y         ;Get from source address (4)
                sta     $FFFF,y         ;Move to destination address (5)
                iny                     ;Increment offset (2)
                bpl     movew           ;Loop back until done (2/3)
;
                smb6    sim_recflg      ;Set dirty block flag (5)
;
                rmb4    sim_recflg      ;Reset write op flag (5)
                lda     #0              ;Zero for no error (2)
                rts                     ;Return to caller (6)
;
;**************************************************************************************************
;
;Get LBA routine
; This is a core routine for SIM and all disk level activity. Now that PEM
; and SIM have been changed to a Record level access, the conversion between
; 128-byte records and 512-byte LBA block sizes can be streamlined.
;
;This routine is common for both Reading and Writing of Records. As there
; is still a requirement to perform blocking/de-blocking from PEM Records
; to disk block sizes, this routine will perform the following tasks:
;
; 1- Add the Reserved Record count to the Requested Record number.
; 2- Calculate the Block Number from the Record number.
; 3- Save the Record offset within the calculated block number.
; 4- Check to see if a valid disk block is loaded in the buffer.
; 5- Perform a block read if there is no valid block loaded.
; 6- Test the valid block loaded against the required block number.
; 7- Test for a pending write for the valid block number.
; 8- Write a pending block and clear the dirty block flag if required.
; 9- Only load a block if needed, else return without disk I/O access.
;
;This is a fair amount of work, but this core routine ensures we have the
; correct block in the default disk buffer on which to read or write
; records to or from. This routine also simplifies the Record Read and Write
; routines to focus only on handling the required 128-byte record.
;
;Another advantage is that this routine will only load a new block when
; it needs to, i.e., if the calculated block is already loaded, it will
; not reload it, but simply return and allow the Record Read and Write
; routines to carry out their record level function.
; This improves the overall disk I/O performance.
;
;Calculate the block number (LBA) based on the Record number.
; The Record number is a 16-bit value (0-65,535) and is 128-bytes in size.
; To calculate the LBA, we simply divide it by 4, as there are 4 records
; per 512-byte block (LBA). We also save the lower 2-bits as this becomes
; the offset of the requested record into the block (as binary, 00,01,10,11).
;
;First, we need to take the Record Number and do the following:
; 1- Save the Record offset (the lower 2-bits of the Record number).
; 2- Add the Reserved Records value to the requested Record.
; 2- Perform a 16-bit Logical Right Shift by 2-bits (divide by 4).
; 3- Add the Drive number offset to the required LBA.
; 4- Save the calculated LBA, then check the flag byte for an LBA loaded.
;
;Second, we need to test for the following conditions:
; 1- If a LBA is loaded, check to see if it is the same LBA that we need.
; 2- If it is, just exit back to the calling routine, saving a disk read.
; 3- If it it NOT, check the dirty block flag in case it's been altered.
; 4- If the dirty block flag is set, write the current block and clear the flag,
;    then update the parameters and load the new LBA.
; 5- If the dirty block is not set, update the parameters and load the new LBA.
;
;The C02BIOS used here is Version 4.02.
; This BIOS supports 24-bit LBA addressing.
; Note: IDE uses 28-bit addressing, but the BIOS zeros the upper 4-bits,
; resulting in a limit of 8GB for the attached IDE device.
;
;The SIM configuration is set for 8 drives (A thru H), each at 8MB.
; Any of the defined drives can have Reserved Records, but in practical
; use, only one drive should have Reserved Records to be use for holding
; the bootable image for the OS (CCM, PEM, SIM, Allocation Tables).
; The first drive will normally have the Reserved Records to hold the
; bootable OS image, where a total of 256 Records will equate to 32KB.
;
;Once the Block number has been calculated from the Record number,
; an offset must be added which is based on the Drive number (0-7).
; Partition sizes are limited to 8MB
; Number of drives is limited to 8 (A - H)
; Maximum available Data is limited to 64MB
;
;The Offset table is 4 bytes per entry and adds an LBA count to
; shift the absolute LBA numbers to the appropriate drive number
; being acessed (16384 blocks per 8MB drive area).
;
;First, calculate the record number to LBA number and
; store the LBA number to sim_pemrec (3 bytes).
;
get_lba
                lda     #%00000011      ;Get mask for record offset bits (2)
                trb     sim_recflg      ;Reset record offset bits in flag (5)
;
; next, we need to add the reserved record value to the PEM record requested.
; the result will be stored in rectemp with high byte cleared.
;
                clc                     ;Clear carry flag for addition (2)
                lda     sim_pemrec+0    ;Get low byte PEM record (4)
                adc     sim_resrec+0    ;Add in Reserve count low (4)
                sta     sim_rectemp+0   ;Store to temp low byte (4)
                lda     sim_pemrec+1    ;Get high byte PEM record (4)
                adc     sim_resrec+1    ;Add in Reserve count high (4)
                sta     sim_rectemp+1   ;Store in temp high byte (4)
                stz     sim_rectemp+2   ;Clear extended temp byte (4)
;
                lda     sim_rectemp+0   ;Get low byte of requested record (4)
                and     #%00000011      ;Mask off upper 6 bits (2)
                ora     sim_recflg      ;OR in other record flag bits (3)
                sta     sim_recflg      ;Update the record flag with new offset (3)
;
; now do the 16-bit shift to the right to divide by 4 and get the LBA number
;
                lsr     sim_rectemp+1   ;Shift high to right, LSB to carry (6)
                ror     sim_rectemp     ;Rotate low to right, MSB from carry (6)
                lsr     sim_rectemp+1   ;Shift high to the right, LSB to carry (6)
                ror     sim_rectemp     ;Rotate low to right, MSB from carry (6)
;
; the sim_rectemp pointer now has the logical LBA number requested by PEM.
; the sim_recflg also contains the record offset in the lower 2 bits,
; plus the other flags, which have been preserved (very important).
;
; Note that the partition offset (common for all drives) has been added as
; a one-time calculation during the first startup of SIM. As all drive
; offsets have been updated with the parition offset, we only need to
; apply the drive offset based on the current drive A-H (0-7).
;
                lda     sim_seldrv      ;Get active drive number (4)
                asl     a               ;Multiply by two (2)
                asl     a               ;Multiply by two (now = 4) (2)
                tax                     ;Xfer offset to X reg (2)
;
                clc                     ;Clear Carry for Add (2)
                lda     sim_rectemp+0   ;Get Low order LBA (4)
                adc     drv_offset+0,x  ;Add in Low order offset (4)
                sta     sim_rectemp+0   ;Save to Low order LBA (4)
;
                lda     sim_rectemp+1   ;Get Middle order LBA (4)
                adc     drv_offset+1,x  ;Add in Middle order offset (4)
                sta     sim_rectemp+1   ;Save to Middle order LBA (4)
;
                lda     sim_rectemp+2   ;Get High order LBA (4)
                adc     drv_offset+2,x  ;Add in High order offset (4)
                sta     sim_rectemp+2   ;Save to High order LBA (4)
;
; now, the actual required LBA has been calculated and is stored as a 24-bit value.
; this is stored in variable "sim_rectemp" (3 consecutive bytes).
; We now need to compare the actual LBA needed to the currently loaded LBA, if there
; is one. We first test for a valid LBA loaded by checking bit7 of the sim_recflg.
;
                bbr7    sim_recflg,need_lba     ;Test sim_recflg for valid LBA (5/6)
;
; we have determined that a valid LBA is loaded, so we need to compare the LBA
; that's been requested against the currently loaded LBA. If they are the same, we're
; done and can return. If they are not, then we need to check the dirty block flag to
; ensure the active block is written before we over-write it with the new one.
;
                 lda    sim_rectemp+0   ;Get low order requested LBA (4)
                 cmp    sim_curlba+0    ;Compare to current LBA (4)
                 bne    test_dirty      ;Bad compare, test for dirty block (2/3)
                 lda    sim_rectemp+1   ;Get middle order requested LBA (4)
                 cmp    sim_curlba+1    ;Compare to current LBA (4)
                 bne    test_dirty      ;Bad compare, test for dirty block (2/3)
                 lda    sim_rectemp+2   ;Get high order requested LBA (4)
                 cmp    sim_curlba+2    ;Compare to current LBA (4)
                 bne    test_dirty      ;Bad compare, test for dirty block (2/3)
                 rts                    ;Good compare, requested LBA is loaded (6)
;
;Test the dirty block flag.
; the currently loaded LBA is not the one we need loaded. Before we
; overwrite it, we'll test the dirty block flag. If it's set,
; we'll write that block to disk to update it, clear the flag
; then transfer the parameters for the new LBA and load it.
;
test_dirty
                bbr6    sim_recflg,need_lba     ;Test sim_recflg for dirty block flag (5/6)
;
; we have a dirty block in the buffer... so we need to write the current LBA back
; to the drive to ensure we don't get corrupted data on the drive!
; note that the existing BIOS parameters still point to the currently loaded LBA.
;
                jsr     b_ide_write_lba ;Call BIOS to Write LBA (6)
                lda     ide_status_ram  ;Get Status from BIOS call (3)
                lsr     a               ;Shift error bit to carry (2)
                bcs     get_lba_err     ;Branch if error (2/3)
                rmb6    sim_recflg      ;Reset dirty block flag (5)
;
; now that the dirty block has been cleared, we can load the required LBA
; into the disk buffer, update the flags and return.
;
; if we branched here from above, no valid LBA is loaded, so we just transfer the
; LBA parameters to the BIOS and read the block into the default LBA buffer.
;
need_lba
                lda     sim_rectemp+0   ;Get required LBA parameters (Low byte) (4)
                ldy     sim_rectemp+1   ; Middle byte (4)
                ldx     sim_rectemp+2   ; High byte (4)
;
                sta     sim_curlba+0    ;Save as Current LBA (4)
                sty     sim_curlba+1    ; for checking the next (4)
                stx     sim_curlba+2    ; time get_lba is called (4)
;
                jsr     b_ide_set_lba   ;Call BIOS to set LBA parameters (6)
;
                jsr     b_ide_read_lba  ;Call BIOS to Read LBA (6)
                lda     ide_status_ram  ;Get Status from BIOS call (3)
                lsr     a               ;Shift error bit to carry (2)
                bcs     get_lba_err     ;Branch if error (2/3)
                smb7    sim_recflg      ;Set valid LBA flag bit (5)
                rts                     ;Return to caller, we're done (6)
get_lba_err
                rmb7    sim_recflg      ;Reset valid LBA flag bit (5)
                lda     #<io_err_msg    ;Point to error message (2)
                ldy     #>io_err_msg    ; (2)
                jsr     mon_prompt      ;Send it to console (6)
                jmp     sim_boot        ;Restart SIM (3)
;
;**************************************************************************************************
;SIM Data area
; All data pointers used by SIM disk routines are contained here. This includes:
;  - Disk Contrl Block (DCB) addresses
;  - Buffer offset addresses for Record to LBA Buffer
;  - Parameter and Flags used for Record and LBA pointers
;
;Table of dcb addresses.
; All 8 drives are accounted for, if not present, use a word value of "0000".
;
sim_dcbtbl      .DW     dcb_a
                .DW     dcb_b
                .DW     dcb_c
                .DW     dcb_d
                .DW     dcb_e
                .DW     dcb_f
                .DW     dcb_g
                .DW     dcb_h
;
;Buffer Offsets.
; The offsets here point to the 4- 128-byte Records in the main Disk Buffer.
;
buff_offset
                .DW     LBA_BUFFER+0    ;Buffer offset 00
                .DW     LBA_BUFFER+128  ;Buffer offset 01
                .DW     LBA_BUFFER+256  ;Buffer offset 10
                .DW     LBA_BUFFER+384  ;Buffer offset 11
;
;Record level data area required by SIM
sim_seldrv      DEFS    1               ;Current selected drive number (0-7)
sim_dmaadr      DEFS    2               ;DMA address to Record transfer
sim_pemrec      DEFS    2               ;Record number requested by PEM
sim_resrec      DEFS    2               ;Reserved Records received from PEM
sim_rectemp     DEFS    3               ;Calculated LBA required by PEM
sim_curlba      DEFS    3               ;LBA number currently loaded in Buffer
;
;**************************************************************************************************
;
;Table of LBA offsets for all drives A thru H (8MB each, 16384 Blocks per drive)
;
;NOTE: These offsets are based on a non-partitioned drive!!! Relative to each other only!
; The partition offset below as "part_offset" is added to the "drv_offset" values the first time
; SIM is started. To ensure this only happens once, the "part_offset" data is zeroed out after it
; is added to the "drv_offset". Once the new values are loaded, the active drive being accessed
; is used as a base offset and the block offset calculated by the requested Record from PEM is
; added to get the actual 512-byte block address on the drive.
;
drv_offset
                .LONG   0               ;Drive A default (w/OS boot image)
                .LONG   16384           ;Drive B
                .LONG   32768           ;Drive C
                .LONG   49152           ;Drive D
                .LONG   65536           ;Drive E
                .LONG   81920           ;Drive F
                .LONG   98304           ;Drive G
                .LONG   114688          ;Drive H
;
;Partition Offset
; this offset is patched in during boot time from the boot loader.
; this compensates for the drive partition being located anywhere on the drive.
;
part_offset
                .LONG   131072          ;Partition offset for drive letters
;
;**************************************************************************************************
;
;Disk Control Blocks (DCBs)
; Support for 8 drives is the default.
;
; Note: The Disk Control Block format has been changed starting with Version 3.20!
;       The second parameter is now defined as the number of records per block.
;       - this is the physical block size of the storage device. For any standard IDE device,
;         the default block size is 512 bytes. This calculates to 4 records per block.
;       The third parameter is now defined as the number of reserved records (128 bytes/record).
;       The last two parameters have been deleted! These last two parameters are not needed
;       for a standard block device, as the device itself can not be removed or ejected.
;
; The changes here frees up 3 bytes per DCB. Also note that the code that supports the checksum
; functions in PEM have also been eliminated. This frees up space in PEM and also provides
; a very slight performance improvement.
;
;drive a (Microdrive 8MB)
dcb_a           .DW     2039            ;Max block number-1
                .DW     4               ;Records per physical block
                .DW     256             ;Number of reserved records (32KB OS-boot)
                .DB     2               ;Block size = 4096
                .DW     1023            ;Max directory-1
                .DW     almpa           ;Address of allocation map
;
;drive b (Microdrive 8MB)
dcb_b           .DW     2047            ;Max block number-1
                .DW     4               ;Records per physical block
                .DW     0               ;Number of reserved records
                .DB     2               ;Block size = 4096
                .DW     1023            ;Max directory-1
                .DW     almpb           ;Address of allocation map

;
;drive c (Microdrive 8MB)
dcb_c           .DW     2047            ;Max block number-1
                .DW     4               ;Records per physical block
                .DW     0               ;Number of reserved records
                .DB     2               ;Block size = 4096
                .DW     1023            ;Max directory-1
                .DW     almpc           ;Address of allocation map
;
;drive d (Microdrive 8MB)
dcb_d           .DW     2047            ;Max block number-1
                .DW     4               ;Records per physical block
                .DW     0               ;Number of reserved records
                .DB     2               ;Block size = 4096
                .DW     1023            ;Max directory-1
                .DW     almpd           ;Address of allocation map
;
;drive e (Microdrive 8MB)
dcb_e           .DW     2047            ;Max block number-1
                .DW     4               ;Records per physical block
                .DW     0               ;Number of reserved records
                .DB     2               ;Block size = 4096
                .DW     1023            ;Max directory-1
                .DW     almpe           ;Address of allocation map
;
;drive f (Microdrive 8MB)
dcb_f           .DW     2047            ;Max block number-1
                .DW     4               ;Records per physical block
                .DW     0               ;Number of reserved records
                .DB     2               ;Block size = 4096
                .DW     1023            ;Max directory-1
                .DW     almpf           ;Address of allocation map
;
;drive g (Microdrive 8MB)
dcb_g           .DW     2047            ;Max block number-1
                .DW     4               ;Records per physical block
                .DW     0               ;Number of reserved records
                .DB     2               ;Block size = 4096
                .DW     1023            ;Max directory-1
                .DW     almpg           ;Address of allocation map
;
;drive h (Microdrive 8MB)
dcb_h           .DW     2047            ;Max block number-1
                .DW     4               ;Records per physical block
                .DW     0               ;Number of reserved records
                .DB     2               ;Block size = 4096
                .DW     1023            ;Max directory-1
                .DW     almph           ;Address of allocation map
;
;**************************************************************************************************
;
                END


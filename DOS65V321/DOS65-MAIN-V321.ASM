;
;**************************************************************************************************
;DOS/65 (CMOS) RAM Version
;
;This is Version 3.21 - 6th March 2025 - KM
;This is the bootable image for a RAM loaded version of DOS/65
;
;**************************************************************************************************
; Based on Richard A. Leary's DOS/65 V3 code - many thanks Richard!!
; below is Richard's license text from his V3 ROM version.
;
; The DOS/65 V3 ROM software and all other DOS/65 software are free software;
; you can redistribute it and/or modify it under the terms of the GNU General Public License as
; published by the Free Software Foundation; either version 3 of the license, or any later version.
; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
; See the GNU General Public License for more details.
; You should have received a copy of the GNU General Public License along with this
; program; if not, see <http://www.gnu.org/licenses/>.
; CP/M is a trademark of Caldera.
;
; Note that CCM and PEM have undergone a major rewrite by Kevin E. Maier (me).
; SIM is a completely new version (by me) that interfaces directly to my C02BIOS.
; I also make all of my code available with the same GNU GPL license as above.
;
;**************************************************************************************************
; The target platform for this build is:
;  A prototype 3.3-Volt system based on the C02 Pocket SBC - KM
;  This prototype uses a NXP 28L92 DUART and a Hitachi MicroDrive PATA interface (IDE)
;  The system has been modified for more RAM, now 60KB RAM and 4KB ROM (less I/O), 8MHz CPU
;
;**************************************************************************************************
; Assembler directives
;
                PW      132             ;Page width
                PL      66              ;Page Length
                CHIP    W65C02S         ;Enable WDC 65C02 instructions
                GLOBALS ON              ;
                CASE    OFF             ;Ignore upper vs lower case
                INCLIST ON              ;Include list of include files
;
;**************************************************************************************************
;
; This version uses CMOS instructions and addressing modes!
;
; There are multiple modules that make up this bootable version and use "include" to
; add them into the main code. The Code Modules are:
;       DOS65-MAIN-V321.ASM (This file)
;       DOS65-CCM-V321.ASM - Concurrent Command Module
;       DOS65-PEM-V321.ASM - Primitive Execution Module
;       DOS65-SIM-V321.ASM - System Interface Module
;
; Other files required contain system variables and constants for C02BIOS and C02Monitor,
; which are included in the SIM source module.
;       C02Constants.asm - C02 BIOS/Monitor constants file
;       C02JMP_Table.asm - C02 BIOS/Monitor JMP table for routines
;
;  The current BIOS and Monitor versions for the Prototype system are 5.0
; - Note that SIM is the only code module that calls the C02 BIOS. No C02 Monitors calls are used.
;
;**************************************************************************************************
;
;               18 February 2023 - KM
;Major revision! This is an entirely RAM based version of DOS/65
;
; Based on my 3.20 CMOS RAM Version
; - All data areas are included in the linked modules
; - Allocation Maps for 8 drives are zeroed and adds 2KB to the 6KB of DOS/65 code
; - Supports 8- 8MB drives configured on the Microdrive.
;
; CCM and PEM modules have been reworked for RAM based and include data areas.
; - Page Zero usage has been reduced - currently, 8 bytes are used.
;
;               03 March 2023 - KM
; Major updates to this version, now V3.20:
; - Traditional Track/Sector call structure has been eliminated!
; - All calls for disk drives are at a 128-byte record level.
; - PEM has been changed to only make calls for record numbers.
; - SIM has been re-written to accept record numbers and perform blocking/de-blocking for LBAs.
; - All checksum code has been removed from PEM, no longer needed with IDE block devices.
; - Drive range (A-H) is managed with an invalid drive message shown for invalid drives.
; - DCB structure has changed: track/sector to record based and no checksum table (now 11 bytes).
;
;               02 April 2023 - KM
; Update to SIM for this version:
; - Record Writes are now more efficient with proper blocking to the 512-byte block size of the
;   physical media being used. This provides a significant performance advantage.
; - PEM has been updated to remove the rest of the code associated with diskette based media.
;
;               20 December 2023 - KM
; Update to CCM and PEM to consolidate routines, now V3.20:
; - There were some routines that are called only by CCM, but were contained in the PEM source.
;   These were likely put there to manage the overall size of CCM. Now that Version 3.21 has
;   resulted in a smaller memory footprint, this version consolidates those routines into CCM.
;
; The routines are:
; - tstdec
; - tsthex
; - mv128
; - adjdb
;
; An additional routine in PEM is used by both PEM and CCM:
; - tstchr
;
; This routine is called once from CCM and twice from within PEM.
; The decision here is to replicate this code in CCM without a JSR/RTS to reduce size and
; execution time. It increases the CCM code size, but has no affect on PEM.
;
;               2 December 2024 - KM
; New updates and changes to Version 3.21
;
; - Disk Parameter tables changed to 4KB Block size - reduces allocation tables by 50% (now 2KB).
; - Moved DOS/65 Start location in RAM to $C000 - increases TEA size by 2KB for user programs.
; - Expanded CCM jump table to PEM calls. Reduces execution time with an increase in memory usage.
;
;               27th February 2025 - KM
; Some changes to CCM:
;
; - A new SYSTEM command has been added to CCM, it displays system memory details:
;   Page Zero free range
;   Memory location of system BIOS
;   Memory Ranges for TEA, CCM, PEM, SIM and Disk Allocation Maps
;
; - Removed Switch routine in PEM. No longer needed as PEM is not reloaded on a Warm Boot.
;
;               6th March 2025 - KM
; Change to CCM:
;
; - Changed the erase command to search for the filespec before attempting to erase it.
;   this simlpy displays a NOT FOUND message if the file entered to be erased does not
;   exist. It also shows the error message if a partial wildcard name is entered.
;
;**************************************************************************************************
;
;Base addresses and definitions
;
; Page Zero is allocated from the top ($FF) down. For end user applications, there is a contiguous
; amount of memory from pzbase, which should normally start at $00, to the start of Page Zero
; memory which is used by DOS/65.
;
; The BIOS location for the C02 Pocket is contained in the upper 2KB of the address map and also
; contains the I/O addresses for the reauired hardware. The uppermost address is $FFFF.
; Currently, the C02 Monitor is allocated 6KB of space starting at $E000.
;
pzbase          .EQU    $00             ;Start of page zero available space
pzend           .EQU    $A0             ;End of page zero available space
pzstrt          .EQU    $C0             ;Start of page zero space for DOS/65
monitor         .EQU    $F000           ;Start of Monitor for C02 Pocket (optional)
bios            .EQU    $F800           ;Start of BIOS for C02 Pocket
;
; Page Zero is allocated from the top ($FF) to the bottom ($00)
; The C02BIOS allocates 48 bytes to support all of the hardware devices and a fair anumber of
; variables, pointers and minimal buffer space. The C02Monitor also allocates 48 bytes to
; support it's functions. This equates to 96 bytes which should not be altered by DOS/65!
; The only Page Zero space required by Version 3.21 is 8 bytes starting as shown above.
;
; Note that the current SIM module does access certain BIOS variables for printing a string
; and for checking on console data being available. As I move away from a ROM based Monitor
; to a streamlined BIOS that automatically boots from the IDE port, this access will also be
; minimized further.
;
;Declare a low memory start address for DOS/65.
;
; The initial default is to use the lower part of Page $01, which is the Stack.
; It's possible to move this... BUT... all utilities and applications would need
; to be updated (assembled or compiled) to work with any changes to low memory.
;
; I'm not suggesting that this be the case, but if you have an operating environment
; which requires a large amount of CPU stack space, this could be a workaround.
;
d65_low         .EQU    $100            ;Start of DOS/65 low memory usage
;
btejmp          .EQU    d65_low+$00     ;Warm boot jump (3 bytes)
pemjmp          .EQU    d65_low+$03     ;Jump to PEM    (3 bytes)
iostat          .EQU    d65_low+$06     ;I/O status     (1 byte)
dflfcb          .EQU    d65_low+$07     ;Default FCB    (32 bytes)
dflbuf          .EQU    d65_low+$28     ;Default buffer (128 bytes)
;
; The TEA defaults to $0800 in memory. This is due in part to the 65C02 hardware features and
; the software structure of the C02 Pocket. The lower 2KB of RAM is allocated for the BIOS,
; Monitor, serial port buffers, IDE LBA buffer, Vectors and hardware configuration data.
;
tea             .EQU    $800            ;TEA start
;
;C02 Pocket SBC memory parameters - 3.3V prototype
; RAM is now from $0000 - $EFFF (60KB)
; ROM is now from $F000 - $FFFF (4KB less I/O)
;
; The next iteration of the C02 Pocket SBC will decrease ROM and I/O to 2KB.
; This will yield a total of 62KB of contiguous RAM. This will allow DOS/65
; to have an additional 2KB of TEA space, allowing for larger applications.
;
ramstrt         .EQU    $D000           ;Starting RAM location for assembly
hstbuf          .EQU    $0600           ;IDE host LBA buffer
;
;Fixed parameters
bs              .EQU    $08             ;Backspace
lf              .EQU    $0A             ;Linefeeed
cr              .EQU    $0D             ;Return
eof             .EQU    $1A             ;End of file
null            .EQU    0               ;Null
ctlc            .EQU    3               ;Abort
ctle            .EQU    5               ;Physical cr lf - not referenced!
ctli            .EQU    9               ;Tab character
ctlp            .EQU    $10             ;Toggle printer
ctlr            .EQU    $12             ;Repeat line
ctls            .EQU    $13             ;Freeze
ctlx            .EQU    $18             ;Cancel
semico          .EQU    $3B             ;Semicolon
delete          .EQU    $7F             ;Delete character
numcmd          .EQU    37              ;Number of PEM commands
maxdrv          .EQU    8               ;Maximum number of drives suppported
;
;Page Zero use
                PAGE0
;CCM / PEM
addinp          .EQU    pzstrt          ;Initialized to a,y
bufadd          .EQU    addinp+2        ;Buffer address
alcpnt          .EQU    bufadd+2        ;Allocation map pointer
;
d65flg          .EQU    alcpnt+2        ;DOS/65 flag for CCM/PEM usage
                                        ;bit 7 used for blmode flag in PEM
                                        ;bit 6 used for exrwfl flag in PEM
                                        ;bit 5 used for drvflg flag in PEM
                                        ;bit 4 used for outflg flag in PEM
                                        ;bits 3,2,1 unused
                                        ;bit 0 used for submit flag in CCM
;
;SIM
sim_recflg      .EQU    d65flg+1        ;Disk I/O status flag for SIM
                                        ;bit 7 used for valid LBA
                                        ;bit 6 used for dirty block
                                        ;bit 5 used for read operation
                                        ;bit 4 used for write operation
                                        ;bits 3,2 unused
                                        ;bits 1,0 used for record offset
;
                ENDS
;
;**************************************************************************************************
;
; Main program code starts here:
                .ORG     ramstrt
;
LOAD_HEADER
;Below is the Load Header required by the BOOT RECORD!
; it contains parameters required to complete the loading and configuration of the Boot Image
; before execution. The load header is a 16-byte field located at the start of the Boot Image file.
; It is structured as:
;
; - The 16-bit address to start loading the Boot Image.
; - The 16-bit address to execute the Boot Image (BOOT RECORD code jumps to this address).
; - The 16-bit address for loading the offset to the start of disk data.
; - A long word for the size of the required disk space (for all assigned drives).
; - A 1 byte count that defines how many blocks are loaded for the image.
; - A 5 character string ID to validate the Boot Image.
;
;Note that this is not yet being used...
; this will be used when the boot record setup is finalized.
; timing is everything... and the timing for this part is not yet.
;
        .DW     ramstrt         ;Address to load Boot Image
        .DW     simram          ;Cold Start location for SIM
        .DW     part_offset     ;Location to copy LBA offset
        .LONG   131072          ;64MB for 8-drives (at 8MB each)
        .DB     #16             ;Block count 16 = 8KB (number of blocks to copy to ramstrt)
;
string_id
        .DB     "dos65"         ;ID string to validate Boot image
;
;**************************************************************************************************
;
        include         DOS65-CCM-V321.ASM      ;Include CCM Module
;
                .ORG    $/256*256+256           ;Align to Page address
;
        include         DOS65-PEM-V321.ASM      ;Include PEM Module
;
                .ORG    $/256*256+256           ;Align to Page address
;
        include         DOS65-SIM-V321.ASM      ;Include SIM Module
;
                .ORG    $/256*256+256           ;Align to Page address

;**************************************************************************************************
;
alloc_maps
;drive allocation maps
;
;drive a
almpa           .EQU    alloc_maps
                .BLKW   128
;drive b
almpb           .EQU    almpa+256
                .BLKW   128
;drive c
almpc           .EQU    almpb+256
                .BLKW   128
;drive d
almpd           .EQU    almpc+256
                .BLKW   128
;drive e
almpe           .EQU    almpd+256
                .BLKW   128
;drive f
almpf           .EQU    almpe+256
                .BLKW   128
;drive g
almpg           .EQU    almpf+256
                .BLKW   128
;drive h
almph           .EQU    almpg+256
                .BLKW   128
;
mapend          .EQU    *
;
        END


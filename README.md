# nes-reset-state-detect

Library and demo software for detecting CPU-to-PPU clock alignment of a NES/Famicom.

## Introduction

This NES/Famicom library intends to auto-detect which of the four CPU-to-PPU clock alignments the console has booted into. These are referred to as different "phases" for brevity.

The end goal of this detection is to allow software to avoid graphical glitches for very timing-sensitive raster code, by either:

a) Adjusting the code based on the detected phase
b) Outright refusing to run until the console is booted into the correct phase

Currently only the NTSC version of the hardware is supported. Running on PAL will detect the system, but not produce valid bit patterns or attempt to detect the phase.

## Methodology

* Sprite#0 is placed to have a single pixel not-hit / hit the background
* Various tests perform PPU register writes mid-scanline just around the same dot as sprite#0's single pixel, to flip the not-hit / hit case
* This test case is repeated over the following permutations:
  - Shift position of sprite and BG pixel to the left by 0, 1 or 2 pixels
  - Even/odd frame
  - Early / late write of new fine-X (achieved by using mirrors of $2005 and taking advantage of the 6502 pulling /WE low before data is ready)
  - Switching between what causing / supressing sprite#0 (TODO: Check if actually needed)

### Test configuration

Each bit in the A register passed to the NRSD_UpdateTestParameters subroutine stores the sprite#0 hit result for a number of test cases:
xOOOttTP
 \ /||||
  | \||+- Frame parity. (0: left / "even", 1: right / "odd")
  |  |+-- Timing of register write. (0: Early, 1: Late)
  |  +--- Test number (0: $2005 fine-X, 1: $2006 coarse-X, 2: $2001 blanking at sprite CHR fetch, 3: Sprite-rendering-turn-on)
  +------ Offset of sprite position / fine-X scroll. (0-7) / scanline delay

Each byte of the testResults stores the results for test number, timing and parity.

Each bit in each testResults byte contains an individual test result for each offset.

The four current tests are:
;
; 2005:
;  - Writes fine-X to $2005 (1st write) to shift background to the left 1 pixel, turning a sprite#0 miss into a sprite#0 hit
; 2006:
;  - Writes coarse-X to $2006 (2nd write) to shift background to the left 1 tile, turning a sprite#0 miss into a sprite#0 hit
; CHRI:
;  - Disables BG and SPR rendering via $2001, to interrupt OAM CHR fetch for sprite#0, turning a sprite#0 hit into a sprite#0 miss
; SPON:
;  - Enables SPR rendering via $2001, turning a sprite#0 miss into a sprite#0 hit

### Program output

The example demo included will upload a nametable showing labels for sprite#0 hit/miss bit patterns of different tests being run, the bit pattern identification, and the detected system / phase.

SYSTEM: ???   PHASE: ?

L-EARLY-2005: ????????
R-EARLY-2005: ????????   ?
L-LATER-2005: ????????
R-LATER-2005: ????????
L-EARLY-2006: ????????
R-EARLY-2006: ????????   ?
L-LATER-2006: ????????
R-LATER-2006: ????????
L-EARLY-CHRI: ????????
R-EARLY-CHRI: ????????   ?
L-LATER-CHRI: ????????
R-LATER-CHRI: ????????
L-EARLY-SPON: ????????
R-EARLY-SPON: ????????   ?
L-LATER-SPON: ????????
R-LATER-SPON: ????????

The NMI handler in the NRSD library will then update the nametable, printing these values out to screen.

The recognized bit patterns are identified using a letter notation which is printed to the right of the bit pattern, in order to more easily be able to identify them by eye.

Letter notation used for different tests:

2005: ABCDE
2006: GH
CHRI: MN
SPON: STU

## Using the NRSD library

### Pre-requisites

The NRSD library is dependent on Blargg's nmi_sync library. http://slack.net/~ant/old/nes-code/nmi_sync.zip

Explanation of the technique:

https://forums.nesdev.org/viewtopic.php?t=6589

http://wiki.nesdev.com/w/index.php/Consistent_frame_synchronization


You must obtain this library separately and place nmi_sync.s in the same directory as nes_reset_state_detect.s to assemble correctly.

### NMI-handler-in-RAM requirement

Because NRSD relies on a cycle-exact NMI that is to no use in your game code, it assumes that the NMI vector pointed to by $FFFA does a "JMP (NRSD_nmiAddr)" to transfer control to a reconfigurable NMI handler.

### CHR requirements

Because the tests performed by NRSD rely on sprite#0 hit detection, it must make certain assumptions on what your CHR looks like.

More specifically, the following assumptions are done:

1) CHR table at $0000 has tile $FF containing a single solid pixel in the top-left corner with color 1 (plane0 set, plane1 clear)
2) CHR table at $1000 has tile $FF containing all-solid pixels with color 3 (plane0 set, plane1 clear)

The example demo included uses CHR-RAM and performs this initialization for you. If using CHR-ROM you will need to take care of this yourself.

You can change these tile indices by changing the symbolic constants SPRITE0_TILE_INDEX and SPRITE0_HIT_BG_TILE_INDEX.

### ASCII CHR requirements for nice output

The example demo will initialize the nametable, and Update the "SYSTEM: ", "PHASE: " and tests' patterns for visual output.

In order for this to display correctly the CHR table at $1000 needs to consist of 128 tiles representing the ASCII character set.

If you'd rather not have any this visible at all, you can set the entire palette to a single color.

### Running the detection

The usual expectation is to run this detection only once at reset.

After having setup your CHR, nametable and palette you can perform a "JSR NRSD_Initialize". The NMI will now be configured to happen at every vblank.

To actually run the tests, you need to call NRSD_UpdateTestParameters appropriately between NMI occurrences with the A register set to the test configuration.
The example demo shows how to use an incrementing "frameCounter" variable for this so that tests are continuously run.

## Bugs / limitations

Sometimes, some of the bits in the test bit patterns can sometimes be unstable. Exactly why is not known. This could be worked around by running tests for even longer, and picking the most stable result.

Phase0 and Phase3 are very close in test results. On the HiDef-NES, the bit patterns for Phase3 are sometimes identical to those for Phase0.
Until this is resolved, you are advised to adjust your raster code to minimize glitches for both cases.

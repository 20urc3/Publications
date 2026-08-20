# LX — Linear eXecutable Module Format

*Format description — June 3, 1992*

The Linear eXecutable (LX) format is the 32-bit executable module format used by
OS/2 2.0 (and related systems). It replaces the older segmented (NE) format for
32-bit modules and is built around a demand-paged, object-based layout. This
document describes the on-disk structure of an LX module: its header, tables, and
data sections.

> **Note on LE vs. LX:** This specification covers the **LX** variant (signature
> `"LX"`), the flat/page-based linear executable. It is closely related to the
> earlier **LE** format but is not byte-for-byte identical.

---

## 1. File Layout

An LX file begins with a DOS-compatible stub (so the OS can print a message or run
a real-mode stub when launched under DOS), followed by the linear executable header
and its associated tables and data sections.

```
    00h +------------------+  <--+
        | DOS 2 Compatible |     |
        |    EXE Header    |     |
    1Ch +------------------+     |
        |      unused      |     |
        +------------------+     |
    24h |  OEM Identifier  |     |
    26h |  OEM Info        |     |
        |                  |     |-- DOS 2.0 Section
    3Ch |  Offset to       |     |   (Discarded)
        |  Linear EXE      |     |
        |  Header          |     |
    40h +------------------+     |
        |   DOS 2.0 Stub   |     |
        |   Program &      |     |
        |   Reloc. Table   |     |
        +------------------+  <--+
        |                  |
    xxh +------------------+  <--+
        |    Executable    |     |
        |       Info       |     |
        +------------------+     |
        |      Module      |     |
        |       Info       |     |
        +------------------+     |-- Linear Executable
        |  Loader Section  |     |   Module Header
        |       Info       |     |   (Resident)
        +------------------+     |
        |   Table Offset   |     |
        |       Info       |     |
        +------------------+  <--+
        |   Object Table   |     |
        +------------------+     |
        | Object Page Table|     |
        +------------------+     |
        |  Resource Table  |     |
        +------------------+     |
        |  Resident Name   |     |
        |      Table       |     |
        +------------------+     |-- Loader Section
        |   Entry Table    |     |   (Resident)
        +------------------+     |
        |   Module Format  |     |
        | Directives Table |     |
        |    (Optional)    |     |
        +------------------+     |
        |     Resident     |     |
        | Directives Data  |     |
        |    (Optional)    |     |
        |  (Verify Record) |     |
        +------------------+     |
        |     Per-Page     |     |
        |     Checksum     |     |
        +------------------+  <--+
        | Fixup Page Table |     |
        +------------------+     |
        |   Fixup Record   |     |
        |       Table      |     |
        +------------------+     |-- Fixup Section
        |   Import Module  |     |   (Optionally Resident)
        |    Name Table    |     |
        +------------------+     |
        | Import Procedure |     |
        |    Name Table    |     |
        +------------------+  <--+
        |   Preload Pages  |     |
        +------------------+     |
        |    Demand Load   |     |
        |       Pages      |     |
        +------------------+     |
        |  Iterated Pages  |     |
        +------------------+     |
        |   Non-Resident   |     |-- (Non-Resident)
        |    Name Table    |     |
        +------------------+     |
        |   Non-Resident   |     |
        | Directives Data  |     |
        |    (Optional)    |     |
        +------------------+  <--+
        |    Debug Info    |     |-- (Not used by Loader)
        +------------------+  <--+
```

The module is logically divided into a few regions:

- **DOS 2.0 section** — discarded once the linear loader takes over.
- **Linear Executable Module Header** — resident; describes the module.
- **Loader Section** — resident; the tables the loader needs while the module is in use.
- **Fixup Section** — optionally resident; relocation data.
- **Data sections** — preload, demand-load, and iterated pages plus the non-resident name table.
- **Debug Info** — not used by the loader.

---

## 2. Linear EXE Header

All multi-byte fields are `DD` (dword) unless noted. Offsets are hexadecimal from
the start of the header.

```
    +-----+-----+-----+-----+-----+-----+-----+-----+
00h | "L"   "X" |B-ORD|W-ORD|     FORMAT LEVEL      |
08h | CPU TYPE  |  OS TYPE  |    MODULE VERSION     |
10h |     MODULE FLAGS      |   MODULE # OF PAGES   |
18h |     EIP OBJECT #      |          EIP          |
20h |     ESP OBJECT #      |          ESP          |
28h |       PAGE SIZE       |   PAGE OFFSET SHIFT   |
30h |  FIXUP SECTION SIZE   | FIXUP SECTION CHECKSUM|
38h |  LOADER SECTION SIZE  |LOADER SECTION CHECKSUM|
40h |    OBJECT TABLE OFF   |  # OBJECTS IN MODULE  |
48h | OBJECT PAGE TABLE OFF | OBJECT ITER PAGES OFF |
50h | RESOURCE TABLE OFFSET |#RESOURCE TABLE ENTRIES|
58h | RESIDENT NAME TBL OFF |   ENTRY TABLE OFFSET  |
60h | MODULE DIRECTIVES OFF | # MODULE DIRECTIVES   |
68h | FIXUP PAGE TABLE OFF  |FIXUP RECORD TABLE OFF |
70h | IMPORT MODULE TBL OFF | # IMPORT MOD ENTRIES  |
78h |  IMPORT PROC TBL OFF  | PER-PAGE CHECKSUM OFF |
80h |   DATA PAGES OFFSET   |    #PRELOAD PAGES     |
88h | NON-RES NAME TBL OFF  | NON-RES NAME TBL LEN  |
90h | NON-RES NAME TBL CKSM |   AUTO DS OBJECT #    |
98h |    DEBUG INFO OFF     |    DEBUG INFO LEN     |
A0h |   #INSTANCE PRELOAD   |   #INSTANCE DEMAND    |
A8h |       HEAPSIZE        |
    +-----+-----+-----+-----+
```

> Table offsets may be set to **zero** to indicate the table does not exist and its
> size is zero.
>
> `OBJECT ITER PAGES OFF` must be either 0 or equal to `DATA PAGES OFFSET` in
> OS/2 2.0 — iterated pages must live in the same file section as regular pages.

### 2.1 Header Field Reference

| Offset | Field | Type | Description |
|--------|-------|------|-------------|
| 00h | Signature `"LX"` | DW | Identifies a valid 32-bit LX module. `L` is the low byte, `X` the high byte. |
| 02h | B-ORD | DB | Byte ordering: `00h` little-endian, `01h` big-endian. |
| 03h | W-ORD | DB | Word ordering: `00h` little-endian, `01h` big-endian. |
| 04h | Format Level | DD | `0` for the initial version. Incremented on each incompatible format change so the loader can reject unknown versions. |
| 08h | CPU Type | DW | `01h` = 80286+, `02h` = 80386+, `03h` = 80486+. |
| 0Ah | OS Type | DW | `00h` unknown, `01h` OS/2 (default), `02h` Windows, `03h` DOS 4.x, `04h` Windows 386. |
| 0Ch | Module Version | DD | User-specified version, set at link time. Useful for distinguishing DLL revisions. |
| 10h | Module Flags | DD | See [Module Flags](#22-module-flags). |
| 14h | Module # of Pages | DD | Number of pages physically present in the module (enumerated, iterated, or zero-fill-with-relocations pages). Used to size the page tables. |
| 18h | EIP Object # | DD | Object number the entry address is relative to. See notes below. |
| 1Ch | EIP | DD | Entry address: program start, or library init/term address. |
| 20h | ESP Object # | DD | Object the starting `ESP` is relative to. Nonzero for programs; ignored for libraries. |
| 24h | ESP | DD | Starting stack pointer for programs. `0` means initialize to the highest offset in the object. Ignored for libraries. |
| 28h | Page Size | DD | System page size, in bytes. `4096` in the initial version. |
| 2Ch | Page Offset Shift | DD | Left-shift count applied to Object Page Table page-offset fields, controlling file alignment of page data. Default `12` (→ 4096-byte alignment). |
| 30h | Fixup Section Size | DD | Total size of the fixup information (fixup page table + fixup record table + import module name table + import procedure name table). |
| 34h | Fixup Section Checksum | DD | Cryptographic checksum over the fixup data. `0` if unused. |
| 38h | Loader Section Size | DD | Total size of the tables that must stay resident (Object Table through Per-Page Checksum Table). |
| 3Ch | Loader Section Checksum | DD | Cryptographic checksum over the loader section. `0` if unused. |
| 40h | Object Table Offset | DD | Offset of the Object Table, relative to the header. |
| 44h | # Objects in Module | DD | Number of Object Table entries. |
| 48h | Object Page Table Offset | DD | Offset of the Object Page Table, relative to the header. |
| 4Ch | Object Iter Pages Offset | DD | Offset of the iterated pages, relative to the **file**. |
| 50h | Resource Table Offset | DD | Offset of the Resource Table, relative to the header. |
| 54h | # Resource Table Entries | DD | Number of Resource Table entries. |
| 58h | Resident Name Table Offset | DD | Offset of the Resident Name Table, relative to the header. |
| 5Ch | Entry Table Offset | DD | Offset of the Entry Table, relative to the header. |
| 60h | Module Directives Offset | DD | Offset of the Module Format Directives Table, relative to the header. |
| 64h | # Module Directives | DD | Number of directive entries. |
| 68h | Fixup Page Table Offset | DD | Offset of the Fixup Page Table, relative to the header. |
| 6Ch | Fixup Record Table Offset | DD | Offset of the Fixup Record Table, relative to the header. |
| 70h | Import Module Table Offset | DD | Offset of the Import Module Name Table, relative to the header. |
| 74h | # Import Mod Entries | DD | Number of Import Module Name Table entries. |
| 78h | Import Proc Table Offset | DD | Offset of the Import Procedure Name Table, relative to the header. |
| 7Ch | Per-Page Checksum Offset | DD | Offset of the Per-Page Checksum Table, relative to the header. |
| 80h | Data Pages Offset | DD | Offset of the data pages, relative to the **file**. |
| 84h | # Preload Pages | DD | Number of preload pages. *(OS/2 2.0 ignores preload requests for performance.)* |
| 88h | Non-Res Name Table Offset | DD | Offset of the Non-Resident Name Table, relative to the **file**. |
| 8Ch | Non-Res Name Table Length | DD | Size of the Non-Resident Name Table, in bytes. |
| 90h | Non-Res Name Table Checksum | DD | Cryptographic checksum of the Non-Resident Name Table. |
| 94h | Auto DS Object # | DD | Auto Data Segment object number (16-bit compatibility only; unused by 32-bit modules). |
| 98h | Debug Info Offset | DD | Offset of the debug information, relative to the header. |
| 9Ch | Debug Info Length | DD | Length of the debug information, in bytes. |
| A0h | # Instance Preload | DD | Instance data pages in the preload section. |
| A4h | # Instance Demand | DD | Instance data pages in the demand section. |
| A8h | Heapsize | DD | Bytes added to the Auto DS object by the loader (16-bit compatibility only). |

### 2.2 Module Flags

| Value | Meaning |
|-------|---------|
| `00000001h` | Reserved for system use. |
| `00000002h` | Reserved for system use. |
| `00000004h` | Per-Process Library Initialization. Requires valid EIP Object # / EIP. If those are valid but this bit is clear, Global Library Initialization is assumed. Invalid for EXE files. |
| `00000008h` | Reserved for system use. |
| `00000010h` | Internal fixups have been applied. Each object has a preferred load address (Object Table Reloc Base Addr); retained relocation records are applied only if those addresses are unavailable. |
| `00000020h` | External fixups have been applied. |
| `00000040h` | Reserved for system use. |
| `00000080h` | Reserved for system use. |
| `00000100h` | Incompatible with PM windowing. |
| `00000200h` | Compatible with PM windowing. |
| `00000300h` | Uses PM windowing API. |
| `00000400h` | Reserved for system use. |
| `00000800h` | Reserved for system use. |
| `00001000h` | Reserved for system use. |
| `00002000h` | Module is not loadable (link errors, or an incrementally-linked module). |
| `00004000h` | Reserved for system use. |
| `00038000h` | **Module type mask.** |
| `00000000h` | Program module. (May not statically link to other program-type modules.) |
| `00008000h` | Library module. |
| `00018000h` | Protected Memory Library module. |
| `00020000h` | Physical Device Driver module. |
| `00028000h` | Virtual Device Driver module. |
| `40000000h` | Per-Process Library Termination. Requires valid EIP Object # / EIP. If those are valid but this bit is clear, Global Library Termination is assumed. Invalid for EXE files. |

**EIP Object # notes:** Must be nonzero for a program module. For a library, a zero
value indicates no entry routine — in which case both the Per-Process
Initialization and Termination bits must be clear, or the load fails. If the
Per-Process Termination bit is set, the referenced object must be a 32-bit object
(the Big/Default flag must be set in the object flags).

---

## 3. Program Startup and Library Entry Registers

### 3.1 Program (EXE) Startup

| Register | Value |
|----------|-------|
| `EIP` | Starting program entry address. |
| `ESP` | Top of stack address. |
| `CS` | Code selector for base of linear address space. |
| `DS = ES = SS` | Data selector for base of linear address space. |
| `FS` | Data selector of base of Thread Information Block (TIB). |
| `GS` | 0. |
| `EAX = EBX = ECX = EDX = ESI = EDI = EBP` | 0. |

Stack on entry:

| Location | Contents |
|----------|----------|
| `[ESP+0]` | Return address to a routine that calls `DosExit(1, EAX)`. |
| `[ESP+4]` | Module handle for the program module. |
| `[ESP+8]` | Reserved. |
| `[ESP+12]` | Environment data object address. |
| `[ESP+16]` | Command-line linear address within the environment data object. |

### 3.2 Library Initialization

Same register conventions as above, except `ESP` is the user program stack and
`EIP` is the library entry address.

- A **32-bit Protected Memory Library** receives a GDT selector in `DS`/`ES`
  (`PROTDS`) addressing the full application linear address space; the init routine
  should save it.
- **Non-Protected Memory Libraries** receive a selector (`FLATDS`) addressing the
  same amount of linear space as an application's `.EXE`.

Stack on entry:

| Location | Contents |
|----------|----------|
| `[ESP+0]` | Return address to system; `EAX` = return code. |
| `[ESP+4]` | Module handle for the library module. |
| `[ESP+8]` | 0 (Initialization). |

> A 32-bit library may place its entry in a 16-bit code object, in which case the
> entry registers follow the segmented (NE) convention. This lets a 16-bit library
> be relinked to gain LX benefits (notably efficient paging).

### 3.3 Library Termination

Same as initialization, with:

| Location | Contents |
|----------|----------|
| `[ESP+0]` | Return address to system. |
| `[ESP+4]` | Module handle for the library module. |
| `[ESP+8]` | 1 (Termination). |

> Library termination is **not** allowed for libraries with 16-bit entries.

---

## 4. Object Table

The number of entries is given by *# Objects in Module*. Entries are numbered from
one.

```
    +-----+-----+-----+-----+-----+-----+-----+-----+
00h |     VIRTUAL SIZE      |    RELOC BASE ADDR    |
08h |     OBJECT FLAGS      |    PAGE TABLE INDEX   |
10h |  # PAGE TABLE ENTRIES |       RESERVED        |
    +-----+-----+-----+-----+-----+-----+-----+-----+
```

| Offset | Field | Type | Description |
|--------|-------|------|-------------|
| 00h | Virtual Size | DD | Bytes allocated when the object loads. Rounded up to page size, must be ≥ the total page size in the file and large enough for all iterated and uninitialized data. |
| 04h | Reloc Base Addr | DD | Address the object is relocated to (or will be allocated at, if internal fixups were removed). |
| 08h | Object Flags | DW | See [Object Flags](#41-object-flags). |
| 0Ah | Page Table Index | DD | Index of the first Object Page Table entry for this object. Object Table entries are sorted by this value. |
| 0Eh | # Page Table Entries | DD | Number of page table entries for this object. |
| 12h | Reserved | DD | Must be zero. |

Logical pages at the end of an object with no page-table entry are treated as
zero-fill or invalid, based on the object's last page-table entry (defaulting to
zero-fill if that entry was neither).

### 4.1 Object Flags

| Value | Meaning |
|-------|---------|
| `0001h` | Readable object. |
| `0002h` | Writable object. |
| `0004h` | Executable object. |
| `0008h` | Resource object. |
| `0010h` | Discardable object. |
| `0020h` | Shared object. |
| `0040h` | Object has preload pages. |
| `0080h` | Object has invalid pages. |
| `0100h` | Object has zero-filled pages. |
| `0200h` | Resident (VDDs/PDDs only). |
| `0300h` | Resident & contiguous (VDDs/PDDs only). |
| `0400h` | Resident & long-lockable (VDDs/PDDs only). |
| `0800h` | Reserved for system use. |
| `1000h` | 16:16 alias required (80x86-specific). |
| `2000h` | Big/Default bit setting (80x86-specific). For data segments, sets the descriptor Big (B) bit (ESP vs SP); for code segments, sets the Default (D) bit (32- vs 16-bit default word size). |
| `4000h` | Conforming for code (80x86-specific). |
| `8000h` | Object I/O privilege level (80x86-specific; used only for 16:16 alias objects). |

The readable/writable/executable flags cover all protection combinations. On
systems that don't support all protections, the loader chooses the closest match.

---

## 5. Object Page Table

Describes each logical page of an object. A logical page may be enumerated, a
pseudo page, or iterated. This table is parallel to the Fixup Page Table (both
indexed by logical page number). Entries are numbered from one.

```
     63                     32 31       16 15         0
    +-----+-----+-----+-----+-----+-----+-----+-----+
00h |    PAGE DATA OFFSET   | DATA SIZE |   FLAGS   |
    +-----+-----+-----+-----+-----+-----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Page Data Offset | DD | Shifted left by *Page Offset Shift*, gives the offset (from the Preload Page section) of the page data. Data may live in the preload, demand-load, or iterated-data sections. `0` for a zero-fill page. For iterated pages, this is the offset into the Iterated Data Pages section. |
| Data Size | DW | Actual bytes representing the page in the file. If less than *Page Size* on a legal physical page, the rest is zero-filled; on an iterated page, iteration records fill the remainder. |
| Flags | DW | Page attributes (below). |

**Flags values:**

| Value | Meaning |
|-------|---------|
| `00h` | Legal physical page (offset from Preload Page section). |
| `01h` | Iterated data page (offset from Iterated Data Pages section). |
| `02h` | Invalid page (zero). |
| `03h` | Zero-filled page (zero). |
| `04h` | Range of pages. |

The logical page number is also used to index the Fixup Page Table for that page's
fixups.

---

## 6. Resource Table

An array of resource entries locating resource objects in the Object Table. Entries
are sorted ascending by Name ID within Type ID, allowing `DosGetResource` to binary-search.

```
    +-----+-----+-----+-----+
00h |  TYPE ID  |  NAME ID  |
04h |     RESOURCE SIZE     |
08h |   OBJECT  |   OFFSET  |
    +-----+-----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Type ID | DW | Resource type. Defined values: `BTMP` (bitmap), `EMSG` (error message string), `FONT` (fonts). |
| Name ID | DW | ID used as a name when the resource is referenced. |
| Resource Size | DD | Size of the resource in bytes. |
| Object | DW | Object number containing the resource. |
| Offset | DD | Offset within that object where the resource begins. |

---

## 7. Resident and Non-Resident Name Tables

These tables map ASCII export names to ordinal numbers. The first entry of the
**Resident Name Table** is the module name. Ordinals index the Entry Table.

- **Resident Name Table** — kept in memory while the module is loaded; for frequently
  by-name linked exports.
- **Non-Resident Name Table** — read from the file on demand; for infrequently
  by-name linked or by-ordinal referenced exports.

By-ordinal imports are fastest (no table search). By-name imports require searching
these tables. Strings are **case-sensitive** and **not null-terminated**.

```
    +-----+-----+-----+-----+     +-----+-----+-----+
00h | LEN |    ASCII STRING  . . .      | ORDINAL # |
    +-----+-----+-----+-----+     +-----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Len | DB | String length in bytes. `0` marks the end of the table. Max name length is 127. **Bit 7** is an *Overload* bit indicating extra parameter-type-checking info (reserved for future use). |
| ASCII String | DB[] | Variable-length, case-sensitive, not null-terminated. |
| Ordinal # | DW | Index into the Entry Table for this entry point. |

---

## 8. Entry Table

Maps ordinals to object + offset for entry points, resolving fixup references.
Ordinals index the table (numbered from one). Not all entries are exported.

Entries are grouped into **bundles** of same-size entries. Each bundle starts with a
count and a type byte.

```
    +-----+-----+-----+-----+-----+
00h | CNT |TYPE | BUNDLE INFO . . .
    +-----+-----+-----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| CNT | DB | Number of entries in the bundle. `0` terminates the Entry Table (single zero byte, no further data). |
| TYPE | DB | Bundle type (below). |

**Bundle types:**

| Value | Type |
|-------|------|
| `00h` | Unused entry. |
| `01h` | 16-bit entry. |
| `02h` | 286 call gate entry. |
| `03h` | 32-bit entry. |
| `04h` | Forwarder entry. |
| `80h` | Parameter typing information present (reserved for future use). |

### 8.1 Unused Entry (Type 0)

```
    +-----+-----+
00h | CNT |TYPE |
    +-----+-----+
```

`CNT` = number of unused entries to skip. `TYPE` = 0.

### 8.2 16-bit Entry (Type 1)

```
    +-----+-----+-----+-----+
00h | CNT |TYPE |   OBJECT  |
04h |FLAGS|  OFFSET   |         (FLAGS+OFFSET repeated CNT times)
    +-----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Object | DW | Object number for the entries in this bundle. |
| Flags | DB | `01h` = exported entry; `F8h` = parameter word count mask. |
| Offset | DW | Offset in the object for the entry point at this ordinal. |

### 8.3 286 Call Gate Entry (Type 2)

Needed only if ring-2 segments are supported. Adds a 2-byte call-gate selector the
loader uses to store an LDT callgate selector.

```
    +-----+-----+-----+-----+
00h | CNT |TYPE |   OBJECT  |
04h |FLAGS|  OFFSET   | CALLGATE  |   (FLAGS+OFFSET+CALLGATE repeated CNT times)
    +-----+-----+-----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Object | DW | Object number. |
| Flags | DB | `01h` = exported entry; `F8h` = parameter word count mask. |
| Offset | DW | Offset in the object. |
| Callgate | DW | Reserved; loader stores a call-gate selector here for ring-2 references. On a ring-3 → ring-2 reference the callgate selector (offset 0) is placed in the fixup address, and the segment/offset go in the LDT callgate. |

### 8.4 32-bit Entry (Type 3)

Emitted only when the object offset can't fit in 16 bits.

```
    +-----+-----+-----+-----+
00h | CNT |TYPE |   OBJECT  |
04h |FLAGS|        OFFSET         |   (FLAGS+OFFSET repeated CNT times)
    +-----+-----+-----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Object | DW | Object number. |
| Flags | DB | `01h` = exported entry; `F8h` = parameter dword count mask. |
| Offset | DD | 32-bit offset in the object. |

### 8.5 Forwarder Entry (Type 4)

```
    +-----+-----+-----+-----+
00h | CNT |TYPE | RESERVED  |
04h |FLAGS| MOD ORD#  | OFFSET / ORDNUM       |
    +-----+-----+-----+-----+-----+-----+-----+   (repeated CNT times)
```

| Field | Type | Description |
|-------|------|-------------|
| Reserved | DW | 0 (reserved for future use). |
| Flags | DB | `01h` = import by ordinal; `F7h` reserved (should be zero). |
| Mod Ord # | DW | Index into the Import Module Name Table for this forwarder. |
| Offset / Ordnum | DD | If import-by-ordinal, the target module's Entry Table ordinal; otherwise the offset into the target module's Procedure Names Table. |

A forwarder's value is an imported reference. At load-time fixup, the loader
resolves the imported address and uses it for the fixup. Forwarders may chain to
other forwarders; the loader follows the chain to a non-forwarded endpoint.
**Circular chains** produce a load-time error, and a chain longer than **1024**
forwarders is also an error.

Forwarders allow merging/recombining API sets across libraries while keeping
application compatibility (e.g. combining `MONCALLS`, `MOUCALLS`, `VIOCALLS` into
one library via forwarding entry points).

---

## 9. Module Format Directives Table

An optional table for additional options and format extensions, and for temporary
tables (incremental-link info, statistics). When absent, the related header fields
are zero.

```
    +-----+-----+-----+-----+-----+-----+----+----+
00h | DIRECT #  | DATA LEN  |     DATA OFFSET     |
    +-----+-----+-----+-----+-----+-----+----+----+
```

| Field | Type | Description |
|-------|------|-------------|
| Direct # | DW | Directive number (below). |
| Data Len | DW | Length of the directive data in bytes. |
| Directive Offset | DD | Offset to the directive data — relative to the header for resident tables, relative to the file for non-resident tables. |

**Directive numbers:**

| Value | Meaning |
|-------|---------|
| `8000h` | Resident Flag Mask — directive data is resident and stays in memory. |
| `8001h` | Verify Record Directive (resident). |
| `0002h` | Language Information Directive (non-resident). |
| `0003h` | Co-Processor Required Support Table. |
| `0004h` | Thread State Initialization Directive. |

Additional directives may be defined later, provided they don't overlap existing numbers.

---

## 10. Verify Record Directive Table

An optional table recording which pages have been fixed up and written back, along
with the module dependencies used, so virtual addresses can be verified efficiently
at load.

```
    +-----+-----+
00h |# OF ENTRY |
02h | MOD ORD # |  VERSION  | MOD # OBJ |
08h | OBJECT #  | BASE ADDR |  VIRTUAL  |
    +-----+-----+-----+-----+-----+-----+   (per-object entries repeat)
```

| Field | Type | Description |
|-------|------|-------------|
| # of Entry | DW | Number of module dependencies (= modules referenced). |
| Mod Ord # | DW | Ordered index into the Import Module Name Table for the referenced module. |
| Version | DW | Version of the referenced module when fixups were performed. Ensures the same version is loaded; requires bumping a module's version whenever entry-point offsets change. |
| Mod # Obj | DW | Number of object verify entries that follow for this referenced module. |
| Object # | DW | Object number in the referenced module being verified. |
| Base Addr | DW | Address the object was loaded at when fixups were performed. |
| Virtual | DW | Total virtual memory required for this object. |

---

## 11. Per-Page Checksum

One cryptographic checksum per physical page. The first entry corresponds to the
first logical code/data page (usually a preload page); the last to the final
logical page (usually an iterated page).

```
                 +-----+-----+-----+-----+
Logical Page #1  |        CHECKSUM       |
                 +-----+-----+-----+-----+
Logical Page #2  |        CHECKSUM       |
                 +-----+-----+-----+-----+
                           . . .
                 +-----+-----+-----+-----+
Logical Page #n  |        CHECKSUM       |
                 +-----+-----+-----+-----+
```

`CHECKSUM` = DD, cryptographic checksum.

---

## 12. Fixup Page Table

Maps each logical page number to an offset into the Fixup Record Table. Parallel to
the Object Page Table, with **one extra entry** marking the end of the fixup records.

```
                 +-----+-----+-----+-----+
Logical Page #1  |  OFFSET FOR PAGE #1   |
                 +-----+-----+-----+-----+
Logical Page #2  |  OFFSET FOR PAGE #2   |
                 +-----+-----+-----+-----+
                           . . .
                 +-----+-----+-----+-----+
Logical Page #n  |  OFFSET FOR PAGE #n   |
                 +-----+-----+-----+-----+
                 |OFF TO END OF FIXUP REC|
                 +-----+-----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Offset for Page # | DD | Offset (from the start of the Fixup Record Table) to the first fixup record for this page. |
| Off to End of Fixup Rec | DD | Offset just past the last fixup record. Equals *offset for page #n + size of fixups for page #n*. |

Because records are ordered by logical page, each page's fixups end where the next
page's begin; the final extra entry provides this boundary for the last page.

---

## 13. Fixup Record Table

Contains all fixups, grouped by logical page and sorted by page number. Within each
page, external fixups and internal selector/pointer fixups come before internal
non-selector/non-pointer fixups, letting the loader skip internal fixups when it can
load every object at its Object Table address.

```
    +-----+-----+-----+-----+
00h | SRC |FLAGS|SRCOFF/CNT*|
03h |           TARGET DATA *           |     (03h or 04h depending on sizes)
    | SRCOFF1 @ |   . . .   | SRCOFFn @ |
    +-----+-----+----   ----+-----+-----+

    * variable size    @ optional
```

### 13.1 SRC — Source Type

| Value | Meaning |
|-------|---------|
| `0Fh` | Source mask. |
| `00h` | Byte fixup (8 bits). |
| `01h` | (undefined). |
| `02h` | 16-bit selector fixup (16 bits). |
| `03h` | 16:16 pointer fixup (32 bits). |
| `04h` | (undefined). |
| `05h` | 16-bit offset fixup (16 bits). |
| `06h` | 16:32 pointer fixup (48 bits). |
| `07h` | 32-bit offset fixup (32 bits). |
| `08h` | 32-bit self-relative offset fixup (32 bits). |
| `10h` | **Fixup to Alias** flag. Source refers to the object's 16:16 alias (valid only for source types 2, 3, 6). Requires the target offset be < 64K. |
| `20h` | **Source List** flag. `SRCOFF` becomes a byte count and a list of source offsets follows the record (after the optional additive value). |

### 13.2 FLAGS — Target Flags

| Value | Meaning |
|-------|---------|
| `03h` | Fixup target type mask. |
| `00h` | Internal reference. |
| `01h` | Imported reference by ordinal. |
| `02h` | Imported reference by name. |
| `03h` | Internal reference via entry table. |
| `04h` | **Additive Fixup** flag — an additive value trails the record (before the optional source-offset list). |
| `08h` | Reserved (must be zero). |
| `10h` | **32-bit Target Offset** flag — target offset is 32-bit, else 16-bit. |
| `20h` | **32-bit Additive** flag — additive value is 32-bit, else 16-bit. |
| `40h` | **16-bit Object Number/Module Ordinal** flag — that field is 16-bit, else 8-bit. |
| `80h` | **8-bit Ordinal** flag — ordinal is 8-bit, else 16-bit. |

### 13.3 Common Fields

| Field | Type | Description |
|-------|------|-------------|
| SRCOFF / CNT | DW or DB | If the Source List flag is set: a byte count of source offsets (list follows the additive field). Otherwise: a single source offset relative to the start of the page. |
| Target Data | var | Format depends on the target flags (see sub-formats below). |
| SRCOFF1..n | DW[] | Present when the Source List flag is set; count given by CNT. Offsets are relative to the start of the page. |

> For fixups crossing a page boundary, a separate record exists on each page. The
> second page uses a **negative** offset (e.g. if only the last byte of a 32-bit
> address is on the fixed-up page, the offset is `-3`).

### 13.4 Target: Internal Reference

```
    +-----+-----+-----+-----+
00h | SRC |FLAGS|SRCOFF/CNT*|
03h |  OBJECT * |        TRGOFF * @     |
    | SRCOFF1 @ |   . . .   | SRCOFFn @ |
    +-----+-----+----   ----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Object | DB/DW | Index into this module's Object Table. Byte when the 16-bit Object Number flag is clear, word when set. |
| Trgoff | DW/DD | Offset into the target object. Absent for a 16-bit selector fixup. Word when the 32-bit Target Offset flag is clear, dword when set. |

### 13.5 Target: Imported Reference by Name

```
    +-----+-----+-----+-----+
00h | SRC |FLAGS|SRCOFF/CNT*|
03h | MOD ORD# *| PROCEDURE NAME OFFSET*|     ADDITIVE * @      |
    | SRCOFF1 @ |   . . .   | SRCOFFn @ |
    +-----+-----+----   ----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Mod Ord # | DB/DW | Ordered index into the Import Module Name Table. Byte/word per the 16-bit Object Number/Module Ordinal flag. |
| Procedure Name Offset | DW/DD | Offset into the Import Procedure Name Table. Word/dword per the 32-bit Target Offset flag. |
| Additive | DW/DD | Present only if the Additive Fixup flag is set; added to the resolved target address. Word/dword per the 32-bit Additive flag. |

### 13.6 Target: Imported Reference by Ordinal

```
    +-----+-----+-----+-----+
00h | SRC |FLAGS|SRCOFF/CNT*|
03h | MOD ORD# *|IMPORT ORD*|     ADDITIVE * @      |
    | SRCOFF1 @ |   . . .   | SRCOFFn @ |
    +-----+-----+----   ----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Mod Ord # | DB/DW | Ordered index into the Import Module Name Table. Byte/word per the 16-bit Object Number/Module Ordinal flag. |
| Import Ord | DB/DW/DD | Imported procedure ordinal. Byte if the 8-bit Ordinal flag is set; otherwise word/dword per the 32-bit Target Offset flag. |
| Additive | DW/DD | Present only if the Additive Fixup flag is set; word/dword per the 32-bit Additive flag. |

### 13.7 Target: Internal Reference via Entry Table

```
    +-----+-----+-----+-----+
00h | SRC |FLAGS|SRCOFF/CNT*|
03h |  ORD # *  |     ADDITIVE * @      |
    | SRCOFF1 @ |   . . .   | SRCOFFn @ |
    +-----+-----+----   ----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Entry # / Ord # | DB/DW | Index into this module's Entry Table (giving target object and offset). Byte/word per the 16-bit Object Number/Module Ordinal flag. |
| Additive | DW/DD | Present only if the Additive Fixup flag is set; word/dword per the 32-bit Additive flag. |

---

## 14. Import Module Name Table

Module-name strings imported via dynamic-link references, referenced by imported
fixups. Its length is *(Import Procedure Name Table offset − Import Module Name Table
offset)*. It is **not** terminated by a special character; the Import Procedure Name
Table follows directly. Strings are **case-sensitive** and **not null-terminated**.

```
    +-----+-----+-----+-----+     +-----+
00h | LEN |    ASCII STRING  . . .      |
    +-----+-----+-----+-----+     +-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Len | DB | String length in bytes (max 127). |
| ASCII String | DB[] | Variable-length, case-sensitive, not null-terminated. |

---

## 15. Import Procedure Name Table

Procedure-name strings imported by this module, referenced by imported fixups. Its
length is *(Fixup Page Table offset + Fixup Section Size) − Import Procedure Name
Table offset*. It is followed by the data pages section; because that section is
page-aligned, zero-filled padding may exist after the last name string. Strings are
**case-sensitive** and **not null-terminated**.

```
    +-----+-----+-----+-----+     +-----+
00h | LEN |    ASCII STRING  . . .      |
    +-----+-----+-----+-----+     +-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Len | DB | String length in bytes (max 127). **Bit 7** is an *Overload* bit indicating extra parameter-type-checking info (reserved for future use). |
| ASCII String | DB[] | Variable-length, case-sensitive, not null-terminated. |

---

## 16. Preload Pages

An optional section coalescing the "preload page set" (the set of first-used pages)
into a contiguous region so it can be read in one disk operation. The set may be
specified by the developer or derived by a profiling tool.

These are non-iterated pages, structurally identical to demand-loaded pages; sizes
come from the corresponding Object Page Table entries. If a page's specified size is
less than *Page Size*, the remainder is zero-filled on load. Every page begins on a
*Page Offset Shift* boundary from the section base, ordered by logical page number.

> **Note:** OS/2 2.0 does **not** respect preload pages — testing showed better
> performance by ignoring the preload request.

---

## 17. Demand Load Pages

All non-iterated pages not preloaded. The whole page is loaded on demand.
Characteristics come from the Object Page Table. Every page begins on a *Page Offset
Shift* boundary from the demand-load base; sizes come from the corresponding page
table entries, with under-sized pages zero-filled to *Page Size*. Ordered by logical
page number.

---

## 18. Iterated Data Pages

All iterated pages. On demand, the iteration records are loaded and expanded to
reconstruct the page. Each set of iteration records begins on a *Page Offset Shift*
offset from *Object Iter Pages Off*; sizes come from the Object Page Table. Ordered
by logical page number.

**Iteration record (per page):**

```
    +-----+-----+-----+-----+
00h |#ITERATIONS|DATA LENGTH|
04h |DATA BYTES |   . . .   | ... |
    +-----+-----+-----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| # Iterations | DW | Number of times the data pattern is replicated. |
| Data Length | DW | Size of the data pattern in bytes. Max = half of *Page Size*; larger patterns are not condensed into iterated data. |
| Data | DB[Data Length] | The pattern to replicate. The next iteration record immediately follows the last pattern byte. |

The offset of the next iteration record = current offset + *Data Length* + the sizes
of the *# Iterations* and *Data Length* fields.

---

## 19. Debug Information

Debug data is defined by the debugger, not by the LX format or linker. The format
only specifies its file offset and length (from the header). The first word is a
type field identifying the debug format.

```
    00h   01h   02h   03h   04h
    +-----+-----+-----+-----+-----+-----+-----+-----+
    | 'N' | 'B' | '0' |  n  |   DEBUGGER DATA  . . . .
    +-----+-----+-----+-----+-----+-----+-----+-----+
```

| Field | Type | Description |
|-------|------|-------------|
| Type | DB × 4 | Signature: the ASCII string `"NB0"` followed by the ASCII value of `n`. |
| Debugger Data | var | Debugger-specific data. |

**Defined `n` values:**

| Value | Meaning |
|-------|---------|
| `00h` | 32-bit CodeView debugger format. |
| `01h` | AIX debugger format. |
| `02h` | 16-bit CodeView debugger format. |
| `04h` | 32-bit OS/2 PM debugger (IBM) format. |

The system does not enforce the type field; it is the linker's or debugging tool's
responsibility to follow this convention.

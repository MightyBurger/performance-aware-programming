// Copyright 2026 Jordan Johnson

 

/*
    Life got busy, but getting back to the course!

    In 002 we only had to worry about register-register moves:

        mov cx, bx
        mov ch, ah
        mov dx, bx
        mov si, bx
        mov bx, di
        mov al, cl
        mov ch, ch
        mov bx, ax
        mov bx, si
        mov sp, di
        mov bp, ax

    These were all two-byte instructions, with this form:

    byte 0:
        [7..2]  0b100010    opcode bit pattern
        [1]     d           0: REG is source. 1: REG is dest.
        [0]     w           0: byte data. 1: word data.
    byte 1:
        [7..6]  mod         mode
                            00: memory mode, no displacement except R/M=0b110
                            01: memory-mode, 8-bit displacement
                            10: memory-mode, 16-bit displacement
                            11: register mode, no displacement <- always this
        [5..3]  reg         register; see register table
        [2..0]  r/m         register; see register table


    Register table:
        -----------
        REG W=0 W=1
        -----------
        000 AL  AX
        001 CL  CX
        010 DL  DX
        011 BL  BX
        100 AH  SP
        101 CH  BP
        110 DH  SI
        111 BH  DI
        -----------

    --------------------------------------------


    Now, we have a few other cases to handle.

    Here's the contents of listing 0039:

        bits 16

        ; Register-to-register
        mov si, bx
        mov dh, al

        ; 8-bit immediate-to-register
        mov cl, 12
        mov ch, -12

        ; 16-bit immediate-to-register
        mov cx, 12
        mov cx, -12
        mov dx, 3948
        mov dx, -3948

        ; Source address calculation
        mov al, [bx + si]
        mov bx, [bp + di]
        mov dx, [bp]

        ; Source address calculation plus 8-bit displacement
        mov ah, [bx + si + 4]

        ; Source address calculation plus 16-bit displacement
        mov al, [bx + si + 4999]

        ; Dest address calculation
        mov [bx + di], cx
        mov [bp + si], cl
        mov [bp], ch

    Again, my job is to recreate these assembly instructions from raw machine
    code.

    Let's look at the mod field first. It'll tell us what to do.

    In addition to register-to-register, I must also handle:
        - immediate-to-register (8- and 16-bit variants)
        - memory to register with various displacements
        - register to memory with various displacements

    byte 0:
        [7..2]  0b100010    opcode bit pattern
        [1]     d           0: REG is source. 1: REG is dest.
        [0]     w           0: byte data. 1: word data.
    byte 1:
        [7..6]  mod         mode
                            00: memory mode, no displacement except R/M=110
                            01: memory-mode, 8-bit displacement
                            10: memory-mode, 16-bit displacement
                            11: register mode, no displacement <- assuming not
        [5..3]  reg         register; see register table
        [2..0]  r/m         for memory moves, tells you what "equation" to use;
                            see effective address table

    --------------------------------
    R/M     Effective Address
    --------------------------------
    000     BX + SI + displacement
    001     BX + DI + displacement
    010     BP + SI + displacement
    011     BP + DI + displacement
    100     SI + displacement
    101     DI + displacement
    110     BP + displacement * unless mod=00, then direct address!
    111     BX + displacement
    --------------------------------
    The displacement is 0, 1, or 2 bytes, depending on the mod field.



*/

#import "Compiler";
#import "Basic";
#import "File";
#import "String";

// Run as script
#run main();

main :: () {
    // Do not produce executable
    set_build_options_dc(.{do_output = false});

    // Read input file
    input := read_entire_file("../submodules/computer_enhance/perfaware/part1/listing_0039_more_movs");
    print("bits 16\n\n");

    machine: [] u8 = .{count = input.count, data = input.data};

    while machine.count > 0 {
        result: string;
        if (machine[0] >> 2) & 0b1111_11 == 0b100010 {
            result = decode_mov(*machine);
        }
        else if (machine[0] >> 4) & 0b1111 == 0b1011 {
            result = immediate_to_register(*machine);
        }
        else {
            result = unimplemented(*machine);
        }
        print("%\n", result);
        reset_temporary_storage();
    }
}

// =============================================================================

immediate_to_register :: (machine: *[] u8) -> string {
    opcode  := (machine.*[0] >> 4) & 0b1111;
    w       := (machine.*[0] >> 3) & 0b1;
    reg     := (machine.*[0] >> 0) & 0b111;

    machine.count -= 1;
    machine.data += 1;

    data: int = machine.*[0];

    machine.count -= 1;
    machine.data += 1;

    if w {
        data += cast(u32, machine.*[0]) << 8;
        machine.count -= 1;
        machine.data += 1;
    }

    the_reg := enum_to_string_lowercase(to_register(reg, w));
    the_immediate := tprint("%", data);

    return tprint("mov %, %", the_reg, the_immediate);
}

// =============================================================================

unimplemented :: (machine: *[] u8) -> string {
    machine.count -= 1;
    machine.data += 1;
    return "unimplemented";
}

// =============================================================================

decode_mov :: (machine: *[] u8) -> string {

    opcode  := (machine.*[0] >> 2) & 0b111111;
    d       := (machine.*[0] >> 1) & 0b1;
    w       := (machine.*[0] >> 0) & 0b1;

    machine.count -= 1;
    machine.data += 1;

    mod     := (machine.*[0] >> 6) & 0b11;
    reg     := (machine.*[0] >> 3) & 0b111;
    rm      := (machine.*[0] >> 0) & 0b111;

    machine.count -= 1;
    machine.data += 1;

    assert(opcode == 0b100010);

    if mod == 0b11 {
        src: Register;
        dst: Register;
        if d {
            dst = to_register(reg, w);
            src = to_register(rm, w);
        }
        else {
            dst = to_register(rm, w);
            src = to_register(reg, w);
        }

        return tprint(
            "mov %, %",
            enum_to_string_lowercase(dst),
            enum_to_string_lowercase(src)
        );
    }
    // weird special case: direct address
    else if mod == 0b00 && rm == 0b110 {
        address := 0;
        address += machine.*[0] << 0;
        address += cast(u32, machine.*[1]) << 8;
        machine.count -= 2;
        machine.data += 2;

        the_reg := enum_to_string_lowercase(to_register(reg, w));
        the_mem := tprint("[%]", address);

        src: string;
        dst: string;
        if d {
            dst = the_reg;
            src = the_mem;
        } else {
            dst = the_mem;
            src = the_reg;
        };

        return tprint("mov %, %", dst, src);
    }
    // memory-to-register or register-to-memory
    else {
        displacement := 0;
        num_displ := mod;
        if num_displ >= 1 {
            displacement += machine.*[0];
            machine.count -= 1;
            machine.data += 1;
        }
        if num_displ >= 2 {
            displacement += cast(u32, machine.*[0]) << 8;
            machine.count -= 1;
            machine.data += 1;
        }

        the_reg := enum_to_string_lowercase(to_register(reg, w));
        builder: String_Builder;
        init_string_builder(*builder);
        builder.allocator = temp;
        append(*builder, "[");
        if rm == {
            case 0b000;
                append(*builder, "bx + si");
            case 0b001;
                append(*builder, "bx + di");
            case 0b010;
                append(*builder, "bp + si");
            case 0b011;
                append(*builder, "bp + di");
            case 0b100;
                append(*builder, "si");
            case 0b101;
                append(*builder, "di");
            case 0b110;
                append(*builder, "bp");
            case 0b111;
                append(*builder, "bx");
        };
        if displacement > 0 {
            print_to_builder(*builder, " + %", displacement);
        };
        append(*builder, "]");
        the_mem := builder_to_string(*builder);

        src: string;
        dst: string;
        if d {
            dst = the_reg;
            src = the_mem;
        } else {
            dst = the_mem;
            src = the_reg;
        };

        return tprint("mov %, %", dst, src);
    }
}

// =============================================================================

/*
    -----------
    REG W=0 W=1
    -----------
    000 AL  AX
    001 CL  CX
    010 DL  DX
    011 BL  BX
    100 AH  SP
    101 CH  BP
    110 DH  SI
    111 BH  DI
    -----------
*/

// Here, I concatenate REG and w.
Register :: enum u8 #specified {
    AL :: 0b000_0;
    CL :: 0b001_0;
    DL :: 0b010_0;
    BL :: 0b011_0;
    AH :: 0b100_0;
    CH :: 0b101_0;
    DH :: 0b110_0;
    BH :: 0b111_0;

    AX :: 0b000_1;
    CX :: 0b001_1;
    DX :: 0b010_1;
    BX :: 0b011_1;
    SP :: 0b100_1;
    BP :: 0b101_1;
    SI :: 0b110_1;
    DI :: 0b111_1;
}

to_register :: (reg: u8, w: u8) -> Register {
    return cast(Register) ((reg << 1) | w);
}

// =============================================================================

// I wanted to get a little funky with metaprogramming.
// This hairball of code takes in ANY enum value and returns that
// enum variant's name. There's probably better ways to do this.

enum_to_string_lowercase :: (value: $T) -> string {
    #assert type_info(T).type == .ENUM;
    #insert -> string {
        builder : String_Builder;
        init_string_builder(*builder);
        print_to_builder(*builder, "if value == {");
        info := cast(*Type_Info_Enum) type_info(T);
        for name, index: info.names {
            print_to_builder(
                *builder, "case %; return \"%\";",
                info.values[index],
                to_lower_copy(name)
            );
        }
        append(*builder, "}");
        return builder_to_string(*builder);
    };
    return "(invalid)";
}


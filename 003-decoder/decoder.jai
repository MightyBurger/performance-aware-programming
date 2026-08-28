// Copyright 2026 Jordan Johnson
// Reference around page 4-22 of Intel 8086 Family User's Manual

/*
    A few styles:
    (1) Register and memory             [OPCODE DW] [MOD REG R/M] DISP
    (2) Immediate                       [OPCODE  W] [MOD xyz R/M] IMM
    (3) Immediate to Accumulator        [OPCODE  W] IMM
    (4) Conditional jump instructions   [01110101] [DISP]

    xyz:
    ADD 000
    SUB 101
    CMP 111
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

    original := read_entire_file("../submodules/computer_enhance/perfaware/part1/listing_0041_add_sub_cmp_jnz.asm");
    print("--- Original ---\n%\n\n\n", original);

    bin0 := read_entire_file("../submodules/computer_enhance/perfaware/part1/listing_0041_add_sub_cmp_jnz");
    print("--- Decoded ---\n%", run(bin0));

}

run :: (input: string) -> string {


    builder: String_Builder;
    init_string_builder(*builder);

    print_to_builder(*builder, "bits 16\n\n");

    machine: [] u8 = .{count = input.count, data = input.data};

    while machine.count > 0 {
        result: string;

        // MOV instructions
        if (machine.data.* >> 2) & 0b1111_11 == 0b100010 {
            result = decode_pattern_rm_to_rm(*machine, "mov");
        }
        else if (machine.data.* >> 1) & 0b1111111 == 0b1100011 {
            result = decode_pattern_imm_to_rm(*machine, "mov");
        }
        else if (machine.data.* >> 4) & 0b1111 == 0b1011 {
            result = decode_mov_immediate_to_register(*machine);
        }
        else if (machine.data.* >> 1) & 0b1111111 == 0b1010000 {
            result = decode_mov_memory_to_accumulator(*machine);
        }
        else if (machine.data.* >> 1) & 0b1111111 == 0b1010001 {
            result = decode_mov_accumulator_to_memory(*machine);
        }

        // Arithmetic instructions
        else if (machine.data.* >> 2) & 0b111111 == 0b000000 {
            result = decode_pattern_rm_to_rm(*machine, "add");
        }
        else if (machine.data.* >> 2) & 0b111111 == 0b001010 {
            result = decode_pattern_rm_to_rm(*machine, "sub");
        }
        else if (machine.data.* >> 2) & 0b111111 == 0b001110 {
            result = decode_pattern_rm_to_rm(*machine, "cmp");
        }
        else if (machine.data.* >> 2) & 0b111111 == 0b100000 {
            // (NOTE: ignoring sign extension bit!!)
            subop := (machine.data[1] >> 3) & 0b111;
            if subop == {
                case 0b000; result = decode_pattern_imm_to_rm(*machine, "add");
                case 0b101; result = decode_pattern_imm_to_rm(*machine, "sub");
                case 0b111; result = decode_pattern_imm_to_rm(*machine, "cmp");
                case;       result = unimplemented(*machine);
            }
        }
        else if (machine.data.* >> 1) & 0b1111111 == 0b0000010 {
            result = decode_pattern_imm_to_acc(*machine, "add");
        }
        else if (machine.data.* >> 1) & 0b1111111 == 0b0010110 {
            result = decode_pattern_imm_to_acc(*machine, "sub");
        }
        else if (machine.data.* >> 1) & 0b1111111 == 0b0011110 {
            result = decode_pattern_imm_to_acc(*machine, "cmp");
        }
        

        // Unrecognized
        else {
            result = unimplemented(*machine);
        }

        print_to_builder(*builder, "%\n", result);
        reset_temporary_storage();
    }

    return builder_to_string(*builder);
}
// =============================================================================

unimplemented :: (machine: *[] u8) -> string {
    machine.count -= 1;
    machine.data += 1;
    return "unimplemented";
}

// =============================================================================

// Style (1)        [OPCODE DW] [MOD REG R/M] DISP
decode_pattern_rm_to_rm :: (machine: *[] u8, prefix: string) -> string {

    opcode  := (machine.data.* >> 2) & 0b111111;
    d       := (machine.data.* >> 1) & 0b1;
    w       := (machine.data.* >> 0) & 0b1;

    machine.count -= 1;
    machine.data += 1;

    mod     := (machine.data.* >> 6) & 0b11;
    reg     := (machine.data.* >> 3) & 0b111;
    rm      := (machine.data.* >> 0) & 0b111;

    machine.count -= 1;
    machine.data += 1;

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
            "% %, % ; style 1",
            prefix,
            enum_to_string_lowercase(dst),
            enum_to_string_lowercase(src)
        );
    }
    // weird special case: direct address
    else if mod == 0b00 && rm == 0b110 {
        address := cast(*u16, machine.data).*;
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

        return tprint("% %, %", prefix, dst, src);
    }
    // memory-to-register or register-to-memory
    else {
        displacement : int = 0;
        num_displ := mod;
        if num_displ == 1 {
            displacement = cast(*s8, machine.data).*;
            machine.count -= 1;
            machine.data += 1;
        }
        else if num_displ == 2 {
            ptr := cast(*s16, machine.data);
            displacement = cast(*s16, machine.data).*;
            machine.count -= 2;
            machine.data += 2;
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
        }
        if displacement > 0 {
            print_to_builder(*builder, " + %", displacement);
        }
        else if displacement < 0 {
            print_to_builder(*builder, " - %", -displacement);
        }
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

        return tprint("% %, % ; style 1", prefix, dst, src);
    }
}

// =============================================================================

// Style (2)    [OPCODE SW] [MOD xyz R/m] [IMM]
// Note, you will need to peek ahead of time to obtain the prefix.
decode_pattern_imm_to_rm :: (machine: *[] u8, prefix: string) -> string {

    machine_at_start := machine.*;

    opcode  := (machine.data.* >> 2) & 0b011_1111;
    s       := (machine.data.* >> 1) & 0b1;
    w       := (machine.data.* >> 0) & 0b1;

    machine.count -= 1;
    machine.data += 1;

    mod     := (machine.data.* >> 6) & 0b11;
    xyz     := (machine.data.* >> 3) & 0b111;
    rm      := (machine.data.* >> 0) & 0b111;

    machine.count -= 1;
    machine.data += 1;

    // register mode
    if mod == 0b11 {
        data : int;
        if w && s {
            data = cast(*s8, machine.data).*;
            machine.count -= 1;
            machine.data += 1;
        }
        else if w {
            data = cast(*s16, machine.data).*;
            machine.count -= 2;
            machine.data += 2;
        }
        else {
            data = cast(*u8, machine.data).*;
            machine.count -= 1;
            machine.data += 1;
        }

        the_reg := enum_to_string_lowercase(to_register(rm, w));
        the_immediate := tprint("%", data);

        debug_string := tprint(
            "style 2: imm to rm. s: %.w: %. [% %]",
            s,
            w,
            formatInt(machine_at_start[0], minimum_digits=2, base=16),
            formatInt(machine_at_start[1], minimum_digits=2, base=16),
        );

        return tprint(
            "% %, % ; %",
            prefix,
            the_reg,
            the_immediate,
            debug_string
        );
    }

    // weird special case: direct address
    else if mod == 0b00 && rm == 0b110 {
        address := cast(*u16, machine.data).*;
        machine.count -= 2;
        machine.data += 2;

        data : int;
        if w && s {
            data = cast(*s8, machine.data).*;
            machine.count -= 1;
            machine.data += 1;
        }
        else if w {
            data = cast(*s16, machine.data).*;
            machine.count -= 2;
            machine.data += 2;
        }
        else {
            data = cast(*u8, machine.data).*;
            machine.count -= 1;
            machine.data += 1;
        }

        the_mem := tprint("[%]", address);
        the_immediate := tprint("%", data);

        return tprint("% %, % ; style 2: imm to rm, special case direct address", prefix, the_mem, the_immediate);
    }
    // memory mode
    else {
        displacement : int = 0;
        num_displ := mod;
        if num_displ == 1 {
            displacement = cast(*s8, machine.data).*;
            machine.count -= 1;
            machine.data += 1;
        }
        else if num_displ == 2 {
            ptr := cast(*s16, machine.data);
            displacement = cast(*s16, machine.data).*;
            machine.count -= 2;
            machine.data += 2;
        }

        data : int;
        if w && s {
            data = cast(*s8, machine.data).*;
            machine.count -= 1;
            machine.data += 1;
        }
        else if w {
            data = cast(*s16, machine.data).*;
            machine.count -= 2;
            machine.data += 2;
        }
        else {
            data = cast(*u8, machine.data).*;
            machine.count -= 1;
            machine.data += 1;
        }

        the_immediate : string;
        if !w {
            the_immediate = tprint("byte %", data);
        } else {
            the_immediate = tprint("word %", data);
        }


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
        }
        if displacement > 0 {
            print_to_builder(*builder, " + %", displacement);
        }
        else if displacement < 0 {
            print_to_builder(*builder, " - %", -displacement);
        }
        append(*builder, "]");
        the_mem := builder_to_string(*builder);

        return tprint("% %, % ; style 2: imm to rm, with memory displacement", prefix, the_mem, the_immediate);
    }
}

// =============================================================================

// Style (3)        [OPCODE W] [DISP]
decode_pattern_imm_to_acc :: (machine: *[] u8, prefix: string) -> string {
    opcode  := (machine.data.* >> 1) & 0b111_1111;
    w       := (machine.data.* >> 0) & 0b1;

    machine.count -= 1;
    machine.data += 1;

    data : int;
    if !w {
        data = cast(*s8, machine.data).*;
        machine.count -= 1;
        machine.data += 1;
    }
    else {
        data = cast(*s16, machine.data).*;
        machine.count -= 2;
        machine.data += 2;
    }

    the_reg: string;
    if w {
        the_reg = "ax";
    } else {
        the_reg = "al";
    }
    the_immediate := tprint("%", data);

    return tprint("% %, %", prefix, the_reg, the_immediate);
}


// =============================================================================

// MOV-specific pattern
decode_mov_immediate_to_register :: (machine: *[] u8) -> string {
    opcode  := (machine.data.* >> 4) & 0b1111;
    w       := (machine.data.* >> 3) & 0b1;
    reg     := (machine.data.* >> 0) & 0b111;

    machine.count -= 1;
    machine.data += 1;

    data : int;
    if !w {
        data = cast(*s8, machine.data).*;
        machine.count -= 1;
        machine.data += 1;
    }
    else {
        data = cast(*s16, machine.data).*;
        machine.count -= 2;
        machine.data += 2;
    }

    the_reg := enum_to_string_lowercase(to_register(reg, w));
    the_immediate := tprint("%", data);

    return tprint("mov %, %", the_reg, the_immediate);
}

// MOV-specific pattern
decode_mov_memory_to_accumulator :: (machine: *[] u8) -> string {

    opcode  := (machine.data.* >> 1) & 0b0111_1111;
    w       := (machine.data.* >> 0) & 0b1;
    machine.data += 1;
    machine.count -= 1;

    addr    := cast(*u16, machine.data).*;
    machine.data += 2;
    machine.count -= 2;
    the_mem := tprint("[%]", addr);

    the_reg: string;
    if w {
        the_reg = "ax";
    } else {
        the_reg = "al";
    }

    return tprint(
        "mov %, %",
        the_reg,
        the_mem,
    );
}

// MOV-specific pattern
decode_mov_accumulator_to_memory :: (machine: *[] u8) -> string {

    opcode  := (machine.data.* >> 1) & 0b0111_1111;
    w       := (machine.data.* >> 0) & 0b1;
    machine.data += 1;
    machine.count -= 1;

    addr    := cast(*u16, machine.data).*;
    machine.data += 2;
    machine.count -= 2;
    the_mem := tprint("[%]", addr);

    the_reg: string;
    if w {
        the_reg = "ax";
    } else {
        the_reg = "al";
    }

    return tprint(
        "mov %, %",
        the_mem,
        the_reg,
    );
}

// =============================================================================

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


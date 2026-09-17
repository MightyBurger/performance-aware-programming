// Copyright 2026 Jordan Johnson
// Reference around page 4-22 of Intel 8086 Family User's Manual
//
// A warning to readers:
// This is really quite nasty code.
// The purpose of this code is just to build the intuition Casey's course is
// trying to teach, and to dip my toes into the Jai language, so I did not
// bother trying to make it maintainable at all.

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

Computer :: struct {
    registers : [8] u16; //ax, bx, cx, dx, sp, bp, si, di
    flags     : Flags;
}

get_reg :: (computer: *Computer, register: Register) -> *u16 {
    if register == {
        // case .AL; *computer.registers[0],
        // case .BL; *computer.registers[0],
        // case .CL; *computer.registers[0],
        // case .DL; *computer.registers[0],
        // case .AH; *computer.registers[0],
        // case .CH; *computer.registers[0],
        // case .DH; *computer.registers[0],
        // case .BH; *computer.registers[0],

        case .AX; return *computer.registers[0];
        case .BX; return *computer.registers[1];
        case .CX; return *computer.registers[2];
        case .DX; return *computer.registers[3];
        case .SP; return *computer.registers[4];
        case .BP; return *computer.registers[5];
        case .SI; return *computer.registers[6];
        case .DI; return *computer.registers[7];
    }
    print("ERROR: Unsupported register\n");
    exit(-1);
    return null;
}

Flags :: struct {
    zero : bool;
    sign : bool;
}

get_flags :: (value: u16) -> Flags {
    zero := value == 0;
    sign := value & 0x8000 == 0x8000;
    return Flags.{zero = zero, sign = sign};
}

// Run as script
#run main();

main :: () {
    // Do not produce executable
    set_build_options_dc(.{do_output = false});

    // args := get_command_line_arguments(); // Oops, this is for runtime command line args. Leaving for future reference!
    args := get_build_options().compile_time_command_line;
    if args.count != 1 {
        print("Pass exactly one argument, the path to the 8086 binary to execute.\n");
        exit(-1);
    }
    file : string = args[0];

    bin0, success := read_entire_file(file);
    if !success {
        // read_entire_file logs for us, no need to print an error.
        exit(-1);
    }
    print("Simulating %\n", file);

    computer: Computer;
    run(.{count=bin0.count, data=bin0.data}, *computer);
    print("\nFinal registers:\n");
    print("    ax: 0x% (%)\n", formatInt(computer.registers[0], base = 16, minimum_digits = 4), computer.registers[0]);
    print("    bx: 0x% (%)\n", formatInt(computer.registers[1], base = 16, minimum_digits = 4), computer.registers[1]);
    print("    cx: 0x% (%)\n", formatInt(computer.registers[2], base = 16, minimum_digits = 4), computer.registers[2]);
    print("    dx: 0x% (%)\n", formatInt(computer.registers[3], base = 16, minimum_digits = 4), computer.registers[3]);
    print("    sp: 0x% (%)\n", formatInt(computer.registers[4], base = 16, minimum_digits = 4), computer.registers[4]);
    print("    bp: 0x% (%)\n", formatInt(computer.registers[5], base = 16, minimum_digits = 4), computer.registers[5]);
    print("    si: 0x% (%)\n", formatInt(computer.registers[6], base = 16, minimum_digits = 4), computer.registers[6]);
    print("    di: 0x% (%)\n", formatInt(computer.registers[7], base = 16, minimum_digits = 4), computer.registers[7]);
    print(" flags:");
    if computer.flags.zero then print(" zero");
    if computer.flags.sign then print(" sign");
    print("\n");
}

run :: (machine: [] u8, computer: *Computer) {

    builder: String_Builder;
    init_string_builder(*builder);

    print_to_builder(*builder, "bits 16\n\n");

    while machine.count > 0 {
        result: string;

        // noop_fn :: (dest: *u16, src: *u16, flags: *Flags) {}
        mov_fn :: (dest: *u16, src: *u16, flags: *Flags) {
            dest.* = src.*;
        }
        add_fn :: (dest: *u16, src: *u16, flags: *Flags) {
            dest.* = dest.* + src.*;
            flags.* = get_flags(dest.*);
        }
        sub_fn :: (dest: *u16, src: *u16, flags: *Flags) {
            dest.* = dest.* - src.*;
            flags.* = get_flags(dest.*);
        }
        cmp_fn :: (dest: *u16, src: *u16, flags: *Flags) {
            would_be_dest := dest.* - src.*;
            flags.* = get_flags(dest.*);
        }

        // MOV instructions
        if      (machine.data.* >> 2) & 0b1111_11 == 0b100010   { result = decode_pattern_rm_to_rm(*machine, computer, "mov", mov_fn); }
        else if (machine.data.* >> 1) & 0b1111111 == 0b1100011  { result = decode_pattern_imm_to_rm(*machine, computer, "mov", mov_fn); }
        else if (machine.data.* >> 4) & 0b1111    == 0b1011     { result = decode_mov_immediate_to_register(*machine, computer); }
        else if (machine.data.* >> 1) & 0b1111111 == 0b1010000  { result = decode_mov_memory_to_accumulator(*machine); }
        else if (machine.data.* >> 1) & 0b1111111 == 0b1010001  { result = decode_mov_accumulator_to_memory(*machine); }

        // Arithmetic instructions
        else if (machine.data.* >> 2) & 0b111111 == 0b000000 { result = decode_pattern_rm_to_rm(*machine, computer, "add", add_fn); }
        else if (machine.data.* >> 2) & 0b111111 == 0b001010 { result = decode_pattern_rm_to_rm(*machine, computer, "sub", sub_fn); }
        else if (machine.data.* >> 2) & 0b111111 == 0b001110 { result = decode_pattern_rm_to_rm(*machine, computer, "cmp", cmp_fn); }
        else if (machine.data.* >> 2) & 0b111111 == 0b100000 {
            subop := (machine.data[1] >> 3) & 0b111;
            if subop == {
                case 0b000; result = decode_pattern_imm_to_rm(*machine, computer, "add", add_fn);
                case 0b101; result = decode_pattern_imm_to_rm(*machine, computer, "sub", sub_fn);
                case 0b111; result = decode_pattern_imm_to_rm(*machine, computer, "cmp", cmp_fn);
                case;       result = unimplemented(*machine);
            }
        }
        else if (machine.data.* >> 1) & 0b1111111 == 0b0000010 { result = decode_pattern_imm_to_acc(*machine, computer, "add", add_fn); }
        else if (machine.data.* >> 1) & 0b1111111 == 0b0010110 { result = decode_pattern_imm_to_acc(*machine, computer, "sub", sub_fn); }
        else if (machine.data.* >> 1) & 0b1111111 == 0b0011110 { result = decode_pattern_imm_to_acc(*machine, computer, "cmp", cmp_fn); }

        // jump instructions
        else if machine.data.* == 0b01110100 {result = decode_pattern_jump(*machine, "je");}
        else if machine.data.* == 0b01111100 {result = decode_pattern_jump(*machine, "jl");}
        else if machine.data.* == 0b01111110 {result = decode_pattern_jump(*machine, "jle");}
        else if machine.data.* == 0b01110010 {result = decode_pattern_jump(*machine, "jb");}
        else if machine.data.* == 0b01110110 {result = decode_pattern_jump(*machine, "jbe");}
        else if machine.data.* == 0b01111010 {result = decode_pattern_jump(*machine, "jp");}
        else if machine.data.* == 0b01110000 {result = decode_pattern_jump(*machine, "jo");}
        else if machine.data.* == 0b01111000 {result = decode_pattern_jump(*machine, "js");}
        else if machine.data.* == 0b01110101 {result = decode_pattern_jump(*machine, "jne");}
        else if machine.data.* == 0b01111101 {result = decode_pattern_jump(*machine, "jnl");}
        else if machine.data.* == 0b01111111 {result = decode_pattern_jump(*machine, "jnle");}
        else if machine.data.* == 0b01110011 {result = decode_pattern_jump(*machine, "jnb");}
        else if machine.data.* == 0b01110111 {result = decode_pattern_jump(*machine, "jnbe");}
        else if machine.data.* == 0b01111011 {result = decode_pattern_jump(*machine, "jnp");}
        else if machine.data.* == 0b01110001 {result = decode_pattern_jump(*machine, "jno");}
        else if machine.data.* == 0b01111001 {result = decode_pattern_jump(*machine, "jns");}
        else if machine.data.* == 0b11100010 {result = decode_pattern_jump(*machine, "loop");}
        else if machine.data.* == 0b11100001 {result = decode_pattern_jump(*machine, "loopz");}
        else if machine.data.* == 0b11100000 {result = decode_pattern_jump(*machine, "loopnz");}
        else if machine.data.* == 0b11100011 {result = decode_pattern_jump(*machine, "jcxz");}


        // Unrecognized
        else {
            result = unimplemented(*machine);
        }

        print_to_builder(*builder, "%\n", result);
        reset_temporary_storage();
    }

    print(builder_to_string(*builder));
    return;
}

// =============================================================================

unimplemented :: (machine: *[] u8) -> string {
    machine.count -= 1;
    machine.data += 1;
    return "unimplemented";
}

decode_pattern_jump :: (machine: *[] u8, prefix: string) -> string {

    opcode := machine.data.*;
    machine.count -= 1;
    machine.data += 1;
    displacement := cast(*s8, machine.data).*;
    machine.count -= 1;
    machine.data += 1;

    return tprint("% %", prefix, displacement);

}

// =============================================================================

// Style (1)        [OPCODE DW] [MOD REG R/M] DISP
decode_pattern_rm_to_rm :: (machine: *[] u8, computer: *Computer, prefix: string, op: (dest: *u16, src: *u16, flags: *Flags)) -> string {

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

        op_dst := get_reg(computer, dst);
        op_src := get_reg(computer, src);
        op(op_dst, op_src, *computer.flags);

        return tprint(
            "% %, %",
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

        return tprint("% %, %", prefix, dst, src);
    }
}

// =============================================================================

// Style (2)    [OPCODE SW] [MOD xyz R/m] [IMM]
// Note, you will need to peek ahead of time to obtain the prefix.
decode_pattern_imm_to_rm :: (machine: *[] u8, computer: *Computer, prefix: string, op: (dest: *u16, src: *u16, flags: *Flags)) -> string {

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

        // Apply the operation to the appropriate data
        // TODO only handling u16s now!
        register := to_register(rm, w);
        dest := get_reg(computer, register);
        op(dest, cast(*u16) *data, *computer.flags);

        the_reg := enum_to_string_lowercase(register);
        the_immediate := tprint("%", data);

        return tprint(
            "% %, %",
            prefix,
            the_reg,
            the_immediate
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

        return tprint("% %, %", prefix, the_mem, the_immediate);
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

        the_immediate := tprint("%", data);

        the_modifier : string;
        if w {the_modifier = "word";} else {the_modifier = "byte";}

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

        return tprint("% % %, %", prefix, the_modifier, the_mem, the_immediate);
    }
}

// =============================================================================

// Style (3)        [OPCODE W] [DISP]
decode_pattern_imm_to_acc :: (machine: *[] u8, computer: *Computer, prefix: string, op: (dest: *u16, src: *u16, flags: *Flags)) -> string {
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

    op(get_reg(computer, .AX), cast(*u16) *data, *computer.flags);

    return tprint("% %, %", prefix, the_reg, the_immediate);
}


// =============================================================================

// MOV-specific pattern
decode_mov_immediate_to_register :: (machine: *[] u8, computer: *Computer) -> string {
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

    reg_enum := to_register(reg, w);
    reg_p := get_reg(computer, reg_enum);
    reg_p.* = cast,no_check(u16, data); // TODO only handling 16-bit for now

    the_reg := enum_to_string_lowercase(reg_enum);
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

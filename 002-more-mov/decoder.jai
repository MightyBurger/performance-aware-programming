// Copyright 2026 Jordan Johnson

// Reference around page 4-22 of Intel 8086 Family User's Manual

#import "Compiler";
#import "Basic";
#import "File";
#import "String";

// Run as script
#run main();

main :: () {
    // Do not produce executable
    set_build_options_dc(.{do_output = false});

    bin0 := read_entire_file("../submodules/computer_enhance/perfaware/part1/listing_0039_more_movs");
    bin1 := read_entire_file("../submodules/computer_enhance/perfaware/part1/listing_0040_challenge_movs");

    print("--- More Movs ---\n");
    print("%\n\n\n", run(bin0));


    print("--- Challenge Movs ---\n");
    print("%\n\n\n", run(bin1));
}

run :: (input: string) -> string {

    print("bits 16\n\n");

    builder: String_Builder;
    init_string_builder(*builder);

    machine: [] u8 = .{count = input.count, data = input.data};

    while machine.count > 0 {
        result: string;
        if (machine.data.* >> 2) & 0b1111_11 == 0b100010 {
            result = decode_mov(*machine);
        }
        else if (machine.data.* >> 1) & 0b1111111 == 0b1100011 {
            result = decode_immediate_to_register_memory(*machine);
        }
        else if (machine.data.* >> 4) & 0b1111 == 0b1011 {
            result = decode_immediate_to_register(*machine);
        }
        else if (machine.data.* >> 1) & 0b1111111 == 0b1010000 {
            result = decode_memory_to_accumulator(*machine);
        }
        else if (machine.data.* >> 1) & 0b1111111 == 0b1010001 {
            result = decode_accumulator_to_memory(*machine);
        }
        else {
            result = unimplemented(*machine);
        }
        print_to_builder(*builder, "%\n", result);
        reset_temporary_storage();
    }

    return builder_to_string(*builder);
}

// =============================================================================

decode_memory_to_accumulator :: (machine: *[] u8) -> string {

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

decode_accumulator_to_memory :: (machine: *[] u8) -> string {

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

decode_immediate_to_register_memory :: (machine: *[] u8) -> string {

    opcode  := (machine.data.* >> 1) & 0b0111_1111;
    w       := (machine.data.* >> 0) & 0b1;

    machine.count -= 1;
    machine.data += 1;

    mod     := (machine.data.* >> 6) & 0b11;
    zero    := (machine.data.* >> 3) & 0b111;
    rm      := (machine.data.* >> 0) & 0b111;

    machine.count -= 1;
    machine.data += 1;

    assert(opcode == 0b1100011);
    assert(zero == 0);

    // register mode
    if mod == 0b11 {
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

        the_reg := to_register(rm, w);
        the_immediate := tprint("%", data);

        return tprint(
            "mov %, %",
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

        the_mem := tprint("[%]", address);
        the_immediate := tprint("%", data);

        return tprint("mov %, %", the_mem, the_immediate);
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

        return tprint("mov %, %", the_mem, the_immediate);
    }
}

// =============================================================================

decode_immediate_to_register :: (machine: *[] u8) -> string {
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

// =============================================================================

unimplemented :: (machine: *[] u8) -> string {
    machine.count -= 1;
    machine.data += 1;
    return "unimplemented";
}

// =============================================================================

decode_mov :: (machine: *[] u8) -> string {

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

        return tprint("mov %, %", dst, src);
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

        return tprint("mov %, %", dst, src);
    }
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

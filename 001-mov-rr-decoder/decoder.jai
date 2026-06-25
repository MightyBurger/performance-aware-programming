// Copyright 2026 Jordan Johnson

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
    // input := read_entire_file("inputs/listing_0037_single_register_mov");
    input := read_entire_file("inputs/listing_0038_many_register_mov");

    print("bits 16\n\n");

    for instr: 0..input.count / 2 - 1 {
        idx := instr * 2;
        mov : [2] u8;
        mov[0] = input[idx];
        mov[1] = input[idx + 1];
        print("%\n", decode_mov(mov));
    };
}

// =============================================================================

decode_mov :: (machine: [2] u8) -> string {

    opcode  := (machine[0] >> 2) & 0b111111;
    d       := (machine[0] >> 1) & 0b1;
    w       := (machine[0] >> 0) & 0b1;

    mod     := (machine[1] >> 6) & 0b11;
    reg     := (machine[1] >> 3) & 0b111;
    rm      := (machine[1] >> 0) & 0b111;

    assert(opcode == 0b100010);
    assert(mod == 0b11);

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

    return sprint(
        "mov %, %",
        enum_to_string_lowercase(dst),
        enum_to_string_lowercase(src)
    );
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


`default_nettype none

`define CLK_PERIOD 2

`define POSITIVE 1'b0
`define NEGATIVE 1'b1

`define I   0
`define II  1
`define III 2

`define IN  0
`define OUT 1

// TODO:
// * Fix support for double step
// * Make rotor selection adjustable
// * Finish FSM for setting up the machine state

typedef enum logic [4:0] {A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z} letter_t;

module rotor #(parameter TYPE=`I, DIR = `IN)
(input letter_t in_letter, output letter_t out_letter);

	letter_t [0:25] lut; // This won't synthesize in Quartus for some reason...

	generate
		if(DIR == `IN) begin
			if(TYPE == `I)
				assign lut = {E,K,M,F,L,G,D,Q,V,Z,N,T,O,W,Y,H,X,U,S,P,A,I,B,R,C,J};
			else if(TYPE == `II)
				assign lut = {A,J,D,K,S,I,R,U,X,B,L,H,W,T,M,C,Q,G,Z,N,P,Y,F,V,O,E};
			else if(TYPE == `III)
				assign lut = {B,D,F,H,J,L,C,P,R,T,X,V,Z,N,Y,E,I,W,G,A,K,M,U,S,Q,O};
			else
				initial
					assert(1) $fatal("ERROR: Need to specify a valid rotor type.");
		end else begin
			if(TYPE == `I)
				assign lut = {U,W,Y,G,A,D,F,P,V,Z,B,E,C,K,M,T,H,X,S,L,R,I,N,Q,O,J};
			else if(TYPE == `II)
				assign lut = {A,J,P,C,Z,W,R,L,F,B,D,K,O,T,Y,U,Q,G,E,N,H,X,M,I,V,S};
			else if(TYPE == `III)
				assign lut = {T,A,G,B,P,C,S,D,Q,E,U,F,V,N,Z,H,Y,I,X,J,W,L,R,K,O,M};
			else
				initial
					assert(1) $fatal("ERROR: Need to specify a valid rotor type.");
		end
	endgenerate

	assign out_letter = lut[in_letter];

endmodule: rotor

module rotor_with_mod #(parameter TYPE=`I, DIR=`IN) (input letter_t in_letter, input logic [4:0] rot_pos, output letter_t out_letter);

	letter_t rotor_in, rotor_out;

	mod #(.W(5), .N(26)) m0(.in(in_letter), .offset(rot_pos), .offset_sign(`POSITIVE), .out(rotor_in));
	rotor #(.TYPE(TYPE), .DIR(DIR)) rot(.in_letter(rotor_in), .out_letter(rotor_out));
	mod #(.W(5), .N(26)) m1(.in(rotor_out), .offset(rot_pos), .offset_sign(`NEGATIVE), .out(out_letter));

endmodule

module reflector (input letter_t in_letter, output letter_t out_letter);

	letter_t [0:25] lut;
	assign lut = {Y,R,U,H,Q,S,L,D,P,X,N,G,O,K,M,I,E,B,F,Z,C,W,V,J,A,T};
	assign out_letter = lut[in_letter];

endmodule

module mod #(parameter W=4, N=4)
(input logic [W-1:0] in, input logic [W-1:0] offset, input logic offset_sign, output logic [W-1:0] out);
	logic [W+1:0] sum;
	assign sum = (offset_sign == `POSITIVE) ? in + offset : in - offset;
	assign out = (sum[W+1]) ? (N + sum) : (sum >= N) ? (sum - N) : sum;
endmodule: mod

module rotor_box(input letter_t in_letter, input logic [2:0][4:0] rot_pos, output letter_t out_letter);

	letter_t r0_out, r1_out, r2_out, r3_out, r4_out, r5_out;
	letter_t ref_out;

	rotor_with_mod #(.TYPE(`III), .DIR(`IN)) r0(.in_letter(in_letter), .rot_pos(rot_pos[0]), .out_letter(r0_out));
	rotor_with_mod #(.TYPE(`II), .DIR(`IN)) r1(.in_letter(r0_out), .rot_pos(rot_pos[1]), .out_letter(r1_out));
	rotor_with_mod #(.TYPE(`I), .DIR(`IN)) r2(.in_letter(r1_out), .rot_pos(rot_pos[2]), .out_letter(r2_out));

	reflector refl(.in_letter(r2_out), .out_letter(ref_out));

	rotor_with_mod #(.TYPE(`I), .DIR(`OUT)) r3(.in_letter(ref_out), .rot_pos(rot_pos[2]), .out_letter(r3_out));
	rotor_with_mod #(.TYPE(`II), .DIR(`OUT)) r4(.in_letter(r3_out), .rot_pos(rot_pos[1]), .out_letter(r4_out));
	rotor_with_mod #(.TYPE(`III), .DIR(`OUT)) r5(.in_letter(r4_out), .rot_pos(rot_pos[0]), .out_letter(out_letter));

endmodule: rotor_box

module ff_ar #(parameter W=1, RV={W{1'b0}})
(input logic clk, rst, input logic[W-1:0] d, output logic [W-1:0] q);
	always_ff @(posedge clk, posedge rst) begin
		if(rst) q <= RV;
		else q <= d;
	end
endmodule: ff_ar

module negedge_detector (input logic in, output logic ed, input logic clk, rst);
	logic in_q;
	ff_ar #(.W(1), .RV(1'b1)) in_ff(.d(in), .q(in_q), .clk, .rst);
	assign ed = ~in & in_q;
endmodule: negedge_detector

module rotor_fsm(
	input logic clk, rst,
	input letter_t in_letter,
	input logic enable,
	input logic rotor_up, rotor_down, // TODO
	input logic r0_set, r1_set, r2_set, // TODO
	input logic r0_choose, r1_choose, r2_choose, // TODO
	output letter_t out_letter);

	logic [2:0][4:0] rot_pos_n;
	logic [2:0][4:0] rot_pos;

	// TODO: this implements double step but it is specifically for rotors I, II, and III
	//       make this general
	assign rot_pos_n[0] = (enable)                                                                ? ((rot_pos[0] == 25) ? 5'b0 : rot_pos[0] + 1'b1) : rot_pos[0];
	assign rot_pos_n[1] = (enable && (rot_pos[0] == 21 || (rot_pos[0] == 22 && rot_pos[1] == 4))) ? ((rot_pos[1] == 25) ? 5'b0 : rot_pos[1] + 1'b1) : rot_pos[1];
	assign rot_pos_n[2] = (enable && rot_pos[1] == 4)                                             ? ((rot_pos[2] == 25) ? 5'b0 : rot_pos[2] + 1'b1) : rot_pos[2];

	ff_ar #(.W(5), .RV(5'b0)) rot0_pos_ff(.q(rot_pos[0]), .d(rot_pos_n[0]), .clk, .rst);
	ff_ar #(.W(5), .RV(5'b0)) rot1_pos_ff(.q(rot_pos[1]), .d(rot_pos_n[1]), .clk, .rst);
	ff_ar #(.W(5), .RV(5'b0)) rot2_pos_ff(.q(rot_pos[2]), .d(rot_pos_n[2]), .clk, .rst);

	rotor_box rb(.*, .rot_pos(rot_pos_n));

endmodule: rotor_fsm

module plugboard(
		input letter_t first,
		input letter_t second,
		input logic enable_swap,
		input logic clear,
		input letter_t first_in_letter,
		input letter_t second_in_letter,
		output letter_t first_out_letter,
		output letter_t second_out_letter,
		input logic clk, rst);

	letter_t [0:25] lut, lut_n;

	always_comb begin
		integer i;
		lut_n = lut;
		if(clear) begin
			for(i=0; i<26; i++)
				lut_n[i] = letter_t' (i);
		end
		else if(enable_swap) begin
			lut_n[first] = second;
			lut_n[second] = first;
		end
	end

	ff_ar #(.W($bits(letter_t))) plugs [0:25] (.q(lut), .d(lut_n), .clk, .rst);

	assign first_out_letter = lut[first_in_letter];
	assign second_out_letter = lut[second_in_letter];

endmodule: plugboard

module enigma_fsm(
	input logic done_pb_button,
	input logic next_rotor_button,
	input logic set_rotor_button,
	input logic set_plug_button,
	input logic rotor_up_button, rotor_down_button,
	input logic encrypt_button,
	output logic r0_choose, r1_choose, r2_choose,
	output logic r0_set, r1_set, r2_set,
	output logic rotor_up, rotor_down,
	output logic enable_swap_pb,
	output logic clear_pb,
	output logic enable_rf,
	output logic [2:0] state,
	input logic clk, rst);

	enum logic [2:0] {R0_CHOOSE, R1_CHOOSE, R2_CHOOSE, R0_SET, R1_SET, R2_SET, PLUG, ENCRYPT} cs, ns;

	assign state = cs;

	// next state logic

	always_comb begin
		ns = cs;
		case(cs)
			R0_CHOOSE: ns = next_rotor_button ? R1_CHOOSE : R0_CHOOSE;
			R1_CHOOSE: ns = next_rotor_button ? R2_CHOOSE : R1_CHOOSE;
			R2_CHOOSE: ns = next_rotor_button ? R0_SET    : R2_CHOOSE;
			R0_SET:    ns = next_rotor_button ? R1_SET    : R0_SET;
			R1_SET:    ns = next_rotor_button ? R2_SET    : R1_SET;
			R2_SET:    ns = next_rotor_button ? PLUG      : R2_SET;
			PLUG:      ns = done_pb_button ? ENCRYPT   : PLUG;
			ENCRYPT:   ns = ENCRYPT;
			default:   ns = cs;
		endcase
	end

	// output logic

	assign rotor_up = (cs == R0_SET || cs == R1_SET || cs == R2_SET) & rotor_up_button;
	assign rotor_down = (cs == R0_SET || cs == R1_SET || cs == R2_SET) & rotor_down_button;

	always_comb begin
		r0_choose = 1'b0;
		r1_choose = 1'b0;
		r2_choose = 1'b0;
		r0_set = 1'b0;
		r1_set = 1'b0;
		r2_set = 1'b0;
		enable_rf = 1'b0;
		enable_swap_pb = 1'b0;
		clear_pb = 1'b0;

		case(cs)
			R0_CHOOSE: r0_choose = next_rotor_button;
			R1_CHOOSE: r1_choose = next_rotor_button;
			R2_CHOOSE: r2_choose = next_rotor_button;
			R0_SET:    r0_set = next_rotor_button;
			R1_SET:    r1_set = next_rotor_button;
			R2_SET: begin
					   r2_set = next_rotor_button;
					   clear_pb = next_rotor_button;
			end
			PLUG:      enable_swap_pb = set_plug_button;
			ENCRYPT:   enable_rf = encrypt_button;
			default: begin
			end
		endcase
	end

	ff_ar #(.W(3), .RV(R0_CHOOSE)) cs_ff(.q(cs), .d(ns), .clk, .rst);

endmodule: enigma_fsm

module enigma(
	input logic clk,
	input logic [3:0] btns,
	input logic [17:0] switches,
	output logic [17:0] ledr,
	output logic [8:0] ledg);

	letter_t in_letter;
	letter_t out_letter;
	logic clear_pb;
	letter_t pb_first;
	letter_t pb_second;
	logic enable_swap_pb;
	logic enable_rf;
	logic rst;

	logic [2:0] state;

	letter_t pb_out_letter;
	letter_t rf_out_letter;

	// I/O
	logic done_pb_button;
	logic next_rotor_button;
	logic next_plug_button;
	logic prev_plug_button; // TODO
	logic set_rotor_button;
	logic rotor_up_button, rotor_down_button;
	logic set_plug_button;
	logic encrypt_button;

	// to rotor_fsm
	logic r0_choose, r1_choose, r2_choose;
	logic r0_set, r1_set, r2_set;
	logic rotor_up, rotor_down;

	logic [3:0] btn_ned;
	negedge_detector btn_ned_det [3:0] (.in(btns), .ed(btn_ned), .clk, .rst);

	assign rst = switches[17];

	assign ledg = 8'b0;
	assign ledr[17:8] = 'b0;
	assign ledr[7:5] = state;

	assign in_letter  = letter_t' (switches[4:0]);
	assign pb_first   = letter_t' (switches[4:0]);
	assign pb_second  = letter_t' (switches[9:5]);
	assign ledr[4:0]  = out_letter;

	assign next_rotor_button = btn_ned[0];
	assign rotor_up_button   = btn_ned[1];
	assign rotor_down_button = btn_ned[2];
	assign set_rotor_button  = btn_ned[3];

	assign done_pb_button    = btn_ned[0];
	assign next_plug_button  = btn_ned[1];
	assign prev_plug_button  = btn_ned[2];
	assign set_plug_button   = btn_ned[3];

	assign encrypt_button    = btn_ned[0];

	enigma_fsm ef(.*);

	plugboard pb(
		.*,
		.first(pb_first),
		.second(pb_second),
		.enable_swap(enable_swap_pb), 
		.clear(clear_pb),
		.first_in_letter(in_letter),
		.first_out_letter(pb_out_letter),
		.second_in_letter(rf_out_letter),
		.second_out_letter(out_letter));

	rotor_fsm rf(.*,
		.enable(enable_rf),
		.in_letter(pb_out_letter),
		.out_letter(rf_out_letter));

endmodule: enigma

module tb_enigma;

	logic clk;
	logic [3:0] btns;
	logic [17:0] switches;
	logic [17:0] ledr;
	logic [8:0] ledg;

	logic rst;
	assign switches[17] = rst;

	enigma en(.*);

	initial begin
		clk <= 1'b0;
		rst <= 1'b1;
		#1;
		rst <= 1'b0;
		forever #(`CLK_PERIOD/2) clk = ~clk;
	end

	initial begin
		switches[16:0] <= 'b0;
		btns[3:0] <= 4'b1111;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk); // proceed to R1_CHOOSE
		btns[0] <= 1'b1;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk); // proceed to R2_CHOOSE
		btns[0] <= 1'b1;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk); // proceed to R0_SET
		btns[0] <= 1'b1;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk); // proceed to R1_SET
		btns[0] <= 1'b1;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk); // proceed to R2_SET
		btns[0] <= 1'b1;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk); // proceed to PLUG
		btns[0] <= 1'b1;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk); // proceed to ENCRYPT
		btns[0] <= 1'b1;

		@(posedge clk);
		switches[4:0] <= A;
		btns[0] <= 1'b0;
		@(posedge clk);
		btns[0] <= 1'b1;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk);
		btns[0] <= 1'b1;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk);
		btns[0] <= 1'b1;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk);
		btns[0] <= 1'b1;

		@(posedge clk);
		btns[0] <= 1'b0;
		@(posedge clk);
		btns[0] <= 1'b1;

		@(posedge clk);
		$finish;
	end

endmodule: tb_enigma

/*module tb_enigma;

	letter_t in_letter;
	letter_t out_letter;
	logic clear_pb;
	letter_t pb_first;
	letter_t pb_second;
	logic enable_swap_pb;
	logic enable_rf;
	logic clk, rst;

	enigma en(.*);

	initial begin
		clk <= 1'b0;
		rst <= 1'b1;
		#1;
		rst <= 1'b0;
		forever #(`CLK_PERIOD/2) clk = ~clk;
	end

	integer i;

	initial begin
		$monitor("%s (%d) -> %s (%d)", in_letter.name(), in_letter, out_letter.name(), out_letter);

		in_letter <= X;
		clear_pb <= 1'b0;
		pb_first <= A;
		pb_second <= A;
		enable_swap_pb <= 1'b0;
		enable_rf <= 1'b0;

		@(posedge clk);
		$display("clearing plugboard");
		clear_pb <= 1'b1;

		@(posedge clk);
		clear_pb <= 1'b0;
		pb_first <= A;
		pb_second <= B;
		enable_swap_pb <= 1'b1;

		@(posedge clk);
		pb_first <= C;
		pb_second <= D;
		enable_swap_pb <= 1'b1;

		@(posedge clk);
		enable_swap_pb <= 1'b0;

		$display("Inputting keys...");

		for(i=0; i<10; i++)
			press_key(A);
		press_key(B);
		press_key(C);
		press_key(D);
		press_key(E);
		press_key(F);

		@(posedge clk);
		$finish;
	end

	task press_key(input letter_t in);
		@(posedge clk);
		in_letter <= in;
		enable_rf <= 1'b1;
	endtask


endmodule: tb_enigma */

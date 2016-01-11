// A simple test design which we can simulate through the yosys->hardcaml flow

module counter (
  input clk, rst, clr, en,
  output reg [7:0] q
);

  always @(posedge clk, posedge rst) begin
    if (rst) q <= 0;
    else if (clr) q <= 0;
    else if (en) q <= q + 1;
  end

endmodule
  

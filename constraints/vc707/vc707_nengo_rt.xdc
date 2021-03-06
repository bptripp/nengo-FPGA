set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_DIP[7]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_DIP[6]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_DIP[5]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_DIP[4]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_DIP[3]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_DIP[2]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_DIP[1]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_DIP[0]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_LED[7]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_LED[6]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_LED[5]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_LED[4]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_LED[3]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_LED[2]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_LED[1]}]
set_property IOSTANDARD LVCMOS18 [get_ports {GPIO_LED[0]}]
set_property PACKAGE_PIN BB31 [get_ports {GPIO_DIP[7]}]
set_property PACKAGE_PIN BA30 [get_ports {GPIO_DIP[6]}]
set_property PACKAGE_PIN AY30 [get_ports {GPIO_DIP[5]}]
set_property PACKAGE_PIN AW30 [get_ports {GPIO_DIP[4]}]
set_property PACKAGE_PIN BA32 [get_ports {GPIO_DIP[3]}]
set_property PACKAGE_PIN BA31 [get_ports {GPIO_DIP[2]}]
set_property PACKAGE_PIN AY33 [get_ports {GPIO_DIP[1]}]
set_property PACKAGE_PIN AV30 [get_ports {GPIO_DIP[0]}]
set_property PACKAGE_PIN AM39 [get_ports {GPIO_LED[0]}]
set_property PACKAGE_PIN AN39 [get_ports {GPIO_LED[1]}]
set_property PACKAGE_PIN AR37 [get_ports {GPIO_LED[2]}]
set_property PACKAGE_PIN AT37 [get_ports {GPIO_LED[3]}]
set_property PACKAGE_PIN AR35 [get_ports {GPIO_LED[4]}]
set_property PACKAGE_PIN AP41 [get_ports {GPIO_LED[5]}]
set_property PACKAGE_PIN AP42 [get_ports {GPIO_LED[6]}]
set_property PACKAGE_PIN AU39 [get_ports {GPIO_LED[7]}]
set_property IOSTANDARD LVCMOS18 [get_ports RST]
set_property PACKAGE_PIN AV40 [get_ports RST]
set_property PACKAGE_PIN AH8 [get_ports SGMIICLK_P]
set_property PACKAGE_PIN AN2 [get_ports SGMII_TXP]
set_property PACKAGE_PIN AJ33 [get_ports PHY_RESET]
set_property IOSTANDARD LVCMOS18 [get_ports PHY_RESET]

create_clock -period 8.000 -name SGMII_P -waveform {0.000 4.000} [get_ports SGMIICLK_P]

set_property PACKAGE_PIN E19 [get_ports SYSCLK_P]
set_property IOSTANDARD LVDS [get_ports SYSCLK_P]
set_property IOSTANDARD LVDS [get_ports SYSCLK_N]

create_clock -period 5.000 -name SYSCLK -waveform {0.000 2.500} [get_ports SYSCLK_P]

create_pblock pblock_1
add_cells_to_pblock [get_pblocks pblock_1] [get_cells -quiet [list NENGO {x_state_reg[1]_i_9} {x_state_reg[1]_i_17} {x_state_reg[1]_i_18} {x_state_reg[1]_i_19} {x_state_reg[1]_i_20} {x_state_reg[-5]_i_5} {x_state_reg[-5]_i_6} {x_state_reg[-5]_i_7} {x_state_reg[-5]_i_8} {x_state_reg[-9]_i_5} {x_state_reg[-9]_i_6} {x_state_reg[-9]_i_7} {x_state_reg[-9]_i_8} {x_state_reg[1]_i_4} {x_state_reg[1]_i_7} {x_state_reg[-5]_i_4} {x_state_reg[-9]_i_4} {x_state_reg[-13]_i_4} {y_out_reg[-7]_i_4} {y_out_reg[-7]_i_5} {y_out_reg[-7]_i_6} {y_out_reg[-7]_i_7} {y_out_reg[1]_i_10} {y_out_reg[1]_i_11} {y_out_reg[1]_i_12} {y_out_reg[1]_i_13} {y_out_reg[1]_i_21} {y_out_reg[1]_i_22} {y_out_reg[1]_i_23} {y_out_reg[1]_i_24} {y_out_reg[1]_i_25} {y_out_reg[1]_i_5} {y_out_reg[1]_i_9} {y_out_reg[-7]_i_3} {x_state_reg[-13]_i_5} {x_state_reg[-13]_i_6} {x_state_reg[-13]_i_7} {x_state_reg[-13]_i_8} {x_state_reg[1]_i_8} arg_i_46 arg_i_45 arg_i_42 arg_i_41 arg_i_40 arg_i_57 arg_i_14 arg_i_33 {DECODERS[3].DECODER/DECODER/decode_intermediate_reg[0]} {DECODERS[3].DECODER/DECODER/decode_intermediate_reg[2]} {DECODERS[3].DECODER/DECODER/decode_intermediate_reg[4]} {DECODERS[3].DECODER/DECODER/decode_intermediate_reg[6]} {DECODERS[2].DECODER/DECODER/decode_intermediate_reg[0]} {DECODERS[2].DECODER/DECODER/decode_intermediate_reg[2]} {DECODERS[2].DECODER/DECODER/decode_intermediate_reg[4]} {DECODERS[2].DECODER/DECODER/decode_intermediate_reg[6]} {DECODERS[1].DECODER/DECODER/decode_intermediate_reg[0]} {DECODERS[1].DECODER/DECODER/decode_intermediate_reg[4]} {DECODERS[1].DECODER/DECODER/decode_intermediate_reg[6]} {DECODERS[1].DECODER/DECODER/decode_intermediate_reg[2]} {DECODERS[0].DECODER/DECODER/decode_intermediate_reg[0]} {DECODERS[0].DECODER/DECODER/decode_intermediate_reg[2]} {DECODERS[0].DECODER/DECODER/decode_intermediate_reg[4]} {DECODERS[0].DECODER/DECODER/decode_intermediate_reg[6]} FILTER/arg__2 FILTER/arg__1 FILTER/arg__0 FILTER/arg ENCODER/arg arg_i_47 arg_i_39 {FSM_onehot_reg_reg_state[6]_i_9} arg_i_48 arg_i_1 RX_FIFO RX_CHANNEL RX_HANDLER]]
resize_pblock [get_pblocks pblock_1] -add {CLOCKREGION_X1Y0:CLOCKREGION_X1Y0}


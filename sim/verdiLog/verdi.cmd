debImport "+v2k" "-sverilog" "-f" "flist.f"
debLoadSimResult /home/ICer/siicpu/OoOcpu/sim/wave_ooocpu.fsdb
wvCreateWindow
debExit
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 wvSetCursor -win $_nWave2 209336.197815 -snap {("G1" 8)}
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvZoom -win $_nWave2 0.000000 34889.366303
wvSelectSignal -win $_nWave2 {( "G1" 4 )} 
wvSetCursor -win $_nWave2 4982.089485 -snap {("G1" 5)}
wvSetCursor -win $_nWave2 12897.421003 -snap {("G1" 7)}
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvSetCursor -win $_nWave2 352655.986481 -snap {("G1" 10)}
wvSetCursor -win $_nWave2 359503.675539 -snap {("G1" 9)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_pc.pc_stall" -win $_nTrace1 -TraceByDConWave \
           -TraceTime 355000 -TraceValue 1
wvSetCursor -win $_nWave2 325693.210815 -snap {("G1" 13)}
wvSetCursor -win $_nWave2 64197.084918 -snap {("G1" 16)}
wvZoom -win $_nWave2 0.000000 134385.897761
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_fu_top" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
wvSelectSignal -win $_nWave2 {( "G1" 7 )} 
wvSelectSignal -win $_nWave2 {( "G1" 7 8 9 10 11 12 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G1" 10)}
wvSelectSignal -win $_nWave2 {( "G1" 7 )} 
wvSelectGroup -win $_nWave2 {G2}
wvSelectGroup -win $_nWave2 {G2}
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_en" -line 8 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_op" -line 9 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_pc" -line 10 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_imm" -line 11 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_rs1_value" -line 12 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_rs2_value" -line 13 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_Pdst" -line 14 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectGroup -win $_nWave2 {G2}
srcDeselectAll -win $_nTrace1
srcSelect -signal "mul_div_issue_en" -line 16 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_op" -line 17 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_rs1_value" -line 18 -pos 1 -win $_nTrace1
srcSelect -toggle -signal "mul_div_issue_queue_rs1_value" -line 18 -pos 1 -win \
          $_nTrace1
srcSelect -signal "mul_div_issue_queue_rs1_value" -line 18 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_rs2_value" -line 19 -pos 1 -win $_nTrace1
srcSelect -toggle -signal "mul_div_issue_queue_rs2_value" -line 19 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_rs2_value" -line 19 -pos 1 -win $_nTrace1
wvSetPosition -win $_nWave2 {("G1" 18)}
wvSetPosition -win $_nWave2 {("G1" 19)}
wvSetPosition -win $_nWave2 {("G1" 20)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G1" 20)}
srcSelect -signal "mul_div_issue_queue_Pdst" -line 20 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_rs1_value" -line 18 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_rs2_value" -line 19 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_Pdst" -line 20 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectGroup -win $_nWave2 {G2}
wvSelectSignal -win $_nWave2 {( "G1" 18 )} 
wvSelectSignal -win $_nWave2 {( "G1" 18 19 20 21 22 )} 
wvSetPosition -win $_nWave2 {("G1" 20)}
wvSetPosition -win $_nWave2 {("G1" 21)}
wvSetPosition -win $_nWave2 {("G1" 22)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSelectGroup -win $_nWave2 {G2}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
wvZoom -win $_nWave2 0.000000 150248.829899
wvSelectSignal -win $_nWave2 {( "G2" 2 )} 
wvSelectSignal -win $_nWave2 {( "G2" 5 )} 
wvSelectSignal -win $_nWave2 {( "G2" 4 )} 
wvSelectSignal -win $_nWave2 {( "G2" 3 )} 
wvSelectSignal -win $_nWave2 {( "G2" 3 4 )} 
wvSelectSignal -win $_nWave2 {( "G1" 16 )} {( "G2" 3 4 )} 
wvSelectSignal -win $_nWave2 {( "G1" 15 16 )} {( "G2" 3 4 )} 
wvSelectSignal -win $_nWave2 {( "G1" 14 15 16 )} {( "G2" 3 4 )} 
wvSelectSignal -win $_nWave2 {( "G1" 14 15 16 )} {( "G2" 3 4 )} 
wvSetRadix -win $_nWave2 -format UDec
wvSelectGroup -win $_nWave2 {G3}
wvSetCursor -win $_nWave2 54694.897791 -snap {("G2" 4)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G2" 2 )} 
wvSelectSignal -win $_nWave2 {( "G2" 3 )} 
wvSelectSignal -win $_nWave2 {( "G2" 2 )} 
wvSelectSignal -win $_nWave2 {( "G2" 3 )} 
wvSelectSignal -win $_nWave2 {( "G2" 4 )} 
wvSelectGroup -win $_nWave2 {G3}
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoom -win $_nWave2 0.000000 173939.923261
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_out" -line 44 -pos 1 -win $_nTrace1
srcSelect -signal "alu_out_valid" -line 45 -pos 1 -win $_nTrace1
srcSelect -signal "alu_dst_Paddr" -line 46 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "div_out" -line 53 -pos 1 -win $_nTrace1
srcSelect -signal "div_out_valid" -line 54 -pos 1 -win $_nTrace1
srcSelect -toggle -signal "div_out_valid" -line 54 -pos 1 -win $_nTrace1
srcSelect -signal "div_dst_Paddr" -line 55 -pos 1 -win $_nTrace1
srcSelect -signal "div_out_valid" -line 54 -pos 1 -win $_nTrace1
srcSelect -signal "div_ready" -line 56 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G2" 9 10 11 12 )} 
wvSelectSignal -win $_nWave2 {( "G2" 7 )} 
wvSelectSignal -win $_nWave2 {( "G2" 6 )} 
wvSelectSignal -win $_nWave2 {( "G2" 6 7 8 9 10 11 12 )} 
wvSetPosition -win $_nWave2 {("G2" 10)}
wvSetPosition -win $_nWave2 {("G2" 11)}
wvSetPosition -win $_nWave2 {("G2" 12)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 7)}
wvSetPosition -win $_nWave2 {("G3" 7)}
wvSelectGroup -win $_nWave2 {G2}
wvSelectSignal -win $_nWave2 {( "G3" 4 )} 
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvSetRadix -win $_nWave2 -format UDec
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvSelectSignal -win $_nWave2 {( "G2" 5 )} 
wvSelectGroup -win $_nWave2 {G3}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_gpr" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "gpr" -line 21 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G3" 8 )} 
wvExpandBus -win $_nWave2 {("G3" 8)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 31868.129825 -snap {("G3" 23)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 34871.409076 -snap {("G3" 3)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 44548.642217 -snap {("G3" 11)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 54809.846323 -snap {("G3" 24)}
wvScrollUp -win $_nWave2 6
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G3" 8 )} 
wvSelectSignal -win $_nWave2 {( "G3" 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 \
           23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 )} 
wvSelectSignal -win $_nWave2 {( "G3" 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 \
           23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 )} 
wvSetRadix -win $_nWave2 -format UDec
wvSelectGroup -win $_nWave2 {G4}
wvSetCursor -win $_nWave2 45299.462029 -snap {("G3" 11)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvSetCursor -win $_nWave2 54118.705036 -snap {("G3" 24)}
wvSetCursor -win $_nWave2 65904.556355 -snap {("G3" 23)}
wvZoomIn -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomIn -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvZoomOut -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 84966.366906 -snap {("G3" 1)}
wvSetCursor -win $_nWave2 94948.261391 -snap {("G3" 1)}
wvSetCursor -win $_nWave2 85146.762590 -snap {("G3" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 65002.577938 -snap {("G2" 1)}
wvSetCursor -win $_nWave2 61154.136691 -snap {("G2" 4)}
wvSetCursor -win $_nWave2 61695.323741 -snap {("G2" 4)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_queue_rs2_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue \
           00000000000000000000000000000000
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G3" 8 )} 
wvSelectSignal -win $_nWave2 {( "G3" 9 )} 
wvScrollDown -win $_nWave2 28
wvSelectSignal -win $_nWave2 {( "G3" 9 10 11 12 13 14 15 16 17 18 19 20 21 22 \
           23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 8)}
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 25 -pos 1 -win $_nTrace1
srcSelect -win $_nTrace1 -range {25 26 17 17 8 8}
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 25 -pos 1 -win $_nTrace1
srcSelect -signal "wb_alu_out" -line 26 -pos 1 -win $_nTrace1
srcSelect -signal "wb_alu_valid" -line 27 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G3" 10 )} 
wvSelectSignal -win $_nWave2 {( "G3" 9 )} 
wvSelectSignal -win $_nWave2 {( "G3" 9 10 11 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 8)}
wvSetCursor -win $_nWave2 61935.851319 -snap {("G2" 4)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_queue_rs2_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue \
           00000000000000000000000000000000
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs2_value   \[issue_tag\]" -line 133 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs2_value   \[issue_tag\]" -line 133 -pos 1 -win \
          $_nTrace1
srcAction -pos 132 6 18 -win $_nTrace1 -name \
          "issue_queue_rs2_value   \[issue_tag\]" -ctrlKey off
wvSelectSignal -win $_nWave2 {( "G3" 4 )} 
wvSetCursor -win $_nWave2 54960.551559 -snap {("G3" 6)}
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
srcDeselectAll -win $_nTrace1
debReload
wvSetCursor -win $_nWave2 62537.170264 -snap {("G2" 4)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_queue_rs2_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue \
           00000000000000000000000000000000
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs2_value   \[issue_tag\]" -line 133 -pos 1 -win \
          $_nTrace1
srcAction -pos 132 6 16 -win $_nTrace1 -name \
          "issue_queue_rs2_value   \[issue_tag\]" -ctrlKey off
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs2_value\[j\]" -line 315 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs2_value_fromGPR" -line 315 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -win $_nTrace1 -range {315 315 2 2 1 22}
srcSearchString "issue_queue_rs2_value" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {312 312 2 2 1 22}
srcSearchString "issue_queue_rs2_value" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {309 309 2 2 1 22}
srcSearchString "issue_queue_rs2_value" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {306 306 2 2 1 22}
srcSearchString "issue_queue_rs2_value" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {133 133 7 7 1 22}
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs2_value   \[issue_tag\]" -line 133 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G3" 11 )} 
wvExpandBus -win $_nWave2 {("G3" 11)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvSelectGroup -win $_nWave2 {G4}
wvSelectSignal -win $_nWave2 {( "G3" 12 )} 
wvSetCursor -win $_nWave2 55140.947242 -snap {("G3" 12)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_pc          \[issue_tag\]" -line 130 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_op          \[issue_tag\]" -line 129 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G3" 11 )} 
wvSelectSignal -win $_nWave2 {( "G3" 12 )} 
wvSelectSignal -win $_nWave2 {( "G3" 12 13 14 15 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 13)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value   \[issue_tag\]" -line 132 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Pdst        \[issue_tag\]" -line 134 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_pc" -line 12 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_op" -line 11 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue2queue_en" -line 10 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_op" -line 11 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_pc" -line 12 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_imm" -line 13 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_alloc_rob" -line 14 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 49729.076739 -snap {("G4" 0)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 16 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 17 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs2_rat_valid" -line 18 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs2_Paddr" -line 19 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_value_fromGPR" -line 20 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs2_value_fromGPR" -line 21 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 44798.261391 -snap {("G3" 25)}
wvSetCursor -win $_nWave2 54359.232614 -snap {("G3" 26)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs2_value_fromGPR" -line 21 -pos 1 -win $_nTrace1
srcAction -pos 20 19 11 -win $_nTrace1 -name "rs2_value_fromGPR" -ctrlKey off
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_value" -line 15 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_addr" -line 11 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs2_addr" -line 12 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G3" 27 )} 
wvSelectSignal -win $_nWave2 {( "G3" 27 28 )} 
wvSetPosition -win $_nWave2 {("G3" 27)}
wvSetPosition -win $_nWave2 {("G3" 28)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 28)}
wvSetPosition -win $_nWave2 {("G3" 27)}
wvSetPosition -win $_nWave2 {("G3" 28)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G4" 2)}
wvSetPosition -win $_nWave2 {("G4" 2)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_value" -line 15 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs2_value" -line 16 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs2_value" -line 16 -pos 1 -win $_nTrace1
srcAction -pos 15 21 3 -win $_nTrace1 -name "rs2_value" -ctrlKey off
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_commit_dst_addr" -line 9 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "commit_en" -line 8 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_commit_dst_addr" -line 9 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_commit_dst_value" -line 10 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G4" 5 )} 
wvSelectSignal -win $_nWave2 {( "G4" 5 7 )} 
wvSelectSignal -win $_nWave2 {( "G4" 5 6 7 )} 
wvSetPosition -win $_nWave2 {("G4" 6)}
wvSetPosition -win $_nWave2 {("G4" 7)}
wvSetPosition -win $_nWave2 {("G5" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G5" 3)}
wvSetPosition -win $_nWave2 {("G5" 3)}
wvSelectSignal -win $_nWave2 {( "G5" 1 2 3 )} 
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_commit_dst_value" -line 10 -pos 1 -win $_nTrace1
srcAction -pos 9 19 10 -win $_nTrace1 -name "rob_commit_dst_value" -ctrlKey off
srcHBSelect "tb_ooocpu.u_ooocpu.u_rat" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_rat" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_rat" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "commit_en" -line 23 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_commit_dst_addr_2rat" -line 24 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_valid\[i\]" -line 49 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_valid" -line 42 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_Paddr" -line 41 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_valid" -line 42 -pos 1 -win $_nTrace1
srcAction -pos 41 2 4 -win $_nTrace1 -name "rat_valid" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "allocate_en" -line 53 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_alloc_dst_addr_2rat" -line 53 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_alloc_dst_wen_2rat" -line 53 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_alloc_dst_wen_2rat" -line 53 -pos 1 -win $_nTrace1
srcAction -pos 52 20 9 -win $_nTrace1 -name "rob_alloc_dst_wen_2rat" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "dst_wen" -line 127 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "dst_wen" -line 127 -pos 1 -win $_nTrace1
srcAction -pos 126 6 2 -win $_nTrace1 -name "dst_wen" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "if_en" -line 58 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -word -line 57 -pos 12 -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -word -line 57 -pos 12 -win $_nTrace1
srcAction -pos 57 12 6 -win $_nTrace1 -name "GPR_WRITE" -ctrlKey off
srcDeselectAll -win $_nTrace1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvZoom -win $_nWave2 0.000000 126810.949640
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 60273.213954 -snap {("G2" 2)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_queue_op\[4:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue 11010
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_op" -line 75 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_pc" -line 76 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_imm" -line 77 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value" -line 78 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs2_value" -line 79 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 20
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G3" 8 )} 
wvSetPosition -win $_nWave2 {("G3" 8)}
wvExpandBus -win $_nWave2 {("G3" 8)}
wvSetPosition -win $_nWave2 {("G5" 16)}
wvScrollUp -win $_nWave2 12
wvSelectSignal -win $_nWave2 {( "G3" 9 )} 
wvScrollDown -win $_nWave2 18
wvSelectSignal -win $_nWave2 {( "G3" 9 10 11 12 13 14 15 16 17 18 19 20 21 22 \
           23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G5" 16)}
wvScrollUp -win $_nWave2 45
wvSelectSignal -win $_nWave2 {( "G1" 6 )} 
wvScrollDown -win $_nWave2 48
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G1" 6 7 8 9 10 11 12 13 14 15 16 17 )} {( "G2" \
           1 2 3 4 5 )} {( "G3" 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 \
           19 20 21 22 23 24 25 26 )} {( "G4" 1 2 3 4 )} {( "G5" 1 2 3 4 5 6 \
           7 8 9 10 11 12 13 14 15 16 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G5" 0)}
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_gpr" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "commit_en" -line 8 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_commit_dst_addr" -line 9 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_commit_dst_value" -line 10 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_addr" -line 11 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "commit_en" -line 8 -pos 1 -win $_nTrace1
srcAction -pos 7 5 5 -win $_nTrace1 -name "commit_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_dst_value_ready \[commit_rob\]" -line 130 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvZoom -win $_nWave2 0.000000 125533.717054
srcDeselectAll -win $_nTrace1
srcSelect -signal "commit_rob" -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G5" 4)}
wvSetPosition -win $_nWave2 {("G5" 3)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G5" 3)}
wvSetPosition -win $_nWave2 {("G5" 4)}
wvSelectSignal -win $_nWave2 {( "G5" 5 )} 
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoom -win $_nWave2 0.000000 207950.522782
wvSelectSignal -win $_nWave2 {( "G5" 5 )} 
wvSetPosition -win $_nWave2 {("G5" 5)}
wvExpandBus -win $_nWave2 {("G5" 5)}
wvScrollUp -win $_nWave2 12
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G5" 5)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_dst_value_ready \[commit_rob\]" -line 130 -pos 1 -win \
          $_nTrace1
srcAction -pos 129 6 13 -win $_nTrace1 -name "rob_dst_value_ready \[commit_rob\]" \
          -ctrlKey off
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 45679.299489 -snap {("G5" 5)}
wvSetCursor -win $_nWave2 14760.996341 -snap {("G1" 5)}
wvSetCursor -win $_nWave2 25133.588365 -snap {("G1" 2)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_valid" -line 182 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 182 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_valid" -line 182 -pos 1 -win $_nTrace1
wvSetCursor -win $_nWave2 31915.667765 -snap {("G5" 7)}
wvSetCursor -win $_nWave2 30818.566686 -snap {("G5" 7)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_rob.wb_alu_valid" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 0 -TraceValue 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_en" -line 16 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_op" -line 6 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_pc" -line 7 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_imm" -line 8 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_rs1_value" -line 9 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_rs2_value" -line 10 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_imm" -line 8 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_op" -line 6 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_en" -line 5 -pos 1 -win $_nTrace1
srcAction -pos 4 5 8 -win $_nTrace1 -name "alu_issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 114 -pos 1 -win $_nTrace1
srcAction -pos 113 6 6 -win $_nTrace1 -name "fifo_issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_empty" -line 113 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 113 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fu_ready" -line 113 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 113 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 113 -pos 1 -win $_nTrace1
srcAction -pos 112 11 2 -win $_nTrace1 -name "issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 102 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[0\]" -line 102 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[1\]" -line 103 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 102 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 102 -pos 1 -win $_nTrace1
srcAction -pos 101 7 14 -win $_nTrace1 -name "issue_queue_ready\[0\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 366 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs2_valid\[j\]" -line 366 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 366 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 366 -pos 1 -win $_nTrace1
srcAction -pos 365 8 17 -win $_nTrace1 -name "issue_queue_Prs1_valid\[j\]" \
          -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 261 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 261 -pos 1 -win $_nTrace1
srcAction -pos 260 5 6 -win $_nTrace1 -name "rs1_rat_valid" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_valid\[id_rs1_addr\]" -line 65 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_rs1_addr" -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_rs1_addr" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_rs1_addr" -win $_nTrace1
srcAction -pos 64 19 13 -win $_nTrace1 -name "rat_valid\[id_rs1_addr\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_addr" -line 56 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 14661.259880 -snap {("G1" 5)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_addr" -line 56 -pos 1 -win $_nTrace1
srcAction -pos 55 5 3 -win $_nTrace1 -name "rs1_addr" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "opcode" -line 56 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G5" 27 )} 
wvSetRadix -win $_nWave2 -format Bin
srcDeselectAll -win $_nTrace1
srcSelect -signal "if_insn" -line 53 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G5" 28 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G6" 0)}
wvSetPosition -win $_nWave2 {("G5" 27)}
wvSelectSignal -win $_nWave2 {( "G5" 27 )} 
srcDeselectAll -win $_nTrace1
srcSelect -signal "if_insn" -line 53 -pos 1 -win $_nTrace1
srcAction -pos 52 6 4 -win $_nTrace1 -name "if_insn" -ctrlKey off
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 1296.574003 -snap {("G1" 5)}
srcActiveTrace "tb_ooocpu.u_ooocpu.pc\[31:0\]" -win $_nTrace1 -TraceByDConWave \
           -TraceTime 0 -TraceValue 00000000000000000000000000000000
wvSetCursor -win $_nWave2 2194.202159 -snap {("G1" 2)}
srcActiveTrace "tb_ooocpu.u_ooocpu.rst_n" -win $_nTrace1 -TraceByDConWave \
           -TraceTime 0 -TraceValue 0
wvSetCursor -win $_nWave2 1695.519850 -snap {("G1" 3)}
wvSetCursor -win $_nWave2 3291.303238 -snap {("G1" 4)}
srcActiveTrace "tb_ooocpu.u_ooocpu.insn\[31:0\]" -win $_nTrace1 -TraceByDConWave \
           -TraceTime 0 -TraceValue 00000000000000000000000000000000
wvSetCursor -win $_nWave2 2892.357391 -snap {("G1" 5)}
srcActiveTrace "tb_ooocpu.u_ooocpu.pc\[31:0\]" -win $_nTrace1 -TraceByDConWave \
           -TraceTime 0 -TraceValue 00000000000000000000000000000000
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 15159.942188 -snap {("G1" 5)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G5" 28 )} 
wvScrollUp -win $_nWave2 4
wvSelectSignal -win $_nWave2 {( "G5" 1 )} 
wvScrollDown -win $_nWave2 4
wvSelectSignal -win $_nWave2 {( "G5" 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 \
           18 19 20 21 22 23 24 25 26 27 28 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G5" 0)}
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_gpr" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "gpr" -line 21 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G5" 1 )} 
wvExpandBus -win $_nWave2 {("G5" 1)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 45679.299489 -snap {("G5" 4)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G5" 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 \
           19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 )} 
wvSelectGroup -win $_nWave2 {G5}
wvSelectSignal -win $_nWave2 {( "G5" 2 )} 
wvScrollDown -win $_nWave2 7
wvSelectSignal -win $_nWave2 {( "G5" 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 \
           19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G5" 1)}
wvSetCursor -win $_nWave2 103975.261391 -snap {("G6" 0)}
srcDeselectAll -win $_nTrace1
wvZoom -win $_nWave2 0.000000 138957.592326
wvSetCursor -win $_nWave2 14728.838323 -snap {("G1" 4)}
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top.u_alu" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_fu_top" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_en" -line 8 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_op" -line 9 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_pc" -line 10 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_imm" -line 11 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_rs1_value" -line 12 -pos 1 -win $_nTrace1
srcSelect -toggle -signal "alu_issue_queue_rs1_value" -line 12 -pos 1 -win \
          $_nTrace1
srcSelect -signal "alu_issue_queue_rs1_value" -line 12 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_rs2_value" -line 13 -pos 1 -win $_nTrace1
srcSelect -signal "alu_issue_queue_Pdst" -line 14 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G5" 1 )} 
wvSelectSignal -win $_nWave2 {( "G5" 2 )} 
wvSelectSignal -win $_nWave2 {( "G5" 2 3 4 5 6 7 8 )} 
wvSetPosition -win $_nWave2 {("G5" 6)}
wvSetPosition -win $_nWave2 {("G5" 5)}
wvSetPosition -win $_nWave2 {("G5" 4)}
wvSetPosition -win $_nWave2 {("G5" 3)}
wvSetPosition -win $_nWave2 {("G5" 2)}
wvSetPosition -win $_nWave2 {("G5" 1)}
wvSetPosition -win $_nWave2 {("G5" 0)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSelectGroup -win $_nWave2 {G4}
wvSetCursor -win $_nWave2 34989.321809 -snap {("G2" 2)}
wvSetCursor -win $_nWave2 33523.102609 -snap {("G2" 4)}
wvSetCursor -win $_nWave2 40454.320644 -snap {("G2" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_en" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 35000 -TraceValue 0
wvSelectSignal -win $_nWave2 {( "G1" 5 )} 
srcHBSelect "tb_ooocpu.u_ooocpu.u_decoder" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_decoder" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_decoder" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_addr" -line 12 -pos 1 -win $_nTrace1
srcSelect -win $_nTrace1 -range {12 13 19 19 6 8}
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_addr" -line 12 -pos 1 -win $_nTrace1
srcSelect -signal "rs2_addr" -line 13 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 8)}
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSetPosition -win $_nWave2 {("G2" 6)}
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSetPosition -win $_nWave2 {("G2" 3)}
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G2" 1)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G1" 5)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G1" 5)}
wvSetPosition -win $_nWave2 {("G1" 7)}
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_op" -line 17 -pos 1 -win $_nTrace1
srcSelect -win $_nTrace1 -range {17 18 19 19 3 5}
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_op" -line 17 -pos 1 -win $_nTrace1
srcSelect -signal "mem_op" -line 18 -pos 1 -win $_nTrace1
srcSelect -signal "imm" -line 20 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectGroup -win $_nWave2 {G2}
wvSetCursor -win $_nWave2 25325.604357 -snap {("G1" 2)}
wvSetCursor -win $_nWave2 25192.311702 -snap {("G1" 4)}
wvSelectSignal -win $_nWave2 {( "G1" 8 )} 
wvSetPosition -win $_nWave2 {("G1" 8)}
wvSetPosition -win $_nWave2 {("G1" 7)}
wvSetPosition -win $_nWave2 {("G1" 6)}
wvSetPosition -win $_nWave2 {("G1" 5)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G1" 5)}
wvSetPosition -win $_nWave2 {("G1" 6)}
wvSetCursor -win $_nWave2 14928.777305 -snap {("G1" 7)}
wvSetCursor -win $_nWave2 24992.372721 -snap {("G1" 7)}
srcDeselectAll -win $_nTrace1
wvSetCursor -win $_nWave2 39254.686753 -snap {("G2" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_en" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 35000 -TraceValue 0
wvSetCursor -win $_nWave2 41320.722898 -snap {("G2" 5)}
wvSetCursor -win $_nWave2 39454.625735 -snap {("G2" 5)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_rs1_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 35000 -TraceValue \
           xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value   \[issue_tag\]" -line 119 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G1" 8)}
wvSetPosition -win $_nWave2 {("G1" 9)}
wvSetPosition -win $_nWave2 {("G1" 10)}
wvSetPosition -win $_nWave2 {("G1" 11)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G2" 1)}
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G2" 3)}
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSetPosition -win $_nWave2 {("G2" 6)}
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 1)}
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvExpandBus -win $_nWave2 {("G3" 1)}
wvScrollDown -win $_nWave2 2
wvScrollUp -win $_nWave2 1
wvSelectGroup -win $_nWave2 {G4}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_op          \[issue_tag\]" -line 116 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 40054.442680 -snap {("G3" 2)}
wvSetCursor -win $_nWave2 38988.101444 -snap {("G3" 2)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_rs1_value\[0\]\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 35000 -TraceValue \
           xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_write_en" -line 313 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_p\[j\]" -line 313 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G3" 8 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G3" 7)}
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_p\[j\]" -line 313 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_p\[j\]" -line 313 -pos 1 -win $_nTrace1
srcAction -pos 312 8 1 -win $_nTrace1 -name "fifo_p\[j\]" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_p" -line 197 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G3" 8 )} 
wvExpandBus -win $_nWave2 {("G3" 8)}
wvScrollDown -win $_nWave2 2
srcDeselectAll -win $_nTrace1
wvSelectSignal -win $_nWave2 {( "G3" 9 )} 
wvSelectSignal -win $_nWave2 {( "G3" 9 10 11 12 13 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 8)}
wvSelectGroup -win $_nWave2 {G6}
wvSelectGroup -win $_nWave2 {G4}
wvSelectSignal -win $_nWave2 {( "G3" 8 )} 
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 39321.333080 -snap {("G3" 2)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_rs1_value\[0\]\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 35000 -TraceValue \
           xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 313 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_en\[j+1\]" -line 317 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_en\[j+1\]" -line 317 -pos 1 -win $_nTrace1
srcAction -pos 316 8 14 -win $_nTrace1 -name "writeback_rs1_en\[j+1\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[i\]" -line 155 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
wvSetCursor -win $_nWave2 25325.604357 -snap {("G3" 2)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[i\]" -line 155 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G3" 11 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G3" 10)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_in_writers1" -line 155 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_p\[i\]" -line 155 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_in_writers1" -line 155 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_in_writers1" -line 155 -pos 1 -win $_nTrace1
srcAction -pos 154 30 12 -win $_nTrace1 -name "wb_alu_in_writers1" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 140 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 140 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 140 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_write_en" -line 140 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 31323.773810 -snap {("G3" 14)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 140 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 140 -pos 1 -win $_nTrace1
srcAction -pos 139 11 2 -win $_nTrace1 -name "rs1_Paddr" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_rs1_addr" -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_Paddr\[id_rs1_addr\]" -line 66 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G3" 18 )} 
wvExpandBus -win $_nWave2 {("G3" 18)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 18)}
srcDeselectAll -win $_nTrace1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 29790.908283 -snap {("G3" 9)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 31723.651773 -snap {("G3" 9)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_en\[1\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 25000 -TraceValue x
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_in_writers1" -line 155 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_in_writers1" -line 155 -pos 1 -win $_nTrace1
srcAction -pos 154 30 8 -win $_nTrace1 -name "wb_alu_in_writers1" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 140 -pos 1 -win $_nTrace1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G3" 14 )} 
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 140 -pos 1 -win $_nTrace1
wvSetCursor -win $_nWave2 19527.373886 -snap {("G3" 15)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.rs1_rat_valid" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 0 -TraceValue 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_rs1_addr" -line 65 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 14728.838323 -snap {("G1" 7)}
wvSetCursor -win $_nWave2 25059.019048 -snap {("G1" 7)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 37988.406535 -snap {("G3" 2)}
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 38921.455117 -snap {("G3" 2)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_rs1_value\[0\]\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 35000 -TraceValue \
           xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue2queue_en" -line 10 -pos 1 -win $_nTrace1
wvScrollDown -win $_nWave2 6
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvScrollDown -win $_nWave2 10
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G3" 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 \
           18 19 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 0)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_op" -line 11 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue2queue_en" -line 10 -pos 1 -win $_nTrace1
srcSelect -win $_nTrace1 -range {10 11 6 18 6 4}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue2queue_en" -line 10 -pos 1 -win $_nTrace1
srcSelect -signal "id_op" -line 11 -pos 1 -win $_nTrace1
srcSelect -signal "id_pc" -line 12 -pos 1 -win $_nTrace1
srcSelect -signal "id_imm" -line 13 -pos 1 -win $_nTrace1
srcSelect -signal "id_alloc_rob" -line 14 -pos 1 -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 16 -pos 1 -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 17 -pos 1 -win $_nTrace1
srcSelect -signal "rs2_rat_valid" -line 18 -pos 1 -win $_nTrace1
srcSelect -signal "rs2_Paddr" -line 19 -pos 1 -win $_nTrace1
srcSelect -signal "rs1_value_fromGPR" -line 20 -pos 1 -win $_nTrace1
srcSelect -signal "rs2_value_fromGPR" -line 21 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 4)}
wvSetPosition -win $_nWave2 {("G3" 5)}
wvSetPosition -win $_nWave2 {("G3" 6)}
wvSetPosition -win $_nWave2 {("G3" 7)}
wvSetPosition -win $_nWave2 {("G3" 8)}
wvSetPosition -win $_nWave2 {("G3" 9)}
wvSetPosition -win $_nWave2 {("G3" 10)}
wvSetPosition -win $_nWave2 {("G3" 11)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G5" 0)}
wvSetPosition -win $_nWave2 {("G5" 1)}
wvSetPosition -win $_nWave2 {("G6" 0)}
wvSetPosition -win $_nWave2 {("G5" 1)}
wvSetPosition -win $_nWave2 {("G5" 0)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G3" 11)}
wvSetPosition -win $_nWave2 {("G3" 10)}
wvSetPosition -win $_nWave2 {("G3" 9)}
wvSetPosition -win $_nWave2 {("G3" 8)}
wvSetPosition -win $_nWave2 {("G3" 7)}
wvSetPosition -win $_nWave2 {("G3" 6)}
wvSetPosition -win $_nWave2 {("G3" 5)}
wvSetPosition -win $_nWave2 {("G3" 4)}
wvSetPosition -win $_nWave2 {("G3" 3)}
wvSetPosition -win $_nWave2 {("G3" 2)}
wvSetPosition -win $_nWave2 {("G3" 1)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G3" 11)}
wvSelectSignal -win $_nWave2 {( "G3" 5 )} 
wvSelectGroup -win $_nWave2 {G4}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G2" 1 )} 
wvSelectSignal -win $_nWave2 {( "G2" 1 2 3 4 5 6 7 )} 
wvSetPosition -win $_nWave2 {("G2" 3)}
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSetPosition -win $_nWave2 {("G2" 6)}
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G3" 1)}
wvSetPosition -win $_nWave2 {("G3" 2)}
wvSetPosition -win $_nWave2 {("G3" 3)}
wvSetPosition -win $_nWave2 {("G3" 5)}
wvSetPosition -win $_nWave2 {("G3" 6)}
wvSetPosition -win $_nWave2 {("G3" 7)}
wvSetPosition -win $_nWave2 {("G3" 8)}
wvSetPosition -win $_nWave2 {("G3" 9)}
wvSetPosition -win $_nWave2 {("G3" 10)}
wvSetPosition -win $_nWave2 {("G3" 11)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G5" 0)}
wvSetPosition -win $_nWave2 {("G5" 1)}
wvSetPosition -win $_nWave2 {("G6" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G6" 7)}
wvSetPosition -win $_nWave2 {("G6" 7)}
wvSelectGroup -win $_nWave2 {G6}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 15195.362614 -snap {("G1" 4)}
wvSetCursor -win $_nWave2 34789.382827 -snap {("G3" 6)}
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 31123.834828 -snap {("G3" 7)}
wvSetCursor -win $_nWave2 31123.834828 -snap {("G3" 7)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.rs1_Paddr\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 25000 -TraceValue 00001
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_Paddr\[id_rs1_addr\]" -line 66 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_Paddr\[id_rs1_addr\]" -line 66 -pos 1 -win $_nTrace1
srcAction -pos 65 19 7 -win $_nTrace1 -name "rat_Paddr\[id_rs1_addr\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_Paddr" -line 41 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G7" 0)}
wvSetPosition -win $_nWave2 {("G6" 8)}
wvSetPosition -win $_nWave2 {("G7" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G7" 1)}
wvSetPosition -win $_nWave2 {("G7" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top.u_alu" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_fu_top.u_alu" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top.u_alu" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_en" -line 5 -pos 1 -win $_nTrace1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 30524.017883 -snap {("G3" 7)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.rs1_Paddr\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 25000 -TraceValue 00001
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_Paddr\[id_rs1_addr\]" -line 66 -pos 1 -win $_nTrace1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G7" 1 )} 
wvExpandBus -win $_nWave2 {("G7" 1)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 7
wvSelectGroup -win $_nWave2 {G8}
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 25059.019048 -snap {("G3" 7)}
wvScrollDown -win $_nWave2 7
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 20793.654103 -snap {("G1" 5)}
wvSetCursor -win $_nWave2 20793.654103 -snap {("G1" 5)}
srcActiveTrace "tb_ooocpu.u_ooocpu.pc\[31:0\]" -win $_nTrace1 -TraceByDConWave \
           -TraceTime 15000 -TraceValue 00000000000000000000000000001000
wvSetCursor -win $_nWave2 22126.580649 -snap {("G1" 6)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_decoder.alu_op\[4:0\]" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 15000 -TraceValue 00001
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvZoom -win $_nWave2 0.000000 40254.381662
wvZoomOut -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "if_pc" -line 6 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "if_insn" -line 7 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G7" 34 )} 
wvSelectSignal -win $_nWave2 {( "G7" 34 35 )} 
wvSetPosition -win $_nWave2 {("G7" 34)}
wvSetPosition -win $_nWave2 {("G7" 33)}
wvSetPosition -win $_nWave2 {("G7" 32)}
wvSetPosition -win $_nWave2 {("G7" 31)}
wvSetPosition -win $_nWave2 {("G7" 30)}
wvSetPosition -win $_nWave2 {("G7" 29)}
wvSetPosition -win $_nWave2 {("G7" 28)}
wvSetPosition -win $_nWave2 {("G7" 27)}
wvSetPosition -win $_nWave2 {("G7" 26)}
wvSetPosition -win $_nWave2 {("G7" 25)}
wvSetPosition -win $_nWave2 {("G7" 24)}
wvSetPosition -win $_nWave2 {("G7" 23)}
wvSetPosition -win $_nWave2 {("G7" 22)}
wvSetPosition -win $_nWave2 {("G7" 21)}
wvSetPosition -win $_nWave2 {("G7" 20)}
wvSetPosition -win $_nWave2 {("G7" 19)}
wvSetPosition -win $_nWave2 {("G7" 18)}
wvSetPosition -win $_nWave2 {("G7" 17)}
wvSetPosition -win $_nWave2 {("G7" 16)}
wvSetPosition -win $_nWave2 {("G7" 15)}
wvSetPosition -win $_nWave2 {("G7" 14)}
wvSetPosition -win $_nWave2 {("G7" 13)}
wvSetPosition -win $_nWave2 {("G7" 12)}
wvSetPosition -win $_nWave2 {("G7" 11)}
wvSetPosition -win $_nWave2 {("G7" 10)}
wvSetPosition -win $_nWave2 {("G7" 9)}
wvSetPosition -win $_nWave2 {("G7" 8)}
wvSetPosition -win $_nWave2 {("G7" 7)}
wvSetPosition -win $_nWave2 {("G7" 6)}
wvSetPosition -win $_nWave2 {("G7" 5)}
wvSetPosition -win $_nWave2 {("G7" 4)}
wvSetPosition -win $_nWave2 {("G7" 3)}
wvSetPosition -win $_nWave2 {("G7" 2)}
wvSetPosition -win $_nWave2 {("G7" 1)}
wvSetPosition -win $_nWave2 {("G7" 0)}
wvSetPosition -win $_nWave2 {("G6" 7)}
wvSetPosition -win $_nWave2 {("G6" 6)}
wvSetPosition -win $_nWave2 {("G6" 5)}
wvSetPosition -win $_nWave2 {("G6" 4)}
wvSetPosition -win $_nWave2 {("G6" 3)}
wvSetPosition -win $_nWave2 {("G6" 2)}
wvSetPosition -win $_nWave2 {("G6" 1)}
wvSetPosition -win $_nWave2 {("G6" 0)}
wvSetPosition -win $_nWave2 {("G5" 1)}
wvSetPosition -win $_nWave2 {("G5" 0)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G3" 11)}
wvSetPosition -win $_nWave2 {("G3" 10)}
wvSetPosition -win $_nWave2 {("G3" 9)}
wvSetPosition -win $_nWave2 {("G3" 8)}
wvSetPosition -win $_nWave2 {("G3" 7)}
wvSetPosition -win $_nWave2 {("G3" 6)}
wvSetPosition -win $_nWave2 {("G3" 5)}
wvSetPosition -win $_nWave2 {("G3" 4)}
wvSetPosition -win $_nWave2 {("G3" 3)}
wvSetPosition -win $_nWave2 {("G3" 2)}
wvSetPosition -win $_nWave2 {("G3" 1)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G1" 10)}
wvSetPosition -win $_nWave2 {("G1" 9)}
wvSetPosition -win $_nWave2 {("G1" 8)}
wvSetPosition -win $_nWave2 {("G1" 7)}
wvSetPosition -win $_nWave2 {("G1" 6)}
wvSetPosition -win $_nWave2 {("G1" 5)}
wvSetPosition -win $_nWave2 {("G1" 4)}
wvSetPosition -win $_nWave2 {("G1" 3)}
wvSetPosition -win $_nWave2 {("G1" 2)}
wvSetPosition -win $_nWave2 {("G1" 1)}
wvSetPosition -win $_nWave2 {("G1" 0)}
wvSetPosition -win $_nWave2 {("G1" 1)}
wvSetPosition -win $_nWave2 {("G1" 2)}
wvSetPosition -win $_nWave2 {("G1" 3)}
wvSetPosition -win $_nWave2 {("G1" 4)}
wvSetPosition -win $_nWave2 {("G1" 5)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G1" 5)}
wvSetPosition -win $_nWave2 {("G1" 7)}
wvSelectSignal -win $_nWave2 {( "G1" 7 )} 
wvSelectSignal -win $_nWave2 {( "G1" 6 )} 
wvSetPosition -win $_nWave2 {("G1" 6)}
wvSetPosition -win $_nWave2 {("G1" 7)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G1" 7)}
wvSelectSignal -win $_nWave2 {( "G1" 6 )} 
wvSelectGroup -win $_nWave2 {G3}
wvSelectSignal -win $_nWave2 {( "G1" 6 )} 
wvSelectSignal -win $_nWave2 {( "G1" 6 7 8 9 10 11 12 )} 
wvSetPosition -win $_nWave2 {("G1" 8)}
wvSetPosition -win $_nWave2 {("G1" 9)}
wvSetPosition -win $_nWave2 {("G1" 10)}
wvSetPosition -win $_nWave2 {("G1" 11)}
wvSetPosition -win $_nWave2 {("G1" 12)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSelectSignal -win $_nWave2 {( "G1" 5 )} 
wvSelectGroup -win $_nWave2 {G2}
wvSelectSignal -win $_nWave2 {( "G2" 1 )} 
wvSelectGroup -win $_nWave2 {G2}
wvSelectSignal -win $_nWave2 {( "G2" 2 )} 
wvSetCursor -win $_nWave2 5096.957678 -snap {("G1" 5)}
wvSetCursor -win $_nWave2 9460.262357 -snap {("G2" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_decoder.if_insn\[31:0\]" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 5000 -TraceValue \
           10010000000000000100000100110111
wvSelectSignal -win $_nWave2 {( "G1" 4 )} 
wvSelectSignal -win $_nWave2 {( "G1" 5 )} 
wvSetPosition -win $_nWave2 {("G1" 5)}
wvSetPosition -win $_nWave2 {("G1" 4)}
wvSetPosition -win $_nWave2 {("G1" 3)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G1" 3)}
wvSetPosition -win $_nWave2 {("G1" 4)}
wvSelectSignal -win $_nWave2 {( "G2" 2 )} 
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G2" 1)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G2" 1)}
wvSelectGroup -win $_nWave2 {G2}
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn" -line 24 -pos 1 -win $_nTrace1
srcAction -pos 23 10 1 -win $_nTrace1 -name "insn" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn" -line 24 -pos 1 -win $_nTrace1
srcAction -pos 23 10 1 -win $_nTrace1 -name "insn" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn" -line 24 -pos 1 -win $_nTrace1
srcAction -pos 23 10 1 -win $_nTrace1 -name "insn" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn" -line 12 -pos 1 -win $_nTrace1
srcAction -pos 11 21 1 -win $_nTrace1 -name "insn" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn" -line 12 -pos 1 -win $_nTrace1
srcShowCalling -win $_nTrace1
srcSelect -win $_nTrace1 -range {203 203 3 4 1 1}
srcHBSelect "tb_ooocpu.u_ooocpu" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn" -line 210 -pos 2 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn" -line 210 -pos 2 -win $_nTrace1
srcAction -pos 209 5 2 -win $_nTrace1 -name "insn" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn" -line 210 -pos 2 -win $_nTrace1
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {210 210 3 4 1 1}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {50 50 17 18 4 1}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {42 42 3 3 4 8}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {13 13 6 6 4 8}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {12 12 19 20 1 1}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {11 11 2 2 4 8}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {221 221 6 7 4 1}
nsMsgSwitchTab -tab general
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {221 221 3 4 4 1}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {213 213 6 7 4 1}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {213 213 3 4 4 1}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {210 210 6 7 1 1}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {210 210 3 4 1 1}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {50 50 17 18 4 1}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {42 42 3 3 4 8}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {13 13 6 6 4 8}
srcSearchString "insn" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {12 12 19 20 1 1}
srcShowCalling -win $_nTrace1
srcSelect -win $_nTrace1 -range {61 61 3 4 1 1}
srcHBSelect "tb_ooocpu" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn_pc_r1\[pc\[31:2\]\]" -line 68 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G2" 2 )} 
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "pc" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn_pc_r1\[pc\[31:2\]\]" -line 68 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "pc" -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn_pc_r1\[i\]" -line 52 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "insn_pc\[i\]" -line 54 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G2" 2 )} 
wvSetPosition -win $_nWave2 {("G2" 2)}
wvExpandBus -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G2" 1029)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 3
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 30
wvScrollDown -win $_nWave2 2
wvScrollUp -win $_nWave2 404
wvScrollUp -win $_nWave2 3
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 3
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 9
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 9
wvScrollUp -win $_nWave2 10
wvScrollUp -win $_nWave2 6
wvScrollUp -win $_nWave2 10
wvScrollUp -win $_nWave2 9
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 3
wvScrollUp -win $_nWave2 6
wvScrollUp -win $_nWave2 3
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 3
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 12
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 12
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 10
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 9
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 3
wvScrollUp -win $_nWave2 11
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 9
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 5
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 3
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 10
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 2
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 3
wvScrollUp -win $_nWave2 9
wvScrollUp -win $_nWave2 4
wvScrollUp -win $_nWave2 3
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G2" 2 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 1029)}
wvSetPosition -win $_nWave2 {("G2" 1028)}
wvSelectSignal -win $_nWave2 {( "G2" 2 )} 
wvScrollDown -win $_nWave2 32
wvScrollDown -win $_nWave2 988
wvSelectSignal -win $_nWave2 {( "G2" 1025 )} 
wvScrollUp -win $_nWave2 1017
wvSelectSignal -win $_nWave2 {( "G2" 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 \
           19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 \
           41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 \
           63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 \
           85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 \
           106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 \
           123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 \
           140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 \
           157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 \
           174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 \
           191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 \
           208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 \
           225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 \
           242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 \
           259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 \
           276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 \
           293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 \
           310 311 312 313 314 315 316 317 318 319 320 321 322 323 324 325 326 \
           327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342 343 \
           344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360 \
           361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 377 \
           378 379 380 381 382 383 384 385 386 387 388 389 390 391 392 393 394 \
           395 396 397 398 399 400 401 402 403 404 405 406 407 408 409 410 411 \
           412 413 414 415 416 417 418 419 420 421 422 423 424 425 426 427 428 \
           429 430 431 432 433 434 435 436 437 438 439 440 441 442 443 444 445 \
           446 447 448 449 450 451 452 453 454 455 456 457 458 459 460 461 462 \
           463 464 465 466 467 468 469 470 471 472 473 474 475 476 477 478 479 \
           480 481 482 483 484 485 486 487 488 489 490 491 492 493 494 495 496 \
           497 498 499 500 501 502 503 504 505 506 507 508 509 510 511 512 513 \
           514 515 516 517 518 519 520 521 522 523 524 525 526 527 528 529 530 \
           531 532 533 534 535 536 537 538 539 540 541 542 543 544 545 546 547 \
           548 549 550 551 552 553 554 555 556 557 558 559 560 561 562 563 564 \
           565 566 567 568 569 570 571 572 573 574 575 576 577 578 579 580 581 \
           582 583 584 585 586 587 588 589 590 591 592 593 594 595 596 597 598 \
           599 600 601 602 603 604 605 606 607 608 609 610 611 612 613 614 615 \
           616 617 618 619 620 621 622 623 624 625 626 627 628 629 630 631 632 \
           633 634 635 636 637 638 639 640 641 642 643 644 645 646 647 648 649 \
           650 651 652 653 654 655 656 657 658 659 660 661 662 663 664 665 666 \
           667 668 669 670 671 672 673 674 675 676 677 678 679 680 681 682 683 \
           684 685 686 687 688 689 690 691 692 693 694 695 696 697 698 699 700 \
           701 702 703 704 705 706 707 708 709 710 711 712 713 714 715 716 717 \
           718 719 720 721 722 723 724 725 726 727 728 729 730 731 732 733 734 \
           735 736 737 738 739 740 741 742 743 744 745 746 747 748 749 750 751 \
           752 753 754 755 756 757 758 759 760 761 762 763 764 765 766 767 768 \
           769 770 771 772 773 774 775 776 777 778 779 780 781 782 783 784 785 \
           786 787 788 789 790 791 792 793 794 795 796 797 798 799 800 801 802 \
           803 804 805 806 807 808 809 810 811 812 813 814 815 816 817 818 819 \
           820 821 822 823 824 825 826 827 828 829 830 831 832 833 834 835 836 \
           837 838 839 840 841 842 843 844 845 846 847 848 849 850 851 852 853 \
           854 855 856 857 858 859 860 861 862 863 864 865 866 867 868 869 870 \
           871 872 873 874 875 876 877 878 879 880 881 882 883 884 885 886 887 \
           888 889 890 891 892 893 894 895 896 897 898 899 900 901 902 903 904 \
           905 906 907 908 909 910 911 912 913 914 915 916 917 918 919 920 921 \
           922 923 924 925 926 927 928 929 930 931 932 933 934 935 936 937 938 \
           939 940 941 942 943 944 945 946 947 948 949 950 951 952 953 954 955 \
           956 957 958 959 960 961 962 963 964 965 966 967 968 969 970 971 972 \
           973 974 975 976 977 978 979 980 981 982 983 984 985 986 987 988 989 \
           990 991 992 993 994 995 996 997 998 999 1000 1001 1002 1003 1004 1005 \
           1006 1007 1008 1009 1010 1011 1012 1013 1014 1015 1016 1017 1018 1019 \
           1020 1021 1022 1023 1024 1025 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 4)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G2" 3 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSetPosition -win $_nWave2 {("G2" 3)}
wvSelectSignal -win $_nWave2 {( "G2" 3 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 3)}
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSelectSignal -win $_nWave2 {( "G2" 2 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G2" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 15097.806455 -snap {("G2" 1)}
wvSetCursor -win $_nWave2 4788.051152 -snap {("G2" 3)}
wvSetCursor -win $_nWave2 10425.595251 -snap {("G2" 3)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_decoder.alu_op\[4:0\]" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 5000 -TraceValue 01010
srcDeselectAll -win $_nTrace1
srcSelect -signal "dst_addr" -line 110 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 3)}
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSetPosition -win $_nWave2 {("G2" 6)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 6)}
wvSelectSignal -win $_nWave2 {( "G2" 8 )} 
wvSetPosition -win $_nWave2 {("G2" 8)}
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSetPosition -win $_nWave2 {("G2" 6)}
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSetPosition -win $_nWave2 {("G2" 3)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 3)}
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSelectSignal -win $_nWave2 {( "G2" 7 )} 
wvSetCursor -win $_nWave2 4981.117731 -snap {("G2" 3)}
wvSetCursor -win $_nWave2 14943.353192 -snap {("G2" 4)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 24828.362023 -snap {("G7" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G7" 2 )} 
wvScrollDown -win $_nWave2 29
wvSelectSignal -win $_nWave2 {( "G7" 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 \
           19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSelectSignal -win $_nWave2 {( "G7" 1 )} 
wvSetPosition -win $_nWave2 {("G7" 1)}
wvExpandBus -win $_nWave2 {("G7" 1)}
wvSelectSignal -win $_nWave2 {( "G7" 33 )} 
wvScrollDown -win $_nWave2 0
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G7" 32)}
wvSelectSignal -win $_nWave2 {( "G7" 31 )} 
wvSelectSignal -win $_nWave2 {( "G7" 32 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G7" 31)}
wvSelectSignal -win $_nWave2 {( "G7" 30 )} 
wvScrollUp -win $_nWave2 15
wvSelectSignal -win $_nWave2 {( "G7" 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 \
           19 20 21 22 23 24 25 26 27 28 29 30 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G7" 2)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G6" 1 )} 
wvSelectSignal -win $_nWave2 {( "G6" 1 2 3 4 5 6 7 )} 
wvSetPosition -win $_nWave2 {("G6" 2)}
wvSetPosition -win $_nWave2 {("G6" 3)}
wvSetPosition -win $_nWave2 {("G6" 4)}
wvSetPosition -win $_nWave2 {("G6" 5)}
wvSetPosition -win $_nWave2 {("G6" 6)}
wvSetPosition -win $_nWave2 {("G6" 7)}
wvSetPosition -win $_nWave2 {("G7" 0)}
wvSetPosition -win $_nWave2 {("G7" 1)}
wvSetPosition -win $_nWave2 {("G7" 2)}
wvSetPosition -win $_nWave2 {("G8" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G8" 7)}
wvSetPosition -win $_nWave2 {("G8" 7)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectGroup -win $_nWave2 {G6}
wvSelectGroup -win $_nWave2 {G5}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
srcHBSelect "tb_ooocpu.u_ooocpu.u_rob" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_rob" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_rob" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_rob" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "allocate_en" -line 8 -pos 1 -win $_nTrace1
srcSelect -win $_nTrace1 -range {8 10 6 17 7 2}
srcDeselectAll -win $_nTrace1
srcSelect -signal "allocate_en" -line 8 -pos 1 -win $_nTrace1
srcSelect -signal "pc" -line 10 -pos 1 -win $_nTrace1
srcSelect -signal "dst_addr" -line 11 -pos 1 -win $_nTrace1
srcSelect -signal "dst_wen" -line 12 -pos 1 -win $_nTrace1
srcSelect -signal "rs1_addr" -line 13 -pos 1 -win $_nTrace1
srcSelect -signal "rs2_addr" -line 14 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G8" 10)}
wvSetPosition -win $_nWave2 {("G8" 9)}
wvSetPosition -win $_nWave2 {("G8" 8)}
wvSetPosition -win $_nWave2 {("G8" 7)}
wvSetPosition -win $_nWave2 {("G8" 6)}
wvSetPosition -win $_nWave2 {("G8" 5)}
wvSetPosition -win $_nWave2 {("G8" 4)}
wvSetPosition -win $_nWave2 {("G8" 3)}
wvSetPosition -win $_nWave2 {("G8" 2)}
wvSetPosition -win $_nWave2 {("G8" 1)}
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G7" 2)}
wvSetPosition -win $_nWave2 {("G7" 1)}
wvSetPosition -win $_nWave2 {("G7" 0)}
wvSetPosition -win $_nWave2 {("G6" 0)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G4" 6)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G3" 1 )} 
wvSelectSignal -win $_nWave2 {( "G3" 1 2 3 4 5 6 7 8 9 10 11 )} 
wvSetPosition -win $_nWave2 {("G3" 6)}
wvSetPosition -win $_nWave2 {("G3" 7)}
wvSetPosition -win $_nWave2 {("G3" 8)}
wvSetPosition -win $_nWave2 {("G3" 9)}
wvSetPosition -win $_nWave2 {("G3" 10)}
wvSetPosition -win $_nWave2 {("G3" 11)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G4" 1)}
wvSetPosition -win $_nWave2 {("G4" 2)}
wvSetPosition -win $_nWave2 {("G4" 3)}
wvSetPosition -win $_nWave2 {("G4" 4)}
wvSetPosition -win $_nWave2 {("G4" 5)}
wvSetPosition -win $_nWave2 {("G4" 6)}
wvSetPosition -win $_nWave2 {("G5" 0)}
wvSetPosition -win $_nWave2 {("G5" 1)}
wvSetPosition -win $_nWave2 {("G6" 0)}
wvSetPosition -win $_nWave2 {("G7" 0)}
wvSetPosition -win $_nWave2 {("G7" 1)}
wvSetPosition -win $_nWave2 {("G7" 2)}
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G8" 1)}
wvSetPosition -win $_nWave2 {("G8" 2)}
wvSetPosition -win $_nWave2 {("G8" 3)}
wvSetPosition -win $_nWave2 {("G8" 4)}
wvSetPosition -win $_nWave2 {("G8" 5)}
wvSetPosition -win $_nWave2 {("G8" 6)}
wvSetPosition -win $_nWave2 {("G8" 7)}
wvSetPosition -win $_nWave2 {("G8" 6)}
wvSetPosition -win $_nWave2 {("G8" 5)}
wvSetPosition -win $_nWave2 {("G8" 4)}
wvSetPosition -win $_nWave2 {("G8" 3)}
wvSetPosition -win $_nWave2 {("G8" 2)}
wvSetPosition -win $_nWave2 {("G8" 1)}
wvSetPosition -win $_nWave2 {("G8" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G8" 11)}
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G8" 12 )} 
wvSelectSignal -win $_nWave2 {( "G8" 12 13 14 15 16 17 18 )} 
wvSetPosition -win $_nWave2 {("G8" 15)}
wvSetPosition -win $_nWave2 {("G8" 16)}
wvSetPosition -win $_nWave2 {("G8" 17)}
wvSetPosition -win $_nWave2 {("G8" 18)}
wvSetPosition -win $_nWave2 {("G9" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 7)}
wvSetPosition -win $_nWave2 {("G9" 7)}
wvSelectGroup -win $_nWave2 {G9}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 14518.606719 -snap {("G2" 3)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
srcDeselectAll -win $_nTrace1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G4" 5 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G10" 0)}
wvSetPosition -win $_nWave2 {("G9" 7)}
wvSelectSignal -win $_nWave2 {( "G4" 5 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G10" 0)}
wvSetPosition -win $_nWave2 {("G9" 7)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G5" 1 )} 
wvSetPosition -win $_nWave2 {("G5" 1)}
wvSetPosition -win $_nWave2 {("G6" 0)}
wvSetPosition -win $_nWave2 {("G7" 0)}
wvSetPosition -win $_nWave2 {("G7" 1)}
wvSetPosition -win $_nWave2 {("G7" 2)}
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G8" 1)}
wvSetPosition -win $_nWave2 {("G8" 2)}
wvSetPosition -win $_nWave2 {("G8" 3)}
wvSetPosition -win $_nWave2 {("G8" 4)}
wvSetPosition -win $_nWave2 {("G8" 5)}
wvSetPosition -win $_nWave2 {("G8" 6)}
wvSetPosition -win $_nWave2 {("G8" 7)}
wvSetPosition -win $_nWave2 {("G8" 8)}
wvSetPosition -win $_nWave2 {("G8" 9)}
wvSetPosition -win $_nWave2 {("G8" 10)}
wvSetPosition -win $_nWave2 {("G8" 11)}
wvSetPosition -win $_nWave2 {("G9" 0)}
wvSetPosition -win $_nWave2 {("G9" 1)}
wvSetPosition -win $_nWave2 {("G9" 2)}
wvSetPosition -win $_nWave2 {("G9" 3)}
wvSetPosition -win $_nWave2 {("G9" 4)}
wvSetPosition -win $_nWave2 {("G9" 5)}
wvSetPosition -win $_nWave2 {("G9" 6)}
wvSetPosition -win $_nWave2 {("G9" 7)}
wvSetPosition -win $_nWave2 {("G10" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G10" 1)}
wvSetPosition -win $_nWave2 {("G10" 1)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_alloc_tag_2rat" -line 51 -pos 1 -win $_nTrace1
srcSelect -signal "rob_alloc_dst_addr_2rat" -line 52 -pos 1 -win $_nTrace1
srcSelect -signal "rob_alloc_dst_wen_2rat" -line 53 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvSetPosition -win $_nWave2 {("G10" 3)}
wvSetPosition -win $_nWave2 {("G10" 2)}
wvSetPosition -win $_nWave2 {("G10" 1)}
wvSetPosition -win $_nWave2 {("G10" 0)}
wvSetPosition -win $_nWave2 {("G9" 7)}
wvSetPosition -win $_nWave2 {("G9" 6)}
wvSetPosition -win $_nWave2 {("G9" 5)}
wvSetPosition -win $_nWave2 {("G9" 4)}
wvSetPosition -win $_nWave2 {("G9" 3)}
wvSetPosition -win $_nWave2 {("G9" 2)}
wvSetPosition -win $_nWave2 {("G9" 1)}
wvSetPosition -win $_nWave2 {("G9" 0)}
wvSetPosition -win $_nWave2 {("G8" 10)}
wvSetPosition -win $_nWave2 {("G8" 9)}
wvSetPosition -win $_nWave2 {("G8" 8)}
wvSetPosition -win $_nWave2 {("G8" 7)}
wvSetPosition -win $_nWave2 {("G8" 6)}
wvSetPosition -win $_nWave2 {("G8" 5)}
wvSetPosition -win $_nWave2 {("G8" 4)}
wvSetPosition -win $_nWave2 {("G8" 3)}
wvSetPosition -win $_nWave2 {("G8" 2)}
wvSetPosition -win $_nWave2 {("G8" 1)}
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G7" 2)}
wvSetPosition -win $_nWave2 {("G7" 1)}
wvSetPosition -win $_nWave2 {("G7" 0)}
wvSetPosition -win $_nWave2 {("G6" 0)}
wvSetPosition -win $_nWave2 {("G5" 0)}
wvSetPosition -win $_nWave2 {("G4" 4)}
wvSetPosition -win $_nWave2 {("G4" 3)}
wvSetPosition -win $_nWave2 {("G4" 2)}
wvSetPosition -win $_nWave2 {("G4" 1)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G2" 8)}
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSetPosition -win $_nWave2 {("G2" 6)}
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSetPosition -win $_nWave2 {("G2" 3)}
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSetPosition -win $_nWave2 {("G2" 6)}
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSetPosition -win $_nWave2 {("G2" 8)}
wvSetPosition -win $_nWave2 {("G3" 0)}
wvSetPosition -win $_nWave2 {("G4" 0)}
wvSetPosition -win $_nWave2 {("G4" 1)}
wvSetPosition -win $_nWave2 {("G4" 2)}
wvSetPosition -win $_nWave2 {("G4" 3)}
wvSetPosition -win $_nWave2 {("G4" 4)}
wvSetPosition -win $_nWave2 {("G5" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G5" 0)}
wvSetPosition -win $_nWave2 {("G5" 3)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_rat" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_rat" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_rat" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_rat" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_rat" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_alloc_tag_2rat" -line 18 -pos 1 -win $_nTrace1
srcSelect -signal "rob_alloc_dst_addr_2rat" -line 19 -pos 1 -win $_nTrace1
srcSelect -signal "rob_alloc_dst_wen_2rat" -line 20 -pos 1 -win $_nTrace1
srcSelect -signal "allocate_en" -line 17 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G5" 6 )} 
wvSelectSignal -win $_nWave2 {( "G5" 4 5 6 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G7" 1)}
wvSetPosition -win $_nWave2 {("G5" 4)}
wvSetCursor -win $_nWave2 5405.864204 -snap {("G5" 3)}
wvSetCursor -win $_nWave2 15290.873034 -snap {("G5" 2)}
wvSetCursor -win $_nWave2 15290.873034 -snap {("G5" 2)}
wvSetCursor -win $_nWave2 14750.286614 -snap {("G5" 1)}
wvSetCursor -win $_nWave2 27956.040598 -snap {("G7" 2)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_rat.rat_Paddr\[2\]\[4:0\]" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 25000 -TraceValue 00001
wvSetCursor -win $_nWave2 16526.499138 -snap {("G7" 2)}
wvSetCursor -win $_nWave2 16989.858927 -snap {("G7" 2)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_rat.rat_Paddr\[2\]\[4:0\]" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 0 -TraceValue 00000
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_valid" -line 42 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G6" 0)}
wvSetPosition -win $_nWave2 {("G7" 0)}
wvSetPosition -win $_nWave2 {("G7" 1)}
wvSetPosition -win $_nWave2 {("G7" 2)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G7" 3)}
wvSetCursor -win $_nWave2 14518.606719 -snap {("G7" 3)}
wvSelectSignal -win $_nWave2 {( "G7" 3 )} 
wvExpandBus -win $_nWave2 {("G7" 3)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G7" 35 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G7" 34)}
wvSelectSignal -win $_nWave2 {( "G7" 34 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G7" 33)}
wvSelectSignal -win $_nWave2 {( "G7" 32 )} 
wvSelectSignal -win $_nWave2 {( "G7" 22 23 24 25 26 27 28 29 30 31 32 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G8" 10)}
wvSetPosition -win $_nWave2 {("G7" 22)}
wvSelectSignal -win $_nWave2 {( "G7" 20 )} 
wvScrollUp -win $_nWave2 6
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G7" 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 \
           20 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 4)}
wvSetPosition -win $_nWave2 {("G7" 5)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 25021.428601 -snap {("G7" 2)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 14673.059982 -snap {("G4" 2)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 25098.655233 -snap {("G7" 2)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G7" 3 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G7" 4)}
wvSetCursor -win $_nWave2 14750.286614 -snap {("G7" 4)}
wvSetCursor -win $_nWave2 25175.881864 -snap {("G7" 2)}
wvSelectSignal -win $_nWave2 {( "G7" 3 )} 
wvSetPosition -win $_nWave2 {("G7" 3)}
wvSetPosition -win $_nWave2 {("G7" 4)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G7" 4)}
wvSetCursor -win $_nWave2 14673.059982 -snap {("G7" 3)}
wvSetCursor -win $_nWave2 25021.428601 -snap {("G7" 2)}
wvSetCursor -win $_nWave2 15290.873034 -snap {("G7" 3)}
wvSetCursor -win $_nWave2 25098.655233 -snap {("G7" 2)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 32049.052067 -snap {("G8" 7)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.rs1_Paddr\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 25000 -TraceValue 00001
wvSetCursor -win $_nWave2 25098.655233 -snap {("G8" 7)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 14827.513245 -snap {("G2" 3)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 44945.899525 -snap {("G10" 1)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 25098.655233 -snap {("G8" 4)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 14981.966508 -snap {("G9" 3)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcHBSelect "tb_ooocpu.u_ooocpu.u_cpu_ctrl" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_decoder" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_fu_top" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_en" -line 8 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_out" -line 44 -pos 1 -win $_nTrace1
srcSelect -signal "alu_out_valid" -line 45 -pos 1 -win $_nTrace1
srcSelect -signal "alu_dst_Paddr" -line 46 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G7" 6)}
wvSetPosition -win $_nWave2 {("G7" 7)}
wvSetPosition -win $_nWave2 {("G8" 0)}
wvSetPosition -win $_nWave2 {("G8" 1)}
wvSetPosition -win $_nWave2 {("G8" 2)}
wvSetPosition -win $_nWave2 {("G8" 4)}
wvSetPosition -win $_nWave2 {("G8" 6)}
wvSetPosition -win $_nWave2 {("G8" 7)}
wvSetPosition -win $_nWave2 {("G8" 8)}
wvSetPosition -win $_nWave2 {("G8" 10)}
wvSetPosition -win $_nWave2 {("G8" 11)}
wvSetPosition -win $_nWave2 {("G9" 0)}
wvSetPosition -win $_nWave2 {("G9" 1)}
wvSetPosition -win $_nWave2 {("G9" 2)}
wvSetPosition -win $_nWave2 {("G9" 3)}
wvSetPosition -win $_nWave2 {("G9" 4)}
wvSetPosition -win $_nWave2 {("G9" 5)}
wvSetPosition -win $_nWave2 {("G9" 6)}
wvSetPosition -win $_nWave2 {("G9" 7)}
wvSetPosition -win $_nWave2 {("G10" 0)}
wvSetPosition -win $_nWave2 {("G10" 1)}
wvSetPosition -win $_nWave2 {("G11" 0)}
wvSetPosition -win $_nWave2 {("G10" 1)}
wvSetPosition -win $_nWave2 {("G11" 0)}
wvSetPosition -win $_nWave2 {("G10" 1)}
wvSetPosition -win $_nWave2 {("G10" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G10" 3)}
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G10" 4 )} 
wvSetPosition -win $_nWave2 {("G10" 4)}
wvSetPosition -win $_nWave2 {("G11" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G11" 1)}
wvSetPosition -win $_nWave2 {("G11" 1)}
wvSelectSignal -win $_nWave2 {( "G10" 1 )} 
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectGroup -win $_nWave2 {G11}
wvSelectSignal -win $_nWave2 {( "G10" 1 )} 
wvSelectGroup -win $_nWave2 {G11}
wvSetCursor -win $_nWave2 55062.588249 -snap {("G10" 1)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "mul_div_issue_en" -line 16 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_op" -line 17 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_rs1_value" -line 18 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_rs2_value" -line 19 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_Pdst" -line 20 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G11" 1 )} 
wvSetPosition -win $_nWave2 {("G11" 1)}
wvSetPosition -win $_nWave2 {("G11" 2)}
wvSetPosition -win $_nWave2 {("G11" 3)}
wvSetPosition -win $_nWave2 {("G11" 4)}
wvSetPosition -win $_nWave2 {("G11" 5)}
wvSetPosition -win $_nWave2 {("G11" 6)}
wvSetPosition -win $_nWave2 {("G12" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 1)}
wvSetPosition -win $_nWave2 {("G12" 1)}
wvSelectGroup -win $_nWave2 {G12}
wvSelectSignal -win $_nWave2 {( "G11" 1 )} 
wvSelectGroup -win $_nWave2 {G12}
wvSelectSignal -win $_nWave2 {( "G11" 1 )} 
wvSetCursor -win $_nWave2 65256.503605 -snap {("G11" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_en" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 0 -TraceValue 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 127 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 127 -pos 1 -win $_nTrace1
srcAction -pos 126 6 6 -win $_nTrace1 -name "fifo_issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_empty" -line 125 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 125 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fu_ready" -line 125 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 125 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 125 -pos 1 -win $_nTrace1
srcAction -pos 124 11 6 -win $_nTrace1 -name "issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAction -pos 104 7 11 -win $_nTrace1 -name "issue_queue_ready\[0\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs2_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs2_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAction -pos 336 13 7 -win $_nTrace1 -name "issue_queue_Prs2_valid\[j\]" \
          -ctrlKey off
wvSetCursor -win $_nWave2 58306.106771 -snap {("G12" 7)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.issue_queue_Prs1_valid\[0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 276 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "j" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 276 -pos 1 -win $_nTrace1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G8" 11 )} 
wvSelectSignal -win $_nWave2 {( "G8" 1 2 3 4 5 6 7 8 9 10 11 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 8)}
wvScrollUp -win $_nWave2 12
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G7" 4 )} 
wvSelectSignal -win $_nWave2 {( "G7" 1 2 3 4 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 8)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G4" 1 )} 
wvSelectSignal -win $_nWave2 {( "G4" 1 )} 
wvSelectSignal -win $_nWave2 {( "G4" 1 )} 
wvSelectSignal -win $_nWave2 {( "G4" 1 2 3 4 )} {( "G5" 1 2 3 4 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 8)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G11" 3 )} 
wvSelectSignal -win $_nWave2 {( "G11" 3 4 )} 
wvSelectSignal -win $_nWave2 {( "G11" 3 4 )} 
wvSetRadix -win $_nWave2 -format UDec
wvSelectSignal -win $_nWave2 {( "G11" 5 )} 
wvSelectSignal -win $_nWave2 {( "G11" 1 )} 
wvSetCursor -win $_nWave2 61395.172031 -snap {("G11" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_en" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 0 -TraceValue 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 127 -pos 1 -win $_nTrace1
srcAction -pos 126 6 7 -win $_nTrace1 -name "fifo_issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 125 -pos 1 -win $_nTrace1
srcAction -pos 124 11 5 -win $_nTrace1 -name "issue_en" -ctrlKey off
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G12" 8 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 7)}
wvSelectSignal -win $_nWave2 {( "G12" 7 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 6)}
wvSelectSignal -win $_nWave2 {( "G12" 6 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 5)}
wvSelectSignal -win $_nWave2 {( "G12" 5 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 4)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAction -pos 104 7 11 -win $_nTrace1 -name "issue_queue_ready\[0\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs2_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAction -pos 336 8 13 -win $_nTrace1 -name "issue_queue_Prs1_valid\[j\]" \
          -ctrlKey off
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 63171.384555 -snap {("G12" 7)}
wvSetCursor -win $_nWave2 62476.344872 -snap {("G12" 7)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.issue_queue_Prs1_valid\[0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_write_en" -line 275 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_p\[j\]" -line 275 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 275 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_value_fromGPR" -line 277 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 276 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G12" 11 )} 
wvSelectSignal -win $_nWave2 {( "G12" 11 )} 
wvSetRadix -win $_nWave2 -format UDec
wvSetCursor -win $_nWave2 54676.455092 -snap {("G12" 12)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 276 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G12" 4 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 12)}
wvSelectSignal -win $_nWave2 {( "G12" 3 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 11)}
wvSelectSignal -win $_nWave2 {( "G12" 3 )} 
wvSelectSignal -win $_nWave2 {( "G12" 4 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 10)}
wvSelectSignal -win $_nWave2 {( "G12" 4 )} 
wvSetCursor -win $_nWave2 59927.866033 -snap {("G12" 4)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.issue_queue_Prs1_valid\[0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue 1
wvSetCursor -win $_nWave2 50351.763728 -snap {("G12" 8)}
wvSetCursor -win $_nWave2 50351.763728 -snap {("G12" 8)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.rs1_value_fromGPR\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 45000 -TraceValue \
           00000000000000000000001111101000
wvSetCursor -win $_nWave2 50351.763728 -snap {("G12" 8)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.rs1_value_fromGPR\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 45000 -TraceValue \
           00000000000000000000001111101000
wvSetCursor -win $_nWave2 59696.186138 -snap {("G12" 8)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.rs1_value_fromGPR\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 45000 -TraceValue \
           00000000000000000000001111101000
wvSetCursor -win $_nWave2 52359.656147 -snap {("G12" 8)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.rs1_value_fromGPR\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 45000 -TraceValue \
           00000000000000000000001111101000
wvSetCursor -win $_nWave2 48807.231099 -snap {("G12" 9)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.rs1_rat_valid" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 45000 -TraceValue 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 59155.599718 -snap {("G11" 2)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_queue_op\[4:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue 11010
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue2queue_en" -line 10 -pos 1 -win $_nTrace1
srcSelect -signal "id_op" -line 11 -pos 1 -win $_nTrace1
srcSelect -signal "id_pc" -line 12 -pos 1 -win $_nTrace1
srcSelect -signal "id_imm" -line 13 -pos 1 -win $_nTrace1
srcSelect -signal "id_alloc_rob" -line 14 -pos 1 -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 16 -pos 1 -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 17 -pos 1 -win $_nTrace1
srcSelect -signal "rs2_rat_valid" -line 18 -pos 1 -win $_nTrace1
srcSelect -signal "rs2_Paddr" -line 19 -pos 1 -win $_nTrace1
srcSelect -signal "rs1_value_fromGPR" -line 20 -pos 1 -win $_nTrace1
srcSelect -signal "rs2_value_fromGPR" -line 21 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 13)}
wvSetPosition -win $_nWave2 {("G12" 14)}
wvSetPosition -win $_nWave2 {("G12" 15)}
wvSetPosition -win $_nWave2 {("G12" 16)}
wvSetPosition -win $_nWave2 {("G12" 17)}
wvSetPosition -win $_nWave2 {("G12" 18)}
wvSetPosition -win $_nWave2 {("G12" 19)}
wvSetPosition -win $_nWave2 {("G12" 20)}
wvSetPosition -win $_nWave2 {("G12" 21)}
wvSetPosition -win $_nWave2 {("G13" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 11)}
wvSetPosition -win $_nWave2 {("G13" 11)}
wvSelectSignal -win $_nWave2 {( "G13" 1 2 3 4 5 6 7 8 9 10 11 )} 
wvSelectGroup -win $_nWave2 {G13}
wvSetCursor -win $_nWave2 45177.579419 -snap {("G13" 10)}
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 51124.030043 -snap {("G13" 8)}
wvSetCursor -win $_nWave2 51278.483306 -snap {("G13" 6)}
wvSetCursor -win $_nWave2 51278.483306 -snap {("G13" 6)}
wvSetCursor -win $_nWave2 51278.483306 -snap {("G13" 6)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.rs1_rat_valid" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 45000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_valid\[id_rs1_addr\]" -line 65 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_valid\[id_rs1_addr\]" -line 65 -pos 1 -win $_nTrace1
srcAction -pos 64 19 4 -win $_nTrace1 -name "rat_valid\[id_rs1_addr\]" -ctrlKey \
          off
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G13" 12 )} 
wvSetCursor -win $_nWave2 35601.477115 -snap {("G13" 12)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "allocate_en" -line 17 -pos 1 -win $_nTrace1
srcSelect -signal "rob_alloc_tag_2rat" -line 18 -pos 1 -win $_nTrace1
srcSelect -signal "rob_alloc_dst_addr_2rat" -line 19 -pos 1 -win $_nTrace1
srcSelect -signal "rob_alloc_dst_wen_2rat" -line 20 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "commit_en" -line 23 -pos 1 -win $_nTrace1
srcSelect -signal "rob_commit_dst_addr_2rat" -line 24 -pos 1 -win $_nTrace1
srcSelect -signal "rob_commit_br_taken" -line 26 -pos 1 -win $_nTrace1
srcSelect -toggle -signal "rob_commit_br_taken" -line 26 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetPosition -win $_nWave2 {("G13" 13)}
wvSetPosition -win $_nWave2 {("G13" 14)}
wvSetPosition -win $_nWave2 {("G14" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 2)}
wvSetPosition -win $_nWave2 {("G14" 2)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 24712.522075 -snap {("G14" 1)}
wvSelectGroup -win $_nWave2 {G15}
wvSetCursor -win $_nWave2 25330.335127 -snap {("G14" 1)}
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_Paddr" -line 41 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "rat_valid" -line 42 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 25098.655233 -snap {("G14" 1)}
wvSetCursor -win $_nWave2 34906.437431 -snap {("G14" 4)}
wvSelectSignal -win $_nWave2 {( "G14" 4 )} 
wvExpandBus -win $_nWave2 {("G14" 4)}
wvSelectSignal -win $_nWave2 {( "G14" 36 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 0)}
wvSetPosition -win $_nWave2 {("G14" 35)}
wvSelectSignal -win $_nWave2 {( "G14" 35 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 0)}
wvSetPosition -win $_nWave2 {("G14" 34)}
wvSelectSignal -win $_nWave2 {( "G14" 33 )} 
wvScrollUp -win $_nWave2 2
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G14" 23 24 25 26 27 28 29 30 31 32 33 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 23)}
wvSelectSignal -win $_nWave2 {( "G14" 20 )} 
wvSelectSignal -win $_nWave2 {( "G14" 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 \
           20 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetCursor -win $_nWave2 15831.459455 -snap {("G14" 7)}
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 24866.975338 -snap {("G14" 1)}
wvSetCursor -win $_nWave2 34751.984168 -snap {("G14" 5)}
wvSetCursor -win $_nWave2 24403.615549 -snap {("G14" 2)}
wvSetCursor -win $_nWave2 25098.655233 -snap {("G14" 1)}
wvSetCursor -win $_nWave2 34674.757537 -snap {("G14" 3)}
wvSelectSignal -win $_nWave2 {( "G14" 4 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 0)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvSelectSignal -win $_nWave2 {( "G14" 3 )} 
wvSetPosition -win $_nWave2 {("G14" 3)}
wvExpandBus -win $_nWave2 {("G14" 3)}
wvSetPosition -win $_nWave2 {("G14" 38)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G14" 34 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 0)}
wvSetPosition -win $_nWave2 {("G14" 37)}
wvSelectSignal -win $_nWave2 {( "G14" 34 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 0)}
wvSetPosition -win $_nWave2 {("G14" 36)}
wvSelectSignal -win $_nWave2 {( "G14" 32 )} 
wvSelectSignal -win $_nWave2 {( "G14" 22 23 24 25 26 27 28 29 30 31 32 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 25)}
wvSelectSignal -win $_nWave2 {( "G14" 19 )} 
wvSelectSignal -win $_nWave2 {( "G14" 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 \
           19 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 8)}
wvSelectGroup -win $_nWave2 {G15}
wvSetCursor -win $_nWave2 24944.201970 -snap {("G14" 1)}
wvSetCursor -win $_nWave2 34365.851011 -snap {("G14" 3)}
wvSelectSignal -win $_nWave2 {( "G14" 8 )} 
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 5)}
wvSetPosition -win $_nWave2 {("G14" 4)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 4)}
wvSetPosition -win $_nWave2 {("G14" 5)}
wvSelectSignal -win $_nWave2 {( "G14" 4 )} 
wvSetPosition -win $_nWave2 {("G14" 4)}
wvSetPosition -win $_nWave2 {("G14" 5)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 8)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 8)}
wvSelectSignal -win $_nWave2 {( "G14" 3 )} 
wvSetPosition -win $_nWave2 {("G14" 3)}
wvSetPosition -win $_nWave2 {("G14" 4)}
wvSetPosition -win $_nWave2 {("G14" 5)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 6)}
wvSelectGroup -win $_nWave2 {G15}
wvSelectSignal -win $_nWave2 {( "G14" 4 )} 
wvSelectSignal -win $_nWave2 {( "G14" 3 )} 
wvSetCursor -win $_nWave2 24866.975338 -snap {("G14" 1)}
wvSetCursor -win $_nWave2 35060.890694 -snap {("G14" 4)}
wvSetCursor -win $_nWave2 45254.806050 -snap {("G13" 8)}
wvSetCursor -win $_nWave2 24944.201970 -snap {("G14" 1)}
wvSetCursor -win $_nWave2 35215.343957 -snap {("G14" 3)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 54830.908355 -snap {("G10" 2)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G13" 1 )} 
wvSelectSignal -win $_nWave2 {( "G13" 1 2 3 4 5 6 7 8 9 10 11 12 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 6)}
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 56838.800773 -snap {("G12" 10)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.issue_queue_Prs1_valid\[0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value\[j\]" -line 277 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G14" 7 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 6)}
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value\[j\]" -line 277 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 276 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_value_fromGPR" -line 277 -pos 1 -win $_nTrace1
wvSetCursor -win $_nWave2 34906.437431 -snap {("G14" 3)}
wvSetCursor -win $_nWave2 54522.001829 -snap {("G14" 6)}
wvSetCursor -win $_nWave2 64793.143816 -snap {("G14" 7)}
wvSetCursor -win $_nWave2 54985.361618 -snap {("G14" 6)}
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 60391.225822 -snap {("G14" 7)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_rat.rat_valid\[14\]" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 45000 -TraceValue 1
wvSetCursor -win $_nWave2 54985.361618 -snap {("G14" 7)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectGroup -win $_nWave2 {G4}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvSelectGroup -win $_nWave2 {G6}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvSelectGroup -win $_nWave2 {G7}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvSelectGroup -win $_nWave2 {G8}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvSelectGroup -win $_nWave2 {G9}
wvSelectGroup -win $_nWave2 {G5}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvSelectGroup -win $_nWave2 {G3}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 54985.361618 -snap {("G10" 3)}
wvSetCursor -win $_nWave2 64561.463922 -snap {("G14" 1)}
wvSetCursor -win $_nWave2 55294.268144 -snap {("G14" 1)}
wvSetCursor -win $_nWave2 55294.268144 -snap {("G14" 2)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_gpr" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "gpr\[i\]" -line 33 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 34906.437431 -snap {("G14" 7)}
wvSetCursor -win $_nWave2 45718.165839 -snap {("G14" 7)}
wvSetCursor -win $_nWave2 54058.642040 -snap {("G14" 7)}
wvSetCursor -win $_nWave2 62399.118240 -snap {("G14" 7)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetPosition -win $_nWave2 {("G14" 8)}
wvSetPosition -win $_nWave2 {("G14" 9)}
wvSetPosition -win $_nWave2 {("G15" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 1)}
wvSetPosition -win $_nWave2 {("G15" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 33052.998276 -snap {("G14" 3)}
wvSelectSignal -win $_nWave2 {( "G15" 1 )} 
wvExpandBus -win $_nWave2 {("G15" 1)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G15" 32 )} 
wvSelectSignal -win $_nWave2 {( "G15" 33 )} 
wvSelectSignal -win $_nWave2 {( "G15" 18 19 20 21 22 23 24 25 26 27 28 29 30 31 \
           32 33 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 17)}
wvSelectSignal -win $_nWave2 {( "G15" 15 )} 
wvSelectSignal -win $_nWave2 {( "G15" 5 6 7 8 9 10 11 12 13 14 15 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 6)}
wvSelectSignal -win $_nWave2 {( "G15" 3 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G16" 0)}
wvSetPosition -win $_nWave2 {("G15" 5)}
wvSelectSignal -win $_nWave2 {( "G15" 2 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G16" 0)}
wvSetPosition -win $_nWave2 {("G15" 4)}
wvSelectSignal -win $_nWave2 {( "G15" 1 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G16" 0)}
wvSetPosition -win $_nWave2 {("G15" 3)}
wvSetCursor -win $_nWave2 15136.419771 -snap {("G14" 3)}
wvSetCursor -win $_nWave2 33670.811328 -snap {("G14" 3)}
wvSetCursor -win $_nWave2 54058.642040 -snap {("G15" 3)}
wvSetCursor -win $_nWave2 64870.370448 -snap {("G15" 2)}
wvSetCursor -win $_nWave2 56529.894247 -snap {("G15" 3)}
wvSetCursor -win $_nWave2 65797.090026 -snap {("G15" 2)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G12" 1 )} 
wvSelectSignal -win $_nWave2 {( "G12" 1 2 3 4 5 6 7 8 9 10 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 3)}
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 54985.361618 -snap {("G11" 4)}
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
srcDeselectAll -win $_nTrace1
debReload
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 44791.446262 -snap {("G14" 3)}
wvSetCursor -win $_nWave2 35215.343957 -snap {("G14" 3)}
wvSetCursor -win $_nWave2 32744.091750 -snap {("G14" 3)}
wvSetCursor -win $_nWave2 32744.091750 -snap {("G14" 2)}
wvSetCursor -win $_nWave2 35833.157009 -snap {("G14" 1)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G1" 1 )} 
wvAddSignal -win $_nWave2 "/tb_ooocpu/u_ooocpu/clk"
wvSetPosition -win $_nWave2 {("G15" 3)}
wvSetPosition -win $_nWave2 {("G15" 4)}
wvSetPosition -win $_nWave2 {("G15" 3)}
wvSetPosition -win $_nWave2 {("G15" 2)}
wvSetPosition -win $_nWave2 {("G15" 1)}
wvSetPosition -win $_nWave2 {("G15" 0)}
wvSetPosition -win $_nWave2 {("G14" 8)}
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvSetPosition -win $_nWave2 {("G14" 5)}
wvSetPosition -win $_nWave2 {("G14" 4)}
wvSetPosition -win $_nWave2 {("G14" 3)}
wvSetPosition -win $_nWave2 {("G14" 2)}
wvSetPosition -win $_nWave2 {("G14" 1)}
wvSetPosition -win $_nWave2 {("G14" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 0)}
wvSetPosition -win $_nWave2 {("G14" 1)}
wvSetCursor -win $_nWave2 15445.326297 -snap {("G14" 4)}
wvSetCursor -win $_nWave2 24712.522075 -snap {("G14" 1)}
wvSetCursor -win $_nWave2 36142.063535 -snap {("G14" 1)}
wvSetCursor -win $_nWave2 46027.072365 -snap {("G14" 1)}
wvSelectGroup -win $_nWave2 {G13}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
verdiDockWidgetSetCurTab -dock windowDock_nSchema_4
schZoom {-20180} {-32649} {232184} {61544} -win $_nSchema4
schSetOptions -win $_nSchema4 -portName on
schSetOptions -win $_nSchema4 -pinName on
schSetOptions -win $_nSchema4 -localNetName on
schZoom {120766} {-7410} {162403} {45646} -win $_nSchema4
schZoom {122813} {17426} {139860} {43284} -win $_nSchema4
schZoom {126731} {25018} {138182} {42599} -win $_nSchema4
schZoom {127061} {33649} {135735} {39890} -win $_nSchema4
schZoomOut -win $_nSchema4 -pos 131916 1489
schZoomOut -win $_nSchema4 -pos 131916 1498
schZoomOut -win $_nSchema4 -pos 131927 1498
schZoomOut -win $_nSchema4 -pos 131926 1498
schZoomOut -win $_nSchema4 -pos 131945 1498
schZoomOut -win $_nSchema4 -pos 131944 1544
schZoomOut -win $_nSchema4 -pos 131945 1600
schZoomOut -win $_nSchema4 -pos 132017 1851
schZoomOut -win $_nSchema4 -pos 132016 2343
schZoomOut -win $_nSchema4 -pos 131289 15129
schZoomOut -win $_nSchema4 -pos 131289 15128
schZoomOut -win $_nSchema4 -pos 131289 15128
schZoomOut -win $_nSchema4 -pos 131289 15129
schSelectAll -win $_nSchema4
schChangeDisplayAttr -color ID_GRAY4
schChangeDisplayAttr -color ID_PURPLE8
schChangeDisplayAttr -color ID_GRAY5
schDeselectAll -win $_nSchema4
schZoom {-15382} {-14779} {72839} {22093} -win $_nSchema4
schZoomOut -win $_nSchema4 -pos 42460 1461
schZoomOut -win $_nSchema4 -pos 42459 1460
schZoomOut -win $_nSchema4 -pos 42459 1460
schZoomOut -win $_nSchema4 -pos 42460 1460
schZoomOut -win $_nSchema4 -pos 42460 1460
schZoomOut -win $_nSchema4 -pos 42460 1459
schZoomIn -win $_nSchema4 -pos 71233 -22237
schZoomIn -win $_nSchema4 -pos 71233 -22237
wvZoom -win $_nWave2 0.000000 173914.374105
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 65728.789830 -snap {("G9" 3)}
wvZoom -win $_nWave2 0.000000 86415.007949
wvZoomOut -win $_nWave2
wvSetCursor -win $_nWave2 35063.355743 -snap {("G10" 2)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoom -win $_nWave2 0.000000 177826.848921
wvZoomOut -win $_nWave2
wvSetCursor -win $_nWave2 45032.410662 -snap {("G14" 4)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 35138.926501 -snap {("G14" 2)}
wvSetCursor -win $_nWave2 44520.678723 -snap {("G14" 2)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 39915.091269 -snap {("G9" 5)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_rs1_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 35000 -TraceValue \
           xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value   \[issue_tag\]" -line 119 -pos 1 -win \
          $_nTrace1
srcAction -pos 118 6 14 -win $_nTrace1 -name \
          "issue_queue_rs1_value   \[issue_tag\]" -ctrlKey off
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
wvSelectSignal -win $_nWave2 {( "G14" 2 )} 
wvSelectSignal -win $_nWave2 {( "G14" 2 )} 
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_en\[j+1\]" -line 317 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_en\[j\]" -line 319 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_en\[j\]" -line 319 -pos 1 -win $_nTrace1
srcAction -pos 318 8 10 -win $_nTrace1 -name "writeback_rs1_en\[j\]" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_in_writers1" -line 155 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_in_writers1" -line 155 -pos 1 -win $_nTrace1
srcAction -pos 154 30 7 -win $_nTrace1 -name "wb_alu_in_writers1" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 140 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 140 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 140 -pos 1 -win $_nTrace1
srcAction -pos 139 11 5 -win $_nTrace1 -name "rs1_Paddr" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 140 -pos 1 -win $_nTrace1
srcSearchString "rs1_Paddr" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {17 17 20 21 1 1}
srcSearchString "rs1_Paddr" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {241 241 13 14 1 1}
nsMsgSwitchTab -tab general
srcSearchString "rs1_Paddr" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {220 220 6 7 1 1}
srcSearchString "rs1_Paddr" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {213 213 6 7 1 1}
srcSearchString "rs1_Paddr" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {148 148 12 13 1 1}
srcSearchString "rs1_Paddr" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {146 146 11 12 1 1}
srcSearchString "rs1_Paddr" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {144 144 12 13 1 1}
srcSearchString "rs1_Paddr" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {142 142 12 13 1 1}
srcSearchString "rs1_Paddr" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {140 140 12 13 1 1}
srcSearchString "rs1_Paddr" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {17 17 20 21 1 1}
srcShowCalling -win $_nTrace1
srcSelect -win $_nTrace1 -range {168 168 2 3 1 1}
srcHBSelect "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 179 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -line 179 -pos 2 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 112069.294716 -snap {("G13" 0)}
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvUnknownSaveResult -win $_nWave2 -clear
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 64989.956296 -snap {("G11" 3)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 68401.502559 -snap {("G11" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_en" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 0 -TraceValue 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 127 -pos 1 -win $_nTrace1
srcAction -pos 126 6 8 -win $_nTrace1 -name "fifo_issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 125 -pos 1 -win $_nTrace1
srcAction -pos 124 11 5 -win $_nTrace1 -name "issue_en" -ctrlKey off
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 65842.842862 -snap {("G11" 5)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAction -pos 104 7 15 -win $_nTrace1 -name "issue_queue_ready\[0\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs2_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAction -pos 336 8 18 -win $_nTrace1 -name "issue_queue_Prs1_valid\[j\]" \
          -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 276 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 276 -pos 1 -win $_nTrace1
srcAction -pos 275 5 6 -win $_nTrace1 -name "rs1_rat_valid" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_rs1_rat_valid" -line 116 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_fromROB_valid" -line 116 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_fu_top" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_out" -line 44 -pos 1 -win $_nTrace1
srcSelect -signal "alu_out_valid" -line 45 -pos 1 -win $_nTrace1
srcSelect -signal "alu_dst_Paddr" -line 46 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetPosition -win $_nWave2 {("G14" 20)}
wvSetPosition -win $_nWave2 {("G14" 19)}
wvSetPosition -win $_nWave2 {("G14" 18)}
wvSetPosition -win $_nWave2 {("G14" 17)}
wvSetPosition -win $_nWave2 {("G14" 16)}
wvSetPosition -win $_nWave2 {("G14" 15)}
wvSetPosition -win $_nWave2 {("G14" 14)}
wvSetPosition -win $_nWave2 {("G14" 13)}
wvSetPosition -win $_nWave2 {("G14" 12)}
wvSetPosition -win $_nWave2 {("G14" 11)}
wvSetPosition -win $_nWave2 {("G14" 10)}
wvSetPosition -win $_nWave2 {("G14" 9)}
wvSetPosition -win $_nWave2 {("G14" 8)}
wvSetPosition -win $_nWave2 {("G14" 7)}
wvSetPosition -win $_nWave2 {("G14" 6)}
wvSetPosition -win $_nWave2 {("G14" 5)}
wvSetPosition -win $_nWave2 {("G14" 4)}
wvSetPosition -win $_nWave2 {("G14" 3)}
wvSetPosition -win $_nWave2 {("G14" 2)}
wvSetPosition -win $_nWave2 {("G14" 1)}
wvSetPosition -win $_nWave2 {("G14" 0)}
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 0)}
wvSetPosition -win $_nWave2 {("G12" 3)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G12" 1 2 3 )} 
wvSetRadix -win $_nWave2 -format UDec
wvSelectGroup -win $_nWave2 {G13}
wvSelectSignal -win $_nWave2 {( "G12" 1 )} 
wvSelectGroup -win $_nWave2 {G13}
wvSetCursor -win $_nWave2 55608.204075 -snap {("G12" 1)}
wvSetCursor -win $_nWave2 65160.533609 -snap {("G12" 1)}
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G11" 3 )} 
wvSetCursor -win $_nWave2 54925.894823 -snap {("G12" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G14" 1 )} 
wvSelectSignal -win $_nWave2 {( "G14" 2 )} 
wvScrollDown -win $_nWave2 10
wvSelectSignal -win $_nWave2 {( "G14" 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 \
           18 19 20 21 22 23 24 25 )} {( "G15" 1 2 3 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 3)}
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcHBSelect "tb_ooocpu.u_spm" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_rob" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_rob" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_rob" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_rob" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_fromROB_valid" -line 76 -pos 1 -win $_nTrace1
srcSelect -signal "rs1_value_fromROB" -line 78 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G12" 5 )} 
wvSelectSignal -win $_nWave2 {( "G12" 5 )} 
wvSetRadix -win $_nWave2 -format UDec
wvSelectGroup -win $_nWave2 {G13}
wvSetCursor -win $_nWave2 65672.265549 -snap {("G12" 5)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 54584.740196 -snap {("G2" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_value_fromROB" -line 78 -pos 1 -win $_nTrace1
srcAction -pos 77 19 8 -win $_nTrace1 -name "rs1_value_fromROB" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 215 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 215 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_dst_value_ready\[rs1_Paddr\]" -line 215 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_dst_value_ready\[rs1_Paddr\]" -line 215 -pos 1 -win \
          $_nTrace1
srcAction -pos 214 12 12 -win $_nTrace1 -name "rob_dst_value_ready\[rs1_Paddr\]" \
          -ctrlKey off
wvSetCursor -win $_nWave2 65160.533609 -snap {("G12" 7)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectGroup -win $_nWave2 {G13}
wvSetCursor -win $_nWave2 71130.739568 -snap {("G11" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_en" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 0 -TraceValue 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 127 -pos 1 -win $_nTrace1
srcAction -pos 126 6 6 -win $_nTrace1 -name "fifo_issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 125 -pos 1 -win $_nTrace1
srcAction -pos 124 11 5 -win $_nTrace1 -name "issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAction -pos 104 7 14 -win $_nTrace1 -name "issue_queue_ready\[0\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs2_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G12" 9 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 8)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAction -pos 336 8 19 -win $_nTrace1 -name "issue_queue_Prs1_valid\[j\]" \
          -ctrlKey off
wvSetCursor -win $_nWave2 70789.584942 -snap {("G12" 8)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.issue_queue_Prs1_valid\[0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 65000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 276 -pos 1 -win $_nTrace1
wvSelectSignal -win $_nWave2 {( "G12" 6 )} 
wvSelectGroup -win $_nWave2 {G13}
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 276 -pos 1 -win $_nTrace1
srcSearchString "rs1_rat_valid" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {161 161 17 18 1 1}
srcSearchString "rs1_rat_valid" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {159 159 16 17 1 1}
srcSearchString "rs1_rat_valid" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {157 157 17 18 1 1}
srcSearchString "rs1_rat_valid" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {155 155 17 18 1 1}
srcSearchString "rs1_rat_valid" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {153 153 17 18 1 1}
srcSearchString "rs1_rat_valid" -win $_nTrace1 -prev -case
srcSelect -win $_nTrace1 -range {16 16 6 7 1 1}
srcShowCalling -win $_nTrace1
srcSelect -win $_nTrace1 -range {267 267 25 26 1 1}
srcHBSelect "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO" \
           -win $_nTrace1
srcShowCalling -win $_nTrace1
srcSelect -win $_nTrace1 -range {215 215 2 3 1 1}
srcHBSelect "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 225 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr_valid" -line 225 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G12" 6 )} 
wvSetCursor -win $_nWave2 61237.255408 -snap {("G12" 6)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_rob.rs1_rat_valid" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 45000 -TraceValue 1
wvSelectSignal -win $_nWave2 {( "G12" 6 )} 
wvSelectSignal -win $_nWave2 {( "G12" 6 )} 
wvSelectSignal -win $_nWave2 {( "G12" 6 )} 
wvSelectSignal -win $_nWave2 {( "G12" 9 )} 
wvSetCursor -win $_nWave2 32239.112178 -snap {("G12" 9)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.rs1_rat_valid" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 25000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_rs1_rat_valid" -line 116 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_fromROB_valid" -line 116 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 63113.605852 -snap {("G12" 9)}
wvSetCursor -win $_nWave2 69595.543750 -snap {("G11" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_en" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 0 -TraceValue 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 125 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 125 -pos 1 -win $_nTrace1
srcAction -pos 124 11 4 -win $_nTrace1 -name "issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 105 -pos 1 -win $_nTrace1
srcAction -pos 104 7 13 -win $_nTrace1 -name "issue_queue_ready\[0\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 337 -pos 1 -win $_nTrace1
srcAction -pos 336 8 16 -win $_nTrace1 -name "issue_queue_Prs1_valid\[j\]" \
          -ctrlKey off
wvSetCursor -win $_nWave2 69766.121063 -snap {("G12" 13)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.issue_queue_Prs1_valid\[0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 65000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 276 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 70960.162255 -snap {("G12" 13)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_write_en" -line 275 -pos 1 -win $_nTrace1
srcAction -pos 274 8 7 -win $_nTrace1 -name "fifo_write_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue2queue_en" -line 124 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_full" -line 124 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_write_en" -line 124 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 70277.853003 -snap {("G12" 13)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.issue_queue_Prs1_valid\[0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 65000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_p\[j\]" -line 275 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 60384.368842 -snap {("G12" 17)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.fifo_write_en" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue2queue_en" -line 124 -pos 1 -win $_nTrace1
srcAction -pos 123 11 7 -win $_nTrace1 -name "issue2queue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "mul_div_insn_en" -line 157 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "mul_div_insn_en" -line 157 -pos 1 -win $_nTrace1
srcAction -pos 156 11 2 -win $_nTrace1 -name "mul_div_insn_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_alu_op" -line 143 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr_valid" -line 116 -pos 1 -win $_nTrace1
srcSelect -win $_nTrace1 -range {116 117 3 3 14 11}
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr_valid" -line 116 -pos 1 -win $_nTrace1
srcSelect -signal "rs2_Paddr_valid" -line 117 -pos 1 -win $_nTrace1
srcSelect -signal "rs1_value_write" -line 118 -pos 1 -win $_nTrace1
srcSelect -signal "rs2_value_write" -line 119 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G12" 21 22 23 24 )} 
wvSelectSignal -win $_nWave2 {( "G12" 24 )} 
wvSelectSignal -win $_nWave2 {( "G12" 21 )} 
wvSelectSignal -win $_nWave2 {( "G12" 20 )} 
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 69595.543750 -snap {("G11" 4)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_queue_rs2_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 65000 -TraceValue \
           00000000000000000000000000010100
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value   \[issue_tag\]" -line 132 -pos 1 -win \
          $_nTrace1
srcAction -pos 131 6 17 -win $_nTrace1 -name \
          "issue_queue_rs1_value   \[issue_tag\]" -ctrlKey off
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 69936.698377 -snap {("G11" 4)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_queue_rs2_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 65000 -TraceValue \
           00000000000000000000000000010100
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs2_value   \[issue_tag\]" -line 133 -pos 1 -win \
          $_nTrace1
srcAction -pos 132 6 14 -win $_nTrace1 -name \
          "issue_queue_rs2_value   \[issue_tag\]" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs2_data\[j\]" -line 306 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs2_value\[j\]" -line 306 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs2_valid\[j\]" -line 305 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_data\[j\]" -line 271 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_data\[j+1\]" -line 274 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_data\[j\]" -line 271 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_data\[j\]" -line 271 -pos 1 -win $_nTrace1
srcAction -pos 270 5 13 -win $_nTrace1 -name "writeback_rs1_data\[j\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 178 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1\[i\]" -line 178 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[i\]" -line 178 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 67378.038680 -snap {("G11" 3)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_queue_rs1_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 65000 -TraceValue \
           00000000000000000000000000000000
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value   \[issue_tag\]" -line 132 -pos 1 -win \
          $_nTrace1
srcAction -pos 131 6 19 -win $_nTrace1 -name \
          "issue_queue_rs1_value   \[issue_tag\]" -ctrlKey off
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G12" 32 )} 
wvScrollUp -win $_nWave2 18
wvSelectSignal -win $_nWave2 {( "G12" 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 \
           20 21 22 23 24 25 26 27 28 29 30 31 32 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 3)}
srcDeselectAll -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_rob" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_rob" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_rob" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 23 -pos 1 -win $_nTrace1
srcSelect -signal "wb_alu_out" -line 24 -pos 1 -win $_nTrace1
srcSelect -signal "wb_alu_valid" -line 25 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G12" 5 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 5)}
wvSelectSignal -win $_nWave2 {( "G12" 5 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 4)}
wvSelectSignal -win $_nWave2 {( "G12" 4 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 3)}
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_pc" -line 97 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_dst_addr" -line 98 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G12" 4 )} 
wvScrollDown -win $_nWave2 2
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_dst_wen" -line 99 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G12" 6 )} 
wvExpandBus -win $_nWave2 {("G12" 6)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 6)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_rs1_addr" -line 100 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_dst_value" -line 103 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_dst_value_ready" -line 104 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G12" 5 )} 
wvSelectSignal -win $_nWave2 {( "G12" 5 )} 
wvSelectSignal -win $_nWave2 {( "G12" 5 )} 
wvSelectSignal -win $_nWave2 {( "G12" 5 )} 
wvSetPosition -win $_nWave2 {("G12" 5)}
wvExpandBus -win $_nWave2 {("G12" 5)}
wvSetPosition -win $_nWave2 {("G12" 40)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G12" 10 )} 
wvScrollDown -win $_nWave2 24
wvSelectSignal -win $_nWave2 {( "G12" 10 11 12 13 14 15 16 17 18 19 20 21 22 23 \
           24 25 26 27 28 29 30 31 32 33 34 35 36 37 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 12)}
wvSelectSignal -win $_nWave2 {( "G12" 10 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 11)}
wvSelectSignal -win $_nWave2 {( "G12" 10 )} 
wvSelectSignal -win $_nWave2 {( "G12" 10 )} 
wvSetPosition -win $_nWave2 {("G12" 10)}
wvExpandBus -win $_nWave2 {("G12" 10)}
wvSetPosition -win $_nWave2 {("G12" 43)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G12" 15 )} 
wvScrollDown -win $_nWave2 12
wvSelectSignal -win $_nWave2 {( "G12" 15 16 17 18 19 20 21 22 23 24 25 26 27 28 \
           29 30 31 32 33 34 35 36 37 38 39 40 41 42 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 15)}
wvSetCursor -win $_nWave2 34456.617249 -snap {("G12" 1)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G12" 1 )} 
wvSetCursor -win $_nWave2 34797.771875 -snap {("G12" 1)}
wvSelectSignal -win $_nWave2 {( "G12" 1 )} 
wvSetRadix -win $_nWave2 -format Hex
wvSelectSignal -win $_nWave2 {( "G12" 15 )} 
wvExpandBus -win $_nWave2 {("G12" 15)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 15)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs2_fromROB_valid" -line 225 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_fromROB_valid" -line 213 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_Paddr" -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 34627.194562 -snap {("G14" 0)}
wvSelectGroup -win $_nWave2 {G14}
wvSetCursor -win $_nWave2 34286.039936 -snap {("G12" 3)}
wvSetCursor -win $_nWave2 55608.204075 -snap {("G12" 3)}
wvSetCursor -win $_nWave2 45202.987975 -snap {("G12" 3)}
wvSetCursor -win $_nWave2 55778.781388 -snap {("G12" 3)}
wvSetCursor -win $_nWave2 45373.565288 -snap {("G12" 3)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_fromROB_valid" -line 216 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rob_dst_value_ready\[rs1_Paddr\]" -line 215 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "rs1_rat_valid" -line 215 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G12" 19 )} 
wvSelectSignal -win $_nWave2 {( "G12" 19 )} 
wvSelectSignal -win $_nWave2 {( "G12" 18 )} 
wvSelectSignal -win $_nWave2 {( "G12" 18 )} 
wvSetPosition -win $_nWave2 {("G12" 18)}
wvExpandBus -win $_nWave2 {("G12" 18)}
wvSetPosition -win $_nWave2 {("G12" 51)}
wvScrollDown -win $_nWave2 6
wvSelectSignal -win $_nWave2 {( "G12" 50 )} 
wvScrollUp -win $_nWave2 7
wvScrollUp -win $_nWave2 11
wvSelectSignal -win $_nWave2 {( "G12" 22 23 24 25 26 27 28 29 30 31 32 33 34 35 \
           36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 22)}
wvSelectSignal -win $_nWave2 {( "G12" 20 )} 
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G12" 17 )} 
wvSetPosition -win $_nWave2 {("G12" 17)}
wvSetPosition -win $_nWave2 {("G12" 18)}
wvSetPosition -win $_nWave2 {("G12" 19)}
wvSetPosition -win $_nWave2 {("G12" 20)}
wvSetPosition -win $_nWave2 {("G12" 21)}
wvSetPosition -win $_nWave2 {("G12" 22)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 22)}
wvSelectSignal -win $_nWave2 {( "G12" 19 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 21)}
wvSelectSignal -win $_nWave2 {( "G12" 18 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 20)}
wvSetCursor -win $_nWave2 54584.740196 -snap {("G12" 12)}
wvSetCursor -win $_nWave2 55096.472136 -snap {("G12" 13)}
wvSetCursor -win $_nWave2 64307.647044 -snap {("G12" 20)}
wvSetCursor -win $_nWave2 45714.719914 -snap {("G12" 19)}
wvSelectSignal -win $_nWave2 {( "G12" 18 )} 
wvSelectSignal -win $_nWave2 {( "G12" 19 )} 
wvSetPosition -win $_nWave2 {("G12" 19)}
wvSetPosition -win $_nWave2 {("G12" 18)}
wvSetPosition -win $_nWave2 {("G12" 17)}
wvSetPosition -win $_nWave2 {("G12" 16)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 16)}
wvSetPosition -win $_nWave2 {("G12" 17)}
wvSelectSignal -win $_nWave2 {( "G12" 18 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 17)}
wvSetCursor -win $_nWave2 44350.101410 -snap {("G12" 16)}
wvSetCursor -win $_nWave2 65160.533609 -snap {("G12" 19)}
wvSetCursor -win $_nWave2 44691.256036 -snap {("G12" 17)}
wvSetCursor -win $_nWave2 56290.513328 -snap {("G12" 18)}
wvSetCursor -win $_nWave2 45202.987975 -snap {("G12" 16)}
srcHBSelect "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 30 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_out" -line 31 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcHBSelect "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO" \
           -win $_nTrace1
srcSetScope -win $_nTrace1 \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO" \
           -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO" \
           -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_en\[i\]" -line 168 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G12" 20 )} 
wvExpandBus -win $_nWave2 {("G12" 20)}
wvScrollDown -win $_nWave2 4
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 58337.441085 -snap {("G12" 21)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_mul_div_issue_queue_OoO.writeback_rs1_en\[0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 55000 -TraceValue x
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[i\]" -line 168 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 168 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1\[i\]" -line 168 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 65331.110923 -snap {("G11" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_gpr" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "gpr" -line 21 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetPosition -win $_nWave2 {("G12" 29)}
wvSetPosition -win $_nWave2 {("G12" 30)}
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G14" 0)}
wvSetPosition -win $_nWave2 {("G14" 1)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 2)}
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G14" 2 )} 
wvExpandBus -win $_nWave2 {("G14" 2)}
wvSetCursor -win $_nWave2 132538.572289 -snap {("G14" 22)}
wvSelectSignal -win $_nWave2 {( "G14" 18 )} 
wvScrollDown -win $_nWave2 2
wvScrollDown -win $_nWave2 0
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G14" 3 )} 
wvScrollDown -win $_nWave2 7
wvScrollUp -win $_nWave2 2
wvSelectSignal -win $_nWave2 {( "G14" 14 )} 
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G14" 6 7 8 9 10 11 12 13 14 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 25)}
wvSelectSignal -win $_nWave2 {( "G14" 14 )} 
wvScrollDown -win $_nWave2 4
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G14" 14 15 16 17 18 19 20 21 22 23 24 25 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 13)}
wvZoom -win $_nWave2 0.000000 425132.494005
wvScrollDown -win $_nWave2 0
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G14" 3 )} 
wvSelectSignal -win $_nWave2 {( "G14" 3 4 5 6 7 8 9 10 11 12 13 )} 
wvSelectSignal -win $_nWave2 {( "G14" 3 4 5 6 7 8 9 10 11 12 13 )} 
wvSetRadix -win $_nWave2 -format UDec
wvSelectGroup -win $_nWave2 {G16}
wvSetCursor -win $_nWave2 49558.302158 -snap {("G14" 5)}
wvSetCursor -win $_nWave2 50044.167866 -snap {("G14" 5)}
wvSetCursor -win $_nWave2 186086.565947 -snap {("G14" 10)}
wvSetCursor -win $_nWave2 46157.242206 -snap {("G14" 5)}
wvSetCursor -win $_nWave2 63648.407674 -snap {("G14" 9)}
wvSetCursor -win $_nWave2 73851.587530 -snap {("G14" 8)}
wvSetCursor -win $_nWave2 184628.968825 -snap {("G14" 10)}
wvSetCursor -win $_nWave2 292491.155875 -snap {("G14" 9)}
wvSetCursor -win $_nWave2 305609.529976 -snap {("G14" 8)}
wvSetCursor -win $_nWave2 334761.472422 -snap {("G14" 12)}
wvSelectSignal -win $_nWave2 {( "G14" 12 )} 
wvSelectSignal -win $_nWave2 {( "G14" 12 )} 
wvSetRadix -win $_nWave2 -format Hex
wvSelectSignal -win $_nWave2 {( "G14" 4 )} 
wvSelectSignal -win $_nWave2 {( "G14" 5 )} 
wvSelectSignal -win $_nWave2 {( "G14" 5 )} 
wvSetRadix -win $_nWave2 -format Hex
wvSelectGroup -win $_nWave2 {G16}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 297349.812950 -snap {("G14" 9)}
wvSetCursor -win $_nWave2 305609.529976 -snap {("G14" 8)}
wvSetCursor -win $_nWave2 333789.741007 -snap {("G14" 12)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvZoom -win $_nWave2 120008.829736 166166.071942
wvSetCursor -win $_nWave2 144980.229836 -snap {("G12" 2)}
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 150182.604857 -snap {("G9" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_en" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 145000 -TraceValue x
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 114 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 114 -pos 1 -win $_nTrace1
srcAction -pos 113 6 10 -win $_nTrace1 -name "fifo_issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_empty" -line 113 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 113 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 113 -pos 1 -win $_nTrace1
srcAction -pos 112 11 3 -win $_nTrace1 -name "issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 102 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[0\]" -line 102 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[1\]" -line 103 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[1\]" -line 103 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[2\]" -line 104 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[2\]" -line 104 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[2\]" -line 104 -pos 1 -win $_nTrace1
srcAction -pos 103 2 15 -win $_nTrace1 -name "issue_queue_ready\[2\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 366 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs2_valid\[j\]" -line 366 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 139556.477155 -snap {("G9" 2)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_op\[4:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 135000 -TraceValue 01111
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_p" -line 92 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_p" -line 88 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G14" 24)}
wvSetPosition -win $_nWave2 {("G14" 23)}
wvSetPosition -win $_nWave2 {("G14" 22)}
wvSetPosition -win $_nWave2 {("G14" 21)}
wvSetPosition -win $_nWave2 {("G14" 20)}
wvSetPosition -win $_nWave2 {("G14" 19)}
wvSetPosition -win $_nWave2 {("G14" 18)}
wvSetPosition -win $_nWave2 {("G14" 17)}
wvSetPosition -win $_nWave2 {("G14" 16)}
wvSetPosition -win $_nWave2 {("G14" 15)}
wvSetPosition -win $_nWave2 {("G14" 14)}
wvSetPosition -win $_nWave2 {("G14" 13)}
wvSetPosition -win $_nWave2 {("G14" 12)}
wvSetPosition -win $_nWave2 {("G14" 11)}
wvSetPosition -win $_nWave2 {("G14" 10)}
wvSetPosition -win $_nWave2 {("G14" 0)}
wvSetPosition -win $_nWave2 {("G13" 0)}
wvSetPosition -win $_nWave2 {("G12" 29)}
wvSetPosition -win $_nWave2 {("G12" 28)}
wvSetPosition -win $_nWave2 {("G12" 27)}
wvSetPosition -win $_nWave2 {("G12" 26)}
wvSetPosition -win $_nWave2 {("G12" 25)}
wvSetPosition -win $_nWave2 {("G12" 24)}
wvSetPosition -win $_nWave2 {("G12" 23)}
wvSetPosition -win $_nWave2 {("G12" 22)}
wvSetPosition -win $_nWave2 {("G12" 21)}
wvSetPosition -win $_nWave2 {("G12" 20)}
wvSetPosition -win $_nWave2 {("G12" 19)}
wvSetPosition -win $_nWave2 {("G12" 18)}
wvSetPosition -win $_nWave2 {("G12" 17)}
wvSetPosition -win $_nWave2 {("G12" 16)}
wvSetPosition -win $_nWave2 {("G12" 15)}
wvSetPosition -win $_nWave2 {("G12" 14)}
wvSetPosition -win $_nWave2 {("G12" 13)}
wvSetPosition -win $_nWave2 {("G12" 12)}
wvSetPosition -win $_nWave2 {("G12" 11)}
wvSetPosition -win $_nWave2 {("G12" 10)}
wvSetPosition -win $_nWave2 {("G12" 9)}
wvSetPosition -win $_nWave2 {("G12" 8)}
wvSetPosition -win $_nWave2 {("G12" 7)}
wvSetPosition -win $_nWave2 {("G12" 6)}
wvSetPosition -win $_nWave2 {("G12" 5)}
wvSetPosition -win $_nWave2 {("G12" 4)}
wvSetPosition -win $_nWave2 {("G12" 3)}
wvSetPosition -win $_nWave2 {("G12" 2)}
wvSetPosition -win $_nWave2 {("G12" 1)}
wvSetPosition -win $_nWave2 {("G12" 0)}
wvSetPosition -win $_nWave2 {("G11" 5)}
wvSetPosition -win $_nWave2 {("G12" 0)}
wvSetPosition -win $_nWave2 {("G12" 1)}
wvSetPosition -win $_nWave2 {("G12" 2)}
wvSetPosition -win $_nWave2 {("G12" 3)}
wvSetPosition -win $_nWave2 {("G12" 4)}
wvSetPosition -win $_nWave2 {("G12" 5)}
wvSetPosition -win $_nWave2 {("G12" 6)}
wvSetPosition -win $_nWave2 {("G12" 7)}
wvSetPosition -win $_nWave2 {("G12" 8)}
wvSetPosition -win $_nWave2 {("G12" 9)}
wvSetPosition -win $_nWave2 {("G12" 8)}
wvSetPosition -win $_nWave2 {("G12" 7)}
wvSetPosition -win $_nWave2 {("G12" 6)}
wvSetPosition -win $_nWave2 {("G12" 5)}
wvSetPosition -win $_nWave2 {("G12" 4)}
wvSetPosition -win $_nWave2 {("G12" 3)}
wvSetPosition -win $_nWave2 {("G12" 2)}
wvSetPosition -win $_nWave2 {("G12" 1)}
wvSetPosition -win $_nWave2 {("G12" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G12" 0)}
wvSetPosition -win $_nWave2 {("G12" 1)}
wvScrollUp -win $_nWave2 6
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetPosition -win $_nWave2 {("G12" 0)}
wvSetPosition -win $_nWave2 {("G11" 5)}
wvSetPosition -win $_nWave2 {("G11" 4)}
wvSetPosition -win $_nWave2 {("G11" 3)}
wvSetPosition -win $_nWave2 {("G11" 2)}
wvSetPosition -win $_nWave2 {("G11" 1)}
wvSetPosition -win $_nWave2 {("G11" 0)}
wvSetPosition -win $_nWave2 {("G10" 3)}
wvSetPosition -win $_nWave2 {("G10" 2)}
wvSetPosition -win $_nWave2 {("G10" 1)}
wvSetPosition -win $_nWave2 {("G10" 0)}
wvSetPosition -win $_nWave2 {("G9" 7)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 7)}
wvSetPosition -win $_nWave2 {("G9" 8)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G9" 8 )} 
wvExpandBus -win $_nWave2 {("G9" 8)}
wvSelectSignal -win $_nWave2 {( "G10" 1 )} 
wvSetCursor -win $_nWave2 24085.889458 -snap {("G9" 13)}
wvSetCursor -win $_nWave2 31878.383106 -snap {("G9" 12)}
wvSetCursor -win $_nWave2 45692.349119 -snap {("G2" 7)}
wvSetCursor -win $_nWave2 64465.174726 -snap {("G9" 1)}
wvSetCursor -win $_nWave2 85717.430131 -snap {("G9" 1)}
wvSetCursor -win $_nWave2 125388.306885 -snap {("G9" 3)}
wvSetCursor -win $_nWave2 136014.434588 -snap {("G9" 12)}
wvSetCursor -win $_nWave2 144869.541006 -snap {("G9" 2)}
wvSetCursor -win $_nWave2 150182.604857 -snap {("G9" 2)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_op\[4:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectGroup -win $_nWave2 {G11}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G11" 1 )} 
wvScrollDown -win $_nWave2 6
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G12" 1 )} 
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G12" 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 \
           18 19 20 21 22 23 24 25 26 27 28 29 )} {( "G14" 1 2 3 4 5 6 7 8 9 \
           10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 13)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
srcDeselectAll -win $_nTrace1
debReload
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 132826.596277 -snap {("G9" 2)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 125742.511142 -snap {("G9" 12)}
wvSetCursor -win $_nWave2 135660.230331 -snap {("G9" 12)}
wvSetCursor -win $_nWave2 146286.358033 -snap {("G9" 5)}
wvSetCursor -win $_nWave2 153370.443168 -snap {("G9" 2)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_op\[4:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_tag" -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_tag" -win $_nTrace1
srcAction -pos 115 6 29 -win $_nTrace1 -name \
          "issue_queue_op          \[issue_tag\]" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 107 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[0\]" -line 107 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[1\]" -line 108 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[2\]" -line 109 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[3\]" -line 110 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 107 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 107 -pos 1 -win $_nTrace1
srcAction -pos 106 7 9 -win $_nTrace1 -name "issue_queue_ready\[0\]" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[j\]" -line 323 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs2_valid\[j\]" -line 323 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 150891.013371 -snap {("G9" 14)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_tag\[1:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue 0x
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 107 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[0\]" -line 107 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_tag" -line 107 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 152307.830398 -snap {("G9" 24)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_tag\[1:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue 0x
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[1\]" -line 108 -pos 1 -win $_nTrace1
wvSelectSignal -win $_nWave2 {( "G9" 24 )} 
wvExpandBus -win $_nWave2 {("G9" 24)}
wvScrollDown -win $_nWave2 4
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[1\]" -line 108 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[1\]" -line 108 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 102 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[1\]" -line 103 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 198233.208633 -snap {("G9" 5)}
wvSetCursor -win $_nWave2 161307.414868 -snap {("G9" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_en" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 135000 -TraceValue 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G9" 3 )} 
wvSetCursor -win $_nWave2 184628.968825 -snap {("G9" 5)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 175883.386091 -snap {("G11" 4)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 179770.311751 -snap {("G9" 5)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_rs1_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 175000 -TraceValue \
           00000000000000000000000000xx00x0
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value   \[issue_tag\]" -line 119 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value   \[issue_tag\]" -line 119 -pos 1 -win \
          $_nTrace1
srcAction -pos 118 6 16 -win $_nTrace1 -name \
          "issue_queue_rs1_value   \[issue_tag\]" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_data\[j\]" -line 257 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_data\[j\]" -line 257 -pos 1 -win $_nTrace1
srcAction -pos 256 5 9 -win $_nTrace1 -name "writeback_rs1_data\[j\]" -ctrlKey \
          off
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 165 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_div_dst_Paddr" -line 166 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1\[i\]" -line 166 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[i\]" -line 166 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_div_out" -line 166 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_div_valid" -line 166 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_data\[i\]" -line 165 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvZoom -win $_nWave2 88913.424460 872614.810552
wvZoomIn -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoom -win $_nWave2 13604.239808 387720.834532
wvZoom -win $_nWave2 60077.236467 218336.630494
wvSetCursor -win $_nWave2 171579.898254 -snap {("G9" 31)}
wvSelectSignal -win $_nWave2 {( "G9" 31 )} 
wvSelectSignal -win $_nWave2 {( "G9" 31 )} 
wvSetPosition -win $_nWave2 {("G9" 31)}
wvExpandBus -win $_nWave2 {("G9" 31)}
wvSetPosition -win $_nWave2 {("G9" 70)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 38)}
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 165 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 165 -pos 1 -win $_nTrace1
srcAction -pos 164 11 10 -win $_nTrace1 -name "wb_alu_dst_Paddr" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_Pdst" -line 94 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_Pdst" -line 94 -pos 1 -win $_nTrace1
srcAction -pos 93 6 14 -win $_nTrace1 -name "alu_issue_queue_Pdst" -ctrlKey off
wvSetCursor -win $_nWave2 115677.370023 -snap {("G9" 40)}
wvSetCursor -win $_nWave2 163344.947696 -snap {("G10" 0)}
wvSetCursor -win $_nWave2 168506.405151 -snap {("G9" 40)}
wvSetCursor -win $_nWave2 170328.096018 -snap {("G9" 40)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_Pdst\[4:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
wvSetCursor -win $_nWave2 170328.096018 -snap {("G9" 40)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_Pdst\[4:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
wvSetCursor -win $_nWave2 170328.096018 -snap {("G9" 40)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_Pdst\[4:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Pdst        \[issue_tag\]" -line 121 -pos 1 -win \
          $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Pdst        \[issue_tag\]" -line 121 -pos 1 -win \
          $_nTrace1
srcAction -pos 120 6 13 -win $_nTrace1 -name \
          "issue_queue_Pdst        \[issue_tag\]" -ctrlKey off
wvSetCursor -win $_nWave2 169113.635440 -snap {("G9" 31)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_data\[0\]\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 165000 -TraceValue \
           00000000000000000000000000xx00x0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 170024.480873 -snap {("G9" 31)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_data\[0\]\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 165000 -TraceValue \
           00000000000000000000000000xx00x0
wvSetCursor -win $_nWave2 170328.096018 -snap {("G9" 32)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.wb_alu_dst_Paddr\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
wvSetCursor -win $_nWave2 170328.096018 -snap {("G9" 32)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.wb_alu_dst_Paddr\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
wvSetCursor -win $_nWave2 170328.096018 -snap {("G9" 32)}
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_Pdst" -line 94 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_Pdst" -line 94 -pos 1 -win $_nTrace1
srcAction -pos 93 6 12 -win $_nTrace1 -name "alu_issue_queue_Pdst" -ctrlKey off
wvSetCursor -win $_nWave2 169720.865729 -snap {("G9" 32)}
wvSetCursor -win $_nWave2 169720.865729 -snap {("G9" 32)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.wb_alu_dst_Paddr\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
wvSetCursor -win $_nWave2 170024.480873 -snap {("G9" 31)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_data\[0\]\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 165000 -TraceValue \
           00000000000000000000000000xx00x0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 165 -pos 1 -win $_nTrace1
srcAction -pos 164 11 8 -win $_nTrace1 -name "wb_alu_dst_Paddr" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_Pdst" -line 94 -pos 1 -win $_nTrace1
srcAction -pos 93 6 11 -win $_nTrace1 -name "alu_issue_queue_Pdst" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs2_value   \[issue_tag\]" -line 120 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Pdst        \[issue_tag\]" -line 121 -pos 1 -win \
          $_nTrace1
wvSetCursor -win $_nWave2 168810.020296 -snap {("G9" 38)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_data\[0\]\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 165000 -TraceValue \
           00000000000000000000000000xx00x0
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1\[i\]" -line 166 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1\[i\]" -line 165 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 169720.865729 -snap {("G9" 39)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.wb_alu_dst_Paddr\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_Pdst" -line 94 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_Pdst" -line 94 -pos 1 -win $_nTrace1
srcAction -pos 93 6 12 -win $_nTrace1 -name "alu_issue_queue_Pdst" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Pdst        \[issue_tag\]" -line 121 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Pdst        \[issue_tag\]" -line 121 -pos 1 -win \
          $_nTrace1
srcAction -pos 120 6 9 -win $_nTrace1 -name \
          "issue_queue_Pdst        \[issue_tag\]" -ctrlKey off
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 169417.250585 -snap {("G9" 34)}
wvSetCursor -win $_nWave2 176704.014051 -snap {("G9" 37)}
wvSetCursor -win $_nWave2 172453.402029 -snap {("G9" 34)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_Prs1\[0\]\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 155000 -TraceValue xxxxx
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 174578.708040 -snap {("G9" 33)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 178525.704917 -snap {("G9" 3)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_pc\[31:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue \
           xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
wvSetCursor -win $_nWave2 150593.111631 -snap {("G9" 1)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_en" -win $_nTrace1 \
           -TraceByDConWave -TraceTime 135000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 114 -pos 1 -win $_nTrace1
srcAction -pos 113 6 8 -win $_nTrace1 -name "fifo_issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 113 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 113 -pos 1 -win $_nTrace1
srcAction -pos 112 11 4 -win $_nTrace1 -name "issue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 102 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[0\]" -line 102 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[1\]" -line 103 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[1\]" -line 103 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 145735.269320 -snap {("G9" 7)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G11" 5 )} 
wvScrollUp -win $_nWave2 35
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 4
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G9" 14 15 16 17 18 19 20 21 22 23 24 25 26 27 \
           28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 )} {( \
           "G10" 1 2 3 )} {( "G11" 1 2 3 4 5 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 13)}
wvSelectGroup -win $_nWave2 {G16}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 13)}
wvSelectGroup -win $_nWave2 {G12}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 13)}
wvSelectGroup -win $_nWave2 {G13}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 13)}
wvSelectGroup -win $_nWave2 {G14}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 13)}
wvSelectGroup -win $_nWave2 {G11}
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 13)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_en" -line 102 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[0\]" -line 102 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[0\]" -line 102 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[1\]" -line 103 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[1\]" -line 103 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_ready\[2\]" -line 104 -pos 1 -win $_nTrace1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 149075.035909 -snap {("G9" 11)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.fifo_p\[2\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 135000 -TraceValue 1
wvSetCursor -win $_nWave2 144824.423887 -snap {("G9" 7)}
wvSetCursor -win $_nWave2 154843.723653 -snap {("G9" 11)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.fifo_p\[2\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 135000 -TraceValue 1
wvSetCursor -win $_nWave2 180347.395784 -snap {("G9" 11)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.fifo_p\[2\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 135000 -TraceValue 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 202 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_write_en" -line 202 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_issue_en" -line 200 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_write_en" -line 202 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_write_en" -line 202 -pos 1 -win $_nTrace1
srcAction -pos 201 13 5 -win $_nTrace1 -name "fifo_write_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "fifo_full" -line 112 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue2queue_en" -line 112 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue2queue_en" -line 112 -pos 1 -win $_nTrace1
srcAction -pos 111 11 6 -win $_nTrace1 -name "issue2queue_en" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_insn_en" -line 156 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_issue_queue_full" -line 156 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_insn_en" -line 156 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "alu_insn_en" -line 156 -pos 1 -win $_nTrace1
srcAction -pos 155 11 4 -win $_nTrace1 -name "alu_insn_en" -ctrlKey off
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 143002.733020 -snap {("G9" 5)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_alu_op" -line 121 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_alu_op" -line 121 -pos 1 -win $_nTrace1
srcAction -pos 120 7 4 -win $_nTrace1 -name "id_alu_op" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "id_alu_op" -line 50 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvZoom -win $_nWave2 0.000000 207950.522782
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_fu_top" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "mul_div_issue_en" -line 16 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "div_out" -line 53 -pos 1 -win $_nTrace1
srcSelect -signal "div_out_valid" -line 54 -pos 1 -win $_nTrace1
srcSelect -signal "div_dst_Paddr" -line 55 -pos 1 -win $_nTrace1
srcSelect -signal "div_ready" -line 56 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 180256.177458 -snap {("G9" 27)}
wvSetCursor -win $_nWave2 114664.306954 -snap {("G10" 0)}
wvSelectGroup -win $_nWave2 {G15}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 240017.659472 -snap {("G9" 23)}
wvSetCursor -win $_nWave2 176369.251799 -snap {("G9" 26)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 129726.143885 -snap {("G9" 4)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_imm\[31:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 125000 -TraceValue \
           00000000000000000000000000000001
wvSetCursor -win $_nWave2 127296.815348 -snap {("G9" 5)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_rs1_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 125000 -TraceValue \
           00000000000000000000000000000000
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value   \[issue_tag\]" -line 119 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value   \[issue_tag\]" -line 119 -pos 1 -win \
          $_nTrace1
srcAction -pos 118 6 18 -win $_nTrace1 -name \
          "issue_queue_rs1_value   \[issue_tag\]" -ctrlKey off
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1        \[j\]" -line 213 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs2        \[j\]" -line 214 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G9" 29 )} 
wvSelectSignal -win $_nWave2 {( "G9" 29 30 )} 
wvSetPosition -win $_nWave2 {("G9" 29)}
wvSetPosition -win $_nWave2 {("G9" 28)}
wvSetPosition -win $_nWave2 {("G9" 27)}
wvSetPosition -win $_nWave2 {("G9" 26)}
wvSetPosition -win $_nWave2 {("G9" 25)}
wvSetPosition -win $_nWave2 {("G9" 24)}
wvSetPosition -win $_nWave2 {("G9" 23)}
wvSetPosition -win $_nWave2 {("G9" 22)}
wvSetPosition -win $_nWave2 {("G9" 21)}
wvSetPosition -win $_nWave2 {("G9" 20)}
wvSetPosition -win $_nWave2 {("G9" 19)}
wvSetPosition -win $_nWave2 {("G9" 18)}
wvSetPosition -win $_nWave2 {("G9" 17)}
wvSetPosition -win $_nWave2 {("G9" 16)}
wvSetPosition -win $_nWave2 {("G9" 15)}
wvSetPosition -win $_nWave2 {("G9" 14)}
wvSetPosition -win $_nWave2 {("G9" 13)}
wvSetPosition -win $_nWave2 {("G9" 12)}
wvSetPosition -win $_nWave2 {("G9" 11)}
wvSetPosition -win $_nWave2 {("G9" 10)}
wvSetPosition -win $_nWave2 {("G9" 9)}
wvSetPosition -win $_nWave2 {("G9" 8)}
wvSetPosition -win $_nWave2 {("G9" 7)}
wvSetPosition -win $_nWave2 {("G9" 6)}
wvSetPosition -win $_nWave2 {("G9" 5)}
wvSetPosition -win $_nWave2 {("G9" 4)}
wvSetPosition -win $_nWave2 {("G9" 3)}
wvSetPosition -win $_nWave2 {("G9" 2)}
wvSetPosition -win $_nWave2 {("G9" 1)}
wvSetPosition -win $_nWave2 {("G9" 0)}
wvSetPosition -win $_nWave2 {("G2" 8)}
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSetPosition -win $_nWave2 {("G2" 6)}
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSetPosition -win $_nWave2 {("G2" 3)}
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G2" 1)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G1" 5)}
wvSetPosition -win $_nWave2 {("G1" 4)}
wvSetPosition -win $_nWave2 {("G1" 3)}
wvSetPosition -win $_nWave2 {("G1" 2)}
wvSetPosition -win $_nWave2 {("G1" 3)}
wvSetPosition -win $_nWave2 {("G1" 4)}
wvSetPosition -win $_nWave2 {("G1" 5)}
wvSetPosition -win $_nWave2 {("G2" 0)}
wvSetPosition -win $_nWave2 {("G2" 1)}
wvSetPosition -win $_nWave2 {("G2" 2)}
wvSetPosition -win $_nWave2 {("G2" 3)}
wvSetPosition -win $_nWave2 {("G2" 4)}
wvSetPosition -win $_nWave2 {("G2" 5)}
wvSetPosition -win $_nWave2 {("G2" 6)}
wvSetPosition -win $_nWave2 {("G2" 7)}
wvSetPosition -win $_nWave2 {("G2" 8)}
wvSetPosition -win $_nWave2 {("G9" 0)}
wvSetPosition -win $_nWave2 {("G9" 1)}
wvSetPosition -win $_nWave2 {("G9" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 0)}
wvSetPosition -win $_nWave2 {("G9" 2)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 134098.935252 -snap {("G9" 2)}
wvSetCursor -win $_nWave2 135070.666667 -snap {("G9" 3)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 144787.980815 -snap {("G9" 13)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 174911.654676 -snap {("G9" 29)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 178798.580336 -snap {("G9" 7)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_rs1_value\[31:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue \
           00000000000000000000000000000000
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs1_value" -line 76 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_rs2_value" -line 77 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 135070.666667 -snap {("G9" 4)}
wvSetCursor -win $_nWave2 239045.928058 -snap {("G9" 3)}
wvSetCursor -win $_nWave2 239045.928058 -snap {("G9" 3)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_rs1_value\[0:3\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue \
           00000000000000000000000000000000, xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx, xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx, xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
srcDeselectAll -win $_nTrace1
srcSelect -signal "writeback_rs1_en\[j\]" -line 255 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 142358.652278 -snap {("G9" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 171510.594724 -snap {("G9" 5)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_en\[1\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 35000 -TraceValue 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[i\]" -line 155 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_alu_dst_Paddr" -line 155 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G9" 7 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 7)}
wvSetPosition -win $_nWave2 {("G9" 6)}
wvSelectSignal -win $_nWave2 {( "G9" 6 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 6)}
wvSetPosition -win $_nWave2 {("G9" 5)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_div_dst_Paddr" -line 156 -pos 1 -win $_nTrace1
wvSetCursor -win $_nWave2 145759.712230 -snap {("G9" 6)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 176855.117506 -snap {("G9" 32)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 179284.446043 -snap {("G9" 3)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_rs1_value\[0:3\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue \
           00000000000000000000000000000000, xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx, xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx, xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
wvSetCursor -win $_nWave2 180256.177458 -snap {("G9" 5)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_en\[1\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 35000 -TraceValue 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[i\]" -line 156 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_div_dst_Paddr" -line 156 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1\[i\]" -line 156 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 179284.446043 -snap {("G9" 8)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_Prs1\[1\]\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
wvSetCursor -win $_nWave2 173939.923261 -snap {("G9" 8)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_Prs1\[1\]\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue xxxxx
wvSetCursor -win $_nWave2 174911.654676 -snap {("G9" 5)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_en\[1\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 35000 -TraceValue 0
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_mul_in_writers1" -line 157 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_div_in_writers1" -line 156 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_mul_valid" -line 157 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "wb_div_valid" -line 156 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetCursor -win $_nWave2 170052.997602 -snap {("G9" 8)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid\[i\]" -line 156 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_valid" -line 70 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G9" 11 )} 
wvExpandBus -win $_nWave2 {("G9" 11)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1" -line 65 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G9" 16 )} 
wvExpandBus -win $_nWave2 {("G9" 16)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 175883.386091 -snap {("G9" 7)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 154505.294964 -snap {("G9" 17)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_Prs1\[0\]\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 145000 -TraceValue 00110
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 128268.546763 -snap {("G9" 26)}
wvSetCursor -win $_nWave2 187058.297362 -snap {("G9" 27)}
wvSelectSignal -win $_nWave2 {( "G9" 25 )} 
wvSelectSignal -win $_nWave2 {( "G9" 25 )} 
wvSetRadix -win $_nWave2 -format UDec
wvSelectSignal -win $_nWave2 {( "G9" 29 )} 
wvSelectSignal -win $_nWave2 {( "G9" 32 )} 
wvScrollDown -win $_nWave2 2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSelectSignal -win $_nWave2 {( "G9" 21 )} 
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 153533.563549 -snap {("G9" 5)}
wvSetCursor -win $_nWave2 146731.443645 -snap {("G9" 5)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_en\[1\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 125000 -TraceValue x
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 194832.148681 -snap {("G9" 21)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1_valid\[i\]" -line 165 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
debReload
wvSetCursor -win $_nWave2 148189.040767 -snap {("G9" 5)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 145759.712230 -snap {("G9" 5)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_en\[1\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 125000 -TraceValue x
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1\[i\]" -line 155 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_Prs1\[i\]" -line 155 -pos 1 -win $_nTrace1
srcAction -pos 154 19 10 -win $_nTrace1 -name "issue_queue_Prs1\[i\]" -ctrlKey \
          off
wvSetCursor -win $_nWave2 126810.949640 -snap {("G9" 22)}
wvSetCursor -win $_nWave2 125353.352518 -snap {("G9" 22)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_Prs1\[1\]\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 115000 -TraceValue xxxxx
wvSetCursor -win $_nWave2 116607.769784 -snap {("G9" 22)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.issue_queue_Prs1\[1\]\[4:0\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 115000 -TraceValue xxxxx
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 146245.577938 -snap {("G9" 5)}
srcActiveTrace \
           "tb_ooocpu.u_ooocpu.u_issue_queue_ctrl.u_alu_issue_queue_OoO.writeback_rs1_en\[1\]" \
           -win $_nTrace1 -TraceByDConWave -TraceTime 125000 -TraceValue x
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvSetCursor -win $_nWave2 77252.647482 -snap {("G9" 24)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 260424.019185 -snap {("G9" 17)}
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G9" 1 )} 
wvScrollDown -win $_nWave2 8
wvSelectSignal -win $_nWave2 {( "G9" 22 )} 
wvSelectSignal -win $_nWave2 {( "G9" 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 \
           18 19 20 21 22 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 22)}
wvSetPosition -win $_nWave2 {("G9" 0)}
wvSelectSignal -win $_nWave2 {( "G9" 28 )} 
wvSelectSignal -win $_nWave2 {( "G9" 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 \
           23 24 25 26 27 28 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 0)}
wvSetCursor -win $_nWave2 272084.796163 -snap {("G9" 1)}
wvSetCursor -win $_nWave2 289090.095923 -snap {("G9" 1)}
wvSetCursor -win $_nWave2 275971.721823 -snap {("G9" 1)}
wvSetCursor -win $_nWave2 89885.155875 -snap {("G9" 3)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.alu_issue_queue_pc\[31:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 85000 -TraceValue \
           00000000000000000000000000011000
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_op" -line 73 -pos 1 -win $_nTrace1
srcSelect -signal "issue_queue_pc" -line 74 -pos 1 -win $_nTrace1
srcSelect -signal "issue_queue_imm" -line 75 -pos 1 -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_op" -line 73 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G9" 2)}
wvSetPosition -win $_nWave2 {("G9" 3)}
wvSetPosition -win $_nWave2 {("G9" 4)}
wvSetPosition -win $_nWave2 {("G9" 5)}
wvSetPosition -win $_nWave2 {("G9" 6)}
wvSetPosition -win $_nWave2 {("G9" 7)}
wvSetPosition -win $_nWave2 {("G9" 8)}
wvSetPosition -win $_nWave2 {("G10" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G10" 1)}
wvSelectSignal -win $_nWave2 {( "G10" 1 )} 
wvExpandBus -win $_nWave2 {("G10" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectGroup -win $_nWave2 {G15}
wvSelectSignal -win $_nWave2 {( "G10" 1 )} 
wvSelectSignal -win $_nWave2 {( "G10" 1 2 3 4 5 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G10" 0)}
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_pc" -line 74 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSelectSignal -win $_nWave2 {( "G10" 1 )} 
wvExpandBus -win $_nWave2 {("G10" 1)}
wvSelectGroup -win $_nWave2 {G10}
wvScrollDown -win $_nWave2 1
wvSelectGroup -win $_nWave2 {G15}
wvZoom -win $_nWave2 0.000000 381890.446043
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 175101.806435 -snap {("G10" 0)}
wvSetCursor -win $_nWave2 176933.415289 -snap {("G9" 5)}
wvSetCursor -win $_nWave2 195249.503828 -snap {("G9" 3)}
wvSetCursor -win $_nWave2 185175.655132 -snap {("G10" 4)}
wvSetCursor -win $_nWave2 195432.664714 -snap {("G10" 3)}
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
srcDeselectAll -win $_nTrace1
debReload
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 175834.449977 -snap {("G10" 5)}
wvSetCursor -win $_nWave2 185358.816017 -snap {("G10" 4)}
wvSetCursor -win $_nWave2 194150.538516 -snap {("G10" 3)}
wvSetCursor -win $_nWave2 175468.128206 -snap {("G10" 5)}
wvSetCursor -win $_nWave2 185358.816017 -snap {("G10" 4)}
wvSetCursor -win $_nWave2 195432.664714 -snap {("G10" 3)}
wvSetCursor -win $_nWave2 175101.806435 -snap {("G10" 5)}
wvSetCursor -win $_nWave2 165577.440395 -snap {("G10" 5)}
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_fu_top" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_fu_top" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "mul_div_issue_en" -line 16 -pos 1 -win $_nTrace1
srcSelect -toggle -signal "mul_div_issue_en" -line 16 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_en" -line 16 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_op" -line 17 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_rs1_value" -line 18 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_rs2_value" -line 19 -pos 1 -win $_nTrace1
srcSelect -signal "mul_div_issue_queue_Pdst" -line 20 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvSetPosition -win $_nWave2 {("G10" 6)}
wvSetPosition -win $_nWave2 {("G10" 7)}
wvSetPosition -win $_nWave2 {("G10" 8)}
wvSetPosition -win $_nWave2 {("G10" 9)}
wvSetPosition -win $_nWave2 {("G10" 10)}
wvSetPosition -win $_nWave2 {("G15" 0)}
wvSetPosition -win $_nWave2 {("G10" 10)}
wvSetPosition -win $_nWave2 {("G10" 7)}
wvSetPosition -win $_nWave2 {("G10" 8)}
wvSetPosition -win $_nWave2 {("G10" 9)}
wvSetPosition -win $_nWave2 {("G10" 10)}
wvSetPosition -win $_nWave2 {("G15" 0)}
wvMoveSelected -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 5)}
wvSetPosition -win $_nWave2 {("G15" 5)}
wvSelectGroup -win $_nWave2 {G15}
wvZoomOut -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 70333.779991 -snap {("G15" 2)}
srcActiveTrace "tb_ooocpu.u_ooocpu.u_fu_top.mul_div_issue_queue_op\[4:0\]" -win \
           $_nTrace1 -TraceByDConWave -TraceTime 65000 -TraceValue 11010
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_pc          \[issue_tag\]" -line 129 -pos 1 -win \
          $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "issue_queue_pc" -line 76 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G15" 6 )} 
wvExpandBus -win $_nWave2 {("G15" 6)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectGroup -win $_nWave2 {G6}
wvSetCursor -win $_nWave2 174369.162893 -snap {("G15" 7)}
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 276206.615172 -snap {("G15" 7)}
wvSetCursor -win $_nWave2 326026.375998 -snap {("G15" 7)}
wvSetCursor -win $_nWave2 275107.649859 -snap {("G15" 5)}
wvSetCursor -win $_nWave2 342510.855684 -snap {("G15" 2)}
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 193784.216745 -snap {("G10" 3)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 165577.440395 -snap {("G15" 1)}
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvSetCursor -win $_nWave2 194516.860287 -snap {("G9" 2)}
wvSetCursor -win $_nWave2 284998.337670 -snap {("G9" 2)}
wvSetCursor -win $_nWave2 274741.328088 -snap {("G9" 1)}
wvSetCursor -win $_nWave2 193417.894974 -snap {("G9" 2)}
wvSetCursor -win $_nWave2 145429.743002 -snap {("G9" 3)}
wvSetCursor -win $_nWave2 113559.748943 -snap {("G9" 3)}
wvSetCursor -win $_nWave2 284265.694129 -snap {("G9" 2)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcSetScope -win $_nTrace1 "tb_ooocpu.u_ooocpu.u_gpr" -delim "."
srcHBSelect "tb_ooocpu.u_ooocpu.u_gpr" -win $_nTrace1
srcDeselectAll -win $_nTrace1
srcSelect -signal "gpr" -line 21 -pos 1 -win $_nTrace1
srcAddSelectedToWave -clipboard -win $_nTrace1
wvDrop -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G15" 11 )} 
wvExpandBus -win $_nWave2 {("G15" 11)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
verdiDockWidgetMaximize -dock windowDock_nWave_2
wvSetCursor -win $_nWave2 138103.307586 -snap {("G15" 20)}
wvSelectSignal -win $_nWave2 {( "G15" 22 )} 
wvScrollDown -win $_nWave2 5
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G15" 43 )} 
wvSelectSignal -win $_nWave2 {( "G15" 36 37 38 39 40 41 42 43 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 35)}
wvSelectSignal -win $_nWave2 {( "G15" 15 )} 
wvSelectSignal -win $_nWave2 {( "G15" 15 16 17 18 19 20 21 22 23 24 )} 
wvCut -win $_nWave2
wvSetPosition -win $_nWave2 {("G15" 25)}
wvSetCursor -win $_nWave2 45423.899577 -snap {("G15" 14)}
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 33701.602912 -snap {("G9" 2)}
wvSetCursor -win $_nWave2 45057.577807 -snap {("G15" 14)}
wvZoom -win $_nWave2 0.000000 579154.719611
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSelectSignal -win $_nWave2 {( "G15" 12 )} 
wvSelectSignal -win $_nWave2 {( "G15" 15 )} 
wvSelectSignal -win $_nWave2 {( "G15" 15 16 17 18 19 20 21 22 23 24 25 )} 
wvSelectSignal -win $_nWave2 {( "G15" 15 16 17 18 19 20 21 22 23 24 25 )} 
wvSetRadix -win $_nWave2 -format UDec
wvSetCursor -win $_nWave2 76387.313138 -snap {("G15" 19)}
wvSelectGroup -win $_nWave2 {G6}
wvSetCursor -win $_nWave2 295271.686785 -snap {("G15" 22)}
wvSetCursor -win $_nWave2 295271.686785 -snap {("G15" 22)}
wvSetCursor -win $_nWave2 290271.789925 -snap {("G15" 22)}
wvSelectSignal -win $_nWave2 {( "G15" 22 )} 
wvSelectSignal -win $_nWave2 {( "G15" 20 )} 
wvSetCursor -win $_nWave2 334993.089616 -snap {("G15" 20)}
wvSetCursor -win $_nWave2 427491.181525 -snap {("G15" 25)}
wvSetCursor -win $_nWave2 175551.934194 -snap {("G15" 8)}
debReload
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 288605.157638 -snap {("G6" 0)}
wvSetCursor -win $_nWave2 325548.839992 -snap {("G15" 2)}
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvZoomOut -win $_nWave2
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 357597.160671 -snap {("G10" 1)}
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvSetCursor -win $_nWave2 328931.083933 -snap {("G15" 1)}
wvSetCursor -win $_nWave2 353710.235012 -snap {("G9" 1)}
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
verdiWindowResize -win $_Verdi_1 "1508" "312" "900" "700"
verdiWindowResize -win $_Verdi_1 "1282" "274" "900" "700"
verdiDockWidgetHide -dock windowDock_nWave_2
verdiWindowResize -win $_Verdi_1 "958" "316" "900" "700"
verdiDockWidgetSetCurTab -dock widgetDock_MTB_SOURCE_TAB_1
srcDeselectAll -win $_nTrace1
wvCreateWindow
verdiDockWidgetSetCurTab -dock widgetDock_MTB_SOURCE_TAB_1
verdiDockWidgetSetCurTab -dock windowDock_OneSearch
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
verdiDockWidgetSetCurTab -dock widgetDock_MTB_SOURCE_TAB_1
verdiDockWidgetSetCurTab -dock windowDock_OneSearch
verdiDockWidgetSetCurTab -dock widgetDock_<Message>
verdiDockWidgetSetCurTab -dock windowDock_nWave_5
wvCloseWindow -win $_nWave5
verdiDockWidgetSetCurTab -dock widgetDock_<Message>
nsMsgSelect -range {0 3-3}
nsMsgSelect -range {0 1-1}
nsMsgAction -tab cmpl -index {0 1}
nsMsgSelect -range {0 1-1}
verdiDockWidgetSetCurTab -dock windowDock_OneSearch
verdiDockWidgetSetCurTab -dock widgetDock_MTB_SOURCE_TAB_1
verdiWindowResize -win $_Verdi_1 "958" "316" "900" "700"
verdiDockWidgetDisplay -dock widgetDock_WelcomePage
verdiDockWidgetDisplay -dock widgetDock_WelcomePage
verdiDockWidgetHide -dock widgetDock_WelcomePage
verdiShowWindow -win $_Verdi_1 -switchFS
verdiDockWidgetSetCurTab -dock widgetDock_<Message>
debReload
verdiDockWidgetSetCurTab -dock widgetDock_MTB_SOURCE_TAB_1
verdiDockWidgetSetCurTab -dock windowDock_OneSearch
verdiDockWidgetSetCurTab -dock windowDock_nWave_2
wvDisplayGridCount -win $_nWave2 -off
wvGetSignalClose -win $_nWave2
wvReloadFile -win $_nWave2
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollUp -win $_nWave2 1
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
wvScrollDown -win $_nWave2 0
debExit
